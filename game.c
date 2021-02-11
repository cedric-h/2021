//------------------------------------------------------------------------------
//  game.c
//------------------------------------------------------------------------------

#define LEN(arr) ((int) (sizeof arr / sizeof arr[0]))

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "math.h"
#include "sokol/sokol_time.h"
#include "sokol/sokol_app.h"
#include "sokol/sokol_gfx.h"
#include "sokol/sokol_glue.h"
#include "renderer.h"

void panic(char *msg) {
    fprintf(stderr, msg);
    sapp_request_quit();
}

static struct {
    /* -- keys_down --
        Sokol-app shuttles events from the operating system to our application
        whenever keys are pressed or released, but this is not amenable to changing
        the game state for the duration of a key press. Increasing the player's
        forward velocity whenever a "W down" event is received makes the movement
        jerky. Increasing the velocity on each update tick where W is down makes
        for a much smoother experience.

        Sokol provides an enum where each variant corresponds to a key. Casting
        these enum variants into integers provides a suitable index for accessing
        this array. For example, to see if the W key is down:

        if (input.keys_down[(int) SAPP_KEYCODE_W])
            printf("W is down!\n");
    */
    bool keys_down[350];

    /* -- keys_pressed --
        Being able to query input state from anywhere in the application, not just
        in the rapidly growing switch/case which handles events received from sokol,
        is exceedingly freeing. One can decentralize input logic, and keep code that
        handles input for separate concerns (user interface, player navigation, etc.)
        near other code which addresses the same concern. To better facilitate this,
        another array, keys_pressed, is maintained. It allows for the same kind of
        queries as the sokol_app event switch/case, namely "was this key pressed
        on this frame."
        
        There is one major difference (aside from how `keys_pressed` can be queried
        from anywhere in the application instead of inside one particular switch/case).
        On some operating systems, "repeat events" are sent in if a key is held for a
        long enough duration. While sokol will still send these events to our switch/case
        should the operating system generate them, these are not reflected when querying
        `keys_pressed`; if a value in this array is true, then the key at that index is
        guaranteed to have transitioned from up to down this frame. No "repeat events" are
        taken into account.
    */
    bool keys_pressed[350];

    /* left mouse button down? */
    bool left_mb_down;
} input;



/* --------- Entities */

typedef enum {
    EntProp_Active,
    EntProp_Hidden,
    EntProp_Phys,
    EntProp_Whackable,
    EntProp_Selectable,
    EntProp_Selected,
    EntProp_COUNT,
} EntProp;

typedef enum {
    Art_Icosahedron,
    Art_Cylinder,
} Art;

typedef enum {
    PruneTag_Default,
    PruneTag_Branch,
    PruneTag_Leaf,
} PruneTag;

typedef enum {
    DebugTag_Player,
    DebugTag_Branch,
    DebugTag_Leaf,
    DebugTag_Limb,
    DebugTag_Trunk,
    DebugTag_Dirt,
    DebugTag_Stone,
} DebugTag;

void print_dbg_tag(DebugTag dbg_tag) {
    switch (dbg_tag) {
        case DebugTag_Player:;
              printf("Player");
              break;
        case DebugTag_Branch:;
              printf("Branch");
              break;
        case DebugTag_Leaf:;
              printf("Leaf");
              break;
        case DebugTag_Limb:;
              printf("Limb");
              break;
        case DebugTag_Trunk:;
              printf("Trunk");
              break;
        case DebugTag_Dirt:;
              printf("Dirt");
              break;
        case DebugTag_Stone:;
              printf("Stone");
              break;
        default:;
            panic("unknown DebugTag");
            break;
    }
}

typedef enum {
    Resource_StickLong,
    Resource_StickShort,
    Resource_Leaf,
} Resource;

typedef enum {
    /* take "orphaned" to mean (ent->parent == NULL) */
    /* take "isolated" to mean (ent->parent == NULL && ent->first_child == NULL) */
    PickUpState_None,         /* no pickup scheduled or in progress */
    PickUpState_WhenIsolated, /* proceed to Ongoing if orphaned */
    PickUpState_WhenOrphaned, /* proceed to Ongoing if isolated */
    PickUpState_Next,         /* proceed to Ongoing unconditionally */
    PickUpState_Ongoing,      /* state when pick up is in progress; do not set directly.
                                 when animation is complete, proceed to PickupState_Done.*/
    PickUpState_Done,         /* ent is parented to the player. removed once processed. */
} PickUpState;

#define MAX_ENT_CHILDREN 50
typedef struct Ent Ent;
struct Ent {
    /* specifies which of the following field groups are valid */
    u64 props[(EntProp_COUNT + 63)/64];
    DebugTag dbg_tag;

    /* building, crafting, & hotbar grouping */
    Resource resource;

    /* shake animation */
    f64 shake_timer;
    Mat4 before_shake;

    /* player item pick up */
    PickUpState pick_up_state;
    Vec3 pick_up_start_pos;
    u64 pick_up_start_time;

    /* used to detach children from parents */
    PruneTag prune_tag;

    /* appearance */
    Art art;
    Vec4 color;

    /* positioning */
    Ent *parent, *first_child, *last_child, *next, *prev;
    Mat4 mat;
    bool independent_children; /* If true, children of this ent are
                                  not positioned relative to it */
    Vec3 scale;

    /* physics & gravity: EntProp_Phys */
    Vec3 vel, acc;
    f32 mass;
};

INLINE Ent default_ent(DebugTag dbg_tag) {
    return (Ent) {
        .dbg_tag = dbg_tag,
        .art = Art_Icosahedron,
        .color = vec4(1.0, 0.0, 1.0, 1.0),
    };
}

INLINE bool has_ent_prop(Ent *ent, EntProp prop) {
    return !!(ent->props[prop/64] & ((u64)1 << (prop%64)));
}

INLINE bool toggle_ent_prop(Ent *ent, EntProp prop) {
    bool before = has_ent_prop(ent, prop);
    ent->props[prop/64] ^= (u64)1 << (prop%64);
    return before;
}

INLINE bool take_ent_prop(Ent *ent, EntProp prop) {
    bool before = has_ent_prop(ent, prop);
    ent->props[prop/64] &= ~((u64)1 << (prop%64));
    return before;
}

INLINE bool give_ent_prop(Ent *ent, EntProp prop) {
    bool before = has_ent_prop(ent, prop);
    ent->props[prop/64] |= (u64)1 << (prop%64);
    return before;
}

INLINE Mat4 get_ent_unscaled_mat(Ent *ent) {
    Mat4 m = ent->mat;
    if (ent->parent && !ent->parent->independent_children)
        m = mul4x4(get_ent_unscaled_mat(ent->parent), m);
    return m;
}

INLINE Mat4 get_ent_mat(Ent *ent) {
    return mul4x4(get_ent_unscaled_mat(ent), scale4x4(ent->scale));
}

INLINE void add_ent_child(Ent *parent, Ent *child) {
    child->parent = parent;
    child->prev = parent->last_child;
    child->next = NULL;
    if (parent->last_child) parent->last_child->next = child;
    else parent->first_child = child;

    parent->last_child = child;
}

/* Used to iterate through an entity's parent-child tree */
INLINE Ent *ent_tree_iter(Ent *node) {
    if (node == NULL) return NULL;
    if (node->first_child) return node->first_child;

    while (node && node->next == NULL)
        node = node->parent;
    
    if (node) return node->next;
    return NULL;
}

/* Similar to ent_tree_iter, but does not crawl up past the second argument.
   This can be useful for iterating only through a node's children,
   and not through its parents or cousins.*/
INLINE Ent *ent_tree_capped_iter(Ent *node, Ent *cap) {
    if (node == NULL) return NULL;
    if (node->first_child) return node->first_child;

    while (node && node->next == NULL)
        node = node->parent;
    
    if (node && node != cap) return node->next;
    return NULL;
}

/* NOTE: positioning assumes child is actually a child of parent */
INLINE void detach_child(Ent *child) {
    child->mat = get_ent_unscaled_mat(child);
    Ent *parent = child->parent;
    if (parent) {
        if (parent->last_child == child) parent->last_child = child->prev;
        if (parent->first_child == child) parent->first_child = child->next;
    }

    if (child->prev) child->prev->next = child->next;
    if (child->next) child->next->prev = child->prev;
    child->next = NULL;
    child->prev = NULL;
    child->parent = NULL;

    if (child->mass > 0.0)
        give_ent_prop(child, EntProp_Phys);
}

INLINE void detach_child_recursive(Ent *child) {
    for (Ent *c = child->first_child; c; c = c->next)
        detach_child_recursive(c);
    
    detach_child(child);
}




/* --------- CAMERA */

typedef struct {
    f32 pitch_deg, yaw_deg;
    Mat3 rotation;
} Camera;

/* Returns a Mat3 oriented according to the camera's pitch and yaw.
    Does not account for being stuck to the side of a sphere
*/
Mat3 cam_mat3_local(Camera *cam) {
    f32 pitch = to_radians(cam->pitch_deg),
          yaw = to_radians(cam->yaw_deg  );
    
    Quat q = mulQ(axis_angleQ(vec3_y(), yaw  ),
                  axis_angleQ(vec3_x(), pitch));
    return (Mat3) {
        .x = mulQ3(q, vec3_x()),
        .y = mulQ3(q, vec3_y()),
        .z = mulQ3(q, vec3_z()),
    };
}

/* Returns a Mat3 oriented according to the camera's pitch and yaw,
    which is also rotated according to the camera's rotation matrix
    which accounts for being stuck to the side of a sphere
*/
Mat3 cam_mat3(Camera *cam) {
    return mul3x3(cam->rotation, cam_mat3_local(cam));
}

/* Applies a rotation directly to the camera, making sure to keep it
    within bounds reasonable for the average human neck.
*/
void turn_cam(Camera *cam, f32 yaw_delta_deg, f32 pitch_delta_deg) {
    cam->pitch_deg = clamp(-89.0f, cam->pitch_deg + pitch_delta_deg, 89.0f);
    cam->yaw_deg = wrap(cam->yaw_deg + yaw_delta_deg, 360.0f);
}



/* --------- PLAYER */

typedef enum {
    SwingState_NoSwing,
    SwingState_PreAct,
    SwingState_ActNow,
    SwingState_PostAct,
} SwingState;

#define EQUIP_LOAD_SECS 0.1f
typedef struct {
    Ent *ent;

    Camera camera;
    f32 eye_height;
    Vec2 cam_vel;

    Ent *grabbed, *equipped;
    u64 last_grab_finish;
    u64 equip_start;

    u64 swing_start;
    SwingState swing;
} Player;

bool player_equipped(Player *plyr) {
    f32 elapsed = stm_sec(stm_since(plyr->equip_start));
    return plyr->equipped && elapsed > EQUIP_LOAD_SECS;
}

/* Applies the camera's turning velocity to the camera.
    The camera's turning velocity is also gradually reduced,
    so as to create a smooth sliding effect.
*/
void update_cam_vel(Camera *cam, Vec2 *cam_vel) {
    *cam_vel = mul2f(*cam_vel, 0.9f);
    turn_cam(cam, cam_vel->x, cam_vel->y);
}

/* Returns a point in world space that the camera can be considered
    to be looking out from.
*/
Vec3 player_eye(Player *plyr) {
    return add3(plyr->ent->mat.w.xyz,
                mul3f(plyr->camera.rotation.y, plyr->eye_height));
}

/* Returns a view matrix for the player based on his position and camera */
Mat4 player_view(Player *plyr) {
    return mat3_translation4x4(cam_mat3(&plyr->camera), player_eye(plyr));
}

void control_player_movement(Player *plyr, Vec3 up) {
    /* reorient camera */
    rotated_up_indefinite_basis(&plyr->camera.rotation, up);
    update_cam_vel(&plyr->camera, &plyr->cam_vel);

    Ent *p_ent = plyr->ent;

    /* move position */
    Mat3 cam_dirs = cam_mat3(&plyr->camera),
             axes = plyr->camera.rotation;
    Vec3 move_dir = vec3f(0.0),
             facing = project_plane_vec3(axes.y, cam_dirs.z),
             side   = project_plane_vec3(axes.y, cam_dirs.x);
    if (input.keys_down[(int) SAPP_KEYCODE_W])
        move_dir = add3(move_dir, facing);
    if (input.keys_down[(int) SAPP_KEYCODE_S])
        move_dir = sub3(move_dir, facing);
    if (input.keys_down[(int) SAPP_KEYCODE_A])
        move_dir = sub3(move_dir, side);
    if (input.keys_down[(int) SAPP_KEYCODE_D])
        move_dir = add3(move_dir, side);

    if (input.keys_pressed[(int) SAPP_KEYCODE_SPACE] && mag3(p_ent->mat.w.xyz) < 1.101)
        p_ent->acc = add3(p_ent->acc, mul3f(up, 0.028));

    f32 len = mag3(move_dir);
    if (len > 0.0) {
        /* Normalizing move_dir prevents "two keys for twice the speed" */
        Vec3 norm = div3f(move_dir, len);
        p_ent->vel = add3(p_ent->vel, mul3f(norm, 0.004f));
    }
}




/* --------- WORLD */

static struct {
    Player player;
    u64 start_time, last_render;
    f64 dt;
    f32 roll;
    Ent ents[2000];
    int ent_count;
} world;

INLINE bool ent_world_iter(Ent **ent) {
    if (*ent == NULL) *ent = world.ents;
    else if (*ent - world.ents <= world.ent_count) (*ent)++;
    else return false;
    return true;
}

INLINE Ent *add_ent(Ent ent) {
    Ent *slot = &world.ents[world.ent_count++];
    *slot = ent;
    return slot;
}

Vec3 spawn_planet_with_tree() {
    Vec3 sphere_points[SPHERE_POINTS_LEN(3)];
    int points = fill_sphere_points(3, sphere_points);
    Vec3 tree_pos;
    for (int i = 0; i < points*2; i++) {
        bool dirt = i == 0;
        f32   out = 0.995f + 0.010f * randf(),
            scale = 0.100f + 0.010f * randf();
        Vec3 pos;
        Vec4 color = vec4(0.37f, 0.25f, 0.5f, 1.0f);
        if (i >= points) {
            out -= scale - 0.05 + 0.1 * randf();
            scale *= 1.08f;
            pos = mul3f(sphere_points[i - points], -1.0f);
            color = mul4f(color, 0.7);
            color.w = 1.0;
        }
        else pos = sphere_points[i];

        if (dirt) {
            color = vec4(0.54f, 0.38f, 0.327f, 1.0f);
            scale *= 0.54f;
            tree_pos = mul3f(pos, 1.0 + scale);
        };

        Mat4 m = translate4x4(mul3f(pos, out));
        m = mul4x4(m, axis_angle4x4(rand3(), randf() * PI32 * 2.0f));

        Ent ent = default_ent(DebugTag_Stone);
        ent.scale = vec3f(scale);
        ent.mat = m;
        ent.color = color;
        ent.art = Art_Icosahedron;
        #define SUB_DIRTS 3
        #define DIRT_SCALE 0.13
        if (dirt) {
            ent.dbg_tag = DebugTag_Dirt;
            Mat3 b = ortho_bases3x3(pos),
                 bases = (Mat3) { .cols = { b.y, b.z, b.x } };
            for (int x = 0; x < SUB_DIRTS; x++)
            for (int y = 0; y < SUB_DIRTS; y++)
            for (int z = 0; z < SUB_DIRTS; z++) {
                     /* xyz in the domain 0..(SUB_DIRTS - 1) */
                Vec3 coord = vec3((f32) x, (f32) y, (f32) z),
                     /* in the domain 0..1 */
                     norm_offset = div3f(coord, (f32) (SUB_DIRTS - 1)),
                     /* in the domain (-1..1)(DIRT_SCALE/2) */
                     abs_offset = mul3f(sub3(vec3f(0.5f), norm_offset), DIRT_SCALE),
                     /* facing out from the planet, not towards north pole */
                     offset = mat3_rel3(bases, abs_offset);
                m = mul4x4(m, axis_angle4x4(rand3(), randf() * PI32 * 2.0f));
                ent.mat = mul4x4(translate4x4(offset), m);
                add_ent(ent);
            }
        } else
            add_ent(ent);
    }
    return tree_pos;
}

void spawn_tree(Vec3 pos) {
    Vec3 scale = vec3(0.015f, 0.31f, 0.015f);
    Vec4 color = vec4(0.29f, 0.22f, 0.175f, 1.0f);
    Mat3 bases = ortho_bases3x3(norm3(pos));
    Mat4 m = translate4x4(pos);
    m = mul4x4(m, mat34x4(bases));

    Ent tree = default_ent(DebugTag_Trunk);
    tree.art = Art_Cylinder;
    tree.color = color;
    tree.mat = m;
    tree.scale = scale;
    Ent *trunk = add_ent(tree);
    trunk->pick_up_state = PickUpState_WhenIsolated;

    const f32 main_limbs[] = { 0.11f, 0.1f, 0.4f, 0.09f, 0.3f, };
    f32 a = 0.0f;
    for (int i = 0; i < LEN(main_limbs); a += main_limbs[i], i++) {
        f32 af = main_limbs[i];
        Mat4 l = translate4x4(mul3(vec3_y(), scale));
        l = mul4x4(l, axis_angle4x4(vec3_y(), a * PI32 * 2.0f));
        l = mul4x4(l, axis_angle4x4(vec3_x(), 1.2f - af * 1.6f));
        Vec3 lscale = vec3(scale.x * (0.5f + af),
                           scale.y * af + 0.2f,
                           scale.z * (0.5f + af));
        Ent *limb = add_ent(tree);
        limb->pick_up_state = PickUpState_WhenOrphaned;
        limb->dbg_tag = DebugTag_Limb;
        limb->mat = l;
        limb->scale = lscale;
        give_ent_prop(limb, EntProp_Selectable);
        give_ent_prop(limb, EntProp_Whackable);
        add_ent_child(trunk, limb);

        const f32 boffset = 0.095f;
        for (f32 branch = boffset; branch < lscale.y; branch += 0.08f) {
            Mat4 b = translate4x4(mul3f(vec3_y(), branch));
            f32 dist = 1.0 - branch / lscale.y;
            Vec3 bscale = mul3(lscale, vec3(0.5f, dist / 3.4f + 0.12f, 0.5f));
            Mat4 rot = axis_angle4x4(vec3_y(), -PI32 * 0.5f);
            rot = mul4x4(rot, axis_angle4x4(vec3_x(), -PI32 * 0.1f));
            f32 twist = 0.3f;

            Ent branch_e = tree;
            branch_e.pick_up_state = PickUpState_WhenOrphaned;
            branch_e.scale = bscale;
            branch_e.parent = limb;
            branch_e.prune_tag = PruneTag_Branch;
            branch_e.dbg_tag = DebugTag_Branch;

            Mat4 lb = mul4x4(mul4x4(b, axis_angle4x4(vec3_z(),  PI32 * twist)), rot);
            Ent *branch_l = add_ent(branch_e);
            branch_l->mat = lb;
            add_ent_child(limb, branch_l);

            Mat4 rb = mul4x4(mul4x4(b, axis_angle4x4(vec3_z(), -PI32 * twist)), rot);
            Ent *branch_r = add_ent(branch_e);
            branch_r->mat = rb;
            add_ent_child(limb, branch_r);

            const f32 leoffset = 0.03f;
            for (f32 dir = -1.0; dir <= 1.0; dir += 2.0)
            for (f32 leaf = leoffset; leaf < bscale.y; leaf += 0.028f) {
                Mat4 pos = translate4x4(vec3(0.0f, leaf, 0.0f));

                f32 dist = 1.0 - leaf / bscale.y;
                Vec3 lescale = mul3f(vec3(0.025f, 0.002f, 0.015f), 0.6 + dist * 0.8);
                Mat4 center_leaf = translate4x4(mul3(mul3f(vec3_x(), dir), lescale)),
                     rot = axis_angle4x4(vec3_x(), PI32 * (0.5 + dist * 0.3f));
                rot = mul4x4(rot, axis_angle4x4(vec3_z(), PI32 * dist * 0.1f));
                Mat4 leafter = mul4x4(rot, center_leaf);

                Ent leaf_ent = tree;
                leaf_ent.scale = lescale;
                leaf_ent.dbg_tag = DebugTag_Leaf;
                leaf_ent.pick_up_state = PickUpState_None;
                leaf_ent.prune_tag = PruneTag_Leaf;
                leaf_ent.mass = 0.6 + 0.4 * randf();
                leaf_ent.mat = mul4x4(pos, leafter);
                leaf_ent.color = vec4(0.38f, 0.54f, 0.327f, 1.0f);

                add_ent_child(branch_l, add_ent(leaf_ent));
                add_ent_child(branch_r, add_ent(leaf_ent));
            }
        }
    }
}

void init(void) {
    init_renderer();

    srandf(9, 12, 32, 10);
    world.ent_count = 0;

    Ent player_ent = default_ent(DebugTag_Player);
    player_ent.mat = translate4x4(vec3(0.0f, 1.0f, 0.0f));
    player_ent.mass = 16.0f;
    player_ent.independent_children = true;
    world.player = (Player) {
        .ent = add_ent(player_ent),
        .eye_height = 0.75f,
        .cam_vel = vec2f(0.0f),
        .camera = (Camera) {
            .rotation = (Mat3) {
                .x = vec3_x(),
                .y = vec3_y(),
                .z = vec3_z(),
            },
            .pitch_deg = 0.0f,
            .yaw_deg = 0.0f,
        },
        .grabbed = NULL,
        .equipped = NULL,
        .swing_start = 0,
    };
    give_ent_prop(world.player.ent, EntProp_Phys);

    Vec3 tree_pos = spawn_planet_with_tree();
    spawn_tree(tree_pos);
    
    stm_setup();
    world.start_time = stm_now();
    world.dt = 0.0;
    world.last_render = stm_now();
}

void event(const sapp_event *ev) {
    switch (ev->type) {
        case SAPP_EVENTTYPE_MOUSE_MOVE:;
            if (sapp_mouse_locked()) {
                world.player.cam_vel.x += ev->mouse_dx * 0.025f;
                world.player.cam_vel.y += ev->mouse_dy * 0.025f;
            }
            break;
        case SAPP_EVENTTYPE_MOUSE_DOWN:;
            if (ev->mouse_button == SAPP_MOUSEBUTTON_LEFT) {
                input.left_mb_down = true;
            }
            break;
        case SAPP_EVENTTYPE_MOUSE_UP:;
            if (ev->mouse_button == SAPP_MOUSEBUTTON_LEFT) {
                input.left_mb_down = false;
            }
            sapp_lock_mouse(true);
            sapp_show_mouse(false);
            break;
        case SAPP_EVENTTYPE_KEY_DOWN:;
            #ifndef NDEBUG
                if (ev->key_code == SAPP_KEYCODE_ESCAPE)
                    sapp_request_quit();
            #endif
            if (!input.keys_down[(int) ev->key_code])
                input.keys_pressed[(int) ev->key_code] = true;
            input.keys_down[(int) ev->key_code] = true;
            break;
        case SAPP_EVENTTYPE_KEY_UP:;
            input.keys_down[(int) ev->key_code] = false;
            if (ev->key_code == SAPP_KEYCODE_ESCAPE) {
                sapp_lock_mouse(false);
                sapp_show_mouse(true);
            }
            break;
        default:;
            break;
    }
}

/* finds a limb that's under the mouse */
Ent *select_limb(EntProp filter, f32 scale) {
    Ray cam = (Ray) { .origin = player_eye(&world.player), 
                      .vector = cam_mat3(&world.player.camera).z };
    f32 hit_dist = 1.0;
    Ent* hit_ent = NULL;
    for (Ent *ent = 0; ent_world_iter(&ent);) {
        if (!has_ent_prop(ent, filter))
            continue;

        Vec3 new_hit;
        if (ray_hit_cylinder(cam, get_ent_mat(ent), &new_hit, scale)) {
            f32 new_dist = magmag3(sub3(cam.origin, new_hit));
            if (new_dist < hit_dist) {
                hit_dist = new_dist;
                hit_ent = ent;
            }
        }
    }

    return hit_ent;
}

void prune_limb(Ent *limb, PruneTag tag, f32 percent) {
    for (Ent *child = limb; child;)
        if (child->prune_tag == tag && randf() < percent) {
            Ent *cached_next = child->next ? child->next : child->parent;
            detach_child_recursive(child);
            child = cached_next;
        } else
            child = ent_tree_capped_iter(child, limb);
}

void grab_limbs(Player *plyr) {
    for (Ent *ent = 0; ent_world_iter(&ent);)
        take_ent_prop(ent, EntProp_Selected);

    f64 delta = stm_sec(stm_since(plyr->last_grab_finish));
    bool cooled_down = plyr->last_grab_finish ? delta > 1.0 : true;
    if (plyr->grabbed == NULL && plyr->equipped == NULL && cooled_down) {
        Ent *limb = select_limb(EntProp_Selectable, 4.0);

        if (limb) {
            give_ent_prop(limb, EntProp_Selected);

            for (Ent *c = limb; c; c = ent_tree_capped_iter(c, limb))
                give_ent_prop(c, EntProp_Selected);

            if (input.left_mb_down && plyr->grabbed != limb) {
                plyr->grabbed = limb;
                limb->before_shake = limb->mat;
            }
        }
    }
    if (plyr->grabbed)
        if (!input.left_mb_down)
            plyr->grabbed = NULL;
        else {
            Ent *grab = plyr->grabbed;
            if (grab->shake_timer > 1.5) {
                grab->shake_timer = 0.0;
                prune_limb(plyr->grabbed, PruneTag_Leaf, 0.2);
                take_ent_prop(grab, EntProp_Whackable);

                plyr->last_grab_finish = stm_now();

                /* detach what's grabbed */
                detach_child(grab);
                plyr->grabbed = NULL;
            } else {
                give_ent_prop(grab, EntProp_Selected);
                for (Ent *c = grab; c; c = ent_tree_capped_iter(c, grab))
                    give_ent_prop(c, EntProp_Selected);
                grab->shake_timer += world.dt * 2.0;
            }
        }
}

void animate_shaking(void) {
    for (Ent *ent = 0; ent_world_iter(&ent);) {
        if (ent->shake_timer > 0.0) {
            ent->shake_timer -= world.dt;
            f32 t = sin(ent->shake_timer * 10.0) * ent->shake_timer * 0.1;

            Mat4 spin = axis_angle4x4(vec3_z(), t);
            ent->mat = mul4x4(ent->before_shake, spin);
        }
    }
}

void draw(Ent *ent) {
    Mat4 mat = get_ent_mat(ent);

    Asset asset;
    switch (ent->art) {
        case Art_Icosahedron:;
            asset = ASSET_ICOSAHEDRON;
            break;
        case Art_Cylinder:;
            asset = ASSET_CYLINDER;
            break;
        default:;
            panic("unknown art");
            break;
    }
    if (has_ent_prop(ent, EntProp_Selected))
        draw_ghost_asset(mat, asset, ent->color);

    draw_asset(mat, asset, ent->color);
}

#define SWING_DUR 1.0f
void control_player_equipped(Player *plyr, Vec3 player_up) {
    if (input.left_mb_down && !plyr->swing) {
        plyr->swing_start = stm_now();
        plyr->swing = SwingState_PreAct;
    }

    f64 swing_d = stm_sec(stm_since(plyr->swing_start));
    if (plyr->swing == SwingState_PreAct && swing_d > 0.61) {
        plyr->swing = SwingState_PostAct;

        Ent *hit = select_limb(EntProp_Whackable, 5.5);
        if (hit) {
            int child_count = 0;
            for (Ent *c = hit; c; c = ent_tree_capped_iter(c, hit))
                child_count++;

            if (child_count < 10) {
                detach_child_recursive(hit);
            } else {
                prune_limb(plyr->equipped,   PruneTag_Leaf, 0.5);
                prune_limb(           hit,   PruneTag_Leaf, 0.5);
                prune_limb(plyr->equipped, PruneTag_Branch, 0.3);
                prune_limb(           hit, PruneTag_Branch, 0.3);
                hit->before_shake = hit->mat;
                hit->shake_timer = 0.75;
            }
        } else
            prune_limb(plyr->equipped, PruneTag_Leaf, 0.2);
    }
    if (swing_d >= SWING_DUR)
        plyr->swing = SwingState_NoSwing;
}

void animate_pick_up(Player *plyr, Vec3 up) {
    for (Ent *ent = 0; ent_world_iter(&ent);) {
        switch (ent->pick_up_state) {
            case (PickUpState_None):;
                break;
            case (PickUpState_WhenIsolated):;
                if (ent->parent || ent->first_child) break;
            case (PickUpState_WhenOrphaned):;
                if (ent->parent) break;
            case (PickUpState_Next):;
                ent->pick_up_start_pos = ent->mat.w.xyz;
                ent->pick_up_start_time = stm_now();
                ent->pick_up_state = PickUpState_Ongoing;
                break;
            case (PickUpState_Ongoing):;
                f64 pick_up_d = stm_sec(stm_since(ent->pick_up_start_time));
                f32 t = pick_up_d / 0.7;
                Vec3 start = ent->pick_up_start_pos,
                      neck = sub3(player_eye(plyr), mul3f(up, 0.6f));
                ent->mat.w.xyz = lerp3(start, ease_in_expo(t), neck);

                if (t >= 1.0f) {
                    ent->pick_up_state = PickUpState_Done;
                    add_ent_child(plyr->ent, ent);
                }
                break;
            case (PickUpState_Done):;
                break;
            default:
                panic("invalid PickUpState variant");
                break;
        }
    }
}

void animate_equipped(Player *plyr) {
    Ent *equip = plyr->equipped;
    Mat4 *mat = &equip->mat;
    f64 start_d = stm_sec(stm_since(plyr->equip_start)),
        swing_d = stm_sec(stm_since(world.player.swing_start));

    Vec3 pos = vec3(0.210f, -0.230f, 0.290f);
    f32 breathe = sinf(start_d * 1.7f) / 15.0f;
    pos.y += breathe * 0.005f;
    Quat rotq = yprQ(PI32 / 4.0f + 0.8f + breathe, -0.4f, breathe / 2.9f);

    if (plyr->swing) {
        typedef struct {
            f32 duration;
            Quat rot;
            Vec3 pos;
        } KeyFrame;

        KeyFrame default_kf = { 0.0f, identQ(), vec3f(0.0f) };
        KeyFrame keyframes[] = {
            {2.0f, yprQ( 1.0f, -1.0f, 0.0f), vec3(-0.24f, 0.10f, 0.02f)},/* move to left   */
            {2.3f, yprQ(-0.5f, -1.0f, 0.0f), vec3(-0.34f, 0.10f, 0.05f)},/* wind up        */
            {1.0f, yprQ( 2.0f, -1.4f, 0.0f), vec3(-0.12f, 0.10f, 0.06f)},/* swing          */
            {1.0f, yprQ( 3.5f, -1.6f, 0.0f), vec3(-0.08f, 0.10f, 0.06f)},/* follow through */
            {2.0f, yprQ( 0.0f,  0.0f, 0.0f), vec3( 0.00f, 0.00f, 0.00f)},/* back home      */
        };

        f32 total = 0.0;
        for (int i = 0; i < LEN(keyframes); i++)
            total += keyframes[i].duration;

        for (int i = 0; i < LEN(keyframes); i++) {
            KeyFrame next = keyframes[i],
                     last = i == 0 ? default_kf : keyframes[i - 1];
            f32 duration = (next.duration / total) * SWING_DUR;
            if (swing_d <= duration) {
                f32 t = swing_d / duration;
                rotq = mulQ(rotq, slerpQ(last.rot, t, next.rot));
                pos = add3(pos, lerp3(last.pos, t, next.pos));
                break;
            }
            swing_d -= duration;
        }
    }

    Mat3 cam = cam_mat3(&plyr->camera);
    Vec3 p =  player_eye(plyr);
    p = add3(p, mul3f(cam.x, pos.x));
    p = add3(p, mul3f(cam.y, pos.y + fminf(start_d * 0.17f, EQUIP_LOAD_SECS)));
    p = add3(p, mul3f(cam.z, pos.z));
    *mat = mat34x4(cam);
    mat->w.xyz = p;
    *mat = mul4x4(*mat, quat4x4(rotq));
}

const f32 planet_mass = 100.0;
const Vec3 planet = { 0.0f, 0.0f, 0.0f };
void gravity_physics(void) {
    for (Ent *ent = 0; ent_world_iter(&ent);)
        if (has_ent_prop(ent, EntProp_Phys)) {
            Mat4 total_mat = get_ent_mat(ent);
            f32 dist = mag3(total_mat.w.xyz),
                mass_g = 6.67e-6 * ent->mass * planet_mass;
            Vec3 up = norm3(sub3(total_mat.w.xyz, planet));
            Mat3 bases = ortho_bases3x3(up);
            ent->vel = add3(ent->vel, mul3f(up, -mass_g / (dist * dist)));

            if (dist < 1.1f) {
                f32 depth = 1.1f - dist;
                ent->mat.w.xyz = add3(ent->mat.w.xyz, mul3f(up, depth));
                ent->vel = add3(ent->vel, mul3f(up, depth * 0.01f));
            } else if (ent->mass <= 1.0) {
                f32 spiral = 0.000008f * fmaxf(world.roll, 1.0);
                ent->vel = add3(ent->vel, mul3f(bases.x, sinf(world.roll) * spiral));
                ent->vel = add3(ent->vel, mul3f(bases.z, cosf(world.roll) * spiral));
            }

            ent->acc = mul3f(ent->acc, 0.92f);
            ent->vel = add3(ent->vel, ent->acc);
            ent->vel = mul3f(ent->vel, 0.87f);
            ent->mat.w.xyz = add3(ent->mat.w.xyz, ent->vel);
        }
}

void inventory_logistics(Player *plyr) {
    for (Ent *itm = plyr->ent->first_child; itm; itm = itm->next)
        if (itm->pick_up_state == PickUpState_Done) {
            puts("once?");
            itm->pick_up_state = PickUpState_None;
            for (Ent *c = itm; c; c = ent_tree_capped_iter(c, itm))
                give_ent_prop(c, EntProp_Hidden);
        }
    if (plyr->equipped == NULL && plyr->ent->first_child) {
        plyr->equipped = plyr->ent->first_child;
        print_dbg_tag(plyr->equipped->dbg_tag);
        plyr->equip_start = stm_now();
        for (Ent *c = plyr->equipped; c; c = ent_tree_capped_iter(c, plyr->equipped))
            take_ent_prop(c, EntProp_Hidden);
    }
}

void frame(void) {
    Vec3 player_up = norm3(sub3(world.player.ent->mat.w.xyz, planet));

    grab_limbs(&world.player);
    control_player_movement(&world.player, player_up);
    if (player_equipped(&world.player))
        control_player_equipped(&world.player, player_up);
    inventory_logistics(&world.player);

    gravity_physics();

    start_render(player_view(&world.player));
    if (player_equipped(&world.player))
        animate_equipped(&world.player);
    animate_pick_up(&world.player, player_up);
    animate_shaking();
    for (Ent *ent = 0; ent_world_iter(&ent);)
        if (!has_ent_prop(ent, EntProp_Hidden))
            draw(ent);

    end_render();
    world.dt = stm_sec(stm_since(world.last_render));
    world.roll = wrap(world.roll + (f32) world.dt, 1.0);
    world.last_render = stm_now();

    for (int i = 0; i < LEN(input.keys_pressed); i++)
        input.keys_pressed[i] = false;
}

void cleanup(void) {
    sg_shutdown();
}

sapp_desc sokol_main(int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    return (sapp_desc) {
        .init_cb = init,
        .frame_cb = frame,
        .cleanup_cb = cleanup,
        .event_cb = event,
        .width = 1280,
        .height = 720,
        .sample_count = 4,
        .gl_force_gles2 = true,
        .window_title = "sandsphere",
    };
}
