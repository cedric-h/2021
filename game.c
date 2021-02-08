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
    FallPattern_Flat,
    FallPattern_Leaf,
} FallPattern;

#define MAX_ENT_CHILDREN 50
typedef struct Ent Ent;
struct Ent {
    /* specifies which of the following field groups are valid */
    u64 props[(EntProp_COUNT + 63)/64];

    /* player interaction */
    f64 shake_timer;
    Mat4 before_shake;

    /* used to detach children from parents */
    u8 prune_tag;

    /* appearance */
    Art art;
    Vec4 color;

    /* positioning */
    Ent *parent, *first_child, *last_child, *next, *prev;
    Mat4 parent_mat;
    Mat4 mat;

    /* physics & gravity: EntProp_Phys */
    Vec3 vel, acc;
    f32 mass;
    FallPattern fall_pattern;
};

INLINE Ent default_ent(void) {
    return (Ent) {
        .art = Art_Icosahedron,
        .color = vec4(1.0, 0.0, 1.0, 1.0),
        .parent_mat = ident4x4(),
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

Mat4 get_ent_parent_mat(Ent *ent) {
    if (ent->parent == NULL) return ident4x4();
    if (ent->parent == ent) panic("auto-parenting is prohibited");

    return mul4x4(get_ent_parent_mat(ent->parent), ent->parent->parent_mat);
}

INLINE Mat4 get_ent_mat(Ent *ent) {
    Mat4 m = mul4x4(ent->parent_mat, ent->mat);
    if (ent->parent)
        m = mul4x4(get_ent_parent_mat(ent), m);
    return m;
}

INLINE void add_ent_child(Ent *parent, Ent* child) {
    child->parent = parent;
    child->prev = parent->last_child;
    child->next = NULL;
    if (parent->last_child) parent->last_child->next = child;
    else parent->first_child = child;

    parent->last_child = child;
}

/* Similar to ent_tree_iter, but not recursive. */
INLINE Ent *ent_child_iter(Ent *node) {
    if (node == NULL) return NULL;
    return node->next;
}

/* Used to iterate through an ent's children, recursively */
INLINE Ent *ent_tree_iter(Ent *node) {
    if (node == NULL) return NULL;
    if (node->first_child) return node->first_child;

    while (node && node->next == NULL)
        node = node->parent;
    
    if (node) return node->next;
    return NULL;
}

/* NOTE: positioning assumes child is actually a child of parent */
INLINE void detach_child(Ent *child) {
    for (Ent *c; c = child->first_child;)
        detach_child(c);

    child->mat = get_ent_mat(child);
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
    give_ent_prop(child, EntProp_Phys);
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
    u64 equip_start;
    bool equip_grabbed;
    Vec3 equip_grab_start_pos;

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

void control_player(Player *plyr, Vec3 up) {
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
        m = mul4x4(m, scale4x4(vec3f(scale)));
        m = mul4x4(m, axis_angle4x4(rand3(), randf() * PI32 * 2.0f));

        Ent ent = default_ent();
        ent.mat = m;
        ent.color = color;
        ent.art = Art_Icosahedron;
        #define SUB_DIRTS 3
        #define DIRT_SCALE 0.13
        if (dirt) {
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

    Ent tree = default_ent();
    tree.art = Art_Cylinder;
    tree.color = color;
    tree.mass = 5.3;
    tree.mat = mul4x4(m, scale4x4(scale));
    add_ent(tree);

    const f32 main_limbs[] = { 0.11f, 0.1f, 0.4f, 0.09f, 0.3f, };
    f32 a = 0.0f;
    for (int i = 0; i < LEN(main_limbs); a += main_limbs[i], i++) {
        f32 af = main_limbs[i];
        Mat4 l = mul4x4(m, translate4x4(mul3(vec3_y(), scale)));
        l = mul4x4(l, axis_angle4x4(vec3_y(), a * PI32 * 2.0f));
        l = mul4x4(l, axis_angle4x4(vec3_x(), 1.2f - af * 1.6f));
        Vec3 lscale = vec3(scale.x * (0.5f + af),
                           scale.y * af + 0.2f,
                           scale.z * (0.5f + af));
        Ent *limb = add_ent(tree);
        limb->parent_mat = l;
        limb->mat = scale4x4(lscale);
        limb->mass = 5.0 + 1.0 * randf();
        give_ent_prop(limb, EntProp_Selectable);
        give_ent_prop(limb, EntProp_Whackable);

        const f32 boffset = 0.095f;
        for (f32 branch = boffset; branch < lscale.y; branch += 0.08f) {
            Mat4 b = translate4x4(mul3f(vec3_y(), branch));
            f32 dist = 1.0 - branch / lscale.y;
            Vec3 bscale = mul3(lscale, vec3(0.5f, dist / 3.4f + 0.12f, 0.5f));
            Mat4 rot = axis_angle4x4(vec3_y(), -PI32 * 0.5f);
            rot = mul4x4(rot, axis_angle4x4(vec3_x(), -PI32 * 0.1f));
            f32 twist = 0.3f;

            Mat4 lb = mul4x4(b, axis_angle4x4(vec3_z(),  PI32 * twist));
            lb = mul4x4(lb, rot);
            Ent *branch_l = add_ent(tree);
            branch_l->mat = mul4x4(lb, scale4x4(bscale));
            branch_l->prune_tag = 2;
            branch_l->parent = limb;
            branch_l->mass = 5.0 + 1.0 * randf();
            add_ent_child(limb, branch_l);

            Mat4 rb = mul4x4(b, axis_angle4x4(vec3_z(), -PI32 * twist));
            rb = mul4x4(rb, rot);
            Ent *branch_r = add_ent(tree);
            branch_r->mat = mul4x4(rb, scale4x4(bscale));
            branch_r->prune_tag = 2;
            branch_r->parent = limb;
            branch_r->mass = 5.0 + 1.0 * randf();
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
                Mat4 leafter = mul4x4(mul4x4(rot, center_leaf), scale4x4(lescale));

                Ent leaf_ent = tree;
                leaf_ent.prune_tag = 1;
                leaf_ent.mass = 0.6 + 0.4 * randf();
                leaf_ent.color = vec4(0.38f, 0.54f, 0.327f, 1.0f);
                leaf_ent.fall_pattern = FallPattern_Leaf;

                Ent *leaf_l = add_ent(leaf_ent);
                leaf_l->mat = mul4x4(mul4x4(lb, pos), leafter);
                add_ent_child(branch_l, leaf_l);

                Ent *leaf_r = add_ent(leaf_ent);
                leaf_r->mat = mul4x4(mul4x4(rb, pos), leafter);
                add_ent_child(branch_r, leaf_r);
            }
        }
    }
}

void init(void) {
    init_renderer();

    srand(9);
    world.ent_count = 0;

    Ent player_ent = default_ent();
    player_ent.mat = translate4x4(vec3(0.0f, 1.0f, 0.0f));
    player_ent.mass = 16.0f;
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
        default:
            break;
    }
}

/* finds a limb that's under the mouse, gives it EntProp_Selected */
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

void prune_limb(Ent *limb, u8 tag, f32 percent) {
    for (Ent *child = limb; child;)
        if (child->prune_tag == tag && randf() < percent) {
            Ent *cached_next = child->next ? child->next : child->parent;
            detach_child(child);
            child = cached_next;
        } else
            child = ent_tree_iter(child);
}

void grab_limbs(void) {
    if (world.player.grabbed == NULL && world.player.equipped == NULL) {
        for (Ent *ent = 0; ent_world_iter(&ent);)
            take_ent_prop(ent, EntProp_Selected);
        Ent *limb = select_limb(EntProp_Selectable, 4.0);

        if (limb) {
            give_ent_prop(limb, EntProp_Selected);

            for (Ent *c = limb; c; c = ent_tree_iter(c))
                give_ent_prop(c, EntProp_Selected);

            if (input.left_mb_down)
                if (world.player.grabbed != limb) {
                    world.player.grabbed = limb;
                    limb->before_shake = limb->parent_mat;
                }
        }
    }
    if (world.player.grabbed)
        if (!input.left_mb_down)
            world.player.grabbed = NULL;
        else {
            Ent *grab = world.player.grabbed;
            if (grab->shake_timer > 1.5) {
                take_ent_prop(grab, EntProp_Selected);

                for (Ent *c = grab; c; c = ent_tree_iter(c))
                    take_ent_prop(c, EntProp_Selected);
                grab->shake_timer = 0.0;
                prune_limb(world.player.grabbed, 1, 0.2);
                take_ent_prop(grab, EntProp_Whackable);

                /* equip what's grabbed */
                world.player.equip_grab_start_pos = grab->parent_mat.w.xyz;
                world.player.equipped = world.player.grabbed;
                world.player.grabbed = NULL;
                world.player.equip_start = stm_now();
                world.player.equip_grabbed = true;
            } else {
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
            ent->parent_mat = mul4x4(ent->before_shake, spin);
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
        default:
            panic("unknown art");
            break;
    }
    if (has_ent_prop(ent, EntProp_Selected))
        draw_ghost_asset(mat, asset, ent->color);

    draw_asset(mat, asset, ent->color);
}

#define SWING_DUR 1.0f
void animate_equipped(Player *plyr, Vec3 up, f32 swing_d) {
    Ent *equip = plyr->equipped;
    Mat4 *mat = &equip->parent_mat;
    f64 start_d = stm_sec(stm_since(plyr->equip_start));

    if (plyr->equip_grabbed) {
        f32 t = start_d / 0.7f;
        Vec3 start = plyr->equip_grab_start_pos,
              neck = sub3(player_eye(plyr), mul3f(up, 0.6f));
        mat->w.xyz = lerp3(start, ease_in_expo(t), neck);
        if (t >= 1.0f) {
            plyr->equip_grabbed = false;
            plyr->equip_start = stm_now();
        }
        return;
    }
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

void frame(void) {
    Vec3 planet = vec3f(0.0f);
    f32 planet_mass = 100.0;
    Vec3 player_up = norm3(sub3(world.player.ent->mat.w.xyz, planet));

    grab_limbs();
    control_player(&world.player, player_up);
    if (player_equipped(&world.player)) {
        if (input.left_mb_down && !world.player.swing && !world.player.equip_grabbed) {
            world.player.swing_start = stm_now();
            world.player.swing = SwingState_PreAct;
        }

        f32 swing_d = stm_sec(stm_since(world.player.swing_start));
        animate_equipped(&world.player, player_up, swing_d);
        if (world.player.swing == SwingState_PreAct && swing_d > 0.61) {
            world.player.swing = SwingState_PostAct;

            Ent *hit = select_limb(EntProp_Whackable, 5.5);
            if (hit) {
                int child_count = 0;
                for (Ent *c = hit; c; c = ent_tree_iter(c))
                    child_count++;

                if (child_count < 10) {
                    detach_child(hit);
                    hit->parent_mat = ident4x4();
                } else {
                    prune_limb(world.player.equipped, 1, 0.5);
                    prune_limb(               hit, 1, 0.5);
                    prune_limb(world.player.equipped, 2, 0.3);
                    prune_limb(               hit, 2, 0.3);
                    hit->before_shake = hit->parent_mat;
                    hit->shake_timer = 0.75;
                }
            } else
                prune_limb(world.player.equipped, 1, 0.2);
        }
        if (swing_d >= SWING_DUR)
            world.player.swing = SwingState_NoSwing;
    }

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
            } else if (ent->fall_pattern == FallPattern_Leaf) {
                f32 spiral = 0.000008f * fmaxf(world.roll, 1.0);
                ent->vel = add3(ent->vel, mul3f(bases.x, sinf(world.roll) * spiral));
                ent->vel = add3(ent->vel, mul3f(bases.z, cosf(world.roll) * spiral));
            }

            ent->acc = mul3f(ent->acc, 0.92f);
            ent->vel = add3(ent->vel, ent->acc);
            ent->vel = mul3f(ent->vel, 0.87f);
            ent->mat.w.xyz = add3(ent->mat.w.xyz, ent->vel);
        }

    start_render(player_view(&world.player));
    animate_shaking();
    for (Ent *ent = 0; ent_world_iter(&ent);)
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
