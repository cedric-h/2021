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
} input;



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

typedef struct {
    Vec3 pos, vel, acc;
    Camera camera;
    f32 eye_height;
    Vec2 cam_vel;
} Player;

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
    return add3(plyr->pos,
                mul3f(plyr->camera.rotation.y, plyr->eye_height));
}

/* Returns a view matrix for the player based on his position and camera */
Mat4 player_view(Player *plyr) {
    return mat3_translation4x4(cam_mat3(&plyr->camera), player_eye(plyr));
}

void move_player(Player *plyr) {
    Mat3 cam_dirs = cam_mat3(&plyr->camera),
             axes = plyr->camera.rotation;
    Vec3 move_dir = vec3f(0.0),
             up     = axes.y,
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

    if (input.keys_pressed[(int) SAPP_KEYCODE_SPACE] && mag3(plyr->pos) < 1.001)
        plyr->acc = add3(plyr->acc, mul3f(up, 0.028));

    f32 len = mag3(move_dir);
    if (len > 0.0) {
        /* Normalizing move_dir prevents "two keys for twice the speed" */
        Vec3 norm = div3f(move_dir, len);
        plyr->vel = add3(plyr->vel, mul3f(norm, 0.004f));
    }
}

void update_player(Player *plyr, Vec3 up) {
    rotated_up_indefinite_basis(&plyr->camera.rotation, up);

    update_cam_vel(&plyr->camera, &plyr->cam_vel);
    move_player(plyr);

    f32 dist = mag3(plyr->pos);
    plyr->vel = add3(plyr->vel, mul3f(up, -0.00775 / dist));

    if (dist < 1.0) {
        f32 depth = 1.0 - dist;
        plyr->pos = add3(plyr->pos, mul3f(up, depth));
        plyr->vel = add3(plyr->vel, mul3f(up, depth * 0.01));
    }

    plyr->acc = mul3f(plyr->acc, 0.92f);
    plyr->vel = add3(plyr->vel, plyr->acc);
    plyr->vel = mul3f(plyr->vel, 0.87f);
    plyr->pos = add3(plyr->pos, plyr->vel);
}

typedef enum {
    EntProp_Grows,
    EntProp_COUNT,
} EntProp;

typedef enum {
    Art_Icosahedron,
    Art_Cylinder,
} Art;

typedef struct {
    Art art;
    Vec4 color;

    u64 props[(EntProp_COUNT + 63)/64];
    f32 growth;

    Mat4 mat;
} Ent;

bool has_ent_prop(Ent *ent, EntProp prop) {
    return !!(ent->props[prop/64] & ((u64)1 << (prop%64)));
}

void set_ent_prop(Ent *ent, EntProp prop) {
    ent->props[prop/64] |= ((u64)1 << (prop%64));
}

static struct {
    Player player;
    u64 start_time;
    Ent ents[2000];
    int ent_count;
} state;

void add_ent(Ent ent) {
    state.ents[state.ent_count++] = ent;
}

void spawn_tree(Vec3 pos) {
    Vec3 scale = vec3(0.015f, 0.31f, 0.015f);
    Vec4 color = vec4(0.29f, 0.22f, 0.175f, 0.8f);
    Mat3 bases = ortho_bases3x3(norm3(pos));
    Mat4 m = translate4x4(pos);
    m = mul4x4(m, mat34x4(bases));

    Ent tree = (Ent) {
        .art = Art_Cylinder,
        .color = color
    };
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
        tree.mat = mul4x4(l, scale4x4(lscale));
        add_ent(tree);
        const f32 boffset = 0.095f;
        for (f32 branch = boffset; branch < lscale.y; branch += 0.08f) {
            Mat4 b = mul4x4(l, translate4x4(mul3f(vec3_y(), branch)));
            f32 dist = 1.0 - branch / lscale.y;
            Vec3 bscale = mul3(lscale, vec3(0.5f, dist / 3.4f + 0.12f, 0.5f));
            Mat4 rot = axis_angle4x4(vec3_y(), -PI32 * 0.5f);
            rot = mul4x4(rot, axis_angle4x4(vec3_x(), -PI32 * 0.1f));
            f32 twist = 0.3f;

            Mat4 lb = mul4x4(b, axis_angle4x4(vec3_z(),  PI32 * twist));
            lb = mul4x4(lb, rot);
            tree.mat = mul4x4(lb, scale4x4(bscale));
            add_ent(tree);

            Mat4 rb = mul4x4(b, axis_angle4x4(vec3_z(), -PI32 * twist));
            rb = mul4x4(rb, rot);
            tree.mat = mul4x4(rb, scale4x4(bscale));
            add_ent(tree);

            const f32 leoffset = 0.03f;
            for (f32 dir = -1.0; dir <= 1.0; dir += 2.0)
            for (f32 leaf = leoffset; leaf < bscale.y; leaf += 0.028f) {
                Vec4 color = vec4(0.38f, 0.54f, 0.327f, 1.0f);
                Mat4 pos         = translate4x4(vec3(        0.0f, leaf, 0.0f));

                f32 dist = 1.0 - leaf / bscale.y;
                Vec3 lescale = mul3f(vec3(0.025f, 0.002f, 0.015f), 0.6 + dist * 0.8);
                Mat4 center_leaf = translate4x4(mul3(mul3f(vec3_x(), dir), lescale)),
                     rot = axis_angle4x4(vec3_x(), PI32 * (0.5 + dist * 0.3f));
                rot = mul4x4(rot, axis_angle4x4(vec3_z(), PI32 * dist * 0.1f));
                Mat4 leafter = mul4x4(mul4x4(rot, center_leaf), scale4x4(lescale));

                tree.color = color;
                Mat4 lle = mul4x4(lb, pos);
                tree.mat = mul4x4(lle, leafter);
                add_ent(tree);

                Mat4 rle = mul4x4(rb, pos);
                tree.mat = mul4x4(rle, leafter);
                add_ent(tree);
            }
            tree.color = color;
        }
    }
}

void init(void) {
    state.player = (Player) {
        .pos = vec3(0.0f, 1.0f, 0.0f),
        .vel = vec3f(0.0f),
        .eye_height = 0.75,
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
    };

    init_renderer();

    srand(9);
    state.ent_count = 0;

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

        Ent ent = (Ent) {
            .mat = m,
            .color = color,
            .art = Art_Icosahedron,
        };
        #define SUB_DIRTS 3
        #define DIRT_SCALE 0.13
        if (dirt) {
            Mat3 bases = ortho_bases3x3(pos);
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
    spawn_tree(tree_pos);
    
    stm_setup();
    state.start_time = stm_now();
}

void event(const sapp_event* ev) {
    switch (ev->type) {
        case SAPP_EVENTTYPE_MOUSE_MOVE:
            if (sapp_mouse_locked()) {
                state.player.cam_vel.x += ev->mouse_dx * 0.025f;
                state.player.cam_vel.y += ev->mouse_dy * 0.025f;
            }
            break;
        case SAPP_EVENTTYPE_MOUSE_UP:
            sapp_lock_mouse(true);
            break;
        case SAPP_EVENTTYPE_KEY_DOWN:
            if (!input.keys_down[(int) ev->key_code])
                input.keys_pressed[(int) ev->key_code] = true;
            input.keys_down[(int) ev->key_code] = true;
            break;
        case SAPP_EVENTTYPE_KEY_UP:
            input.keys_down[(int) ev->key_code] = false;
            if (ev->key_code == SAPP_KEYCODE_ESCAPE) {
                sapp_lock_mouse(false);
            }
            break;
        default:
            break;
    }
}

void frame(void) {
    Vec3 planet = vec3f(0.0f);
    Vec3 up = norm3(sub3(state.player.pos, planet));

    update_player(&state.player, up);
    
    start_render(player_view(&state.player));
    for (int i = 0; i < state.ent_count; i++) {
        Ent *ent = &state.ents[i];
        switch (ent->art) {
            case Art_Icosahedron:
                draw(ent->mat, ASSET_ICOSAHEDRON, ent->color);
                break;
            case Art_Cylinder:
                Vec3 bottom = ent->mat.w.xyz,
                        top = mul4x44(ent->mat, vec4(0.0, 1.0, 0.0, 1.0)).xyz,
                     b_side = mul4x44(ent->mat, vec4(1.0, 0.0, 0.0, 1.0)).xyz,
                        hit = line_plane_intersect((LinePlaneIntersect) {
                            .line_origin  = player_eye(&state.player),
                            .line         = cam_mat3(&state.player.camera).z,
                            .plane_origin = bottom,
                            .plane_normal = norm3(sub3(top, bottom)),
                        });
                f32 radius = mag3(sub3(bottom, b_side)),
                      dist = mag3(sub3(hit, bottom));

                if (dist < radius)
                    draw_ghost(ent->mat, ASSET_CYLINDER, ent->color);
                else
                    draw(ent->mat, ASSET_CYLINDER, ent->color);
                break;
            default:
                panic("unknown art");
                break;
        }
    }

    end_render();

    for (int i = 0; i < LEN(input.keys_pressed); i++)
        input.keys_pressed[i] = false;
}

void cleanup(void) {
    sg_shutdown();
}

sapp_desc sokol_main(int argc, char* argv[]) {
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
