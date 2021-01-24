//------------------------------------------------------------------------------
//  game.c
//------------------------------------------------------------------------------
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
    return (Mat3){
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

static struct {
    Player player;
    u64 start;
} state;

void init(void) {
    state.player = (Player){
        .pos = vec3(0.0f, 1.0f, 0.0f),
        .vel = vec3f(0.0f),
        .eye_height = 0.75,
        .cam_vel = vec2f(0.0f),
        .camera = (Camera){
            .rotation = (Mat3){
                .x = vec3_x(),
                .y = vec3_y(),
                .z = vec3_z(),
            },
            .pitch_deg = 0.0f,
            .yaw_deg = 0.0f,
        },
    };

    init_renderer();

    stm_setup();
    state.start = stm_now();
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

    draw(translate4x4(planet), ART_SPHERE);
    srand(10);
    for (int i = 0; i < 10; i++) {
        /* random location on planet surface */
        Vec3 p = rand3();

        /* small icosahedron at that location */
        draw(mul4x4(translate4x4(p), scale4x4(vec3f(0.2f))),
             ART_ICOSAHEDRON);

        /* let's start .5 units above the icosahedron */
        Mat4 m = scale4x4(vec3f(1.5f));
        m = mul4x4(m, translate4x4(p));

        /* same size as icosahedron (cancel out the planet scale) */
        m = mul4x4(m, scale4x4(vec3f(0.2f / 1.5f)));

        /* gentle spin over time across random axis */
        f64 time = stm_sec(stm_since(state.start));
        Mat4 rotation = axis_angle4x4(rand3(), time);
        /* relative to center of cylinder, not bottom */
        m = mul4x4(m, pivot4x4(rotation, mul3f(vec3_y(), 0.5f)));

        draw(m, ART_CYLINDER);
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

    return (sapp_desc){
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
