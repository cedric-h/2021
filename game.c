//------------------------------------------------------------------------------
//  game.c
//------------------------------------------------------------------------------
#define HANDMADE_MATH_IMPLEMENTATION
#define HANDMADE_MATH_NO_SSE
#include "HandmadeMath.h"

#include "sokol/sokol_app.h"
#include "sokol/sokol_gfx.h"
#include "sokol/sokol_glue.h"
#include <stdio.h>
#include <stdlib.h>
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
	float pitch_deg, yaw_deg;
} Camera;

/* Takes a point, `p`, relative to the top (i.e. (0, 1, 0))
	of the sphere, and rotates it so that it is relative
	to `up` instead.
*/
hmm_vec3 spheretop_slide(hmm_vec3 up, hmm_vec3 p) {
	hmm_vec3 y_axis = HMM_Vec3(0.0, 1.0, 0.0);
	return HMM_MultiplyQuaternionVec3(
		HMM_QuaternionBetweenVector3s(y_axis, up),
		HMM_AddVec3(y_axis, p)
	);
}

/* Similar to cam_facing, but only uses the horizontal aspect of the
	camera's orientation.

	This is useful for walking in the direction the camera is facing,
	or any other operation which need not take into account the vertical
	component of the camera's orientation.
*/
hmm_vec3 cam_facing_flat(Camera *cam, hmm_vec3 up) {
	float yaw = HMM_ToRadians(cam->yaw_deg);
	hmm_vec3 look_pos = HMM_Vec3(sinf(yaw), 0.0, cosf(yaw));
	return HMM_SubtractVec3(spheretop_slide(up, look_pos), up);
}

/* Returns a unit vector pointing from the eye of the camera
	toward where the camera is currently oriented.
*/
hmm_vec3 cam_facing(Camera *cam, hmm_vec3 up) {
	float pitch = HMM_ToRadians(cam->pitch_deg),
		  yaw   = HMM_ToRadians(cam->yaw_deg  );

	hmm_vec3 look_pos = HMM_Vec3(
		sinf(yaw) * cosf(pitch),
		sinf(pitch),
		cosf(yaw) * cosf(pitch)
	);
	return HMM_SubtractVec3(spheretop_slide(up, look_pos), up);
}

/* Applies a rotation directly to the camera, making sure to keep it
	within bounds reasonable for the average human neck.
*/
void turn_cam(Camera *cam, float yaw_delta_deg, float pitch_delta_deg) {
	#define WRAP(a, b) (a) > (b) ? (a) - (b) : (a)
	cam->pitch_deg = HMM_Clamp(-89.0f, cam->pitch_deg - pitch_delta_deg, 89.0f);
	cam->yaw_deg = WRAP(cam->yaw_deg - yaw_delta_deg, 360.0f);
}



/* --------- PLAYER */

typedef struct {
	hmm_vec3 pos, vel, acc;
	Camera camera;
	float eye_height;
	hmm_vec2 cam_vel;
} Player;

/* Applies the camera's turning velocity to the camera.
	The camera's turning velocity is also gradually reduced,
	so as to create a smooth sliding effect.
*/
void update_cam_vel(Camera *cam, hmm_vec2 *cam_vel) {
	*cam_vel = HMM_MultiplyVec2f(*cam_vel, 0.9f);
	turn_cam(cam, cam_vel->X, cam_vel->Y);
}

/* Returns a point in world space that the camera can be considered
	to be looking out from.
*/
hmm_vec3 player_eye(Player *plyr, hmm_vec3 up) {
	return HMM_AddVec3(plyr->pos, HMM_MultiplyVec3f(up, plyr->eye_height));
}

void move_player(Player *plyr, hmm_vec3 up) {
	hmm_vec3 move_dir = HMM_Vec3(0.0, 0.0, 0.0),
	         facing = cam_facing_flat(&plyr->camera, up),
	         side = HMM_Cross(facing, up);
	if (input.keys_down[(int) SAPP_KEYCODE_W])
		move_dir = HMM_AddVec3(move_dir, facing);
	if (input.keys_down[(int) SAPP_KEYCODE_S])
		move_dir = HMM_SubtractVec3(move_dir, facing);
	if (input.keys_down[(int) SAPP_KEYCODE_A])
		move_dir = HMM_SubtractVec3(move_dir, side);
	if (input.keys_down[(int) SAPP_KEYCODE_D])
		move_dir = HMM_AddVec3(move_dir, side);

	if (input.keys_pressed[(int) SAPP_KEYCODE_SPACE] && HMM_LengthVec3(plyr->pos) < 1.001)
		plyr->acc = HMM_AddVec3(plyr->acc, HMM_MultiplyVec3f(up, 0.028));

	float len = HMM_LengthVec3(move_dir);
	if (len > 0.0) {
		/* Normalizing move_dir prevents "two keys for twice the speed" */
		hmm_vec3 norm = HMM_DivideVec3f(move_dir, len);
		plyr->vel = HMM_AddVec3(plyr->vel, HMM_MultiplyVec3f(norm, 0.006f));
	}
}

void update_player(Player *plyr, hmm_vec3 up) {
	update_cam_vel(&plyr->camera, &plyr->cam_vel);
	move_player(plyr, up);

	float dist = HMM_LengthVec3(plyr->pos);
	plyr->vel = HMM_AddVec3(plyr->vel, HMM_MultiplyVec3f(up, -0.00775 / dist));

	if (dist < 1.0) {
		float depth = 1.0 - dist;
		plyr->pos = HMM_AddVec3(plyr->pos, HMM_MultiplyVec3f(up, depth));
		plyr->vel = HMM_AddVec3(plyr->vel, HMM_MultiplyVec3f(up, depth * 0.01));
	}

	plyr->acc = HMM_MultiplyVec3f(plyr->acc, 0.92f);
	plyr->vel = HMM_AddVec3(plyr->vel, plyr->acc);
	plyr->vel = HMM_MultiplyVec3f(plyr->vel, 0.87f);
	plyr->pos = HMM_AddVec3(plyr->pos, plyr->vel);
}

static struct {
	Player player;
} state;

void init(void) {
	state.player = (Player){
		.pos = HMM_Vec3(0.0f, 1.0f, 0.0f),
		.vel = HMM_Vec3(0.0f, 0.0f, 0.0f),
		.eye_height = 0.75,
		.cam_vel = HMM_Vec2(0.0f, 0.0f),
		.camera = (Camera){
			.pitch_deg = 0.0f,
			.yaw_deg = 0.0f,
		},
	};

	init_renderer();
}

void event(const sapp_event* ev) {
	switch (ev->type) {
		case SAPP_EVENTTYPE_MOUSE_MOVE:
			if (sapp_mouse_locked()) {
				state.player.cam_vel.X += ev->mouse_dx * 0.025f;
				state.player.cam_vel.Y += ev->mouse_dy * 0.025f;
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
	hmm_vec3 planet = HMM_Vec3(0.0, 0.0, 0.0);
	hmm_vec3 up = HMM_NormalizeVec3(HMM_SubtractVec3(state.player.pos, planet));
	update_player(&state.player, up);
	
	start_render((CameraInfo) {
		.up = up,
		.eye = player_eye(&state.player, up),
		.look = cam_facing(&state.player.camera, up),
	});
	draw(planet, ART_SPHERE);
	draw(HMM_Vec3(1.0, 0.0, 0.0), ART_ICOSAHEDRON);
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
