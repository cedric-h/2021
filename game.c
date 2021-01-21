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

typedef union {
	struct { hmm_vec3 x, y, z; };
	hmm_vec3 cols[3];
	float nums[3][3];
} Mat3;



/* --------- CAMERA */

typedef struct {
	float pitch_deg, yaw_deg;
	Mat3 rotation;
} Camera;

hmm_vec3 project_plane_vector(hmm_vec3 n, hmm_vec3 bd) {
	return HMM_SubtractVec3(bd, HMM_MultiplyVec3f(n, HMM_DotVec3(bd, n)));
}

/* Rotates the given rotation matrix so that the Y basis vector
	points to `new_y`. The X basis vector is orthogonalized with
	the new Y and old Z basis vector projected onto the Y plane.
*/
void rotated_up_indefinite_basis(Mat3 *rot, hmm_vec3 up) {
    rot->cols[1] = up;
    rot->cols[2] = HMM_NormalizeVec3(project_plane_vector(up, rot->cols[2]));
    rot->cols[0] = HMM_Cross(rot->cols[1], rot->cols[2]);
}

/* Multiplies two Mat3s, returning a new one */
Mat3 mat3_mul(Mat3 a, Mat3 b) {
	Mat3 out;
	int k, r, c;
	for (c = 0; c < 3; ++c)
		for (r = 0; r < 3; ++r) {
			out.nums[c][r] = 0.0f;
			for (k = 0; k < 3; ++k)
				out.nums[c][r] += a.nums[k][r] * b.nums[c][k];
		}
	return out;
}


/* Returns a Mat3 oriented according to the camera's pitch and yaw.
	Does not account for being stuck to the side of a sphere
*/
Mat3 cam_mat3_local(Camera *cam) {
	float pitch = HMM_ToRadians(cam->pitch_deg),
		  yaw   = HMM_ToRadians(cam->yaw_deg  );
	
	hmm_vec3 x = HMM_Vec3(1.0, 0.0, 0.0),
	         y = HMM_Vec3(0.0, 1.0, 0.0),
	         z = HMM_Vec3(0.0, 0.0, 1.0);

	hmm_quaternion q = HMM_MultiplyQuaternion(
		HMM_QuaternionFromAxisAngle(y, yaw),
		HMM_QuaternionFromAxisAngle(x, pitch)
	);

	return (Mat3){
		.x = HMM_MultiplyQuaternionVec3(q, x),
		.y = HMM_MultiplyQuaternionVec3(q, y),
		.z = HMM_MultiplyQuaternionVec3(q, z),
	};
}

/* Returns a Mat3 oriented according to the camera's pitch and yaw,
	which is also rotated according to the camera's rotation matrix
	which accounts for being stuck to the side of a sphere
*/
Mat3 cam_mat3(Camera *cam) {
	return mat3_mul(cam->rotation, cam_mat3_local(cam));
}

hmm_mat4 mat4_from_mat3_and_translation(Mat3 basis_vectors, hmm_vec3 pos) {
	hmm_mat4 Result;
	Result.Elements[0][0] = basis_vectors.x.X;
	Result.Elements[0][1] = basis_vectors.y.X;
	Result.Elements[0][2] = -basis_vectors.z.X;
	Result.Elements[0][3] = 0.0;

	Result.Elements[1][0] = basis_vectors.x.Y;
	Result.Elements[1][1] = basis_vectors.y.Y;
	Result.Elements[1][2] = -basis_vectors.z.Y;
	Result.Elements[1][3] = 0.0;

	Result.Elements[2][0] = basis_vectors.x.Z;
	Result.Elements[2][1] = basis_vectors.y.Z;
	Result.Elements[2][2] = -basis_vectors.z.Z;
	Result.Elements[2][3] = 0.0;

	Result.Elements[3][0] = -HMM_DotVec3(basis_vectors.x, pos);
	Result.Elements[3][1] = -HMM_DotVec3(basis_vectors.y, pos);
	Result.Elements[3][2] = HMM_DotVec3(basis_vectors.z, pos);
	Result.Elements[3][3] = 1.0;
	return Result;
}

/* Applies a rotation directly to the camera, making sure to keep it
	within bounds reasonable for the average human neck.
*/
void turn_cam(Camera *cam, float yaw_delta_deg, float pitch_delta_deg) {
	#define WRAP(a, b) (a) > (b) ? (a) - (b) : (a)
	cam->pitch_deg = HMM_Clamp(-89.0f, cam->pitch_deg + pitch_delta_deg, 89.0f);
	cam->yaw_deg = WRAP(cam->yaw_deg + yaw_delta_deg, 360.0f);
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
hmm_vec3 player_eye(Player *plyr) {
	return HMM_AddVec3(
		plyr->pos,
		HMM_MultiplyVec3f(plyr->camera.rotation.y, plyr->eye_height)
	);
}

/* Returns a view matrix for the player based on his position and camera */
hmm_mat4 player_view(Player *plyr) {
	return mat4_from_mat3_and_translation(cam_mat3(&plyr->camera), player_eye(plyr));
}

void move_player(Player *plyr) {
	Mat3 cam_dirs = cam_mat3(&plyr->camera),
		     axes = plyr->camera.rotation;
	hmm_vec3 move_dir = HMM_Vec3(0.0, 0.0, 0.0),
	         up 	= axes.y,
	         facing = project_plane_vector(axes.y, cam_dirs.z),
	         side   = project_plane_vector(axes.y, cam_dirs.x);
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
		plyr->vel = HMM_AddVec3(plyr->vel, HMM_MultiplyVec3f(norm, 0.004f));
	}
}

void update_player(Player *plyr, hmm_vec3 up) {
	rotated_up_indefinite_basis(&plyr->camera.rotation, up);

	update_cam_vel(&plyr->camera, &plyr->cam_vel);
	move_player(plyr);

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
			.rotation = (Mat3){
				.x = HMM_Vec3(1.0, 0.0, 0.0),
				.y = HMM_Vec3(0.0, 1.0, 0.0),
				.z = HMM_Vec3(0.0, 0.0, 1.0),
			},
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

float randf() {
	return (float) rand() / (float) RAND_MAX;
}

hmm_vec3 rand_vec3() {
	return HMM_NormalizeVec3(HMM_Vec3(0.5 - randf(), 0.5 - randf(), 0.5 - randf()));
}

void frame(void) {
	hmm_vec3 planet = HMM_Vec3(0.0, 0.0, 0.0);
	hmm_vec3 up = HMM_NormalizeVec3(HMM_SubtractVec3(state.player.pos, planet));

	update_player(&state.player, up);
	
	start_render(player_view(&state.player));

	draw(HMM_Translate(planet), ART_SPHERE);
	srand(10);
	for (int i = 0; i < 10; i++)
		draw(HMM_MultiplyMat4(
			HMM_Translate(rand_vec3()),
			HMM_Scale(HMM_Vec3(0.2, 0.2, 0.2)
		)), ART_ICOSAHEDRON);

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
