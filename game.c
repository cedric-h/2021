//------------------------------------------------------------------------------
//  game.c
//------------------------------------------------------------------------------
#define HANDMADE_MATH_IMPLEMENTATION
#define HANDMADE_MATH_NO_SSE
#include "HandmadeMath.h"

#include "sokol/sokol_app.h"
#include "sokol/sokol_gfx.h"
#include "sokol/sokol_glue.h"
#include "./build/game.glsl.h"
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

static struct {
    sg_pipeline pip;
    sg_bindings bind;
	hmm_vec3 vel, pos;
} state;

static bool keys_down[350];

typedef struct {
	hmm_vec3 pos;
	hmm_vec3 norm;
} Vertex;

Vertex vert_pos(hmm_vec3 pos) {
	return (Vertex){
		.pos = pos,
		.norm = HMM_Vec3(0, 0, 0),
	};
}

#define DETAIL 3
#define SIZE (1 << DETAIL)
#define LEN(arr) (int) (sizeof arr / sizeof arr[0])
typedef struct {
	uint16_t indices[12 * SIZE * SIZE * 5];
	Vertex vertices[12 * SIZE * SIZE];
	uint16_t vert_writer;
} Geometry;

uint16_t vert_insert(Geometry *geo, Vertex vert) {
	for (uint16_t i = 0; i < geo->vert_writer; i++)
		if (HMM_EqualsVec3(geo->vertices[i].pos, vert.pos))
			return i;

	geo->vertices[geo->vert_writer] = vert;
	return geo->vert_writer++;
}

Geometry geometry() {
	Geometry geo;
	geo.vert_writer = 0;
	for (int i = 0; i < 12 * SIZE * SIZE * 5; i++) {
		if (i < LEN(geo.vertices))
			geo.vertices[i] = vert_pos(HMM_Vec3(0.0, 0.0, 0.0));
		geo.indices[i] = 0;
	}

    /* icosahedron vertex buffer */
	float t = 1.0f + sqrtf(5.0f) / 2.0f;
    hmm_vec3 vert_poses[] = {
		HMM_Vec3(-1.0,    t,  0.0),
		HMM_Vec3( 1.0,    t,  0.0),
		HMM_Vec3(-1.0,   -t,  0.0),
		HMM_Vec3( 1.0,   -t,  0.0),
		HMM_Vec3( 0.0, -1.0,    t),
		HMM_Vec3( 0.0,  1.0,    t),
		HMM_Vec3( 0.0, -1.0,   -t),
		HMM_Vec3( 0.0,  1.0,   -t),
		HMM_Vec3(   t,  0.0, -1.0),
		HMM_Vec3(   t,  0.0,  1.0),
		HMM_Vec3(  -t,  0.0, -1.0),
		HMM_Vec3(  -t,  0.0,  1.0),
    };

    /* create an index buffer for the icosahedron */
    uint16_t base_indices[] = {
		 0, 11,  5,
		 0,  5,  1,
		 0,  1,  7,
		 0,  7, 10,
		 0, 10, 11,
		 1,  5,  9,
		 5, 11,  4,
		11, 10,  2,
		10,  7,  6,
		 7,  1,  8,
		 3,  9,  4,
		 3,  4,  2,
		 3,  2,  6,
		 3,  6,  8,
		 3,  8,  9,
		 4,  9,  5,
		 2,  4, 11,
		 6,  2, 10,
		 8,  6,  7,
		 9,  8,  1,
    };
	
	int index_writer = 0;
	size_t tri, x, y, rows;
	hmm_vec3 a, b, c, ay, by, v[SIZE + 1][SIZE + 1];
	for (tri = 0; tri < LEN(base_indices); tri += 3) {
		a = vert_poses[base_indices[tri + 0]],
		b = vert_poses[base_indices[tri + 1]],
		c = vert_poses[base_indices[tri + 2]];
		for (x = 0; x <= SIZE; x++) {
			ay = HMM_LerpVec3(a, (float) x / (float) SIZE, c),
			by = HMM_LerpVec3(b, (float) x / (float) SIZE, c);
			rows = SIZE - x;
			for (y = 0; y <= rows; y++)
				if (y == 0 && x == SIZE)
					v[x][y] = ay;
				else 
					v[x][y] = HMM_LerpVec3(ay, (float) y / (float) rows, by);
		}

		for (x = 0; x < SIZE; x++) {
			for (y = 0; y < 2 * (SIZE - x) - 1; y++) {
				int k = (int) floorf((float) y / 2.0f);
				if (y % 2 == 0) {
					geo.indices[index_writer++] = vert_insert(&geo, vert_pos(v[x    ][k + 1]));
					geo.indices[index_writer++] = vert_insert(&geo, vert_pos(v[x + 1][k    ]));
					geo.indices[index_writer++] = vert_insert(&geo, vert_pos(v[x    ][k    ]));
				} else {
					geo.indices[index_writer++] = vert_insert(&geo, vert_pos(v[x    ][k + 1]));
					geo.indices[index_writer++] = vert_insert(&geo, vert_pos(v[x + 1][k + 1]));
					geo.indices[index_writer++] = vert_insert(&geo, vert_pos(v[x + 1][k    ]));
				}
			}
		}
	}

	for (size_t i = 0; i < LEN(geo.vertices); i++) {
		geo.vertices[i].norm = HMM_NormalizeVec3(geo.vertices[i].pos);
		geo.vertices[i].pos = geo.vertices[i].norm;
	}

	return geo;
}

static struct {
	float pitch_deg, yaw_deg;
	hmm_vec2 turn_vel;
} cam;

hmm_vec2 pos_to_sphere_uv(hmm_vec3 pos) {
	hmm_vec3 p = HMM_NormalizeVec3(pos);
	return HMM_Vec2(
		HMM_PI32 / 2.0f - acosf(p.Y),
		atan2f(p.X, p.Z)
	);
}

hmm_vec3 sphere_uv_to_pos(hmm_vec2 sphere_uv) {
	return HMM_MultiplyVec3f(
		HMM_Vec3(
			sinf(sphere_uv.Y) * cosf(sphere_uv.X),
			sinf(sphere_uv.X),
			cosf(sphere_uv.Y) * cosf(sphere_uv.X)
		),
		1.04f
	);
}

void check_sphere_round_trip(hmm_vec3 pos) {
	hmm_vec2 uv = pos_to_sphere_uv(pos);
	printf("lat %f, lon %f\n", uv.X, uv.Y);

	hmm_vec3 out = sphere_uv_to_pos(uv);
	printf("got x: %f, y: %f, z: %f\n", out.X, out.Y, out.Z);
	printf("expected x: %f, y: %f, z: %f\n", pos.X, pos.Y, pos.Z);
	assert(HMM_LengthVec3(HMM_SubtractVec3(out, pos)) < 0.05);
}

hmm_vec3 cam_facing_side() {
	//check_sphere_round_trip(state.pos);
	hmm_vec2 sphere_uv = pos_to_sphere_uv(state.pos);

	float yaw = HMM_ToRadians(cam.yaw_deg);
	sphere_uv = HMM_AddVec2(sphere_uv, HMM_MultiplyVec2f(HMM_Vec2(cosf(yaw), cosf(yaw)), 0.2f));
	printf("cos yaw: %f, sin yaw: %f\n", cosf(yaw), sinf(yaw));

	return HMM_NormalizeVec3(HMM_SubtractVec3(sphere_uv_to_pos(sphere_uv), state.pos));
}

hmm_vec3 cam_facing() {
	hmm_vec3 p = cam_facing_side();
	//printf("cam facing x: %f, y: %f, z: %f\n", p.X, p.Y, p.Z);
	return p;
}

void turn_cam(float yaw_delta_deg, float pitch_delta_deg) {
	#define WRAP(a, b) (a) > (b) ? (a) - (b) : (a)
	cam.pitch_deg = HMM_Clamp(-89.0f, cam.pitch_deg - pitch_delta_deg, 89.0f);
	cam.yaw_deg = WRAP(cam.yaw_deg - yaw_delta_deg, 360.0f);
}

void init(void) {
	state.vel = HMM_Vec3(0.0, 0.0, 0.0);
	state.pos = HMM_Vec3(0.0, 1.0, 0.0);

    sg_setup(&(sg_desc){
        .context = sapp_sgcontext()
    });
	
	Geometry geo = geometry();
    sg_buffer vbuf = sg_make_buffer(&(sg_buffer_desc){
        .size = sizeof geo.vertices,
        .content = geo.vertices,
        .label = "icosahedron-vertices"
    });
    sg_buffer ibuf = sg_make_buffer(&(sg_buffer_desc){
        .type = SG_BUFFERTYPE_INDEXBUFFER,
        .size = sizeof geo.indices,
        .content = geo.indices,
        .label = "icosahedron-indices"
    });

    /* create shader */
    sg_shader shd = sg_make_shader(cube_shader_desc());

    /* create pipeline object */
    state.pip = sg_make_pipeline(&(sg_pipeline_desc){
        .layout = {
            .attrs = {
                [ATTR_vs_position].format = SG_VERTEXFORMAT_FLOAT3,
                [ATTR_vs_normal].format = SG_VERTEXFORMAT_FLOAT3,
            }
        },
        .shader = shd,
        .index_type = SG_INDEXTYPE_UINT16,
        .depth_stencil = {
            .depth_compare_func = SG_COMPAREFUNC_LESS_EQUAL,
            .depth_write_enabled = true,
        },
        .rasterizer.cull_mode = SG_CULLMODE_BACK,
        .label = "icosahedron-pipeline"
    });

    /* setup resource bindings */
    state.bind = (sg_bindings) {
        .vertex_buffers[0] = vbuf,
        .index_buffer = ibuf
    };

	cam.pitch_deg = 0.0f;
	cam.yaw_deg = 0.0f;
	cam.turn_vel = HMM_Vec2(0.0f, 0.0f);
}

void event(const sapp_event* ev) {
	switch (ev->type) {
		case SAPP_EVENTTYPE_MOUSE_MOVE:
			if (sapp_mouse_locked())
				cam.turn_vel.X += ev->mouse_dx * 0.025f;
				cam.turn_vel.Y += ev->mouse_dy * 0.025f;
			break;
		case SAPP_EVENTTYPE_KEY_DOWN:
			keys_down[(int) ev->key_code] = true;
			break;
		case SAPP_EVENTTYPE_KEY_UP:
			keys_down[(int) ev->key_code] = false;
			break;
		default:
			break;
	}
}

void frame(void) {
	sapp_lock_mouse(true);

	cam.turn_vel = HMM_MultiplyVec2f(cam.turn_vel, 0.9f);
	turn_cam(cam.turn_vel.X, cam.turn_vel.Y);

	hmm_vec3 planet = HMM_Vec3(0.0f, 0.0f, 0.0f);
	hmm_vec3 up = HMM_NormalizeVec3(HMM_SubtractVec3(state.pos, planet));

	hmm_vec3 move_dir = HMM_Vec3(0.0, 0.0, 0.0);
	hmm_vec3 facing = cam_facing_side();
	hmm_vec3 side = HMM_Cross(facing, up);
	if (keys_down[(int) SAPP_KEYCODE_W]) {
		move_dir = HMM_AddVec3(move_dir, facing);
	}
	if (keys_down[(int) SAPP_KEYCODE_S]) {
		move_dir = HMM_SubtractVec3(move_dir, facing);
	}
	if (keys_down[(int) SAPP_KEYCODE_A]) {
		move_dir = HMM_SubtractVec3(move_dir, side);
	}
	if (keys_down[(int) SAPP_KEYCODE_D]) {
		move_dir = HMM_AddVec3(move_dir, side);
	}
	float len = HMM_LengthVec3(move_dir);
	if (len > 0.0) {
		hmm_vec3 norm = HMM_DivideVec3f(move_dir, len);
		state.vel = HMM_AddVec3(state.vel, HMM_MultiplyVec3f(norm, 0.002f));
	}

	state.vel = HMM_MultiplyVec3f(state.vel, 0.95f);
	state.pos = HMM_AddVec3(state.pos, state.vel);
	state.pos = HMM_MultiplyVec3f(HMM_NormalizeVec3(state.pos), 1.04f);
	
    /* NOTE: the vs_params_t struct has been code-generated by the shader-code-gen */
    vs_params_t vs_params;
    const float w = (float) sapp_width();
    const float h = (float) sapp_height();
    hmm_mat4 proj = HMM_Perspective(45.0f, w/h, 0.01f, 10.0f);
    hmm_mat4 view = HMM_LookAt(state.pos, HMM_AddVec3(state.pos, cam_facing()), up);
    vs_params.view_proj = HMM_MultiplyMat4(proj, view);
    vs_params.model = HMM_Translate(planet);
    vs_params.inv_trans_model = HMM_Invert(HMM_Transpose(vs_params.model));

    sg_pass_action pass_action = {
        .colors[0] = { .action = SG_ACTION_CLEAR, .val = {0.17f, 0.02f, 0.22f, 1.0f} }
    };
    sg_begin_default_pass(&pass_action, (int)w, (int)h);
    sg_apply_pipeline(state.pip);
    sg_apply_bindings(&state.bind);
    sg_apply_uniforms(SG_SHADERSTAGE_VS, SLOT_vs_params, &vs_params, sizeof vs_params );
    sg_draw(0, 3840, 1);
    sg_end_pass();
    sg_commit();
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
        .window_title = "Cube (sokol-app)",
    };
}
