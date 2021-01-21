#include "./build/game.glsl.h"

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

typedef struct {
	uint16_t sphere[2];
	uint16_t icosahedron[2];
} ArtLocations;

#define ART_SPHERE renderer.art_locations.sphere
#define ART_ICOSAHEDRON renderer.art_locations.icosahedron

#define DETAIL 4
#define SIZE (1 << DETAIL)
#define LEN(arr) (int) (sizeof arr / sizeof arr[0])
typedef struct {
	uint16_t indices[SIZE * SIZE * SIZE * 4];
	Vertex vertices[SIZE * SIZE * SIZE];
	uint16_t vert_writer;
	uint16_t index_writer;
	ArtLocations art_locations;
} Geometry;

/* If the supplied vertex is already in the stack, this function returns its index.
	Otherwise, the vertex is pushed onto the stack and its new index is returned.

	Note that vertices are useless if no indices point to them; you may want to
	send the output of this function to `insert_index`.
*/
uint16_t insert_vert(Geometry *geo, Vertex vert) {
	for (uint16_t i = 0; i < geo->vert_writer; i++)
		if (HMM_EqualsVec3(geo->vertices[i].pos, vert.pos))
			return i;

	if (LEN(geo->vertices) - 1 < geo->vert_writer)
		printf(
			"ran out of vertex space! had: %d, need: %d\n",
			LEN(geo->vertices),
			geo->vert_writer
		);

	geo->vertices[geo->vert_writer] = vert;
	return geo->vert_writer++;
}

/* Pushes an index pointing to the given vertex onto the index stack. */
void insert_index(Geometry *geo, uint16_t index) {
	if (LEN(geo->indices) - 1 < geo->index_writer)
		printf(
			"ran out of index space! had: %d, need: %d\n",
			LEN(geo->indices),
			geo->index_writer
		);

	geo->indices[geo->index_writer++] = index;
}


Geometry *geometry() {
	Geometry *geo = malloc(sizeof(Geometry));
	geo->vert_writer = 0;
	geo->index_writer = 0;

	for (int i = 0; i < LEN(geo->indices); i++) {
		if (i < LEN(geo->vertices))
			geo->vertices[i] = vert_pos(HMM_Vec3(0.0, 0.0, 0.0));
		geo->indices[i] = 0;
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

	/* ---- ICOSAHEDRON */
	geo->art_locations.icosahedron[0] = geo->index_writer;
	for (int i = 0; i < LEN(vert_poses); i++)
		insert_vert(geo, vert_pos(vert_poses[i]));
	for (int i = 0; i < LEN(base_indices); i++)
		insert_index(geo, base_indices[i]);
	geo->art_locations.icosahedron[1] = geo->index_writer;

	/* ---- SPHERE */
	geo->art_locations.sphere[0] = geo->index_writer;
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
					insert_index(geo, insert_vert(geo, vert_pos(v[x    ][k + 1])));
					insert_index(geo, insert_vert(geo, vert_pos(v[x + 1][k    ])));
					insert_index(geo, insert_vert(geo, vert_pos(v[x    ][k    ])));
				} else {
					insert_index(geo, insert_vert(geo, vert_pos(v[x    ][k + 1])));
					insert_index(geo, insert_vert(geo, vert_pos(v[x + 1][k + 1])));
					insert_index(geo, insert_vert(geo, vert_pos(v[x + 1][k    ])));
				}
			}
		}
	}
	geo->art_locations.sphere[1] = geo->index_writer;

	for (size_t i = 0; i < geo->vert_writer; i++) {
		geo->vertices[i].norm = HMM_NormalizeVec3(geo->vertices[i].pos);
		geo->vertices[i].pos = geo->vertices[i].norm;
	}

	printf(
		"used [%d/%d] indices (%f%%)\n",
		geo->index_writer,
		LEN(geo->indices), 
		100.0f * (float) geo->index_writer / (float) LEN(geo->indices)
	);

	printf(
		"used [%d/%d] vertices (%f%%)\n",
		geo->vert_writer,
		LEN(geo->vertices), 
		100.0f * (float) geo->vert_writer / (float) LEN(geo->vertices)
	);

	return geo;
}

static struct {
    sg_pipeline pip;
    sg_bindings bind;
    vs_params_t vs_params;
	ArtLocations art_locations;
} renderer;

void init_renderer() {
    sg_setup(&(sg_desc){
        .context = sapp_sgcontext()
    });

	Geometry *geo = geometry();
    sg_buffer vbuf = sg_make_buffer(&(sg_buffer_desc){
        .size = sizeof geo->vertices[0] * geo->vert_writer,
        .content = geo->vertices,
        .label = "icosahedron-vertices"
    });
    sg_buffer ibuf = sg_make_buffer(&(sg_buffer_desc){
        .type = SG_BUFFERTYPE_INDEXBUFFER,
        .size = sizeof geo->indices[0] * geo->index_writer,
        .content = geo->indices,
        .label = "icosahedron-indices"
    });
	renderer.art_locations = geo->art_locations;
	free(geo);

    /* create shader */
    sg_shader shd = sg_make_shader(cube_shader_desc());

    /* create pipeline object */
    renderer.pip = sg_make_pipeline(&(sg_pipeline_desc){
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
        .rasterizer = {
			.cull_mode = SG_CULLMODE_BACK,
		},
        .label = "icosahedron-pipeline"
    });

    /* setup resource bindings */
    renderer.bind = (sg_bindings) {
        .vertex_buffers[0] = vbuf,
        .index_buffer = ibuf
    };
}

void start_render(hmm_mat4 view) {
    /* NOTE: the vs_params_t struct has been code-generated by the shader-code-gen */
    vs_params_t vs_params;
    const float w = (float) sapp_width();
    const float h = (float) sapp_height();
    hmm_mat4 proj = HMM_Perspective(45.0f, w/h, 0.01f, 10.0f);
    renderer.vs_params.view_proj = HMM_MultiplyMat4(proj, view);

    sg_pass_action pass_action = {
        .colors[0] = { .action = SG_ACTION_CLEAR, .val = {0.17f, 0.02f, 0.22f, 1.0f} }
    };
    sg_begin_default_pass(&pass_action, (int)w, (int)h);
    sg_apply_pipeline(renderer.pip);
    sg_apply_bindings(&renderer.bind);
}

void draw(hmm_mat4 mat, uint16_t art_location[2]) {
	renderer.vs_params.model = mat;
	renderer.vs_params.inv_trans_model = HMM_Invert(HMM_Transpose(mat));
    sg_apply_uniforms(
		SG_SHADERSTAGE_VS,
		SLOT_vs_params,
		&renderer.vs_params,
		sizeof renderer.vs_params
	);

	//printf("start: %d, end: %d\n", (int) art_location[0], (int) art_location[1]);
	sg_draw(art_location[0], art_location[1], 1);
}

void end_render() {
    sg_end_pass();
    sg_commit();
}
