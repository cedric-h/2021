#include "./build/game.glsl.h"

typedef struct {
	Vec3 pos;
	Vec3 norm;
} Vertex;

INLINE Vertex vert_at(Vec3 pos) {
	return (Vertex){
		.pos = pos,
		.norm = vec3f(0.0),
	};
}

INLINE bool vert_eq(Vertex a, Vertex b) {
	return mag3(sub3(a.pos, b.pos)) < 0.01;
}

typedef struct {
	u16 sphere[2];
	u16 icosahedron[2];
	u16 cylinder[2];
} ArtLocations;

static struct {
    sg_pipeline pip;
    sg_bindings bind;
    vs_params_t vs_params;
	ArtLocations art_locations;
} renderer;


#define ART_SPHERE renderer.art_locations.sphere
#define ART_ICOSAHEDRON renderer.art_locations.icosahedron
#define ART_CYLINDER renderer.art_locations.cylinder

#define DETAIL 4
#define SIZE (1 << DETAIL)
#define LEN(arr) (int) (sizeof arr / sizeof arr[0])
typedef struct {
	u16 indices[SIZE * SIZE * SIZE * 4];
	Vertex vertices[SIZE * SIZE * SIZE];
	u16 vert_writer;
	u16 index_writer;
} Geometry;

/* If the supplied vertex is already in the stack, this function returns its index.
	Otherwise, the vertex is pushed onto the stack and its new index is returned.

	Note that vertices are useless if no indices point to them; you may want to
	send the output of this function to `insert_tri`.
*/
u16 insert_vert(Geometry *geo, u16 index, Vertex vert) {
	for (; index < geo->index_writer; index++)
		if (vert_eq(geo->vertices[geo->indices[index]], vert))
			return geo->indices[index];

	if (geo->vert_writer > LEN(geo->vertices) - 1)
		printf("ran out of vertex space! had: %d, need: %d\n",
    		   LEN(geo->vertices),
    		   geo->vert_writer);

	geo->vertices[geo->vert_writer] = vert;
	return geo->vert_writer++;
}

/* Pushes indexes pointing to the given vertices onto the index stack. */
void insert_tri(Geometry *geo, u16 i0, u16 i1, u16 i2) {
	if ((geo->index_writer + 3) > LEN(geo->indices))
		printf("ran out of index space! had: %d, need: %d\n",
			   LEN(geo->indices),
			   geo->index_writer + 3);

	geo->indices[geo->index_writer++] = i0;
	geo->indices[geo->index_writer++] = i1;
	geo->indices[geo->index_writer++] = i2;
}

void set_smooth_normals(Geometry *geo, u16 start_index, u16 end_index) {
	for (u16 v, i = start_index; v = geo->indices[i], i < end_index; i++)
		geo->vertices[v].norm = vec3f(0.0);

	for (int i = start_index; i < end_index; i += 3) {
		Vertex *v0 = &geo->vertices[geo->indices[i  ]],
			   *v1 = &geo->vertices[geo->indices[i+1]],
			   *v2 = &geo->vertices[geo->indices[i+2]];
		Vec3 edge0 = sub3(v1->pos, v0->pos),
		     edge1 = sub3(v2->pos, v0->pos),
			  norm = cross3(edge0, edge1);
		v0->norm = add3(v0->norm, norm);
		v1->norm = add3(v1->norm, norm);
		v2->norm = add3(v2->norm, norm);
	}

	for (u16 v, i = start_index; v = geo->indices[i], i < end_index; i++)
		geo->vertices[v].norm = norm3(geo->vertices[v].norm);
}


Geometry *geometry() {
	Geometry *geo = malloc(sizeof(Geometry));
	geo->vert_writer = 0;
	geo->index_writer = 0;

	for (int i = 0; i < LEN(geo->indices); i++) {
		if (i < LEN(geo->vertices))
			geo->vertices[i] = vert_at(vec3(0.0, 0.0, 0.0));
		geo->indices[i] = 0;
	}

    /* icosahedron vertex buffer */
	f32 t = 1.0f + sqrtf(5.0f) / 2.0f;
    Vec3 vert_poses[] = {
		vec3(-1.0,    t,  0.0),
		vec3( 1.0,    t,  0.0),
		vec3(-1.0,   -t,  0.0),
		vec3( 1.0,   -t,  0.0),
		vec3( 0.0, -1.0,    t),
		vec3( 0.0,  1.0,    t),
		vec3( 0.0, -1.0,   -t),
		vec3( 0.0,  1.0,   -t),
		vec3(   t,  0.0, -1.0),
		vec3(   t,  0.0,  1.0),
		vec3(  -t,  0.0, -1.0),
		vec3(  -t,  0.0,  1.0),
    };

    /* create an index buffer for the icosahedron */
    u16 base_indices[] = {
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
	ART_ICOSAHEDRON[0] = geo->index_writer;
	for (int i = 0; i < LEN(vert_poses); i++)
		insert_vert(geo, ART_ICOSAHEDRON[0], vert_at(norm3(vert_poses[i])));
	for (int i = 0; i < LEN(base_indices); i += 3)
		insert_tri(geo, base_indices[i], base_indices[i+1], base_indices[i+2]);
	ART_ICOSAHEDRON[1] = geo->index_writer;
	set_smooth_normals(geo, ART_ICOSAHEDRON[0], ART_ICOSAHEDRON[1]);

	/* ---- CYLINDER */
	ART_CYLINDER[0] = geo->index_writer;
	#define CYL_VERT(a) insert_vert(geo, ART_CYLINDER[0], vert_at(a))
	u16 fan_center_bottom = CYL_VERT(vec3(0.0, 0.0, 0.0)),
	    fan_center_top    = CYL_VERT(vec3(0.0, 1.0, 0.0));
	for (f32 i = 0.0f; i < 10.0f; i++) {
		f32 t0 =  i         / 10.0f * TAU32,
		    t1 = (i + 1.0f) / 10.0f * TAU32;
		u16 bottom_l = CYL_VERT(vec3(sinf(t0) / 2.0, 0.0, cosf(t0) / 2.0)),
		    bottom_r = CYL_VERT(vec3(sinf(t1) / 2.0, 0.0, cosf(t1) / 2.0)),
			   top_l = CYL_VERT(vec3(sinf(t0) / 2.0, 1.0, cosf(t0) / 2.0)),
			   top_r = CYL_VERT(vec3(sinf(t1) / 2.0, 1.0, cosf(t1) / 2.0));
		insert_tri(geo, bottom_l, bottom_r, top_l);
		insert_tri(geo, bottom_r, top_r, top_l);

		insert_tri(geo, top_l, top_r, fan_center_top);
		insert_tri(geo, bottom_l, fan_center_bottom, bottom_r);
	}
	ART_CYLINDER[1] = geo->index_writer;
	set_smooth_normals(geo, ART_CYLINDER[0], ART_CYLINDER[1]);

	/* ---- SPHERE */
	ART_SPHERE[0] = geo->index_writer;
	size_t tri, x, y, rows;
	Vec3 a, b, c, ay, by, v[SIZE + 1][SIZE + 1];
	for (tri = 0; tri < LEN(base_indices); tri += 3) {
		a = vert_poses[base_indices[tri + 0]],
		b = vert_poses[base_indices[tri + 1]],
		c = vert_poses[base_indices[tri + 2]];
		for (x = 0; x <= SIZE; x++) {
			ay = lerp3(a, (f32) x / (f32) SIZE, c),
			by = lerp3(b, (f32) x / (f32) SIZE, c);
			rows = SIZE - x;
			for (y = 0; y <= rows; y++)
				if (y == 0 && x == SIZE)
					v[x][y] = ay;
				else 
					v[x][y] = lerp3(ay, (f32) y / (f32) rows, by);
		}

		for (x = 0; x < SIZE; x++) {
			for (y = 0; y < 2 * (SIZE - x) - 1; y++) {
				int k = (int) floorf((f32) y / 2.0f);
				if (y % 2 == 0) {
					insert_tri(geo, insert_vert(geo, ART_SPHERE[0], vert_at(v[x    ][k + 1])),
					                insert_vert(geo, ART_SPHERE[0], vert_at(v[x + 1][k    ])),
					                insert_vert(geo, ART_SPHERE[0], vert_at(v[x    ][k    ])));
				} else {
					insert_tri(geo, insert_vert(geo, ART_SPHERE[0], vert_at(v[x    ][k + 1])),
					                insert_vert(geo, ART_SPHERE[0], vert_at(v[x + 1][k + 1])),
					                insert_vert(geo, ART_SPHERE[0], vert_at(v[x + 1][k    ])));
				}
			}
		}
	}
	ART_SPHERE[1] = geo->index_writer;

	for (int s = ART_SPHERE[0]; s < ART_SPHERE[1]; s++) {
		u16 i = geo->indices[s];
		geo->vertices[i].norm = norm3(geo->vertices[i].pos);
		geo->vertices[i].pos = geo->vertices[i].norm;
	}

	printf("used [%d/%d] indices (%f%%)\n",
		   geo->index_writer,
		   LEN(geo->indices), 
		   100.0f * (f32) geo->index_writer / (f32) LEN(geo->indices));

	printf("used [%d/%d] vertices (%f%%)\n",
		   geo->vert_writer,
		   LEN(geo->vertices), 
		   100.0f * (f32) geo->vert_writer / (f32) LEN(geo->vertices));

	return geo;
}

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

void start_render(Mat4 view) {
    /* NOTE: the vs_params_t struct has been code-generated by the shader-code-gen */
    vs_params_t vs_params;
    const f32 w = (f32) sapp_width();
    const f32 h = (f32) sapp_height();
    Mat4 proj = perspective4x4(45.0f, w/h, 0.01f, 10.0f);
    renderer.vs_params.view_proj = mul4x4(proj, view);

    sg_pass_action pass_action = {
        .colors[0] = { .action = SG_ACTION_CLEAR, .val = {0.17f, 0.02f, 0.22f, 1.0f} }
    };
    sg_begin_default_pass(&pass_action, (int)w, (int)h);
    sg_apply_pipeline(renderer.pip);
    sg_apply_bindings(&renderer.bind);
}

void draw(Mat4 mat, u16 art_location[2]) {
	renderer.vs_params.model = mat;
	renderer.vs_params.inv_trans_model = invert4x4(transpose4x4(mat));
    sg_apply_uniforms(SG_SHADERSTAGE_VS,
					  SLOT_vs_params,
					  &renderer.vs_params,
		              sizeof renderer.vs_params);

	sg_draw(art_location[0], art_location[1] - art_location[0], 1);
}

void end_render() {
    sg_end_pass();
    sg_commit();
}
