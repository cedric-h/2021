#include "./build/game.glsl.h"

typedef struct {
    Vec3 pos;
    Vec3 norm;
} Vertex;

INLINE Vertex vert_at(Vec3 pos) {
    return (Vertex) {
        .pos = pos,
        .norm = vec3f(0.0),
    };
}

INLINE bool vert_eq(Vertex a, Vertex b) {
    return mag3(sub3(a.pos, b.pos)) < 0.01;
}

typedef struct {
    u16 start;
    u16 end;
} Asset;

typedef struct {
    Asset asset;
    Mat4 mat;
    Vec4 color;
} Draw;

typedef struct {
    Asset icosahedron;
    Asset cylinder;
} AssetLocations;

static struct {
    sg_pipeline pip;
    sg_pipeline ghost_pip;

    Draw ghosts[20];
    int ghost_count;

    sg_bindings bind;
    vs_params_t vs_params;
    AssetLocations asset_locations;
} renderer;


#define ASSET_ICOSAHEDRON renderer.asset_locations.icosahedron
#define ASSET_CYLINDER renderer.asset_locations.cylinder

typedef struct {
    u16 indices[1000];
    Vertex vertices[500];
    u16 vert_writer;
    u16 index_writer;
} Geometry;

/* If the supplied vertex is already in the stack, this function returns its index.
    Otherwise, the vertex is pushed onto the stack and its new index is returned.

    Note that vertices are useless if no indices point to them; you may want to
    send the output of this function to `insert_tri`.
*/
u16 insert_vert(Geometry *geo, Asset asset, Vertex vert) {
    u16 index = asset.start;
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

void set_smooth_normals(Geometry *geo, Asset asset) {
    for (u16 v, i = asset.start; v = geo->indices[i], i < asset.end; i++)
        geo->vertices[v].norm = vec3f(0.0);

    for (int i = asset.start; i < asset.end; i += 3) {
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

    for (u16 v, i = asset.start; v = geo->indices[i], i < asset.end; i++)
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

    f32 t = 1.0f + sqrtf(5.0f) / 2.0f;
    Vec3 reference_verts[] = {
        vec3(-1.0,    t,  0.0), vec3( 1.0,    t,  0.0), vec3(-1.0,   -t,  0.0),
        vec3( 1.0,   -t,  0.0), vec3( 0.0, -1.0,    t), vec3( 0.0,  1.0,    t),
        vec3( 0.0, -1.0,   -t), vec3( 0.0,  1.0,   -t), vec3(   t,  0.0, -1.0),
        vec3(   t,  0.0,  1.0), vec3(  -t,  0.0, -1.0), vec3(  -t,  0.0,  1.0),
    };
    for (int i = 0; i < LEN(icosa_verts); i++)
        icosa_verts[i] = reference_verts[i];

    /* ---- ICOSAHEDRON */
    ASSET_ICOSAHEDRON.start = geo->index_writer;
    for (int i = 0; i < LEN(icosa_verts); i++)
        insert_vert(geo, ASSET_ICOSAHEDRON, vert_at(norm3(icosa_verts[i])));
    for (int i = 0; i < LEN(icosa_indices); i++)
        insert_tri(geo, icosa_indices[i][0], icosa_indices[i][1], icosa_indices[i][2]);
    ASSET_ICOSAHEDRON.end = geo->index_writer;
    set_smooth_normals(geo, ASSET_ICOSAHEDRON);

    /* ---- CYLINDER */
    ASSET_CYLINDER.start = geo->index_writer;
    #define CYL_VERT(a) insert_vert(geo, ASSET_CYLINDER, vert_at(a))
    u16 fan_center_bottom = CYL_VERT(vec3(0.0, 0.0, 0.0)),
        fan_center_top    = CYL_VERT(vec3(0.0, 1.0, 0.0));
    for (f32 i = 0.0f; i < 10.0f; i++) {
        f32 t0 =  i         / 10.0f * TAU32,
            t1 = (i + 1.0f) / 10.0f * TAU32;
        u16 bottom_l = CYL_VERT(vec3(sinf(t0), 0.0, cosf(t0))),
            bottom_r = CYL_VERT(vec3(sinf(t1), 0.0, cosf(t1))),
               top_l = CYL_VERT(vec3(sinf(t0), 1.0, cosf(t0))),
               top_r = CYL_VERT(vec3(sinf(t1), 1.0, cosf(t1)));
        insert_tri(geo, bottom_l, bottom_r, top_l);
        insert_tri(geo, bottom_r, top_r, top_l);

        insert_tri(geo, top_l, top_r, fan_center_top);
        insert_tri(geo, bottom_l, fan_center_bottom, bottom_r);
    }
    ASSET_CYLINDER.end = geo->index_writer;
    set_smooth_normals(geo, ASSET_CYLINDER);

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
    sg_setup(&(sg_desc) {
        .context = sapp_sgcontext()
    });

    Geometry *geo = geometry();
    sg_buffer vbuf = sg_make_buffer(&(sg_buffer_desc) {
        .size = sizeof geo->vertices[0] * geo->vert_writer,
        .content = geo->vertices,
        .label = "icosahedron-vertices"
    });
    sg_buffer ibuf = sg_make_buffer(&(sg_buffer_desc) {
        .type = SG_BUFFERTYPE_INDEXBUFFER,
        .size = sizeof geo->indices[0] * geo->index_writer,
        .content = geo->indices,
        .label = "icosahedron-indices"
    });
    free(geo);

    /* create shader */
    sg_shader shd = sg_make_shader(cube_shader_desc());

    /* create pipeline object */
    sg_pipeline_desc pip_desc = (sg_pipeline_desc) {
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
    };
    renderer.pip = sg_make_pipeline(&pip_desc);

    pip_desc.label = "ghost-pipeline";
    pip_desc.depth_stencil.depth_write_enabled = false;
    pip_desc.blend = (sg_blend_state) {
        .enabled = true,
        .src_factor_rgb = SG_BLENDFACTOR_SRC_ALPHA, 
        .dst_factor_rgb = SG_BLENDFACTOR_ONE, 
        .src_factor_alpha = SG_BLENDFACTOR_SRC_ALPHA, 
        .dst_factor_alpha = SG_BLENDFACTOR_ONE, 
    };
    renderer.ghost_pip = sg_make_pipeline(&pip_desc);

    /* setup resource bindings */
    renderer.bind = (sg_bindings) {
        .vertex_buffers[0] = vbuf,
        .index_buffer = ibuf
    };
}

void start_render(Mat4 view) {
    renderer.ghost_count = 0;

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

void draw(Mat4 mat, Asset asset, Vec4 color) {
    renderer.vs_params.model = mat;
    renderer.vs_params.inv_trans_model = transpose4x4(invert4x4(mat));
    renderer.vs_params.color = color;
    sg_apply_uniforms(SG_SHADERSTAGE_VS,
                      SLOT_vs_params,
                      &renderer.vs_params,
                      sizeof renderer.vs_params);

    sg_draw(asset.start, asset.end - asset.start, 1);
}

void draw_ghost(Mat4 mat, Asset asset, Vec4 color) {
    renderer.ghosts[renderer.ghost_count++] = (Draw) {
        .mat = mat,
        .asset = asset,
        .color = color,
    };
}

void end_render() {
    sg_apply_pipeline(renderer.ghost_pip);
    sg_apply_bindings(&renderer.bind);
    Draw *d;
    for (int i = 0; d = &renderer.ghosts[i], i < renderer.ghost_count; i++)
        draw(d->mat, d->asset, d->color);

    sg_end_pass();
    sg_commit();
}
