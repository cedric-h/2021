//------------------------------------------------------------------------------
//  cube.zig
//
//  Shader with uniform data.
//------------------------------------------------------------------------------
const sg = @import("sokol").gfx;
const sapp = @import("sokol").app;
const sgapp = @import("sokol").app_gfx_glue;
usingnamespace @import("math.zig");

const state = struct {
    var rx: f32 = 0.0;
    var ry: f32 = 0.0;
    var pip: sg.Pipeline = .{};
    var bind: sg.Bindings = .{};
    var pass_action: sg.PassAction = .{};
    const view: Mat4 = Mat4.lookat(.{ .x = 0.0, .y = 1.5, .z = 6.0 }, Vec3.zero(), Vec3.y_axis());
};

// a uniform block struct with a model-view-projection matrix
const VsParams = packed struct {
    view_proj: Mat4,
    model: Mat4,
    inv_trans_model: Mat4,

    fn init() @This() {
        const rxm = Mat4.rotate(rx, .{ .x = 1.0, .y = 0.0, .z = 0.0 });
        const rym = Mat4.rotate(ry, .{ .x = 0.0, .y = 1.0, .z = 0.0 });
        const model = Mat4.mul(rxm, rym);
        const aspect = @intToFloat(f32, sapp.width()) / @intToFloat(f32, sapp.height());
        const proj = Mat4.persp(60.0, aspect, 0.01, 10.0);
        return VsParams{ .mvp = Mat4.mul(Mat4.mul(proj, state.view), model) };
    }
};

export fn init() void {
    sg.setup(.{ .context = sgapp.context() });

    const Vertex = extern struct {
        pos: Vec3,
        norm: Vec3,

        fn init(x: f32, y: f32, z: f32) @This() {
            return .{ .pos = Vec3.init(x, y, z), .norm = Vec3.zero() };
        }
    };

    // cube vertex buffer
    comptime const t = (1.0 + @sqrt(5.0)) / 2.0;
    var vertices = [_]Vertex{
        Vertex.init(-1.0, t, 0.0),
        Vertex.init(1.0, t, 0.0),
        Vertex.init(-1.0, -t, 0.0),
        Vertex.init(1.0, -t, 0.0),
        Vertex.init(0.0, -1.0, t),
        Vertex.init(0.0, 1.0, t),
        Vertex.init(0.0, -1.0, -t),
        Vertex.init(0.0, 1.0, -t),
        Vertex.init(t, 0.0, -1.0),
        Vertex.init(t, 0.0, 1.0),
        Vertex.init(-t, 0.0, -1.0),
        Vertex.init(-t, 0.0, 1.0),
    };
    state.bind.vertex_buffers[0] = sg.makeBuffer(.{ .data = sg.range(vertices) });

    // cube index buffer
    const indices = [_]u16{
        0,  11, 5,
        0,  5,  1,
        0,  1,  7,
        0,  7,  10,
        0,  10, 11,
        1,  5,  9,
        5,  11, 4,
        11, 10, 2,
        10, 7,  6,
        7,  1,  8,
        3,  9,  4,
        3,  4,  2,
        3,  2,  6,
        3,  6,  8,
        3,  8,  9,
        4,  9,  5,
        2,  4,  11,
        6,  2,  10,
        8,  6,  7,
        9,  8,  1,
    };
    state.bind.index_buffer = sg.makeBuffer(.{
        .type = .INDEXBUFFER,
        .data = sg.range(indices),
    });

    // shader and pipeline object
    const shd = sg.makeShader(shaderDesc());
    var pip_desc: sg.PipelineDesc = .{
        .shader = shd,
        .index_type = .UINT16,
        .depth_stencil = .{
            .depth_compare_func = .LESS_EQUAL,
            .depth_write_enabled = true,
        },
        .rasterizer = .{ .cull_mode = .BACK },
    };
    pip_desc.layout.attrs[0].format = .FLOAT3;
    pip_desc.layout.attrs[1].format = .FLOAT4;
    state.pip = sg.makePipeline(pip_desc);

    // framebuffer clear color
    state.pass_action.colors[0] = .{ .action = .CLEAR, .val = .{ 0.25, 0.5, 0.75, 1.0 } };
}

export fn frame() void {
    state.rx += 1.0;
    state.ry += 2.0;
    const vs_params = computeVsParams(state.rx, state.ry);

    sg.beginDefaultPass(state.pass_action, sapp.width(), sapp.height());
    sg.applyPipeline(state.pip);
    sg.applyBindings(state.bind);
    sg.applyUniforms(.VS, 0, &vs_params, @sizeOf(@TypeOf(vs_params)));
    sg.draw(0, 60, 1);
    sg.endPass();
    sg.commit();
}

export fn cleanup() void {
    sg.shutdown();
}

pub fn main() void {
    sapp.run(.{
        .init_cb = init,
        .frame_cb = frame,
        .cleanup_cb = cleanup,
        .width = 800,
        .height = 600,
        .sample_count = 4,
        .window_title = "cube.zig",
    });
}

// build a backend-specific ShaderDesc struct
fn shaderDesc() sg.ShaderDesc {
    var desc: sg.ShaderDesc = .{};
    desc.vs.uniform_blocks[0].size = @sizeOf(VsParams);
    switch (sg.queryBackend()) {
        .D3D11 => {
            desc.attrs[0] = .{ .sem_name = "POSITION" };
            desc.attrs[1] = .{ .sem_name = "NORM" };
            desc.vs.source =
                \\ cbuffer params: register(b0) {
                \\ 	float4x4 view_proj;
                \\ 	float4x4 model;
                \\ 	float4x4 inv_trans_model;
                \\ };
                \\ struct vs_in {
                \\ 	float4 pos: POS;
                \\ 	float4 norm: NORM;
                \\ };
                \\ struct vs_out {
                \\ 	float4 pos: SV_POSITION;
                \\ 	float4 norm: NORM0;
                \\ };
                \\ vs_out main(vs_in inp) {
                \\ 	vs_out outp;
                \\ 	outp.pos = mul(mul(view_proj, model), inp.pos);
                \\ 	outp.norm = float4(mul((float3x3)inv_trans_model, inp.pos.xyz), 1.0);
                \\ 	return outp;
                \\ }
            ;
            desc.fs.source =
                \\ float4 main(float4 color: COLOR0): SV_Target0 {
                \\   return color;
                \\ }
            ;
        },
        .GLCORE33 => {
            // use explicit vertex attribute locations in the shader source,
            // we don't need to provide attribute names in that case

            // we need to describe the 'interiour' of uniform blocks though:
            desc.vs.uniform_blocks[0].uniforms[0] = .{ .name = "mvp", .type = .MAT4 };
            desc.vs.source =
                \\ #version 330
                \\ uniform mat4 mvp;
                \\ layout(location=0) in vec4 position;
                \\ layout(location=1) in vec4 color0;
                \\ out vec4 color;
                \\ void main() {
                \\   gl_Position = mvp * position;
                \\   color = color0;
                \\ }
            ;
            desc.fs.source =
                \\ #version 330
                \\ in vec4 color;
                \\ out vec4 frag_color;
                \\ void main() {
                \\   frag_color = color;
                \\ }
            ;
        },
        .METAL_MACOS => {
            desc.vs.source =
                \\ #include <metal_stdlib>
                \\ using namespace metal;
                \\ struct params_t {
                \\   float4x4 mvp;
                \\ };
                \\ struct vs_in {
                \\   float4 position [[attribute(0)]];
                \\   float4 color [[attribute(1)]];
                \\ };
                \\ struct vs_out {
                \\   float4 pos [[position]];
                \\   float4 color;
                \\ };
                \\ vertex vs_out _main(vs_in in [[stage_in]], constant params_t& params [[buffer(0)]]) {
                \\   vs_out out;
                \\   out.pos = params.mvp * in.position;
                \\   out.color = in.color;
                \\   return out;
                \\ }
            ;
            desc.fs.source =
                \\ #include <metal_stdlib>
                \\ using namespace metal;
                \\ fragment float4 _main(float4 color [[stage_in]]) {
                \\   return color;
                \\ }
            ;
        },
        else => {},
    }
    return desc;
}
