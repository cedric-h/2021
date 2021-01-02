package main

import sg "../odin-sokol/src/sokol_gfx"
import sapp "../odin-sokol/src/sokol_app"
import "core:runtime";
import "core:math/linalg"

import "core:os"
import "core:fmt"

index_len: int;

state: struct {
	pass_action: sg.Pass_Action,
	bind:        sg.Bindings,
	pip:         sg.Pipeline,
};

init_callback :: proc "c" () {
	context = runtime.default_context();
	sg.setup({
		mtl_device                   = sapp.metal_get_device(),
		mtl_renderpass_descriptor_cb = sapp.metal_get_renderpass_descriptor,
		mtl_drawable_cb              = sapp.metal_get_drawable,
		d3d11_device                 = sapp.d3d11_get_device(),
		d3d11_device_context         = sapp.d3d11_get_device_context(),
		d3d11_render_target_view_cb  = sapp.d3d11_get_render_target_view,
		d3d11_depth_stencil_view_cb  = sapp.d3d11_get_depth_stencil_view,
	});

	Vertex :: struct {
		pos: [3]f32,
		col: [4]f32,
	};

	vertices := [?]Vertex{
        /* positions        colors */
		{{-1.0, -1.0, -1.0},  {1.0, 0.0, 0.0, 1.0}},
        {{+1.0, -1.0, -1.0},  {1.0, 0.0, 0.0, 1.0}},
        {{+1.0,  1.0, -1.0},  {1.0, 0.0, 0.0, 1.0}},
        {{-1.0,  1.0, -1.0},  {1.0, 0.0, 0.0, 1.0}},

        {{-1.0, -1.0,  1.0},  {0.0, 1.0, 0.0, 1.0}},
        {{+1.0, -1.0,  1.0},  {0.0, 1.0, 0.0, 1.0}},
        {{+1.0,  1.0,  1.0},  {0.0, 1.0, 0.0, 1.0}},
        {{-1.0,  1.0,  1.0},  {0.0, 1.0, 0.0, 1.0}},

        {{-1.0, -1.0, -1.0},  {0.0, 0.0, 1.0, 1.0}},
        {{-1.0,  1.0, -1.0},  {0.0, 0.0, 1.0, 1.0}},
        {{-1.0,  1.0,  1.0},  {0.0, 0.0, 1.0, 1.0}},
        {{-1.0, -1.0,  1.0},  {0.0, 0.0, 1.0, 1.0}},

        {{+1.0, -1.0, -1.0},  {1.0, 0.5, 0.0, 1.0}},
        {{+1.0,  1.0, -1.0},  {1.0, 0.5, 0.0, 1.0}},
        {{+1.0,  1.0,  1.0},  {1.0, 0.5, 0.0, 1.0}},
        {{+1.0, -1.0,  1.0},  {1.0, 0.5, 0.0, 1.0}},

        {{-1.0, -1.0, -1.0},  {0.0, 0.5, 1.0, 1.0}},
        {{-1.0, -1.0,  1.0},  {0.0, 0.5, 1.0, 1.0}},
        {{+1.0, -1.0,  1.0},  {0.0, 0.5, 1.0, 1.0}},
        {{+1.0, -1.0, -1.0},  {0.0, 0.5, 1.0, 1.0}},

        {{-1.0,  1.0, -1.0},  {1.0, 0.0, 0.5, 1.0}},
        {{-1.0,  1.0,  1.0},  {1.0, 0.0, 0.5, 1.0}},
        {{+1.0,  1.0,  1.0},  {1.0, 0.0, 0.5, 1.0}},
        {{+1.0,  1.0, -1.0},  {1.0, 0.0, 0.5, 1.0}},
    };
	state.bind.vertex_buffers[0] = sg.make_buffer({
		type = .VERTEXBUFFER,
		size = len(vertices)*size_of(vertices[0]),
		content = &vertices[0],
		label = "triangle-vertices",
	});

	indices := [?]u16{
        0, 1, 2,  0, 2, 3,
        6, 5, 4,  7, 6, 4,
        8, 9, 10,  8, 10, 11,
        14, 13, 12,  15, 14, 12,
        16, 17, 18,  16, 18, 19,
        22, 21, 20, 23, 22, 20
    };
	index_len = len(indices);
	state.bind.index_buffer = sg.make_buffer({
		type = .INDEXBUFFER,
		size = len(indices)*size_of(indices[0]),
		content = &indices[0],
		label = "triangle-indices",
	});

	state.pip = sg.make_pipeline({
		shader = sg.make_shader({
			attrs = {
				0 = {sem_name = "POS"},
				1 = {sem_name = "COLOR"},
			},
			vs = {
				source = `
					cbuffer params: register(b0) {
						float4x4 view_proj;
					};
					struct vs_in {
						float4 pos: POS;
						float4 col: COLOR;
					};
					struct vs_out {
						float4 col: COLOR0;
						float4 pos: SV_POSITION;
					};
					vs_out main(vs_in inp) {
						vs_out outp;
						outp.pos = mul(view_proj, inp.pos);
						outp.col = inp.col;
						return outp;
					}
				`,
				uniform_blocks = {
					0 = {
						size = size_of(linalg.Matrix4),
						uniforms = {
							0 = { name = "view_proj", type = .MAT4 },
						}
					}
				},
			},
			fs = {
				source = `
					float4 main(float4 col: COLOR0): SV_TARGET0 {
						return col;
					}
				`,
			},
		}),
		label = "triangle-pipeline",
		primitive_type = .TRIANGLES,
		index_type = .UINT16,
		layout = {
			attrs = {
				0 = {format = .FLOAT3},
				1 = {format = .FLOAT4},
			},
		},
        depth_stencil = {
            depth_compare_func = .LESS_EQUAL,
            depth_write_enabled = true
        },
        rasterizer = {
            cull_mode = .BACK,
        }
	});

	state.pass_action.colors[0] = {action = .CLEAR, val = {0.5, 0.7, 1.0, 1}};
}

frame_callback :: proc "c" () {
	using linalg;

	context = runtime.default_context();
	sg.begin_default_pass(state.pass_action, sapp.framebuffer_size());
	sg.apply_pipeline(state.pip);
	sg.apply_bindings(state.bind);

	w, h := sapp.framebuffer_size();
	view_proj := matrix4_perspective(60.0, f32(w) / f32(h), 0.01, 10.0)
		* matrix4_look_at({0.0, 1.5, 6.0}, {0.0, 0.0, 0.0}, {0.0, 1.0, 0.0});
	sg.apply_uniforms(.VS, 0, &view_proj, size_of(view_proj));

	sg.draw(0, index_len, 1);
	sg.end_pass();
	sg.commit();
}

event_callback :: proc "c" (event: ^sapp.Event) {
	if event.type == .KEY_DOWN && !event.key_repeat {
		#partial switch event.key_code {
		case .ESCAPE:
			sapp.request_quit();
		case .Q:
			if .CTRL in event.modifiers {
				sapp.request_quit();
			}
		}
	}
}

main :: proc() {
	err := sapp.run({
		init_cb      = init_callback,
		frame_cb     = frame_callback,
		cleanup_cb   = proc "c" () { sg.shutdown(); },
		event_cb     = event_callback,
		width        = 1080,
		height       = 720,
		window_title = "SOKOL Quad",
	});
	os.exit(int(err));
}
