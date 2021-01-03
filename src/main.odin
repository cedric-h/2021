package main

import sg "../odin-sokol/src/sokol_gfx"
import sapp "../odin-sokol/src/sokol_app"
import "core:runtime";
import "core:math/linalg";
import "core:math";

import "core:os"
import "core:fmt"

state: struct {
	pass_action: sg.Pass_Action,
	bind:        sg.Bindings,
	pip:         sg.Pipeline,
};

Uniforms :: struct {
	 view_proj:       linalg.Matrix4,
	 model:           linalg.Matrix4,
	 inv_trans_model: linalg.Matrix4,
}

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
		pos: linalg.Vector3,
		norm: linalg.Vector3,
	};

	t : f32 = (1 + math.sqrt_f32(5)) / 2;
	vertices := [?]Vertex {
		{{-1,  t, +0}, {0, 0, 0}},
		{{+1,  t, +0}, {0, 0, 0}},
		{{-1, -t, +0}, {0, 0, 0}},
		{{+1, -t, +0}, {0, 0, 0}},
		{{+0, -1,  t}, {0, 0, 0}},
		{{+0, +1,  t}, {0, 0, 0}},
		{{+0, -1, -t}, {0, 0, 0}},
		{{+0, +1, -t}, {0, 0, 0}},
		{{ t, +0, -1}, {0, 0, 0}},
		{{ t, +0, +1}, {0, 0, 0}},
		{{-t, +0, -1}, {0, 0, 0}},
		{{-t, +0, +1}, {0, 0, 0}},
    };
	for vert in &vertices {
		vert.norm = linalg.normalize(-vert.pos);
	}
	state.bind.vertex_buffers[0] = sg.make_buffer({
		size = len(vertices)*size_of(vertices[0]),
		content = &vertices[0],
		label = "triangle-vertices",
	});

	indices := [?]u16 {
		0 , 11, 5 ,
		0 , 5 , 1 ,
		0 , 1 , 7 ,
		0 , 7 , 10,
		0 , 10, 11,
		1 , 5 , 9 ,
		5 , 11, 4 ,
		11, 10, 2 ,
		10, 7 , 6 ,
		7 , 1 , 8 ,
		3 , 9 , 4 ,
		3 , 4 , 2 ,
		3 , 2 , 6 ,
		3 , 6 , 8 ,
		3 , 8 , 9 ,
		4 , 9 , 5 ,
		2 , 4 , 11,
		6 , 2 , 10,
		8 , 6 , 7 ,
		9 , 8 , 1 ,
	};
	state.bind.index_buffer = sg.make_buffer({
		size = len(indices)*size_of(indices[0]),
		content = &indices[0],
		label = "triangle-indices",
	});

	state.pip = sg.make_pipeline({
		shader = sg.make_shader({
			vs = {
				uniform_blocks = {
					0 = {
						size = size_of(Uniforms),
						uniforms = {
							0 = { name = "view_proj", type = .MAT4 },
							1 = { name = "model", type = .MAT4 },
							2 = { name = "inv_trans_model", type = .MAT4 },
						}
					}
				},
				source = `
				cbuffer params: register(b0) {
					float4x4 view_proj;
					float4x4 model;
					float4x4 inv_trans_model;
				};
				struct vs_in {
					float4 pos: POS;
					float4 norm: NORM;
				};
				struct vs_out {
					float4 pos: SV_POSITION;
					float4 norm: NORM0;
				};
				vs_out main(vs_in inp) {
					vs_out outp;
					outp.pos = mul(mul(view_proj, model), inp.pos);
					outp.norm = float4(mul((float3x3)inv_trans_model, inp.pos.xyz), 1.0);
					return outp;
				}
				`,
			},
			fs = {
				source = `
				float4 main(float4 pos: SV_POSITION, float4 norm: NORM0): SV_TARGET0 {
					float3 light_dir = normalize(float3(500.0, 500.0, 500.0) - pos.xyz);
					float3 light_color = float3(0.912, 0.802, 1.0);

					float3 ambient = light_color * 0.3;
					float3 diffuse = max(dot(normalize(norm.xyz), light_dir), 0.0) * light_color * 0.7;
					return float4(0.37, 0.25, 0.5, 1.0) * float4((ambient + diffuse), 1.0);
				}
				`,
			},

			attrs = {
				0 = {sem_name = "POS"},
				1 = {sem_name = "NORM"},
			},
		}),
		label = "triangle-pipeline",
		primitive_type = .TRIANGLES,
		index_type = .UINT16,
		layout = {
			attrs = {
				0 = {format = .FLOAT3},
				1 = {format = .FLOAT3},
			},
		},
		rasterizer = {
			cull_mode = .BACK,
		},
	});

	state.pass_action.colors[0] = {action = .CLEAR, val = {0.17, 0.02, 0.22, 1}};
}

rot : f32 = 0.0;

frame_callback :: proc "c" () {
	using linalg;

	context = runtime.default_context();
	sg.begin_default_pass(state.pass_action, sapp.framebuffer_size());
	sg.apply_pipeline(state.pip);
	sg.apply_bindings(state.bind);

	rot += 0.1;

	w, h := sapp.framebuffer_size();
	view_proj := mul(matrix4_perspective(45.0, f32(w) / f32(h), 0.00, 10.0),
				 mul(matrix4_look_at({0.0, 1.5, 6.0}, {0.0, 0.0, 0.0}, {0.0, 1.0, 0.0}),
					 matrix4_rotate(rot, {0.3, 0.3, 0.0})));
	uni := Uniforms {
		view_proj       = view_proj,
		model           = MATRIX4_IDENTITY,
		inv_trans_model = matrix4_inverse_transpose(MATRIX4_IDENTITY),
	};
	sg.apply_uniforms(.VS, 0, &uni, size_of(uni));

	sg.draw(0, 60, 1);
	sg.end_pass();
	sg.commit();
}

main :: proc() {
	err := sapp.run({
		init_cb      = init_callback,
		frame_cb     = frame_callback,
		cleanup_cb   = proc "c" () { sg.shutdown(); },
		event_cb     = event_callback,
		width        = 1280,
		height       = 720,
		window_title = "SOKOL Quad",
	});
	os.exit(int(err));
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
