#pragma sokol @ctype mat4 Mat4
#pragma sokol @ctype vec4 Vec4

#pragma sokol @vs vs
uniform vs_params {
    mat4 view_proj;
    mat4 model;
    mat4 inv_trans_model;
    vec4 color;
};

in vec4 position;
in vec4 normal;

out vec3 frag_pos;
out vec3 norm;
out vec4 base_color;

void main() {
    gl_Position = view_proj * model * position;
    frag_pos = vec3(model * vec4(position.xyz, 1.0));
    norm = mat3(inv_trans_model) * normal.xyz;
    base_color = color;
}
#pragma sokol @end

#pragma sokol @fs fs
in vec3 frag_pos;
in vec3 norm;
in vec4 base_color;

out vec4 frag_color;

void main() {
    vec3 light_dir = normalize(vec3(500.0, 500.0, 500.0) - frag_pos);
    vec3 light_color = vec3(0.912, 0.802, 1.0);

    vec3 ambient = light_color * 0.5;
    vec3 diffuse = max(dot(normalize(norm), light_dir), 0.0) * light_color * 0.5;
    frag_color = base_color * vec4((ambient + diffuse), 1.0);
}
#pragma sokol @end

#pragma sokol @program cube vs fs
