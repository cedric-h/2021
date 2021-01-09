#pragma sokol @ctype mat4 hmm_mat4

#pragma sokol @vs vs
uniform vs_params {
    mat4 mvp;
};

in vec4 position;
in vec4 color0;

out vec4 color;

void main() {
    gl_Position = mvp * position;
    color = color0;
}
#pragma sokol @end

#pragma sokol @fs fs
in vec4 color;
out vec4 frag_color;

void main() {
    frag_color = color;
}
#pragma sokol @end

#pragma sokol @program cube vs fs
