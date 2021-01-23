typedef int8_t   i8;
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef float    f32;
typedef double   f64;

#define PI32 3.14159265359f
#define INLINE static inline

INLINE float randf() {
	return (float) rand() / (float) RAND_MAX;
}

INLINE f32 wrap(f32 a, f32 b) {
	return (a > b) ? (a - b) : a;
}

INLINE f32 clamp(f32 min, f32 val, f32 max) {
	if (val < min) return min;
	if (val > max) return max;
	return val;
}

INLINE f32 lerp(f32 a, f32 t, f32 b) {
    return (1.0f - t) * a + t * b;
}

INLINE f32 to_radians(f32 degrees) {
	return degrees * (PI32 / 180.0f);
}

typedef union {
	struct { f32 x, y; };
	struct { f32 u, v; };
	f32 nums[2];
} Vec2;

INLINE Vec2 print2(Vec2 v) {
	for (int i = 0; i < 2; i++)
		printf("%f ", v.nums[i]);
	printf("\n");
	return v;
}

INLINE Vec2 vec2(f32 x, f32 y) {
	return (Vec2){ .nums = { x, y } };
}

INLINE Vec2 vec2f(f32 f) {
	return vec2(f, f);
}

INLINE Vec2 vec2_x() {
	return vec2(1.0, 0.0);
}

INLINE Vec2 vec2_y() {
	return vec2(0.0, 1.0);
}

INLINE Vec2 add2(Vec2 a, Vec2 b) {
	return vec2(a.x + b.x,
				a.y + b.y);
}

INLINE Vec2 sub2(Vec2 a, Vec2 b) {
	return vec2(a.x - b.x,
				a.y - b.y);
}

INLINE Vec2 div2(Vec2 a, Vec2 b) {
	return vec2(a.x / b.x,
				a.y / b.y);
}

INLINE Vec2 div2f(Vec2 a, f32 f) {
	return vec2(a.x / f,
				a.y / f);
}

INLINE Vec2 mul2(Vec2 a, Vec2 b) {
	return vec2(a.x * b.x,
				a.y * b.y);
}

INLINE Vec2 mul2f(Vec2 a, f32 f) {
	return vec2(a.x * f,
				a.y * f);
}

INLINE f32 dot2(Vec2 a, Vec2 b) {
	return a.x*b.x + a.y*b.y;
}

INLINE Vec2 lerp2(Vec2 a, f32 t, Vec2 b) {
    return add2(mul2f(a, 1.0f - t), mul2f(b, t));
}

INLINE f32 mag2(Vec2 a) {
    return sqrtf(dot2(a, a));
}

INLINE f32 magmag2(Vec2 a) {
    return dot2(a, a);
}

INLINE Vec2 norm2(Vec2 a) {
    return div2f(a, mag2(a));
}

INLINE bool eq2(Vec2 a, Vec2 b) {
    return a.x == b.x &&
		   a.y == b.y;
}

INLINE Vec2 rand2() {
	f32 theta = randf() * PI32 * 2.0;
	return vec2(cosf(theta),
	            sinf(theta));
}

INLINE Vec2 perp2(Vec2 a) {
    return vec2(a.y, -a.x);
}


typedef union {
	struct { f32 x, y, z; };
	f32 nums[3];
} Vec3;

INLINE Vec3 print3(Vec3 v) {
	for (int i = 0; i < 3; i++)
		printf("%f ", v.nums[i]);
	printf("\n");
	return v;
}

INLINE Vec3 vec3(f32 x, f32 y, f32 z) {
	return (Vec3){ .nums = { x, y, z } };
}

INLINE Vec3 vec3f(f32 f) {
	return vec3(f, f, f);
}

INLINE Vec3 vec3_x() {
	return vec3(1.0, 0.0, 0.0);
}

INLINE Vec3 vec3_y() {
	return vec3(0.0, 1.0, 0.0);
}

INLINE Vec3 vec3_z() {
	return vec3(0.0, 0.0, 1.0);
}

INLINE Vec3 add3(Vec3 a, Vec3 b) {
	return vec3(a.x + b.x,
				a.y + b.y,
				a.z + b.z);
}

INLINE Vec3 sub3(Vec3 a, Vec3 b) {
	return vec3(a.x - b.x,
				a.y - b.y,
				a.z - b.z);
}

INLINE Vec3 div3(Vec3 a, Vec3 b) {
	return vec3(a.x / b.x,
				a.y / b.y,
				a.z / b.z);
}

INLINE Vec3 div3f(Vec3 a, f32 f) {
	return vec3(a.x / f,
				a.y / f,
				a.z / f);
}

INLINE Vec3 mul3(Vec3 a, Vec3 b) {
	return vec3(a.x * b.x,
				a.y * b.y,
				a.z * b.z);
}

INLINE Vec3 mul3f(Vec3 a, f32 f) {
	return vec3(a.x * f,
				a.y * f,
				a.z * f);
}

INLINE f32 dot3(Vec3 a, Vec3 b) {
	return a.x*b.x + a.y*b.y + a.z*b.z;
}

INLINE Vec3 lerp3(Vec3 a, f32 t, Vec3 b) {
    return add3(mul3f(a, 1.0f - t), mul3f(b, t));
}

INLINE f32 mag3(Vec3 a) {
    return sqrtf(dot3(a, a));
}

INLINE f32 magmag3(Vec3 a) {
    return dot3(a, a);
}

INLINE Vec3 norm3(Vec3 a) {
    return div3f(a, mag3(a));
}

INLINE bool eq3(Vec3 a, Vec3 b) {
    return a.x == b.x &&
           a.y == b.y &&
		   a.z == b.z;
}

/* source: https://math.stackexchange.com/a/44691 */
INLINE Vec3 rand3() {
	f32 theta = randf() * PI32 * 2.0,
	        z = 1.0 - randf() * 2.0,
		   cz = sqrtf(1.0 - powf(z, 2.0));

	return vec3(cz * cosf(theta),
	            cz * sinf(theta),
			    z               );
}

INLINE Vec3 cross3(Vec3 a, Vec3 b) {
    return vec3((a.y * b.z) - (a.z * b.y),
				(a.z * b.x) - (a.x * b.z),
				(a.x * b.y) - (a.y * b.x));
}

INLINE Vec3 project_plane_vec3(Vec3 n, Vec3 bd) {
	return sub3(bd, mul3f(n, dot3(bd, n)));
}

typedef union {
	struct { f32 x, y, z, w; };
	f32 nums[4];
} Vec4;

INLINE Vec4 print4(Vec4 v) {
	for (int i = 0; i < 4; i++)
		printf("%f ", v.nums[i]);
	printf("\n");
	return v;
}

INLINE Vec4 vec4(f32 x, f32 y, f32 z, f32 w) {
	return (Vec4){ .nums = { x, y, z, w } };
}

INLINE Vec4 vec4f(f32 f) {
	return vec4(f, f, f, f);
}

INLINE Vec4 vec4_x() {
	return vec4(1.0, 0.0, 0.0, 0.0);
}

INLINE Vec4 vec4_y() {
	return vec4(0.0, 1.0, 0.0, 0.0);
}

INLINE Vec4 vec4_z() {
	return vec4(0.0, 0.0, 1.0, 0.0);
}

INLINE Vec4 vec4_w() {
	return vec4(0.0, 0.0, 0.0, 1.0);
}

INLINE Vec4 add4(Vec4 a, Vec4 b) {
	return vec4(a.x + b.x,
				a.y + b.y,
				a.z + b.z,
				a.w + b.w);
}

INLINE Vec4 sub4(Vec4 a, Vec4 b) {
	return vec4(a.x - b.x,
				a.y - b.y,
				a.z - b.z,
				a.w - b.w);
}

INLINE Vec4 div4(Vec4 a, Vec4 b) {
	return vec4(a.x / b.x,
				a.y / b.y,
				a.z / b.z,
				a.w / b.w);
}

INLINE Vec4 div4f(Vec4 a, f32 f) {
	return vec4(a.x / f,
				a.y / f,
				a.z / f,
				a.w / f);
}

INLINE Vec4 mul4(Vec4 a, Vec4 b) {
	return vec4(a.x * b.x,
				a.y * b.y,
				a.z * b.z,
				a.w * b.w);
}

INLINE Vec4 mul4f(Vec4 a, f32 f) {
	return vec4(a.x * f,
				a.y * f,
				a.z * f,
				a.w * f);
}

INLINE f32 dot4(Vec4 a, Vec4 b) {
	return a.x*b.x + a.y*b.y + a.z*b.z + a.w*b.w;
}

INLINE Vec4 lerp4(Vec4 a, f32 t, Vec4 b) {
    return add4(mul4f(a, 1.0f - t), mul4f(b, t));
}

INLINE f32 mag4(Vec4 a) {
    return sqrtf(dot4(a, a));
}

INLINE f32 magmag4(Vec4 a) {
    return dot4(a, a);
}

INLINE Vec4 norm4(Vec4 a) {
    return div4f(a, mag4(a));
}

INLINE bool eq4(Vec3 a, Vec3 b) {
    return a.x == b.x &&
           a.y == b.y &&
		   a.z == b.z;
}

typedef union {
	struct { Vec3 x, y, z; };
	Vec3 cols[3];
	f32 nums[3][3];
} Mat3;

/* Multiplies two Mat3s, returning a new one */
INLINE Mat3 mul3x3(Mat3 a, Mat3 b) {
	Mat3 out;
	i8 k, r, c;
	for (c = 0; c < 3; ++c)
		for (r = 0; r < 3; ++r) {
			out.nums[c][r] = 0.0f;
			for (k = 0; k < 3; ++k)
				out.nums[c][r] += a.nums[k][r] * b.nums[c][k];
		}
	return out;
}

/* Rotates the given rotation matrix so that the Y basis vector
	points to `new_y`. The X basis vector is orthogonalized with
	the new Y and old Z basis vector projected onto the Y plane.
*/
INLINE void rotated_up_indefinite_basis(Mat3 *rot, Vec3 up) {
    rot->cols[1] = up;
    rot->cols[2] = norm3(project_plane_vec3(up, rot->cols[2]));
    rot->cols[0] = cross3(rot->cols[1], rot->cols[2]);
}

typedef union {
	struct { Vec4 x, y, z, w; };
	Vec4 cols[4];
	float nums[4][4];
} Mat4;

INLINE Mat4 mul4x4(Mat4 a, Mat4 b) {
	Mat4 out;
	i8 k, r, c;
	for (c = 0; c < 4; ++c)
		for (r = 0; r < 4; ++r) {
			out.nums[c][r] = 0.0f;
			for (k = 0; k < 4; ++k)
				out.nums[c][r] += a.nums[k][r] * b.nums[c][k];
		}
	return out;
}

INLINE Mat4 mat4_from_mat3_and_translation(Mat3 basis_vectors, Vec3 pos) {
	Mat4 res;
	res.nums[0][0] =  basis_vectors.x.x;
	res.nums[0][1] =  basis_vectors.y.x;
	res.nums[0][2] = -basis_vectors.z.x;
	res.nums[0][3] =  0.0;

	res.nums[1][0] =  basis_vectors.x.y;
	res.nums[1][1] =  basis_vectors.y.y;
	res.nums[1][2] = -basis_vectors.z.y;
	res.nums[1][3] =  0.0;

	res.nums[2][0] =  basis_vectors.x.z;
	res.nums[2][1] =  basis_vectors.y.z;
	res.nums[2][2] = -basis_vectors.z.z;
	res.nums[2][3] =  0.0;

	res.nums[3][0] = -dot3(basis_vectors.x, pos);
	res.nums[3][1] = -dot3(basis_vectors.y, pos);
	res.nums[3][2] =  dot3(basis_vectors.z, pos);
	res.nums[3][3] =  1.0;
	return res;
}

INLINE Mat4 mat4x4() {
	return (Mat4){0};
}

INLINE Mat4 diag4x4(f32 f) {
	Mat4 res = mat4x4();
	res.nums[0][0] = f;
    res.nums[1][1] = f;
    res.nums[2][2] = f;
    res.nums[3][3] = f;
	return res;
}

INLINE Mat4 scale4x4(Vec3 scale) {
	Mat4 res = diag4x4(1.0);
    res.nums[0][0] = scale.x;
    res.nums[1][1] = scale.y;
    res.nums[2][2] = scale.z;
	return res;
}

INLINE Mat4 translate4x4(Vec3 pos) {
	Mat4 res = diag4x4(1.0);
    res.nums[3][0] = pos.x;
    res.nums[3][1] = pos.y;
    res.nums[3][2] = pos.z;
	return res;
}

INLINE Mat4 print4x4(Mat4 a) {
    for(int c = 0; c < 4; ++c) {
        for(int r = 0; r < 4; ++r) {
			printf("%f ", a.nums[c][r]);
		}
		printf("\n");
	}
	return a;
}

INLINE Mat4 transpose4x4(Mat4 a) {
	Mat4 res;
    for(int c = 0; c < 4; ++c)
        for(int r = 0; r < 4; ++r)
            res.nums[r][c] = a.nums[c][r];
	return res;
}

INLINE Mat4 invert4x4(Mat4 a) {
    f32 s[6], c[6];
	s[0] = a.nums[0][0]*a.nums[1][1] - a.nums[1][0]*a.nums[0][1];
	s[1] = a.nums[0][0]*a.nums[1][2] - a.nums[1][0]*a.nums[0][2];
	s[2] = a.nums[0][0]*a.nums[1][3] - a.nums[1][0]*a.nums[0][3];
	s[3] = a.nums[0][1]*a.nums[1][2] - a.nums[1][1]*a.nums[0][2];
	s[4] = a.nums[0][1]*a.nums[1][3] - a.nums[1][1]*a.nums[0][3];
	s[5] = a.nums[0][2]*a.nums[1][3] - a.nums[1][2]*a.nums[0][3];

	c[0] = a.nums[2][0]*a.nums[3][1] - a.nums[3][0]*a.nums[2][1];
	c[1] = a.nums[2][0]*a.nums[3][2] - a.nums[3][0]*a.nums[2][2];
	c[2] = a.nums[2][0]*a.nums[3][3] - a.nums[3][0]*a.nums[2][3];
	c[3] = a.nums[2][1]*a.nums[3][2] - a.nums[3][1]*a.nums[2][2];
	c[4] = a.nums[2][1]*a.nums[3][3] - a.nums[3][1]*a.nums[2][3];
	c[5] = a.nums[2][2]*a.nums[3][3] - a.nums[3][2]*a.nums[2][3];
	
	/* Assumes it is invertible */
	f32 idet = 1.0f/( s[0]*c[5]-s[1]*c[4]+s[2]*c[3]+s[3]*c[2]-s[4]*c[1]+s[5]*c[0] );
	
    Mat4 res;
	res.nums[0][0] = ( a.nums[1][1] * c[5] - a.nums[1][2] * c[4] + a.nums[1][3] * c[3]) * idet;
	res.nums[0][1] = (-a.nums[0][1] * c[5] + a.nums[0][2] * c[4] - a.nums[0][3] * c[3]) * idet;
	res.nums[0][2] = ( a.nums[3][1] * s[5] - a.nums[3][2] * s[4] + a.nums[3][3] * s[3]) * idet;
	res.nums[0][3] = (-a.nums[2][1] * s[5] + a.nums[2][2] * s[4] - a.nums[2][3] * s[3]) * idet;

	res.nums[1][0] = (-a.nums[1][0] * c[5] + a.nums[1][2] * c[2] - a.nums[1][3] * c[1]) * idet;
	res.nums[1][1] = ( a.nums[0][0] * c[5] - a.nums[0][2] * c[2] + a.nums[0][3] * c[1]) * idet;
	res.nums[1][2] = (-a.nums[3][0] * s[5] + a.nums[3][2] * s[2] - a.nums[3][3] * s[1]) * idet;
	res.nums[1][3] = ( a.nums[2][0] * s[5] - a.nums[2][2] * s[2] + a.nums[2][3] * s[1]) * idet;

	res.nums[2][0] = ( a.nums[1][0] * c[4] - a.nums[1][1] * c[2] + a.nums[1][3] * c[0]) * idet;
	res.nums[2][1] = (-a.nums[0][0] * c[4] + a.nums[0][1] * c[2] - a.nums[0][3] * c[0]) * idet;
	res.nums[2][2] = ( a.nums[3][0] * s[4] - a.nums[3][1] * s[2] + a.nums[3][3] * s[0]) * idet;
	res.nums[2][3] = (-a.nums[2][0] * s[4] + a.nums[2][1] * s[2] - a.nums[2][3] * s[0]) * idet;

	res.nums[3][0] = (-a.nums[1][0] * c[3] + a.nums[1][1] * c[1] - a.nums[1][2] * c[0]) * idet;
	res.nums[3][1] = ( a.nums[0][0] * c[3] - a.nums[0][1] * c[1] + a.nums[0][2] * c[0]) * idet;
	res.nums[3][2] = (-a.nums[3][0] * s[3] + a.nums[3][1] * s[1] - a.nums[3][2] * s[0]) * idet;
	res.nums[3][3] = ( a.nums[2][0] * s[3] - a.nums[2][1] * s[1] + a.nums[2][2] * s[0]) * idet;
    return res;
}

// See https://www.khronos.org/registry/OpenGL-Refpages/gl2.1/xhtml/gluPerspective.xml
INLINE Mat4 perspective4x4(f32 fov, f32 aspect, f32 near, f32 far) {
    f32 cotangent = 1.0f / tanf(fov * (PI32 / 360.0f));

	Mat4 res = mat4x4();
    res.nums[0][0] = cotangent / aspect;
    res.nums[1][1] = cotangent;
    res.nums[2][3] = -1.0f;
    res.nums[2][2] = (near + far) / (near - far);
    res.nums[3][2] = (2.0f * near * far) / (near - far);
    res.nums[3][3] = 0.0f;
    return res;
}

typedef union {
    struct {
        union {
            Vec3 xyz;
            struct { f32 x, y, z; };
        };

        float w;
    };

	f32 nums[4];
} Quat;

INLINE Quat quat(f32 x, f32 y, f32 z, f32 w) {
	return (Quat){ .nums = { x, y, z, w } };
}

INLINE Quat printQ(Quat q) {
	for (int i = 0; i < 4; i++)
		printf("%f ", q.nums[i]);
	printf("\n");
	return q;
}

INLINE Quat mulQ(Quat a, Quat b) {
    return quat(( a.x * b.w) + (a.y * b.z) - (a.z * b.y) + (a.w * b.x),
                (-a.x * b.z) + (a.y * b.w) + (a.z * b.x) + (a.w * b.y),
                ( a.x * b.y) - (a.y * b.x) + (a.z * b.w) + (a.w * b.z),
                (-a.x * b.x) - (a.y * b.y) - (a.z * b.z) + (a.w * b.w));
}

INLINE Vec3 mulQ3(Quat q, Vec3 v) {
	Vec3 t = mul3f(cross3(q.xyz, v), 2.0);
	return add3(add3(v, mul3f(t, q.w)), cross3(q.xyz, t));
}

INLINE Quat axis_angleQ(Vec3 axis, f32 angle) {
    Vec3 axis_norm = norm3(axis);
    f32 rot_sin = sinf(angle / 2.0f);

    Quat res;
    res.xyz = mul3f(axis_norm, rot_sin);
    res.w = cosf(angle / 2.0f);
    return res;
}
