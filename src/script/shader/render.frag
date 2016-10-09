#define MARCH_ITER 100
#define RAYAMP_MIN 0.01
#define REFLECT_MAX 10.0
#define REFLECT_PER_PATH 3
#define INIT_LEN 0.01
#define SKY_COLOR vec3( 0.0 )

// ------

#define PI 3.14159265
#define V vec2(0.,1.)
#define saturate(i) clamp(i,0.,1.)
#define lofi(i,m) (floor((i)/(m))*(m))

// ------

#extension GL_EXT_draw_buffers : require
precision highp float;

uniform float time;
uniform vec2 resolution;
uniform bool reset;

uniform sampler2D textureRandom;
uniform sampler2D textureRandomStatic;

uniform sampler2D textureDrawBuffers0;
uniform sampler2D textureDrawBuffers1;
uniform sampler2D textureDrawBuffers2;
uniform sampler2D textureDrawBuffers3;

uniform sampler2D textureImageArrow;
uniform sampler2D textureImageVertical;
uniform sampler2D textureImageUtsunomiya;
uniform sampler2D textureImageFoot;
uniform sampler2D textureImageStop;
uniform sampler2D textureImageFive;
uniform sampler2D textureImageSix;
uniform sampler2D textureImageThirtythree;
uniform sampler2D textureImageSuica;

// ------

vec4 seed;
float random() { // weird prng
  const vec4 q = vec4(   1225.0,    1585.0,    2457.0,    2098.0);
  const vec4 r = vec4(   1112.0,     367.0,      92.0,     265.0);
  const vec4 a = vec4(   3423.0,    2646.0,    1707.0,    1999.0);
  const vec4 m = vec4(4194287.0, 4194277.0, 4194191.0, 4194167.0);

  vec4 beta = floor(seed / q);
  vec4 p = a * (seed - beta * q) - beta * r;
  beta = (sign(-p) + vec4(1.0)) * vec4(0.5) * m;
  seed = (p + beta);

  return fract(dot(seed / m, vec4(1.0, -1.0, 1.0, -1.0)));
}

vec4 random4() {
  return vec4(
    random(),
    random(),
    random(),
    random()
  );
}

// ------

mat2 rotate2D( float _t ) {
  return mat2( cos( _t ), sin( _t ), -sin( _t ), cos( _t ) );
}

bool isUvValid( vec2 _v ) {
  return ( 0.0 <= _v.x ) && ( _v.x <= 1.0 ) && ( 0.0 <= _v.y ) && ( _v.y <= 1.0 );
}

// ------

struct Camera {
  vec3 pos;
  vec3 dir;
  vec3 sid;
  vec3 top;
};

struct Ray {
  vec3 dir;
  vec3 ori;
  bool inside;
};

struct Material {
  vec3 color;

  vec3 emissive;
  vec3 edgeEmissive;

  float reflective;
  float reflectiveRoughness;
  float refractive;
  float refractiveIndex;
};

struct Map {
  float dist;
  Material material;
};

struct March {
  Ray ray;
  Map map;
  float len;
  vec3 pos;
  vec3 normal;
};

// ------

Camera camInit( in vec3 _pos, in vec3 _tar ) {
  Camera cam;
  cam.pos = _pos;
  cam.dir = normalize( _tar - _pos );
  cam.sid = normalize( cross( cam.dir, V.xyx ) );
  cam.top = normalize( cross( cam.sid, cam.dir ) );

  return cam;
}

Map distFunc( in vec3 _p );
Ray rayInit( in vec3 _ori, in vec3 _dir ) {
  Ray ray;
  ray.dir = _dir;
  ray.ori = _ori;
  ray.inside = distFunc( ray.ori ).dist < 0.0;
  return ray;
}

Ray rayFromCam( in vec2 _p, in Camera _cam ) {
  vec3 dir = normalize( _p.x * _cam.sid + _p.y * _cam.top + _cam.dir * 2.0 );
  return rayInit( _cam.pos, dir );
}

Material mtlInit( in vec3 _col ) {
  Material material;
  material.color = _col;

  material.emissive = V.xxx;
  material.edgeEmissive = V.xxx;

  material.reflective = 0.0;
  material.reflectiveRoughness = 0.0;
  material.refractive = 0.0;
  material.refractiveIndex = 1.0;

  return material;
}

Map mapInit( in float _dist ) {
  Map map;
  map.dist = _dist;
  map.material = mtlInit( V.xxx );
  return map;
}

March marchInit( in Ray _ray ) {
  March march;
  march.ray = _ray;
  march.len = INIT_LEN;
  march.pos = _ray.ori + _ray.dir * march.len;
  return march;
}

// ------

float sphere( in vec3 _p, in float _r ) {
  return length( _p ) - _r;
}

float box( in vec3 _p, in vec3 _size ) {
  vec3 d = abs( _p ) - _size;
  return min( max( d.x, max( d.y, d.z ) ), 0.0 ) + length( max( d, 0.0 ) );
}

Map distFunc( in vec3 _p ) {
  Map map = mapInit( 1E9 );

  { // floor, roof
    vec3 p = _p;
    p.z = mod( p.z, 2.0 ) - 1.0;
    float dist = 5.0 - abs( p.y );
    dist = max( dist, 0.2 - abs( p.z ) );

    if ( dist < map.dist ) {
      map = mapInit( dist );
      map.material = mtlInit( vec3( 0.4 ) );
      // map.material.reflective = 0.1;
    }
  }

  { // arrow
    vec3 p = _p;
    p -= vec3( -0.4, 1.1, 1.5 );
    p.zx = rotate2D( 0.1 * sin( time * PI * 2.0 + 4.0 ) ) * p.zx;
    p.yz = rotate2D( 0.1 * sin( time * PI * 2.0 + 2.0 ) ) * p.yz;
    float dist = box( p, vec3( 1.0 + 0.02, 0.3 + 0.02, 0.2 ) );

    if ( dist < map.dist ) {
      map = mapInit( dist );
      vec2 uv = p.xy / vec2( 1.0, 0.3 ) / 2.0 + 0.5;
      vec3 tex = texture2D( textureImageArrow, uv ).xyz;

      map.material = mtlInit( vec3( 0.0 ) );
      if ( isUvValid( uv ) ) {
        map.material.color = tex;
        map.material.reflective = 0.2;
        map.material.emissive = tex * 1.0;
      } else {
        map.material.color = vec3( 0.2 );
        map.material.reflective = 0.2;
        map.material.reflectiveRoughness = 0.05;
      }
    }
  }

  { // vertical
    vec3 p = _p;
    p -= vec3( -0.7, -0.3, 2.0 );
    p.zx = rotate2D( 0.4 + 0.1 * sin( time * PI * 2.0 ) ) * p.zx;
    p.xy = rotate2D( 0.1 * sin( time * PI * 2.0 + 1.0 ) ) * p.xy;
    float dist = box( p, vec3( 0.1, 0.5, 0.02 ) );

    if ( dist < map.dist ) {
      map = mapInit( dist );
      vec2 uv = p.xy / vec2( 0.1, 0.5 ) / 2.0 + 0.5;
      vec3 tex = texture2D( textureImageVertical, uv ).xyz;

      map.material = mtlInit( vec3( 0.0 ) );
      map.material.color = tex;
      map.material.reflective = 0.1;
    }
  }

  { // foot
    vec3 p = _p;
    p -= vec3( -0.6, -0.9, 3.0 );
    p.yz = rotate2D( 0.4 + 0.1 * sin( time * PI * 2.0 + 2.0 ) ) * p.yz;
    p.xy = rotate2D( 0.1 * sin( time * PI * 2.0 + 3.0 ) ) * p.xy;
    float dist = box( p, vec3( 0.5, 0.1, 0.02 ) );

    if ( dist < map.dist ) {
      map = mapInit( dist );
      vec2 uv = p.xy / vec2( 0.5, 0.1 ) / 2.0 + 0.5;
      vec3 tex = texture2D( textureImageFoot, uv ).xyz;

      map.material = mtlInit( vec3( 0.0 ) );
      map.material.color = tex;
    }
  }

  { // utsunomiya
    vec3 p = _p;
    p -= vec3( 0.5, 0.0, 1.7 );
    p.zx = rotate2D( -0.2 + 0.1 * sin( time * PI * 2.0 + 3.0 ) ) * p.zx;
    p.yz = rotate2D( -0.2 + 0.1 * sin( time * PI * 2.0 + 1.0 ) ) * p.yz;
    float dist = box( p, vec3( 0.5 + 0.01, 0.2 + 0.01, 0.1 ) );

    if ( dist < map.dist ) {
      map = mapInit( dist );
      vec2 uv = p.xy / vec2( 0.5, 0.2 ) / 2.0 + 0.5;
      vec3 tex = texture2D( textureImageUtsunomiya, uv ).xyz;

      map.material = mtlInit( vec3( 0.0 ) );
      if ( isUvValid( uv ) ) {
        map.material.color = tex;
        map.material.reflective = 0.2;
        map.material.emissive = tex * 1.0;
      } else {
        map.material.color = vec3( 0.2 );
        map.material.reflective = 0.2;
        map.material.reflectiveRoughness = 0.05;
      }
    }
  }

  { // stop
    vec3 p = _p;
    p -= vec3( 1.2, 0.5, 2.4 );
    p.zx = rotate2D( 0.4 + 0.1 * sin( time * PI * 2.0 + 2.0 ) ) * p.zx;
    p.yz = rotate2D( -0.2 + 0.1 * sin( time * PI * 2.0 + 1.0 ) ) * p.yz;
    float dist = box( p, vec3( 0.2, 0.5, 0.02 ) );

    if ( dist < map.dist ) {
      map = mapInit( dist );
      vec2 uv = p.xy / vec2( 0.2, 0.5 ) / 2.0 + 0.5;
      vec3 tex = texture2D( textureImageStop, uv ).xyz;

      map.material = mtlInit( vec3( 0.0 ) );
      map.material.color = tex;
      map.material.reflective = 0.04;
      map.material.reflectiveRoughness = 0.02;
    }
  }

  { // five
    vec3 p = _p;
    p -= vec3( 0.8, -0.7, 1.4 );
    p.yz = rotate2D( 0.2 + 0.1 * sin( time * PI * 2.0 + 6.0 ) ) * p.yz;
    p.xy = rotate2D( -0.1 + 0.1 * sin( time * PI * 2.0 + 4.0 ) ) * p.xy;
    float dist = box( p, vec3( 0.2, 0.2, 0.01 ) );

    if ( dist < map.dist ) {
      map = mapInit( dist );
      vec2 uv = p.xy / vec2( 0.2, 0.2 ) / 2.0 + 0.5;
      vec3 tex = texture2D( textureImageFive, uv ).xyz;

      map.material = mtlInit( vec3( 0.0 ) );
      map.material.color = tex;
    }
  }

  { // six
    vec3 p = _p;
    p -= vec3( 0.0, -0.7, 0.4 );
    p.yz = rotate2D( 0.1 * sin( time * PI * 2.0 + 2.0 ) ) * p.yz;
    p.xy = rotate2D( 0.4 + 0.1 * sin( time * PI * 2.0 + 3.0 ) ) * p.xy;
    float dist = box( p, vec3( 0.2, 0.2, 0.01 ) );

    if ( dist < map.dist ) {
      map = mapInit( dist );
      vec2 uv = p.xy / vec2( 0.2, 0.2 ) / 2.0 + 0.5;
      vec3 tex = texture2D( textureImageSix, uv ).xyz;

      map.material = mtlInit( vec3( 0.0 ) );
      map.material.color = tex;
    }
  }

  { // thirtythree
    vec3 p = _p;
    p -= vec3( 1.0, -1.1, 2.0 );
    p.yz = rotate2D( -0.6 + 0.1 * sin( time * PI * 2.0 + 4.0 ) ) * p.yz;
    p.xy = rotate2D( 0.4 + 0.1 * sin( time * PI * 2.0 + 1.0 ) ) * p.xy;
    float dist = box( p, vec3( 0.35, 0.2, 0.01 ) );

    if ( dist < map.dist ) {
      map = mapInit( dist );
      vec2 uv = p.xy / vec2( 0.35, 0.2 ) / 2.0 + 0.5;
      vec3 tex = texture2D( textureImageThirtythree, uv ).xyz;

      map.material = mtlInit( vec3( 0.0 ) );
      map.material.color = tex;
    }
  }

  { // neon
    vec3 p = _p;
    p.xy = rotate2D( p.z * 0.03 ) * p.xy;
    p.x = abs( p.x ) - 2.0;
    p.z = mod( p.z - 2.0, 4.0 ) - 2.0;
    float dist = length( p.zx ) - 0.1;
    dist = max( dist, abs( _p.z ) - 25.0 );

    if ( dist < map.dist ) {
      map = mapInit( dist );
      map.material = mtlInit( vec3( 0.0 ) );
      map.material.emissive = vec3( 0.8, 0.9, 1.0 ) * 20.0;
    }
  }

  return map;
}

vec3 normalFunc( in vec3 _p, in float _d ) {
  vec2 d = V * _d;
  return normalize( vec3(
    distFunc( _p + d.yxx ).dist - distFunc( _p - d.yxx ).dist,
    distFunc( _p + d.xyx ).dist - distFunc( _p - d.xyx ).dist,
    distFunc( _p + d.xxy ).dist - distFunc( _p - d.xxy ).dist
  ) );
}

// ------

March march( in Ray _ray ) {
  Ray ray = _ray;
  March march = marchInit( ray );

  for ( int iMarch = 0; iMarch < MARCH_ITER; iMarch ++ ) {
    Map map = distFunc( march.pos );
    map.dist *= ( ray.inside ? -1.0 : 1.0 ) * 0.8;

    march.map = map;
    march.len += map.dist;
    march.pos = ray.ori + ray.dir * march.len;

    if ( 1E3 < march.len || abs( map.dist ) < INIT_LEN * 0.01 ) { break; }
  }

  march.normal = normalFunc( march.pos, 1E-4 );

  return march;
}

// ------

vec2 randomCircle() {
  vec2 v = V.xx;
  for ( int i = 0; i < 99; i ++ ) {
    v = random4().xy * 2.0 - 1.0;
    if ( length( v ) < 1.0 ) { break; }
  }
  return v;
}

vec3 randomSphere() {
  vec3 v = V.xxx;
  for ( int i = 0; i < 99; i ++ ) {
    v = random4().xyz * 2.0 - 1.0;
    if ( length( v ) < 1.0 ) { break; }
  }
  v = normalize( v );
  return v;
}

vec3 randomHemisphere( in vec3 _normal ) {
  vec3 v = randomSphere();
  if ( dot( v, _normal ) < 0.0 ) { v = -v; }
  return v;
}

Ray shade( in March _march, inout vec3 colorAdd, inout vec3 colorMul ) {
  March march = _march;

  if ( abs( march.map.dist ) < 1E-2 ) {
    bool inside = march.ray.inside;
    vec3 normal = march.normal;
    float edge = length( saturate( ( normalFunc( march.pos, 4E-4 ) - normal ) * 4.0 ) );

    normal = inside ? -normal : normal;
    Material material = march.map.material;

    vec3 dir = V.xxx;
    float dice = random4().x;

    // colorAdd += colorMul * max( 0.0, dot( normal, -march.ray.dir ) ) * march.map.material.emissive;
    colorAdd += colorMul * march.map.material.emissive;
    colorAdd += colorMul * edge * march.map.material.edgeEmissive;

    colorMul *= march.map.material.color;
    if ( dice < material.reflective ) { // reflect
      vec3 ref = normalize( reflect(
        march.ray.dir,
        normal
      ) );
      vec3 dif = randomHemisphere( normal );
      dir = normalize( mix(
        ref,
        dif,
        material.reflectiveRoughness
      ) );
      colorMul *= max( dot( dir, ref ), 0.0 );

    } else if ( dice < material.reflective + material.refractive ) { // refract
      vec3 inc = normalize( march.ray.dir );
      bool toAir = ( 0.0 < dot( normal, inc ) );
      float eta = 1.0 / march.map.material.refractiveIndex;
      eta = inside ? 1.0 / eta : eta;

      dir = refract(
        inc,
        toAir ? -normal : normal,
        toAir ? 1.0 / eta : eta
      );
      dir = ( dir == V.xxx )
      ? ( normalize( reflect(
        march.ray.dir,
        normal
      ) ) )
      : normalize( dir );
      inside = !inside;

    } else { // diffuse
      dir = randomHemisphere( normal );
      colorMul *= max( dot( dir, normal ), 0.0 );
    }

    Ray ray = rayInit( march.pos, dir );
    ray.inside = inside;
    return ray;
  } else {
    colorAdd += colorMul * SKY_COLOR;
    colorMul *= 0.0;

    return rayInit( V.xxy, V.xxy );
  }
}

// ------

void main() {
  vec2 uv = gl_FragCoord.xy / resolution;
  seed = texture2D( textureRandom, gl_FragCoord.xy / resolution );

  vec4 tex0 = texture2D( textureDrawBuffers0, uv );
  vec4 tex1 = texture2D( textureDrawBuffers1, uv );
  vec4 tex2 = texture2D( textureDrawBuffers2, uv );
  vec4 tex3 = texture2D( textureDrawBuffers3, uv );

  vec3 colorAdd = tex1.xyz;
  vec3 colorMul = tex2.xyz;
  vec3 colorOut = tex3.xyz;
  float depth = abs( tex2.w );
  float samples = abs( tex3.w );

  Ray ray;
  vec3 dir = vec3( tex0.w, tex1.w, 0.0 );
  dir.z = sqrt( 1.0 - dot( dir, dir ) ) * sign( tex2.w );
  ray = rayInit( tex0.xyz, dir );
  ray.inside = 0.0 < tex3.w;

  if ( reset ) {
    colorOut = V.xxx;
    samples = 0.0;
  }

  for ( int i = 0; i < REFLECT_PER_PATH; i ++ ) {

    if ( reset || REFLECT_MAX <= depth || length( colorMul ) < RAYAMP_MIN ) {
      samples += 1.0;
      depth = 1.0;

      colorOut = mix(
        colorOut,
        max( V.xxx, colorAdd ),
        1.0 / samples
      );

      // ------

      float camRot = sin( time * PI * 2.0 ) * 0.1;
      Camera cam = camInit(
        vec3( 5.0 * sin( camRot ), -0.2, 5.0 * cos( camRot ) ),
        vec3( 0.0, 0.0, 0.0 )
      );

      // dof
      vec2 dofCirc = randomCircle() * 0.01;
      cam.pos += dofCirc.x * cam.sid;
      cam.pos += dofCirc.y * cam.top;

      cam = camInit( cam.pos, vec3( 0.0, 0.0, 0.0 ) );

      // antialias
      vec2 pix = gl_FragCoord.xy + random4().xy - 0.5;

      vec2 p = ( pix * 2.0 - resolution ) / resolution.x;
      ray = rayFromCam( p, cam );

      colorAdd = V.xxx;
      colorMul = V.yyy;
    } else {
      depth += 1.0;
    }

    March m = march( ray );
    ray = shade( m, colorAdd, colorMul );

  }

  gl_FragData[ 0 ] = vec4( ray.ori, ray.dir.x );
  gl_FragData[ 1 ] = vec4( colorAdd, ray.dir.y );
  gl_FragData[ 2 ] = vec4( colorMul, depth * ( ( 0.0 < ray.dir.z ) ? 1.0 : -1.0 ) );
  gl_FragData[ 3 ] = vec4( colorOut, samples * ( ray.inside ? 1.0 : -1.0 ) );
}
