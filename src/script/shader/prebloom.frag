precision highp float;

uniform vec2 resolution;
uniform sampler2D texture;

float smin( float _a, float _b, float _k ) {
  float h = clamp( 0.5 + 0.5 * ( _b - _a ) / _k, 0.0, 1.0 );
  return mix( _b, _a, h ) - _k * h * ( 1.0 - h );
}

float smax( float _a, float _b, float _k ) {
  return -smin( -_a, -_b, _k );
}

void main() {
  vec2 uv = gl_FragCoord.xy / resolution;
  vec3 tex = texture2D( texture, uv ).xyz;
  vec3 col = vec3(
    smax( 0.0, tex.x - 0.5, 0.1 ),
    smax( 0.0, tex.y - 0.5, 0.1 ),
    smax( 0.0, tex.z - 0.5, 0.1 )
  );
  gl_FragColor = vec4( col, 1.0 );
}
