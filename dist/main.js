(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160922/src/script/glcat.js":[function(require,module,exports){
'use strict';

Object.defineProperty(exports, "__esModule", {
	value: true
});

var _createClass = function () {
	function defineProperties(target, props) {
		for (var i = 0; i < props.length; i++) {
			var descriptor = props[i];descriptor.enumerable = descriptor.enumerable || false;descriptor.configurable = true;if ("value" in descriptor) descriptor.writable = true;Object.defineProperty(target, descriptor.key, descriptor);
		}
	}return function (Constructor, protoProps, staticProps) {
		if (protoProps) defineProperties(Constructor.prototype, protoProps);if (staticProps) defineProperties(Constructor, staticProps);return Constructor;
	};
}();

function _classCallCheck(instance, Constructor) {
	if (!(instance instanceof Constructor)) {
		throw new TypeError("Cannot call a class as a function");
	}
}

var GLCat = function () {
	function GLCat(_gl) {
		_classCallCheck(this, GLCat);

		var it = this;

		it.gl = _gl;
		var gl = it.gl;

		gl.enable(gl.DEPTH_TEST);
		gl.depthFunc(gl.LEQUAL);
		gl.enable(gl.BLEND);
		gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

		gl.getExtension('OES_texture_float');
		gl.getExtension('OES_float_linear');
		gl.getExtension('OES_half_float_linear');

		it.extDrawBuffers = gl.getExtension('WEBGL_draw_buffers');

		it.currentProgram = null;
	}

	_createClass(GLCat, [{
		key: 'createProgram',
		value: function createProgram(_vert, _frag, _onError) {

			var it = this;
			var gl = it.gl;

			var error = void 0;
			if (typeof _onError === 'function') {
				error = _onError;
			} else {
				error = function error(_str) {
					console.error(_str);
				};
			}

			var vert = gl.createShader(gl.VERTEX_SHADER);
			gl.shaderSource(vert, _vert);
			gl.compileShader(vert);
			if (!gl.getShaderParameter(vert, gl.COMPILE_STATUS)) {
				error(gl.getShaderInfoLog(vert));
				return null;
			}

			var frag = gl.createShader(gl.FRAGMENT_SHADER);
			gl.shaderSource(frag, _frag);
			gl.compileShader(frag);
			if (!gl.getShaderParameter(frag, gl.COMPILE_STATUS)) {
				error(gl.getShaderInfoLog(frag));
				return null;
			}

			var program = gl.createProgram();
			gl.attachShader(program, vert);
			gl.attachShader(program, frag);
			gl.linkProgram(program);
			if (gl.getProgramParameter(program, gl.LINK_STATUS)) {
				program.locations = {};
				return program;
			} else {
				error(gl.getProgramInfoLog(program));
				return null;
			}
		}
	}, {
		key: 'useProgram',
		value: function useProgram(_program) {

			var it = this;
			var gl = it.gl;

			gl.useProgram(_program);
			it.currentProgram = _program;
		}
	}, {
		key: 'createVertexbuffer',
		value: function createVertexbuffer(_array) {

			var it = this;
			var gl = it.gl;

			var buffer = gl.createBuffer();

			gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
			gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(_array), gl.STATIC_DRAW);
			gl.bindBuffer(gl.ARRAY_BUFFER, null);

			buffer.length = _array.length;
			return buffer;
		}
	}, {
		key: 'createIndexbuffer',
		value: function createIndexbuffer(_array) {

			var it = this;
			var gl = it.gl;

			var buffer = gl.createBuffer();

			gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, buffer);
			gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, new Int16Array(_array), gl.STATIC_DRAW);
			gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, null);

			buffer.length = _array.length;
			return buffer;
		}
	}, {
		key: 'attribute',
		value: function attribute(_name, _buffer, _stride) {

			var it = this;
			var gl = it.gl;

			var location = void 0;
			if (it.currentProgram.locations[_name]) {
				location = it.currentProgram.locations[_name];
			} else {
				location = gl.getAttribLocation(it.currentProgram, _name);
				it.currentProgram.locations[_name] = location;
			}

			gl.bindBuffer(gl.ARRAY_BUFFER, _buffer);
			gl.enableVertexAttribArray(location);
			gl.vertexAttribPointer(location, _stride, gl.FLOAT, false, 0, 0);

			gl.bindBuffer(gl.ARRAY_BUFFER, null);
		}
	}, {
		key: 'getUniformLocation',
		value: function getUniformLocation(_name) {

			var it = this;
			var gl = it.gl;

			var location = void 0;

			if (it.currentProgram.locations[_name]) {
				location = it.currentProgram.locations[_name];
			} else {
				location = gl.getUniformLocation(it.currentProgram, _name);
				it.currentProgram.locations[_name] = location;
			}

			return location;
		}
	}, {
		key: 'uniform1i',
		value: function uniform1i(_name, _value) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.uniform1i(location, _value);
		}
	}, {
		key: 'uniform1f',
		value: function uniform1f(_name, _value) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.uniform1f(location, _value);
		}
	}, {
		key: 'uniform2fv',
		value: function uniform2fv(_name, _value) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.uniform2fv(location, _value);
		}
	}, {
		key: 'uniform3fv',
		value: function uniform3fv(_name, _value) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.uniform3fv(location, _value);
		}
	}, {
		key: 'uniform4fv',
		value: function uniform4fv(_name, _value) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.uniform4fv(location, _value);
		}
	}, {
		key: 'uniformCubemap',
		value: function uniformCubemap(_name, _texture, _number) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.activeTexture(gl.TEXTURE0 + _number);
			gl.bindTexture(gl.TEXTURE_CUBE_MAP, _texture);
			gl.uniform1i(location, _number);
		}
	}, {
		key: 'uniformTexture',
		value: function uniformTexture(_name, _texture, _number) {

			var it = this;
			var gl = it.gl;

			var location = it.getUniformLocation(_name);
			gl.activeTexture(gl.TEXTURE0 + _number);
			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.uniform1i(location, _number);
		}
	}, {
		key: 'createTexture',
		value: function createTexture() {

			var it = this;
			var gl = it.gl;

			var texture = gl.createTexture();
			gl.bindTexture(gl.TEXTURE_2D, texture);
			gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
			gl.bindTexture(gl.TEXTURE_2D, null);

			return texture;
		}
	}, {
		key: 'textureFilter',
		value: function textureFilter(_texture, _filter) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, _filter);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, _filter);
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'textureWrap',
		value: function textureWrap(_texture, _wrap) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, _wrap);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, _wrap);
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'setTexture',
		value: function setTexture(_texture, _image) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, _image);
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'setTextureFromArray',
		value: function setTextureFromArray(_texture, _width, _height, _array) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.UNSIGNED_BYTE, new Uint8Array(_array));
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'setTextureFromFloatArray',
		value: function setTextureFromFloatArray(_texture, _width, _height, _array) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.FLOAT, new Float32Array(_array));
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'copyTexture',
		value: function copyTexture(_texture, _width, _height) {

			var it = this;
			var gl = it.gl;

			gl.bindTexture(gl.TEXTURE_2D, _texture);
			gl.copyTexImage2D(gl.TEXTURE_2D, 0, gl.RGBA, 0, 0, _width, _height, 0);
			gl.bindTexture(gl.TEXTURE_2D, null);
		}
	}, {
		key: 'createCubemap',
		value: function createCubemap(_arrayOfImage) {

			var it = this;
			var gl = it.gl;

			// order : X+, X-, Y+, Y-, Z+, Z-
			var texture = gl.createTexture();

			gl.bindTexture(gl.TEXTURE_CUBE_MAP, texture);
			for (var i = 0; i < 6; i++) {
				gl.texImage2D(gl.TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, _arrayOfImage[i]);
			}
			gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
			gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
			gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
			gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
			gl.texParameteri(gl.TEXTURE_CUBE_MAP, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
			gl.bindTexture(gl.TEXTURE_CUBE_MAP, null);

			return texture;
		}
	}, {
		key: 'createFramebuffer',
		value: function createFramebuffer(_width, _height) {

			var it = this;
			var gl = it.gl;

			var framebuffer = {};
			framebuffer.framebuffer = gl.createFramebuffer();
			gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer.framebuffer);

			framebuffer.depth = gl.createRenderbuffer();
			gl.bindRenderbuffer(gl.RENDERBUFFER, framebuffer.depth);
			gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, _width, _height);
			gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, framebuffer.depth);

			framebuffer.texture = it.createTexture();
			gl.bindTexture(gl.TEXTURE_2D, framebuffer.texture);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
			gl.bindTexture(gl.TEXTURE_2D, null);

			gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, framebuffer.texture, 0);
			gl.bindFramebuffer(gl.FRAMEBUFFER, null);

			return framebuffer;
		}
	}, {
		key: 'resizeFramebuffer',
		value: function resizeFramebuffer(_framebuffer, _width, _height) {

			var it = this;
			var gl = it.gl;

			gl.bindFramebuffer(gl.FRAMEBUFFER, _framebuffer.framebuffer);
			gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, _width, _height);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
			gl.bindFramebuffer(gl.FRAMEBUFFER, null);
		}
	}, {
		key: 'createFloatFramebuffer',
		value: function createFloatFramebuffer(_width, _height) {

			var it = this;
			var gl = it.gl;

			var framebuffer = {};
			framebuffer.framebuffer = gl.createFramebuffer();
			gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer.framebuffer);

			framebuffer.depth = gl.createRenderbuffer();
			gl.bindRenderbuffer(gl.RENDERBUFFER, framebuffer.depth);
			gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, _width, _height);
			gl.framebufferRenderbuffer(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.RENDERBUFFER, framebuffer.depth);

			framebuffer.texture = it.createTexture();
			gl.bindTexture(gl.TEXTURE_2D, framebuffer.texture);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.FLOAT, null);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
			gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
			gl.bindTexture(gl.TEXTURE_2D, null);

			gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, framebuffer.texture, 0);
			gl.bindFramebuffer(gl.FRAMEBUFFER, null);

			return framebuffer;
		}
	}, {
		key: 'resizeFloatFramebuffer',
		value: function resizeFloatFramebuffer(_framebuffer, _width, _height) {

			var it = this;
			var gl = it.gl;

			gl.bindFramebuffer(gl.FRAMEBUFFER, _framebuffer.framebuffer);
			gl.renderbufferStorage(gl.RENDERBUFFER, gl.DEPTH_COMPONENT16, _width, _height);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.FLOAT, null);
			gl.bindFramebuffer(gl.FRAMEBUFFER, null);
		}
	}, {
		key: 'createDrawBuffers',
		value: function createDrawBuffers(_width, _height, _numDrawBuffers) {

			var it = this;
			var gl = it.gl;
			var ext = it.extDrawBuffers;

			if (ext.MAX_DRAW_BUFFERS_WEBGL < _numDrawBuffers) {
				throw "createDrawBuffers: MAX_DRAW_BUFFERS_WEBGL is " + ext.MAX_DRAW_BUFFERS_WEBGL;
			}

			var framebuffer = {};
			framebuffer.framebuffer = gl.createFramebuffer();
			gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer.framebuffer);

			framebuffer.textures = [];
			for (var i = 0; i < _numDrawBuffers; i++) {
				framebuffer.textures[i] = it.createTexture();
				gl.bindTexture(gl.TEXTURE_2D, framebuffer.textures[i]);
				gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, _width, _height, 0, gl.RGBA, gl.FLOAT, null);
				gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
				gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
				gl.bindTexture(gl.TEXTURE_2D, null);

				gl.framebufferTexture2D(gl.FRAMEBUFFER, ext.COLOR_ATTACHMENT0_WEBGL + i, gl.TEXTURE_2D, framebuffer.textures[i], 0);
			}

			var status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
			if (status !== gl.FRAMEBUFFER_COMPLETE) {
				throw "createDrawBuffers: gl.checkFramebufferStatus( gl.FRAMEBUFFER ) is " + status;
			}
			gl.bindFramebuffer(gl.FRAMEBUFFER, null);

			return framebuffer;
		}
	}, {
		key: 'drawBuffers',
		value: function drawBuffers(_numDrawBuffers) {

			var it = this;
			var gl = it.gl;
			var ext = it.extDrawBuffers;

			var array = [];
			if (typeof _numDrawBuffers === 'number') {
				for (var i = 0; i < _numDrawBuffers; i++) {
					array.push(ext.COLOR_ATTACHMENT0_WEBGL + i);
				}
			} else {
				array = array.concat(_numDrawBuffers);
			}
			ext.drawBuffersWEBGL(array);
		}
	}, {
		key: 'clear',
		value: function clear(_r, _g, _b, _a, _d) {

			var it = this;
			var gl = it.gl;

			var r = _r || 0.0;
			var g = _g || 0.0;
			var b = _b || 0.0;
			var a = typeof _a === 'number' ? _a : 1.0;
			var d = typeof _d === 'number' ? _d : 1.0;

			gl.clearColor(r, g, b, a);
			gl.clearDepth(d);
			gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
		}
	}, {
		key: 'render',
		value: function render(_props) {

			var it = this;
			var gl = it.gl;

			if (_props.viewport) {
				gl.viewport(_props.viewport[0], _props.viewport[1], _props.viewport[2], _props.viewport[3]);
			}
			if (_props.program) {
				it.useProgram(_props.program);
			}
			if (_props.framebuffer) {
				gl.bindFramebuffer(gl.FRAMEBUFFER, _props.framebuffer);
			}

			var clearBit = 0;
			if (_props.clearColor) {
				gl.clearColor(_props.clearColor[0], _props.clearColor[1], _props.clearColor[2], _props.clearColor[3]);
				clearBit = clearBit | gl.COLOR_BUFFER_BIT;
			}
			if (_props.clearDepth) {
				gl.clearDepth(gl.clearDepth);
				clearBit = clearBit | gl.DEPTH_BUFFER_BIT;
			}
			gl.clear;
		}
	}]);

	return GLCat;
}();

exports.default = GLCat;

},{}],"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160922/src/script/main.js":[function(require,module,exports){
'use strict';

var _xorshift = require('./xorshift');

var _xorshift2 = _interopRequireDefault(_xorshift);

var _glcat = require('./glcat');

var _glcat2 = _interopRequireDefault(_glcat);

var _step = require('./step');

var _step2 = _interopRequireDefault(_step);

var _tweak = require('./tweak');

var _tweak2 = _interopRequireDefault(_tweak);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

(0, _xorshift2.default)(187);




// ------

var clamp = function clamp(_value, _min, _max) {
  return Math.min(Math.max(_value, _min), _max);
};
var saturate = function saturate(_value) {
  return clamp(_value, 0.0, 1.0);
};

// ------

var width = canvas.width = 300;
var height = canvas.height = 300;

var gl = canvas.getContext('webgl');
var glCat = new _glcat2.default(gl);

// ------

var tweak = new _tweak2.default(divTweak);

// ------

var frame = 0;
var frames = 120;
var iSample = 0;
var nSample = 1;
var time = 0.0;
var deltaTime = 1.0 / 60.0;

var timeUpdate = function timeUpdate() {
  var timePrev = time;
  time = (frame + iSample / nSample) / frames;
  time = time % 1.0;
};

// ------

var vboQuad = glCat.createVertexbuffer([-1, -1, 1, -1, -1, 1, 1, 1]);

// ------

var vertQuad = "#define GLSLIFY 1\nattribute vec2 p;\n\nvoid main() {\n  gl_Position = vec4( p, 0.0, 1.0 );\n}\n";

var programReturn = glCat.createProgram(vertQuad, "precision highp float;\n#define GLSLIFY 1\n\nuniform vec2 resolution;\nuniform sampler2D texture;\nuniform bool hoge;\n\nvoid main() {\n  vec2 uv = gl_FragCoord.xy / resolution;\n  gl_FragColor = texture2D( texture, uv );\n}\n");

var programReturnDb = glCat.createProgram(vertQuad, "#extension GL_EXT_draw_buffers : require\nprecision highp float;\n#define GLSLIFY 1\n\nuniform vec2 resolution;\nuniform sampler2D texture0;\nuniform sampler2D texture1;\nuniform sampler2D texture2;\nuniform sampler2D texture3;\n\nvoid main() {\n  vec2 uv = gl_FragCoord.xy / resolution;\n  gl_FragData[ 0 ] = texture2D( texture0, uv );\n  gl_FragData[ 1 ] = texture2D( texture1, uv );\n  gl_FragData[ 2 ] = texture2D( texture2, uv );\n  gl_FragData[ 3 ] = texture2D( texture3, uv );\n}\n");

var programRender = glCat.createProgram(vertQuad, "#define MARCH_ITER 100\n#define RAYAMP_MIN 0.01\n#define REFLECT_MAX 10.0\n#define REFLECT_PER_PATH 3\n#define INIT_LEN 0.01\n#define SKY_COLOR vec3( 0.0 )\n\n// ------\n\n#define PI 3.14159265\n#define V vec2(0.,1.)\n#define saturate(i) clamp(i,0.,1.)\n#define lofi(i,m) (floor((i)/(m))*(m))\n\n// ------\n\n#extension GL_EXT_draw_buffers : require\nprecision highp float;\n#define GLSLIFY 1\n\nuniform float time;\nuniform vec2 resolution;\nuniform bool reset;\n\nuniform sampler2D textureRandom;\nuniform sampler2D textureRandomStatic;\n\nuniform sampler2D textureDrawBuffers0;\nuniform sampler2D textureDrawBuffers1;\nuniform sampler2D textureDrawBuffers2;\nuniform sampler2D textureDrawBuffers3;\n\nuniform sampler2D textureImageArrow;\nuniform sampler2D textureImageVertical;\nuniform sampler2D textureImageUtsunomiya;\nuniform sampler2D textureImageFoot;\nuniform sampler2D textureImageStop;\nuniform sampler2D textureImageFive;\nuniform sampler2D textureImageSix;\nuniform sampler2D textureImageThirtythree;\nuniform sampler2D textureImageSuica;\n\n// ------\n\nvec4 seed;\nfloat random() { // weird prng\n  const vec4 q = vec4(   1225.0,    1585.0,    2457.0,    2098.0);\n  const vec4 r = vec4(   1112.0,     367.0,      92.0,     265.0);\n  const vec4 a = vec4(   3423.0,    2646.0,    1707.0,    1999.0);\n  const vec4 m = vec4(4194287.0, 4194277.0, 4194191.0, 4194167.0);\n\n  vec4 beta = floor(seed / q);\n  vec4 p = a * (seed - beta * q) - beta * r;\n  beta = (sign(-p) + vec4(1.0)) * vec4(0.5) * m;\n  seed = (p + beta);\n\n  return fract(dot(seed / m, vec4(1.0, -1.0, 1.0, -1.0)));\n}\n\nvec4 random4() {\n  return vec4(\n    random(),\n    random(),\n    random(),\n    random()\n  );\n}\n\n// ------\n\nmat2 rotate2D( float _t ) {\n  return mat2( cos( _t ), sin( _t ), -sin( _t ), cos( _t ) );\n}\n\nbool isUvValid( vec2 _v ) {\n  return ( 0.0 <= _v.x ) && ( _v.x <= 1.0 ) && ( 0.0 <= _v.y ) && ( _v.y <= 1.0 );\n}\n\n// ------\n\nstruct Camera {\n  vec3 pos;\n  vec3 dir;\n  vec3 sid;\n  vec3 top;\n};\n\nstruct Ray {\n  vec3 dir;\n  vec3 ori;\n  bool inside;\n};\n\nstruct Material {\n  vec3 color;\n\n  vec3 emissive;\n  vec3 edgeEmissive;\n\n  float reflective;\n  float reflectiveRoughness;\n  float refractive;\n  float refractiveIndex;\n};\n\nstruct Map {\n  float dist;\n  Material material;\n};\n\nstruct March {\n  Ray ray;\n  Map map;\n  float len;\n  vec3 pos;\n  vec3 normal;\n};\n\n// ------\n\nCamera camInit( in vec3 _pos, in vec3 _tar ) {\n  Camera cam;\n  cam.pos = _pos;\n  cam.dir = normalize( _tar - _pos );\n  cam.sid = normalize( cross( cam.dir, V.xyx ) );\n  cam.top = normalize( cross( cam.sid, cam.dir ) );\n\n  return cam;\n}\n\nMap distFunc( in vec3 _p );\nRay rayInit( in vec3 _ori, in vec3 _dir ) {\n  Ray ray;\n  ray.dir = _dir;\n  ray.ori = _ori;\n  ray.inside = distFunc( ray.ori ).dist < 0.0;\n  return ray;\n}\n\nRay rayFromCam( in vec2 _p, in Camera _cam ) {\n  vec3 dir = normalize( _p.x * _cam.sid + _p.y * _cam.top + _cam.dir * 2.0 );\n  return rayInit( _cam.pos, dir );\n}\n\nMaterial mtlInit( in vec3 _col ) {\n  Material material;\n  material.color = _col;\n\n  material.emissive = V.xxx;\n  material.edgeEmissive = V.xxx;\n\n  material.reflective = 0.0;\n  material.reflectiveRoughness = 0.0;\n  material.refractive = 0.0;\n  material.refractiveIndex = 1.0;\n\n  return material;\n}\n\nMap mapInit( in float _dist ) {\n  Map map;\n  map.dist = _dist;\n  map.material = mtlInit( V.xxx );\n  return map;\n}\n\nMarch marchInit( in Ray _ray ) {\n  March march;\n  march.ray = _ray;\n  march.len = INIT_LEN;\n  march.pos = _ray.ori + _ray.dir * march.len;\n  return march;\n}\n\n// ------\n\nfloat sphere( in vec3 _p, in float _r ) {\n  return length( _p ) - _r;\n}\n\nfloat box( in vec3 _p, in vec3 _size ) {\n  vec3 d = abs( _p ) - _size;\n  return min( max( d.x, max( d.y, d.z ) ), 0.0 ) + length( max( d, 0.0 ) );\n}\n\nMap distFunc( in vec3 _p ) {\n  Map map = mapInit( 1E9 );\n\n  { // floor, roof\n    vec3 p = _p;\n    p.z = mod( p.z, 2.0 ) - 1.0;\n    float dist = 5.0 - abs( p.y );\n    dist = max( dist, 0.2 - abs( p.z ) );\n\n    if ( dist < map.dist ) {\n      map = mapInit( dist );\n      map.material = mtlInit( vec3( 0.4 ) );\n      // map.material.reflective = 0.1;\n    }\n  }\n\n  { // arrow\n    vec3 p = _p;\n    p -= vec3( -0.4, 1.1, 1.5 );\n    p.zx = rotate2D( 0.1 * sin( time * PI * 2.0 + 4.0 ) ) * p.zx;\n    p.yz = rotate2D( 0.1 * sin( time * PI * 2.0 + 2.0 ) ) * p.yz;\n    float dist = box( p, vec3( 1.0 + 0.02, 0.3 + 0.02, 0.2 ) );\n\n    if ( dist < map.dist ) {\n      map = mapInit( dist );\n      vec2 uv = p.xy / vec2( 1.0, 0.3 ) / 2.0 + 0.5;\n      vec3 tex = texture2D( textureImageArrow, uv ).xyz;\n\n      map.material = mtlInit( vec3( 0.0 ) );\n      if ( isUvValid( uv ) ) {\n        map.material.color = tex;\n        map.material.reflective = 0.2;\n        map.material.emissive = tex * 1.0;\n      } else {\n        map.material.color = vec3( 0.2 );\n        map.material.reflective = 0.2;\n        map.material.reflectiveRoughness = 0.05;\n      }\n    }\n  }\n\n  { // vertical\n    vec3 p = _p;\n    p -= vec3( -0.7, -0.3, 2.0 );\n    p.zx = rotate2D( 0.4 + 0.1 * sin( time * PI * 2.0 ) ) * p.zx;\n    p.xy = rotate2D( 0.1 * sin( time * PI * 2.0 + 1.0 ) ) * p.xy;\n    float dist = box( p, vec3( 0.1, 0.5, 0.02 ) );\n\n    if ( dist < map.dist ) {\n      map = mapInit( dist );\n      vec2 uv = p.xy / vec2( 0.1, 0.5 ) / 2.0 + 0.5;\n      vec3 tex = texture2D( textureImageVertical, uv ).xyz;\n\n      map.material = mtlInit( vec3( 0.0 ) );\n      map.material.color = tex;\n      map.material.reflective = 0.1;\n    }\n  }\n\n  { // foot\n    vec3 p = _p;\n    p -= vec3( -0.6, -0.9, 3.0 );\n    p.yz = rotate2D( 0.4 + 0.1 * sin( time * PI * 2.0 + 2.0 ) ) * p.yz;\n    p.xy = rotate2D( 0.1 * sin( time * PI * 2.0 + 3.0 ) ) * p.xy;\n    float dist = box( p, vec3( 0.5, 0.1, 0.02 ) );\n\n    if ( dist < map.dist ) {\n      map = mapInit( dist );\n      vec2 uv = p.xy / vec2( 0.5, 0.1 ) / 2.0 + 0.5;\n      vec3 tex = texture2D( textureImageFoot, uv ).xyz;\n\n      map.material = mtlInit( vec3( 0.0 ) );\n      map.material.color = tex;\n    }\n  }\n\n  { // utsunomiya\n    vec3 p = _p;\n    p -= vec3( 0.5, 0.0, 1.7 );\n    p.zx = rotate2D( -0.2 + 0.1 * sin( time * PI * 2.0 + 3.0 ) ) * p.zx;\n    p.yz = rotate2D( -0.2 + 0.1 * sin( time * PI * 2.0 + 1.0 ) ) * p.yz;\n    float dist = box( p, vec3( 0.5 + 0.01, 0.2 + 0.01, 0.1 ) );\n\n    if ( dist < map.dist ) {\n      map = mapInit( dist );\n      vec2 uv = p.xy / vec2( 0.5, 0.2 ) / 2.0 + 0.5;\n      vec3 tex = texture2D( textureImageUtsunomiya, uv ).xyz;\n\n      map.material = mtlInit( vec3( 0.0 ) );\n      if ( isUvValid( uv ) ) {\n        map.material.color = tex;\n        map.material.reflective = 0.2;\n        map.material.emissive = tex * 1.0;\n      } else {\n        map.material.color = vec3( 0.2 );\n        map.material.reflective = 0.2;\n        map.material.reflectiveRoughness = 0.05;\n      }\n    }\n  }\n\n  { // stop\n    vec3 p = _p;\n    p -= vec3( 1.2, 0.5, 2.4 );\n    p.zx = rotate2D( 0.4 + 0.1 * sin( time * PI * 2.0 + 2.0 ) ) * p.zx;\n    p.yz = rotate2D( -0.2 + 0.1 * sin( time * PI * 2.0 + 1.0 ) ) * p.yz;\n    float dist = box( p, vec3( 0.2, 0.5, 0.02 ) );\n\n    if ( dist < map.dist ) {\n      map = mapInit( dist );\n      vec2 uv = p.xy / vec2( 0.2, 0.5 ) / 2.0 + 0.5;\n      vec3 tex = texture2D( textureImageStop, uv ).xyz;\n\n      map.material = mtlInit( vec3( 0.0 ) );\n      map.material.color = tex;\n      map.material.reflective = 0.04;\n      map.material.reflectiveRoughness = 0.02;\n    }\n  }\n\n  { // five\n    vec3 p = _p;\n    p -= vec3( 0.8, -0.7, 1.4 );\n    p.yz = rotate2D( 0.2 + 0.1 * sin( time * PI * 2.0 + 6.0 ) ) * p.yz;\n    p.xy = rotate2D( -0.1 + 0.1 * sin( time * PI * 2.0 + 4.0 ) ) * p.xy;\n    float dist = box( p, vec3( 0.2, 0.2, 0.01 ) );\n\n    if ( dist < map.dist ) {\n      map = mapInit( dist );\n      vec2 uv = p.xy / vec2( 0.2, 0.2 ) / 2.0 + 0.5;\n      vec3 tex = texture2D( textureImageFive, uv ).xyz;\n\n      map.material = mtlInit( vec3( 0.0 ) );\n      map.material.color = tex;\n    }\n  }\n\n  { // six\n    vec3 p = _p;\n    p -= vec3( 0.0, -0.7, 0.4 );\n    p.yz = rotate2D( 0.1 * sin( time * PI * 2.0 + 2.0 ) ) * p.yz;\n    p.xy = rotate2D( 0.4 + 0.1 * sin( time * PI * 2.0 + 3.0 ) ) * p.xy;\n    float dist = box( p, vec3( 0.2, 0.2, 0.01 ) );\n\n    if ( dist < map.dist ) {\n      map = mapInit( dist );\n      vec2 uv = p.xy / vec2( 0.2, 0.2 ) / 2.0 + 0.5;\n      vec3 tex = texture2D( textureImageSix, uv ).xyz;\n\n      map.material = mtlInit( vec3( 0.0 ) );\n      map.material.color = tex;\n    }\n  }\n\n  { // thirtythree\n    vec3 p = _p;\n    p -= vec3( 1.0, -1.1, 2.0 );\n    p.yz = rotate2D( -0.6 + 0.1 * sin( time * PI * 2.0 + 4.0 ) ) * p.yz;\n    p.xy = rotate2D( 0.4 + 0.1 * sin( time * PI * 2.0 + 1.0 ) ) * p.xy;\n    float dist = box( p, vec3( 0.35, 0.2, 0.01 ) );\n\n    if ( dist < map.dist ) {\n      map = mapInit( dist );\n      vec2 uv = p.xy / vec2( 0.35, 0.2 ) / 2.0 + 0.5;\n      vec3 tex = texture2D( textureImageThirtythree, uv ).xyz;\n\n      map.material = mtlInit( vec3( 0.0 ) );\n      map.material.color = tex;\n    }\n  }\n\n  { // neon\n    vec3 p = _p;\n    p.xy = rotate2D( p.z * 0.03 ) * p.xy;\n    p.x = abs( p.x ) - 2.0;\n    p.z = mod( p.z - 2.0, 4.0 ) - 2.0;\n    float dist = length( p.zx ) - 0.1;\n    dist = max( dist, abs( _p.z ) - 25.0 );\n\n    if ( dist < map.dist ) {\n      map = mapInit( dist );\n      map.material = mtlInit( vec3( 0.0 ) );\n      map.material.emissive = vec3( 0.8, 0.9, 1.0 ) * 20.0;\n    }\n  }\n\n  return map;\n}\n\nvec3 normalFunc( in vec3 _p, in float _d ) {\n  vec2 d = V * _d;\n  return normalize( vec3(\n    distFunc( _p + d.yxx ).dist - distFunc( _p - d.yxx ).dist,\n    distFunc( _p + d.xyx ).dist - distFunc( _p - d.xyx ).dist,\n    distFunc( _p + d.xxy ).dist - distFunc( _p - d.xxy ).dist\n  ) );\n}\n\n// ------\n\nMarch march( in Ray _ray ) {\n  Ray ray = _ray;\n  March march = marchInit( ray );\n\n  for ( int iMarch = 0; iMarch < MARCH_ITER; iMarch ++ ) {\n    Map map = distFunc( march.pos );\n    map.dist *= ( ray.inside ? -1.0 : 1.0 ) * 0.8;\n\n    march.map = map;\n    march.len += map.dist;\n    march.pos = ray.ori + ray.dir * march.len;\n\n    if ( 1E3 < march.len || abs( map.dist ) < INIT_LEN * 0.01 ) { break; }\n  }\n\n  march.normal = normalFunc( march.pos, 1E-4 );\n\n  return march;\n}\n\n// ------\n\nvec2 randomCircle() {\n  vec2 v = V.xx;\n  for ( int i = 0; i < 99; i ++ ) {\n    v = random4().xy * 2.0 - 1.0;\n    if ( length( v ) < 1.0 ) { break; }\n  }\n  return v;\n}\n\nvec3 randomSphere() {\n  vec3 v = V.xxx;\n  for ( int i = 0; i < 99; i ++ ) {\n    v = random4().xyz * 2.0 - 1.0;\n    if ( length( v ) < 1.0 ) { break; }\n  }\n  v = normalize( v );\n  return v;\n}\n\nvec3 randomHemisphere( in vec3 _normal ) {\n  vec3 v = randomSphere();\n  if ( dot( v, _normal ) < 0.0 ) { v = -v; }\n  return v;\n}\n\nRay shade( in March _march, inout vec3 colorAdd, inout vec3 colorMul ) {\n  March march = _march;\n\n  if ( abs( march.map.dist ) < 1E-2 ) {\n    bool inside = march.ray.inside;\n    vec3 normal = march.normal;\n    float edge = length( saturate( ( normalFunc( march.pos, 4E-4 ) - normal ) * 4.0 ) );\n\n    normal = inside ? -normal : normal;\n    Material material = march.map.material;\n\n    vec3 dir = V.xxx;\n    float dice = random4().x;\n\n    // colorAdd += colorMul * max( 0.0, dot( normal, -march.ray.dir ) ) * march.map.material.emissive;\n    colorAdd += colorMul * march.map.material.emissive;\n    colorAdd += colorMul * edge * march.map.material.edgeEmissive;\n\n    colorMul *= march.map.material.color;\n    if ( dice < material.reflective ) { // reflect\n      vec3 ref = normalize( reflect(\n        march.ray.dir,\n        normal\n      ) );\n      vec3 dif = randomHemisphere( normal );\n      dir = normalize( mix(\n        ref,\n        dif,\n        material.reflectiveRoughness\n      ) );\n      colorMul *= max( dot( dir, ref ), 0.0 );\n\n    } else if ( dice < material.reflective + material.refractive ) { // refract\n      vec3 inc = normalize( march.ray.dir );\n      bool toAir = ( 0.0 < dot( normal, inc ) );\n      float eta = 1.0 / march.map.material.refractiveIndex;\n      eta = inside ? 1.0 / eta : eta;\n\n      dir = refract(\n        inc,\n        toAir ? -normal : normal,\n        toAir ? 1.0 / eta : eta\n      );\n      dir = ( dir == V.xxx )\n      ? ( normalize( reflect(\n        march.ray.dir,\n        normal\n      ) ) )\n      : normalize( dir );\n      inside = !inside;\n\n    } else { // diffuse\n      dir = randomHemisphere( normal );\n      colorMul *= max( dot( dir, normal ), 0.0 );\n    }\n\n    Ray ray = rayInit( march.pos, dir );\n    ray.inside = inside;\n    return ray;\n  } else {\n    colorAdd += colorMul * SKY_COLOR;\n    colorMul *= 0.0;\n\n    return rayInit( V.xxy, V.xxy );\n  }\n}\n\n// ------\n\nvoid main() {\n  vec2 uv = gl_FragCoord.xy / resolution;\n  seed = texture2D( textureRandom, gl_FragCoord.xy / resolution );\n\n  vec4 tex0 = texture2D( textureDrawBuffers0, uv );\n  vec4 tex1 = texture2D( textureDrawBuffers1, uv );\n  vec4 tex2 = texture2D( textureDrawBuffers2, uv );\n  vec4 tex3 = texture2D( textureDrawBuffers3, uv );\n\n  vec3 colorAdd = tex1.xyz;\n  vec3 colorMul = tex2.xyz;\n  vec3 colorOut = tex3.xyz;\n  float depth = abs( tex2.w );\n  float samples = abs( tex3.w );\n\n  Ray ray;\n  vec3 dir = vec3( tex0.w, tex1.w, 0.0 );\n  dir.z = sqrt( 1.0 - dot( dir, dir ) ) * sign( tex2.w );\n  ray = rayInit( tex0.xyz, dir );\n  ray.inside = 0.0 < tex3.w;\n\n  if ( reset ) {\n    colorOut = V.xxx;\n    samples = 0.0;\n  }\n\n  for ( int i = 0; i < REFLECT_PER_PATH; i ++ ) {\n\n    if ( reset || REFLECT_MAX <= depth || length( colorMul ) < RAYAMP_MIN ) {\n      samples += 1.0;\n      depth = 1.0;\n\n      colorOut = mix(\n        colorOut,\n        max( V.xxx, colorAdd ),\n        1.0 / samples\n      );\n\n      // ------\n\n      float camRot = sin( time * PI * 2.0 ) * 0.1;\n      Camera cam = camInit(\n        vec3( 5.0 * sin( camRot ), -0.2, 5.0 * cos( camRot ) ),\n        vec3( 0.0, 0.0, 0.0 )\n      );\n\n      // dof\n      vec2 dofCirc = randomCircle() * 0.01;\n      cam.pos += dofCirc.x * cam.sid;\n      cam.pos += dofCirc.y * cam.top;\n\n      cam = camInit( cam.pos, vec3( 0.0, 0.0, 0.0 ) );\n\n      // antialias\n      vec2 pix = gl_FragCoord.xy + random4().xy - 0.5;\n\n      vec2 p = ( pix * 2.0 - resolution ) / resolution.x;\n      ray = rayFromCam( p, cam );\n\n      colorAdd = V.xxx;\n      colorMul = V.yyy;\n    } else {\n      depth += 1.0;\n    }\n\n    March m = march( ray );\n    ray = shade( m, colorAdd, colorMul );\n\n  }\n\n  gl_FragData[ 0 ] = vec4( ray.ori, ray.dir.x );\n  gl_FragData[ 1 ] = vec4( colorAdd, ray.dir.y );\n  gl_FragData[ 2 ] = vec4( colorMul, depth * ( ( 0.0 < ray.dir.z ) ? 1.0 : -1.0 ) );\n  gl_FragData[ 3 ] = vec4( colorOut, samples * ( ray.inside ? 1.0 : -1.0 ) );\n}\n");

var programSum = glCat.createProgram(vertQuad, "precision highp float;\n#define GLSLIFY 1\n\nuniform bool init;\nuniform float add;\nuniform vec2 resolution;\nuniform sampler2D textureAdd;\nuniform sampler2D textureBase;\n\nvoid main() {\n  vec2 uv = gl_FragCoord.xy / resolution;\n  vec3 ret = texture2D( textureAdd, uv ).xyz * add;\n  if ( !init ) {\n    ret += texture2D( textureBase, uv ).xyz;\n  }\n  gl_FragColor = vec4( ret, 1.0 );\n}\n");

var programBloom = glCat.createProgram(vertQuad, "#define V vec2(0.,1.)\n#define saturate(i) clamp(i,0.,1.)\n#define PI 3.14159265\n#define SAMPLES 40\n\n// ------\n\nprecision highp float;\n#define GLSLIFY 1\n\nuniform vec2 resolution;\nuniform bool isVert;\nuniform sampler2D texture;\nuniform sampler2D textureBase;\n\nuniform float gaussVar;\n\nfloat gaussian( float _x, float _v ) {\n  return 1.0 / sqrt( 2.0 * PI * _v ) * exp( - _x * _x / 2.0 / _v );\n}\n\nvoid main() {\n  vec2 uv = gl_FragCoord.xy / resolution;\n  vec2 bv = ( isVert ? vec2( 0.0, 1.0 ) : vec2( 1.0, 0.0 ) ) / resolution;\n\n  vec3 sum = V.xxx;\n  for ( int i = -SAMPLES; i <= SAMPLES; i ++ ) {\n    vec2 v = saturate( uv + bv * float( i ) );\n    vec3 tex = texture2D( texture, v ).xyz;\n    float mul = gaussian( float( i ), gaussVar );\n    sum += tex * mul;\n  }\n\n  sum *= 0.1;\n\n  if ( isVert ) {\n    sum += texture2D( textureBase, uv ).xyz;\n  }\n\n  gl_FragColor = vec4( sum, 1.0 );\n}\n");

var programPreBloom = glCat.createProgram(vertQuad, "precision highp float;\n#define GLSLIFY 1\n\nuniform vec2 resolution;\nuniform sampler2D texture;\n\nfloat smin( float _a, float _b, float _k ) {\n  float h = clamp( 0.5 + 0.5 * ( _b - _a ) / _k, 0.0, 1.0 );\n  return mix( _b, _a, h ) - _k * h * ( 1.0 - h );\n}\n\nfloat smax( float _a, float _b, float _k ) {\n  return -smin( -_a, -_b, _k );\n}\n\nvoid main() {\n  vec2 uv = gl_FragCoord.xy / resolution;\n  vec3 tex = texture2D( texture, uv ).xyz;\n  vec3 col = vec3(\n    smax( 0.0, tex.x - 0.5, 0.1 ),\n    smax( 0.0, tex.y - 0.5, 0.1 ),\n    smax( 0.0, tex.z - 0.5, 0.1 )\n  );\n  gl_FragColor = vec4( col, 1.0 );\n}\n");

var programFinal = glCat.createProgram(vertQuad, "precision highp float;\n#define GLSLIFY 1\n\nuniform vec2 resolution;\nuniform sampler2D texture;\n\nfloat gray( vec3 _c ) {\n  return _c.x * 0.299 + _c.y * 0.587 + _c.z * 0.114;\n}\n\nvoid main() {\n  vec2 uv = gl_FragCoord.xy / resolution;\n  vec3 tex = texture2D( texture, uv ).xyz;\n\n  float len = length( gl_FragCoord.xy - resolution / 2.0 ) / resolution.x * sqrt( 2.0 );\n  vec3 vig = mix(\n    tex,\n    vec3( 0.2, 0.3, 0.5 ) * gray( tex ),\n    len\n  );\n\n  vec3 gamma = pow( vig, vec3( 1.0 / 1.7 ) );\n\n  gl_FragColor = vec4( gamma, 1.0 );\n}\n");

// ------

var framebufferDrawBuffers = glCat.createDrawBuffers(width, height, 4);
var framebufferDrawBuffersReturn = glCat.createDrawBuffers(width, height, 4);

var framebufferReturn = glCat.createFloatFramebuffer(width, height);
var framebufferPreBloom = glCat.createFloatFramebuffer(width, height);
var framebufferBloom = glCat.createFloatFramebuffer(width, height);
var framebufferBloomTemp = glCat.createFloatFramebuffer(width, height);

// ------

var textureRandomSize = 256;

var textureRandomUpdate = function textureRandomUpdate(_tex) {
  glCat.setTextureFromArray(_tex, textureRandomSize, textureRandomSize, function () {
    var len = textureRandomSize * textureRandomSize * 4;
    var ret = new Uint8Array(len);
    for (var i = 0; i < len; i++) {
      ret[i] = Math.floor((0, _xorshift2.default)() * 256.0);
    }
    return ret;
  }());
};

var textureRandom = glCat.createTexture();
glCat.textureWrap(textureRandom, gl.REPEAT);

var textureRandomStatic = glCat.createTexture();
glCat.textureWrap(textureRandomStatic, gl.REPEAT);
textureRandomUpdate(textureRandomStatic);

// ------

var loadImage = function loadImage(_url, _callback) {
  var image = new Image();
  image.onload = function () {
    _callback(image);
  };
  image.src = _url;
};

var textureImageArrow = glCat.createTexture();
var textureImageVertical = glCat.createTexture();
var textureImageUtsunomiya = glCat.createTexture();
var textureImageStop = glCat.createTexture();
var textureImageFoot = glCat.createTexture();
var textureImageFive = glCat.createTexture();
var textureImageSix = glCat.createTexture();
var textureImageThirtythree = glCat.createTexture();
var textureImageSuica = glCat.createTexture();

// ------

var renderA = document.createElement('a');

var saveFrame = function saveFrame() {
  renderA.href = canvas.toDataURL();
  renderA.download = ('0000' + frame).slice(-5) + '.png';
  renderA.click();
};

// ------

var render = function render() {
  gl.viewport(0, 0, width, height);
  glCat.useProgram(programReturnDb);
  gl.bindFramebuffer(gl.FRAMEBUFFER, framebufferDrawBuffersReturn.framebuffer);
  glCat.drawBuffers(framebufferDrawBuffersReturn.textures.length);
  gl.blendFunc(gl.ONE, gl.ONE);
  glCat.clear(0.0, 0.0, 0.0, 0.0);

  glCat.attribute('p', vboQuad, 2);

  glCat.uniform1f('time', time);
  glCat.uniform2fv('resolution', [width, height]);

  glCat.uniformTexture('texture0', framebufferDrawBuffers.textures[0], 0);
  glCat.uniformTexture('texture1', framebufferDrawBuffers.textures[1], 1);
  glCat.uniformTexture('texture2', framebufferDrawBuffers.textures[2], 2);
  glCat.uniformTexture('texture3', framebufferDrawBuffers.textures[3], 3);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

  // ------

  gl.viewport(0, 0, width, height);
  glCat.useProgram(programRender);
  gl.bindFramebuffer(gl.FRAMEBUFFER, framebufferDrawBuffers.framebuffer);
  glCat.drawBuffers(framebufferDrawBuffers.textures.length);
  gl.blendFunc(gl.ONE, gl.ONE);
  glCat.clear(0.0, 0.0, 0.0, 0.0);

  glCat.attribute('p', vboQuad, 2);

  glCat.uniform1f('time', time);
  glCat.uniform2fv('resolution', [width, height]);
  glCat.uniform1i('reset', iSample === 0);

  glCat.uniformTexture('textureRandom', textureRandom, 0);
  glCat.uniformTexture('textureRandomStatic', textureRandomStatic, 1);

  glCat.uniformTexture('textureDrawBuffers0', framebufferDrawBuffersReturn.textures[0], 2);
  glCat.uniformTexture('textureDrawBuffers1', framebufferDrawBuffersReturn.textures[1], 3);
  glCat.uniformTexture('textureDrawBuffers2', framebufferDrawBuffersReturn.textures[2], 4);
  glCat.uniformTexture('textureDrawBuffers3', framebufferDrawBuffersReturn.textures[3], 5);

  glCat.uniformTexture('textureImageArrow', textureImageArrow, 6);
  glCat.uniformTexture('textureImageVertical', textureImageVertical, 7);
  glCat.uniformTexture('textureImageUtsunomiya', textureImageUtsunomiya, 8);
  glCat.uniformTexture('textureImageFoot', textureImageFoot, 9);
  glCat.uniformTexture('textureImageStop', textureImageStop, 10);
  glCat.uniformTexture('textureImageFive', textureImageFive, 11);
  glCat.uniformTexture('textureImageSix', textureImageSix, 12);
  glCat.uniformTexture('textureImageThirtythree', textureImageThirtythree, 13);
  glCat.uniformTexture('textureImageSuica', textureImageSuica, 14);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
};

// ------

var preview = function preview() {
  gl.viewport(0, 0, width, height);
  glCat.useProgram(programReturn);
  glCat.drawBuffers([gl.COLOR_ATTACHMENT0]);
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);
  gl.blendFunc(gl.ONE, gl.ONE);
  glCat.clear();

  glCat.attribute('p', vboQuad, 2);
  glCat.uniform2fv('resolution', [width, height]);

  glCat.uniformTexture('texture', framebufferDrawBuffers.textures[3], 0);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
};

// ------

var post = function post() {
  for (var i = 0; i < 4; i++) {
    var gaussVar = Math.exp(i) * Math.max(width, height) / 60.0;

    // ------

    gl.viewport(0, 0, width, height);
    glCat.useProgram(programReturn);
    glCat.drawBuffers([gl.COLOR_ATTACHMENT0]);
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebufferReturn.framebuffer);
    gl.blendFunc(gl.ONE, gl.ONE);
    glCat.clear();

    glCat.attribute('p', vboQuad, 2);
    glCat.uniform2fv('resolution', [width, height]);

    glCat.uniformTexture('texture', i === 0 ? framebufferDrawBuffers.textures[3] : framebufferBloom.texture, 0);

    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

    // ------

    gl.viewport(0, 0, width, height);
    glCat.useProgram(programPreBloom);
    glCat.drawBuffers([gl.COLOR_ATTACHMENT0]);
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebufferPreBloom.framebuffer);
    gl.blendFunc(gl.ONE, gl.ONE);
    glCat.clear();

    glCat.attribute('p', vboQuad, 2);
    glCat.uniform2fv('resolution', [width, height]);

    glCat.uniformTexture('texture', framebufferDrawBuffers.textures[3], 0);

    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

    // ------

    gl.viewport(0, 0, width, height);
    glCat.useProgram(programBloom);
    glCat.drawBuffers([gl.COLOR_ATTACHMENT0]);
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebufferBloomTemp.framebuffer);
    gl.blendFunc(gl.ONE, gl.ONE);
    glCat.clear();

    glCat.attribute('p', vboQuad, 2);
    glCat.uniform1i('isVert', false);
    glCat.uniform1f('gaussVar', gaussVar);
    glCat.uniform2fv('resolution', [width, height]);

    glCat.uniformTexture('texture', framebufferPreBloom.texture, 0);

    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);

    // ------

    gl.viewport(0, 0, width, height);
    glCat.useProgram(programBloom);
    glCat.drawBuffers([gl.COLOR_ATTACHMENT0]);
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebufferBloom.framebuffer);
    gl.blendFunc(gl.ONE, gl.ONE);
    glCat.clear();

    glCat.attribute('p', vboQuad, 2);
    glCat.uniform1i('isVert', true);
    glCat.uniform1f('gaussVar', gaussVar);
    glCat.uniform2fv('resolution', [width, height]);

    glCat.uniformTexture('texture', framebufferBloomTemp.texture, 0);
    glCat.uniformTexture('textureBase', framebufferReturn.texture, 1);

    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
  }

  // ------

  gl.viewport(0, 0, width, height);
  glCat.useProgram(programFinal);
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);
  glCat.drawBuffers([gl.BACK]);
  gl.blendFunc(gl.ONE, gl.ONE);
  glCat.clear();

  glCat.attribute('p', vboQuad, 2);
  glCat.uniform2fv('resolution', [width, height]);

  glCat.uniformTexture('texture', framebufferBloom.texture, 0);

  gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
};

// ------

var update = function update() {
  if (!checkboxPlay.checked) {
    setTimeout(update, 10);
    return;
  }

  timeUpdate();
  textureRandomUpdate(textureRandom);

  render();

  iSample++;
  if (iSample === nSample) {
    iSample = 0;
    console.log(frame);

    post();

    if (checkboxSave.checked) {
      saveFrame();
    }
    nSample = Math.floor(tweak.range('nSample', { value: 100.0, min: 1.0, max: 10000.0, step: 1.0 }));
    frame++;
  } else {
    preview();
  }

  // requestAnimationFrame( update );
  setTimeout(update, 1);
};

// ------

(0, _step2.default)({
  0: function _(done) {
    loadImage('image/arrow.png', function (_image) {
      glCat.setTexture(textureImageArrow, _image);
      done();
    });

    loadImage('image/vertical.png', function (_image) {
      glCat.setTexture(textureImageVertical, _image);
      done();
    });

    loadImage('image/utsunomiya.png', function (_image) {
      glCat.setTexture(textureImageUtsunomiya, _image);
      done();
    });

    loadImage('image/foot.png', function (_image) {
      glCat.setTexture(textureImageFoot, _image);
      done();
    });

    loadImage('image/stop.png', function (_image) {
      glCat.setTexture(textureImageStop, _image);
      done();
    });

    loadImage('image/five.png', function (_image) {
      glCat.setTexture(textureImageFive, _image);
      done();
    });

    loadImage('image/six.png', function (_image) {
      glCat.setTexture(textureImageSix, _image);
      done();
    });

    loadImage('image/thirtythree.png', function (_image) {
      glCat.setTexture(textureImageThirtythree, _image);
      done();
    });

    loadImage('image/suica.png', function (_image) {
      glCat.setTexture(textureImageSuica, _image);
      done();
    });
  },

  9: function _(done) {
    update();
  }
});

window.addEventListener('keydown', function (_e) {
  if (_e.which === 27) {
    checkboxPlay.checked = false;
  }
});

checkboxPlay.checked = true;

},{"./glcat":"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160922/src/script/glcat.js","./step":"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160922/src/script/step.js","./tweak":"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160922/src/script/tweak.js","./xorshift":"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160922/src/script/xorshift.js"}],"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160922/src/script/step.js":[function(require,module,exports){
'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
var step = function step(_obj) {
  var obj = _obj;
  var count = -1;

  var func = function func() {
    count++;
    if (typeof obj[count] === 'function') {
      obj[count](func);
    }
  };
  func();
};

exports.default = step;

},{}],"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160922/src/script/tweak.js":[function(require,module,exports){
'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Tweak = function () {
  function Tweak(_el) {
    _classCallCheck(this, Tweak);

    var it = this;

    it.parent = _el;
    it.values = {};
    it.elements = {};
  }

  _createClass(Tweak, [{
    key: 'button',
    value: function button(_name, _props) {
      var it = this;

      var props = _props || {};

      if (typeof it.values[_name] === 'undefined') {
        var div = document.createElement('div');
        it.parent.appendChild(div);

        var input = document.createElement('input');
        div.appendChild(input);
        input.type = 'button';
        input.value = _name;

        input.addEventListener('click', function () {
          it.values[_name] = true;
        });

        it.elements[_name] = {
          div: div,
          input: input
        };
      }

      var tempvalue = it.values[_name];
      it.values[_name] = false;
      if (typeof props.set === 'boolean') {
        it.values[_name] = props.set;
      }

      return tempvalue;
    }
  }, {
    key: 'checkbox',
    value: function checkbox(_name, _props) {
      var it = this;

      var props = _props || {};

      var value = void 0;

      if (typeof it.values[_name] === 'undefined') {
        value = props.value || false;

        var div = document.createElement('div');
        it.parent.appendChild(div);

        var name = document.createElement('span');
        div.appendChild(name);
        name.innerText = _name;

        var input = document.createElement('input');
        div.appendChild(input);
        input.type = 'checkbox';
        input.checked = value;

        it.elements[_name] = {
          div: div,
          name: name,
          input: input
        };
      } else {
        value = it.elements[_name].input.checked;
      }

      if (typeof props.set === 'boolean') {
        value = props.set;
      }

      it.elements[_name].input.checked = value;
      it.values[_name] = value;

      return it.values[_name];
    }
  }, {
    key: 'range',
    value: function range(_name, _props) {
      var it = this;

      var props = _props || {};

      var value = void 0;

      if (typeof it.values[_name] === 'undefined') {
        var min = props.min || 0.0;
        var max = props.max || 1.0;
        var step = props.step || 0.001;
        value = props.value || min;

        var div = document.createElement('div');
        it.parent.appendChild(div);

        var name = document.createElement('span');
        div.appendChild(name);
        name.innerText = _name;

        var input = document.createElement('input');
        div.appendChild(input);
        input.type = 'range';
        input.value = value;
        input.min = min;
        input.max = max;
        input.step = step;

        var val = document.createElement('span');
        div.appendChild(val);

        it.elements[_name] = {
          div: div,
          name: name,
          input: input,
          val: val
        };
      } else {
        value = parseFloat(it.elements[_name].input.value);
      }

      if (typeof props.set === 'number') {
        value = props.set;
      }

      it.values[_name] = value;
      it.elements[_name].input.value = value;
      it.elements[_name].val.innerText = value.toFixed(3);

      return it.values[_name];
    }
  }]);

  return Tweak;
}();

exports.default = Tweak;

},{}],"/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160922/src/script/xorshift.js":[function(require,module,exports){
"use strict";

Object.defineProperty(exports, "__esModule", {
  value: true
});
var seed = void 0;
var xorshift = function xorshift(_seed) {
  seed = _seed || seed || 1;
  seed = seed ^ seed << 13;
  seed = seed ^ seed >>> 17;
  seed = seed ^ seed << 5;
  return seed / Math.pow(2, 32) + 0.5;
};

exports.default = xorshift;

},{}]},{},["/Users/Yutaka/Dropbox/pro/_Projects/_eom/20160922/src/script/main.js"]);
