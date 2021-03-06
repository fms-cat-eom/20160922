import xorshift from './xorshift';
xorshift( 187 );
import GLCat from './glcat';
import step from './step';
import Tweak from './tweak';

const glslify = require( 'glslify' );

// ------

const clamp = ( _value, _min, _max ) => Math.min( Math.max( _value, _min ), _max );
const saturate = ( _value ) => clamp( _value, 0.0, 1.0 );

// ------

let width = canvas.width = 300;
let height = canvas.height = 300;

let gl = canvas.getContext( 'webgl' );
let glCat = new GLCat( gl );

// ------

let tweak = new Tweak( divTweak );

// ------

let frame = 0;
let frames = 120;
let iSample = 0;
let nSample = 1;
let time = 0.0;
let deltaTime = 1.0 / 60.0;

let timeUpdate = () => {
  let timePrev = time;
  time = ( frame + iSample / nSample ) / frames;
  time = time % 1.0;
};

// ------

let vboQuad = glCat.createVertexbuffer( [ -1, -1, 1, -1, -1, 1, 1, 1 ] );

// ------

let vertQuad = glslify( './shader/quad.vert' );

let programReturn = glCat.createProgram(
  vertQuad,
  glslify( './shader/return.frag' )
);

let programReturnDb = glCat.createProgram(
  vertQuad,
  glslify( './shader/returndb.frag' )
);

let programRender = glCat.createProgram(
  vertQuad,
  glslify( './shader/render.frag' )
);

let programSum = glCat.createProgram(
  vertQuad,
  glslify( './shader/sum.frag' )
);

let programBloom = glCat.createProgram(
  vertQuad,
  glslify( './shader/bloom.frag' )
);

let programPreBloom = glCat.createProgram(
  vertQuad,
  glslify( './shader/prebloom.frag' )
);

let programFinal = glCat.createProgram(
  vertQuad,
  glslify( './shader/final.frag' )
);

// ------

let framebufferDrawBuffers = glCat.createDrawBuffers( width, height, 4 );
let framebufferDrawBuffersReturn = glCat.createDrawBuffers( width, height, 4 );

let framebufferReturn = glCat.createFloatFramebuffer( width, height );
let framebufferPreBloom = glCat.createFloatFramebuffer( width, height );
let framebufferBloom = glCat.createFloatFramebuffer( width, height );
let framebufferBloomTemp = glCat.createFloatFramebuffer( width, height );

// ------

let textureRandomSize = 256;

let textureRandomUpdate = ( _tex ) => {
  glCat.setTextureFromArray( _tex, textureRandomSize, textureRandomSize, ( () => {
    let len = textureRandomSize * textureRandomSize * 4;
    let ret = new Uint8Array( len );
    for ( let i = 0; i < len; i ++ ) {
      ret[ i ] = Math.floor( xorshift() * 256.0 );
    }
    return ret;
  } )() );
};

let textureRandom = glCat.createTexture();
glCat.textureWrap( textureRandom, gl.REPEAT );

let textureRandomStatic = glCat.createTexture();
glCat.textureWrap( textureRandomStatic, gl.REPEAT );
textureRandomUpdate( textureRandomStatic );

// ------

let loadImage = ( _url, _callback ) => {
  let image = new Image();
  image.onload = () => {
    _callback( image );
  };
  image.src = _url;
};

let textureImageArrow = glCat.createTexture();
let textureImageVertical = glCat.createTexture();
let textureImageUtsunomiya = glCat.createTexture();
let textureImageStop = glCat.createTexture();
let textureImageFoot = glCat.createTexture();
let textureImageFive = glCat.createTexture();
let textureImageSix = glCat.createTexture();
let textureImageThirtythree = glCat.createTexture();
let textureImageSuica = glCat.createTexture();

// ------

let renderA = document.createElement( 'a' );

let saveFrame = () => {
  renderA.href = canvas.toDataURL();
  renderA.download = ( '0000' + frame ).slice( -5 ) + '.png';
  renderA.click();
};

// ------

let render = () => {
  gl.viewport( 0, 0, width, height );
  glCat.useProgram( programReturnDb );
  gl.bindFramebuffer( gl.FRAMEBUFFER, framebufferDrawBuffersReturn.framebuffer );
  glCat.drawBuffers( framebufferDrawBuffersReturn.textures.length );
  gl.blendFunc( gl.ONE, gl.ONE );
  glCat.clear( 0.0, 0.0, 0.0, 0.0 );

  glCat.attribute( 'p', vboQuad, 2 );

  glCat.uniform1f( 'time', time );
  glCat.uniform2fv( 'resolution', [ width, height ] );

  glCat.uniformTexture( 'texture0', framebufferDrawBuffers.textures[ 0 ], 0 );
  glCat.uniformTexture( 'texture1', framebufferDrawBuffers.textures[ 1 ], 1 );
  glCat.uniformTexture( 'texture2', framebufferDrawBuffers.textures[ 2 ], 2 );
  glCat.uniformTexture( 'texture3', framebufferDrawBuffers.textures[ 3 ], 3 );

  gl.drawArrays( gl.TRIANGLE_STRIP, 0, 4 );

  // ------

  gl.viewport( 0, 0, width, height );
  glCat.useProgram( programRender );
  gl.bindFramebuffer( gl.FRAMEBUFFER, framebufferDrawBuffers.framebuffer );
  glCat.drawBuffers( framebufferDrawBuffers.textures.length );
  gl.blendFunc( gl.ONE, gl.ONE );
  glCat.clear( 0.0, 0.0, 0.0, 0.0 );

  glCat.attribute( 'p', vboQuad, 2 );

  glCat.uniform1f( 'time', time );
  glCat.uniform2fv( 'resolution', [ width, height ] );
  glCat.uniform1i( 'reset', iSample === 0 );

  glCat.uniformTexture( 'textureRandom', textureRandom, 0 );
  glCat.uniformTexture( 'textureRandomStatic', textureRandomStatic, 1 );

  glCat.uniformTexture( 'textureDrawBuffers0', framebufferDrawBuffersReturn.textures[ 0 ], 2 );
  glCat.uniformTexture( 'textureDrawBuffers1', framebufferDrawBuffersReturn.textures[ 1 ], 3 );
  glCat.uniformTexture( 'textureDrawBuffers2', framebufferDrawBuffersReturn.textures[ 2 ], 4 );
  glCat.uniformTexture( 'textureDrawBuffers3', framebufferDrawBuffersReturn.textures[ 3 ], 5 );

  glCat.uniformTexture( 'textureImageArrow', textureImageArrow, 6 );
  glCat.uniformTexture( 'textureImageVertical', textureImageVertical, 7 );
  glCat.uniformTexture( 'textureImageUtsunomiya', textureImageUtsunomiya, 8 );
  glCat.uniformTexture( 'textureImageFoot', textureImageFoot, 9 );
  glCat.uniformTexture( 'textureImageStop', textureImageStop, 10 );
  glCat.uniformTexture( 'textureImageFive', textureImageFive, 11 );
  glCat.uniformTexture( 'textureImageSix', textureImageSix, 12 );
  glCat.uniformTexture( 'textureImageThirtythree', textureImageThirtythree, 13 );
  glCat.uniformTexture( 'textureImageSuica', textureImageSuica, 14 );

  gl.drawArrays( gl.TRIANGLE_STRIP, 0, 4 );
};

// ------

let preview = () => {
  gl.viewport( 0, 0, width, height );
  glCat.useProgram( programReturn );
  glCat.drawBuffers( [ gl.COLOR_ATTACHMENT0 ] );
  gl.bindFramebuffer( gl.FRAMEBUFFER, null );
  gl.blendFunc( gl.ONE, gl.ONE );
  glCat.clear();

  glCat.attribute( 'p', vboQuad, 2 );
  glCat.uniform2fv( 'resolution', [ width, height ] );

  glCat.uniformTexture( 'texture', framebufferDrawBuffers.textures[ 3 ], 0 );

  gl.drawArrays( gl.TRIANGLE_STRIP, 0, 4 );
};

// ------

let post = () => {
  for ( let i = 0; i < 4; i ++ ) {
    let gaussVar = Math.exp( i ) * Math.max( width, height ) / 60.0;

    // ------

    gl.viewport( 0, 0, width, height );
    glCat.useProgram( programReturn );
    glCat.drawBuffers( [ gl.COLOR_ATTACHMENT0 ] );
    gl.bindFramebuffer( gl.FRAMEBUFFER, framebufferReturn.framebuffer );
    gl.blendFunc( gl.ONE, gl.ONE );
    glCat.clear();

    glCat.attribute( 'p', vboQuad, 2 );
    glCat.uniform2fv( 'resolution', [ width, height ] );

    glCat.uniformTexture( 'texture', i === 0 ? framebufferDrawBuffers.textures[ 3 ] : framebufferBloom.texture, 0 );

    gl.drawArrays( gl.TRIANGLE_STRIP, 0, 4 );

    // ------

    gl.viewport( 0, 0, width, height );
    glCat.useProgram( programPreBloom );
    glCat.drawBuffers( [ gl.COLOR_ATTACHMENT0 ] );
    gl.bindFramebuffer( gl.FRAMEBUFFER, framebufferPreBloom.framebuffer );
    gl.blendFunc( gl.ONE, gl.ONE );
    glCat.clear();

    glCat.attribute( 'p', vboQuad, 2 );
    glCat.uniform2fv( 'resolution', [ width, height ] );

    glCat.uniformTexture( 'texture', framebufferDrawBuffers.textures[ 3 ], 0 );

    gl.drawArrays( gl.TRIANGLE_STRIP, 0, 4 );

    // ------

    gl.viewport( 0, 0, width, height );
    glCat.useProgram( programBloom );
    glCat.drawBuffers( [ gl.COLOR_ATTACHMENT0 ] );
    gl.bindFramebuffer( gl.FRAMEBUFFER, framebufferBloomTemp.framebuffer );
    gl.blendFunc( gl.ONE, gl.ONE );
    glCat.clear();

    glCat.attribute( 'p', vboQuad, 2 );
    glCat.uniform1i( 'isVert', false );
    glCat.uniform1f( 'gaussVar', gaussVar );
    glCat.uniform2fv( 'resolution', [ width, height ] );

    glCat.uniformTexture( 'texture', framebufferPreBloom.texture, 0 );

    gl.drawArrays( gl.TRIANGLE_STRIP, 0, 4 );

    // ------

    gl.viewport( 0, 0, width, height );
    glCat.useProgram( programBloom );
    glCat.drawBuffers( [ gl.COLOR_ATTACHMENT0 ] );
    gl.bindFramebuffer( gl.FRAMEBUFFER, framebufferBloom.framebuffer );
    gl.blendFunc( gl.ONE, gl.ONE );
    glCat.clear();

    glCat.attribute( 'p', vboQuad, 2 );
    glCat.uniform1i( 'isVert', true );
    glCat.uniform1f( 'gaussVar', gaussVar );
    glCat.uniform2fv( 'resolution', [ width, height ] );

    glCat.uniformTexture( 'texture', framebufferBloomTemp.texture, 0 );
    glCat.uniformTexture( 'textureBase', framebufferReturn.texture, 1 );

    gl.drawArrays( gl.TRIANGLE_STRIP, 0, 4 );
  }

  // ------

  gl.viewport( 0, 0, width, height );
  glCat.useProgram( programFinal );
  gl.bindFramebuffer( gl.FRAMEBUFFER, null );
  glCat.drawBuffers( [ gl.BACK ] );
  gl.blendFunc( gl.ONE, gl.ONE );
  glCat.clear();

  glCat.attribute( 'p', vboQuad, 2 );
  glCat.uniform2fv( 'resolution', [ width, height ] );

  glCat.uniformTexture( 'texture', framebufferBloom.texture, 0 );

  gl.drawArrays( gl.TRIANGLE_STRIP, 0, 4 );
};

// ------

let update = () => {
  if ( !checkboxPlay.checked ) {
    setTimeout( update, 10 );
    return;
  }

  timeUpdate();
  textureRandomUpdate( textureRandom );

  render();

  iSample ++;
  if ( iSample === nSample ) {
    iSample = 0;
    console.log( frame );

    post();

    if ( checkboxSave.checked ) {
      saveFrame();
    }
    nSample = Math.floor( tweak.range( 'nSample', { value: 100.0, min: 1.0, max: 10000.0, step: 1.0 } ) );
    frame ++;
  } else {
    preview();
  }

  // requestAnimationFrame( update );
  setTimeout( update, 1 );
};

// ------

step( {
  0: ( done ) => {
    loadImage( 'image/arrow.png', ( _image ) => {
      glCat.setTexture( textureImageArrow, _image );
      done();
    } );

    loadImage( 'image/vertical.png', ( _image ) => {
      glCat.setTexture( textureImageVertical, _image );
      done();
    } );

    loadImage( 'image/utsunomiya.png', ( _image ) => {
      glCat.setTexture( textureImageUtsunomiya, _image );
      done();
    } );

    loadImage( 'image/foot.png', ( _image ) => {
      glCat.setTexture( textureImageFoot, _image );
      done();
    } );

    loadImage( 'image/stop.png', ( _image ) => {
      glCat.setTexture( textureImageStop, _image );
      done();
    } );

    loadImage( 'image/five.png', ( _image ) => {
      glCat.setTexture( textureImageFive, _image );
      done();
    } );

    loadImage( 'image/six.png', ( _image ) => {
      glCat.setTexture( textureImageSix, _image );
      done();
    } );

    loadImage( 'image/thirtythree.png', ( _image ) => {
      glCat.setTexture( textureImageThirtythree, _image );
      done();
    } );

    loadImage( 'image/suica.png', ( _image ) => {
      glCat.setTexture( textureImageSuica, _image );
      done();
    } );
  },

  9: ( done ) => {
    update();
  }
} );

window.addEventListener( 'keydown', ( _e ) => {
  if ( _e.which === 27 ) {
    checkboxPlay.checked = false;
  }
} );

checkboxPlay.checked = true;
