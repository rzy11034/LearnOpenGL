unit LearnOpenGL.Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  ctypes,
  DeepStar.Utils,
  GLAD_GL,
  FpImage,
  FPReadJPEG,
  FPReadBMP,
  FPReadPNG;

const
  LE = LineEnding;

type
  TArr_GLboolean = array of GLboolean;
  TArr_GLfloat = array of GLfloat;
  TArr_GLint = array of GLint;
  TArr_GLuint = array of GLuint;
  TArr_GLchar = array of GLchar;
  TArr_GLdouble = array of GLdouble;

  TArrayUtils_GLfloat = specialize TArrayUtils<GLfloat>;
  TArrayUtils_GLint = specialize TArrayUtils<GLint>;

type
  TOpenGLTexture = class(TObject)
  public
    Width, Height: GLint;
    Data: PGLubyte;
  end;

implementation

end.
