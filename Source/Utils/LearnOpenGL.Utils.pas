unit LearnOpenGL.Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GLAD_GL;

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

implementation

end.
