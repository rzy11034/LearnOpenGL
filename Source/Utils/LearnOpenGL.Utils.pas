unit LearnOpenGL.Utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLAD_GL;

const
  LE = LineEnding;

type
  TArr_GLboolean = array of GLboolean;
  TArr_GLfloat = array of GLfloat;
  TArr_GLint = array of GLint;
  TArr_GLuint = array of GLuint;
  TArr_GLchar = array of GLchar;
  TArr_GLdouble = array of GLdouble;

  TArr_TVec2 = array of TVec2;
  TArr_TVec3 = array of TVec3;
  TArr_TVec4 = array of TVec4;
  TArr_TMat3 = array of TMat3;
  TArr_TMat4 = array of TMat4;

  TArrayUtils_GLfloat = specialize TArrayUtils<GLfloat>;
  TArrayUtils_GLint = specialize TArrayUtils<GLint>;

  TArr_GLfloat4 = array[0..3] of GLfloat;
  TArr_GLfloat16 = array[0..15] of GLfloat;

function RGBAToOpenGLColor(red, green, blue: GLubyte; alpha: GLubyte = 0): TArr_GLfloat4;
function HtmlRGBToOpenGLColor(HtmlColor: GLuint): TArr_GLfloat4;
function SizeOfArray(arr: TArr_GLfloat): integer;
function cfn(const s: string): string;

implementation

function RGBAToOpenGLColor(red, green, blue: GLubyte; alpha: GLubyte): TArr_GLfloat4;
var
  res: TArr_GLfloat4;
begin
  res[0] := red / 255;
  res[1] := green / 255;
  res[2] := blue / 255;
  res[3] := alpha / 255;
  Result := res;
end;

function HtmlRGBToOpenGLColor(HtmlColor: GLuint): TArr_GLfloat4;
var
  r, g, b, a: GLubyte;
  p: PGLubyte;
begin
  p := @HtmlColor;
  b := p[0];
  g := p[1]; //Inc(p);
  r := p[2]; //Inc(p);
  a := p[3];

  Result := RGBAToOpenGLColor(r, g, b, a);
end;

function SizeOfArray(arr: TArr_GLfloat): integer;
begin
  Result := TArrayUtils_GLfloat.MemorySize(arr);
end;

function cfn(const s: string): string;
begin
  Result := CrossFixFileName(s);
end;

end.
