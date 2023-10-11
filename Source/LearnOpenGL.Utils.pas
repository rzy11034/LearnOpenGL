unit LearnOpenGL.Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  GLAD_GL;

const
  LE = LineEnding;

type
  TArr_GLfloat = array of GLfloat;
  TArr_GLint = array of GLint;
  TArr_GLuint = array of GLuint;
  TArr_GLchar = array of GLchar;
  TArr_GLdouble = Array of GLdouble;

generic function DynArrayMemSize<T>(const dynArray: T): GLint;

implementation

generic function DynArrayMemSize<T>(const dynArray: T): GLint;
begin
  Result := SizeOf(dynArray[0]) * Length(dynArray);
end;

end.
