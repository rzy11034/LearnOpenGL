unit LearnOpenGL.Utils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  GLAD_GL;

type
  TArr_GLfloat = array of GLfloat;
  TArr_GLint = array of GLint;
  TArr_GLuint = array of GLuint;
  TArr_GLchar = array of GLchar;

function DynArrayMemSize(const dynArray: TArr_GLfloat): GLint;
function DynArrayMemSize(const dynArray: TArr_GLint): GLint;
function DynArrayMemSize(const dynArray: TArr_GLuint): GLint;
function DynArrayMemSize(const dynArray: TArr_GLchar): GLint;

implementation

function DynArrayMemSize(const dynArray: TArr_GLfloat): GLint;
begin
  Result := SizeOf(dynArray[0]) * Length(dynArray);
end;

function DynArrayMemSize(const dynArray: TArr_GLint): GLint;
begin
  Result := SizeOf(dynArray[0]) * Length(dynArray);
end;

function DynArrayMemSize(const dynArray: TArr_GLuint): GLint;
begin
  Result := SizeOf(dynArray[0]) * Length(dynArray);
end;

function DynArrayMemSize(const dynArray: TArr_GLchar): GLint;
begin
  Result := SizeOf(dynArray[0]) * Length(dynArray);
end;

end.
