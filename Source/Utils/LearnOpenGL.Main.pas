unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  {%H-}matrix,
  {%H-}DeepStar.UString,
  {%H-}DeepStar.Utils,
  {%H-}DeepStar.OpenGL.GLAD_GL,
  {%H-}DeepStar.OpenGL.GLFW,
  {%H-}DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.Matrix,
  {%H-}LearnOpenGL.Utils,
  {%H-}LearnOpenGL.Shader;

procedure Run();

implementation

uses
  Case01_07_01_Camera_Circle;

procedure Test;
var
  m: TMat4;
  mm: matrix.Tmatrix4_single;
begin
  m.Create(
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10, 11, 12,
    13, 14, 15, 16);

  m := m.Transpose;

  mm.init(
    1, 2, 3, 4,
    5, 6, 7, 8,
    9, 10, 11, 12,
    13, 14, 15, 16);
  Exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
