unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch defaultparameters}

interface

uses
  Classes,
  SysUtils,
  {%H-}DeepStar.UString,
  {%H-}DeepStar.Utils,
  {%H-}DeepStar.OpenGL.GLAD_GL,
  {%H-}DeepStar.OpenGL.GLFW,
  {%H-}DeepStar.OpenGL.GLM,
  {%H-}LearnOpenGL.Utils,
  {%H-}LearnOpenGL.Shader,
  {%H-}LearnOpenGL.Texture;

procedure Run();

implementation

uses
  Case01_01_01_Hello_Window;

procedure Test;
var
  m, n: TMat4;
begin
  m.Data := [
    [1.81066, 0, 0, 0],
    [0, 2.4142134, 0, 0],
    [0, 0, -1.002002, -0.2002002],
    [0, 0, -1, 0]];

  n := m.Transpose;

  Exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
