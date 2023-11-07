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
  {%H-}LearnOpenGL.Utils,
  {%H-}LearnOpenGL.Shader;

procedure Run();

implementation

uses
  Case01_07_03_Camera_Mouse_Zoom;

procedure Test;
var
  m: TMat4;
begin
  m := TGLM.Mat4_Init(
    0,0,0,1,
    0,0,0,2,
    0,0,0,3,
    0,0,0,4);

  m := m.Transpose;

   Exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
