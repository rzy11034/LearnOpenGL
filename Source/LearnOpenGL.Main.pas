unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  {%H-}DeepStar.Utils,
  {%H-}DeepStar.OpenGL.GLAD_GL,
  {%H-}DeepStar.OpenGL.GLFW,
  {%H-}DeepStar.OpenGL.GLM,
  {%H-}DeepStar.OpenGL.Utils,
  {%H-}DeepStar.OpenGL.Shader,
  {%H-}DeepStar.OpenGL.Texture,
  {%H-}DeepStar.OpenGL.Mesh,
  {%H-}DeepStar.OpenGL.Model;

procedure Run();

implementation

uses
  Case04_04_01_Face_Culling_Exercise1;

procedure Test;
begin
  Exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
