unit LearnOpenGL.Main;

{$mode delphi}{$H+}
{$ModeSwitch unicodestrings}{$J-}

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
  Case02_02_01_Basic_Lighting_Diffuse;

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
