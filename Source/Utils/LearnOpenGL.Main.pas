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
  Case01_06_02_Coordinate_Systems_Depth;

procedure Test;
var
  s: string;
  i: integer;
begin
  for i := integer(0) to 10 do
  begin
    s += i.ToString;
    Continue;
  end;

  Exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
