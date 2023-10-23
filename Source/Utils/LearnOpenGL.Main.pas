unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  {%H-}DeepStar.UString,
  {%H-}DeepStar.Utils,
  {%H-}GLAD_GL,
  {%H-}GLFW,
  {%H-}LearnOpenGL.Utils,
  {%H-}LearnOpenGL.Shader,
  FpImage;

procedure Run();

implementation

uses
  Case01_04_04_Textures_Exercise2;

procedure Test;
var
  v: TVec4f;
begin
  v := RGBAToOpenGLColor(241, 50, 39);


  exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
