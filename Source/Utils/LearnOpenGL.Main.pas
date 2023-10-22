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
  Case01_04_03_Textures_Exercise1;

procedure Test;
begin

end;

procedure Run();
begin
  //Test;
  Main;
end;

end.
