unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
interface

uses
  Classes,
  SysUtils,
  {%H-}DeepStar.UString,
  {%H-}DeepStar.Utils,
  {%H-}GLAD_GL,
  {%H-}GLFW,
  {%H-}LearnOpenGL.Utils,
  {%H-}LearnOpenGL.Shader;

procedure Run();

implementation

uses
  Case01_04_02_Textures_Combined;

procedure Test;
begin
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
