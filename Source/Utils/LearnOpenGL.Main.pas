unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  {%H-}matrix,
  {%H-}DeepStar.UString,
  {%H-}DeepStar.Utils,
  {%H-}GLAD_GL,
  {%H-}GLFW,
  {%H-}LearnOpenGL.GLM,
  {%H-}LearnOpenGL.Utils,
  {%H-}LearnOpenGL.Shader;

procedure Run();

implementation

uses
  Case01_05_02_Transformations_Exercise1;

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
