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
  Case02_06_02_Multiple_Lights_Exercise1;

procedure aa(str: string);
var
  s: UnicodeString;
begin
  s := str
end;

procedure Test;
var
  s: array[0..512] of AChar;
  str: string;
begin
  s := 'aaaaaaaaaaa';
  str := s;
  aa(s);
  Exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
