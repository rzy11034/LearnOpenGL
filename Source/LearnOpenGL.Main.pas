unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  Rtti,
  {%H-}DeepStar.Utils,
  {%H-}DeepStar.OpenGL.GLAD_GL,
  {%H-}DeepStar.OpenGL.GLFW,
  {%H-}DeepStar.OpenGL.GLM,
  {%H-}DeepStar.OpenGL.Utils,
  {%H-}DeepStar.OpenGL.Shader,
  {%H-}DeepStar.OpenGL.Texture;

procedure Run();

implementation

uses
  Case02_01_01_Colors;

procedure Test;
var
  v: TVec3;
  s: String;
  va:TValue;
begin
  s := '';
  v := TGLM.Vec3(5);

  TValue.Make(@v, TypeInfo(TVec3), va);
  s := va.ToString;
  getv

  Exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
