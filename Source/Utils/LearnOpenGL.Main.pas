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
  {%H-}dynlibs,
  {%H-}ctypes,
  {%H-}Math;

procedure Run();

implementation

uses
  {%H-}Case01_03_Shaders_Uniform,
  LearnOpenGL.Shader;

procedure Test;
var
  sd: TShader;
begin
  sd := TShader.Create();
  try
    sd.LoadShaderFile('Source\Utils\vertexShader.inc', '');
  finally
    sd.Free;
  end;
  Exit;
end;

procedure Run();
begin
  Test;
  //Main;
end;

end.
