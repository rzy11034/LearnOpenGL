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
  {%H-}LearnOpenGL.Utils;

procedure Run();

implementation

uses
  {%H-}Case01_04_Textures;

procedure Test;
var
  tx :TOpenGLTexture;
  i: Integer;
begin
  tx:= TOpenGLTexture.Create('Resources\textures\wall.jpg');
  tx.Free;
  Exit;
end;

procedure Run();
begin
  Test;
  //Main;
end;

end.
