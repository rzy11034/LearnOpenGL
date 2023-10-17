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
  {%H-}LearnOpenGL.Utils;

procedure Run();

implementation

uses
  {%H-}Case01_04_Textures;

procedure Test;
var
  tx: TOpenGLTexture;
  s: string;
begin
  tx := TOpenGLTexture.Create(CrossFixFileName('../../Resources/textures/wall.jpg'));
  tx.Free;

  s := GetCurrentDir;
  WriteLn(s);

  ReadLn;
  Exit;
end;

procedure Run();
begin
  //Test;
  Main;
end;

end.
