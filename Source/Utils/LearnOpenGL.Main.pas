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
  {%H-}LearnOpenGL.Shader, FpImage;

procedure Run();

implementation

uses
  Case01_04_01_Textures;

procedure Test;
var
  img: TFPMemoryImage;
  reader: TFPCustomImageReaderClass;
begin
  img:=TFPMemoryImage.Create(0,0);
  reader:= img.FindReaderFromFileName('D:\Users\DELL\Documents\Projects\01_Pictures\sample2.jpg');

  reader.Create
end;

procedure Run();
begin
  Test;
  //Main;
end;

end.
