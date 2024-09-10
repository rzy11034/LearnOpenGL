unit LearnOpenGL.Main;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}
{$ModeSwitch implicitfunctionspecialization}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}

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
  Case05_03_02_02_Point_Shadows_Soft;

procedure Test;
begin
  Exit;
end;

procedure Run();
begin
  //Test;
  //WriteLn(END_OF_PROGRAM_CH);
  //ReadLn;
  Main;
end;

end.
