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
  {%H-}Math,
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
  Case07_03_2D_Game_Program;

procedure Test;
var
  count: Cardinal;
  i: Integer;
begin
  count := 0;
  //count -= 1;

  for i := 0 to Count - 1 do
  begin
    if i = count then
      Break;


  end;

  WriteLn(i);

  Exit;
end;

procedure Run();
begin
  (*═══════════════════════════════════════════════════════════════════════
  Test;
  WriteLn(END_OF_PROGRAM_CH);
  ReadLn;
  //═══════════════════════════════════════════════════════════════════════*)


  Main;
end;

end.
