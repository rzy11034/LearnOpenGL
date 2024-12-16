﻿unit LearnOpenGL.Main;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}
{$ModeSwitch implicitfunctionspecialization}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}
{$ModeSwitch duplicatelocals}

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
  Case06_01_02_Lighting_Textured;

type
  PTT = ^TTT;
  TTT = Record
  public
    a: integer;
    b: string;
    constructor Create(a: integer; b: string);
  end;

procedure Test;
var
  p: PTT;
  t: TTT;
begin
  p := PTT(nil);

  //if p = nil then t := TTT.Create(100, 'aaa') else t := p^;

  //t := ifthen(p = nil, TTT.Create(100, 'aaa'), p^);

  Exit;
end;

procedure Run();
begin
  //(*═══════════════════════════════════════════════════════════════════════
  Test;
  WriteLn(END_OF_PROGRAM_CH);
  ReadLn;
  //═══════════════════════════════════════════════════════════════════════*)


  //Main;
end;

{ TTT }

constructor TTT.Create(a: integer; b: string);
begin
  Self.a := a;
  Self.b := b;
end;

end.
