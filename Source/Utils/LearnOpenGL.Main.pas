﻿unit LearnOpenGL.Main;

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

procedure AA(var data);
begin
  exit;
end;

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
