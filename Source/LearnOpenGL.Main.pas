﻿unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  {%H-}GLAD_GL,
  {%H-}LearnOpenGL.Utils,
  {%H-}dynlibs,
  {%H-}ctypes,
  {%H-}Math;

procedure Run();

implementation

uses
  {%H-}Case01_02_Hello_Triangle;

const
  {%H-}LE = LineEnding;

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
