unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  gl,
  {%H-}GLAD_GL,
  {%H-}dynlibs,
  {%H-}ctypes,
  {%H-}Math;

procedure Run();

implementation

uses
  Case01_02_Hello_Triangle;

const
  LE = LineEnding;

procedure Test;
begin
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
