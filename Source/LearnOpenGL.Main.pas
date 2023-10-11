unit LearnOpenGL.Main;

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
  {%H-}Case01_02_Exercise_01;

const
  {%H-}LE = LineEnding;

procedure Test;
var
  indices: TArr_GLint;
  i: GLint;
begin
  indices := TArr_GLint(nil);
  indices := [
    // 注意索引从0开始!
    // 此例的索引(0,1,2,3)就是顶点数组vertices的下标，
    // 这样可以由下标代表顶点组合成矩形
    0, 1, 3, // 第一个三角形
    1, 2, 3];  // 第二个三角形

  i := specialize DynArrayMemSize<TArr_GLint>(indices);

  i += 1;

  Exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
