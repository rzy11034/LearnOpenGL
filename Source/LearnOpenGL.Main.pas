unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}

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
  {%H-}DeepStar.OpenGL.Model, DeepStar.DSA.Linear.ArrayList;

procedure Run();

implementation

uses
  Case04_11_02_Anti_Aliasing_Offscreen;

type
  IAAA = interface
    ['{9D4D55EE-BC63-49D0-BE20-559D3F82E651}']
    procedure Clear;
    property Count: integer;
  end;

  TAAA = class(TInterfacedObject, IAAA)
  private
    function __GetCount: integer;
  public
    procedure Clear;
    property Count: integer read __GetCount;
  end;

procedure Test;
var
  ia: IAAA;
  ta: TAAA;
  i: Integer;
begin
  ia := TAAA.Create;
  i := (ia as TAAA).Count;

  Exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

{ TAAA }

procedure TAAA.Clear;
begin
  exit;
end;

function TAAA.__GetCount: integer;
begin
  Result := 2;
end;

end.
