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
  {%H-}DeepStar.OpenGL.Model;

procedure Run();

implementation

uses
  Case02_06_02_Multiple_Lights_Exercise1;

type
  PVertexObj = ^TVertexObj;
  TVertexObj = object(TObj_Base)
    Position: TVec3;
    Normal: TVec3;
    TexCoords: TVec2;
    Tangent: TVec3;
    Bitangent: TVec3;
    BoneIDs: array[0..MAX_BONE_INFLUENCE - 1] of integer;
    Weights: array[0..MAX_BONE_INFLUENCE - 1] of single;

    constructor Init;
    destructor Done; virtual;

    class function Create: TVertexObj; static;
    class function CreatePtr: PVertexObj; static;
  end;

  PVertexRec = ^TVertexRec;
  TVertexRec = record
    Position: TVec3;
    Normal: TVec3;
    TexCoords: TVec2;
    Tangent: TVec3;
    Bitangent: TVec3;
    BoneIDs: array[0..MAX_BONE_INFLUENCE - 1] of integer;
    Weights: array[0..MAX_BONE_INFLUENCE - 1] of single;

    class function Create: TVertexRec; static;
    class function CreatePtr: PVertexRec; static;
  end;

procedure Test;
type
  T_int = array[0..MAX_BONE_INFLUENCE - 1] of integer;
  P_int = ^T_int;

  T_float = array[0..MAX_BONE_INFLUENCE - 1] of single;
  P_float = ^T_float;
var
  i: integer;
  j: PtrUInt;
  tObj: TVertexObj;
  tRec: TVertexRec;
  Position: TVec3;
  Normal: TVec3;
  TexCoords: TVec2;
  Tangent: TVec3;
  Bitangent: TVec3;
  BoneIDs: array[0..MAX_BONE_INFLUENCE - 1] of integer;
  Weights: array[0..MAX_BONE_INFLUENCE - 1] of single;
  p: Pointer;
  p1, p2: Pointer;
  pObj: PVertexObj;
  pRec: PVertexRec;
begin
  tObj := TVertexObj.Create;
  pObj := TVertexObj.CreatePtr;
  tRec := TVertexRec.Create;
  pRec := TVertexRec.CreatePtr;

  i := SizeOf(pObj^);
  i := SizeOf(tObj);
  i := SizeOf(pRec^);

  p := pObj;
  p1 := Pointer(0);
  p2 := nil;

  j := OffsetOf(pObj^, pObj^.Position);
  Position := PVec3(p + j)^;
  p1 := p1 + j;
  p2 := Pointer(0) + j;

  j := OffsetOf(pObj^, pObj^.BoneIDs);
  BoneIDs := P_int(p + j)^;
  p1 := p1 + j;
  p2 := Pointer(0) + j;

  j := OffsetOf(pObj^, pObj^.Weights);
  Weights := P_float(p + j)^;
  p1 := p1 + j;
  p2 := Pointer(0) + j;

  for j := 0 to 100 do
  begin
    Writeln(j);
  end;

  FreeObjAndNil(pObj);
  FreeMemAndNil(pRec);

  Exit;
end;

procedure Run();
begin
  Test;
  //Main;
end;

{ TVertexRec }

class function TVertexRec.Create: TVertexRec;
begin
  Result := Default(TVertexRec);

  Result.Position := TVec3.Create(100, 100, 100);
  Result.Normal := TVec3.Create(200, 200, 200);
  Result.TexCoords := TVec2.Create(300, 300);
  Result.Tangent := TVec3.Create(400, 400, 400);
  Result.Bitangent := TVec3.Create(500, 500, 500);
  Result.BoneIDs := [600, 600, 600, 600];
  Result.Weights := [700, 700, 700, 700];
end;

class function TVertexRec.CreatePtr: PVertexRec;
begin
  New(Result);
  Result^ := TVertexRec.Create;
end;

{ TVertexObj }

constructor TVertexObj.Init;
begin
  inherited;

  Self.Position := TVec3.Create(100, 100, 100);
  Self.Normal := TVec3.Create(200, 200, 200);
  Self.TexCoords := TVec2.Create(300, 300);
  Self.Tangent := TVec3.Create(400, 400, 400);
  Self.Bitangent := TVec3.Create(500, 500, 500);
  Self.BoneIDs := [600, 600, 600, 600];
  Self.Weights := [700, 700, 700, 700];
end;

class function TVertexObj.Create: TVertexObj;
begin
  Result.Init;
end;

class function TVertexObj.CreatePtr: PVertexObj;
begin
  new(Result, Init);
end;

destructor TVertexObj.Done;
begin
  inherited Done;
end;

end.
