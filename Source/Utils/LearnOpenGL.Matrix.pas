unit LearnOpenGL.Matrix;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  SysUtils,
  LearnOpenGL.Vector,
  DeepStar.OpenGL.GLAD_GL;

type
  Tmat2x2 = array[0..1] of TVector2f;
  Pmat2x2 = ^Tmat2x2;
  Tmat3x3 = array[0..2] of TVector3f;
  Pmat3x3 = ^Tmat3x3;
  Tmat4x4 = array[0..3] of TVector4f;
  Pmat4x4 = ^Tmat4x4;

  Tmat3x2 = array[0..2] of TVector2f;
  Pmat3x2 = ^Tmat3x2;
  Tmat4x2 = array[0..3] of TVector2f;
  Pmat4x2 = ^Tmat4x2;

  Tmat3x4 = array[0..2] of TVector4f;
  Pmat3x4 = ^Tmat3x4;
  Tmat4x3 = array[0..3] of TVector3f;
  Pmat4x3 = ^Tmat4x3;

  TMatrix = Tmat4x4;
  PMatrix = ^TMatrix;

  TMatrix2D = Tmat3x3;
  PMatrix2D = ^TMatrix2D;

  //=== ct9999 =====================
  TFace3D = Tmat3x3;
  TFace3DArray = array of TFace3D;
  //================================

  { Tmat2x2Helper }

  Tmat2x2Helper = type Helper for Tmat2x2
  public
    procedure Identity;
    procedure Zero;
    procedure Uniform(ShaderID: GLint);
  end;

  { Tmat3x3Helper }

  Tmat3x3Helper = type Helper for Tmat3x3
  public
    procedure Identity;
    procedure Zero;

    procedure Scale(x, y: GLfloat); overload;
    procedure Scale(s: GLfloat); overload;
    procedure Translate(x, y: GLfloat);
    procedure TranslateLocalspace(x, y: GLfloat);
    procedure Rotate(w: GLfloat);
    procedure Transpose;
    procedure Shear(x, y: GLfloat);

    procedure Frustum(left, right, zNear, zFar: GLfloat);

    // Mesh / Vektor manipulieren
    procedure ScaleV(x, y, z: GLfloat); overload;
    procedure ScaleV(s: GLfloat); overload;
    procedure TranslateV(x, y, z: GLfloat);
    procedure CrossV(const m: Tmat3x3); overload;
    procedure CrossV(const v0, v1: TVector3f); overload;
    procedure CrossV(const v0, v1, v2: TVector3f); overload;


    procedure Uniform(ShaderID: GLint);
  end;

  { Tmat4x4Helper }

  Tmat4x4Helper = type Helper for Tmat4x4
  public
    procedure Identity;
    procedure Zero;

    procedure Ortho(left, right, bottom, top, znear, zfar: GLfloat);
    procedure Frustum(left, right, bottom, top, znear, zfar: GLfloat);
    procedure Perspective(fovy, aspect, znear, zfar: GLfloat);

    procedure Scale(Faktor: GLfloat); overload;
    procedure Scale(FaktorX, FaktorY, FaktorZ: GLfloat); overload;
    procedure Translate(x, y, z: GLfloat); overload;     // Worldspace Translation
    procedure Translate(const v: TVector3f); overload;
    procedure TranslateX(x: GLfloat);
    procedure TranslateY(y: GLfloat);
    procedure TranslateZ(z: GLfloat);
    procedure TranslateLocalspace(x, y, z: GLfloat);     // Localspace Translation
    procedure Rotate(Winkel, x, y, z: GLfloat); overload;
    procedure Rotate(Winkel: GLfloat; const a: TVector3f); overload;
    procedure RotateA(Winkel: GLfloat);
    procedure RotateB(Winkel: GLfloat);
    procedure RotateC(Winkel: GLfloat);
    procedure Transpose;
    procedure ShearA(y, z: GLfloat);
    procedure ShearB(x, z: GLfloat);
    procedure ShearC(x, y: GLfloat);

    procedure Uniform(ID: GLint);

    procedure WriteMatrix;
  end;

  //=== ct9999 =====================================

  { TMatrix2D }

  TMatrix2DObj = class(TObject)
  private
    FMatrix: TMat3x3;
    BackupMatrix: array of Tmat3x3;
  public
    property Matrix: TMat3x3 read FMatrix;

    constructor Create;
    procedure Identity;
    procedure Assign(m: TMatrix2DObj);
    procedure Multiply(m1, m2: TMatrix2DObj);

    procedure Push;
    procedure Pop;
    procedure LoadStack;

    procedure Uniform(ShaderID: GLint);

    procedure Scale(x, y: single); overload;
    procedure Scale(s: single); overload;
    procedure Translate(x, y: single);
    procedure Rotate(w: single);
    procedure Shear(x, y: single);

    function Vektor_Multi(Vector: TVector3f): TVector3f;

    procedure SetMatrix(m: TMat3x3);

    procedure Frustum(left, right, zNear, zFar: single);
  end;

  { TMatrix }

  TMatrixObj = class(TObject)
  private
    FMatrix: Tmat4x4;
    BackupMatrix: array of Tmat4x4;
  public
    constructor Create;
    procedure Identity;
    procedure Assign(m: TMatrixObj);
    procedure Multiply(const m1t, m2t: TMatrixObj);

    procedure Push;
    procedure Pop;
    procedure LoadStack;

    procedure Uniform(ShaderID: GLint);

    procedure Ortho(left, right, bottom, top, znear, zfar: single);
    procedure Frustum(left, right, bottom, top, znear, zfar: single);
    procedure Perspective(fovy, aspect, znear, zfar: single);

    procedure Rotate(Angele, x, y, z: GLfloat); overload;
    procedure Rotate(Angele: GLfloat; a: TVector3f); overload;
    procedure RotateA(Angele: GLfloat);
    procedure RotateB(Angele: GLfloat);
    procedure RotateC(Angele: GLfloat);
    procedure Translate(x, y, z: GLfloat);
    procedure NewTranslate(x, y, z: GLfloat);
    procedure Scale(Faktor: GLfloat); overload;
    procedure Scale(FaktorX, FaktorY, FaktorZ: GLfloat); overload;
    procedure Transpose;

    procedure WriteMatrix;
  end;

procedure FaceToNormale(var Face, Normal: array of TFace3D);
//================================================


function mat3(const v0, v1, v2: TVector3f): Tmat3x3;
function mat4(const v0, v1, v2, v3: TVector4f): Tmat4x4;
function mat3x2(const v0, v1, v2: TVector2f): Tmat3x2;


operator * (const m: Tmat2x2; const v: TVector2f) Res: TVector2f;
operator * (const m: Tmat3x3; const v: TVector3f) Res: TVector3f;
operator * (const m: Tmat4x4; const v: TVector4f) Res: TVector4f;

operator +(const mat0, mat1: Tmat2x2) Res: Tmat2x2;
operator +(const mat0, mat1: Tmat3x3) Res: Tmat3x3;
operator +(const mat0, mat1: Tmat4x4) Res: Tmat4x4;

operator -(const mat0, mat1: Tmat2x2) Res: Tmat2x2;
operator -(const mat0, mat1: Tmat3x3) Res: Tmat3x3;
operator -(const mat0, mat1: Tmat4x4) Res: Tmat4x4;

operator * (const mat0, mat1: Tmat2x2) Res: Tmat2x2;
operator * (const mat0, mat1: Tmat3x3) Res: Tmat3x3;
operator * (const mat0, mat1: Tmat4x4) Res: Tmat4x4;

// === Privater Teil ===

implementation

{ Tmat2x2Helper }

procedure Tmat2x2Helper.Identity; inline;
const
  m: TMat2x2 = ((1.0, 0.0), (0.0, 1.0));
begin
  Self := m;
end;

procedure Tmat2x2Helper.Zero; inline;
const
  m: TMat2x2 = ((0.0, 0.0), (0.0, 0.0));
begin
  Self := m;
end;

procedure Tmat2x2Helper.Uniform(ShaderID: GLint); inline;
begin
  glUniformMatrix2fv(ShaderID, 1, false.ToInteger, @Self);
end;

{ Tmat3x3Helper }

procedure Tmat3x3Helper.Identity; inline;
const
  m: TMat3x3 = ((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0));
begin
  Self := m;
end;

procedure Tmat3x3Helper.Zero; inline;
const
  m: TMat3x3 = ((0.0, 0.0, 0.0), (0.0, 0.0, 0.0), (0.0, 0.0, 0.0));
begin
  Self := m;
end;

procedure Tmat3x3Helper.Scale(x, y: GLfloat);
var
  i: integer;
begin
  for i := 0 to 1 do
  begin
    Self[i, 0] *= x;
    Self[i, 1] *= y;
  end;
end;

procedure Tmat3x3Helper.Scale(s: GLfloat);
var
  i: integer;
begin
  for i := 0 to 1 do
  begin
    Self[i, 0] *= s;
    Self[i, 1] *= s;
  end;
end;

procedure Tmat3x3Helper.ScaleV(x, y, z: GLfloat);
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    Self[i, 0] *= x;
    Self[i, 1] *= y;
    Self[i, 2] *= z;
  end;
end;

procedure Tmat3x3Helper.ScaleV(s: GLfloat);
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    Self[i, 0] *= s;
    Self[i, 1] *= s;
    Self[i, 2] *= s;
  end;
end;

procedure Tmat3x3Helper.TranslateV(x, y, z: GLfloat);
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    Self[i, 0] += x;
    Self[i, 1] += y;
    Self[i, 2] += z;
  end;
end;

procedure Tmat3x3Helper.CrossV(const m: Tmat3x3);
begin
  CrossV(m[0], m[1], m[2]);
end;

procedure Tmat3x3Helper.CrossV(const v0, v1: TVector3f);
var
  v: TVector3f;
begin
  v.Cross(v0, v1);
  Self[0] := v;
  Self[1] := v;
  Self[2] := v;
end;

procedure Tmat3x3Helper.CrossV(const v0, v1, v2: TVector3f);
var
  v: TVector3f;
begin
  v.Cross(v0, v1, v2);
  Self[0] := v;
  Self[1] := v;
  Self[2] := v;
end;

procedure Tmat3x3Helper.Translate(x, y: GLfloat); inline;
begin
  Self[2, 0] += x;
  Self[2, 1] += y;
end;

procedure Tmat3x3Helper.TranslateLocalspace(x, y: GLfloat);
var
  i: integer;
begin
  for i := 0 to 2 do Self[2, i] += Self[0, i] * x + Self[1, i] * y;
end;

procedure Tmat3x3Helper.Rotate(w: GLfloat);
var
  i: integer;
  x, y: GLfloat;
begin
  for i := 0 to 1 do
  begin
    x := Self[i, 0];
    y := Self[i, 1];
    Self[i, 0] := x * Cos(w) - y * Sin(w);
    Self[i, 1] := x * Sin(w) + y * Cos(w);
  end;
end;

procedure Tmat3x3Helper.Transpose;
var
  i, j: integer;
  m: Tmat3x3;
begin
  for i := 0 to 2 do for j := 0 to 2 do m[i, j] := Self[j, i];
  Self := m;
end;

procedure Tmat3x3Helper.Uniform(ShaderID: GLint); inline;
begin
  glUniformMatrix3fv(ShaderID, 1, false.ToInteger, @Self);
end;

procedure Tmat3x3Helper.Shear(x, y: GLfloat);
begin
  Self[0, 1] += y;
  Self[1, 0] += x;
end;

procedure Tmat3x3Helper.Frustum(left, right, zNear, zFar: GLfloat);
begin
  Identity;
  Self[0, 0] := 2 * zNear / (right - left);
  Self[1, 0] := (right + left) / (right - left);
  Self[1, 1] := -(zFar + zNear) / (zFar - zNear);
  Self[1, 2] := -1.0;
  Self[2, 1] := -2 * zFar * zNear / (zFar - zNear);
  Self[2, 2] := 0.0;
end;

{ TMatrix }

procedure Tmat4x4Helper.Identity; inline;
const
  m: Tmat4x4 = ((1.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.0),
    (0.0, 0.0, 1.0, 0.0), (0.0, 0.0, 0.0, 1.0));
begin
  Self := m;
end;

procedure Tmat4x4Helper.Zero; inline;
const
  m: Tmat4x4 = ((0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0),
    (0.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 0.0));
begin
  Self := m;
end;

procedure Tmat4x4Helper.Ortho(left, right, bottom, top, znear, zfar: GLfloat);
begin
  Identity;
  Self[0, 0] := 2 / (right - left);
  Self[1, 1] := 2 / (top - bottom);
  Self[2, 2] := -2 / (zfar - znear);
  Self[3, 0] := -(right + left) / (right - left);
  Self[3, 1] := -(top + bottom) / (top - bottom);
  Self[3, 2] := -(zfar + znear) / (zfar - znear);
end;

procedure Tmat4x4Helper.Frustum(left, right, bottom, top, znear, zfar: GLfloat);
begin
  Identity;
  Self[0, 0] := 2 * znear / (right - left);
  Self[1, 1] := 2 * znear / (top - bottom);
  Self[2, 0] := (right + left) / (right - left);
  Self[2, 1] := (top + bottom) / (top - bottom);
  Self[2, 2] := -(zfar + znear) / (zfar - znear);
  Self[2, 3] := -1.0;
  Self[3, 2] := -2 * zfar * znear / (zfar - znear);
  Self[3, 3] := 0.0;
end;

procedure Tmat4x4Helper.Perspective(fovy, aspect, znear, zfar: GLfloat);
var
  p, right, top: GLfloat;
begin
  p := fovy * Pi / 360;
  top := znear * (Sin(p) / Cos(p)); // top := znear * tan(p);
  right := top * aspect;
  Frustum(-right, right, -top, top, znear, zfar);
end;


//mat4 rotationMatrix(vec3 axis, float angle)
//{
//    axis = normalize(axis);
//    float s = sin(angle);
//    float c = cos(angle);
//    float oc = 1.0 - c;

//    return mat4(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,  0.0,
//                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,  0.0,
//                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c,           0.0,
//                0.0,                                0.0,                                0.0,                                1.0);


procedure Tmat4x4Helper.Rotate(Winkel, x, y, z: GLfloat);
var
  c, s: GLfloat;      // Funktionierts ?????
  m: Tmat4x4;
begin
  m.Identity;
  c := Cos(Winkel);
  s := Sin(Winkel);

  m[0, 0] := x * x * (1 - c) + c;
  m[1, 0] := x * y * (1 - c) - z * s;
  m[2, 0] := x * z * (1 - c) + y * s;

  m[0, 1] := y * x * (1 - c) + z * s;
  m[1, 1] := y * y * (1 - c) + c;
  m[2, 1] := y * z * (1 - c) - x * s;

  m[0, 2] := x * z * (1 - c) - y * s;
  m[1, 2] := y * z * (1 - c) + x * s;
  m[2, 2] := z * z * (1 - c) + c;
  Self := m;
end;

procedure Tmat4x4Helper.Rotate(Winkel: GLfloat; const a: TVector3f); inline;
begin
  Rotate(Winkel, a[0], a[1], a[2]);
end;

procedure Tmat4x4Helper.Translate(x, y, z: GLfloat); inline;
begin
  Self[0, 3] += x;
  Self[1, 3] += y;
  Self[2, 3] += z;
end;

procedure Tmat4x4Helper.Translate(const v: TVector3f); inline;
begin
  Self[0, 3] += v[0];
  Self[1, 3] += v[1];
  Self[2, 3] += v[2];
end;

procedure Tmat4x4Helper.TranslateX(x: GLfloat);
begin
  Self[3, 0] += x;
end;

procedure Tmat4x4Helper.TranslateY(y: GLfloat);
begin
  Self[3, 1] += y;
end;

procedure Tmat4x4Helper.TranslateZ(z: GLfloat);
begin
  Self[3, 2] += z;
end;

procedure Tmat4x4Helper.TranslateLocalspace(x, y, z: GLfloat);
var
  i: integer;
begin
  for i := 0 to 3 do Self[3, i] += Self[0, i] * x + Self[1, i] * y + Self[2, i] * z;
end;

procedure Tmat4x4Helper.RotateA(Winkel: GLfloat);
var
  i: integer;
  y, z, c, s: GLfloat;
begin
  c := Cos(Winkel);
  s := Sin(Winkel);
  for i := 0 to 2 do
  begin
    y := Self[i, 1];
    z := Self[i, 2];
    Self[i, 1] := y * c - z * s;
    Self[i, 2] := y * s + z * c;
  end;
end;

procedure Tmat4x4Helper.RotateB(Winkel: GLfloat);
var
  i: integer;
  x, z, c, s: GLfloat;
begin
  c := Cos(Winkel);
  s := Sin(Winkel);
  for i := 0 to 2 do
  begin
    x := Self[i, 0];
    z := Self[i, 2];
    Self[i, 0] := x * c - z * s;
    Self[i, 2] := x * s + z * c;
  end;
end;

procedure Tmat4x4Helper.RotateC(Winkel: GLfloat);
var
  i: integer;
  x, y, c, s: GLfloat;
begin
  c := Cos(Winkel);
  s := Sin(Winkel);
  for i := 0 to 2 do
  begin
    x := Self[i, 0];
    y := Self[i, 1];
    Self[i, 0] := x * c - y * s;
    Self[i, 1] := x * s + y * c;
  end;
end;

procedure Tmat4x4Helper.Scale(Faktor: GLfloat);
var
  x, y: integer;
begin
  for x := 0 to 2 do for y := 0 to 2 do Self[x, y] *= Faktor;
end;

procedure Tmat4x4Helper.Scale(FaktorX, FaktorY, FaktorZ: GLfloat);
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    Self[i, 0] *= FaktorX;
    Self[i, 1] *= FaktorY;
    Self[i, 2] *= FaktorZ;
  end;
end;

procedure Tmat4x4Helper.ShearA(y, z: GLfloat); inline;
begin
  Self[2, 1] += y;
  Self[1, 2] += z;
end;

procedure Tmat4x4Helper.ShearB(x, z: GLfloat); inline;
begin
  Self[2, 0] += x;
  Self[0, 2] += z;
end;

procedure Tmat4x4Helper.ShearC(x, y: GLfloat); inline;
begin
  Self[1, 0] += x;
  Self[0, 1] += y;
end;


procedure Tmat4x4Helper.Transpose;
var
  i, j: integer;
  m: Tmat4x4;
begin
  for i := 0 to 3 do for j := 0 to 3 do m[i, j] := Self[j, i];
  Self := m;
end;

procedure Tmat4x4Helper.Uniform(ID: GLint); inline;
begin
  glUniformMatrix4fv(ID, 1, false.ToInteger, @Self);
end;


procedure Tmat4x4Helper.WriteMatrix;
var
  x, y: integer;
begin
  for y := 0 to 3 do
  begin
    for x := 0 to 3 do Write(FormatFloat('###0.0000', Self[x, y]));
    WriteLn;
  end;
end;

// === Hilfsfunktionen, Ã¤hnlich GLSL ===

function mat3(const v0, v1, v2: TVector3f): Tmat3x3; inline;
begin
  Result[0] := v0;
  Result[1] := v1;
  Result[2] := v2;
end;

function mat4(const v0, v1, v2, v3: TVector4f): Tmat4x4; inline;
begin
  Result[0] := v0;
  Result[1] := v1;
  Result[2] := v2;
  Result[3] := v3;
end;

function mat3x2(const v0, v1, v2: TVector2f): Tmat3x2; inline;
begin
  Result[0] := v0;
  Result[1] := v1;
  Result[2] := v2;
end;

// === Ãœberladene Opertoren fÃ¼r Matrixmultiplikation ===

operator * (const m: Tmat2x2; const v: TVector2f)Res: TVector2f;
var
  i: integer;
begin
  for i := 0 to 1 do Res[i] := m[0, i] * v[0] + m[1, i] * v[1];
end;

operator * (const m: Tmat3x3; const v: TVector3f)Res: TVector3f;
var
  i: integer;
begin
  for i := 0 to 2 do Res[i] := m[0, i] * v[0] + m[1, i] * v[1] + m[2, i] * v[2];
end;

operator +(const mat0, mat1: Tmat2x2)Res: Tmat2x2;
var
  i, j: integer;
begin
  for i := 0 to 1 do for j := 0 to 1 do Res[i, j] := mat0[i, j] + mat1[i, j];
end;

operator +(const mat0, mat1: Tmat3x3)Res: Tmat3x3;
var
  i, j: integer;
begin
  for i := 0 to 2 do for j := 0 to 2 do Res[i, j] := mat0[i, j] + mat1[i, j];
end;

operator +(const mat0, mat1: Tmat4x4)Res: Tmat4x4;
var
  i, j: integer;
begin
  for i := 0 to 3 do for j := 0 to 3 do Res[i, j] := mat0[i, j] + mat1[i, j];
end;

operator -(const mat0, mat1: Tmat2x2)Res: Tmat2x2;
var
  i, j: integer;
begin
  for i := 0 to 1 do for j := 0 to 1 do Res[i, j] := mat0[i, j] - mat1[i, j];
end;

operator -(const mat0, mat1: Tmat3x3)Res: Tmat3x3;
var
  i, j: integer;
begin
  for i := 0 to 2 do for j := 0 to 2 do Res[i, j] := mat0[i, j] - mat1[i, j];
end;

operator -(const mat0, mat1: Tmat4x4)Res: Tmat4x4;
var
  i, j: integer;
begin
  for i := 0 to 3 do for j := 0 to 3 do Res[i, j] := mat0[i, j] - mat1[i, j];
end;

operator * (const mat0, mat1: Tmat2x2) Res: Tmat2x2;
var
  i, j, k: integer;
begin
  for i := 0 to 1 do for j := 0 to 1 do
    begin
      Res[i, j] := 0.0;
      for k := 0 to 1 do Res[i, j] += mat1[i, k] * mat0[k, j];
    end;
end;

operator * (const mat0, mat1: Tmat3x3) Res: Tmat3x3;
var
  i, j, k: integer;
begin
  for i := 0 to 2 do for j := 0 to 2 do
    begin
      Res[i, j] := 0.0;
      for k := 0 to 2 do Res[i, j] += mat1[i, k] * mat0[k, j];
    end;
end;

{$if defined(cpux86_64) or defined(cpux86)}
{$asmmode intel}
operator * (const m: Tmat4x4; const v: TVector4f)Res: TVector4f; assembler;
  nostackframe; register;
asm
         MOVUPS  XMM4, [m + $00]
         MOVUPS  XMM5, [m + $10]
         MOVUPS  XMM6, [m + $20]
         MOVUPS  XMM7, [m + $30]
         MOVUPS  XMM2, [v]

         // Zeile 0
         PSHUFD  XMM0, XMM2, 00000000b
         MULPS   XMM0, XMM4

         // Zeile 1
         PSHUFD  XMM1, XMM2, 01010101b
         MULPS   XMM1, XMM5
         ADDPS   XMM0, XMM1

         // Zeile 2
         PSHUFD  XMM1, XMM2, 10101010b
         MULPS   XMM1, XMM6
         ADDPS   XMM0, XMM1

         // Zeile 3
         PSHUFD  XMM1, XMM2, 11111111b
         MULPS   XMM1, XMM7
         ADDPS   XMM0, XMM1

         MOVUPS  [Res], XMM0
end;

operator * (const mat0, mat1: Tmat4x4)Res: Tmat4x4; assembler; nostackframe; register;
asm
         MOVUPS  XMM4, [mat0 + $00]
         MOVUPS  XMM5, [mat0 + $10]
         MOVUPS  XMM6, [mat0 + $20]
         MOVUPS  XMM7, [mat0 + $30]

         // Spalte 0
         MOVUPS  XMM2, [mat1 + $00]

         PSHUFD  XMM0, XMM2, 00000000b
         MULPS   XMM0, XMM4

         PSHUFD  XMM1, XMM2, 01010101b
         MULPS   XMM1, XMM5
         ADDPS   XMM0, XMM1

         PSHUFD  XMM1, XMM2, 10101010b
         MULPS   XMM1, XMM6
         ADDPS   XMM0, XMM1

         PSHUFD  XMM1, XMM2, 11111111b
         MULPS   XMM1, XMM7
         ADDPS   XMM0, XMM1

         MOVUPS  [Result + $00], XMM0

         // Spalte 1
         MOVUPS  XMM2, [mat1 + $10]

         PSHUFD  XMM0, XMM2, 00000000b
         MULPS   XMM0, XMM4

         PSHUFD  XMM1, XMM2, 01010101b
         MULPS   XMM1, XMM5
         ADDPS   XMM0, XMM1

         PSHUFD  XMM1, XMM2, 10101010b
         MULPS   XMM1, XMM6
         ADDPS   XMM0, XMM1

         PSHUFD  XMM1, XMM2, 11111111b
         MULPS   XMM1, XMM7
         ADDPS   XMM0, XMM1

         MOVUPS   [Result + $10], XMM0

         // Spalte 2
         MOVUPS  XMM2, [mat1 + $20]

         PSHUFD  XMM0, XMM2, 00000000b
         MULPS   XMM0, XMM4

         PSHUFD  XMM1, XMM2, 01010101b
         MULPS   XMM1, XMM5
         ADDPS   XMM0, XMM1

         PSHUFD  XMM1, XMM2, 10101010b
         MULPS   XMM1, XMM6
         ADDPS   XMM0, XMM1

         PSHUFD  XMM1, XMM2, 11111111b
         MULPS   XMM1, XMM7
         ADDPS   XMM0, XMM1

         MOVUPS  [Result + $20], XMM0

         // Spalte 3
         MOVUPS  XMM2, [mat1 + $30]

         PSHUFD  XMM0, XMM2, 00000000b
         MULPS   XMM0, XMM4

         PSHUFD  XMM1, XMM2, 01010101b
         MULPS   XMM1, XMM5
         ADDPS   XMM0, XMM1

         PSHUFD  XMM1, XMM2, 10101010b
         MULPS   XMM1, XMM6
         ADDPS   XMM0, XMM1

         PSHUFD  XMM1, XMM2, 11111111b
         MULPS   XMM1, XMM7
         ADDPS   XMM0, XMM1

         MOVUPS  [Result + $30], XMM0
end;
{$else}
operator * (const m: Tmat4x4; v: TVector4f) Res: TVector4f;
var
  i: integer;
begin
  for i := 0 to 3 do Res[i] := m[0, i] * v[0] + m[1, i] * v[1] + m[2, i] * v[2] + m[3, i] * v[3];
end;

operator * (const mat0, mat1: Tmat4x4) Res: Tmat4x4;
var
  i, j, k: integer;
begin
  for i := 0 to 3 do for j := 0 to 3 do
    begin
      Res[i, j] := 0.0;
      for k := 0 to 3 do Res[i, j] += mat1[i, k] * mat0[k, j];
    end;
end;

{$endif}


//=== ct9999 =================================
//============================================


{ TMatrix2DObjObj }

constructor TMatrix2DObj.Create;
begin
  inherited Create;
  Identity;
end;

procedure TMatrix2DObj.Identity;
const
  m: TMat3x3 = ((1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0));
begin
  FMatrix := m;
end;

procedure TMatrix2DObj.Assign(m: TMatrix2DObj);
begin
  FMatrix := m.FMatrix;
end;

procedure TMatrix2DObj.Scale(x, y: single);
var
  i: integer;
begin
  for i := 0 to 1 do
  begin
    FMatrix[i, 0] := FMatrix[i, 0] * x;
    FMatrix[i, 1] := FMatrix[i, 1] * y;
  end;
end;

procedure TMatrix2DObj.Scale(s: single);
begin
  Scale(s, s);
end;

procedure TMatrix2DObj.Translate(x, y: single);
begin
  FMatrix[2, 0] := FMatrix[2, 0] + x;
  FMatrix[2, 1] := FMatrix[2, 1] + y;
end;

procedure TMatrix2DObj.Rotate(w: single);
var
  i: integer;
  x, y: GLfloat;
begin
  for i := 0 to 1 do
  begin
    x := FMatrix[i, 0];
    y := FMatrix[i, 1];
    FMatrix[i, 0] := x * Cos(w) - y * Sin(w);
    FMatrix[i, 1] := x * Sin(w) + y * Cos(w);
  end;
end;

procedure TMatrix2DObj.Multiply(m1, m2: TMatrix2DObj);
var
  i, j, k: integer;
  m: TMat3x3;
begin
  for i := 0 to 2 do for j := 0 to 2 do
    begin
      m[i, j] := 0;
      for k := 0 to 2 do m[i, j] := m[i, j] + m2.FMatrix[i, k] * m1.FMatrix[k, j];
    end;
  FMatrix := m;
end;

procedure TMatrix2DObj.Push;
var
  pos: integer;
begin
  pos := Length(BackupMatrix);
  SetLength(BackupMatrix, pos + 1);
  BackupMatrix[pos] := FMatrix;
end;

procedure TMatrix2DObj.Pop;
var
  pos: integer;
begin
  pos := Length(BackupMatrix) - 1;
  FMatrix := BackupMatrix[pos];
  SetLength(BackupMatrix, pos);
end;

procedure TMatrix2DObj.LoadStack;
begin
  FMatrix := BackupMatrix[Length(BackupMatrix) - 1];
end;

procedure TMatrix2DObj.Uniform(ShaderID: GLint);
begin
  glUniformMatrix3fv(ShaderID, 1, false.ToInteger, @FMatrix);
end;

function TMatrix2DObj.Vektor_Multi(Vector: TVector3f): TVector3f;
var
  i: integer;
begin
  for i := 0 to 2 do Result[i] := FMatrix[0, i] * Vector[0] + FMatrix[1, i] * Vector[1] + FMatrix[2, i] * Vector[2];
end;

procedure TMatrix2DObj.SetMatrix(m: TMat3x3);
begin
  FMatrix := m;
end;

procedure TMatrix2DObj.Shear(x, y: single);
begin
  FMatrix[0, 1] += y;
  FMatrix[1, 0] += x;
end;

procedure TMatrix2DObj.Frustum(left, right, zNear, zFar: single); // geht nicht.
begin
  Identity;
  FMatrix[0, 0] := 2 * zNear / (right - left);
  FMatrix[1, 0] := (right + left) / (right - left);
  FMatrix[1, 1] := -(zFar + zNear) / (zFar - zNear);
  FMatrix[1, 2] := -1;
  FMatrix[2, 1] := -2 * zFar * zNear / (zFar - zNear);
  FMatrix[2, 2] := 0;
end;

{ TMatrixObj }

constructor TMatrixObj.Create;
begin
  inherited Create;
  Identity;
end;

procedure TMatrixObj.Identity;
const
  m: Tmat4x4 = ((1.0, 0.0, 0.0, 0.0), (0.0, 1.0, 0.0, 0.0), (0.0, 0.0, 1.0, 0.0), (0.0, 0.0, 0.0, 1.0));
begin
  FMatrix := m;
end;

procedure TMatrixObj.Assign(m: TMatrixObj);
begin
  FMatrix := m.FMatrix;
end;

procedure TMatrixObj.Ortho(left, right, bottom, top, znear, zfar: single);
begin
  Identity;
  FMatrix[0, 0] := 2 / (right - left);
  FMatrix[1, 1] := 2 / (top - bottom);
  FMatrix[2, 2] := -2 / (zfar - znear);
  FMatrix[3, 0] := -(right + left) / (right - left);
  FMatrix[3, 1] := -(top + bottom) / (top - bottom);
  FMatrix[3, 2] := -(zfar + znear) / (zfar - znear);
end;

procedure TMatrixObj.Frustum(left, right, bottom, top, znear, zfar: single);
begin
  Identity;
  FMatrix[0, 0] := 2 * znear / (right - left);
  FMatrix[1, 1] := 2 * znear / (top - bottom);
  FMatrix[2, 0] := (right + left) / (right - left);
  FMatrix[2, 1] := (top + bottom) / (top - bottom);
  FMatrix[2, 2] := -(zfar + znear) / (zfar - znear);
  FMatrix[2, 3] := -1;
  FMatrix[3, 2] := -2 * zfar * znear / (zfar - znear);
  FMatrix[3, 3] := 0;
end;

procedure TMatrixObj.Perspective(fovy, aspect, znear, zfar: single);
var
  p, right, top: single;
begin
  p := fovy * Pi / 360;
  top := znear * (Sin(p) / Cos(p));
  //    top := znear * tan(p);
  right := top * aspect;
  Frustum(-right, right, -top, top, znear, zfar);
end;


procedure TMatrixObj.Rotate(Angele, x, y, z: GLfloat);
var
  c, s: GLfloat;      // Funktionierts ?????
  m: Tmat4x4;
begin
  c := Cos(Angele);
  s := Sin(Angele);

  m[0, 0] := x * x * (1 - c) + c;
  m[1, 0] := x * y * (1 - c) - z * s;
  m[2, 0] := x * z * (1 - c) + y * s;

  m[0, 1] := y * x * (1 - c) + z * s;
  m[1, 1] := y * y * (1 - c) + c;
  m[2, 1] := y * z * (1 - c) - x * s;

  m[0, 2] := x * z * (1 - c) - y * s;
  m[1, 2] := y * z * (1 - c) + x * s;
  m[2, 2] := z * z * (1 - c) + c;
  FMatrix := m;
end;

procedure TMatrixObj.Rotate(Angele: GLfloat; a: TVector3f);
begin
  Rotate(Angele, a[0], a[1], a[2]);
end;

procedure TMatrixObj.Translate(x, y, z: GLfloat);
begin
  FMatrix[3, 0] := FMatrix[3, 0] + x;
  FMatrix[3, 1] := FMatrix[3, 1] + y;
  FMatrix[3, 2] := FMatrix[3, 2] + z;
end;

procedure TMatrixObj.NewTranslate(x, y, z: GLfloat);
var
  i: integer;
begin
  for i := 0 to 3 do FMatrix[3, i] += FMatrix[0, i] * x + FMatrix[1, i] * y + FMatrix[2, i] * z;
end;


procedure TMatrixObj.RotateA(Angele: GLfloat);
var
  i: integer;
  y, z, c, s: GLfloat;
begin
  c := Cos(Angele);
  s := Sin(Angele);
  for i := 0 to 2 do
  begin
    y := FMatrix[i, 1];
    z := FMatrix[i, 2];
    FMatrix[i, 1] := y * c - z * s;
    FMatrix[i, 2] := y * s + z * c;
  end;
end;


procedure TMatrixObj.RotateB(Angele: GLfloat);
var
  i: integer;
  x, z, c, s: GLfloat;
begin
  c := Cos(Angele);
  s := Sin(Angele);
  for i := 0 to 2 do
  begin
    x := FMatrix[i, 0];
    z := FMatrix[i, 2];
    FMatrix[i, 0] := x * c - z * s;
    FMatrix[i, 2] := x * s + z * c;
  end;
end;


procedure TMatrixObj.RotateC(Angele: GLfloat);
var
  i: integer;
  x, y, c, s: GLfloat;
begin
  c := Cos(Angele);
  s := Sin(Angele);
  for i := 0 to 2 do
  begin
    x := FMatrix[i, 0];
    y := FMatrix[i, 1];
    FMatrix[i, 0] := x * c - y * s;
    FMatrix[i, 1] := x * s + y * c;
  end;
end;


procedure TMatrixObj.Scale(Faktor: GLfloat);
var
  x, y: integer;
begin
  for x := 0 to 2 do for y := 0 to 2 do FMatrix[x, y] *= Faktor;
end;

procedure TMatrixObj.Scale(FaktorX, FaktorY, FaktorZ: GLfloat);
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    FMatrix[i, 0] *= FaktorX;
    FMatrix[i, 1] *= FaktorY;
    FMatrix[i, 2] *= FaktorZ;
  end;
end;

procedure TMatrixObj.Transpose;
var
  i, j: integer;
  m: Tmat4x4;
begin
  for i := 0 to 3 do for j := 0 to 3 do m[i, j] := FMatrix[j, i];
  FMatrix := m;
end;

procedure TMatrixObj.Multiply(const m1t, m2t: TMatrixObj);
var
  x, y, i: integer;
  m: Tmat4x4;
begin
  for x := 0 to 3 do for y := 0 to 3 do
    begin
      m[x, y] := 0;
      for i := 0 to 3 do m[x, y] += m1t.FMatrix[i, y] * m2t.FMatrix[x, i];
    end;
  FMatrix := m;
end;

procedure TMatrixObj.Push;
var
  pos: integer;
begin
  pos := Length(BackupMatrix);
  SetLength(BackupMatrix, pos + 1);
  BackupMatrix[pos] := FMatrix;
end;

procedure TMatrixObj.Pop;
var
  pos: integer;
begin
  pos := Length(BackupMatrix) - 1;
  FMatrix := BackupMatrix[pos];
  SetLength(BackupMatrix, pos);
end;

procedure TMatrixObj.LoadStack;
begin
  FMatrix := BackupMatrix[Length(BackupMatrix) - 1];
end;

procedure TMatrixObj.Uniform(ShaderID: GLint);
begin
  glUniformMatrix4fv(ShaderID, 1, false.ToInteger, @FMatrix);
end;


procedure TMatrixObj.WriteMatrix;
var
  x, y: integer;
begin
  for y := 0 to 3 do
  begin
    for x := 0 to 3 do Write(FormatFloat('###0.0000', FMatrix[x, y]));
    WriteLn;
  end;
end;


procedure FaceToNormale(var Face, Normal: array of TFace3D);

  function GetCrossProduct(P0, P1, P2: TVector3f): TVector3f;
  var
    a, b: TVector3f;
    i: integer;
  begin
    for i := 0 to 2 do
    begin
      a[i] := P1[i] - P0[i];
      b[i] := P2[i] - P0[i];
    end;
    Result[0] := a[1] * b[2] - a[2] * b[1];
    Result[1] := a[2] * b[0] - a[0] * b[2];
    Result[2] := a[0] * b[1] - a[1] * b[0];

    Result.Normalize;
  end;

var
  i: integer;
  v: TVector3f;
begin
  if Length(Normal) < Length(Face) then Exit//ShowMessage('Error: Lenght(Normal) <> Length(Face)');
  ;
  for i := 0 to Length(Face) - 1 do
  begin
    v := GetCrossProduct(Face[i, 0], Face[i, 1], Face[i, 2]);
    Normal[i, 0] := v;
    Normal[i, 1] := v;
    Normal[i, 2] := v;
  end;
end;

//=============================================================


end.
