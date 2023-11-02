unit LearnOpenGL.GLM;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  LearnOpenGL.Vector,
  LearnOpenGL.Matrix;

type
  // 向量
  TVec2 = TVector2f;
  TVec3 = TVector3f;
  TVec4 = TVector4f;
  // 矩阵
  TMat2 = Tmat2x2;
  TMat3 = Tmat3x3;
  TMat4 = Tmat4x4;

  TArr_Single16 = array[0..15] of single;

  // 线性代数辅助类
  TGLM = class(TObject)
  public
    class function Vec2(x, y: single): TVec2; static;
    class function Vec3(x, y, z: single): TVec3; static;
    class function Vec4(x, y, z, w: single): TVec4; static;
    class function Mat4_Identity: TMat4; static; // 返回一个单位矩阵(Identity Matrix)
    class function Mat4_Zero: TMat4; static; // 返回一个空矩阵(Zero Matrix)
    class function Mat4_Init(aa, ab, ac, ad, ba, bb, bc, bd, ca, cb, cc, cd, da,
      db, dc, dd: single): TMat4; static;

    // 返回弧度值
    class function Radians(degrees: single): single; static;

    // 返回位移矩阵
    class function Translate(m: TMat4; vec: TVec3): TMat4; static;
    // 返回旋转矩阵
    class function Rotate(m: TMat4; degree: single; vec: TVec3): TMat4; static;
    // 返回缩放矩阵
    class function Scale(m: TMat4; vec: TVec3): TMat4; static;
    // 按列优先返回一个一维数组指针
    class function ValuePtr(const m: TMat4): TArr_Single16; static;
    // 透视矩阵
    class function Perspective(fovy, aspect, znear, zfar: single): TMat4; static;
    // 正交矩阵
    class function Ortho(left, right, bottom, top, znear, zfar: single): TMat4; static;
    // 平截头体
    class function Frustum(left, right, bottom, top, znear, zfar: single): TMat4; static;
  end;

implementation


{ TGLM }

class function TGLM.Frustum(left, right, bottom, top, znear, zfar: single): TMat4;
var
  res: TMat4;
begin
  res.Identity;
  res.Frustum(left, right, bottom, top, znear, zfar);
  Result := res;
end;

class function TGLM.Mat4_Identity: TMat4;
var
  res: TMat4;
begin
  res.Identity;
  Result := res;
end;

class function TGLM.Mat4_Init(aa, ab, ac, ad, ba, bb, bc, bd, ca, cb, cc, cd, da, db, dc, dd: single): TMat4;
var
  res: TMat4;
begin
  res := [[aa, ab, ac, ad], [ba, bb, bc, bd], [ca, cb, cc, cd], [da, db, dc, dd]];
  Result := res;
end;

class function TGLM.Mat4_Zero: TMat4;
var
  res: TMat4;
begin
  res.Zero;
  Result := res;
end;

class function TGLM.Ortho(left, right, bottom, top, znear, zfar: single): TMat4;
var
  res: TMat4;
begin
  res.Identity;
  res.Ortho(left, right, bottom, top, znear, zfar);
  Result := res;
end;

class function TGLM.Perspective(fovy, aspect, znear, zfar: single): TMat4;
var
  res: TMat4;
begin
  res.Identity;
  res.Perspective(fovy, aspect, znear, zfar);
  Result := res;
end;

class function TGLM.Radians(degrees: single): single;
begin
  Result := DegToRad(degrees);
end;

class function TGLM.Rotate(m: TMat4; degree: single; vec: TVec3): TMat4;
var
  res: TMat4;
begin
  res.Identity;
  vec.Normalize;
  res.Rotate(degree, vec);
  Result := m * res;
end;

class function TGLM.Scale(m: TMat4; vec: TVec3): TMat4;
var
  res: TMat4;
begin
  res := m;
  res.Scale(vec[0], vec[1], vec[2]);
  Result := res;
end;

class function TGLM.Translate(m: TMat4; vec: TVec3): TMat4;
var
  res: TMat4;
begin
  res.Identity;
  res.Translate(vec[0], vec[1], vec[2]);
  Result := m * res;
end;

class function TGLM.ValuePtr(const m: TMat4): TArr_Single16;
var
  res: TArr_Single16;
  i, j: integer;
  temp: TMat4;
begin
  temp := m;
  temp.Transpose;

  for i := 0 to High(temp) do
    for j := 0 to High(temp[0]) do
      res[j + i * Length(temp[i])] := temp[i, j];

  Result := res;
end;

class function TGLM.Vec2(x, y: single): TVec2;
begin
  Result := LearnOpenGL.Vector.vec2(x, y);
end;

class function TGLM.Vec3(x, y, z: single): TVec3;
begin
  Result := LearnOpenGL.Vector.vec3(x, y, z);
end;

class function TGLM.Vec4(x, y, z, w: single): TVec4;
begin
  Result := LearnOpenGL.Vector.vec4(x, y, z, w);
end;

end.
