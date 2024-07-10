﻿unit DeepStar.OpenGL.GLM;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
interface

uses
  Classes,
  SysUtils,
  Math,
  DeepStar.UString,
  DeepStar.OpenGL.Vector,
  DeepStar.OpenGL.Matrix;

type
  // 向量
  TVec2 = DeepStar.OpenGL.Vector.TVec2;
  TVec3 = DeepStar.OpenGL.Vector.TVec3;
  TVec4 = DeepStar.OpenGL.Vector.TVec4;
  // 矩阵
  TMat3 = DeepStar.OpenGL.Matrix.TMat3;
  TMat4 = DeepStar.OpenGL.Matrix.TMat4;

  TArr_Single16 = array[0..15] of single;

  // 线性代数辅助类
  TGLM = class(TObject)
  public
    class function Vec2(x, y: single): TVec2;
    class function Vec3(x, y, z: single): TVec3;
    class function Vec4(x, y, z, w: single): TVec4;

    // 返回一个单位矩阵(Identity Matrix)
    class function Mat3_Identity: TMat3;
    // 返回一个空矩阵(Zero Matrix)
    class function Mat3_Zero: TMat3;
    // 使用给定值沿对角线创建一个矩阵
    class function Mat3(x: single): TMat3;
    class function Mat3_Init(x00, x01, x02, x10, x11, x12, x20, x21, x22: single): TMat3;

    // 返回一个单位矩阵(Identity Matrix)
    class function Mat4_Identity: TMat4;  // 返回一个单位矩阵(Identity Matrix)
    // 返回一个空矩阵(Zero Matrix)
    class function Mat4_Zero: TMat4;  // 返回一个空矩阵(Zero Matrix)
    // 使用给定值沿对角线创建一个矩阵
    class function Mat4(x: single): TMat4;
    class function Mat4_Init(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
      x30, x31, x32, x33: single): TMat4;

    //═════════════════════════════════════════════════════════════════════════

    // 向量归一化
    class function Normalize(vec: TVec3): TVec3;
    // 向量点乘：（内积）
    class function Dot(a, b: TVec3): single;
    // 向量叉乘：（外积）
    class function Cross(a, b: TVec3): TVec3;

    //═════════════════════════════════════════════════════════════════════════

    // 返回弧度值
    class function Radians(deg: single): single;
    // 返回角度值
    class function Degrees(Rad: single): single;

    //═════════════════════════════════════════════════════════════════════════
    // TMat4

    // 返回位移矩阵
    class function Translate(m: TMat4; vec: TVec3): TMat4;
    // 返回旋转矩阵
    class function Rotate(m: TMat4; deg: single; vec: TVec3): TMat4;
    // 返回缩放矩阵
    class function Scale(m: TMat4; vec: TVec3): TMat4;
    // 使用视场和创建透视图投影矩阵纵横比，以确定左，右，上，下平面。这
    // 方法类似于现在已弃用的gluPerspective方法。
    class function Perspective(fovy, aspect, znear, zfar: single): TMat4;
    // 正交矩阵
    class function Ortho(left, right, bottom, top, znear, zfar: single): TMat4;
    // 平截头体
    class function Frustum(left, right, bottom, top, znear, zfar: single): TMat4;
    // 视点转换
    class function LookAt(const eyes, center, up: TVec3): TMat4;
    // 创建一个2D正交投影矩阵。这种方法类似现在已弃用的gluOrtho2D方法。
    class function Ortho2d(left, right, bottom, top: single): TMat4;

    //═════════════════════════════════════════════════════════════════════════

    class function Vec2ToString(VecName: string; v: TVec2): string;
    class function Vec3ToString(VecName: string; v: TVec3): string;
    class function Vec4ToString(VecName: string; v: TVec4): string;
    class function Mat3ToString(matName: string; m: TMat3): string;
    class function Mat4ToString(matName: string; m: TMat4): string;
  end;

implementation

{ TGLM }

class function TGLM.Cross(a, b: TVec3): TVec3;
begin
  Result := a >< b;
end;

class function TGLM.Degrees(Rad: single): single;
begin
  Result := RadToDeg(Rad);
end;

class function TGLM.Dot(a, b: TVec3): single;
begin
  Result := a ** b;
end;

class function TGLM.Frustum(left, right, bottom, top, znear, zfar: single): TMat4;
begin
  Result.Init_Zero;
  Result.Data[0, 0] := 2 * znear / (right - left);
  Result.Data[1, 1] := 2 * znear / (top - bottom);
  Result.Data[2, 0] := (right + left) / (right - left);
  Result.Data[2, 1] := (top + bottom) / (top - bottom);
  Result.Data[2, 2] := -(zfar + znear) / (zfar - znear);
  Result.Data[2, 3] := -1.0;
  Result.Data[3, 2] := -2 * zfar * znear / (zfar - znear);
  Result.Data[3, 3] := 0.0;
end;

class function TGLM.LookAt(const eyes, center, up: TVec3): TMat4;
  function __LookAt1__(eye, center, up: TVec3): TMat4;
  var
    f, s, u: TVec3;
  begin
    f := Normalize(center - eye);
    s := Normalize(Cross(f, up));
    u := Cross(s, f);

    Result.Create(
      s.Data[0],  s.Data[1],  s.Data[2], -Dot(s, eyes),
      u.Data[0],  u.Data[1],  u.Data[2], -Dot(u, eyes),
      -f.Data[0], -f.Data[1], -f.Data[2], Dot(f, eyes),
      0,          0,          0,          1          );
  end;

  //function __LookAt2__(eyes, center, up: TVec3): TMat4;
  //var
  //  translation, rotation: TMat4;
  //  position, target, worldUp, zaxis, xaxis, yaxis: TVec3;
  //begin
  //  position := eyes;
  //  target := center;
  //  worldUp := up;
  //
  //  zaxis := Normalize(position - target);
  //  xaxis := Normalize(Cross(Normalize(worldUp), zaxis));
  //  yaxis := Cross(zaxis, xaxis);
  //
  //  translation := Mat4_Identity; // Identity matrix by default
  //  translation.Data[3][0] := -position.x; // Third column, first row
  //  translation.Data[3][1] := -position.y;
  //  translation.Data[3][2] := -position.z;
  //
  //  rotation := Mat4_Identity;
  //  rotation.Data[0][0] := xaxis.x; // First column, first row
  //  rotation.Data[1][0] := xaxis.y;
  //  rotation.Data[2][0] := xaxis.z;
  //  rotation.Data[0][1] := yaxis.x; // First column, second row
  //  rotation.Data[1][1] := yaxis.y;
  //  rotation.Data[2][1] := yaxis.z;
  //  rotation.Data[0][2] := zaxis.x; // First column, third row
  //  rotation.Data[1][2] := zaxis.y;
  //  rotation.Data[2][2] := zaxis.z;
  //
  //  // Return lookAt matrix as combination of translation and rotation matrix
  //  Result := (rotation * translation).Transpose;
  //end;
  
  //function __LookAt3__(eye, center, up: TVec3): TMat4;
  //var
  //  f, s, u: TVec3;
  //begin
  //  f := Normalize(center - eye);
  //  s := Normalize(Cross(f, up));
  //  u := Cross(s, f);
  //
  //
  //  Result := Mat4_Identity;
  //  Result.Data[0, 0] := s.x;
  //  Result.Data[1, 0] := s.y;
  //  Result.Data[2, 0] := s.z;
  //  Result.Data[0, 1] := u.x;
  //  Result.Data[1, 1] := u.y;
  //  Result.Data[2, 1] := u.z;
  //  Result.Data[0, 2] := -f.x;
  //  Result.Data[1, 2] := -f.y;
  //  Result.Data[2, 2] := -f.z;
  //  Result.Data[3, 0] := -Dot(s, eye);
  //  Result.Data[3, 1] := -Dot(u, eye);
  //  Result.Data[3, 2] := Dot(f, eye);
  //
  //  Result := Result.Transpose;
  //end;

begin
  Result := __LookAt1__(eyes, center, up);
end;

class function TGLM.Mat3(x: single): TMat3;
begin
  Result.Create(
    x, 0, 0,
    0, x, 0,
    0, 0, x);
end;

class function TGLM.Mat3ToString(matName: string; m: TMat3): string;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.AppendLine(matName + ': -------> ');

    for i := 0 to High(m.Data) do
    begin
      sb.Append('[');
      for j := 0 to High(m.Data[i]) do
      begin
        sb.Append('%16s', [m.Data[i, j].ToString]);

        if j <> High(m.Data[i]) then
          sb.Append(', ');
      end;

      if i <> High(m.Data) then
        sb.AppendLine('], ')
      else
        sb.AppendLine(']');
    end;

    sb.AppendLine;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class function TGLM.Mat3_Identity: TMat3;
begin
  Result.Init_Identity;
end;

class function TGLM.Mat3_Init(x00, x01, x02, x10, x11, x12, x20, x21, x22: single): TMat3;
begin
  Result.Create(x00, x01, x02, x10, x11, x12, x20, x21, x22);
end;

class function TGLM.Mat3_Zero: TMat3;
begin
  Result.Init_Zero;
end;

class function TGLM.Mat4(x: single): TMat4;
begin
  Result.Create(
    x, 0, 0, 0,
    0, x, 0, 0,
    0, 0, x, 0,
    0, 0, 0, x);
end;

class function TGLM.Mat4ToString(matName: string; m: TMat4): string;
var
  sb: TStringBuilder;
  i, j: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.AppendLine(matName + ': -------> ');

    for i := 0 to High(m.Data) do
    begin
      sb.Append('[');
      for j := 0 to High(m.Data[i]) do
      begin
        sb.Append('%16s', [m.Data[i, j].ToString]);

        if j <> High(m.Data[i]) then
          sb.Append(', ');
      end;

      if i <> High(m.Data) then
        sb.AppendLine('], ')
      else
        sb.AppendLine(']');
    end;

    sb.AppendLine;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class function TGLM.Mat4_Identity: TMat4;
begin
  Result.Init_Identity;
end;

class function TGLM.Mat4_Init(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
  x30, x31, x32, x33: single): TMat4;
begin
  Result.Create(x00, x01, x02, x03, x10, x11, x12, x13, x20, x21, x22, x23,
    x30, x31, x32, x33);
end;

class function TGLM.Mat4_Zero: TMat4;
begin
  Result.Init_Zero;
end;

class function TGLM.Normalize(vec: TVec3): TVec3;
var
  len, oneOverLen: single;
  res: TVec3;
begin
  len := vec.length;

  if len <= 0 then
    res.Create(1, 0, 0);

  oneOverLen := 1 / len;
  res.Create(vec.Data[0] * oneOverLen, vec.Data[1] * oneOverLen, vec.Data[2] * oneOverLen);

  Result := res;
end;

class function TGLM.Ortho(left, right, bottom, top, znear, zfar: single): TMat4;
var
  m00, m11, m22, m30, m31, m32: single;
begin
  m00 := single(2 / (right - left));
  m11 := single(2 / (top - bottom));
  m22 := single(-2 / (zFar - zNear));
  m30 := single(-(right + left) / (right - left));
  m31 := single(-(top + bottom) / (top - bottom));
  m32 := single(-(zFar + zNear) / (zFar - zNear));

  Result := Mat4_Init(
    m00,  0,    0,    m30,
    0,    m11,  0,    m31,
    0,    0,    m22,  m32,
    0,    0,    0,    1);
end;

class function TGLM.Ortho2d(left, right, bottom, top: single): TMat4;
var
  m00, m11, m22, m30, m31: single;
begin
  m00 := single(2 / (right - left));
  m11 := single(2 / (top - bottom));
  m22 := single(-1);
  m30 := single(-(right + left) / (right - left));
  m31 := single(-(top + bottom) / (top - bottom));

  Result.Create(
    m00,  0,    0,    m30,
    0,    m11,  0,    m31,
    0,    0,    m22,  0,
    0,    0,    0,    1);
end;

class function TGLM.Perspective(fovy, aspect, znear, zfar: single): TMat4;
var
  right, top: single;
begin
  top := znear * Tan(fovy / 2);
  right := top * aspect;
  Result := Frustum(-right, right, -top, top, znear, zfar).transpose;
end;

class function TGLM.Radians(deg: single): single;
begin
  Result := DegToRad(deg);
end;

class function TGLM.Rotate(m: TMat4; deg: single; vec: TVec3): TMat4;
var
  c, s, x, y, z: single;
  res: Tmat4;
begin
  //Mat4_Init rotationMatrix(vec3 axis, float angle)
  //{
  //    axis = normalize(axis);
  //    float s = sin(angle);
  //    float c = cos(angle);
  //    float oc = 1.0 - c;
  //
  //    return mat4(oc * axis.x * axis.x + c,           oc * axis.x * axis.y - axis.z * s,  oc * axis.z * axis.x + axis.y * s,  0.0,
  //                oc * axis.x * axis.y + axis.z * s,  oc * axis.y * axis.y + c,           oc * axis.y * axis.z - axis.x * s,  0.0,
  //                oc * axis.z * axis.x - axis.y * s,  oc * axis.y * axis.z + axis.x * s,  oc * axis.z * axis.z + c,           0.0,
  //                0.0,                                0.0,                                0.0,                                1.0);

  res.Init_Identity;
  c := Cos(deg);
  s := Sin(deg);

  vec := Normalize(vec);
  x := vec.Data[0];
  y := vec.Data[1];
  z := vec.Data[2];

  res.Data[0, 0] := x * x * (1 - c) + c;
  res.Data[1, 0] := x * y * (1 - c) + z * s;
  res.Data[2, 0] := x * z * (1 - c) - y * s;

  res.Data[0, 1] := x * y * (1 - c) - z * s;
  res.Data[1, 1] := y * y * (1 - c) + c;
  res.Data[2, 1] := y * z * (1 - c) + x * s;

  res.Data[0, 2] := z * x * (1 - c) + y * s;
  res.Data[1, 2] := y * z * (1 - c) - x * s;
  res.Data[2, 2] := z * z * (1 - c) + c;

  Result := m * res;
end;

class function TGLM.Scale(m: TMat4; vec: TVec3): TMat4;
var
  res: TMat4;
begin
  res.init_identity;
  res.Data[0, 0] := vec.Data[0];
  res.Data[1, 1] := vec.Data[1];
  res.Data[2, 2] := vec.Data[2];

  Result := m * res;
end;

class function TGLM.Translate(m: TMat4; vec: TVec3): TMat4;
var
  res: TMat4;
begin
  res.Init_Identity;

  res.Data[0, 3] += vec.Data[0];
  res.Data[1, 3] += vec.Data[1];
  res.Data[2, 3] += vec.Data[2];

  Result := m * res;
end;

class function TGLM.Vec4ToString(VecName: string; v: TVec4): string;
var
  sb: TStringBuilder;
  i: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.AppendLine(vecName + ': -------> ');

    sb.Append('[');
    for i := 0 to High(v.Data) do
    begin
      sb.Append('%16s', [v.Data[i].ToString]);

      if i <> High(v.Data) then
        sb.Append(', ');
    end;
    sb.AppendLine(']');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class function TGLM.Vec2(x, y: single): TVec2;
begin
  Result.Create(x, y);
end;

class function TGLM.Vec2ToString(VecName: string; v: TVec2): string;
var
  sb: TStringBuilder;
  i: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.AppendLine(vecName + ': -------> ');

    sb.Append('[');
    for i := 0 to High(v.Data) do
    begin
      sb.Append('%16s', [v.Data[i].ToString]);

      if i <> High(v.Data) then
        sb.Append(', ');
    end;
    sb.AppendLine(']');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class function TGLM.Vec3(x, y, z: single): TVec3;
begin
  Result.Create(x, y, z);
end;

class function TGLM.Vec3ToString(VecName: string; v: TVec3): string;
var
  sb: TStringBuilder;
  i: integer;
begin
  sb := TStringBuilder.Create();
  try
    sb.AppendLine(vecName + ': -------> ');

    sb.Append('[');
    for i := 0 to High(v.Data) do
    begin
      sb.Append('%16s', [v.Data[i].ToString]);

      if i <> High(v.Data) then
        sb.Append(', ');
    end;
    sb.AppendLine(']');

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

class function TGLM.Vec4(x, y, z, w: single): TVec4;
begin
  Result.Create(x, y, z, w);
end;

end.
