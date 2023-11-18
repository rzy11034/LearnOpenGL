unit LearnOpenGL.Camera;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM;

type
  TCamera_Movement = (FORWARD, BACKWARD, LEFT, RIGHT);

  TCamera = class(TObject)
  private const
    d_vec3: TVec3 = (x: 0; y: 0; z: 0);

  private
    _yaw: GLfloat;
    _pitch: GLfloat;
    _movementSpeed: GLfloat;
    _mouseSensitivity: GLfloat;
    _zoom: GLfloat;

    _position: TVec3;
    _front: TVec3;
    _up: TVec3;
    _right: TVec3;
    _worldUp: TVec3;

    procedure __updateCameraVectors;

  public
    constructor Create;
    constructor Create(aposition: TVec3);
    constructor Create(aposition, aup: TVec3);
    constructor Create(posX, posY, posZ, upX, upY, upZ, aYaw, aPitch: GLfloat);
    destructor Destroy; override;

    function GetViewMatrix: TMat4;
    procedure ProcessKeyboard(direction: TCamera_Movement; deltaTime: GLfloat);
    procedure ProcessMouseMovement(xoffset, yoffset: GLfloat;
      constrainPitch: GLboolean = GLboolean(true));
    procedure ProcessMouseScroll(yoffset: GLfloat);
  end;


implementation

{ TCamera }

constructor TCamera.Create(posX, posY, posZ, upX, upY, upZ, aYaw, aPitch: GLfloat);
begin
  _yaw := aYaw;
  _pitch := aPitch;
  _movementSpeed := 2.5;
  _mouseSensitivity := 0.1;
  _zoom := 45;

  _position := TGLM.Vec3(posX, posY, posZ);
  _worldUp := TGLM.Vec3(upX, upY, upZ);
  _front := TGLM.Vec3(0, 0, -1);

  __updateCameraVectors;
end;

constructor TCamera.Create(aposition, aup: TVec3);
begin
  _yaw := -90.0;
  _pitch := 0.0;
  _movementSpeed := 2.5;
  _mouseSensitivity := 0.1;
  _zoom := 45;

  _position := aposition;
  _worldUp := aup;
  _front := TGLM.Vec3(0, 0, -1);

  __updateCameraVectors;
end;

constructor TCamera.Create(aposition: TVec3);
begin
  Create(aposition, TGLM.Vec3(0, 1, 0));
end;

constructor TCamera.Create;
begin
  Create(TGLM.Vec3(0, 0, 0), TGLM.Vec3(0, 1, 0));
end;

destructor TCamera.Destroy;
begin
  inherited Destroy;
end;

function TCamera.GetViewMatrix: TMat4;
begin
  Result := TGLM.LookAt(_position, _position + _front, _up);
end;

procedure TCamera.ProcessKeyboard(direction: TCamera_Movement; deltaTime: GLfloat);
var
  velocity: GLfloat;
begin
  velocity := _movementSpeed * deltaTime;

  case direction of
    FORWARD: _position += _front * velocity;
    BACKWARD: _position -= _front * velocity;
    LEFT: _position -= _right * velocity;
    RIGHT: _position += _right * velocity;
  end;
end;

procedure TCamera.ProcessMouseMovement(xoffset, yoffset: GLfloat; constrainPitch: GLboolean);
begin
  xoffset *= _mouseSensitivity;
  yoffset *= _mouseSensitivity;

  _yaw += xoffset;
  _pitch += yoffset;

  // make sure that when _pitch is out of bounds, screen doesn't get flipped
  if constrainPitch.ToBoolean then
  begin
    if _pitch > 89 then _pitch := 89;
    if _pitch < -89 then _pitch := -89;
  end;

  // update Front, Right and Up Vectors using the updated Euler angles
  __updateCameraVectors;
end;

procedure TCamera.ProcessMouseScroll(yoffset: GLfloat);
begin
  _zoom -= yoffset;
  if _zoom < 1 then _zoom := 1;
  if _zoom > 45 then _zoom := 45;
end;

procedure TCamera.__updateCameraVectors;
var
  f: TVec3;
begin
  f := TGLM.Vec3(0,0,0);
  f.x := Cos(TGLM.Radians(_yaw)) * Cos(TGLM.Radians(_pitch));
  f.y := Sin(TGLM.Radians(_pitch));
  f.z := Sin(TGLM.Radians(_yaw)) * Cos(TGLM.Radians(_pitch));
  f := TGLM.Normalize(f);

  // also re-calculate the Right and Up vector
  // normalize the vectors, because their length gets closer to 0 the more you
  // look up or down which results in slower movement.
  _right := TGLM.Normalize(TGLM.Cross(_front, _worldUp));
  _up := TGLM.Normalize(TGLM.Cross(_right, _front));
end;

end.
