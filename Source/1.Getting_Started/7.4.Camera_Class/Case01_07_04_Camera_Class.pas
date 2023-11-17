unit Case01_07_04_Camera_Class;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.GLM;

type
  TCamera_Movement = (FORWARD, BACKWARD, LEFT, RIGHT);

  TCamera = class(TObject)
  private
    _yaw: GLfloat;
    _pitch: GLfloat;
    _speed: GLfloat;
    _sensitivity: GLfloat;
    _zoom: GLfloat;

  private
    procedure updateCameraVectors;

  public


    constructor Create;
    destructor Destroy; override;

    property Yaw: GLfloat read _yaw write _yaw;
    property Pitch: GLfloat read _pitch write _pitch;
    property Speed: GLfloat read _speed write _speed;
    property Sensitivity: GLfloat read _sensitivity write _sensitivity;
    property Zoom: GLfloat read _zoom write _zoom;
  end;


implementation

{ TCamera }

constructor TCamera.Create;
begin
  _yaw := -90.0;
  _pitch := 0.0;
  _speed := 2.5;
  _sensitivity := 0.1;
  _zoom := 45.0;
end;

destructor TCamera.Destroy;
begin
  inherited Destroy;
end;

procedure TCamera.updateCameraVectors;
begin

end;

end.
