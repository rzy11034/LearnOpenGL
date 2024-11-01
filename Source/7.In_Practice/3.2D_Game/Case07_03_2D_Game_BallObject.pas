unit Case07_03_2D_Game_BallObject;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}
{$ModeSwitch implicitfunctionspecialization}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}
{$ModeSwitch duplicatelocals}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM,
  Case07_03_2D_Game_ResourceManager,
  Case07_03_2D_Game_GameObject,
  Case07_03_2D_Game_Texture2D;

type
  TBallObject = class(TGameObject)
  public
    // Ball state
    Radius: GLfloat;
    Stuck: Boolean;

    // Constructor(s)
    constructor Create;
    constructor Create(pos: TVec2; radius: GLfloat; velocity: TVec2; sprite: TTexture2D);
    destructor Destroy; override;

    // 移动球，将其限制在窗口范围内（除了底部边缘）；返回新位置
    function Move(dt: GLfloat; window_width: GLuint): TVec2;

    // 以给定的位置和速度将球重置为原始状态
    procedure Reset(position, velocity: TVec2);

  end;

implementation

{ TBallObject }

constructor TBallObject.Create(pos: TVec2; radius: GLfloat; velocity:
  TVec2; sprite: TTexture2D);
var
  color: TVec3;
begin
  color := TGLM.Vec3(1.0);

  inherited Create
  (
    pos,
    TGLM.Vec2(radius * 2, radius * 2),
    sprite,
    @color,
    @Velocity
  );

  Self.Radius := 12.5;
  Self.Stuck := true;
end;

constructor TBallObject.Create;
begin
  inherited Create;

  Self.Radius := 12.5;
  Self.Stuck := true;
end;

destructor TBallObject.Destroy;
begin
  inherited Destroy;
end;

function TBallObject.Move(dt: GLfloat; window_width: GLuint): TVec2;
begin
  // 如果不粘在玩家板上
  if not Self.Stuck then
  begin
    // 移动球
    Self.Position += Self.Velocity * dt;
    // 然后检查是否在窗口边界之外，如果是，反转速度并恢复到正确位置
    if Self.Position.x <= 0.0 then
    begin
      Self.Velocity.x := -Self.Velocity.x;
      Self.Position.x := 0.0;
    end
    else if Self.Position.x + Self.Size.x >= window_width then
    begin
      Self.Velocity.x := -Self.Velocity.x;
      Self.Position.x := window_width - Self.Size.x;
    end;

    if Self.Position.y <= 0.0 then
    begin
      Self.Velocity.y := -Self.Velocity.y;
      Self.Position.y := 0.0;
    end;
  end;

  Result := Self.Position;
end;

procedure TBallObject.Reset(position, velocity: TVec2);
begin
  Self.Position := position;
  Self.Velocity := velocity;
  Self.Stuck := true;
end;

end.

