unit Case07_03_2D_Game_Game;

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
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.Utils,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM,
  Case07_03_2D_Game_Consts,
  Case07_03_2D_Game_SpriteRenderer,
  Case07_03_2D_Game_ResourceManager,
  Case07_03_2D_Game_GameObject,
  Case07_03_2D_Game_GameLevel,
  Case07_03_2D_Game_Texture2D,
  Case07_03_2D_Game_BallObject;

type
  TGameState = (GAME_ACTIVE, GAME_MENU, GAME_WIN);
  TDirection = (UP, RIGHT, DOWN, LEFT);

  TGame = class(TInterfacedObject)
  public type
    TArrayList_TGameLevel = specialize TArrayList<TGameLevel>;

  private const
    // 初始化挡板的大小
    PLAYER_SIZE: TVec2 = (x: 100; y: 20);
    // 初始化当班的速率
    PLAYER_VELOCITY: GLfloat = (5000.0);
    // 初始化球的速度
    INITIAL_BALL_VELOCITY: TVec2 = (x: 2000.0; y: -2000.0);
    // 球的半径
    BALL_RADIUS: GLfloat = 12.5;

  public
    Height: GLuint;
    Width: GLuint;
    Keys: array[0..1023] of Boolean;
    State: TGameState;
    Renderer: TSpriteRenderer;
    Player: TGameObject;
    Ball: TBallObject;

    Levels: TArrayList_TGameLevel;
    Level: GLuint;

    constructor Create(width, height: GLuint);
    destructor Destroy; override;

    // Initialize game state (load all shaders/textures/levels)
    procedure Init;

    // GameLoop
    procedure ProcessInput(dt: GLfloat);
    procedure Update(dt: GLfloat);
    procedure Render;

    procedure DoCollisions;
    function CheckCollision(one, two: TGameObject): Boolean;
    function CheckCollision(one: TBallObject; two: TGameObject): Boolean;
  end;

implementation

{ TGame }

constructor TGame.Create(width, height: GLuint);
begin
  Self.Width := width;
  Self.Height := height;

  State := TGameState.GAME_ACTIVE;

  Levels := TArrayList_TGameLevel.Create;
end;

function TGame.CheckCollision(one, two: TGameObject): Boolean;
var
  collisionX, collisionY: Boolean;
begin
  collisionX := false;
  collisionY := false;

  // x轴方向碰撞？
  collisionX := (one.Position.x + one.Size.x >= two.Position.x)
    and (two.Position.x + two.Size.x >= one.Position.x);

  // y轴方向碰撞？
  collisionY := (one.Position.y + one.Size.y >= two.Position.y)
    and (two.Position.y + two.Size.y >= one.Position.y);

  // 只有两个轴向都有碰撞时才碰撞
  Result := collisionX and collisionY;
end;

function TGame.CheckCollision(one: TBallObject; two: TGameObject): Boolean;
var
  center, aabb_half_extents, aabb_center, difference, clamped, closest: TVec2;
begin
  // 获取圆的中心
  center := one.Position + one.Radius;

  // 计算AABB的信息（中心、半边长）
  aabb_half_extents := TGLM.Vec2(two.Size.x / 2, two.Size.y / 2);
  aabb_center := TGLM.Vec2(
    two.Position.x + aabb_half_extents.x,
    two.Position.y + aabb_half_extents.y);

  // 获取两个中心的差矢量
  difference := center - aabb_center;
  clamped := TGLM.Clamp(difference, -aabb_half_extents, aabb_half_extents);

  // AABB_center加上clamped这样就得到了碰撞箱上距离圆最近的点closest
  closest := aabb_center + clamped;

  // 获得圆心center和最近点closest的矢量并判断是否 length <= radius
  difference := closest - center;

  Result := TGLM.Length(difference) < one.Radius;
end;

destructor TGame.Destroy;
var
  i: Integer;
begin
  if Renderer <> nil then
    FreeAndNil(Renderer);

  if Player <> nil then
    FreeAndNil(Player);

  if Ball <> nil then
    FreeAndNil(Ball);

  if not Levels.IsEmpty then
  begin
    for i := 0 to Levels.Count - 1 do
    begin
      Levels[i].Free;
    end;

    FreeAndNil(Levels);
  end;

  inherited Destroy;
end;

procedure TGame.DoCollisions;
var
  i: Integer;
  box: TGameObject;
begin
  for i := 0 to Levels[Self.Level].Bricks.Count - 1 do
  begin
    box := Levels[Self.Level].Bricks[i];

    if not box.Destroyed then
    begin
      if CheckCollision(Ball, box) then
      begin
        if not box.IsSolid then
        begin
          box.Destroyed := true;
        end;
      end;
    end;
  end;
end;

procedure TGame.Init;
var
  projection: TMat4;
  one, tow, three, four: TGameLevel;
  playerPos, ballPos: TVec2;
  tx: TTexture2D;
begin
  TResourceManager.LoadShader(SPRITE_NAME, SPRITE_VS, SPRITE_FS, '');

  projection := TGLM.Ortho(0.0, Width, Height, 0.0, -1.0, 1.0);
  TResourceManager.GetShader(SPRITE_NAME).Use.SetInteger('image', 0);
  TResourceManager.GetShader(SPRITE_NAME).SetMatrix4('projection', projection);

  // 设置专用于渲染的控制
  Renderer := TSpriteRenderer.Create(TResourceManager.GetShader(SPRITE_NAME));

  // 加载纹理
  TResourceManager.LoadTexture(IMG_BACKGROUND_NAME, IMG_BACKGROUND, true);
  TResourceManager.LoadTexture(IMG_AWESOMEFACE_NAME, IMG_AWESOMEFACE, true);
  TResourceManager.LoadTexture(IMG_BLOCK_NAME, IMG_BLOCK, true);
  TResourceManager.LoadTexture(IMG_BLOCK_SOLID_NAME, IMG_BLOCK_SOLID, true);
  TResourceManager.LoadTexture(IMG_PADDLE_NAME, IMG_PADDLE, true);

  one := TGameLevel.Create;
  tow := TGameLevel.Create;
  three := TGameLevel.Create;
  four := TGameLevel.Create;

  one.Load(LEVEL_1, Width, Round(Height * 0.5));
  tow.Load(LEVEL_2, Width, Round(Height * 0.5));
  three.Load(LEVEL_3, Width, Round(Height * 0.5));
  four.Load(LEVEL_4, Width, Round(Height * 0.5));

  Self.Levels.AddLast(one);
  Self.Levels.AddLast(tow);
  Self.Levels.AddLast(three);
  Self.Levels.AddLast(four);

  Self.Level := 0;

  playerPos := TGLM.Vec2(Width / 2 - PLAYER_SIZE.x / 2, Height - PLAYER_SIZE.y);
  tx := TResourceManager.GetTexture(IMG_PADDLE_NAME);
  Player := TGameObject.Create(playerPos, PLAYER_SIZE, tx);

  ballPos := playerPos + TGLM.Vec2(PLAYER_SIZE.x / 2 - BALL_RADIUS, -BALL_RADIUS * 2);
  Ball := TBallObject.Create(ballPos, BALL_RADIUS, INITIAL_BALL_VELOCITY,
    TResourceManager.GetTexture(IMG_AWESOMEFACE_NAME));
end;

procedure TGame.ProcessInput(dt: GLfloat);
var
  velocity: GLfloat;
begin
  if Self.State = GAME_ACTIVE then
  begin
    velocity := GLfloat(PLAYER_VELOCITY * dt);

    // 移动挡板
    if Self.Keys[GLFW_KEY_A] then
    begin
      if Player.Position.x >= 0 then
      begin
        Player.Position.x -= velocity;

        if Ball.Stuck then
        begin
          Ball.Position.x -= velocity;
        end;
      end;
    end;

    if Self.Keys[GLFW_KEY_D] then
    begin
      if Player.Position.x <= Self.Width - Player.Size.x then
      begin
        Player.Position.x += velocity;

        if Ball.Stuck then
        begin
          Ball.Position.x += velocity;
        end;
      end;
    end;

    if Self.Keys[GLFW_KEY_SPACE] then
      Ball.Stuck := false;
  end;
end;

procedure TGame.Render;
var
  position, size: TVec2;
  rotate: Single;
  texture: TTexture2D;
begin
  if Self.State = GAME_ACTIVE then
  begin
    texture := TResourceManager.GetTexture(IMG_BACKGROUND_NAME);
    position := TGLM.Vec2(0, 0);
    size := TGLM.Vec2(Self.Width, Self.Height);
    rotate := 0.0;
    Renderer.DrawSprite(texture, @position, @size, @rotate);

    Self.Levels[Self.Level].Draw(Renderer);

    Player.Draw(Renderer);
    Ball.Draw(Renderer);
  end;
end;

procedure TGame.Update(dt: GLfloat);
begin
  Ball.Move(dt, Self.Width);

  // 检测碰撞
  Self.DoCollisions;
end;

end.

