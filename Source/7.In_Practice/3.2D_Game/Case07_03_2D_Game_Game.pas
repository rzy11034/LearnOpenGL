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
  Case07_03_2D_Game_BallObject,
  Case07_03_2D_Game_ParticleGenerator,
  Case07_03_2D_Game_PostProcessor,
  Case07_03_2D_Game_Powerups,
  Case07_03_2D_Game_Sound,
  Case07_03_2D_Game_TextRenderer;

type
  TGameState = (GAME_ACTIVE, GAME_MENU, GAME_WIN);
  TDirection = (UP, RIGHT, DOWN, LEFT);

  TCollision = Record
    bool: Boolean;
    dir:  TDirection;
    vec:  TVec2;
    constructor Create(bool: Boolean; dir: TDirection; vec: TVec2);
  end;

  TGame = class(TInterfacedObject)
  public type
    TList_TGameLevel = specialize TArrayList<TGameLevel>;
    TList_TPowerup = specialize TArrayList<TPowerup>;

  private const
    // 初始化挡板的大小
    PLAYER_SIZE: TVec2 = (x: 100; y: 20);
    // 初始化当班的速率
    PLAYER_VELOCITY: float = (1000.0);
    // 初始化球的速度
    INITIAL_BALL_VELOCITY: TVec2 = (x: 200.0; y: -200.0);
    // 球的半径
    BALL_RADIUS: float = 12.5;

  private
    function __CheckCollision(one, two: TGameObject): boolean;
    function __CheckCollision(one: TBallObject; two: TGameObject): TCollision;
    function __IsOtherPowerUpActive(powerUps: TList_TPowerup; typ: string): boolean;
    function __ShouldSpawn(chance: GLuint): boolean;
    function __VectorDirection(target: TVec2): TDirection;
    procedure __ActivatePowerUp(powerUp: TPowerUp);

  public
    Lives: integer;
    Sound: TSound;
    Effects: TPostProcessor;
    Height: GLuint;
    Particles: TParticleGenerator;
    PowerUps: TList_TPowerup;
    ShakeTime: GLfloat;
    Text: TTextRenderer;
    Width: GLuint;
    Keys: array[0..1023] of Boolean;
    KeysProcessed: array[0..1023] of Boolean;
    State: TGameState;
    Renderer: TSpriteRenderer;
    Player: TGameObject;
    Ball: TBallObject;

    Levels: TList_TGameLevel;
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

    // Reset
    procedure ResetLevel;
    procedure ResetPlayer;

    procedure SpawnPowerUps(block: TGameObject);
    procedure UpdatePowerUps(dt: GLfloat);
  end;

implementation

{ TCollision }

constructor TCollision.Create(bool: Boolean; dir: TDirection; vec: TVec2);
begin
  Self.bool := bool;
  Self.dir := dir;
  Self.vec := vec;
end;

{ TGame }

constructor TGame.Create(width, height: GLuint);
var
  i: Integer;
begin
  Self.Width := width;
  Self.Height := height;
  Self.Lives := 3;

  for i := 0 to 1023 do
  begin
    Keys[i] := false;
    KeysProcessed[i] := false;
  end;

  Levels := TList_TGameLevel.Create;

  PowerUps := TList_TPowerup.Create;

  Text := TTextRenderer.Create(Self.Width, Self.Height);
  Text.Load(FONT_OCRAEXT, 24);
end;

destructor TGame.Destroy;
var
  i: Integer;
begin
  if Sound <> nil then
    FreeAndNil(Sound);

  if Renderer <> nil then
    FreeAndNil(Renderer);

  if Player <> nil then
    FreeAndNil(Player);

  if Ball <> nil then
    FreeAndNil(Ball);

  if PowerUps <> nil then
  begin
    if not PowerUps.IsEmpty then
    begin
      for i := 0 to PowerUps.Count - 1 do
      begin
        PowerUps[i].Free;
      end;
    end;

    FreeAndNil(PowerUps);
  end;

  if Levels <> nil then
  begin
    if not Levels.IsEmpty then
    begin
      for i := 0 to Levels.Count - 1 do
      begin
        Levels[i].Free;
      end;
    end;

    FreeAndNil(Levels);
  end;

  if Particles <> nil then
    FreeAndNil(Particles);

  if Effects <> nil then
    FreeAndNil(Effects);

  if Text <> nil then
    FreeAndNil(Text);

  inherited Destroy;
end;

procedure TGame.DoCollisions;
var
  i: Integer;
  box: TGameObject;
  collision: TCollision;
  dir: TDirection;
  diff_vector, oldVelocity: TVec2;
  penetration: float;
  centerBoard, distance, percentage, strength: Single;
  powerUp: TPowerup;
begin
  for i := 0 to Levels[Self.Level].Bricks.Count - 1 do
  begin
    box := Levels[Self.Level].Bricks[i];

    if not box.Destroyed then
    begin
      collision := __CheckCollision(Ball, box);

      if collision.bool then // 如果 collision 是 true
      begin
        // 如果砖块不是实心就销毁砖块
        if not box.IsSolid then
        begin
          box.Destroyed := true;
          SpawnPowerUps(box);
          Self.Sound.BleepMp3Play;
        end
        else
        begin
          // 如果块是固体，则启用震动效果
          ShakeTime := 0.05;
          Effects.Shake := true;
          Self.Sound.SolidPlay;
        end;

        // 碰撞处理
        dir := collision.dir;
        diff_vector :=  collision.vec;

        if (dir = LEFT) or (dir = RIGHT) then // 水平方向碰撞
        begin
          Ball.Velocity.x := -Ball.Velocity.x; // 反转水平速度

          // 重定位
          penetration := float(Ball.Radius - Abs(diff_vector.x));

          if dir = LEFT then
            Ball.Position.x += penetration // 将球右移
          else
            Ball.Position.x -= penetration; // 将球左移
        end
        else // 垂直方向碰撞
        begin
          Ball.Velocity.y := -Ball.Velocity.y; // 反转垂直速度
          // 重定位
          penetration := Ball.Radius - Abs(diff_vector.y);

          if dir = TDirection.UP then
            Ball.Position.y -= penetration // 将球上移
          else
            Ball.Position.y += penetration; // 将球下移
        end;
      end;
    end;
  end;

  // 还要检查powerup的碰撞，如果是，激活它们
  for i := 0 to PowerUps.Count - 1 do
  begin
    powerUp := Self.PowerUps[i];

    if not powerUp.Destroyed then
    begin
      // 首先检查升级是否通过底部边缘，如果是，保持为非活动状态并销毁
      if powerUp.Position.y >= Self.Height then
        powerUp.Destroyed := true;

      if __CheckCollision(Player, powerUp) then // 与玩家相撞，现在激活能量
      begin
        __ActivatePowerUp(powerUp);
        powerUp.Destroyed := true;
        powerUp.Activated := true;
        Self.Sound.PowerupPlay;
      end;
    end;
  end;

  collision := __CheckCollision(Ball, Player);
  if (not Ball.Stuck) and (collision.bool) then
  begin
    // 检查碰到了挡板的哪个位置，并根据碰到哪个位置来改变速度
    centerBoard := Player.Position.x + Player.Size.x / 2;
    distance := (Ball.Position.x + Ball.Radius) - centerBoard;
    percentage := distance / (Player.Size.x / 2);

    // 依据结果移动
    strength := single(2.0);
    oldVelocity := Ball.Velocity;
    Ball.Velocity.x := INITIAL_BALL_VELOCITY.x * percentage * strength;
    Ball.Velocity.y := -Ball.Velocity.y;
    Ball.Velocity := TGLM.Normalize(Ball.Velocity) * TGLM.Length(oldVelocity);

    // 如果粘性道具被激活，重新计算矢量速度
    Ball.Stuck := Ball.Sticky;

    Self.Sound.BleepWavPlay;
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
  TResourceManager.LoadShader(PARTICLE_NAME, PARTICLE_VS, PARTICLE_FS, '');
  TResourceManager.LoadShader(POST_PROCESSING_NAME, POST_PROCESSING_VS, POST_PROCESSING_FS, '');

  projection := TGLM.Ortho(0.0, Width, Height, 0.0, -1.0, 1.0);
  TResourceManager.GetShader(SPRITE_NAME).Use.SetInteger('image', 0);
  TResourceManager.GetShader(SPRITE_NAME).SetMatrix4('projection', projection);

  TResourceManager.GetShader(PARTICLE_NAME).Use.SetInteger('sprite', 0);
  TResourceManager.GetShader(PARTICLE_NAME).SetMatrix4('projection', projection);

  //═════════════════════════════════════════════════════════════════════════

  // 加载纹理
  TResourceManager.LoadTexture(IMG_BACKGROUND_NAME, IMG_BACKGROUND, true);
  TResourceManager.LoadTexture(IMG_AWESOMEFACE_NAME, IMG_AWESOMEFACE, true);
  TResourceManager.LoadTexture(IMG_BLOCK_NAME, IMG_BLOCK, true);
  TResourceManager.LoadTexture(IMG_BLOCK_SOLID_NAME, IMG_BLOCK_SOLID, true);
  TResourceManager.LoadTexture(IMG_PADDLE_NAME, IMG_PADDLE, true);
  TResourceManager.LoadTexture(IMG_PARTICLE_NAME, IMG_PARTICLE, true);

  TResourceManager.LoadTexture(IMG_POWERUP_SPEED_NAME, IMG_POWERUP_SPEED, true);
  TResourceManager.LoadTexture(IMG_POWERUP_STICKY_NAME, IMG_POWERUP_STICKY, true);
  TResourceManager.LoadTexture(IMG_POWERUP_INCREASE_NAME, IMG_POWERUP_INCREASE, true);
  TResourceManager.LoadTexture(IMG_POWERUP_CONFUSE_NAME, IMG_POWERUP_CONFUSE, true);
  TResourceManager.LoadTexture(IMG_POWERUP_CHAOS_NAME, IMG_POWERUP_CHAOS, true);
  TResourceManager.LoadTexture(IMG_POWERUP_PASSTHROUGH_NAME, IMG_POWERUP_PASSTHROUGH, true);

  //═════════════════════════════════════════════════════════════════════════

  // 设置专用于渲染的控制
  Renderer := TSpriteRenderer.Create(TResourceManager.GetShader(SPRITE_NAME));
  Particles := TParticleGenerator.Create(TResourceManager.GetShader(PARTICLE_NAME),
    TResourceManager.GetTexture(IMG_PARTICLE_NAME), 500);
  Effects := TPostProcessor.Create(TResourceManager.GetShader(POST_PROCESSING_NAME),
    Self.Width, Self.Height);

  //═════════════════════════════════════════════════════════════════════════

  one   := TGameLevel.Create;
  tow   := TGameLevel.Create;
  three := TGameLevel.Create;
  four  := TGameLevel.Create;

  one.Load  (LEVEL_1, Width, Height div 2);
  tow.Load  (LEVEL_2, Width, Height div 2);
  three.Load(LEVEL_3, Width, Height div 2);
  four.Load (LEVEL_4, Width, Height div 2);

  Self.Levels.AddLast(one);
  Self.Levels.AddLast(tow);
  Self.Levels.AddLast(three);
  Self.Levels.AddLast(four);

  Self.Level := 0;

  //═════════════════════════════════════════════════════════════════════════

  playerPos := TGLM.Vec2(Width / 2 - PLAYER_SIZE.x / 2, Height - PLAYER_SIZE.y);
  tx := TResourceManager.GetTexture(IMG_PADDLE_NAME);
  Player := TGameObject.Create(playerPos, PLAYER_SIZE, tx);

  ballPos := playerPos + TGLM.Vec2(PLAYER_SIZE.x / 2 - BALL_RADIUS, -BALL_RADIUS * 2);
  Ball := TBallObject.Create(ballPos, BALL_RADIUS, INITIAL_BALL_VELOCITY,
    TResourceManager.GetTexture(IMG_AWESOMEFACE_NAME));

  Sound := TSound.Create;
  Sound.Init;
  Sound.BreakoutPlay;
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

  if Self.State = GAME_MENU then
  begin
    if Self.Keys[GLFW_KEY_ENTER] and not(Self.KeysProcessed[GLFW_KEY_ENTER]) then
    begin
      Self.State := GAME_ACTIVE;
      KeysProcessed[GLFW_KEY_ENTER] := true;
    end;

    if Self.Keys[GLFW_KEY_W] and not(Self.KeysProcessed[GLFW_KEY_W]) then
    begin
      Self.Level := (Self.Level + 1) mod 4;
      Self.KeysProcessed[GLFW_KEY_W] := true;
    end;

    if Self.Keys[GLFW_KEY_S] and not(Self.KeysProcessed[GLFW_KEY_S]) then
    begin
      if Self.Level > 0 then
        Self.Level -= 1
      else
        Self.Level := 3;

      Self.KeysProcessed[GLFW_KEY_S] := true;
    end;
  end;

  if Self.State = GAME_WIN then
  begin
    if Self.Keys[GLFW_KEY_ENTER] then
    begin
      Self.KeysProcessed[GLFW_KEY_ENTER] := true;
      Effects.Chaos := false;
      Self.State := GAME_MENU;
    end;
  end;
end;

procedure TGame.Render;
var
  position, size: TVec2;
  rotate: Single;
  texture: TTexture2D;
  i: Integer;
  color: TVec3;
begin
  // 开始渲染到后处理四边形
  Effects .BeginRender;
    texture := TResourceManager.GetTexture(IMG_BACKGROUND_NAME);
    position := TGLM.Vec2(0, 0);
    size := TGLM.Vec2(Self.Width, Self.Height);
    rotate := 0.0;
    Renderer.DrawSprite(texture, @position, @size, @rotate);

    Self.Levels[Self.Level].Draw(Renderer);

    Player.Draw(Renderer);

    for i := 0 to PowerUps.Count - 1 do
    begin
      if not PowerUps[i].Destroyed then
        PowerUps[i].Draw(Renderer);
    end;

    Particles.Draw;
    Ball.Draw(Renderer);
  // 结束渲染到后处理四边形
  Effects.EndRender;
  // 渲染后处理四边形
  Effects.Render(glfwGetTime);

  Text.RenderText('Lives:' + Lives.ToString, 5.0, 5.0, 1.0);
  Text.RenderText('Level:' + (Level + 1).ToString, Width - 120, 5.0, 1.0);

  if Self.State = GAME_MENU then
  begin
    Text.RenderText('Press ENTER to start', 250.0, Height / 2, 1.0);
    Text.RenderText('Press W or S to select level', 245.0, Height / 2 + 20.0, 0.75);
  end;

  if Self.State = GAME_WIN then
  begin
    color := TGLM.Vec3(0.0, 1.0, 0.0);
    Text.RenderText('You WON!!!', 320.0, Height / 2 - 20.0, 1.0, @color);

    color := TGLM.Vec3(1.0, 1.0, 0.0);
    Text.RenderText('Press ENTER to retry or ESC to quit', 130.0, Height / 2,
      1.0, @color);
  end;
end;

procedure TGame.ResetLevel;
begin
  case Self.Level of
    0: Self.Levels[0].Load(LEVEL_1, Self.Width, Self.Height div 2);
    1: Self.Levels[0].Load(LEVEL_2, Self.Width, Self.Height div 2);
    2: Self.Levels[0].Load(LEVEL_3, Self.Width, Self.Height div 2);
    3: Self.Levels[0].Load(LEVEL_4, Self.Width, Self.Height div 2);
  end;

  Self.Lives := 3;
end;

procedure TGame.ResetPlayer;
begin
  // Reset player/ball stats
  Player.Size := PLAYER_SIZE;
  Player.Position := TGLM.Vec2(Self.Width / 2 - PLAYER_SIZE.x / 2,
    Self.Height - PLAYER_SIZE.y);

  Ball.Reset(
    Player.Position + TGLM.Vec2(PLAYER_SIZE.x / 2 - BALL_RADIUS, -(BALL_RADIUS * 2)),
    INITIAL_BALL_VELOCITY
    );

  // 同时禁用所有激活的能量
  Effects.Confuse := false;
  Effects.Chaos := false;
  Ball.PassThrough := false;
  Ball.Sticky := false;
  Player.Color := TGLM.Vec3(1.0);
  Ball.Color := TGLM.Vec3(1.0);
end;

procedure TGame.SpawnPowerUps(block: TGameObject);
begin
  if __ShouldSpawn(75) then // 1/75 的机会
  begin
    Self.PowerUps.AddLast
    (
      TPowerup.Create
      (
        IMG_POWERUP_SPEED_NAME,
        TGlm.Vec3(0.5, 0.5, 1.0),
        0.0,
        block.Position,
        TResourceManager.GetTexture(IMG_POWERUP_SPEED_NAME)
      )
    );
  end;


  if __ShouldSpawn(75) then
    Self.PowerUps.AddLast
    (
      TPowerup.Create
      (
        IMG_POWERUP_STICKY_NAME,
        TGlm.Vec3(1.0, 0.5, 1.0),
        20.0,
        block.Position,
        TResourceManager.GetTexture(IMG_POWERUP_STICKY_NAME)
      )
    );

  if __ShouldSpawn(75) then
    Self.PowerUps.AddLast
    (
      TPowerup.Create
      (
        IMG_POWERUP_PASSTHROUGH_NAME,
        TGlm.Vec3(0.5, 1.0, 0.5),
        10.0,
        block.Position,
        TResourceManager.GetTexture(IMG_POWERUP_PASSTHROUGH_NAME)
      )
    );

  if __ShouldSpawn(75) then
    Self.PowerUps.AddLast
    (
      TPowerup.Create
      (
        IMG_POWERUP_INCREASE_NAME,
        TGlm.Vec3(1.0, 0.6, 0.4),
        0.0,
        block.Position,
        TResourceManager.GetTexture(IMG_POWERUP_INCREASE_NAME)
      )
    );

  if __ShouldSpawn(15) then // 消极道具应该更频繁地出现
    Self.PowerUps.AddLast
    (
      TPowerup.Create
      (
        IMG_POWERUP_CONFUSE_NAME,
        TGlm.Vec3(1.0, 0.3, 0.3),
        15.0, block.Position,
        TResourceManager.GetTexture(IMG_POWERUP_CONFUSE_NAME)
      )
    );

  if __ShouldSpawn(15) then
    Self.PowerUps.AddLast
    (
      TPowerup.Create
      (
        IMG_POWERUP_CHAOS_NAME,
        TGlm.Vec3(0.9, 0.25, 0.25),
        15.0,
        block.Position,
        TResourceManager.GetTexture(IMG_POWERUP_CHAOS_NAME)
      )
    );
end;

procedure TGame.Update(dt: GLfloat);
var
  offset: TVec2;
begin
  Ball.Move(dt, Self.Width);

  // 检测碰撞
  Self.DoCollisions;

  offset := TGLM.Vec2(Ball.Radius / 2);
  Particles.Update(dt, Ball, 2, @offset);

  UpdatePowerUps(dt);

  if ShakeTime > 0.0 then
  begin
    ShakeTime -= dt;

    if ShakeTime <= 0.0 then
      Effects.Shake := false;
  end;

  //(*═══════════════════════════════════════════════════════════════════════
  // 球是否接触底部边界？
  if Ball.Position.y >= Self.Height then
  begin
    Self.Lives -= 1;

    if Self.Lives = 0 then
    begin
      Self.ResetLevel;
      Self.State := GAME_MENU;
    end;

    Self.ResetPlayer;
  end;
  //═══════════════════════════════════════════════════════════════════════*)

  (*═══════════════════════════════════════════════════════════════════════
  // 测试用
  if Ball.Position.y >= Self.Height then
  begin
    Ball.Position.y *= -1;
  end;
  //═══════════════════════════════════════════════════════════════════════*)


  if (Self.State = GAME_ACTIVE) and Self.Levels[Self.Level].IsCompleted then
  begin
    Self.ResetLevel;
    Self.ResetPlayer;
    Effects.Chaos := true;
    Self.State := GAME_WIN;
  end;
end;

procedure TGame.UpdatePowerUps(dt: GLfloat);
var
  i: Integer;
  powerUp: TPowerup;
begin
  for i := 0 to Self.PowerUps.Count - 1 do
  begin
    powerUp := Self.PowerUps[i];

    powerUp.Position += powerUp.Velocity * dt;
    if powerUp.Activated then
    begin
      powerUp.Duration -= dt;

      if powerUp.Duration <= 0.0 then
      begin
        // 从列表中移除能量（稍后会移除）
        powerUp.Activated := false;
        // 关闭的影响
        if powerUp.Type_ = IMG_POWERUP_STICKY_NAME then
        begin
          if not __IsOtherPowerUpActive(PowerUps, IMG_POWERUP_STICKY_NAME) then
          begin	// 只有当没有其他类型的粘性激活时才复位
              Ball.Sticky := false;
              Player.Color := TGlm.vec3(1.0);
          end;
        end
        else if powerUp.Type_ = IMG_POWERUP_PASSTHROUGH_NAME then
        begin
          if not __IsOtherPowerUpActive(PowerUps, IMG_POWERUP_PASSTHROUGH_NAME) then
          begin	// 只有当没有其他类型的直通激活时才复位
            Ball.PassThrough := false;
            Ball.Color := TGlm.vec3(1.0);
          end;
        end
        else if powerUp.Type_ = IMG_POWERUP_CONFUSE_NAME then
        begin
          if not __IsOtherPowerUpActive(PowerUps, IMG_POWERUP_CONFUSE_NAME) then
          begin	// 只有在没有其他混乱类型的能量激活时才会重置
            Effects.Confuse := false;
          end;
        end
        else if powerUp.Type_ = IMG_POWERUP_CHAOS_NAME then
        begin
          if not __IsOtherPowerUpActive(PowerUps, IMG_POWERUP_CHAOS_NAME) then
          begin	// 只有当没有其他混乱类型的能量激活时才会重置
            Effects.Chaos := false;
          end;
        end;
      end;
    end;
  end;

  //从矢量中移除所有被破坏和激活的强化道具（因此要么离开地图要么完成）
  i := 0;
  while i < PowerUps.Count do
  begin
    powerUp := PowerUps[i];

    if powerUp.Destroyed and (not powerUp.Activated) then
    begin
      PowerUps.Remove(i);
      FreeAndNil(powerUp);
      Continue;
    end;

    i += 1;
  end;
end;

procedure TGame.__ActivatePowerUp(powerUp: TPowerUp);
begin
  case powerUp.Type_ of
    IMG_POWERUP_SPEED_NAME:
      Ball.Velocity *= 2;

    IMG_POWERUP_STICKY_NAME:
    begin
      Ball.Sticky := true;
      Player.Color := TGLM.Vec3(1.0, 0.5, 1.0);
    end;

    IMG_POWERUP_PASSTHROUGH_NAME:
    begin
      Ball.PassThrough := true;
      Ball.Color := TGLM.Vec3(1.0, 0.5, 0.5);
    end;

    IMG_POWERUP_INCREASE_NAME:
      Player.Size.x += 50;

    IMG_POWERUP_CONFUSE_NAME:
    begin
      if not Effects.Chaos then
        Effects.Confuse := true;
    end;

    IMG_POWERUP_CHAOS_NAME:
    begin
      if not Effects.Confuse then
        Effects.Chaos := true;
    end;
  end;
end;

function TGame.__CheckCollision(one, two: TGameObject): boolean;
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

function TGame.__CheckCollision(one: TBallObject; two: TGameObject): TCollision;
var
  center, aabb_half_extents, aabb_center, difference, clamped, closest: TVec2;
  res: TCollision;
begin
  // 获取圆的中心
  center := one.Position + one.Radius;

  // 计算AABB的信息（中心、半边长）
  aabb_half_extents := TGLM.Vec2(two.Size.x / 2, two.Size.y / 2);
  aabb_center := TGLM.Vec2(
    two.Position.x + aabb_half_extents.x,
    two.Position.y + aabb_half_extents.y
    );

  // 获取两个中心的差矢量
  difference := center - aabb_center;
  clamped := TGLM.Clamp(difference, -aabb_half_extents, aabb_half_extents);

  // AABB_center加上clamped这样就得到了碰撞箱上距离圆最近的点closest
  closest := aabb_center + clamped;

  // 获得圆心center和最近点closest的矢量并判断是否 length <= radius
  difference := closest - center;

  if TGLM.Length(difference) < one.Radius then
    res := TCollision.Create(true, __VectorDirection(difference), difference)
  else
    res := TCollision.Create(false, TDirection.UP, TGLM.Vec2(0));

  Result := res;
end;

function TGame.__IsOtherPowerUpActive(powerUps: TList_TPowerup; typ: string): boolean;
var
  i: Integer;
begin
  // 检查另一个相同类型的PowerUp是否仍然处于活动状态
  // 在这种情况下，我们不禁用它的效果
  for i := 0 to powerUps.Count - 1 do
  begin
    if (powerUps[i].Activated) and (powerUps[i].Type_ = typ) then
    begin
      Exit(true);
    end;
  end;

  Result := false;
end;

function TGame.__ShouldSpawn(chance: GLuint): boolean;
var
  temp: integer;
begin
  temp := Random(chance);
  Result := temp = 0;
end;

function TGame.__VectorDirection(target: TVec2): TDirection;
var
  compass: array[0..3] of TVec2;
  max: float;
  best_match, i: Integer;
  dot_product: Single;
begin
  compass := [
    TGLM.Vec2( 0.0,  1.0), // 上
    TGLM.Vec2( 1.0,  0.0), // 右
    TGLM.Vec2( 0.0, -1.0), // 下
    TGLM.Vec2(-1.0,  0.0)  // 左
    ];

  max := float(0.0);
  best_match := -1;

  for i := 0 to High(compass) do
  begin
    dot_product := TGLM.Dot(TGLM.Normalize(target), compass[i]);

    if dot_product > max then
    begin
      max := dot_product;
      best_match := i;
    end;
  end;

  Result := TDirection(best_match);
end;

end.

