unit Case07_03_2D_Game_Game;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}
{$ModeSwitch implicitfunctionspecialization}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}

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
  Case07_03_2D_Game_Texture2D;

type
  TGameState = (GAME_ACTIVE, GAME_MENU, GAME_WIN);

  TGame = class(TInterfacedObject)
  public type
    TArrayList_TGameLevel = specialize TArrayList<TGameLevel>;

  private const
    // 初始化挡板的大小
    PLAYER_SIZE: TVec2 = (x:100; y: 20);
    // 初始化当班的速率
    PLAYER_VELOCITY: GLfloat = (500.0);

  public
    Height: GLuint;
    Width: GLuint;
    Keys: array of GLboolean;
    State: TGameState;
    Renderer: TSpriteRenderer;
    Player: TGameObject;

    Levels: TArrayList_TGameLevel;
    Level: GLuint;

    constructor Create(aWidth, aHeight: GLuint);
    destructor Destroy; override;

    // Initialize game state (load all shaders/textures/levels)
    procedure Init;

    // GameLoop
    procedure ProcessInput(dt: GLfloat);
    procedure Update(dt: GLfloat);
    procedure Render;
  end;

implementation

{ TGame }

constructor TGame.Create(aWidth, aHeight: GLuint);
begin
  Width := aWidth;
  Height := aHeight;

  SetLength(Keys, 1024);
  State := TGameState.GAME_ACTIVE;

  Levels := TArrayList_TGameLevel.Create;
end;

destructor TGame.Destroy;
var
  i: Integer;
begin
  if Renderer <> nil then
    FreeAndNil(Renderer);

  if Player <> nil then
    FreeAndNil(Player);

  if not Levels.IsEmpty then
  begin
    for i := 0 to Levels.Count - 1 do
      Levels[i].Free;

    FreeAndNil(Levels);
  end;

  inherited Destroy;
end;

procedure TGame.Init;
var
  projection: TMat4;
  one, tow, three, four: TGameLevel;
  playerPos: TVec2;
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
end;

procedure TGame.ProcessInput(dt: GLfloat);
var
  velocity: GLfloat;
begin
  if Self.State = GAME_ACTIVE then
  begin
    velocity := GLfloat(PLAYER_VELOCITY * dt);

    // 移动挡板
    if Self.Keys[GLFW_KEY_A] = GL_TRUE then
    begin
      if Player.Position.x >= 0 then
        Player.Position.x -= velocity;
    end;

    if Self.Keys[GLFW_KEY_D]  = GL_TRUE then
    begin
      if Player.Position.x <= Self.Width - Player.Size.x then
        Player.Position.x += velocity;
    end;
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
  end;
end;

procedure TGame.Update(dt: GLfloat);
begin

end;

end.

