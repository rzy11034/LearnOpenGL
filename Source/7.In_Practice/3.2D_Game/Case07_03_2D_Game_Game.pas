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
  DeepStar.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM,
  Case07_03_2D_Game_Consts,
  Case07_03_2D_Game_SpriteRenderer,
  Case07_03_2D_Game_ResourceManager;

type
  TGameState = (GAME_ACTIVE, GAME_MENU, GAME_WIN);

  TGame = class(TInterfacedObject)
  public
    Height: GLuint;
    Width: GLuint;
    Keys: array of GLboolean;
    State: TGameState;
    Renderer: TSpriteRenderer;

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
  //inherited Create;

  Width := aWidth;
  Height := aHeight;

  SetLength(Keys, 1024);
  State := TGameState.GAME_ACTIVE;
end;

destructor TGame.Destroy;
begin
  if Renderer <> nil then
    FreeAndNil(Renderer);

  inherited Destroy;
end;

procedure TGame.Init;
var
  projection: TMat4;
begin
  if not TResourceManager.Shaders.ContainsKey(SPRITE_NAME) then
    TResourceManager.LoadShader(SPRITE_NAME, SPRITE_VS, SPRITE_FS, '');

  projection := TGLM.Ortho(0.0, Width, Height, 0.0, -1.0, 1.0);
  TResourceManager.GetShader(SPRITE_NAME).Use.SetInteger('image', 0);
  TResourceManager.GetShader(SPRITE_NAME).SetMatrix4('projection', projection);

  // 设置专用于渲染的控制
  Renderer := TSpriteRenderer.Create(TResourceManager.GetShader(SPRITE_NAME));
  // 加载纹理

  if not TResourceManager.Textures.ContainsKey(IMG_AWESOMEFACE_NAME) then
    TResourceManager.LoadTexture(IMG_AWESOMEFACE_NAME, IMG_AWESOMEFACE, true);

end;

procedure TGame.ProcessInput(dt: GLfloat);
begin

end;

procedure TGame.Render;
var
  position, size: TVec2;
  rotate: Single;
  color: TVec3;
begin
  position := TGLM.Vec2(200, 200);
  size := TGLM.Vec2(300, 400);
  rotate := 45.0;
  color := TGLM.Vec3(0.0, 1.0, 0.0);
  Renderer.DrawSprite(TResourceManager.GetTexture(IMG_AWESOMEFACE_NAME),
    @position, @size, @rotate, @color);
end;

procedure TGame.Update(dt: GLfloat);
begin

end;

end.

