unit Case07_03_2D_Game_Game;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.OpenGL.GLAD_GL,
  Case07_03_2D_Game_SpriteRenderer,
  Case07_03_2D_Game_ResourceManager;

type
  TGameState = (GAME_ACTIVE, GAME_MENU, GAME_WIN);

  TGame = class(TInterfacedObject)
  private
    _Renderer: TSpriteRenderer;

  public
    Keys: array of GLboolean;
    State: TGameState;

    constructor Create(width, height: GLuint);
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

constructor TGame.Create(width, height: GLuint);
begin
  inherited Create;

  Init;
end;

destructor TGame.Destroy;
begin
  inherited Destroy;
end;

procedure TGame.Init;
var
  resourceManager_managed: IInterface;
  resourceManager: TResourceManager;
begin
  SetLength(Keys, 1024);
  State := TGameState.GAME_ACTIVE;

  resourceManager_managed := IInterface(TResourceManager.Create);
  resourceManager := resourceManager_managed as TResourceManager;



end;

procedure TGame.ProcessInput(dt: GLfloat);
begin

end;

procedure TGame.Render;
begin

end;

procedure TGame.Update(dt: GLfloat);
begin

end;

end.

