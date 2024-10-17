unit Case07_03_2D_Game_Main;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL;

type
  TGameState = (GAME_ACTIVE, GAME_MENU, GAME_WIN);

  TGame = class(TInterfacedObject)
  private

  protected

  public
    Keys: array of GLboolean;
    State: TGameState;

    constructor Create(width, height: GLuint); overload;
    destructor Destroy; override;

    // Initialize game state (load all shaders/textures/levels)
    procedure Init;

    // GameLoop
    procedure ProcessInput(dt: GLfloat);
    procedure Update(dt: GLfloat);
    procedure Render;

  published

  end;

procedure Main;

implementation

procedure Main;
var
  game: TGame;
  game_managed: IInterface;
begin
  game_managed := IInterface(TGame.Create);
  game := game_managed as TGame;

end;

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
begin
  SetLength(Keys, 1024);
  State := TGameState.GAME_ACTIVE;
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

