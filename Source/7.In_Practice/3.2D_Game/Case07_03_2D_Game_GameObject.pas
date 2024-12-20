unit Case07_03_2D_Game_GameObject;

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
  DeepStar.OpenGL.GLM,
  Case07_03_2D_Game_SpriteRenderer,
  Case07_03_2D_Game_Texture2D;

type
  TGameObject = class(TInterfacedObject)
  public
    // object state
    Position, Size, Velocity: TVec2;
    Color: TVec3;
    Rotation: float;
    IsSolid: Boolean;
    Destroyed: Boolean;

    // render state
    Sprite: TTexture2D;

    // constructor(s)
    constructor Create;
    constructor Create(pos: TVec2; size: TVec2; sprite: TTexture2D;
      color: PVec3 = nil; velocity: PVec2 = nil);

    // draw sprite
    procedure Draw(renderer: TSpriteRenderer);
  end;

implementation

{ TGameObject }

constructor TGameObject.Create(pos: TVec2; size: TVec2; sprite: TTexture2D;
  color: PVec3; velocity: PVec2);
begin
  Self.Position := pos;
  Self.Size := size;
  Self.Rotation := 0.0;
  Self.Sprite := sprite;
  Self.IsSolid := false;
  Self.Destroyed := false;

  Self.Color := TGLM.Vec3(1.0);
  if color <> nil then Self.Color := color^;

  Self.Velocity := TGLM.Vec2(0.0);
  if velocity <> nil then Self.Velocity := velocity^;
end;

constructor TGameObject.Create();
begin
  Self.Position := TGLM.Vec2(0.0, 0.0);
  Self.Size := TGLM.Vec2(1.0, 1.0);
  Self.Velocity := TGLM.Vec2(0.0);
  Self.Color := TGLM.Vec3(1.0);
  Self.Rotation := 0.0;
  Self.Sprite := nil;
  Self.IsSolid := false;
  Self.Destroyed := false;
end;

procedure TGameObject.Draw(renderer: TSpriteRenderer);
begin
  renderer.DrawSprite(Sprite, @Position, @Size, @Rotation, @Color);
end;

end.

