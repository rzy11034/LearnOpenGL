unit Case07_03_2D_Game_GameObject;

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
    constructor Create();
    constructor Create(aPos: TVec2; aSize: TVec2; aSprite: TTexture2D;
      aColor: PVec3 = nil; aVelocity: PVec2 = nil);

    // draw sprite
    procedure Draw(renderer: TSpriteRenderer);
  end;

implementation

{ TGameObject }

constructor TGameObject.Create(aPos: TVec2; aSize: TVec2; aSprite: TTexture2D;
  aColor: PVec3; aVelocity: PVec2);
begin
  Position := aPos;
  Size := aSize;
  Velocity := IfThen(aVelocity = nil, TGLM.Vec2(0.0), aVelocity^);
  Color := IfThen(aColor = nil, TGLM.Vec3(1.0), aColor^);
  Rotation := 0.0;
  Sprite := aSprite;
  IsSolid := false;
  Destroyed := false;
end;

constructor TGameObject.Create();
begin
  Position := TGLM.Vec2(0.0, 0.0);
  Size := TGLM.Vec2(1.0, 1.0);
  Velocity := TGLM.Vec2(0.0);
  Color := TGLM.Vec3(1.0);
  Rotation := 0.0;
  Sprite := nil;
  IsSolid := false;
  Destroyed := false;
end;

procedure TGameObject.Draw(renderer: TSpriteRenderer);
begin
  renderer.DrawSprite(Sprite, @Position, @Size, @Rotation, @Color);
end;

end.

