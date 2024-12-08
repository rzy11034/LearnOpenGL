unit Case07_03_2D_Game_Powerups;

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
  DeepStar.OpenGL.GLAD_GL,
  Case07_03_2D_Game_ResourceManager,
  Case07_03_2D_Game_GameObject,
  Case07_03_2D_Game_Texture2D;

type
  TPowerup = class(TGameObject)
  private const
    _SIZE: TVec2 = (x: 60; y: 20);
    _VELOCITY: TVec2 = (x: 0.0; y: 150.0);

  public
    // powerup state
    Type_: string;
    Duration: GLfloat;
    Activated: Boolean;

    constructor Create(type_: string; color: TVec3; duration: Single;
      position: TVec2; texture: TTexture2D);
    destructor Destroy; override;
  end;

implementation

{ TPowerup }

constructor TPowerup.Create(type_: string; color: TVec3; duration: Single;
  position: TVec2; texture: TTexture2D);
begin
  inherited Create(position, _SIZE, texture, @color, @_VELOCITY);

  Self.Type_ := type_;
  Self.Duration := duration;
  Self.Activated := Default(Boolean);
end;

destructor TPowerup.Destroy;
begin
  inherited Destroy;
end;

end.

