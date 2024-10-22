unit Case07_03_2D_Game_Shader;

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
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.Shader;

type
  TShader = class(TShaderProgram)
  public
    constructor Create;
    destructor Destroy; override;

    function Use: TShader;

    procedure SetFloat    (const name: string; value: GLfloat; useShader:Boolean = false);
    procedure SetInteger  (const name: string; value: GLint; useShader:Boolean = false);
    procedure SetVector2f (const name: string; x, y: GLfloat; useShader:Boolean = false);
    procedure SetVector2f (const name: string; value: TVec2; useShader:Boolean = false);
    procedure SetVector3f (const name: string; x, y, z: GLfloat; useShader:Boolean = false);
    procedure SetVector3f (const name: string; value: TVec3; useShader:Boolean = false);
    procedure SetVector4f (const name: string; x, y, z, w: GLfloat; useShader:Boolean = false);
    procedure SetVector4f (const name: string; value: TVec4; useShader:Boolean = false);
    procedure SetMatrix4  (const name: string; matrix: TMat4; useShader:Boolean = false);
  end;

implementation

{ TShader }

constructor TShader.Create;
begin
  inherited;
end;

destructor TShader.Destroy;
begin
  inherited Destroy;
end;

procedure TShader.SetFloat(const name: string; value: GLfloat; useShader: Boolean);
begin
  if useShader then
    Self.Use;

  inherited SetUniformFloat(name, value);
end;

procedure TShader.SetInteger(const name: string; value: GLint; useShader: Boolean);
begin
  if useShader then
    Self.Use;

  inherited SetUniformInt(name, value);
end;

procedure TShader.SetMatrix4(const name: string; matrix: TMat4; useShader: Boolean);
begin
  if useShader then
    Self.Use;

  inherited SetUniformMatrix4fv(name, matrix);
end;

procedure TShader.SetVector2f(const name: string; x, y: GLfloat; useShader: Boolean);
begin
  if useShader then
    Self.Use;

  inherited SetUniformFloat(name, x, y);
end;

procedure TShader.SetVector2f(const name: string; value: TVec2; useShader: Boolean);
begin
  if useShader then
    Self.Use;

  inherited SetUniformFloat(name, value.x, value.y);
end;

procedure TShader.SetVector3f(const name: string; x, y, z: GLfloat; useShader: Boolean);
begin
  if useShader then
    Self.Use;

  inherited SetUniformFloat(name, x, y, z);
end;

procedure TShader.SetVector3f(const name: string; value: TVec3; useShader: Boolean);
begin
  if useShader then
    Self.Use;

  inherited SetUniformVec3(name, value);
end;

procedure TShader.SetVector4f(const name: string; x, y, z, w: GLfloat; useShader: Boolean);
begin
  if useShader then
    Self.Use;

  inherited SetUniformFloat(name, x, y, z, w);
end;

procedure TShader.SetVector4f(const name: string; value: TVec4; useShader: Boolean);
begin
  if useShader then
    Self.Use;

  inherited SetUniformVec4(name, value);
end;

function TShader.Use: TShader;
begin
  inherited UseProgram;
  Result := Self;
end;

end.
