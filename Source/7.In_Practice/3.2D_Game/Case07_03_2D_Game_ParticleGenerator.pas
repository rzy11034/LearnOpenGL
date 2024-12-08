unit Case07_03_2D_Game_ParticleGenerator;

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
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM,
  Case07_03_2D_Game_Shader,
  Case07_03_2D_Game_ResourceManager,
  Case07_03_2D_Game_GameObject,
  Case07_03_2D_Game_Texture2D;

type
  // 表示单个粒子及其状态
  TParticle = Record
    Position, Velocity: TVec2;
    Color: TVec4;
    Life: float;
    class function Create: TParticle; static;
  end;

  TArrayList_TParticle = specialize TArrayList<TParticle>;

  // ParticleGenerator 作为一个容器来渲染大量的
  // 通过重复刷出和更新粒子和杀死
  // 在给定的时间后返回。
  TParticleGenerator = class(TInterfacedObject)
  private
    // State
    _Particles: TArrayList_TParticle;
    _Amount: Cardinal;
    // Render state
    _Shader: TShader;
    _Texture: TTexture2D;
    _VAO: Cardinal;
    _LastUsedParticle: Cardinal;

    //初始化 buffer 和 vertex 属性
    procedure __Init;

    // 返回当前未使用的第一个粒子索引。
    // 如果当前没有粒子处于非活动状态，则寿命<= 0
    function __FirstUnusedParticle: Cardinal;

    // 重生粒子
    procedure __RespawnParticle(var particle: TParticle; obj: TGameObject;
      PtrOffset: PVec2 = nil);

  public
    constructor Create(shader: TShader; texture: TTexture2D; amount: Cardinal);
    destructor Destroy; override;

    // Update all particles
    procedure Update(dt: float; obj: TGameObject; newParticles: cardinal;
      PtrOffset: PVec2 = nil);

    // Render all particles
    procedure Draw;
  end;

implementation

{ TParticle }

class function TParticle.Create: TParticle;
var
  res: TParticle;
begin
  res := Default(TParticle);

  with res do
  begin
    Position := TGLM.Vec2(0);
    Velocity := TGLM.Vec2(0);
    Color    := TGLM.Vec4(1);
    Life     := 0.0;
  end;

  Result := res;
end;

{ TParticleGenerator }

constructor TParticleGenerator.Create(shader: TShader; texture: TTexture2D;
  amount: Cardinal);
begin
  _Particles := TArrayList_TParticle.Create;
  _Shader := shader;
  _Texture := texture;
  _Amount := amount;

  Self.__Init;
end;

destructor TParticleGenerator.Destroy;
begin
  _Particles.Free;

  inherited Destroy;
end;

procedure TParticleGenerator.Draw;
var
  i: Integer;
  particle: TParticle;
begin
  // 使用添加的混合给它一个“发光”的效果
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);

  _Shader.Use;
  for i := 0  to Self._Particles.Count - 1 do
  begin
    particle := _Particles[i];

    if particle.Life > 0.0 then
    begin
      _Shader.SetVector2f('offset', particle.Position);
      _Shader.SetVector4f('color', particle.Color);
      _Texture.Bind;
      glBindVertexArray(Self._VAO);
      glDrawArrays(GL_TRIANGLES, 0, 6);
      glBindVertexArray(0);
    end;
  end;

  // 不要忘记重置为默认混合模式
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

procedure TParticleGenerator.Update(dt: float; obj: TGameObject;
  newParticles: cardinal; PtrOffset: PVec2);
var
  offset: TVec2;
  unusedParticle: Cardinal;
  i: Integer;
  p: ^TParticle;
begin
  offset := IfThen(PtrOffset = nil, TGLM.Vec2(0), PtrOffset^);

  // 添加新粒子
  for i := 0 to newParticles - 1 do
  begin
    unusedParticle := Self.__FirstUnusedParticle;
    p := _Particles.ItemPtr[unusedParticle];
    Self.__RespawnParticle(p^, obj, @offset);
  end;

  // 更新所有粒子
  for i := 0 to  Self._Amount - 1 do
  begin
    p :=  Self._Particles.ItemPtr[i];
    p^.Life -= dt; // 降低生命值

    if p^.Life > 0.0 then
    begin
      // 粒子生命值大于0，就更新
      p^.Position -= p^.Velocity * dt;
      p^.Color.a -= dt * 2.5;
    end;
  end;
end;

function TParticleGenerator.__FirstUnusedParticle: Cardinal;
var
  i: integer;
begin
  // 对最后使用的粒子进行第一次搜索，这通常会立即返回
  for i := _LastUsedParticle to Self._Amount do
  begin
    if i = Self._Amount then Break;

    if Self._Particles[i].Life <= 0.0 then
    begin
      _LastUsedParticle := i;
      Exit(i);
    end;
  end;

  // 否则，进行线性查找
  for i := 0 to _LastUsedParticle do
  begin
    if i = _LastUsedParticle then Break;

    if Self._Particles[i].Life <= 0.0 then
    begin
      _LastUsedParticle := i;
      Exit(i);
    end;
  end;

  // 所有的粒子都被占用了，覆盖第一个
  //（注意，如果它重复击中这种情况，应该保留更多的粒子）
  _LastUsedParticle := 0;

  Result := 0;
end;

procedure TParticleGenerator.__Init;
var
  VBO: Cardinal;
  particle_quad: TArr_GLfloat;
  i: Integer;
begin
  VBO := Cardinal(0);
  particle_quad := TArr_GLfloat([
      0.0, 1.0, 0.0, 1.0,
      1.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 0.0,

      0.0, 1.0, 0.0, 1.0,
      1.0, 1.0, 1.0, 1.0,
      1.0, 0.0, 1.0, 0.0]);

  glGenVertexArrays(1, @_VAO);
  glGenBuffers(1, @VBO);
  glBindVertexArray(Self._VAO);

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, particle_quad.MemSize, @particle_quad[0], GL_STATIC_DRAW);

  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * SIZE_OF_F, Pointer(0));
  glBindVertexArray(0);

  // 创造默认粒子实例的数量
  for i := 0 to _Amount - 1 do
    _Particles.AddLast(TParticle.Create);
end;

procedure TParticleGenerator.__RespawnParticle(var particle: TParticle;
  obj: TGameObject; PtrOffset: PVec2);
var
  offset: TVec2;
  rand, rColor: float;
begin
  offset := IfThen(PtrOffset = nil, TGLM.Vec2(0), PtrOffset^);

  Randomize;
  rand := (Random(100) - 50) / 10;
  rColor := 0.5 + Random(100) / 100.0;
  particle.Position := obj.Position + rand + offset;
  particle.Color := TGLM.Vec4(rColor, rColor, rColor, 1.0);
  particle.Life := 1.0;
  particle.Velocity := obj.Velocity * 0.1;
end;

end.

