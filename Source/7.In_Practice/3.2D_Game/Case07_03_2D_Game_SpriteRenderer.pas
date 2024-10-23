unit Case07_03_2D_Game_SpriteRenderer;

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
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM,
  Case07_03_2D_Game_Shader,
  Case07_03_2D_Game_Texture2D;

type
  TSpriteRenderer = class(TInterfacedObject)
  private
    // Render state
    _Shader: TShader;
    _QuadVAO: GLuint;

    // Initializes and configures the quad's buffer and vertex attributes
    procedure __InitRenderData;

  public
    constructor Create(shader: TShader);
    destructor Destroy; override;

    // 用给定的精灵渲染一个定义的四边形纹理
    procedure DrawSprite(aTexture: TTexture2D; aPosition: PVec2; aSize: PVec2 = nil;
      aRotate: PGLfloat = nil; aColor: PVec3 = nil);
  end;

implementation

{ TSpriteRenderer }

constructor TSpriteRenderer.Create(shader: TShader);
begin
  _Shader := shader;
  __InitRenderData;
end;

destructor TSpriteRenderer.Destroy;
begin
  glDeleteVertexArrays(1, @_QuadVAO);

  inherited Destroy;
end;

procedure TSpriteRenderer.DrawSprite(aTexture: TTexture2D; aPosition: PVec2;
  aSize: PVec2; aRotate: PGLfloat; aColor: PVec3);
var
  size, position: TVec2;
  color: TVec3;
  model: TMat4;
  texture: TTexture2D;
  rotate: GLfloat;
begin
  texture := aTexture;
  size := IfThen(aSize = nil, TGLM.Vec2(10), aSize^);
  color := IfThen(aColor = nil, TGLM.Vec3(1.0), aColor^);
  position := aPosition^;
  rotate := IfThen(aRotate = nil, 0.0, aRotate^);

  // Prepare transformations
  _Shader.Use;

  model := TGLM.Mat4_Identity;
  // 首先是平移(变换是：首先发生缩放，然后是旋转，最后发生平移；颠倒顺序)
  model := TGLM.Translate(model, TGLM.Vec3(position.x, position.y, 0.0));
  // 将旋转原点移动到四边形中心
  model := TGLM.Translate(model, TGLM.Vec3(0.5 * size.x, 0.5 * size.y, 0.0));
  // 然后旋转
  model := TGLM.Rotate(model, rotate, TGLM.Vec3(0.0, 0.0, 1.0));
  // 移动原点
  model := TGLM.Translate(model, TGLM.Vec3(-0.5 * size.x, -0.5 * size.y, 0.0));
  // 最后缩放
  model := TGLM.Scale(model, TGLM.Vec3(size.x, size.y, 1.0));

  _Shader.SetMatrix4('model', model);

  // Render textured quad
  _Shader.SetVector3f('spriteColor', color);

  glActiveTexture(GL_TEXTURE0);
  texture.Bind;

  glBindVertexArray(_quadVAO);
  glDrawArrays(GL_TRIANGLES, 0, 6);
  glBindVertexArray(0);
end;

procedure TSpriteRenderer.__InitRenderData;
var
  vertices: TArr_GLfloat;
  VBO: GLuint;
begin
  // Configure VAO/VBO
  VBO := GLuint(0);
  vertices := TArr_GLfloat([
      // Pos      // Tex
      0.0, 1.0,   0.0, 1.0,
      1.0, 0.0,   1.0, 0.0,
      0.0, 0.0,   0.0, 0.0,

      0.0, 1.0,   0.0, 1.0,
      1.0, 1.0,   1.0, 1.0,
      1.0, 0.0,   1.0, 0.0]);

  glGenVertexArrays(1, @_QuadVAO);
  glGenBuffers(1, @VBO);

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, vertices.MemSize, @vertices[0], GL_STATIC_DRAW);

  glBindVertexArray(_QuadVAO);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * SIZE_OF_F, Pointer(0));
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
end;

end.

