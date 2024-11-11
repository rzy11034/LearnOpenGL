unit Case07_03_2D_Game_PostProcessor;

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
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Utils,
  Case07_03_2D_Game_Shader,
  Case07_03_2D_Game_ResourceManager,
  Case07_03_2D_Game_Texture2D;

type
  // PostProcessor 承载了 Breakout 的所有后处理效果
  // Game。它在纹理四边形上渲染游戏，之后可以
  // 通过启用迷惑，混乱或摇布尔值。
  // 在渲染游戏之前需要调用BeginRender 和 EndRender 后渲染游戏类的工作。
  TPostProcessor = class(TInterfacedObject)
  private
    // Render state
    _MSFBO: GLuint; // MSFBO = 超级全场景多重采样抗失真功能
    _FBO: GLuint; // FBO用于将MS颜色规则的缓冲位元到纹理
    _RBO: GLuint; // RBO is used for multisampled color buffer
    _VAO: GLuint;

    // 初始化四边形以渲染后处理纹理
    procedure __InitRenderData;

  public
    // State
    PostProcessingShader: TShader;
    Texture: TTexture2D;
    Width, Height: GLuint;
    // Options
    Confuse, Chaos, Shake: boolean;

    constructor Create(shader: TShader; Width, Height: GLuint);
    destructor Destroy; override;


    // 在渲染游戏之前准备后处理器的帧缓冲操作
    procedure BeginRender;

    // 应该在渲染游戏后调用，所以它将所有渲染数据存储到纹理对象中
    procedure EndRender();

    // 渲染 PostProcessor 纹理四边形（作为一个覆盖屏幕的大精灵）
    procedure Render(time: float);

  end;

implementation

{ TPostProcessor }

constructor TPostProcessor.Create(shader: TShader; Width, Height: GLuint);
var
  offset: GLfloat;
  offsets: TArr2D_GLfloat;
  edge_kernel: TArr_GLint;
  blur_kernel: TArr_GLfloat;
begin
  Self.PostProcessingShader := shader;
  Self.Texture := nil;
  Self.Width := width;
  Self.Height := height;
  Self.Confuse := false;
  Self.Chaos := false;
  Self.Shake := false;

  // 初始化 renderbuffer/framebuffer 对象
  glGenFramebuffers(1, @Self._MSFBO);
  glGenFramebuffers(1, @Self._FBO);
  glGenRenderbuffers(1, @Self._RBO);

  // 使用多采样颜色缓冲区初始化renderbuffer存储（不需要深度/模板缓冲区）
  glBindFramebuffer(GL_FRAMEBUFFER, Self._MSFBO);
  glBindRenderbuffer(GL_RENDERBUFFER, Self._RBO);
  // 为渲染缓冲区对象分配存储空间
  glRenderbufferStorageMultisample(GL_RENDERBUFFER, 8, GL_RGB, width, height);
  // 将MS渲染缓冲区对象附加到framebuffer
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, Self._RBO);
  if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
      WriteLn('ERROR::POSTPROCESSOR: Failed to initialize MSFBO');

  // 同时初始化FBO/纹理，将多采样颜色缓冲区blit为；用于着色器操作（用于后期处理效果）
  glBindFramebuffer(GL_FRAMEBUFFER, Self._FBO);
  Self.Texture.Generate(width, height, nil);

  // 将纹理作为其颜色附件附加到framebuffer
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, Self.Texture.ID, 0);
  if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
      WriteLn('ERROR::POSTPROCESSOR: Failed to initialize FBO');
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  // 初始化渲染数据和制服
  Self.__InitRenderData;
  Self.PostProcessingShader.SetInteger('scene', 0, true);

  offset := GLfloat(1.0 / 300.0);
  offsets := TArr2D_GLfloat([
      [ -offset,  offset  ],  // top-left
      [  0.0 ,    offset  ],  // top-center
      [  offset,  offset  ],  // top-right
      [ -offset,  0.0     ],  // center-left
      [  0.0 ,    0.0     ],  // center-center
      [  offset,  0.0     ],  // center - right
      [ -offset, -offset  ],  // bottom-left
      [  0.0 ,   -offset  ],  // bottom-center
      [  offset, -offset  ]   // bottom-right
      ]);
  glUniform2fv(glGetUniformLocation(Self.PostProcessingShader.ID, 'offsets'), 9, @offsets[0, 0]);

  edge_kernel := TArr_GLint([
      -1, -1, -1,
      -1,  8, -1,
      -1, -1, -1
      ]);
  glUniform1iv(glGetUniformLocation(Self.PostProcessingShader.ID, 'edge_kernel'),
    9, @edge_kernel[0]);

  blur_kernel := TArr_GLfloat([
      1.0 / 16, 2.0 / 16, 1.0 / 16,
      2.0 / 16, 4.0 / 16, 2.0 / 16,
      1.0 / 16, 2.0 / 16, 1.0 / 16
      ]);
  glUniform1fv(glGetUniformLocation(Self.PostProcessingShader.ID, 'blur_kernel'),
    9, @blur_kernel[0]);
end;

procedure TPostProcessor.BeginRender;
begin
  glBindFramebuffer(GL_FRAMEBUFFER, Self._MSFBO);
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
end;

destructor TPostProcessor.Destroy;
begin
  inherited Destroy;
end;

procedure TPostProcessor.EndRender;
begin
  // 现在将多采样颜色缓冲区解析为中间FBO以存储到纹理中
  glBindFramebuffer(GL_READ_FRAMEBUFFER, Self._MSFBO);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, Self._FBO);
  glBlitFramebuffer(0, 0, Self.Width, Self.Height, 0, 0, Self.Width, Self.Height,
    GL_COLOR_BUFFER_BIT, GL_NEAREST);

  // 将读和写帧缓冲区绑定到默认帧缓冲区
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

procedure TPostProcessor.Render(time: float);
begin
  // Set uniforms/options
  Self.PostProcessingShader.Use;
  Self.PostProcessingShader.SetFloat('time', time);
  Self.PostProcessingShader.SetInteger('confuse', Self.Confuse.ToInteger);
  Self.PostProcessingShader.SetInteger('chaos', Self.Chaos.ToInteger);
  Self.PostProcessingShader.SetInteger('shake', Self.Shake.ToInteger);

  // Render textured quad
  glActiveTexture(GL_TEXTURE0);
  Self.Texture.Bind();
  glBindVertexArray(Self._VAO);
  glDrawArrays(GL_TRIANGLES, 0, 6);
  glBindVertexArray(0);
end;

procedure TPostProcessor.__InitRenderData;
var
  VBO: GLuint;
  vertices: TArr_GLfloat;
begin
  // Configure VAO/VBO
  VBO := GLuint(0);
  vertices := TArr_GLfloat([
	  // Pos        // Tex
	  -1.0, -1.0,   0.0, 0.0,
	   1.0,  1.0,   1.0, 1.0,
	  -1.0,  1.0,   0.0, 1.0,

	  -1.0, -1.0,   0.0, 0.0,
	   1.0, -1.0,   1.0, 0.0,
	   1.0,  1.0,   1.0, 1.0
     ]);

  glGenVertexArrays(1, @Self._VAO);
  glGenBuffers(1, @VBO);

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, vertices.MemSize, @vertices[0], GL_STATIC_DRAW);

  glBindVertexArray(Self._VAO);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * SIZE_OF_F, nil);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
end;

end.

