unit Case05_09_01_SSAO;

{$mode objfpc}{$H+}
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
  DeepStar.OpenGL.Texture,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.Camera,
  DeepStar.OpenGL.Model;

procedure Main;

implementation

// 每当窗口大小发生变化(由操作系统或用户调整大小)，这个回调函数就会执行
procedure Framebuffer_size_callback(window: PGLFWwindow; witdth, Height: integer); cdecl; forward;
// 每当鼠标滚轮滚动时，这个回调就被调用
procedure Scroll_callback(window: PGLFWwindow; xoffset, yoffset: double); cdecl; forward;
// 每当鼠标移动时，就调用这个回调
procedure Mouse_callback(window: PGLFWwindow; xpos, ypos: double); cdecl; forward;
// 处理所有输入:查询GLFW是否按下/释放了相关的键，并做出相应的反应
procedure ProcessInput(window: PGLFWwindow); forward;
// glfw & glad  初始化
function InitWindows: PGLFWwindow; forward;
// 加载贴图
function LoadTexture(fileName: string; gammaCorrection: boolean = false;
  inverse: boolean = true): cardinal; forward;

procedure RenderQuad; forward;
procedure RenderCube; forward;

const
  SCR_WIDTH = 800;
  SCR_HEIGHT = 600;

var
  camera: TCamera;

  deltaTime: float = 0.0;  // time between current frame and last frame
  lastFrame: float = 0.0;

  firstMouse: boolean = true;

  //偏航被初始化为-90.0度，因为0.0的偏航导致一个指向右的方向矢量，所以我们最初
  //向左旋转一点。
  lastX: float = SCR_WIDTH / 2.0;
  lastY: float = SCR_HEIGHT / 2.0;

  cubeVAO: Cardinal = 0;
  cubeVBO: Cardinal = 0;

  quadVAO: Cardinal = 0;
  quadVBO: Cardinal = 0;

function Lerp(a, b, f: float): float;
begin
  Result := a + f * (b - a);
end;

procedure Main;
const
  dir_path = '..\Source\5.Advanced_Lighting\9.1.SSAO\';

  ssao_vs = dir_path + '9.ssao.vs';
  ssao_fs = dir_path + '9.ssao.fs';

  ssao_blur_fs = dir_path + '9.ssao_blur.fs';
  ssao_lighting_fs = dir_path + '9.ssao_lighting.fs';

  ssao_geometry_vs = dir_path + '9.ssao_geometry.vs';
  ssao_geometry_fs = dir_path + '9.ssao_geometry.fs';

  ObjBackpack = '..\Resources\objects\backpack\backpack.obj';
var
  window: PGLFWwindow;
  camera_managed, shaderGeometryPass_managed, shaderLightingPass_managed,
    shaderSSAO_managed, shaderSSAOBlur_managed, backpack_managed: IInterface;
  shaderGeometryPass, shaderLightingPass, shaderSSAO, shaderSSAOBlur: TShaderProgram;
  gPosition, gNormal, gAlbedo, ssaoFBO, ssaoBlurFBO, ssaoColorBufferBlur,
    ssaoColorBuffer, gBuffer, rboDepth, noiseTexture: Cardinal;
  scale, linear, quadratic: float;
  sample, noise, lightPos, lightColor, lightPosView: TVec3;
  ssaoNoise, ssaoKernel: TArr_TVec3;
  view, model, projection: TMat4;
  backpack: TModel;
  i: Integer;
  attachments: TArr_GLuint;
  currentFrame: GLfloat;
begin
  window := InitWindows;
  if window = nil then
  begin
    glfwTerminate;
    Exit;
  end;

  //═════════════════════════════════════════════════════════════════════════'

  camera_managed := IInterface(TCamera.Create(TGLM.Vec3(0, 0, 5)));
  camera := camera_managed as TCamera;

  //═════════════════════════════════════════════════════════════════════════

  // configure global opengl state
  glEnable(GL_DEPTH_TEST);

  //═════════════════════════════════════════════════════════════════════════

  shaderGeometryPass_managed := IInterface(TShaderProgram.Create);
  shaderGeometryPass := shaderGeometryPass_managed as TShaderProgram;
  shaderGeometryPass.LoadShaderFile(ssao_geometry_vs, ssao_geometry_fs);

  shaderLightingPass_managed := IInterface(TShaderProgram.Create);
  shaderLightingPass := shaderLightingPass_managed as TShaderProgram;
  shaderLightingPass.LoadShaderFile(ssao_vs, ssao_lighting_fs);

  shaderSSAO_managed := IInterface(TShaderProgram.Create);
  shaderSSAO := shaderSSAO_managed as TShaderProgram;
  shaderSSAO.LoadShaderFile(ssao_vs, ssao_fs);

  shaderSSAOBlur_managed := IInterface(TShaderProgram.Create);
  shaderSSAOBlur := shaderSSAOBlur_managed as TShaderProgram;
  shaderSSAOBlur.LoadShaderFile(ssao_vs, ssao_blur_fs);

  //═════════════════════════════════════════════════════════════════════════

  // load models
  backpack_managed := IInterface(TModel.Create(ObjBackpack));
  backpack := backpack_managed as TModel;

  //═════════════════════════════════════════════════════════════════════════

  // configure g-buffer framebuffer
  gBuffer := Cardinal(0);
  glGenFramebuffers(1, @gBuffer);
  glBindFramebuffer(GL_FRAMEBUFFER, gBuffer);

  gPosition := Cardinal(0);
  gNormal := Cardinal(0);
  gAlbedo := Cardinal(0);

  // position color buffer
  glGenTextures(1, @gPosition);
  glBindTexture(GL_TEXTURE_2D, gPosition);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGBA, GL_FLOAT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gPosition, 0);

  // normal color buffer
  glGenTextures(1, @gNormal);
  glBindTexture(GL_TEXTURE_2D, gNormal);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGBA, GL_FLOAT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, gNormal, 0);

  // color + specular color buffer
  glGenTextures(1, @gAlbedo);
  glBindTexture(GL_TEXTURE_2D, gAlbedo);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT2, GL_TEXTURE_2D, gAlbedo, 0);

  // 告诉OpenGL我们将使用哪个颜色附件(这个framebuffer)进行渲染
  attachments := TArr_GLuint([GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1, GL_COLOR_ATTACHMENT2]);
  glDrawBuffers(3, @attachments[0]);

  // 创建并附加深度缓冲(renderbuffer)
  rboDepth := Cardinal(0);
  glGenRenderbuffers(1, @rboDepth);
  glBindRenderbuffer(GL_RENDERBUFFER, rboDepth);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, SCR_WIDTH, SCR_HEIGHT);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, rboDepth);

  // 最后检查framebuffer是否完成
  if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
      WriteLn('Framebuffer not complete!');
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  //═════════════════════════════════════════════════════════════════════════

  // 还创建 framebuffer 来容纳 SSAO 处理阶段
  ssaoFBO := Cardinal(0);
  ssaoBlurFBO := Cardinal(0);

  glGenFramebuffers(1, @ssaoFBO);
  glGenFramebuffers(1, @ssaoBlurFBO);
  glBindFramebuffer(GL_FRAMEBUFFER, ssaoFBO);

  ssaoColorBuffer := Cardinal(0);
  ssaoColorBufferBlur := Cardinal(0);

  // SSAO color buffer
  glGenTextures(1, @ssaoColorBuffer);
  glBindTexture(GL_TEXTURE_2D, ssaoColorBuffer);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, SCR_WIDTH, SCR_HEIGHT, 0, GL_RED, GL_FLOAT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, ssaoColorBuffer, 0);

  if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
      WriteLn('SSAO Framebuffer not complete!');

  // 模糊阶段
  glBindFramebuffer(GL_FRAMEBUFFER, ssaoBlurFBO);
  glGenTextures(1, @ssaoColorBufferBlur);
  glBindTexture(GL_TEXTURE_2D, ssaoColorBufferBlur);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, SCR_WIDTH, SCR_HEIGHT, 0, GL_RED, GL_FLOAT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, ssaoColorBufferBlur, 0);

  if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
      WriteLn('SSAO Blur Framebuffer not complete!');

  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  //═════════════════════════════════════════════════════════════════════════

  // 生成示例内核
  Randomize;

  ssaoKernel := TArr_TVec3(nil);
  SetLength(ssaoKernel, 64);

  for i := 0 to High(ssaoKernel) do
  begin
    sample := TGLM.Vec3(Random * 2.0 - 1.0, Random * 2.0 - 1.0, Random);
    sample := TGLM.Normalize(sample);
    sample *= Random;
    scale := float(i) / 64.0;

    // s尺度样本更接近核的中心
    scale := lerp(0.1, 1.0, scale * scale);
    sample *= scale;
    ssaoKernel[i] := sample;
  end;

  //═════════════════════════════════════════════════════════════════════════

  // 生成噪声纹理
  ssaoNoise := TArr_TVec3(nil);
  SetLength(ssaoNoise, 16);

  for i := 0 to High(ssaoNoise) do
  begin
    noise := TGLM.Vec3(Random * 2.0 - 1.0, Random * 2.0 - 1.0, 0.0);
    ssaoNoise[i] := noise;
  end;

  noiseTexture := Cardinal(0);
  glGenTextures(1, @noiseTexture);
  glBindTexture(GL_TEXTURE_2D, noiseTexture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA32F, 4, 4, 0, GL_RGB, GL_FLOAT, @ssaoNoise[0]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);


  //═════════════════════════════════════════════════════════════════════════

  lightPos := TGLM.Vec3(2.0, 4.0, -2.0);
  lightColor := TGLM.Vec3(0.2, 0.2, 0.7);

  //═════════════════════════════════════════════════════════════════════════

  // shader configuration
  shaderLightingPass.UseProgram;
  shaderLightingPass.SetUniformInt('gPosition', 0);
  shaderLightingPass.SetUniformInt('gNormal', 1);
  shaderLightingPass.SetUniformInt('gAlbedo', 2);
  shaderLightingPass.SetUniformInt('ssao', 3);

  shaderSSAO.UseProgram;
  shaderSSAO.SetUniformInt('gPosition', 0);
  shaderSSAO.SetUniformInt('gNormal', 1);
  shaderSSAO.SetUniformInt('texNoise', 2);

  shaderSSAOBlur.UseProgram;
  shaderSSAOBlur.SetUniformInt('ssaoInput', 0);

  //═════════════════════════════════════════════════════════════════════════

  // 渲染循环
  while not glfwWindowShouldClose(window).ToBoolean do
  begin
    // 每帧时时逻辑
    currentFrame := GLfloat(glfwGetTime);
    deltaTime := currentFrame - lastFrame;
    lastFrame := currentFrame;

    // 输入
    ProcessInput(window);

    // render
    glClearColor(0.1, 0.1, 0.1, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    // 1. 几何传递:渲染场景的几何/颜色数据到 gbuffer
        // -----------------------------------------------------------------
    glBindFramebuffer(GL_FRAMEBUFFER, gBuffer);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      projection := TGLM.Perspective(TGLM.Radians(camera.Zoom),
        SCR_WIDTH / SCR_HEIGHT, 0.1, 50.0);
      view := camera.GetViewMatrix();
      model := TGLM.Mat4(1.0);
      shaderGeometryPass.UseProgram;
      shaderGeometryPass.SetUniformMatrix4fv('projection', projection);
      shaderGeometryPass.SetUniformMatrix4fv('view', view);

      // room cube
      model := TGLM.Mat4(1.0);
      model := TGLM.Translate(model, TGLM.Vec3(0.0, 7.0, 0.0));
      model := TGLM.Scale(model, TGLM.Vec3(7.5, 7.5, 7.5));
      shaderGeometryPass.SetUniformMatrix4fv('model', model);
      shaderGeometryPass.SetUniformInt('invertedNormals', 1); // 当我们进入立方体时，反法线
      RenderCube;
      shaderGeometryPass.SetUniformInt('invertedNormals', 0);

      // 地板上的背包模型
      model := TGLM.Mat4(1.0);
      model := TGLM.Translate(model, TGLM.Vec3(0.0, 0.5, 0.0));
      model := TGLM.Rotate(model, TGLM.Radians(-90.0), TGLM.Vec3(1.0, 0.0, 0.0));
      model := TGLM.Scale(model, TGLM.Vec3(1.0));
      shaderGeometryPass.SetUniformMatrix4fv('model', model);
      backpack.Draw(shaderGeometryPass);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);


    // 2. generate SSAO texture
    // ------------------------
    glBindFramebuffer(GL_FRAMEBUFFER, ssaoFBO);
      glClear(GL_COLOR_BUFFER_BIT);
      shaderSSAO.UseProgram;
      // Send kernel + rotation
      for i := 0 to High(ssaoKernel) do
          shaderSSAO.SetUniformVec3('samples[' + i.ToString + ']', ssaoKernel[i]);
      shaderSSAO.SetUniformMatrix4fv('projection', projection);
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, gPosition);
      glActiveTexture(GL_TEXTURE1);
      glBindTexture(GL_TEXTURE_2D, gNormal);
      glActiveTexture(GL_TEXTURE2);
      glBindTexture(GL_TEXTURE_2D, noiseTexture);
      RenderQuad;
    glBindFramebuffer(GL_FRAMEBUFFER, 0);


    // 3. 模糊SSAO纹理去噪
    // ------------------------------------
    glBindFramebuffer(GL_FRAMEBUFFER, ssaoBlurFBO);
      glClear(GL_COLOR_BUFFER_BIT);
      shaderSSAOBlur.UseProgram;
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, ssaoColorBuffer);
      RenderQuad;
    glBindFramebuffer(GL_FRAMEBUFFER, 0);


    // 4. 照明传递:传统的延迟Blinn-Phong照明与增加屏幕空间的环境遮挡
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    shaderLightingPass.UseProgram;

    // send light relevant uniforms
    lightPosView := TGLM.Vec3(camera.GetViewMatrix * TGLM.Vec4(lightPos, 1.0));
    shaderLightingPass.SetUniformVec3('light.Position', lightPosView);
    shaderLightingPass.SetUniformVec3('light.Color', lightColor);

    // Update attenuation parameters
    linear := float(0.09);
    quadratic := float(0.032);
    shaderLightingPass.SetUniformFloat('light.Linear', linear);
    shaderLightingPass.SetUniformFloat('light.Quadratic', quadratic);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, gPosition);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, gNormal);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, gAlbedo);
    glActiveTexture(GL_TEXTURE3); // add extra SSAO texture to lighting pass
    glBindTexture(GL_TEXTURE_2D, ssaoColorBufferBlur);
    RenderQuad;

    //═════════════════════════════════════════════════════════════════════════

    // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
    glfwSwapBuffers(window);
    glfwPollEvents;
  end;

  // 释放 / 删除之前的分配的所有资源
  glfwTerminate;
end;

function InitWindows: PGLFWwindow;
var
  window: PGLFWwindow = nil;
begin
  if not glfwInit.ToBoolean then Exit(nil);

  // 设置主要版本和次要版本
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  // 创建一个窗口对象
  window := glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, string('LearnOpenGL'), nil, nil);
  if window = nil then
  begin
    WriteLn('Failed to create GLFW window');
    Exit(nil);
  end;

  // 将窗口的上下文设置为当前线程的主上下文
  glfwMakeContextCurrent(window);

  // 初始化GLAD
  if gladLoadGL(TLoadProc(@glfwGetProcAddress)) = false then
  begin
    WriteLn('Failed to initialize GLAD');
    Exit(nil);
  end;

  // 设置窗口的维度(Dimension)
  glViewport(0, 0, SCR_WIDTH, SCR_HEIGHT);

  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

  // 注册一个回调函数(Callback Function)，它会在每次窗口大小被调整的时候被调用
  glfwSetFramebufferSizeCallback(window, @Framebuffer_size_callback);
  glfwSetCursorPosCallback(window, @Mouse_callback);
  glfwSetScrollCallback(window, @Scroll_callback);

  Result := window;
end;

function LoadTexture(fileName: string; gammaCorrection: boolean; inverse: boolean): cardinal;
var
  texture_ID: GLuint;
  tx: TTexture;
  tx_managed: IInterface;
  internalFormat: GLenum;
begin
  texture_ID := GLuint(0);
  glGenTextures(1, @texture_ID);
  internalFormat := GLenum(0);

  tx_managed := IInterface(TTexture.Create);
  tx := tx_managed as TTexture;

  tx.LoadFormFile(fileName, inverse);

  if gammaCorrection then
    internalFormat := GL_SRGB
  else
    internalFormat := GL_RGBA;

  glBindTexture(GL_TEXTURE_2D, texture_ID);
  glTexImage2D(GL_TEXTURE_2D, 0, internalFormat, tx.Width, tx.Height, 0, GL_RGBA,
    GL_UNSIGNED_BYTE, tx.Pixels);
  glGenerateMipmap(GL_TEXTURE_2D);

  if tx.UseAlpha then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end
  else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  end;

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  Result := texture_ID;
end;

procedure RenderQuad;
var
  quadVertices: TArr_GLfloat;
begin
  if quadVAO = 0 then
  begin
    quadVertices := TArr_GLfloat([
      // positions        // texture Coords
      -1.0,  1.0, 0.0,    0.0, 1.0,
      -1.0, -1.0, 0.0,    0.0, 0.0,
       1.0,  1.0, 0.0,    1.0, 1.0,
       1.0, -1.0, 0.0,    1.0, 0.0]);

    // setup plane VAO
    glGenVertexArrays(1, @quadVAO);
    glGenBuffers(1, @quadVBO);
    glBindVertexArray(quadVAO);
    glBindBuffer(GL_ARRAY_BUFFER, quadVBO);
    glBufferData(GL_ARRAY_BUFFER, quadVertices.MemSize, @quadVertices[0], GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));
  end;

  glBindVertexArray(quadVAO);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  glBindVertexArray(0);
end;

procedure RenderCube;
var
  vertices: TArr_GLfloat;
begin
  if cubeVAO = 0 then
  begin
    vertices := TArr_GLfloat([
      // back face
      -1.0, -1.0, -1.0,  0.0,  0.0, -1.0, 0.0, 0.0, // bottom-left
       1.0,  1.0, -1.0,  0.0,  0.0, -1.0, 1.0, 1.0, // top-right
       1.0, -1.0, -1.0,  0.0,  0.0, -1.0, 1.0, 0.0, // bottom-right
       1.0,  1.0, -1.0,  0.0,  0.0, -1.0, 1.0, 1.0, // top-right
      -1.0, -1.0, -1.0,  0.0,  0.0, -1.0, 0.0, 0.0, // bottom-left
      -1.0,  1.0, -1.0,  0.0,  0.0, -1.0, 0.0, 1.0, // top-left
      // front face
      -1.0, -1.0,  1.0,  0.0,  0.0,  1.0, 0.0, 0.0, // bottom-left
       1.0, -1.0,  1.0,  0.0,  0.0,  1.0, 1.0, 0.0, // bottom-right
       1.0,  1.0,  1.0,  0.0,  0.0,  1.0, 1.0, 1.0, // top-right
       1.0,  1.0,  1.0,  0.0,  0.0,  1.0, 1.0, 1.0, // top-right
      -1.0,  1.0,  1.0,  0.0,  0.0,  1.0, 0.0, 1.0, // top-left
      -1.0, -1.0,  1.0,  0.0,  0.0,  1.0, 0.0, 0.0, // bottom-left
      // left face
      -1.0,  1.0,  1.0, -1.0,  0.0,  0.0, 1.0, 0.0, // top-right
      -1.0,  1.0, -1.0, -1.0,  0.0,  0.0, 1.0, 1.0, // top-left
      -1.0, -1.0, -1.0, -1.0,  0.0,  0.0, 0.0, 1.0, // bottom-left
      -1.0, -1.0, -1.0, -1.0,  0.0,  0.0, 0.0, 1.0, // bottom-left
      -1.0, -1.0,  1.0, -1.0,  0.0,  0.0, 0.0, 0.0, // bottom-right
      -1.0,  1.0,  1.0, -1.0,  0.0,  0.0, 1.0, 0.0, // top-right
      // right face
       1.0,  1.0,  1.0,  1.0,  0.0,  0.0, 1.0, 0.0, // top-left
       1.0, -1.0, -1.0,  1.0,  0.0,  0.0, 0.0, 1.0, // bottom-right
       1.0,  1.0, -1.0,  1.0,  0.0,  0.0, 1.0, 1.0, // top-right
       1.0, -1.0, -1.0,  1.0,  0.0,  0.0, 0.0, 1.0, // bottom-right
       1.0,  1.0,  1.0,  1.0,  0.0,  0.0, 1.0, 0.0, // top-left
       1.0, -1.0,  1.0,  1.0,  0.0,  0.0, 0.0, 0.0, // bottom-left
      // bottom face
      -1.0, -1.0, -1.0,  0.0, -1.0,  0.0, 0.0, 1.0, // top-right
       1.0, -1.0, -1.0,  0.0, -1.0,  0.0, 1.0, 1.0, // top-left
       1.0, -1.0,  1.0,  0.0, -1.0,  0.0, 1.0, 0.0, // bottom-left
       1.0, -1.0,  1.0,  0.0, -1.0,  0.0, 1.0, 0.0, // bottom-left
      -1.0, -1.0,  1.0,  0.0, -1.0,  0.0, 0.0, 0.0, // bottom-right
      -1.0, -1.0, -1.0,  0.0, -1.0,  0.0, 0.0, 1.0, // top-right
      // top face
      -1.0,  1.0, -1.0,  0.0,  1.0,  0.0, 0.0, 1.0, // top-left
       1.0,  1.0 , 1.0,  0.0,  1.0,  0.0, 1.0, 0.0, // bottom-right
       1.0,  1.0, -1.0,  0.0,  1.0,  0.0, 1.0, 1.0, // top-right
       1.0,  1.0,  1.0,  0.0,  1.0,  0.0, 1.0, 0.0, // bottom-right
      -1.0,  1.0, -1.0,  0.0,  1.0,  0.0, 0.0, 1.0, // top-left
      -1.0,  1.0,  1.0,  0.0,  1.0,  0.0, 0.0, 0.0  // bottom-left
      ]);

    glGenVertexArrays(1, @cubeVAO);
    glGenBuffers(1, @cubeVBO);

    // fill buffer
    glBindBuffer(GL_ARRAY_BUFFER, cubeVBO);
    glBufferData(GL_ARRAY_BUFFER, vertices.MemSize, @vertices[0], GL_STATIC_DRAW);

    // link vertex attributes
    glBindVertexArray(cubeVAO);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(6 * SIZE_OF_F));
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
  end;

  // render Cube
  glBindVertexArray(cubeVAO);
  glDrawArrays(GL_TRIANGLES, 0, 36);
  glBindVertexArray(0);
end;

procedure Framebuffer_size_callback(window: PGLFWwindow; witdth, Height: integer); cdecl;
begin
  //确保视口匹配新的窗口尺寸;注意宽度和
  //高度将明显大于视网膜显示器上的指定。
  glViewport(0, 0, witdth, Height);
end;

procedure ProcessInput(window: PGLFWwindow);
begin
  if glfwGetKey(window, GLFW_KEY_ESCAPE) = GLFW_PRESS then
    glfwSetWindowShouldClose(window, true.ToInteger);

  if glfwGetKey(window, GLFW_KEY_W) = GLFW_PRESS then
    camera.ProcessKeyboard(TCamera_Movement.FORWARD, deltaTime);
  if glfwGetKey(window, GLFW_KEY_S) = GLFW_PRESS then
    camera.ProcessKeyboard(TCamera_Movement.BACKWARD, deltaTime);
  if glfwGetKey(window, GLFW_KEY_A) = GLFW_PRESS then
    camera.ProcessKeyboard(TCamera_Movement.LEFT, deltaTime);
  if glfwGetKey(window, GLFW_KEY_D) = GLFW_PRESS then
    camera.ProcessKeyboard(TCamera_Movement.RIGHT, deltaTime);
end;

procedure Mouse_callback(window: PGLFWwindow; xpos, ypos: double); cdecl;
var
  xoffset, yoffset: GLfloat;
begin
  if firstMouse then
  begin
    lastX := xpos;
    lastY := ypos;
    firstMouse := false;
  end;

  xoffset := GLfloat(xpos - lastX);
  yoffset := GLfloat(lastY - ypos);
  lastX := xpos;
  lastY := ypos;

  camera.ProcessMouseMovement(xoffset, yoffset);
end;

procedure Scroll_callback(window: PGLFWwindow; xoffset, yoffset: double); cdecl;
begin
  camera.ProcessMouseScroll(yoffset);
end;

end.
