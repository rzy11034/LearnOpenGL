unit Case05_08_02_Deferred_Shading_Volumes;

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
  Math,
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

procedure Main;
const
  shader_path = '..\Source\5.Advanced_Lighting\8.2.Deferred_Shading_Volumes\';

  deferred_shading_vs = shader_path + '8.2.deferred_shading.vs';
  deferred_shading_fs = shader_path + '8.2.deferred_shading.fs';

  deferred_light_box_vs = shader_path + '8.2.deferred_light_box.vs';
  deferred_light_box_fs = shader_path + '8.2.deferred_light_box.fs';

  g_buffer_vs = shader_path + '8.2.g_buffer.vs';
  g_buffer_fs = shader_path + '8.2.g_buffer.fs';

  ObjBackpack = '..\Resources\objects\backpack\backpack.obj';
var
  window: PGLFWwindow;
  shaderGeometryPass_managed, shaderLightingPass_managed, camera_managed,
    shaderLightBox_managed, backpack_managed, objectPositions_managed,
    lightPositions_managed, lightColors_managed: IInterface;
  shaderGeometryPass, shaderLightingPass, shaderLightBox: TShaderProgram;
  backpack: TModel;
  objectPositions, lightPositions, lightColors: TArrayList_TVec3;
  gBuffer, gPosition, gNormal, gAlbedoSpec, rboDepth, NR_LIGHTS: Cardinal;
  attachments: TArr_GLuint;
  i: Integer;
  xPos, yPos, zPos, rColor, gColor, bColor, maxBrightness, radius: Single;
  projection, view, model: TMat4;
  linear, quadratic, constant: float;
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
  shaderGeometryPass.LoadShaderFile(g_buffer_vs, g_buffer_fs);

  shaderLightingPass_managed := IInterface(TShaderProgram.Create);
  shaderLightingPass := shaderLightingPass_managed as TShaderProgram;
  shaderLightingPass.LoadShaderFile(deferred_shading_vs, deferred_shading_fs);

  shaderLightBox_managed := IInterface(TShaderProgram.Create);
  shaderLightBox := shaderLightBox_managed as TShaderProgram;
  shaderLightBox.LoadShaderFile(deferred_light_box_vs, deferred_light_box_fs);

  //═════════════════════════════════════════════════════════════════════════

  // load models
  backpack_managed := IInterface(TModel.Create(ObjBackpack));
  backpack := backpack_managed as TModel;

  //═════════════════════════════════════════════════════════════════════════

  objectPositions_managed := IInterface(TArrayList_TVec3.Create);
  objectPositions := objectPositions_managed as TArrayList_TVec3;

  objectPositions.AddLast(TGLM.Vec3(-3.0, -0.5, -3.0));
  objectPositions.AddLast(TGLM.Vec3( 0.0, -0.5, -3.0));
  objectPositions.AddLast(TGLM.Vec3( 3.0, -0.5, -3.0));
  objectPositions.AddLast(TGLM.Vec3(-3.0, -0.5,  0.0));
  objectPositions.AddLast(TGLM.Vec3( 0.0, -0.5,  0.0));
  objectPositions.AddLast(TGLM.Vec3( 3.0, -0.5,  0.0));
  objectPositions.AddLast(TGLM.Vec3(-3.0, -0.5,  3.0));
  objectPositions.AddLast(TGLM.Vec3( 0.0, -0.5,  3.0));
  objectPositions.AddLast(TGLM.Vec3( 3.0, -0.5,  3.0));

  //═════════════════════════════════════════════════════════════════════════

  // configure g-buffer framebuffer
  gBuffer := Cardinal(0);
  glGenFramebuffers(1, @gBuffer);
  glBindFramebuffer(GL_FRAMEBUFFER, gBuffer);

  gPosition := Cardinal(0);
  gNormal := Cardinal(0);
  gAlbedoSpec := Cardinal(0);

  // position color buffer
  glGenTextures(1, @gPosition);
  glBindTexture(GL_TEXTURE_2D, gPosition);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGBA, GL_FLOAT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, gPosition, 0);

  // normal color buffer
  glGenTextures(1, @gNormal);
  glBindTexture(GL_TEXTURE_2D, gNormal);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGBA, GL_FLOAT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, gNormal, 0);

  // color + specular color buffer
  glGenTextures(1, @gAlbedoSpec);
  glBindTexture(GL_TEXTURE_2D, gAlbedoSpec);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT2, GL_TEXTURE_2D, gAlbedoSpec, 0);

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

  // lighting info
  NR_LIGHTS := Cardinal(32);

  lightPositions_managed := IInterface(TArrayList_TVec3.Create);
  lightPositions := lightPositions_managed as TArrayList_TVec3;

  lightColors_managed := IInterface(TArrayList_TVec3.Create);
  lightColors := lightColors_managed as TArrayList_TVec3;

  Randomize;

  for i := 0 to NR_LIGHTS - 1 do
  begin
    // 计算稍微随机的偏移量
    xPos := (Random(100) / 100) * 6.0 - 3.0;
    yPos := (Random(100) / 100) * 6.0 - 4.0;
    zPos := (Random(100) / 100) * 6.0 - 3.0;
    lightPositions.AddLast(TGLM.Vec3(xPos, yPos, zPos));

    // 计算随机颜色
    rColor := (Random(100) / 200) + 0.5;
    gColor := (Random(100) / 200) + 0.5;
    bColor := (Random(100) / 200) + 0.5;
    lightColors.AddLast(TGLM.Vec3(rColor, gColor, bColor));
  end;

  //═════════════════════════════════════════════════════════════════════════

  // shader configuration
  shaderLightingPass.UseProgram;
  shaderLightingPass.SetUniformInt('gPosition', 0);
  shaderLightingPass.SetUniformInt('gNormal', 1);
  shaderLightingPass.SetUniformInt('gAlbedoSpec', 2);

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

    // 1. 几何传递:渲染场景的几何/颜色数据到gbuffer
    glBindFramebuffer(GL_FRAMEBUFFER, gBuffer);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      projection := TGLM.perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT,
        0.1, 100.0);
      view := camera.GetViewMatrix;
      model := TGLM.Mat4(1.0);
      shaderGeometryPass.UseProgram;
      shaderGeometryPass.SetUniformMatrix4fv('projection', projection);
      shaderGeometryPass.SetUniformMatrix4fv('view', view);
      for i := 0 to objectPositions.Count - 1 do
      begin
        model := TGLM.Mat4(1.0);
        model := TGLM.Translate(model, objectPositions[i]);
        model := TGLM.Scale(model, TGLM.Vec3(0.25));
        shaderGeometryPass.SetUniformMatrix4fv('model', model);
        backpack.Draw(shaderGeometryPass);
      end;
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // 2. 光照传递:使用gbuffer的内容，通过迭代填充在屏幕上的四像素来计算光照。
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    shaderLightingPass.UseProgram;
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, gPosition);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, gNormal);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, gAlbedoSpec);
    // 送光相关 uniforms
    for i := 0 to lightPositions.Count - 1 do
    begin
      shaderLightingPass.SetUniformVec3('lights[' + i.ToString + '].Position', lightPositions[i]);
      shaderLightingPass.SetUniformVec3('lights[' + i.ToString + '].Color', lightColors[i]);
      // 更新衰减参数并计算半径
      // 注意，我们没有将其发送到着色器，我们假设它总是1.0(在我们的情况下)。
      constant := float(1.0);
      linear := float(0.7);
      quadratic := float(1.8);
      shaderLightingPass.SetUniformFloat('lights[' + i.ToString + '].Linear', linear);
      shaderLightingPass.SetUniformFloat('lights[' + i.ToString + '].Quadratic', quadratic);

      // then calculate radius of light volume/sphere
      maxBrightness := Max(Max(lightColors[i].r, lightColors[i].g), lightColors[i].b);
      radius := (-linear + Sqrt(linear * linear - 4 * quadratic *
        (constant - (256.0 / 5.0) * maxBrightness))) / (2.0 * quadratic);
      shaderLightingPass.SetUniformFloat('lights[' + i.ToString + '].Radius', radius);
    end;
    shaderLightingPass.SetUniformVec3('viewPos', camera.Position);
    RenderQuad;

    // 2.5. 将geometry深度缓冲区的内容复制到默认framebuffer的深度缓冲区
    glBindFramebuffer(GL_READ_FRAMEBUFFER, gBuffer);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
    glBlitFramebuffer(0, 0, SCR_WIDTH, SCR_HEIGHT, 0, 0, SCR_WIDTH, SCR_HEIGHT, GL_DEPTH_BUFFER_BIT, GL_NEAREST);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // 3. 渲染灯光在场景的顶部
    shaderLightBox.UseProgram;
    shaderLightBox.SetUniformMatrix4fv('projection', projection);
    shaderLightBox.SetUniformMatrix4fv('view', view);
    for i := 0 to lightPositions.Count - 1 do
    begin
      model := TGLM.Mat4(1.0);
      model := TGLM.Translate(model, lightPositions[i]);
      model := TGLM.Scale(model, TGLM.Vec3(0.125));
      shaderLightBox.SetUniformMatrix4fv('model', model);
      shaderLightBox.SetUniformVec3('lightColor', lightColors[i]);
      RenderCube;
    end;

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
