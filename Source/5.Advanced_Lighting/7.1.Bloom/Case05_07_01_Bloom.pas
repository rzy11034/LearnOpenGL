unit Case05_07_01_Bloom;

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

  bloom: Boolean = true;
  bloomKeyPressed: Boolean = false;
  exposure: float = 1.0;

  cubeVAO: Cardinal = 0;
  cubeVBO: Cardinal = 0;

  quadVAO: Cardinal = 0;
  quadVBO: Cardinal = 0;

procedure Main;
const
  shader_path = '..\Source\5.Advanced_Lighting\7.1.Bloom\';

  bloom_vs = shader_path + '7.bloom.vs';
  bloom_fs = shader_path + '7.bloom.fs';

  blur_vs = shader_path + '7.blur.vs';
  blur_fs = shader_path + '7.blur.fs';

  light_box_fs = shader_path + '7.light_box.fs';

  bloom_final_vs = shader_path + '7.bloom_final.vs';
  bloom_final_fs = shader_path + '7.bloom_final.fs';

  img_wood = '..\Resources\textures\wood.png';
  img_containerTexture = '..\Resources\textures\container2.png';
var
  window: PGLFWwindow;
  currentFrame: GLfloat;
  projection, view, model: TMat4;
  hdrFBO, rboDepth, woodTexture, containerTexture, amount: Cardinal;
  shader, shaderLight, shaderBlur, shaderBloomFinal: TShaderProgram;
  shader_managed, camera_managed: IInterface;
  lightPosition_managed, lightColors_managed, shaderLight_managed: IInterface;
  shaderBlur_managed, shaderBloomFinal_managed: IInterface;
  lightPositions, lightColors: TArrayList_TVec3;
  i: Integer;
  colorBuffers, attachments, pingpongFBO, pingpongColorbuffers: TArr_GLuint;
  first_iteration, horizontal: Boolean;
begin
  window := InitWindows;
  if window = nil then
  begin
    glfwTerminate;
    Exit;
  end;

  //═════════════════════════════════════════════════════════════════════════

  // configure global opengl state
  glEnable(GL_DEPTH_TEST);

  //═════════════════════════════════════════════════════════════════════════

  shader_managed := IInterface(TShaderProgram.Create);
  shader := shader_managed as TShaderProgram;
  shader.LoadShaderFile(bloom_vs, bloom_fs);

  shaderLight_managed := IInterface(TShaderProgram.Create);
  shaderLight := shaderLight_managed as TShaderProgram;
  shaderLight.LoadShaderFile(bloom_vs, light_box_fs);

  shaderBlur_managed := IInterface(TShaderProgram.Create);
  shaderBlur := shaderBlur_managed as TShaderProgram;
  shaderBlur.LoadShaderFile(blur_vs, blur_fs);

  shaderBloomFinal_managed := IInterface(TShaderProgram.Create);
  shaderBloomFinal := shaderBloomFinal_managed as TShaderProgram;
  shaderBloomFinal.LoadShaderFile(bloom_final_vs, bloom_final_fs);

  //═════════════════════════════════════════════════════════════════════════

  camera_managed := IInterface(TCamera.Create(TGLM.Vec3(0, 0, 5)));
  camera := camera_managed as TCamera;

  //═════════════════════════════════════════════════════════════════════════

  // load textures
  // 请注意，我们将纹理加载为SRGB纹理
  woodTexture := LoadTexture(img_wood, true);
  containerTexture := LoadTexture(img_containerTexture, true);

  //═════════════════════════════════════════════════════════════════════════

  // 配置浮点帧缓冲区
  hdrFBO := Cardinal(0);
  glGenFramebuffers(1, @hdrFBO);
  glBindFramebuffer(GL_FRAMEBUFFER, hdrFBO);

  //═════════════════════════════════════════════════════════════════════════

  // 创建2个浮点颜色缓冲区(一个用于正常渲染，另一个用于亮度阈值)
  colorBuffers := TArr_GLuint([0, 0]);
  glGenTextures(2, @colorBuffers[0]);

  for i := 0 to High(colorBuffers) do
  begin
    glBindTexture(GL_TEXTURE_2D, colorBuffers[i]);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGBA, GL_FLOAT, nil);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    // 我们钳住边缘，否则模糊滤镜会采样重复的纹理值!
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    // 将纹理附加到framebuffer
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + i, GL_TEXTURE_2D, colorBuffers[i], 0);
  end;

  //═════════════════════════════════════════════════════════════════════════

  //创建并附加深度缓冲区(renderbuffer)
  rboDepth := Cardinal(0);
  glGenRenderbuffers(1, @rboDepth);
  glBindRenderbuffer(GL_RENDERBUFFER, rboDepth);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, SCR_WIDTH, SCR_HEIGHT);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, rboDepth);

  //═════════════════════════════════════════════════════════════════════════

  //告诉OpenGL我们将使用(这个framebuffer的)哪个颜色附件进行渲染
  attachments := TArr_GLuint([GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1]);
  glDrawBuffers(2, @attachments[0]);

  // finally check if framebuffer is complete
  if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
      WriteLn('Framebuffer not complete!');

  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  //═════════════════════════════════════════════════════════════════════════

  // ping-pong-framebuffer for blurring
  pingpongFBO := TArr_GLuint([0, 0]);
  pingpongColorbuffers := TArr_GLuint([0, 0]);

  glGenFramebuffers(2, @pingpongFBO[0]);
  glGenTextures(2, @pingpongColorbuffers[0]);

  for i := 0 to 1 do
  begin
    glBindFramebuffer(GL_FRAMEBUFFER, pingpongFBO[i]);
    glBindTexture(GL_TEXTURE_2D, pingpongColorbuffers[i]);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGBA, GL_FLOAT, nil);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    //我们钳住边缘，否则模糊滤镜会采样重复的纹理值!
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D,
      pingpongColorbuffers[i], 0);

    // 还要检查framebuffers是否完成(不需要深度缓冲区)
    if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
        WriteLn('Framebuffer not complete!');
  end;

  //═════════════════════════════════════════════════════════════════════════
  // lighting info

  // position
  lightPosition_managed := IInterface(TArrayList_TVec3.Create);
  lightPositions := lightPosition_managed as TArrayList_TVec3;
  lightPositions.AddLast(TGLM.Vec3( 0.0, 0.5,  1.5));
  lightPositions.AddLast(TGLM.Vec3(-4.0, 0.5, -3.0));
  lightPositions.AddLast(TGLM.Vec3( 3.0, 0.5,  1.0));
  lightPositions.AddLast(TGLM.Vec3(-0.8, 2.4, -1.0));

  // colors
  lightColors_managed := IInterface(TArrayList_TVec3.Create);
  lightColors := lightColors_managed as TArrayList_TVec3;
  lightColors.AddFirst(TGLM.Vec3( 5.0, 5.0,  5.0));
  lightColors.AddFirst(TGLM.Vec3(10.0, 0.0,  0.0));
  lightColors.AddFirst(TGLM.Vec3( 0.0, 0.0, 15.0));
  lightColors.AddFirst(TGLM.Vec3( 0.0, 5.0,  0.0));

  //═════════════════════════════════════════════════════════════════════════

  // shader configuration
  shader.UseProgram;
  shader.SetUniformInt('diffuseTexture', 0);

  shaderBlur.UseProgram;
  shaderBlur.SetUniformInt('image', 0);

  shaderBloomFinal.UseProgram;
  shaderBloomFinal.SetUniformInt('scene', 0);
  shaderBloomFinal.SetUniformInt('bloomBlur', 1);

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

    // 1. 渲染场景到浮点帧缓冲区
    glBindFramebuffer(GL_FRAMEBUFFER, hdrFBO);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT,
      0.1, 100.0);
    view := camera.GetViewMatrix;

    shader.UseProgram;
    shader.SetUniformMatrix4fv('projection', projection);
    shader.SetUniformMatrix4fv('view', view);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, woodTexture);

    // 设置 lighting uniforms
    for i := 0 to lightPositions.Count - 1 do
    begin
      shader.SetUniformVec3('lights[' + i.ToString + '].Position', lightPositions[i]);
      shader.SetUniformVec3('lights[' + i.ToString + '].Color', lightColors[i]);
    end;
    shader.SetUniformVec3('viewPos', camera.Position);

    // 创建一个大的立方体作为地板
    model := TGLM.mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(0.0, -1.0, 0.0));
    model := TGLM.Scale(model, TGLM.Vec3(12.5, 0.5, 12.5));
    shader.SetUniformMatrix4fv('model', model);
    RenderCube();

    // 然后创建多个立方体作为场景，然后创建多个立方体作为场景
    glBindTexture(GL_TEXTURE_2D, containerTexture);
    model := TGLM.mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(0.0, 1.5, 0.0));
    model := TGLM.Scale(model, TGLM.Vec3(0.5));
    shader.SetUniformMatrix4fv('model', model);
    RenderCube;

    model := TGLM.mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(2.0, 0.0, 1.0));
    model := TGLM.Scale(model, TGLM.Vec3(0.5));
    shader.SetUniformMatrix4fv('model', model);
    RenderCube;

    model := TGLM.mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(-1.0, -1.0, 2.0));
    model := TGLM.Rotate(model, TGLM.Radians(60.0), TGLM.Normalize(TGLM.Vec3(1.0, 0.0, 1.0)));
    shader.SetUniformMatrix4fv('model', model);
    RenderCube;

    model := TGLM.mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(0.0, 2.7, 4.0));
    model := TGLM.Rotate(model, TGLM.Radians(23.0), TGLM.Normalize(TGLM.Vec3(1.0, 0.0, 1.0)));
    model := TGLM.Scale(model, TGLM.Vec3(1.25));
    shader.SetUniformMatrix4fv('model', model);
    RenderCube;

    model := TGLM.mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(-2.0, 1.0, -3.0));
    model := TGLM.Rotate(model, TGLM.Radians(124.0), TGLM.Normalize(TGLM.Vec3(1.0, 0.0, 1.0)));
    shader.SetUniformMatrix4fv('model', model);
    RenderCube;

    model := TGLM.mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(-3.0, 0.0, 0.0));
    model := TGLM.Scale(model, TGLM.Vec3(0.5));
    shader.SetUniformMatrix4fv('model', model);
    RenderCube;

    // 最后将所有光源显示为明亮的立方体
    shaderLight.UseProgram;
    shaderLight.SetUniformMatrix4fv('projection', projection);
    shaderLight.SetUniformMatrix4fv('view', view);

    for i := 0 to lightPositions.Count - 1 do
    begin
      model := TGLM.Mat4(1.0);
      model := TGLM.Translate(model, lightPositions[i]);
      model := TGLM.Scale(model, TGLM.Vec3(0.25));
      shaderLight.SetUniformMatrix4fv('model', model);
      shaderLight.SetUniformVec3('lightColor', lightColors[i]);
      RenderCube;
    end;
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // 2 模糊明亮的碎片与两次高斯模糊
    horizontal := true;
    first_iteration := true;
    amount := Cardinal(10);

    shaderBlur.UseProgram;

    for i := 0 to amount - 1 do
    begin
      glBindFramebuffer(GL_FRAMEBUFFER, pingpongFBO[horizontal.ToInteger]);
      shaderBlur.SetUniformInt('horizontal', horizontal.ToInteger);
      // 绑定其他帧缓冲区的纹理(或者第一次迭代时绑定场景)
      glBindTexture(GL_TEXTURE_2D, IfThen(first_iteration, colorBuffers[1] ,
        pingpongColorbuffers[(not horizontal).ToInteger]));
      renderQuad;
      horizontal := not horizontal;
      if first_iteration then
        first_iteration := false;
    end;
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // 3. 现在将浮点颜色缓冲区渲染为2D四边形，
    // 并将 HDR 颜色色调映射为默认 framebuffer 的(固定)颜色范围
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    shaderBloomFinal.UseProgram;
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, colorBuffers[0]);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, pingpongColorbuffers[(not horizontal).ToInteger]);
    shaderBloomFinal.SetUniformInt('bloom', bloom.ToInteger);
    shaderBloomFinal.SetUniformFloat('exposure', exposure);
    RenderQuad;

    WriteLn('bloom: ', IfThen(bloom, 'on', 'off'), '| exposure: ', exposure);

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

  if (glfwGetKey(window, GLFW_KEY_SPACE) = GLFW_PRESS) and (not bloomKeyPressed) then
  begin
    bloom := not bloom;
    bloomKeyPressed := true;
  end
  else if glfwGetKey(window, GLFW_KEY_SPACE) = GLFW_RELEASE then
  begin
    bloomKeyPressed := false;
  end;

  if glfwGetKey(window, GLFW_KEY_Q) = GLFW_PRESS then
  begin
    if exposure > 0.0 then
      exposure -= 0.001
    else
      exposure := 0.0;
  end
  else if glfwGetKey(window, GLFW_KEY_E) = GLFW_PRESS then
  begin
    exposure += 0.001;
  end;
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
