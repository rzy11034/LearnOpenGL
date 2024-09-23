unit Case05_03_01_03_Shadow_Mapping;

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
function LoadTexture(fileName: string; inverse: boolean = true): cardinal; forward;

procedure RenderScene(const shader: TShaderProgram); forward;
procedure RenderCube; forward;
procedure RenderQuad; forward;

const
  SCR_WIDTH = 800;
  SCR_HEIGHT = 600;
  SHADOW_WIDTH = 1024;
  SHADOW_HEIGHT = 1024;

var
  camera: TCamera;

  deltaTime: float = 0.0;  // time between current frame and last frame
  lastFrame: float = 0.0;

  firstMouse: boolean = true;
  //偏航被初始化为-90.0度，因为0.0的偏航导致一个指向右的方向矢量，所以我们最初
  //向左旋转一点。
  lastX: float = SCR_WIDTH / 2.0;
  lastY: float = SCR_HEIGHT / 2.0;

  cubeVAO: GLuint = 0;
  cubeVBO: GLuint = 0;

  quadVAO: GLuint = 0;
  quadVBO: GLuint = 0;

  planeVAO, planeVBO: GLuint;

procedure Main;
const
  shader_path = '..\Source\5.Advanced_Lighting\3.1.3.Shadow_Mapping\';
  debug_quad_vs = shader_path + '3.1.3.debug_quad.vs';
  debug_quad_depth_fs = shader_path + '3.1.3.debug_quad_depth.fs';
  shadow_mapping_vs = shader_path + '3.1.3.shadow_mapping.vs';
  shadow_mapping_fs = shader_path + '3.1.3.shadow_mapping.fs';
  shadow_mapping_depth_vs = shader_path + '3.1.3.shadow_mapping_depth.vs';
  shadow_mapping_depth_fs = shader_path + '3.1.3.shadow_mapping_depth.fs';
  img_wood = '..\Resources\textures\wood.png';
var
  window: PGLFWwindow;
  currentFrame: GLfloat;
  planeVertices: TArr_GLfloat;
  cubeVAO, cubeVBO: GLuint;
  lightProjection, lightView, lightSpaceMatrix, projection, view: TMat4;
  woodTexture, depthMapFBO, depthMap: Cardinal;
  simpleDepthShader, debugDepthQuad, shader: TShaderProgram;
  lightPos: TVec3;
  far_plane, near_plane: float;
  {%H-}simpleDepthShader_managed: IInterface;
  {%H-}debugDepthQuad_managed: IInterface;
  {%H-}camera__managed: IInterface;
  {%H-}shader_managed: IInterface;
begin
  window := InitWindows;
  if window = nil then
  begin
    glfwTerminate;
    Exit;
  end;

  //═════════════════════════════════════════════════════════════════════════

   glEnable(GL_DEPTH_TEST);

  //═════════════════════════════════════════════════════════════════════════

  shader := TShaderProgram.Create;
  shader_managed := shader as IInterface;
  shader.LoadShaderFile(shadow_mapping_vs, shadow_mapping_fs);

  simpleDepthShader := TShaderProgram.Create;
  simpleDepthShader_managed := simpleDepthShader as IInterface;
  simpleDepthShader.LoadShaderFile(shadow_mapping_depth_vs, shadow_mapping_depth_fs);

  debugDepthQuad := TShaderProgram.Create;
  debugDepthQuad_managed := debugDepthQuad as IInterface;
  debugDepthQuad.LoadShaderFile(debug_quad_vs, debug_quad_depth_fs);

  camera := TCamera.Create(TGLM.Vec3(0, 0, 3));
  camera__managed := camera as IInterface;

  //═════════════════════════════════════════════════════════════════════════

  planeVertices := TArr_GLfloat([
    // positions         // normals      // texcoords
     25.0, -0.5,  25.0,  0.0, 1.0, 0.0,  25.0,  0.0,
    -25.0, -0.5,  25.0,  0.0, 1.0, 0.0,   0.0,  0.0,
    -25.0, -0.5, -25.0,  0.0, 1.0, 0.0,   0.0, 25.0,

     25.0, -0.5,  25.0,  0.0, 1.0, 0.0,  25.0,  0.0,
    -25.0, -0.5, -25.0,  0.0, 1.0, 0.0,   0.0, 25.0,
     25.0, -0.5, -25.0,  0.0, 1.0, 0.0,  25.0, 25.0]);

  planeVAO := GLuint(0);
  planeVBO := GLuint(0);

  glGenVertexArrays(1, @planeVAO);
  glGenBuffers(1, @planeVBO);
  glBindVertexArray(planeVAO);
  glBindBuffer(GL_ARRAY_BUFFER, planeVBO);
  glBufferData(GL_ARRAY_BUFFER, planeVertices.MemSize, @planeVertices[0], GL_STATIC_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(0));
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));
  glEnableVertexAttribArray(2);
  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(6 * SIZE_OF_F));
  glBindVertexArray(0);

  //═════════════════════════════════════════════════════════════════════════

  // load textures
  woodTexture := LoadTexture(img_wood);

  //═════════════════════════════════════════════════════════════════════════

  // configure depth map FBO
  depthMapFBO := cardinal(0);
  glGenFramebuffers(1, @depthMapFBO);

  // create depth texture
  depthMap := cardinal(0);
  glGenTextures(1, @depthMap);

  glBindTexture(GL_TEXTURE_2D, depthMap);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, SHADOW_WIDTH, SHADOW_HEIGHT,
    0, GL_DEPTH_COMPONENT, GL_FLOAT, nil);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

  // attach depth texture as FBO's depth buffer
  glBindFramebuffer(GL_FRAMEBUFFER, depthMapFBO);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, depthMap, 0);
  glDrawBuffer(GL_NONE);
  glReadBuffer(GL_NONE);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  //═════════════════════════════════════════════════════════════════════════

  // shader configuration
  shader.UseProgram;
  shader.SetUniformInt('diffuseTexture', 0);
  shader.SetUniformInt('shadowMap', 1);

  debugDepthQuad.UseProgram;
  debugDepthQuad.SetUniformInt('depthMap', 0);

  //═════════════════════════════════════════════════════════════════════════

  // lighting info
  lightPos := TGLM.Vec3(-2.0, 4.0, -1.0);

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

    // change light position over time
    lightPos.x := Sin(glfwGetTime()) * 3.0;
    lightPos.z := Cos(glfwGetTime()) * 2.0;
    lightPos.y := 5.0 + cos(glfwGetTime()) * 1.0;

    // render
    glClearColor(0.1, 0.1, 0.1, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    // 1. 渲染场景到纹理的深度(从光的角度)
    near_plane := float(1.0);
    far_plane := float(7.5);

    lightProjection := TGLM.Ortho(-10.0, 10.0, -10.0, 10.0, near_plane, far_plane);
    lightView := TGLM.LookAt(lightPos, TGLM.Vec3(0), TGLM.Vec3(0, 1, 0));
    lightSpaceMatrix := lightProjection * lightView;

    // 从光的角度渲染场景
    simpleDepthShader.UseProgram;
    simpleDepthShader.SetUniformMatrix4fv('lightSpaceMatrix', lightSpaceMatrix);

    glViewport(0, 0, SHADOW_WIDTH, SHADOW_HEIGHT);
    glBindFramebuffer(GL_FRAMEBUFFER, depthMapFBO);
        glClear(GL_DEPTH_BUFFER_BIT);
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, woodTexture);
        RenderScene(simpleDepthShader);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // reset viewport
    glViewport(0, 0, SCR_WIDTH, SCR_HEIGHT);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    // 2.使用生成的深度/阴影贴图正常渲染场景
    shader.UseProgram;
    projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT,
      0.1, 100.0);
    view := camera.GetViewMatrix;
    shader.SetUniformMatrix4fv('projection', projection);
    shader.SetUniformMatrix4fv('view', view);

    // set light uniforms
    shader.SetUniformVec3('viewPos', camera.Position);
    shader.SetUniformVec3('lightPos', lightPos);
    shader.SetUniformMatrix4fv('lightSpaceMatrix', lightSpaceMatrix);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, woodTexture);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, depthMap);
    renderScene(shader);

    // 渲染深度图到quad进行可视化调试
    debugDepthQuad.UseProgram;
    debugDepthQuad.SetUniformFloat('near_plane', near_plane);
    debugDepthQuad.SetUniformFloat('far_plane', far_plane);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, depthMap);
    //RenderQuad;

    //═════════════════════════════════════════════════════════════════════════

    // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
    glfwSwapBuffers(window);
    glfwPollEvents;
  end;

  glDeleteVertexArrays(1, @planeVAO);
  glDeleteBuffers(1, @planeVBO);

  glDeleteVertexArrays(1, @cubeVAO);
  glDeleteBuffers(1, @cubeVBO);

  glDeleteVertexArrays(1, @quadVAO);
  glDeleteBuffers(1, @quadVBO);

  glDeleteFramebuffers(1, @depthMapFBO);

  glDeleteTextures(1, @depthMap);
  glDeleteTextures(1, @woodTexture);

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

function LoadTexture(fileName: string; inverse: boolean): cardinal;
var
  texture_ID: GLuint;
  tx: TTexture;
begin
  texture_ID := GLuint(0);
  glGenTextures(1, @texture_ID);

  tx := TTexture.Create();
  try
    tx.LoadFormFile(fileName, inverse);

    glBindTexture(GL_TEXTURE_2D, texture_ID);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tx.Width, tx.Height, 0, GL_RGBA,
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

  finally
    tx.Destroy;
  end;
end;

procedure RenderScene(const shader: TShaderProgram);
var
  model: TMat4;
begin
  // floor
  model := TGLM.Mat4_Identity;
  shader.SetUniformMatrix4fv('model', model);
  glBindVertexArray(planeVAO);
  glDrawArrays(GL_TRIANGLES, 0, 6);

  // cubes
  model := TGLM.Mat4(1.0);
  model := TGLM.Translate(model, TGLM.Vec3(0.0, 1.5, 0.0));
  model := TGLM.Scale(model, TGLM.Vec3(0.5));
  shader.SetUniformMatrix4fv('model', model);
  renderCube();

  model := TGLM.Mat4(1.0);
  model := TGLM.Translate(model, TGLM.Vec3(2.0, 0.0, 1.0));
  model := TGLM.Scale(model, TGLM.Vec3(0.5));
  shader.SetUniformMatrix4fv('model', model);
  renderCube();

  model := TGLM.Mat4(1.0);
  model := TGLM.Translate(model, TGLM.Vec3(-1.0, 0.0, 2.0));
  model := TGLM.Rotate(model, TGLM.Radians(60), TGLM.Normalize(TGLM.Vec3(1, 0, 1)));
  model := TGLM.Scale(model, TGLM.Vec3(0.25));
  shader.SetUniformMatrix4fv('model', model);
  renderCube();
end;

procedure RenderCube;
var
  vertices: TArr_GLfloat;
begin
  if cubeVAO = 0 then
  begin
    vertices := TArr_GLfloat([
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

  if (glfwGetKey(window, GLFW_KEY_W) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.FORWARD, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_S) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.BACKWARD, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_A) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.LEFT, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_D) = GLFW_PRESS) then
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
