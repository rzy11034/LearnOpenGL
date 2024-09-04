unit Case05_03_02_01_Point_Shadows;

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

  shadows: Boolean = false;
  shadowsKeyPressed: Boolean = false;

  planeVAO, planeVBO: GLuint;

procedure Main;
const
  dir_path = '..\Source\5.Advanced_Lighting\3.2.1.Point_Shadows\';
  point_shadows_vs = dir_path + '3.2.1.point_shadows.vs';
  point_shadows_fs = dir_path + '3.2.1.point_shadows.fs';
  point_shadows_depth_vs = dir_path + '3.2.1.point_shadows_depth.vs';
  point_shadows_depth_fs = dir_path + '3.2.1.point_shadows_depth.fs';
  point_shadows_depth_gs = dir_path + '3.2.1.point_shadows_depth.gs';
  img_wood = '..\Resources\textures\wood.png';
var
  window: PGLFWwindow;
  currentFrame: GLfloat;
  cubeVAO, cubeVBO: GLuint;
  projection, view, shadowProj: TMat4;
  woodTexture, depthMapFBO, depthMap, depthCubemap: Cardinal;
  simpleDepthShader, shader: TShaderProgram;
  lightPos, center, up: TVec3;
  far_plane, near_plane: float;
  shader_managed, simpleDepthShader_managed, camera_managed, shadowTransforms_managed: IInterface;
  shadowTransforms: TArrayList_TMat4;
  i: Integer;
  indexStr: string;
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
   glEnable(GL_CULL_FACE);

  //═════════════════════════════════════════════════════════════════════════

  shader_managed := IInterface(TShaderProgram.Create);
  shader := shader_managed as TShaderProgram;
  shader.LoadShaderFile(point_shadows_vs, point_shadows_fs);

  simpleDepthShader_managed := IInterface(TShaderProgram.Create);
  simpleDepthShader := simpleDepthShader_managed as TShaderProgram;
  simpleDepthShader.LoadShaderFile(point_shadows_depth_vs, point_shadows_depth_fs,
    point_shadows_depth_gs);

  camera_managed := IInterface(TCamera.Create(TGLM.Vec3(0, 0, 3)));
  camera := camera_managed as TCamera;

  //═════════════════════════════════════════════════════════════════════════

  // load textures
  woodTexture := LoadTexture(img_wood);

  //(*═══════════════════════════════════════════════════════════════════════
  // configure depth map FBO
  depthMapFBO := cardinal(0);
  glGenFramebuffers(1, @depthMapFBO);

  // create depth texture
  depthMap := cardinal(0);
  glGenTextures(1, @depthMap);

  // create depth cubemap texture
  depthCubemap := Cardinal(0);
  glGenTextures(1, @depthCubemap);
  glBindTexture(GL_TEXTURE_CUBE_MAP, depthCubemap);

  for i := 0 to 5 do
  begin
    glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_DEPTH_COMPONENT,
      SHADOW_WIDTH, SHADOW_HEIGHT, 0, GL_DEPTH_COMPONENT, GL_FLOAT, nil);
  end;

  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

  // attach depth texture as FBO's depth buffer
  glBindFramebuffer(GL_FRAMEBUFFER, depthMapFBO);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, depthMap, 0);
  glDrawBuffer(GL_NONE);
  glReadBuffer(GL_NONE);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  //═══════════════════════════════════════════════════════════════════════*)

  // shader configuration
  shader.UseProgram;
  shader.SetUniformInt('diffuseTexture', 0);
  shader.SetUniformInt('depthMap', 1);

  //═════════════════════════════════════════════════════════════════════════

  // lighting info
  lightPos := TGLM.Vec3(0.0, 0.0, 0.0);

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

    // move light position over time
    lightPos.z := Sin(glfwGetTime * 0.5) * 3.0;

    // render
    glClearColor(0.1, 0.1, 0.1, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    // 0. 创建深度立方体映射变换矩阵
    near_plane := float(1.0);
    far_plane := float(25.0);

    shadowProj := TGLM.Perspective(TGLM.Radians(90), SHADOW_WIDTH / SHADOW_HEIGHT,
      near_plane, far_plane);

    shadowTransforms_managed := IInterface(TArrayList_TMat4.Create);
    shadowTransforms := shadowTransforms_managed as TArrayList_TMat4;

    center := lightPos + TGLM.Vec3( 1.0,  0.0,  0.0);
    up := TGLM.Vec3(0.0, -1.0,  0.0);
    shadowTransforms.AddLast(shadowProj * TGLM.LookAt(lightPos, center, up));

    center := lightPos + TGLM.Vec3(-1.0,  0.0,  0.0);
    up := TGLM.Vec3(0.0, -1.0,  0.0);
    shadowTransforms.AddLast(shadowProj * TGLM.LookAt(lightPos, center, up));

    center := lightPos + TGLM.Vec3( 0.0,  1.0,  0.0);
    up := TGLM.Vec3(0.0,  0.0,  1.0);
    shadowTransforms.AddLast(shadowProj * TGLM.LookAt(lightPos, center, up));

    center := lightPos + TGLM.Vec3( 0.0, -1.0,  0.0);
    up := TGLM.Vec3(0.0,  0.0, -1.0);
    shadowTransforms.AddLast(shadowProj * TGLM.LookAt(lightPos, center, up));

    center := lightPos + TGLM.Vec3( 0.0,  0.0,  1.0);
    up := TGLM.Vec3(0.0, -1.0,  0.0);
    shadowTransforms.AddLast(shadowProj * TGLM.LookAt(lightPos, center, up));

    center := lightPos + TGLM.Vec3( 0.0,  0.0, -1.0);
    up := TGLM.Vec3(0.0, -1.0,  0.0);
    shadowTransforms.AddLast(shadowProj * TGLM.LookAt(lightPos, center, up));

    // 1. 渲染场景到深度立方体贴图
    glViewport(0, 0, SHADOW_WIDTH, SHADOW_HEIGHT);
    glBindFramebuffer(GL_FRAMEBUFFER, depthMapFBO);
        glClear(GL_DEPTH_BUFFER_BIT);
        simpleDepthShader.UseProgram;
        for i := 0 to 5 do
        begin
          indexStr := string(i.ToString);

          simpleDepthShader.SetUniformMatrix4fv('shadowMatrices[' + indexStr + ']',
            shadowTransforms[i]);
        end;
        simpleDepthShader.SetUniformFloat('far_plane', far_plane);
        simpleDepthShader.SetUniformVec3('lightPos', lightPos);
        RenderScene(simpleDepthShader);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // 2. 正常渲染场景
    glViewport(0, 0, SCR_WIDTH, SCR_HEIGHT);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    shader.UseProgram;

    projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 100.0);
    view := camera.GetViewMatrix;
    shader.SetUniformMatrix4fv('projection', projection);
    shader.SetUniformMatrix4fv('view', view);

    // set lighting uniforms
    shader.SetUniformVec3('lightPos', lightPos);
    shader.SetUniformVec3('viewPos', camera.Position);

    //按空格键启用/禁用阴影
    shader.SetUniformInt('shadows', shadows.ToInteger);
    shader.SetUniformFloat('far_plane', far_plane);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, woodTexture);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_CUBE_MAP, depthCubemap);
    RenderScene(shader);

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
  tx_managed: IInterface;
begin
  texture_ID := GLuint(0);
  glGenTextures(1, @texture_ID);

  tx_managed := IInterface(TTexture.Create);
  tx := tx_managed as TTexture;

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
end;

procedure RenderScene(const shader: TShaderProgram);
var
  model: TMat4;
begin
  // room cube
  model := TGLM.Mat4(1.0);
  model := TGLM.Scale(model, TGLM.Vec3(5.0));
  shader.SetUniformMatrix4fv('model', model);
  // 注意，我们在这里禁用了剔除，因为我们渲染立方体的“内部”而不是通常的“外部”，
  // 这会抛出正常的剔除方法。
  glDisable(GL_CULL_FACE);
  // 当从内部绘制立方体时，一个小技巧来反转法线，这样照明仍然可以工作。
  shader.SetUniformInt('reverse_normals', 1);
  RenderCube;
  shader.SetUniformInt('reverse_normals', 0);
  glEnable(GL_CULL_FACE);

  // cubes
  model := TGLM.Mat4(1.0);
  model := TGLM.Translate(model, TGLM.Vec3(4.0, -3.5, 0.0));
  model := TGLM.Scale(model, TGLM.Vec3(0.5));
  shader.SetUniformMatrix4fv('model', model);
  RenderCube;

  model := TGLM.Mat4(1.0);
  model := TGLM.Translate(model, TGLM.Vec3(2.0, 3.0, 1.0));
  model := TGLM.Scale(model, TGLM.Vec3(0.75));
  shader.SetUniformMatrix4fv('model', model);
  RenderCube;

  model := TGLM.Mat4(1.0);
  model := TGLM.Translate(model, TGLM.Vec3(-3.0, -1.0, 0.0));
  model := TGLM.Scale(model, TGLM.Vec3(0.5));
  shader.SetUniformMatrix4fv('model', model);
  RenderCube;

  model := TGLM.Mat4(1.0);
  model := TGLM.Translate(model, TGLM.Vec3(-1.5, 1.0, 1.5));
  model := TGLM.Scale(model, TGLM.Vec3(0.5));
  shader.SetUniformMatrix4fv('model', model);
  RenderCube;

  model := TGLM.Mat4(1.0);
  model := TGLM.Translate(model, TGLM.Vec3(-1.5, 2.0, -3.0));
  model := TGLM.Rotate(model, TGLM.Radians(60), TGLM.Normalize(TGLM.Vec3(1.0, 0.0, 1.0)));
  model := TGLM.Scale(model, TGLM.Vec3(0.75));
  shader.SetUniformMatrix4fv('model', model);
  RenderCube;
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

  if (glfwGetKey(window, GLFW_KEY_W) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.FORWARD, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_S) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.BACKWARD, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_A) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.LEFT, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_D) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.RIGHT, deltaTime);

  if (glfwGetKey(window, GLFW_KEY_SPACE) = GLFW_PRESS) and (not shadowsKeyPressed) then
  begin
    shadows := not shadows;
    shadowsKeyPressed := true;
  end;

  if glfwGetKey(window, GLFW_KEY_SPACE) = GLFW_RELEASE then
  begin
    shadowsKeyPressed := false;
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
