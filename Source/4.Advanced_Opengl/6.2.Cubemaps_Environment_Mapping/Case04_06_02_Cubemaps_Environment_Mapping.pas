unit Case04_06_02_Cubemaps_Environment_Mapping;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.OpenGL.Texture,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.Camera;

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
// 从6个单独的纹理面加载一个立方体贴图纹理
// 顺序:
// +X (right)
// -X (left)
// +Y (top)
// -Y (bottom)
// +Z (front)
// -Z (back)
function LoadCubemap(faces: TArr_str; inverse: boolean = false): cardinal; forward;

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

procedure Main;
const
  fs = '..\Source\4.Advanced_Opengl\6.2.Cubemaps_Environment_Mapping\6.2.cubemaps.fs';
  vs = '..\Source\4.Advanced_Opengl\6.2.Cubemaps_Environment_Mapping\6.2.cubemaps.vs';
  skybox_fs = '..\Source\4.Advanced_Opengl\6.2.Cubemaps_Environment_Mapping\6.2.skybox.fs';
  skybox_vs = '..\Source\4.Advanced_Opengl\6.2.Cubemaps_Environment_Mapping\6.2.skybox.vs';
  imgContainer = '..\Resources\textures\container.jpg';
  imgFaces: TArr_str = (
    '..\Resources/textures/skybox/right.jpg',
    '..\Resources/textures/skybox/left.jpg',
    '..\Resources/textures/skybox/top.jpg',
    '..\Resources/textures/skybox/bottom.jpg',
    '..\Resources/textures/skybox/front.jpg',
    '..\Resources/textures/skybox/back.jpg');
var
  window: PGLFWwindow;
  cubeVertices, skyboxVertices: TArr_GLfloat;
  cubeVAO, cubeVBO, skyboxVAO, skyboxVBO: GLuint;
  cubeTexture, cubemapTexture: Cardinal;
  shader, skyboxShader: TShaderProgram;
  projection, view, model: TMat4;
  currentFrame: GLfloat;
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
  skyboxShader := TShaderProgram.Create;
  camera := TCamera.Create(TGLM.Vec3(0, 0, 3));

  try
    cubeVertices := TArr_GLfloat([
       // positions       // normals
      -0.5, -0.5, -0.5,   0.0,  0.0, -1.0,
       0.5, -0.5, -0.5,   0.0,  0.0, -1.0,
       0.5,  0.5, -0.5,   0.0,  0.0, -1.0,
       0.5,  0.5, -0.5,   0.0,  0.0, -1.0,
      -0.5,  0.5, -0.5,   0.0,  0.0, -1.0,
      -0.5, -0.5, -0.5,   0.0,  0.0, -1.0,

      -0.5, -0.5,  0.5,   0.0,  0.0,  1.0,
       0.5, -0.5,  0.5,   0.0,  0.0,  1.0,
       0.5,  0.5,  0.5,   0.0,  0.0,  1.0,
       0.5,  0.5,  0.5,   0.0,  0.0,  1.0,
      -0.5,  0.5,  0.5,   0.0,  0.0,  1.0,
      -0.5, -0.5,  0.5,   0.0,  0.0,  1.0,

      -0.5,  0.5,  0.5, - 1.0,  0.0,  0.0,
      -0.5,  0.5, -0.5, - 1.0,  0.0,  0.0,
      -0.5, -0.5, -0.5, - 1.0,  0.0,  0.0,
      -0.5, -0.5, -0.5, - 1.0,  0.0,  0.0,
      -0.5, -0.5,  0.5, - 1.0,  0.0,  0.0,
      -0.5,  0.5,  0.5, - 1.0,  0.0,  0.0,

       0.5,  0.5,  0.5,   1.0,  0.0,  0.0,
       0.5,  0.5, -0.5,   1.0,  0.0,  0.0,
       0.5, -0.5, -0.5,   1.0,  0.0,  0.0,
       0.5, -0.5, -0.5,   1.0,  0.0,  0.0,
       0.5, -0.5,  0.5,   1.0,  0.0,  0.0,
       0.5,  0.5,  0.5,   1.0,  0.0,  0.0,

      -0.5, -0.5, -0.5,   0.0, -1.0,  0.0,
       0.5, -0.5, -0.5,   0.0, -1.0,  0.0,
       0.5, -0.5,  0.5,   0.0, -1.0,  0.0,
       0.5, -0.5,  0.5,   0.0, -1.0,  0.0,
      -0.5, -0.5,  0.5,   0.0, -1.0,  0.0,
      -0.5, -0.5, -0.5,   0.0, -1.0,  0.0,

      -0.5,  0.5, -0.5,   0.0,  1.0,  0.0,
       0.5,  0.5, -0.5,   0.0,  1.0,  0.0,
       0.5,  0.5,  0.5,   0.0,  1.0,  0.0,
       0.5,  0.5,  0.5,   0.0,  1.0,  0.0,
      -0.5,  0.5,  0.5,   0.0,  1.0,  0.0,
      -0.5,  0.5, -0.5,   0.0,  1.0,  0.0]);

    skyboxVertices := TArr_GLfloat([
      // positions
      -1.0,  1.0, -1.0,
      -1.0, -1.0, -1.0,
       1.0, -1.0, -1.0,
       1.0, -1.0, -1.0,
       1.0,  1.0, -1.0,
      -1.0,  1.0, -1.0,

      -1.0, -1.0,  1.0,
      -1.0, -1.0, -1.0,
      -1.0,  1.0, -1.0,
      -1.0,  1.0, -1.0,
      -1.0,  1.0,  1.0,
      -1.0, -1.0,  1.0,

       1.0, -1.0, -1.0,
       1.0, -1.0,  1.0,
       1.0,  1.0,  1.0,
       1.0,  1.0,  1.0,
       1.0,  1.0, -1.0,
       1.0, -1.0, -1.0,

      -1.0, -1.0,  1.0,
      -1.0,  1.0,  1.0,
       1.0,  1.0,  1.0,
       1.0,  1.0,  1.0,
       1.0, -1.0,  1.0,
      -1.0, -1.0,  1.0,

      -1.0,  1.0, -1.0,
       1.0,  1.0, -1.0,
       1.0,  1.0,  1.0,
       1.0,  1.0,  1.0,
      -1.0,  1.0,  1.0,
      -1.0,  1.0, -1.0,

      -1.0, -1.0, -1.0,
      -1.0, -1.0,  1.0,
       1.0, -1.0, -1.0,
       1.0, -1.0, -1.0,
      -1.0, -1.0,  1.0,
       1.0, -1.0,  1.0]);

    //═════════════════════════════════════════════════════════════════════════

    // cube VAO
    cubeVAO := GLuint(0);
    cubeVBO := GLuint(0);

    glGenVertexArrays(1, @cubeVAO);
    glGenBuffers(1, @cubeVBO);

    glBindVertexArray(cubeVAO);
    glBindBuffer(GL_ARRAY_BUFFER, cubeVBO);
    glBufferData(GL_ARRAY_BUFFER, cubeVertices.MemSize, @cubeVertices[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * SIZE_OF_F, Pointer(0));

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));

    //═════════════════════════════════════════════════════════════════════════

    // skybox VAO
    skyboxVAO := GLuint(0);
    skyboxVBO := GLuint(0);

    glGenVertexArrays(1, @skyboxVAO);
    glGenBuffers(1, @skyboxVBO);

    glBindVertexArray(skyboxVAO);
    glBindBuffer(GL_ARRAY_BUFFER, skyboxVBO);
    glBufferData(GL_ARRAY_BUFFER, skyboxVertices.MemSize, @skyboxVertices[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * SIZE_OF_F, Pointer(0));

    //═════════════════════════════════════════════════════════════════════════

    // 加载纹理
    cubeTexture := LoadTexture(imgContainer);
    cubemapTexture := LoadCubemap(imgFaces);

    // shader configuration
    shader.LoadShaderFile(vs, fs);
    shader.UseProgram;
    shader.SetUniformInt('skybox', 0);

    skyboxShader.LoadShaderFile(skybox_vs, skybox_fs);
    skyboxShader.UseProgram;
    skyboxShader.SetUniformInt('skybox', 0);

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

      // 正常绘制场景
      shader.UseProgram;
      model := TGLM.Mat4_Identity;
      view := camera.GetViewMatrix;
      projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 100);
      shader.SetUniformMatrix4fv('model', model);
      shader.SetUniformMatrix4fv('view', view);
      shader.SetUniformMatrix4fv('projection', projection);
      shader.SetUniformVec3('cameraPos', camera.Position);

      // cubes
      glBindVertexArray(cubeVAO);
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_CUBE_MAP, cubemapTexture);
      glDrawArrays(GL_TRIANGLES, 0, 36);

      // 最后绘制 skybox
      // 改变depth函数，当depth值等于depth buffer的内容时，depth测试通过
      glDepthFunc(GL_LEQUAL);
      skyboxShader.UseProgram;
      // 从视图矩阵中删除平移
      view := TGLM.Mat4(TGLM.Mat3(camera.GetViewMatrix));
      skyboxShader.SetUniformMatrix4fv('view', view);
      skyboxShader.SetUniformMatrix4fv('projection', projection);

      // skybox cube
      glBindVertexArray(skyboxVAO);
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_CUBE_MAP, cubemapTexture);
      glDrawArrays(GL_TRIANGLES, 0, 36);
      glDepthFunc(GL_LESS);

      //═════════════════════════════════════════════════════════════════════════

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @cubeVAO);
    glDeleteVertexArrays(1, @skyboxVAO);

    glDeleteBuffers(1, @cubeVBO);
    glDeleteBuffers(1, @skyboxVBO);

    glDeleteTextures(1, @cubeTexture);
    glDeleteTextures(1, @cubemapTexture);
  finally
    camera.Free;
    shader.Free;
    skyboxShader.Free;

    // 释放 / 删除之前的分配的所有资源
    glfwTerminate;
  end;
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

function LoadCubemap(faces: TArr_str; inverse: boolean): cardinal;
var
  texture_ID: Cardinal;
  i: Integer;
  tx: TTexture;
begin
  texture_ID := cardinal(0);
  glGenTextures(1, @texture_ID);
  glBindTexture(GL_TEXTURE_CUBE_MAP, texture_ID);

  for i := 0 to High(faces) do
  begin
    tx := TTexture.Create();
    try
      tx.LoadFormFile(faces[i], inverse);
      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_RGBA, tx.Width,
        tx.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, tx.Pixels);
    finally
      tx.Free;
    end;
  end;

  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

  Result := texture_ID;
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

