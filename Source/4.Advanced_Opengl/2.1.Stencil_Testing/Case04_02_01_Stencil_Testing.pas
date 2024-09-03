unit Case04_02_01_Stencil_Testing;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils, DeepStar.Utils;

procedure Main;

implementation

uses
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.Camera,
  DeepStar.OpenGL.Texture;

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
function LoadTexture(fileName: string): cardinal; forward;

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
  fs = '..\Source\4.Advanced_Opengl\2.1.Stencil_Testing\2.stencil_testing.fs';
  vs = '..\Source\4.Advanced_Opengl\2.1.Stencil_Testing\2.stencil_testing.vs';
  singleColor_fs = '..\Source\4.Advanced_Opengl\2.1.Stencil_Testing\2.stencil_single_color.fs';
  imgMarble = '..\Resources\textures\marble.jpg';
  imgMetal = '..\Resources\textures\metal.png';
var
  window: PGLFWwindow;
  cubeVertices, planeVertices: TArr_GLfloat;
  cubeVAO, cubeVBO, planeVAO, planeVBO: GLuint;
  cubeTexture, floorTexture: Cardinal;
  shader, shaderSingleColor: TShaderProgram;
  projection, view, model: TMat4;
  currentFrame: GLfloat;
begin
  window := InitWindows;
  if window = nil then
  begin
    glfwTerminate;
    Exit;
  end;

  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);

  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_NOTEQUAL, 1, $FF);
  glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);

  shader := TShaderProgram.Create;
  shaderSingleColor := TShaderProgram.Create;
  camera := TCamera.Create(TGLM.Vec3(0, 0, 3));
  try
    cubeVertices := TArr_GLfloat([
       // positions       // texture Coords
      -0.5, -0.5, -0.5,   0.0, 0.0,
       0.5, -0.5, -0.5,   1.0, 0.0,
       0.5,  0.5, -0.5,   1.0, 1.0,
       0.5,  0.5, -0.5,   1.0, 1.0,
      -0.5,  0.5, -0.5,   0.0, 1.0,
      -0.5, -0.5, -0.5,   0.0, 0.0,

      -0.5, -0.5,  0.5,   0.0, 0.0,
       0.5, -0.5,  0.5,   1.0, 0.0,
       0.5,  0.5,  0.5,   1.0, 1.0,
       0.5,  0.5,  0.5,   1.0, 1.0,
      -0.5,  0.5,  0.5,   0.0, 1.0,
      -0.5, -0.5,  0.5,   0.0, 0.0,

      -0.5,  0.5,  0.5,   1.0, 0.0,
      -0.5,  0.5, -0.5,   1.0, 1.0,
      -0.5, -0.5, -0.5,   0.0, 1.0,
      -0.5, -0.5, -0.5,   0.0, 1.0,
      -0.5, -0.5,  0.5,   0.0, 0.0,
      -0.5,  0.5,  0.5,   1.0, 0.0,

       0.5,  0.5,  0.5,   1.0, 0.0,
       0.5,  0.5, -0.5,   1.0, 1.0,
       0.5, -0.5, -0.5,   0.0, 1.0,
       0.5, -0.5, -0.5,   0.0, 1.0,
       0.5, -0.5,  0.5,   0.0, 0.0,
       0.5,  0.5,  0.5,   1.0, 0.0,

      -0.5, -0.5, -0.5,   0.0, 1.0,
       0.5, -0.5, -0.5,   1.0, 1.0,
       0.5, -0.5,  0.5,   1.0, 0.0,
       0.5, -0.5,  0.5,   1.0, 0.0,
      -0.5, -0.5,  0.5,   0.0, 0.0,
      -0.5, -0.5, -0.5,   0.0, 1.0,

      -0.5,  0.5, -0.5,   0.0, 1.0,
       0.5,  0.5, -0.5,   1.0, 1.0,
       0.5,  0.5,  0.5,   1.0, 0.0,
       0.5,  0.5,  0.5,   1.0, 0.0,
      -0.5,  0.5,  0.5,   0.0, 0.0,
      -0.5,  0.5, -0.5,   0.0, 1.0]);

    // 注意我们将这些设置为大于1(连同GL_REPEAT作为纹理包裹模式)。
    // 这将导致地板纹理重复
    planeVertices := TArr_GLfloat([
      // positions        // texture Coords
       5.0, -0.5,  5.0,   2.0, 0.0,
      -5.0, -0.5,  5.0,   0.0, 0.0,
      -5.0, -0.5, -5.0,   0.0, 2.0,

       5.0, -0.5,  5.0,   2.0, 0.0,
      -5.0, -0.5, -5.0,   0.0, 2.0,
       5.0, -0.5, -5.0,   2.0, 2.0]);

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
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(0));

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));

    //═════════════════════════════════════════════════════════════════════════
    // plane VAO
    planeVAO := GLuint(0);
    planeVBO := GLuint(0);

    glGenVertexArrays(1, @planeVAO);
    glGenBuffers(1, @planeVBO);

    glBindVertexArray(planeVAO);
    glBindBuffer(GL_ARRAY_BUFFER, planeVBO);
    glBufferData(GL_ARRAY_BUFFER, planeVertices.MemSize, @planeVertices[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(0));

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));

    //═════════════════════════════════════════════════════════════════════════

    // 加载纹理
    cubeTexture := LoadTexture(imgMarble);
    floorTexture := LoadTexture(imgMetal);

    // shader configuration
    shader.LoadShaderFile(vs, fs);
    shaderSingleColor.LoadShaderFile(vs, singleColor_fs);

    shader.UseProgram;
    shader.SetUniformInt('texture1', 0);

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
      // 不要忘记清除模板缓冲区!
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

      shaderSingleColor.UseProgram;
      view := camera.GetViewMatrix;
      projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 100);
      shaderSingleColor.SetUniformMatrix4fv('view', view);
      shaderSingleColor.SetUniformMatrix4fv('projection', projection);

      shader.UseProgram;
      shader.SetUniformMatrix4fv('view', view);
      shader.SetUniformMatrix4fv('projection', projection);

      // 像往常一样绘制地板，但不要将地板写入模板缓冲区，我们只关心容器。
      // 我们将其掩码设置为0x00，以不写入模板缓冲区
      glStencilMask($00);

      // floor
      glBindVertexArray(planeVAO);
      glBindTexture(GL_TEXTURE_2D, floorTexture);
      shader.SetUniformMatrix4fv('model', TGLM.Mat4_Identity);
      glDrawArrays(GL_TRIANGLES, 0, 6);

      // 第一。渲染通过，正常绘制对象，写入模板缓冲区
      glStencilFunc(GL_ALWAYS, 1, $FF);
      glStencilMask($FF);
      // cubes
      glBindVertexArray(cubeVAO);
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, cubeTexture);
      model := TGLM.Translate(TGLM.Mat4_Identity, TGLM.Vec3(-1, 0, -1));
      shader.SetUniformMatrix4fv('model', model);
      glDrawArrays(GL_TRIANGLES, 0, 36);
      model := TGLM.Translate(TGLM.Mat4_Identity, TGLM.Vec3(2, 0, 0));
      shader.SetUniformMatrix4fv('model', model);
      glDrawArrays(GL_TRIANGLES, 0, 36);

      // 第二。渲染通道:现在绘制物体的稍微缩放版本，这次禁用模板书写。
      // 因为模板缓冲区现在填满了 1
      // 对象的大小不同，使其看起来像边界。
      glStencilFunc(GL_NOTEQUAL, 1, $FF);
      glStencilMask($00);
      glDisable(GL_DEPTH_TEST);

      shaderSingleColor.UseProgram;
      // cubes
      glBindVertexArray(cubeVAO);
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, cubeTexture);
      model := TGLM.Translate(TGLM.Mat4_Identity, TGLM.Vec3(-1, 0, -1));
      model := TGLM.Scale(model, TGLM.Vec3(1.1));
      shaderSingleColor.SetUniformMatrix4fv('model', model);
      glDrawArrays(GL_TRIANGLES, 0, 36);
      model := TGLM.Translate(TGLM.Mat4_Identity, TGLM.Vec3(2, 0, 0));
      model := TGLM.Scale(model, TGLM.Vec3(1.1));
      shaderSingleColor.SetUniformMatrix4fv('model', model);
      glDrawArrays(GL_TRIANGLES, 0, 36);

      glStencilMask($FF);
      glStencilFunc(GL_ALWAYS, 0, $FF);
      glEnable(GL_DEPTH_TEST);

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @cubeVAO);
    glDeleteVertexArrays(1, @planeVAO);
    glDeleteBuffers(1, @cubeVBO);
    glDeleteBuffers(1, @planeVBO);
    glDeleteTextures(1, @cubeTexture);
    glDeleteTextures(1, @floorTexture);
  finally
    camera.Free;
    shader.Free;
    shaderSingleColor.Free;

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
    WriteLn(' Failed to create GLFW window');
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

function LoadTexture(fileName: string): cardinal;
var
  texture_ID: GLuint;
  tx: TTexture;
begin
  texture_ID := GLuint(0);
  glGenTextures(1, @texture_ID);

  tx := TTexture.Create();
  try
    tx.LoadFormFile(fileName);

    glBindTexture(GL_TEXTURE_2D, texture_ID);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tx.Width, tx.Height, 0, GL_RGBA,
      GL_UNSIGNED_BYTE, tx.Pixels);
    glGenerateMipmap(GL_TEXTURE_2D);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    Result := texture_ID;
  finally
    tx.Destroy;
  end;
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

