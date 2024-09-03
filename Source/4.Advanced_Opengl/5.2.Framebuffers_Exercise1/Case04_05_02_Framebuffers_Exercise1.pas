unit Case04_05_02_Framebuffers_Exercise1;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils;

procedure Main;

implementation

uses
  DeepStar.Utils,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL,
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
function LoadTexture(fileName: string; inverse: boolean = true): cardinal; forward;

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

  function LoadTexture2(fileName: string): cardinal;
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

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

      Result := texture_ID;
    finally
      tx.Destroy;
    end;
  end;

procedure Main;
const
  fs = '..\Source\4.Advanced_Opengl\5.2.Framebuffers_Exercise1\5.2.framebuffers.fs';
  vs = '..\Source\4.Advanced_Opengl\5.2.Framebuffers_Exercise1\5.2.framebuffers.vs';
  screen_fs = '..\Source\4.Advanced_Opengl\5.2.Framebuffers_Exercise1\5.2.framebuffers_screen.fs';
  screen_vs = '..\Source\4.Advanced_Opengl\5.2.Framebuffers_Exercise1\5.2.framebuffers_screen.vs';
  imgContainer = '..\Resources\textures\container.jpg';
  imgMetal = '..\Resources\textures\metal.png';
var
  window: PGLFWwindow;
  cubeVertices, planeVertices, quadVertices: TArr_GLfloat;
  cubeVAO, cubeVBO, planeVAO, planeVBO, quadVAO, quadVBO: GLuint;
  framebuffer, textureColorbuffer, rbo: GLuint;
  cubeTexture, floorTexture: Cardinal;
  shader, shaderScreen: TShaderProgram;
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
  shaderScreen := TShaderProgram.Create;
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

    planeVertices := TArr_GLfloat([
      // positions        // texture Coords
       5.0, -0.5,  5.0,   2.0, 0.0,
      -5.0, -0.5,  5.0,   0.0, 0.0,
      -5.0, -0.5, -5.0,   0.0, 2.0,

       5.0, -0.5,  5.0,   2.0, 0.0,
      -5.0, -0.5, -5.0,   0.0, 2.0,
       5.0, -0.5, -5.0,   2.0, 2.0]);

    quadVertices := TArr_GLfloat([
      // positions    // texture Coords
      -0.3,  1.0,  0.0, 1.0,
      -0.3,  0.7,  0.0, 0.0,
       0.3,  0.7,  1.0, 0.0,

      -0.3,  1.0,  0.0, 1.0,
       0.3,  0.7,  1.0, 0.0,
       0.3,  1.0,  1.0, 1.0]);

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

    // screen quad VAO
    quadVAO := GLuint(0);
    quadVBO := GLuint(0);

    glGenVertexArrays(1, @quadVAO);
    glGenBuffers(1, @quadVBO);

    glBindVertexArray(quadVAO);
    glBindBuffer(GL_ARRAY_BUFFER, quadVBO);
    glBufferData(GL_ARRAY_BUFFER, quadVertices.MemSize, @quadVertices[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * SIZE_OF_F, Pointer(0));

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * SIZE_OF_F, Pointer(2 * SIZE_OF_F));

    //═════════════════════════════════════════════════════════════════════════

    // 加载纹理
    cubeTexture := LoadTexture(imgContainer);
    floorTexture := LoadTexture(imgMetal );

    // shader configuration
    shader.LoadShaderFile(vs, fs);
    shader.UseProgram;
    shader.SetUniformInt('texture1', 0);

    shaderScreen.LoadShaderFile(screen_vs, screen_fs);
    shaderScreen.UseProgram;
    shaderScreen.SetUniformInt('screenTexture', 0);

    //═════════════════════════════════════════════════════════════════════════

    // framebuffer configuration
    framebuffer := GLuint(0);
    glGenFramebuffers(1, @framebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);

    // create a color attachment texture
    textureColorbuffer := GLuint(0);
    glGenTextures(1, @textureColorbuffer);
    glBindTexture(GL_TEXTURE_2D, textureColorbuffer);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, textureColorbuffer, 0);

    // create a renderbuffer object for depth and stencil attachment (we won't be sampling these)
    rbo := GLuint(0);
    glGenRenderbuffers(1, @rbo);
    glBindRenderbuffer(GL_RENDERBUFFER, rbo);
    // 使用一个renderbuffer对象作为深度缓冲和模板缓冲。
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, SCR_WIDTH, SCR_HEIGHT);
    // 现在实际添加它
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, rbo);
    // 现在我们实际上创建了framebuffer并添加了所有附件，我们想要检查它是否实际上已经完成
    if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
        WriteLn('ERROR::FRAMEBUFFER:: Framebuffer is not complete!');
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

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

      // 首次渲染通过：镜像纹理。
      // 绑定到帧缓冲并以正常方式绘制到颜色纹理，
      // 但视图相机是反向的。
      // 绑定到帧缓冲并按正常方式绘制场景到颜色纹理
      glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
      // 启用深度测试（在渲染屏幕空间四边形时禁用）
      glEnable(GL_DEPTH_TEST);

      // render
      glClearColor(0.1, 0.1, 0.1, 1.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      shader.UseProgram;
      // 将相机的偏航旋转 180 度
      camera.Yaw := camera.Yaw + 180;
      // 调用此函数以确保它更新其相机矢量，请注意，
      // 我们禁用了此特定情况下的倾斜度约束（否则我们无法反转相机的倾斜度值）
      camera.ProcessMouseMovement(0, 0, GL_FALSE);
      view := camera.GetViewMatrix;
      //将其重置为初始方向
      camera.Yaw := camera.Yaw - 180;
      camera.ProcessMouseMovement(0, 0, GL_TRUE);
      projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 100);
      shader.SetUniformMatrix4fv('view', view);
      shader.SetUniformMatrix4fv('projection', projection);

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

      // floor
      glBindVertexArray(planeVAO);
      glBindTexture(GL_TEXTURE_2D, floorTexture);
      shader.SetUniformMatrix4fv('model', TGLM.Mat4_Identity);
      glDrawArrays(GL_TRIANGLES, 0, 6);
      glBindVertexArray(0);

      //═════════════════════════════════════════════════════════════════════════

      // 第二个渲染通道:正常绘制
      glBindFramebuffer(GL_FRAMEBUFFER, 0);

      glClearColor(0.1, 0.1, 0.1, 1.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      model := TGLM.Mat4_Identity;
      view := camera.GetViewMatrix;
      shader.SetUniformMatrix4fv('view', view);

      // cubes
      glBindVertexArray(cubeVAO);
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, cubeTexture);
      model := TGLM.Translate(model, TGLM.Vec3(-1.0, 0.0, -1.0));
      shader.SetUniformMatrix4fv('model', model);
      glDrawArrays(GL_TRIANGLES, 0, 36);
      model := TGLM.Mat4(1.0);
      model := TGLM.Translate(model, TGLM.Vec3(2.0, 0.0, 0.0));
      shader.SetUniformMatrix4fv('model', model);
      glDrawArrays(GL_TRIANGLES, 0, 36);
      // floor
      glBindVertexArray(planeVAO);
      glBindTexture(GL_TEXTURE_2D, floorTexture);
      shader.SetUniformMatrix4fv('model', TGLM.Mat4_Identity);
      glDrawArrays(GL_TRIANGLES, 0, 6);

      //═════════════════════════════════════════════════════════════════════════

      // 现在用屏幕纹理绘制镜面四边形
      // 禁用深度测试，以便不会因深度测试而丢弃屏幕空间四边形。
      glDisable(GL_DEPTH_TEST);

      // 清除所有相关缓冲区
      // 将透明色设置为白色（实际上并不是真的必要，
      // 因为我们无论如何都无法看到四边形后面
      shaderScreen.UseProgram();
      glBindVertexArray(quadVAO);
      // 使用颜色附件纹理作为四边形平面的纹理
      glBindTexture(GL_TEXTURE_2D, textureColorbuffer);
      glDrawArrays(GL_TRIANGLES, 0, 6);

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @cubeVAO);
    glDeleteVertexArrays(1, @planeVAO);
    glDeleteVertexArrays(1, @quadVAO);

    glDeleteBuffers(1, @cubeVBO);
    glDeleteBuffers(1, @planeVBO);
    glDeleteBuffers(1, @quadVBO);

    glDeleteTextures(1, @cubeTexture);
    glDeleteTextures(1, @floorTexture);
  finally
    camera.Free;
    shader.Free;
    shaderScreen.Free;

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

