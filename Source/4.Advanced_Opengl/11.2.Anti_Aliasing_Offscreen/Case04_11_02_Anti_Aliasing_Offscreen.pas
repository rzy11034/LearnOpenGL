unit Case04_11_02_Anti_Aliasing_Offscreen;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

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
  shader_path = '..\Source\4.Advanced_Opengl\11.2.Anti_Aliasing_Offscreen\';
  vs = shader_path + '11.2.anti_aliasing.vs';
  fs = shader_path + '11.2.anti_aliasing.fs';
  screen_vs = shader_path + '11.2.aa_post.vs';
  screen_fs = shader_path + '11.2.aa_post.fs';
var
  window: PGLFWwindow;
  currentFrame: GLfloat;
  shader, screenShader: TShaderProgram;
  cubeVertices, quadVertices: TArr_GLfloat;
  cubeVAO, cubeVBO, quadVBO, quadVAO, framebuffer: GLuint;
  textureColorBufferMultiSampled, RBO, intermediateFBO, screenTexture: GLuint;
  projection, view, model: TMat4;
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
  screenShader := TShaderProgram.Create;

  camera := TCamera.Create(TGLM.Vec3(0, 0, 3));

  try
    shader.LoadShaderFile(vs, fs);
    screenShader.LoadShaderFile(screen_vs, screen_fs);

    //═════════════════════════════════════════════════════════════════════════

    cubeVertices := TArr_GLfloat([
    // Positions
    -0.5, -0.5, -0.5,
     0.5, -0.5, -0.5,
     0.5,  0.5, -0.5,
     0.5,  0.5, -0.5,
    -0.5,  0.5, -0.5,
    -0.5, -0.5, -0.5,

    -0.5, -0.5,  0.5,
     0.5, -0.5,  0.5,
     0.5,  0.5,  0.5,
     0.5,  0.5,  0.5,
    -0.5,  0.5,  0.5,
    -0.5, -0.5,  0.5,

    -0.5,  0.5,  0.5,
    -0.5,  0.5, -0.5,
    -0.5, -0.5, -0.5,
    -0.5, -0.5, -0.5,
    -0.5, -0.5,  0.5,
    -0.5,  0.5,  0.5,

     0.5,  0.5,  0.5,
     0.5,  0.5, -0.5,
     0.5, -0.5, -0.5,
     0.5, -0.5, -0.5,
     0.5, -0.5,  0.5,
     0.5,  0.5,  0.5,

    -0.5, -0.5, -0.5,
     0.5, -0.5, -0.5,
     0.5, -0.5,  0.5,
     0.5, -0.5,  0.5,
    -0.5, -0.5,  0.5,
    -0.5, -0.5, -0.5,

    -0.5,  0.5, -0.5,
     0.5,  0.5, -0.5,
     0.5,  0.5,  0.5,
     0.5,  0.5,  0.5,
    -0.5,  0.5,  0.5,
    -0.5,  0.5, -0.5]);

    cubeVAO := GLuint(0);
    cubeVBO := GLuint(0);

    glGenVertexArrays(1, @cubeVAO);
    glGenBuffers(1, @cubeVBO);
    glBindVertexArray(cubeVAO);
    glBindBuffer(GL_ARRAY_BUFFER, cubeVBO);
    glBufferData(GL_ARRAY_BUFFER, cubeVertices.MemSize, @cubeVertices[0], GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * SIZE_OF_F, Pointer(0));

    //═════════════════════════════════════════════════════════════════════════

    // 在标准化设备坐标中填充整个屏幕的四边形的顶点属性。
    quadVertices := TArr_GLfloat([
      -1.0,  1.0,  0.0, 1.0,
      -1.0, -1.0,  0.0, 0.0,
       1.0, -1.0,  1.0, 0.0,

      -1.0,  1.0,  0.0, 1.0,
       1.0, -1.0,  1.0, 0.0,
       1.0,  1.0,  1.0, 1.0]);

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

    //配置MSAA帧缓冲区
    framebuffer := GLuint(0);
    glGenFramebuffers(1, @framebuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);

    // 创建一个多采样颜色附件纹理
    textureColorBufferMultiSampled := GLuint(0);
    glGenTextures(1, @textureColorBufferMultiSampled);
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, textureColorBufferMultiSampled);
    glTexImage2DMultisample(GL_TEXTURE_2D_MULTISAMPLE, 4, GL_RGB, SCR_WIDTH, SCR_HEIGHT, GL_TRUE);
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, 0);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D_MULTISAMPLE,
      textureColorBufferMultiSampled, 0);

    //═════════════════════════════════════════════════════════════════════════

    // 为深度和模板附件创建一个(也是多采样)renderbuffer对象
    RBO := GLuint(0);
    glGenRenderbuffers(1, @RBO);
    glBindRenderbuffer(GL_RENDERBUFFER, RBO);
    glRenderbufferStorageMultisample(GL_RENDERBUFFER, 4, GL_DEPTH24_STENCIL8, SCR_WIDTH, SCR_HEIGHT);
    glBindRenderbuffer(GL_RENDERBUFFER, 0);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, RBO);

    if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
    begin
      WriteLn('ERROR::FRAMEBUFFER:: Framebuffer is not complete!');
    end;
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    //═════════════════════════════════════════════════════════════════════════

    // 配置第二个后处理帧缓冲区
    intermediateFBO := GLuint(0);
    glGenFramebuffers(1, @intermediateFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, intermediateFBO);

    // 创建一个颜色附件纹理
    screenTexture := GLuint(0);
    glGenTextures(1, @screenTexture);
    glBindTexture(GL_TEXTURE_2D, screenTexture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGB, GL_UNSIGNED_BYTE, nil);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    //我们只需要一个颜色缓冲
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, screenTexture, 0);

    if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
    begin
      WriteLn('ERROR::FRAMEBUFFER:: Intermediate framebuffer is not complete!');
    end;
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    //═════════════════════════════════════════════════════════════════════════

    screenShader.UseProgram;
    screenShader.SetUniformInt('screenTexture', 0);

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

      // 1. 在多采样缓冲区中正常绘制场景
      glBindFramebuffer(GL_FRAMEBUFFER, framebuffer);
      glClearColor(0.1, 0.1, 0.1, 1.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      glEnable(GL_DEPTH_TEST);

      // 设置变换矩阵
      shader.UseProgram;
      projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 1000);
      view := camera.GetViewMatrix;
      model := TGLM.Mat4_Identity;
      shader.SetUniformMatrix4fv('projection', projection);
      shader.SetUniformMatrix4fv('view', view);
      shader.SetUniformMatrix4fv('model', model);

      glBindVertexArray(cubeVAO);
      glDrawArrays(GL_TRIANGLES, 0, 36);

      // 2. 现在 blit 多采样缓冲(s)到正常的彩色缓冲的中间FBO。图像存储在屏幕纹理中
      glBindFramebuffer(GL_READ_FRAMEBUFFER, framebuffer);
      glBindFramebuffer(GL_DRAW_FRAMEBUFFER, intermediateFBO);
      glBlitFramebuffer(0, 0, SCR_WIDTH, SCR_HEIGHT, 0, 0, SCR_WIDTH, SCR_HEIGHT,
        GL_COLOR_BUFFER_BIT, GL_NEAREST);

      // 3. 现在用场景的视觉效果作为它的纹理图像来渲染quad
      glBindFramebuffer(GL_FRAMEBUFFER, 0);
      glClearColor(1.0, 1.0, 1.0, 1.0);
      glClear(GL_COLOR_BUFFER_BIT);
      glDisable(GL_DEPTH_TEST);

      // 绘制屏幕四边形
      screenShader.UseProgram;
      glBindVertexArray(quadVAO);
      glActiveTexture(GL_TEXTURE0);
      // 使用现在解决的颜色附件作为四边形的纹理
      glBindTexture(GL_TEXTURE_2D, screenTexture);
      glDrawArrays(GL_TRIANGLES, 0, 6);

      //═════════════════════════════════════════════════════════════════════════

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @cubeVAO);
    glDeleteBuffers(1, @cubeVBO);

    glDeleteVertexArrays(1, @quadVAO);
    glDeleteBuffers(1, @quadVBO);

    glDeleteFramebuffers(1, @framebuffer);
    glDeleteRenderbuffers(1, @RBO);

    glDeleteTextures(1, @screenTexture);
    glDeleteTextures(1, @textureColorBufferMultiSampled);
  finally
    camera.Free;
    shader.Free;
    screenShader.Free;

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

