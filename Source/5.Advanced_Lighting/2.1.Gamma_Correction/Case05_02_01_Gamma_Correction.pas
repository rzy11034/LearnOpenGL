﻿unit Case05_02_01_Gamma_Correction;

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
function LoadTexture(fileName: string; gammaCorrection: boolean; inverse: boolean = true): cardinal; forward;
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

  gammaEnabled: Boolean = false;
  gammaKeyPressed: Boolean = false;

procedure Main;
const
  shader_path = '..\Source\5.Advanced_Lighting\2.1.Gamma_Correction\';
  vs = shader_path + '2.gamma_correction.vs';
  fs = shader_path + '2.gamma_correction.fs';
  imgFloor = '..\Resources\textures\wood.png';
var
  window: PGLFWwindow;
  currentFrame: GLfloat;
  shader: TShaderProgram;
  planeVertices: TArr_GLfloat;
  planeVAO, planeVBO: GLuint;
  projection, view, {%H-}model: TMat4;
  outStr: String;
  floorTexture, floorTextureGammaCorrected: Cardinal;
  lightPositions, lightColors: TArr_TVec3;
  uniformLocation: GLint;
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

  camera := TCamera.Create(TGLM.Vec3(0, 0, 3));

  try
    shader.LoadShaderFile(vs, fs);
    floorTexture := LoadTexture(imgFloor, false);
    floorTextureGammaCorrected := LoadTexture(imgFloor, true);

    //═════════════════════════════════════════════════════════════════════════

    planeVertices := TArr_GLfloat([
       // positions        // normals      // texcoords
       10.0, -0.5,  10.0,  0.0, 1.0, 0.0,  10.0,  0.0,
      -10.0, -0.5,  10.0,  0.0, 1.0, 0.0,   0.0,  0.0,
      -10.0, -0.5, -10.0,  0.0, 1.0, 0.0,   0.0, 10.0,

       10.0, -0.5,  10.0,  0.0, 1.0, 0.0,  10.0,  0.0,
      -10.0, -0.5, -10.0,  0.0, 1.0, 0.0,   0.0, 10.0,
       10.0, -0.5, -10.0,  0.0, 1.0, 0.0,  10.0, 10.0]);

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

    // lighting info
    lightPositions := TArr_TVec3([
      TGLM.Vec3(-3.0, 0.0, 0.0),
      TGLM.Vec3(-1.0, 0.0, 0.0),
      TGLM.Vec3( 1.0, 0.0, 0.0),
      TGLM.Vec3( 3.0, 0.0, 0.0)]);

    lightColors := TArr_TVec3([
      TGLM.Vec3(0.25),
      TGLM.Vec3(0.50),
      TGLM.Vec3(0.75),
      TGLM.Vec3(1.00)]);

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

      // 设置变换矩阵
      shader.UseProgram;
      projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 1000);
      view := camera.GetViewMatrix;
      shader.SetUniformMatrix4fv('projection', projection);
      shader.SetUniformMatrix4fv('view', view);

      //═════════════════════════════════════════════════════════════════════════

      // set light uniforms
      uniformLocation := GLint(0);

      uniformLocation := glGetUniformLocation(shader.ID, 'lightPositions');
      glUniform3fv(uniformLocation, 4,  @lightPositions[0]);

      uniformLocation := glGetUniformLocation(shader.ID, 'lightColors');
      glUniform3fv(uniformLocation, 4,  @lightColors[0]);

      //═════════════════════════════════════════════════════════════════════════

      // floor
      glBindVertexArray(planeVAO);
      glActiveTexture(GL_TEXTURE0);

      if gammaEnabled then
        glBindTexture(GL_TEXTURE_2D, floorTextureGammaCorrected)
      else
        glBindTexture(GL_TEXTURE_2D, floorTexture);

      glDrawArrays(GL_TRIANGLES, 0, 6);

      if gammaEnabled then
        outStr := 'Gamma enabled'
      else
        outStr := 'Gamma disabled';
      WriteLn('Gamma Correction: ', outStr);

      //═════════════════════════════════════════════════════════════════════════

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @planeVAO);
    glDeleteBuffers(1, @planeVBO);

    glDeleteTextures(1, @floorTexture);
    glDeleteTextures(1, @floorTextureGammaCorrected);
  finally
    camera.Free;
    shader.Free;

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

function LoadTexture(fileName: string; gammaCorrection: boolean; inverse: boolean): cardinal;
var
  texture_ID: GLuint;
  tx: TTexture;
  internalFormat: GLenum;
  tx_managed: IInterface;
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

  if (glfwGetKey(window, GLFW_KEY_B) = GLFW_PRESS) and not(gammaKeyPressed) then
  begin
    gammaEnabled := not gammaEnabled;
    gammaKeyPressed := true;
  end;

  if glfwGetKey(window, GLFW_KEY_B) = GLFW_RELEASE then
    gammaKeyPressed := false;
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

