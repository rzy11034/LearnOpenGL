﻿unit Case05_06_01_HDR;

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

  hdr: Boolean = true;
  hdrKeyPressed: Boolean = false;
  exposure: float = 1.0;

  quadVAO: GLuint = 0;
  quadVBO: GLuint = 0;

  cubeVAO, cubeVBO: Cardinal;

procedure Main;
const
  dir_path = '..\Source\5.Advanced_Lighting\6.1.HDR\';
  hdr_vs = dir_path + '6.hdr.vs';
  hdr_fs = dir_path + '6.hdr.fs';
  lighting_vs = dir_path + '6.lighting.vs';
  lighting_fs = dir_path + '6.lighting.vs';
  img_wood = '..\Resources\textures\wood.png';
var
  window: PGLFWwindow;
  currentFrame: GLfloat;
  projection, view, model: TMat4;
  diffuseMap, normalMap, heightMap, hdrFBO, colorBuffer, rboDepth: Cardinal;
  shader, hdrShader: TShaderProgram;
  lightPos: TVec3;
  shader_managed, camera_managed, hdrShader_managed: IInterface;
  lightPosition_managed, lightColors_managed: IInterface;
  lightPositions, lightColors: TArrayList_TVec3;
  i: Integer;
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
  shader.LoadShaderFile(lighting_vs, lighting_fs);

  hdrShader_managed := IInterface(TShaderProgram.Create);
  hdrShader := hdrShader_managed as TShaderProgram;
  hdrShader.LoadShaderFile(hdr_vs, hdr_fs);

  camera_managed := IInterface(TCamera.Create(TGLM.Vec3(0, 0, 5)));
  camera := camera_managed as TCamera;

  //═════════════════════════════════════════════════════════════════════════

  // load textures
  woodTexture := LoadTexture(img_wood);

  //═════════════════════════════════════════════════════════════════════════

  // 配置浮点帧缓冲区
  hdrFBO := Cardinal(0);
  glGenFramebuffers(1, @hdrFBO);

  // 创建浮点颜色缓冲
  colorBuffer := Cardinal(0);
  glGenTextures(1, @colorBuffer);
  glBindTexture(GL_TEXTURE_2D, colorBuffer);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA16F, SCR_WIDTH, SCR_HEIGHT, 0, GL_RGBA, GL_FLOAT, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // 创建深度缓冲(renderbuffer)
  rboDepth := Cardinal(0);
  glGenRenderbuffers(1, @rboDepth);
  glBindRenderbuffer(GL_RENDERBUFFER, rboDepth);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, SCR_WIDTH, SCR_HEIGHT);

  // 附加缓冲
  glBindFramebuffer(GL_FRAMEBUFFER, hdrFBO);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, colorBuffer, 0);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, rboDepth);
  if glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE then
    WriteLn('Framebuffer not complete!');
  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  //═════════════════════════════════════════════════════════════════════════
  // lighting info

  // position
  lightPosition_managed := IInterface(TArrayList_TVec3.Create);
  lightPositions := lightPosition_managed as TArrayList_TVec3;
  lightPositions.AddLast(TGLM.Vec3( 0.0,  0.0, 49.5));
  lightPositions.AddLast(TGLM.Vec3(-1.4, -1.9,  9.0));
  lightPositions.AddLast(TGLM.Vec3( 0.0, -1.8,  4.0));
  lightPositions.AddLast(TGLM.Vec3( 0.8, -1.7,  6.0));

  // colors
  lightColors_managed := IInterface(TArrayList_TVec3.Create);
  lightColors := lightColors_managed as TArrayList_TVec3;
  lightColors.AddFirst(TGLM.Vec3(200.0, 200.0, 200.0));
  lightColors.AddFirst(TGLM.Vec3(  0.1,   0.0,   0.0));
  lightColors.AddFirst(TGLM.Vec3(  0.0,   0.0,   0.2));
  lightColors.AddFirst(TGLM.Vec3(  0.0,   0.1,   0.0));

  //═════════════════════════════════════════════════════════════════════════

  // shader configuration
  shader.UseProgram;
  shader.SetUniformInt('diffuseMap', 0);

  hdrShader.UseProgram;
  hdrShader.SetUniformInt('hdrBuffer', 0);

  //═════════════════════════════════════════════════════════════════════════

  // lighting info
  lightPos := TGLM.Vec3(0.5, 1.0, 0.3);

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
      glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
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

      //渲染隧道
      model := TGLM.Mat4(1.0);
      model := TGLM.Translate(model, TGLM.Vec3(0.0, 0.0, 25));
      model := TGLM.Scale(model, TGLM.Vec3(2.5, 2.5, 27.5));
      shader.SetUniformMatrix4fv('model', model);
      shader.SetUniformInt('inverse_normals', true.ToInteger);
      RenderCube;
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // 2. 现在将浮点颜色缓冲区渲染为2D四边形，
    // 并将色调映射HDR颜色渲染为默认framebuffer的(固定)颜色范围
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    hdrShader.UseProgram;
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, colorBuffer);
    hdrShader.setInt('hdr', hdr);
    hdrShader.setFloat('exposure', exposure);
    renderQuad;

    WriteLn('HDR: ', IfThen(hdr, 'on', 'off'), '| exposure: ', exposure.ToString);

    //═════════════════════════════════════════════════════════════════════════

    // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
    glfwSwapBuffers(window);
    glfwPollEvents;
  end;

  glDeleteVertexArrays(1, @quadVAO);
  glDeleteBuffers(1, @quadVBO);

  glDeleteTextures(1, @diffuseMap);
  glDeleteTextures(1, @normalMap);

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

function LoadTexture(fileName: string; inverse: boolean): Cardinal;
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

procedure RenderQuad;
var
  pos1, pos2, pos3, pos4, nm: TVec3;
  tangent1, bitangent1, tangent2, bitangent2, edge1, edge2: TVec3;
  uv1, uv2, uv3, uv4, deltaUV1, deltaUV2: TVec2;
  f: float;
  quadVertices: TArr_GLfloat;
begin
  if quadVAO = 0 then
  begin
    // positions
    pos1 := TGLM.Vec3(-1.0,  1.0, 0.0);
    pos2 := TGLM.Vec3(-1.0, -1.0, 0.0);
    pos3 := TGLM.Vec3( 1.0, -1.0, 0.0);
    pos4 := TGLM.Vec3( 1.0,  1.0, 0.0);

    // texture coordinates
    uv1:= TGLM.Vec2(0.0, 1.0);
    uv2:= TGLM.Vec2(0.0, 0.0);
    uv3:= TGLM.Vec2(1.0, 0.0);
    uv4:= TGLM.Vec2(1.0, 1.0);

    // normal vector
    nm := TGLM.Vec3(0.0, 0.0, 1.0);

    // calculate tangent/bitangent vectors of both triangles
    tangent1 := TGLM.Vec3(0);
    bitangent1 := TGLM.Vec3(0);

    tangent2 := TGLM.Vec3(0);
    bitangent2:= TGLM.Vec3(0);

    // triangle 1
    // ----------
    edge1 := pos2 - pos1;
    edge2 := pos3 - pos1;
    deltaUV1 := uv2 - uv1;
    deltaUV2 := uv3 - uv1;

    f := float(0);
    f := 1.0 / (deltaUV1.x * deltaUV2.y - deltaUV2.x * deltaUV1.y);

    tangent1.x := f * (deltaUV2.y * edge1.x - deltaUV1.y * edge2.x);
    tangent1.y := f * (deltaUV2.y * edge1.y - deltaUV1.y * edge2.y);
    tangent1.z := f * (deltaUV2.y * edge1.z - deltaUV1.y * edge2.z);
    tangent1 := TGLM.Normalize(tangent1);

    bitangent1.x := f * (-deltaUV2.x * edge1.x + deltaUV1.x * edge2.x);
    bitangent1.y := f * (-deltaUV2.x * edge1.y + deltaUV1.x * edge2.y);
    bitangent1.z := f * (-deltaUV2.x * edge1.z + deltaUV1.x * edge2.z);
    bitangent1 := TGLM.Normalize(bitangent1);

    // triangle 2
    // ----------
    edge1 := pos3 - pos1;
    edge2 := pos4 - pos1;
    deltaUV1 := uv3 - uv1;
    deltaUV2 := uv4 - uv1;

    f := 1.0 / (deltaUV1.x * deltaUV2.y - deltaUV2.x * deltaUV1.y);

    tangent2.x := f * (deltaUV2.y * edge1.x - deltaUV1.y * edge2.x);
    tangent2.y := f * (deltaUV2.y * edge1.y - deltaUV1.y * edge2.y);
    tangent2.z := f * (deltaUV2.y * edge1.z - deltaUV1.y * edge2.z);
    tangent2 := TGLM.Normalize(tangent2);

    bitangent2.x := f * (-deltaUV2.x * edge1.x + deltaUV1.x * edge2.x);
    bitangent2.y := f * (-deltaUV2.x * edge1.y + deltaUV1.x * edge2.y);
    bitangent2.z := f * (-deltaUV2.x * edge1.z + deltaUV1.x * edge2.z);
    bitangent2 := TGLM.Normalize(bitangent2);

    quadVertices := TArr_GLfloat([
      // positions            // normal         // texcoords  // tangent                          // bitangent
      pos1.x, pos1.y, pos1.z, nm.x, nm.y, nm.z, uv1.x, uv1.y, tangent1.x, tangent1.y, tangent1.z, bitangent1.x, bitangent1.y, bitangent1.z,
      pos2.x, pos2.y, pos2.z, nm.x, nm.y, nm.z, uv2.x, uv2.y, tangent1.x, tangent1.y, tangent1.z, bitangent1.x, bitangent1.y, bitangent1.z,
      pos3.x, pos3.y, pos3.z, nm.x, nm.y, nm.z, uv3.x, uv3.y, tangent1.x, tangent1.y, tangent1.z, bitangent1.x, bitangent1.y, bitangent1.z,

      pos1.x, pos1.y, pos1.z, nm.x, nm.y, nm.z, uv1.x, uv1.y, tangent2.x, tangent2.y, tangent2.z, bitangent2.x, bitangent2.y, bitangent2.z,
      pos3.x, pos3.y, pos3.z, nm.x, nm.y, nm.z, uv3.x, uv3.y, tangent2.x, tangent2.y, tangent2.z, bitangent2.x, bitangent2.y, bitangent2.z,
      pos4.x, pos4.y, pos4.z, nm.x, nm.y, nm.z, uv4.x, uv4.y, tangent2.x, tangent2.y, tangent2.z, bitangent2.x, bitangent2.y, bitangent2.z
    ]);

    // configure plane VAO
    glGenVertexArrays(1, @quadVAO);
    glGenBuffers(1, @quadVBO);
    glBindVertexArray(quadVAO);
    glBindBuffer(GL_ARRAY_BUFFER, quadVBO);
    glBufferData(GL_ARRAY_BUFFER, quadVertices.MemSize, @quadVertices[0], GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 14 * SIZE_OF_F, Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 14 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 14 * SIZE_OF_F, Pointer(6 * SIZE_OF_F));
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 3, GL_FLOAT, GL_FALSE, 14 * SIZE_OF_F, Pointer(8 * SIZE_OF_F));
    glEnableVertexAttribArray(4);
    glVertexAttribPointer(4, 3, GL_FLOAT, GL_FALSE, 14 * SIZE_OF_F, Pointer(11 * SIZE_OF_F));
  end;

  glBindVertexArray(quadVAO);
  glDrawArrays(GL_TRIANGLES, 0, 6);
  glBindVertexArray(0);
end;

procedure RenderCube;
begin
  cubeVAO := Cardinal(0);
  cubeVBO := Cardinal(0);
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

  if (glfwGetKey(window, GLFW_KEY_SPACE) = GLFW_PRESS) and (not hdrKeyPressed) then
  begin
    hdr := not hdr;
    hdrKeyPressed := true;
  end
  else if glfwGetKey(window, GLFW_KEY_SPACE) = GLFW_RELEASE then
  begin
    hdrKeyPressed := false;
  end;

  if glfwGetKey(window, GLFW_KEY_Q) = GLFW_PRESS then
  begin
    if heightScale > 0.0 then
      heightScale -= 0.0005
    else
      heightScale := 0.0;
  end
  else if glfwGetKey(window, GLFW_KEY_E) = GLFW_PRESS then
  begin
    if heightScale < 1.0 then
      heightScale += 0.0005
    else
      heightScale := 1.0;
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
