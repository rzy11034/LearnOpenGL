unit Case05_05_03_Parallax_Occlusion_Mapping;

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

  heightScale: float = 0.1;

  quadVAO: GLuint = 0;
  quadVBO: GLuint = 0;

procedure Main;
const
  dir_path = '..\Source\5.Advanced_Lighting\5.3.Parallax_Occlusion_Mapping\';
  normal_mapping_vs = dir_path + '5.3.parallax_mapping.vs';
  normal_mapping_fs = dir_path + '5.3.parallax_mapping.fs';
  {%H-}img_bricks2 = '..\Resources\textures\bricks2.jpg';
  {%H-}img_bricks2_normal = '..\Resources\textures\bricks2_normal.jpg';
  {%H-}img_bricks2_disp = '..\Resources\textures\bricks2_disp.jpg';
  {%H-}img_toy_box_diffuse = '..\Resources\textures\toy_box_diffuse.png';
  {%H-}img_toy_box_normal = '..\Resources\textures\toy_box_normal.png';
  {%H-}img_toy_box_disp = '..\Resources\textures\toy_box_disp.png';
var
  window: PGLFWwindow;
  currentFrame: GLfloat;
  projection, view, model: TMat4;
  diffuseMap, normalMap, heightMap: Cardinal;
  shader: TShaderProgram;
  lightPos: TVec3;
  shader_managed, camera_managed: IInterface;
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
  shader.LoadShaderFile(normal_mapping_vs, normal_mapping_fs);

  camera_managed := IInterface(TCamera.Create(TGLM.Vec3(0, 0, 3)));
  camera := camera_managed as TCamera;

  //═════════════════════════════════════════════════════════════════════════

  // load textures
  diffuseMap := LoadTexture(img_bricks2);
  normalMap := LoadTexture(img_bricks2_normal);
  heightMap := LoadTexture(img_bricks2_disp);

  //diffuseMap := LoadTexture(img_toy_box_diffuse);
  //normalMap := LoadTexture(img_toy_box_normal);
  //heightMap := LoadTexture(img_toy_box_disp);

  //═══════════════════════════════════════════════════════════════════════*)

  // shader configuration
  shader.UseProgram;
  shader.SetUniformInt('diffuseMap', 0);
  shader.SetUniformInt('normalMap', 1);
  shader.SetUniformInt('depthMap', 2);

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

    // 配置视图/投影矩阵
    projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 100.0);
    view := camera.GetViewMatrix;
    shader.UseProgram;
    shader.SetUniformMatrix4fv('projection', projection);
    shader.SetUniformMatrix4fv('view', view);

    // 渲染法线映射四边形
    model := TGLM.Mat4(1.0);
    model := TGLM.Rotate(model, TGLM.Radians(glfwGetTime * -10), TGLM.Normalize(TGLM.Vec3(1, 0, 1)));

    shader.SetUniformMatrix4fv('model', model);
    shader.SetUniformVec3('viewPos', camera.Position);
    shader.SetUniformVec3('lightPos', lightPos);
    shader.SetUniformFloat('heightScale', heightScale);
    WriteLn(heightScale);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, diffuseMap);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, normalMap);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, heightMap);
    RenderQuad;

    // 渲染光源(简单地在光源位置重新渲染一个较小的平面，以便调试/可视化)
    model := TGLM.Mat4(1.0);
    model := TGLM.Translate(model, lightPos);
    model := TGLM.Scale(model, TGLM.Vec3(0.1));
    shader.SetUniformMatrix4fv('model', model);
    RenderQuad;

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
