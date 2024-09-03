unit Case04_04_01_Face_Culling_Exercise1;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}

interface

uses
  Classes,
  SysUtils;

procedure Main;

implementation

uses
  DeepStar.Utils,
  DeepStar.DSA.Tree.PriorityQueue,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.Camera,
  DeepStar.OpenGL.Texture;

type
  TDistance = Record
    Distance: single;
    Position: TVec3;
    class function Comparer(const a, b: TDistance): integer; static;
  end;

  TPriorityQueue_Distance = specialize TPriorityQueue<TDistance>;

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

procedure Main;
const
  fs = '..\Source\4.Advanced_Opengl\4.1.Face_Culling_Exercise1\4.1.Face_Culling.fs';
  vs = '..\Source\4.Advanced_Opengl\4.1.Face_Culling_Exercise1\4.1.Face_Culling.vs';
  imgMarble = '..\Resources\textures\marble.jpg';
  imgMetal = '..\Resources\textures\metal.png';
  imgTransparentTexture = '..\Resources\textures\window.png';
var
  window: PGLFWwindow;
  cubeVertices, planeVertices, transparentVertices: TArr_GLfloat;
  cubeVAO, cubeVBO, planeVAO, planeVBO, transparentVAO, transparentVBO: GLuint;
  cubeTexture, floorTexture, transparentTexture: Cardinal;
  shader: TShaderProgram;
  projection, view, model: TMat4;
  currentFrame: GLfloat;
  windows: TArr_TVec3;
  i: integer;
  sorted: TPriorityQueue_Distance;
  tempDistance: TDistance;
begin
  window := InitWindows;
  if window = nil then
  begin
    glfwTerminate;
    Exit;
  end;

  glEnable(GL_DEPTH_TEST);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  //═════════════════════════════════════════════════════════════════════════

  shader := TShaderProgram.Create;
  camera := TCamera.Create(TGLM.Vec3(0, 0, 3));
  sorted := TPriorityQueue_Distance.Create(@TDistance.Comparer, THeapkind.Max);

  try
    cubeVertices := TArr_GLfloat([
       // Back face
      -0.5, -0.5, -0.5,  0.0, 0.0, // Bottom-left
       0.5,  0.5, -0.5,  1.0, 1.0, // top-right
       0.5, -0.5, -0.5,  1.0, 0.0, // bottom-right
       0.5,  0.5, -0.5,  1.0, 1.0, // top-right
      -0.5, -0.5, -0.5,  0.0, 0.0, // bottom-left
      -0.5,  0.5, -0.5,  0.0, 1.0, // top-left
      // Front face
      -0.5, -0.5,  0.5,  0.0, 0.0, // bottom-left
       0.5, -0.5,  0.5,  1.0, 0.0, // bottom-right
       0.5,  0.5,  0.5,  1.0, 1.0, // top-right
       0.5,  0.5,  0.5,  1.0, 1.0, // top-right
      -0.5,  0.5,  0.5,  0.0, 1.0, // top-left
      -0.5, -0.5,  0.5,  0.0, 0.0, // bottom-left
      // Left face
      -0.5,  0.5,  0.5,  1.0, 0.0, // top-right
      -0.5,  0.5, -0.5,  1.0, 1.0, // top-left
      -0.5, -0.5, -0.5,  0.0, 1.0, // bottom-left
      -0.5, -0.5, -0.5,  0.0, 1.0, // bottom-left
      -0.5, -0.5,  0.5,  0.0, 0.0, // bottom-right
      -0.5,  0.5,  0.5,  1.0, 0.0, // top-right
      // Right face
       0.5,  0.5,  0.5,  1.0, 0.0, // top-left
       0.5, -0.5, -0.5,  0.0, 1.0, // bottom-right
       0.5,  0.5, -0.5,  1.0, 1.0, // top-right
       0.5, -0.5, -0.5,  0.0, 1.0, // bottom-right
       0.5,  0.5,  0.5,  1.0, 0.0, // top-left
       0.5, -0.5,  0.5,  0.0, 0.0, // bottom-left
      // Bottom face
      -0.5, -0.5, -0.5,  0.0, 1.0, // top-right
       0.5, -0.5, -0.5,  1.0, 1.0, // top-left
       0.5, -0.5,  0.5,  1.0, 0.0, // bottom-left
       0.5, -0.5,  0.5,  1.0, 0.0, // bottom-left
      -0.5, -0.5,  0.5,  0.0, 0.0, // bottom-right
      -0.5, -0.5, -0.5,  0.0, 1.0, // top-right
      // Top face
      -0.5,  0.5, -0.5,  0.0, 1.0, // top-left
       0.5,  0.5,  0.5,  1.0, 0.0, // bottom-right
       0.5,  0.5, -0.5,  1.0, 1.0, // top-right
       0.5,  0.5,  0.5,  1.0, 0.0, // bottom-right
      -0.5,  0.5, -0.5,  0.0, 1.0, // top-left
      -0.5,  0.5,  0.5,  0.0, 0.0]);  // bottom-left

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

    transparentVertices := TArr_GLfloat([
      // positions       // texture Coords
      0.0,  0.5,  0.0,   0.0,  1.0,
      0.0, -0.5,  0.0,   0.0,  0.0,
      1.0, -0.5,  0.0,   1.0,  0.0,

      0.0,  0.5,  0.0,   0.0,  1.0,
      1.0, -0.5,  0.0,   1.0,  0.0,
      1.0,  0.5,  0.0,   1.0,  1.0]);

    // transparent vegetation locations
    windows := TArr_TVec3([
      TGLM.Vec3(-1.5,  0.0, -0.48),
      TGLM.Vec3( 1.5,  0.0,  0.51),
      TGLM.Vec3( 0.0,  0.0,  0.7 ),
      TGLM.Vec3(-0.3,  0.0, -2.3 ),
      TGLM.Vec3( 0.5,  0.0, -0.6 )]);

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

    // transparent VAO
    transparentVAO := GLuint(0);
    transparentVBO := GLuint(0);

    glGenVertexArrays(1, @transparentVAO);
    glGenBuffers(1, @transparentVBO);

    glBindVertexArray(transparentVAO);
    glBindBuffer(GL_ARRAY_BUFFER, transparentVBO);
    glBufferData(GL_ARRAY_BUFFER, transparentVertices.MemSize, @transparentVertices[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(0));

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));

    //═════════════════════════════════════════════════════════════════════════

    // 加载纹理
    cubeTexture := LoadTexture(imgMarble);
    floorTexture := LoadTexture(imgMetal);
    transparentTexture := LoadTexture(imgTransparentTexture);

    // shader configuration
    shader.LoadShaderFile(vs, fs);
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

      for i := 0 to High(windows) do
      begin
        tempDistance := Default(TDistance);
        tempDistance.Distance := TGLM.Length(camera.Position - windows[i]);
        tempDistance.Position := windows[i];
        sorted.EnQueue(tempDistance);
      end;

      // render
      glClearColor(0.1, 0.1, 0.1, 1.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      shader.UseProgram;
      view := camera.GetViewMatrix;
      projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 100);
      shader.SetUniformMatrix4fv('view', view);
      shader.SetUniformMatrix4fv('projection', projection);

      glEnable(GL_CULL_FACE);
      glCullFace(GL_BACK);
      glFrontFace(GL_CCW);

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

      glDisable(GL_CULL_FACE);

      // floor
      glBindVertexArray(planeVAO);
      glBindTexture(GL_TEXTURE_2D, floorTexture);
      shader.SetUniformMatrix4fv('model', TGLM.Mat4_Identity);
      glDrawArrays(GL_TRIANGLES, 0, 6);

      // windows
      glBindVertexArray(transparentVAO);
      glBindTexture(GL_TEXTURE_2D, transparentTexture);
      while not sorted.IsEmpty do
      begin
        tempDistance := sorted.DeQueue;
        model := TGLM.Translate(TGLM.Mat4_Identity, tempDistance.Position);
        shader.SetUniformMatrix4fv('model', model);
        glDrawArrays(GL_TRIANGLES, 0, 6);
      end;

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
    glDeleteTextures(1, @transparentTexture);
  finally
    camera.Free;
    shader.Free;
    sorted.Free;

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

{ TDistance }

class function TDistance.Comparer(const a, b: TDistance): integer;
var
  res: Integer;
begin
  if a.Distance > b.Distance then
    res := 1
  else if a.Distance < b.Distance then
    res := -1
  else if a.Distance = b.Distance then
    res := 0;

  Result := res;
end;

end.

