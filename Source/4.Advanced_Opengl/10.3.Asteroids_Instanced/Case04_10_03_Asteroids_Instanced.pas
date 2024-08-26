unit Case04_10_03_Asteroids_Instanced;

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
  dir_path = '..\Source\4.Advanced_Opengl\10.3.Asteroids_Instanced\';
  asteroids_vs = dir_path + '10.3.asteroids.vs';
  asteroids_fs = dir_path + '10.3.asteroids.fs';
  planet_vs = dir_path + '10.3.planet.vs';
  planet_fs = dir_path + '10.3.planet.fs';
  rock_model = '..\Resources\objects\rock\rock.obj';
  planet_model = '..\Resources\objects\planet\planet.obj';
var
  window: PGLFWwindow;
  currentFrame, offset: GLfloat;
  asteroidShader, planetShader: TShaderProgram;
  quadVAO, quadVBO, amount, buffer, VAO: Cardinal;
  rock, planet: TModel;
  modelMatrices: TArr_TMat4;
  radius, angle, displacement, x, y, z, scale, rotAngle: float;
  model, projection, view: TMat4;
  i: Integer;
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

  asteroidShader := TShaderProgram.Create;
  planetShader := TShaderProgram.Create;

  camera := TCamera.Create(TGLM.Vec3(0, 0, 55));

  rock := TModel.Create(rock_model);
  planet := TModel.Create(planet_model);

  try
    asteroidShader.LoadShaderFile(asteroids_vs, asteroids_fs);
    planetShader.LoadShaderFile(planet_vs, planet_fs);

    //═════════════════════════════════════════════════════════════════════════

    Randomize;

    // 生成一个大的半随机模型转换矩阵列表
    amount := cardinal(100000);
    modelMatrices := TArr_TMat4(nil);
    SetLength(modelMatrices, amount);

    radius := float(150.0);
    offset := float(25);

    for i := 0 to amount-1 do
    begin
      model := TGLM.Mat4_Identity;

      // 1.Translate: 在[-offset, offset]的范围内，沿半径圆移动
      angle := float(i / amount * 360.0);
      displacement := float(Random(Trunc(2 * 2.5 * 100)) /100 - offset);

      //保持小行星场的高度小于x和z的宽度
      x := float(Sin(angle) * radius + displacement);
      displacement := Random(Trunc(2 * 2.5 * 100)) / 100 - offset;
      y := float(displacement * 0.4);
      displacement := Random(Trunc(2 * 2.5 * 100)) / 100 - offset;
      z := float(Cos(angle) * radius + displacement);
      model := TGLM.Translate(model, TGLM.Vec3(x, y, z));

      // 2. scale: 在0.05到0.25f之间缩放
      scale := float(Random(20) / 100 + 0.05);
      model := TGLM.Scale(model, TGLM.Vec3(scale));

      // 3. 旋转:围绕(半)随机选择的旋转轴矢量添加随机旋转
      rotAngle := float(Random(360));
      model := TGLM.Rotate(model, rotAngle, TGLM.Vec3(0.4, 0.6, 0.8));

      // 4. 现在添加到矩阵列表中
      modelMatrices[i] := model;
    end;

    //═════════════════════════════════════════════════════════════════════════

    // 配置实例数组
    buffer := cardinal(0);
    glGenBuffers(1, @buffer);
    glBindBuffer(GL_ARRAY_BUFFER, buffer);
    glBufferData(GL_ARRAY_BUFFER, amount * SizeOf(TMat4), @modelMatrices[0], GL_STATIC_DRAW);

    // 把变换矩阵设置为实例顶点属性（带有除数1）
    // 注意：我们稍微作弊了一下，通过获取模型网格的现在已公开声明的VAO，并添加新的vertexAttribPointers
    // 通常情况下，你会想以更有组织的方式来做这件事，但是为了学习目的，这样做就可以了。
    for i := 0 to rock.Meshes.Count - 1 do
    begin
      VAO := rock.Meshes[i].VAO;
      glBindVertexArray(VAO);

      // set attribute pointers for matrix (4 times vec4)
      glEnableVertexAttribArray(3);
      glVertexAttribPointer(3, 4, GL_FLOAT, GL_FALSE, SizeOf(TMat4), Pointer(0));
      glEnableVertexAttribArray(4);
      glVertexAttribPointer(4, 4, GL_FLOAT, GL_FALSE, SizeOf(TMat4), Pointer(SizeOf(TVec4)));
      glEnableVertexAttribArray(5);
      glVertexAttribPointer(5, 4, GL_FLOAT, GL_FALSE, SizeOf(TMat4), Pointer(2 * SizeOf(TVec4)));
      glEnableVertexAttribArray(6);
      glVertexAttribPointer(6, 4, GL_FLOAT, GL_FALSE, SizeOf(TMat4), Pointer(3 * SizeOf(TVec4)));

      glVertexAttribDivisor(3, 1);
      glVertexAttribDivisor(4, 1);
      glVertexAttribDivisor(5, 1);
      glVertexAttribDivisor(6, 1);

      glBindVertexArray(0);
    end;

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

      projection := TGLM.Perspective(TGLM.Radians(45), SCR_WIDTH / SCR_HEIGHT, 0.1, 1000);
      view := camera.GetViewMatrix;

      asteroidShader.UseProgram;
      asteroidShader.SetUniformMatrix4fv('projection', projection);
      asteroidShader.SetUniformMatrix4fv('view', view);

      planetShader.UseProgram;
      planetShader.SetUniformMatrix4fv('projection', projection);
      planetShader.SetUniformMatrix4fv('view', view);

      // draw planet
      model := TGLM.Mat4_Identity;
      model := TGLM.Translate(model, TGLM.Vec3(0.0, -3.0, 0.0));
      model := TGLM.Scale(model, TGLM.Vec3(4.0, 4.0, 4.0));
      planetShader.SetUniformMatrix4fv('model', model);
      planet.Draw(planetShader);

      // draw meteorites
      asteroidShader.UseProgram;
      asteroidShader.SetUniformInt('texture_diffuse1', [0]);
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, rock.textures_loaded[0].ID);

      for i := 0 to rock.Meshes.Count - 1 do
      begin
        glBindVertexArray(rock.Meshes[i].VAO);
        glDrawElementsInstanced(GL_TRIANGLES, rock.Meshes[i].Indices.Count,
          GL_UNSIGNED_INT, nil, amount);
        glBindVertexArray(0);
      end;

      //═════════════════════════════════════════════════════════════════════════

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @quadVAO);
    glDeleteBuffers(1, @quadVBO);
  finally
    planet.Free;
    rock.Free;

    camera.Free;
    asteroidShader.Free;
    planetShader.Free;

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

