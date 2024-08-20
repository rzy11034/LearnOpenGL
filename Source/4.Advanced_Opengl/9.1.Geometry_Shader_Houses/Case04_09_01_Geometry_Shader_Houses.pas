unit Case04_09_01_Geometry_Shader_Houses;

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
// glfw & glad  初始化
function InitWindows: PGLFWwindow; forward;

const
  SCR_WIDTH = 800;
  SCR_HEIGHT = 600;

var
  deltaTime: float = 0.0;  // time between current frame and last frame
  lastFrame: float = 0.0;

  firstMouse: boolean = true;
  //偏航被初始化为-90.0度，因为0.0的偏航导致一个指向右的方向矢量，所以我们最初
  //向左旋转一点。
  lastX: float = SCR_WIDTH / 2.0;
  lastY: float = SCR_HEIGHT / 2.0;

procedure Main;
const
  vs = '..\Source\4.Advanced_Opengl\8.1.Advanced_GLSL_UBO\8.advanced_glsl.vs';
  blue_fs = '..\Source\4.Advanced_Opengl\8.1.Advanced_GLSL_UBO\8.blue.fs';
  green_fs = '..\Source\4.Advanced_Opengl\8.1.Advanced_GLSL_UBO\8.green.fs';
  red_fs = '..\Source\4.Advanced_Opengl\8.1.Advanced_GLSL_UBO\8.red.fs';
  yellow_fs = '..\Source\4.Advanced_Opengl\8.1.Advanced_GLSL_UBO\8.yellow.fs';
var
  window: PGLFWwindow;
  cubeVertices: TArr_GLfloat;
  cubeVAO, cubeVBO: cardinal;
  uniformBlockIndexRed, uniformBlockIndexGreen: cardinal;
  uniformBlockIndexBlue, uniformBlockIndexYellow: cardinal;
  uboMatrices: Cardinal;
  projection, view, model: TMat4;
  currentFrame: GLfloat;
  shaderRed, shaderGreen, shaderblue, shaderYellow: TShaderProgram;
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

  try

    cubeVertices := TArr_GLfloat([
      // positions
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
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * SIZE_OF_F, Pointer(0));

    //═════════════════════════════════════════════════════════════════════════

    // 配置一个统一的缓冲区对象
    // -------------------------
    // 第一。我们得到了相关的块索引
    uniformBlockIndexRed := Cardinal(0);
    uniformBlockIndexRed := glGetUniformBlockIndex(shaderRed.ID, 'Matrices');

    uniformBlockIndexGreen := Cardinal(0);
    uniformBlockIndexGreen := glGetUniformBlockIndex(shaderGreen.ID, 'Matrices');

    uniformBlockIndexBlue := Cardinal(0);
    uniformBlockIndexBlue := glGetUniformBlockIndex(shaderBlue.ID, 'Matrices');

    uniformBlockIndexYellow  := Cardinal(0);
    uniformBlockIndexYellow := glGetUniformBlockIndex(shaderYellow.ID, 'Matrices');

    // 然后我们将每个着色器的统一块链接到这个统一的绑定点
    glUniformBlockBinding(shaderRed.ID, uniformBlockIndexRed, 0);
    glUniformBlockBinding(shaderGreen.ID, uniformBlockIndexGreen, 0);
    glUniformBlockBinding(shaderBlue.ID, uniformBlockIndexBlue, 0);
    glUniformBlockBinding(shaderYellow.ID, uniformBlockIndexYellow, 0);

    // 现在实际创建缓冲区
    uboMatrices := cardinal(0);
    glGenBuffers(1, @uboMatrices);
    glBindBuffer(GL_UNIFORM_BUFFER, uboMatrices);
    glBufferData(GL_UNIFORM_BUFFER, 2 * SizeOf(TMat4), nil, GL_STATIC_DRAW);
    glBindBuffer(GL_UNIFORM_BUFFER, 0);
    // 定义链接到统一绑定点的缓冲区的范围
    glBindBufferRange(GL_UNIFORM_BUFFER, 0, uboMatrices, 0, 2 * SizeOf(TMat4));

    //═════════════════════════════════════════════════════════════════════════

    // 存储投影矩阵(我们现在只这样做一次)(注意:我们不再通过改变视场来使用缩放)
    projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 100);
    glBindBuffer(GL_UNIFORM_BUFFER, uboMatrices);
    glBufferSubData(GL_UNIFORM_BUFFER, 0, SizeOf(TMat4), TGLM.ValuePtr(projection));
    glBindBuffer(GL_UNIFORM_BUFFER, 0);

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

      // 在统一块中设置视图和投影矩阵-我们只需要在每次循环迭代中这样做一次。
      view := camera.GetViewMatrix;
      glBindBuffer(GL_UNIFORM_BUFFER, uboMatrices);
      glBufferSubData(GL_UNIFORM_BUFFER, SizeOf(TMat4), SizeOf(TMat4), TGLM.ValuePtr(view));
      glBindBuffer(GL_UNIFORM_BUFFER, 0);

      // 画4个立方体
      // -----------------
      // red
      glBindVertexArray(cubeVAO);
      shaderRed.UseProgram();
      model := TGLM.Mat4(1.0);
      model := TGLM.Translate(model, TGLM.Vec3(-0.75, 0.75, 0.0)); // move top-left
      shaderRed.SetUniformMatrix4fv('model', model);
      glDrawArrays(GL_TRIANGLES, 0, 36);
      // GREEN
      shaderGreen.UseProgram();
      model := TGLM.mat4(1.0);
      model := TGLM.translate(model, TGLM.vec3(0.75, 0.75, 0.0)); // move top-right
      shaderGreen.SetUniformMatrix4fv('model', model);
      glDrawArrays(GL_TRIANGLES, 0, 36);
      // YELLOW
      shaderYellow.UseProgram();
      model := TGLM.Mat4(1.0);
      model := TGLM.Translate(model, TGLM.vec3(-0.75, -0.75, 0.0)); // move bottom-left
      shaderYellow.SetUniformMatrix4fv('model', model);
      glDrawArrays(GL_TRIANGLES, 0, 36);
      // BLUE
      shaderBlue.UseProgram();
      model := TGLM.Mat4(1.0);
      model := TGLM.Translate(model, TGLM.Vec3(0.75, -0.75, 0.0)); // move bottom-right
      shaderBlue.SetUniformMatrix4fv('model', model);
      glDrawArrays(GL_TRIANGLES, 0, 36);

      //═════════════════════════════════════════════════════════════════════════

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @cubeVAO);
    glDeleteBuffers(1, @cubeVBO);
  finally
    camera.Free;
    shaderRed.Free;
    shaderGreen.Free;
    shaderblue.Free;
    shaderYellow.Free;

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

  glfwSetInputMode(window, GLFW_CURSOR);
  // 注册一个回调函数(Callback Function)，它会在每次窗口大小被调整的时候被调用
  glfwSetFramebufferSizeCallback(window, @Framebuffer_size_callback);
  glfwSetCursorPosCallback(window, @Mouse_callback);
  glfwSetScrollCallback(window, @Scroll_callback);

  Result := window;
end;

procedure Framebuffer_size_callback(window: PGLFWwindow; witdth, Height: integer); cdecl;
begin
  //确保视口匹配新的窗口尺寸;注意宽度和
  //高度将明显大于视网膜显示器上的指定。
  glViewport(0, 0, witdth, Height);
end;

end.

