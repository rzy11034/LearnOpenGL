unit Case01_06_04_Coordinate_Systems_Exercise1;

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
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.Texture;

const
  SCR_WIDTH = 800;
  SCR_HEIGHT = 600;

  // 每当窗口大小发生变化(由操作系统或用户调整大小)，这个回调函数就会执行
procedure Framebuffer_size_callback(window: PGLFWwindow; witdth, Height: integer); cdecl;
begin
  //确保视口匹配新的窗口尺寸;注意宽度和
  //高度将明显大于视网膜显示器上的指定。
  glViewport(0, 0, witdth, Height);
end;

// 处理所有输入:查询GLFW是否按下/释放了相关的键，并做出相应的反应
procedure ProcessInput(window: PGLFWwindow);
begin
  if glfwGetKey(window, GLFW_KEY_ESCAPE) = GLFW_PRESS then
    glfwSetWindowShouldClose(window, true.ToInteger);
end;

function InitWindows: PGLFWwindow;
var
  window: PGLFWwindow;
begin
  glfwInit;

  // 设置主要版本和次要版本
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  // 创建一个窗口对象
  window := glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, PGLchar('LearnOpenGL'), nil, nil);
  if window = nil then
  begin
    WriteLn(' Failed to create GLFW window');
    glfwTerminate;
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
  // 注册一个回调函数(Callback Function)，它会在每次窗口大小被调整的时候被调用
  glfwSetFramebufferSizeCallback(window, @Framebuffer_size_callback);

  Result := window;
end;

procedure Main;
const
  vs = '..\Source\1.Getting_Started\6.4.Coordinate_Systems_Exercise1\6.4.Coordinate_Systems.vs';
  fs = '..\Source\1.Getting_Started\6.4.Coordinate_Systems_Exercise1\6.4.Coordinate_Systems.fs';
  tx1 = '..\Resources\textures\container.jpg';
  tx2 = '..\Resources\textures\awesomeface.png';
var
  window: PGLFWwindow;
  shader: TShaderProgram;
  ot: TTexture;
  VAO, VBO, EBO, texture0, texture1: GLuint;
  model, projection, view: TMat4;
  i: integer;
  angle: GLfloat;
  vertices: TArr_GLfloat;
  cubePositions: TArr_TVec3;
begin
  Randomize;

  window := InitWindows;

  shader := TShaderProgram.Create;
  try
    shader.LoadShaderFile(vs, fs);

    vertices := TArr_GLfloat([
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

      -0.5, 0.5, -0.5,    0.0, 1.0,
       0.5, 0.5, -0.5,    1.0, 1.0,
       0.5, 0.5,  0.5,    1.0, 0.0,
       0.5, 0.5,  0.5,    1.0, 0.0,
      -0.5, 0.5,  0.5,    0.0, 0.0,
      -0.5, 0.5, -0.5,    0.0, 1.0]);

    cubePositions := TArr_TVec3([
      TGLM.Vec3( 0.0,  0.0,   0.0),
      TGLM.Vec3( 2.0,  5.0, -15.0),
      TGLM.Vec3(-1.5, -2.2,  -2.5),
      TGLM.Vec3(-3.8, -2.0, -12.3),
      TGLM.Vec3( 2.4, -0.4,  -3.5),
      TGLM.Vec3(-1.7,  3.0,  -7.5),
      TGLM.Vec3( 1.3, -2.0,  -2.5),
      TGLM.Vec3( 1.5,  2.0,  -2.5),
      TGLM.Vec3( 1.5,  0.2,  -1.5),
      TGLM.Vec3(-1.3,  1.0,  -1.5)]);

    VAO := GLuint(0);
    VBO := GLuint(0);
    EBO := GLuint(0);

    glGenVertexArrays(1, @VAO);
    glGenBuffers(1, @VBO);
    glGenBuffers(1, @EBO);

    glBindVertexArray(VAO);

    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, vertices.MemSize, @vertices[0], GL_STATIC_DRAW);

    // position attribute ---位置属性
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * SIZE_F, Pointer(0));
    glEnableVertexAttribArray(0);

    // texture coord attribute ---纹理坐标属性
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * SIZE_F, Pointer(3 * SIZE_F));
    glEnableVertexAttribArray(1);

    // 新建并加载一个纹理
    texture0 := GLuint(0);
    glGenTextures(1, @texture0);
    glBindTexture(GL_TEXTURE_2D, texture0);
    // 为当前绑定的纹理对象设置环绕、过滤方式
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    ot := TTexture.Create();
    try
      ot.LoadFormFile(tx1);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, ot.Width, ot.Height, 0,
        GL_RGBA, GL_UNSIGNED_BYTE, ot.Pixels);
      glGenerateMipmap(GL_TEXTURE_2D);
    finally
      ot.Free;
    end;

    texture1 := GLuint(0);
    glGenTextures(1, @texture1);
    glBindTexture(GL_TEXTURE_2D, texture1);
    // 为当前绑定的纹理对象设置环绕、过滤方式
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    ot := TTexture.Create();
    try
      ot.LoadFormFile(tx2);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, ot.Width, ot.Height, 0,
        GL_RGBA, GL_UNSIGNED_BYTE, ot.Pixels);
      glGenerateMipmap(GL_TEXTURE_2D);
    finally
      ot.Free;
    end;

    shader.UseProgram;
    shader.SetUniformInt('texture1', [0]);
    shader.SetUniformInt('texture2', [1]);

    // 取消此调用的注释以绘制线框多边形。
    //glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

    glEnable(GL_DEPTH_TEST);

    // 渲染循环
    while glfwWindowShouldClose(window) = 0 do
    begin
      // 输入
      ProcessInput(window);

      // render
      glClearColor(0.2, 0.3, 0.3, 1.0);
      //glClear(GL_COLOR_BUFFER_BIT);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, texture0);
      glActiveTexture(GL_TEXTURE1);
      glBindTexture(GL_TEXTURE_2D, texture1);

      view := TGLM.Mat4_Identity;
      projection := TGLM.Mat4_Identity;
      view := TGLM.Translate(view, TGLM.Vec3(0, 0, -3));
      projection := TGLM.Perspective(cos(glfwGetTime)+30, SCR_WIDTH / SCR_HEIGHT, 1, 100);
      shader.SetUniformMatrix4fv('view', view);
      shader.SetUniformMatrix4fv('projection', projection);

      for i := 0 to High(cubePositions) do
      begin
        model := TGLM.Mat4_Identity;

        angle := GLfloat(20 * i);
        model := TGLM.Translate(model, cubePositions[i]);
        model := TGLM.Rotate(model, glfwGetTime * TGLM.Radians(angle), TGLM.Vec3(1, 1, 1));

        shader.SetUniformMatrix4fv('model', model);
        glDrawArrays(GL_TRIANGLES, 0, 36);
      end;

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @VAO);
    glDeleteBuffers(1, @VBO);
    glDeleteBuffers(1, @EBO);
  finally
    shader.Free;
  end;

  // 释放 / 删除之前的分配的所有资源
  glfwTerminate;
end;

end.
