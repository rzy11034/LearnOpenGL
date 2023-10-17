unit Case01_03_06_Shaders_Exercise3;

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
  GLAD_GL,
  GLFW,
  LearnOpenGL.Utils,
  LearnOpenGL.Shader;

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
    Exit;
  end;

  // 设置窗口的维度(Dimension)
  glViewport(0, 0, SCR_WIDTH, SCR_HEIGHT);
  // 注册一个回调函数(Callback Function)，它会在每次窗口大小被调整的时候被调用
  glfwSetFramebufferSizeCallback(window, @Framebuffer_size_callback);

  Result := window;
end;

procedure Main;
const
  vs = '..\..\Source\1.Getting_Started\3.6.Shaders_Exercise3\3.6.shader.vs';
  fs = '..\..\Source\1.Getting_Started\3.6.Shaders_Exercise3\3.6.shader.fs';
var
  window: PGLFWwindow;
  VAO, VBO: GLuint;
  vertices: TArr_GLfloat;
  shader: TShaderProgram;
begin
  window := InitWindows;

  shader := TShaderProgram.Create;
  try
    shader.LoadShaderFile(CrossFixFileName(vs), CrossFixFileName(fs));

    // 创建一个VAO
    VAO := GLuint(0);
    glGenVertexArrays(1, @VAO);
    glBindVertexArray(VAO);

    // 顶点数组模, 设置顶点数据(和缓冲区)并配置顶点属性
    vertices := TArr_GLfloat(nil);
    vertices := [
      // 位置          // 颜色
      +0.5, -0.5, 0.0, 1.0, 0.0, 0.0,   // 右下
      -0.5, -0.5, 0.0, 0.0, 1.0, 0.0,   // 左下
      +0.0, +0.5, 0.0, 0.0, 0.0, 1.0];  // 顶部
    // 生成一个VBO对象, 新创建的缓冲绑定到GL_ARRAY_BUFFER目标上
    VBO := GLuint(0);
    glGenBuffers(1, @VBO);
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    // 把之前定义的顶点数据复制到缓冲的内存中
    glBufferData(GL_ARRAY_BUFFER, TArrayUtils_GLfloat.MemorySize(vertices),
      @vertices[0], GL_STATIC_DRAW);
    // 解析顶点数据
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * SizeOf(GLfloat), Pointer(0));
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * SizeOf(GLfloat), Pointer(3 * SizeOf(GLfloat)));
    glEnableVertexAttribArray(1);

    // 取消此调用的注释以绘制线框多边形。
    //glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

    // 渲染循环
    while glfwWindowShouldClose(window) = 0 do
    begin
      // 输入
      ProcessInput(window);

      // render
      glClearColor(0.2, 0.3, 0.3, 1.0);
      glClear(GL_COLOR_BUFFER_BIT);

      //// 激活这个程序对象
      shader.UseProgram;
      // 画出第一个三角形
      glBindVertexArray(VAO);
      glDrawArrays(GL_TRIANGLES, 0, 3);

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @VAO);
    glDeleteBuffers(1, @VBO);
  finally
    shader.Free;
  end;

  // 释放 / 删除之前的分配的所有资源
  glfwTerminate;
end;

end.
