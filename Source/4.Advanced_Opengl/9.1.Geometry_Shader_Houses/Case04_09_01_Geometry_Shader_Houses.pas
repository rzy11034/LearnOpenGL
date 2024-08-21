unit Case04_09_01_Geometry_Shader_Houses;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.GLFW;

procedure Main;

implementation

// glfw & glad  初始化
function InitWindows: PGLFWwindow; forward;

const
  SCR_WIDTH = 800;
  SCR_HEIGHT = 600;

procedure Main;
const
  vs = '..\Source\4.Advanced_Opengl\9.1.Geometry_Shader_Houses\9.1.geometry_shader.vs';
  fs = '..\Source\4.Advanced_Opengl\9.1.Geometry_Shader_Houses\9.1.geometry_shader.fs';
  gs = '..\Source\4.Advanced_Opengl\9.1.Geometry_Shader_Houses\9.1.geometry_shader.gs';
var
  window: PGLFWwindow;
  points: TArr_GLfloat;
  VAO, VBO: cardinal;
  shader: TShaderProgram;
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

    points := TArr_GLfloat([
      -0.5,  0.5, 1.0, 0.0, 0.0, // top-left
       0.5,  0.5, 0.0, 1.0, 0.0, // top-right
       0.5, -0.5, 0.0, 0.0, 1.0, // bottom-right
      -0.5, -0.5, 1.0, 1.0, 0.0  // bottom-left
     ]);

    //═════════════════════════════════════════════════════════════════════════

    // cube VAO
    VAO := GLuint(0);
    VBO := GLuint(0);

    glGenVertexArrays(1, @VAO);
    glGenBuffers(1, @VBO);

    glBindVertexArray(VAO);
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, points.MemSize, @points[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(2 * SIZE_OF_F));

    glBindVertexArray(0);

    //═════════════════════════════════════════════════════════════════════════

    shader.LoadShaderFile(vs, fs, gs);

    //═════════════════════════════════════════════════════════════════════════

    // 渲染循环
    while not glfwWindowShouldClose(window).ToBoolean do
    begin
      // render
      glClearColor(0.1, 0.1, 0.1, 1.0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      shader.UseProgram;
      glBindVertexArray(VAO);
      glDrawArrays(GL_POINTS, 0, 4);

      //═════════════════════════════════════════════════════════════════════════

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @VAO);
    glDeleteBuffers(1, @VBO);
  finally
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

  Result := window;
end;

end.

