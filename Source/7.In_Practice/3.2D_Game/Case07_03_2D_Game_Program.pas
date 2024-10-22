unit Case07_03_2D_Game_Program;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.Model;

procedure Main;

implementation

uses
  Case07_03_2D_Game_Game;

// glfw & glad  初始化
function InitWindows: PGLFWwindow; forward;
procedure key_callback(p: PGLFWwindow; key, scancode, action, mode: longint); cdecl; forward;

const
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 600;

var
  deltaTime: GLfloat = 0.0;
  lastFrame: GLfloat = 0.0;

procedure Main;
var
  window: PGLFWwindow;
  breakout_managed: IInterface;
  breakout: TGame;
  currentFrame: float;
begin
  window := InitWindows;
  if window = nil then
  begin
    glfwTerminate;
    Exit;
  end;

  //═════════════════════════════════════════════════════════════════════════

  glEnable(GL_CULL_FACE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  //═════════════════════════════════════════════════════════════════════════

  breakout_managed := IInterface(TGame.Create(SCREEN_WIDTH, SCREEN_HEIGHT));
  breakout := breakout_managed as TGame;
  breakout.Init;
  breakout.State := GAME_ACTIVE;

  //═════════════════════════════════════════════════════════════════════════

  // 渲染循环
  while not glfwWindowShouldClose(window).ToBoolean do
  begin
    currentFrame := glfwGetTime;
    deltaTime := currentFrame - lastFrame;
    lastFrame := currentFrame;
    glfwPollEvents;

    deltaTime := 0.001;
    // Manage user input
    breakout.ProcessInput(deltaTime);

    // Update Game state
    breakout.Update(deltaTime);

    // render
    glClearColor(0.0, 0.0, 0.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);

    breakout.Render;

    // 交换缓冲区
    glfwSwapBuffers(window);
  end;

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
  glfwWindowHint(GLFW_RESIZABLE, GL_FALSE);

  // 创建一个窗口对象
  window := glfwCreateWindow(SCREEN_WIDTH, SCREEN_HEIGHT, string('Breakout'), nil, nil);
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
  glViewport(0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);

  //glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
  glfwSetKeyCallback(window, @key_callback);

  Result := window;
end;

procedure key_callback(p: PGLFWwindow; key, scancode, action, mode: longint); cdecl;
begin
  if (key = GLFW_KEY_ESCAPE) and (action =  GLFW_PRESS) then
    glfwSetWindowShouldClose(p, GL_TRUE);
end;

end.
