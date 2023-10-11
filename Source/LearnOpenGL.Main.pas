unit LearnOpenGL.Main;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  {%H-}GLAD_GL,
  {%H-}GLFW,
  {%H-}LearnOpenGL.Utils,
  {%H-}dynlibs,
  {%H-}ctypes,
  {%H-}Math;

procedure Run();

implementation

uses
  {%H-}Case01_02_Exercise_03;

procedure Test;
var
  nrAttributes: GLint;
  window: PGLFWwindow;
begin
  glfwInit;
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  window := glfwCreateWindow(100, 100, PGLchar('LearnOpenGL'), nil, nil);
  if window = nil then
  begin
    WriteLn(' Failed to create GLFW window');
    glfwTerminate;
  end;

  glfwMakeContextCurrent(window);

  if gladLoadGL(TLoadProc(@glfwGetProcAddress)) = false then
  begin
    WriteLn('Failed to initialize GLAD');
    Exit;
  end;

  nrAttributes := GLint(0);
  glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, @nrAttributes);
  WriteLn('Maximum nr of vertex attributes supported: ', nrAttributes);
  Exit;
end;

procedure Run();
begin
  Test;
  Main;
end;

end.
