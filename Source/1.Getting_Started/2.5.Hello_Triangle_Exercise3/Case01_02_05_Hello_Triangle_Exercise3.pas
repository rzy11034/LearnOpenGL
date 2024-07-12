unit Case01_02_05_Hello_Triangle_Exercise3;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.Utils;

procedure Main;

implementation

const
  SCR_WIDTH = 800;
  SCR_HEIGHT = 600;

const
  vertexShaderSource: PGLchar = '#version 330 core' + LE
    + 'layout (location = 0) in vec3 aPos; '
    + 'void main() '
    + '{ '
    + '   gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0); '
    + '} ';

  fragmentShaderSource1: PGLchar = '#version 330 core' + LE
    + 'out vec4 FragColor;'
    + 'void main()'
    + '{'
    + '   FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);'
    + '}';

  fragmentShaderSource2: PGLchar = '#version 330 core' + LE
    + 'out vec4 FragColor;'
    + 'void main()'
    + '{'
    + '   FragColor = vec4(1.0f, 1.0f, 0.0f, 1.0f);'
    + '}';

procedure Framebuffer_size_callback(window: PGLFWwindow; witdth, Height: integer); cdecl;
begin
  glViewport(0, 0, witdth, Height);
end;

procedure ProcessInput(window: PGLFWwindow);
begin
  if glfwGetKey(window, GLFW_KEY_ESCAPE) = GLFW_PRESS then
    glfwSetWindowShouldClose(window, true.ToInteger);
end;

procedure Main;
var
  window: PGLFWwindow;
  vertexShader, fragmentShader1, fragmentShader2, shaderProgram1, shaderProgram2: GLuint;
  success: GLint;
  infoLog: TArr_GLchar;
  vertices: TArr_GLfloat;
  VAOs, VBOs: TArr_GLuint;
begin
  glfwInit;
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  window := glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, PGLchar('LearnOpenGL'), nil, nil);
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

  glViewport(0, 0, SCR_WIDTH, SCR_HEIGHT);
  glfwSetFramebufferSizeCallback(window, @Framebuffer_size_callback);

  vertexShader := GLuint(0);
  vertexShader := glCreateShader(GL_VERTEX_SHADER);
  glShaderSource(vertexShader, 1, @vertexShaderSource, nil);
  glCompileShader(vertexShader);
  success := GLint(false.ToInteger);
  infoLog := TArr_GLchar(nil);
  SetLength(infoLog, 512);
  glGetShaderiv(vertexShader, GL_COMPILE_STATUS, @success);
  if not success.ToBoolean then
  begin
    glGetShaderInfoLog(vertexShader, 512, nil, @infoLog[0]);
    WriteLn('ERROR::SHADER::VERTEX::COMPILATION_FAILED' + LE, PGLchar(infoLog));
  end;

  ////////////////////////////////////////////////////////////////////////////

  fragmentShader1 := GLuint(0);
  fragmentShader2 := GLuint(0);

  fragmentShader1 := glCreateShader(GL_FRAGMENT_SHADER);
  fragmentShader2 := glCreateShader(GL_FRAGMENT_SHADER);

  glShaderSource(fragmentShader1, 1, @fragmentShaderSource1, nil);
  glShaderSource(fragmentShader2, 1, @fragmentShaderSource2, nil);

  glCompileShader(fragmentShader1);
  infoLog := nil;
  SetLength(infoLog, 512);
  success := false.ToInteger;
  glGetShaderiv(fragmentShader1, GL_COMPILE_STATUS, @success);
  if not success.ToBoolean then
  begin
    glGetShaderInfoLog(fragmentShader1, 512, nil, @infoLog[0]);
    WriteLn('ERROR::SHADER::FRAGMENT1::COMPILATION_FAILED' + LE, PGLchar(infoLog));
  end;

  glCompileShader(fragmentShader2);
  infoLog := nil;
  SetLength(infoLog, 512);
  success := false.ToInteger;
  glGetShaderiv(fragmentShader2, GL_COMPILE_STATUS, @success);
  if not success.ToBoolean then
  begin
    glGetShaderInfoLog(fragmentShader2, 512, nil, @infoLog[0]);
    WriteLn('ERROR::SHADER::FRAGMENT2::COMPILATION_FAILED' + LE, PGLchar(infoLog));
  end;

  //////////////////////////////////////////////////////////////////////////

  shaderProgram1 := GLuint(0);
  shaderProgram2 := GLuint(0);

  shaderProgram1 := glCreateProgram();
  shaderProgram2 := glCreateProgram();

  glAttachShader(shaderProgram1, vertexShader);
  glAttachShader(shaderProgram1, fragmentShader1);
  glLinkProgram(shaderProgram1);

  glAttachShader(shaderProgram2, vertexShader);
  glAttachShader(shaderProgram2, fragmentShader2);
  glLinkProgram(shaderProgram2);

  infoLog := nil;
  SetLength(infoLog, 512);
  success := false.ToInteger;
  glGetProgramiv(shaderProgram1, GL_LINK_STATUS, @success);
  if not success.ToBoolean then
  begin
    glGetProgramInfoLog(shaderProgram1, 512, nil, @infoLog[0]);
    WriteLn('ERROR::SHADER::SHADERPROGRAM1::COMPILATION_FAILED' + LE, PGLchar(infoLog));
  end;

  infoLog := nil;
  SetLength(infoLog, 512);
  success := false.ToInteger;
  glGetProgramiv(shaderProgram2, GL_LINK_STATUS, @success);
  if not success.ToBoolean then
  begin
    glGetProgramInfoLog(shaderProgram2, 512, nil, @infoLog[0]);
    WriteLn('ERROR::SHADER::SHADERPROGRAM2::COMPILATION_FAILED' + LE, PGLchar(infoLog));
  end;

  /////////////////////////////////////////////////////////////////////////////

  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader1);
  glDeleteShader(fragmentShader2);

  vertices := TArr_GLfloat(nil);
  vertices := [
    // 第一个三角形
    -0.9, -0.5, 0.0,
    -0.0, -0.5, 0.0,
    -0.45, 0.5, 0.0,
    // 第二个三角形
    0.0, -0.5, 0.0,
    0.9, -0.5, 0.0,
    0.45, 0.5, 0.0];

  VAOs := TArr_GLuint(nil);
  VBOs := TArr_GLuint(nil);
  SetLength(VAOs, 2);
  SetLength(VBOs, 2);

  glGenVertexArrays(2, @VAOs[0]);
  glGenBuffers(2, @VBOs[0]);

  glBindBuffer(GL_ARRAY_BUFFER, VBOs[0]);
  glBindBuffer(GL_ARRAY_BUFFER, VBOs[1]);
  glBufferData(GL_ARRAY_BUFFER, vertices.MemSize, @vertices[0], GL_STATIC_DRAW);

  glBindVertexArray(VAOs[0]);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * SizeOf(GLfloat), Pointer(0));
  glEnableVertexAttribArray(0);

  glBindVertexArray(VAOs[1]);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * SizeOf(GLfloat), Pointer(9 * SizeOf(GLfloat)));
  glEnableVertexAttribArray(0);

  //glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

  while glfwWindowShouldClose(window) = 0 do
  begin
    ProcessInput(window);

    glClearColor(0.2, 0.3, 0.3, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);

    glUseProgram(shaderProgram1);
    glBindVertexArray(VAOs[0]);
    glDrawArrays(GL_TRIANGLES, 0, 3);

    glUseProgram(shaderProgram2);
    glBindVertexArray(VAOs[1]);
    glDrawArrays(GL_TRIANGLES, 0, 3);

    glfwSwapBuffers(window);
    glfwPollEvents;
  end;

  glDeleteVertexArrays(2, @VAOs[0]);
  glDeleteBuffers(2, @VBOs[0]);
  glDeleteProgram(shaderProgram1);
  glDeleteProgram(shaderProgram2);

  glfwTerminate;
end;

end.

