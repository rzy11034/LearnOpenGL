unit Case01_02_Exercise_01;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  GLAD_GL,
  GLFW,
  LearnOpenGL.Utils;

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

  fragmentShaderSource: PGLchar = '#version 330 core' + LE
    + 'out vec4 FragColor;'
    + 'void main()'
    + '{'
    + '   FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);'
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
  vertexShader, fragmentShader, shaderProgram, VAO, VBO: GLuint;
  success: GLint;
  infoLog: TArr_GLchar;
  vertices: TArr_GLfloat;
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

  fragmentShader := GLuint(0);
  fragmentShader := glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(fragmentShader, 1, @fragmentShaderSource, nil);
  glCompileShader(fragmentShader);
  infoLog := nil;
  SetLength(infoLog, 512);
  success := false.ToInteger;
  glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, @success);
  if not success.ToBoolean then
  begin
    glGetShaderInfoLog(fragmentShader, 512, nil, @infoLog[0]);
    WriteLn('ERROR::SHADER::FRAGMENT::COMPILATION_FAILED' + LE, PGLchar(infoLog));
  end;

  shaderProgram := GLuint(0);
  shaderProgram := glCreateProgram();
  glAttachShader(shaderProgram, vertexShader);
  glAttachShader(shaderProgram, fragmentShader);
  glLinkProgram(shaderProgram);
  infoLog := nil;
  SetLength(infoLog, 512);
  success := false.ToInteger;
  glGetProgramiv(shaderProgram, GL_LINK_STATUS, @success);
  if not success.ToBoolean then
  begin
    glGetProgramInfoLog(shaderProgram, 512, nil, @infoLog[0]);
    WriteLn('ERROR::SHADER::SHADERPROGRAM::COMPILATION_FAILED' + LE, PAnsiChar(infoLog));
  end;

  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);

  VAO := GLuint(0);
  glGenVertexArrays(1, @VAO);
  glBindVertexArray(VAO);

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
  VBO := GLuint(0);
  glGenBuffers(1, @VBO);
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, TArrayUtils_GLfloat.MemorySize(vertices),
    @vertices[0], GL_STATIC_DRAW);

  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * SizeOf(GLfloat), Pointer(0));
  glEnableVertexAttribArray(0);

  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  //glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

  while glfwWindowShouldClose(window) = 0 do
  begin
    ProcessInput(window);

    glClearColor(0.2, 0.3, 0.3, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);

    glUseProgram(shaderProgram);
    glBindVertexArray(VAO);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    glfwSwapBuffers(window);
    glfwPollEvents;
  end;

  glDeleteVertexArrays(1, @VAO);
  glDeleteBuffers(1, @VBO);
  glDeleteProgram(shaderProgram);

  glfwTerminate;
end;

end.
