unit Case01_02_01_Hello_Triangle;

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

  fragmentShaderSource: PGLchar = '#version 330 core' + LE
    + 'out vec4 FragColor;'
    + 'void main()'
    + '{'
    + '   FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);'
    + '}';

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

procedure Main;
var
  window: PGLFWwindow;
  success, EBO: GLint;
  vertices: TArr_GLfloat;
  infoLog: TArr_GLchar;
  vertexShader, fragmentShader, shaderProgram, VBO, VAO: GLuint;
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

  // 用glCreateShader创建这个着色器
  vertexShader := GLuint(0);
  vertexShader := glCreateShader(GL_VERTEX_SHADER);
  // 把这个着色器源码附加到着色器对象上，然后编译它
  glShaderSource(vertexShader, 1, @vertexShaderSource, nil);
  glCompileShader(vertexShader);
  // 检测编译时错误
  success := GLint(false.ToInteger);
  infoLog := TArr_GLchar(nil);
  SetLength(infoLog, 512);
  glGetShaderiv(vertexShader, GL_COMPILE_STATUS, @success);
  // 获取错误消息，然后打印它。
  if not success.ToBoolean then
  begin
    glGetShaderInfoLog(vertexShader, 512, nil, @infoLog[0]);
    WriteLn('ERROR::SHADER::VERTEX::COMPILATION_FAILED' + LE, PGLchar(infoLog));
  end;

  // 编译片段着色器的过程与顶点着色器类似，
  // 只不过使用GL_FRAGMENT_SHADER常量作为着色器类型：
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

  // 把每个着色器的输出链接到下个着色器的输入
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

  //记得删除着色器对象
  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);

  // 创建一个VAO
  VAO := GLuint(0);
  glGenVertexArrays(1, @VAO);
  glBindVertexArray(VAO);

  // 顶点数组模
  //设置顶点数据(和缓冲区)并配置顶点属性
  vertices := TArr_GLfloat([
    -0.5, -0.5, 0.0,
     0.5, -0.5, 0.0,
     0.0,  0.5, 0.0]);

  // 生成一个VBO对象, 新创建的缓冲绑定到GL_ARRAY_BUFFER目标上
  VBO := GLuint(0);
  glGenBuffers(1, @VBO);
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  // 把之前定义的顶点数据复制到缓冲的内存中
  glBufferData(GL_ARRAY_BUFFER, vertices.MemSize, @vertices[0], GL_STATIC_DRAW);
  // 解析顶点数据
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 3 * SizeOf(GLfloat), Pointer(0));
  glEnableVertexAttribArray(0);

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
    glUseProgram(shaderProgram);
    // 画出第一个三角形
    glBindVertexArray(VAO);
    glDrawArrays(GL_TRIANGLES, 0, 3);

    // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
    glfwSwapBuffers(window);
    glfwPollEvents;
  end;


  glDeleteVertexArrays(1, @VAO);
  glDeleteBuffers(1, @VBO);
  glDeleteBuffers(1, @EBO);
  glDeleteProgram(shaderProgram);

  // 释放 / 删除之前的分配的所有资源
  glfwTerminate;
end;

end.
