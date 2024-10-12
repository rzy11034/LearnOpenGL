unit Case07_01_Debugging;

{$mode objfpc}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}
{$ModeSwitch implicitfunctionspecialization}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.Model,

  Imaging,
  ImagingTypes;

procedure Main;

implementation

// 每当窗口大小发生变化(由操作系统或用户调整大小)，这个回调函数就会执行
procedure Framebuffer_size_callback(window: PGLFWwindow; witdth, Height: integer); cdecl; forward;
// 处理所有输入:查询GLFW是否按下/释放了相关的键，并做出相应的反应
procedure ProcessInput(window: PGLFWwindow); forward;
// glfw & glad  初始化
function InitWindows: PGLFWwindow; forward;

procedure RenderQuad; forward;

function glCheckError(const fileName: string; line: integer): GLenum; forward;

procedure glDebugOutput(source: GLenum;
    typ: GLenum;
    id: GLuint;
    severity: GLenum;
    length: GLsizei;
    message: PGLchar;
    userParam: pointer); stdcall; forward;

const
  SCR_WIDTH  = 800;
  SCR_HEIGHT = 600;

var
  quadVAO: Cardinal = 0;
  quadVBO: Cardinal;

procedure Main;
const
  shader_path = '..\Source\7.In_Practice\1.Debugging\';
  debugging_vs = shader_path + 'debugging.vs';
  debugging_fs = shader_path + 'debugging.fs';

  img_path = '..\Resources\textures\';
  img_wood = img_path + 'wood.png';
var
  window: PGLFWwindow;
  shader_managed: IInterface;
  shader: TShaderProgram;
  cubeVAO, cubeVBO, texture: Cardinal;
  vertices: TArr_GLfloat;
  imgData: TImageData;
  projection, model: TMat4;
  rotationSpeed, angle: float;
begin
  window := InitWindows;
  if window = nil then
  begin
    glfwTerminate;
    Exit;
  end;

  //═════════════════════════════════════════════════════════════════════════

  // configure global opengl state
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);

  //═════════════════════════════════════════════════════════════════════════

  shader_managed := IInterface(TShaderProgram.Create);
  shader := shader_managed as TShaderProgram;
  shader.LoadShaderFile(debugging_vs, debugging_fs);

  //═════════════════════════════════════════════════════════════════════════

  cubeVAO := Cardinal(0);
  cubeVBO := Cardinal(0);

  vertices := TArr_GLfloat([
    // back face
    -0.5, -0.5, -0.5,  0.0,  0.0, // bottom-left
     0.5,  0.5, -0.5,  1.0,  1.0, // top-right
     0.5, -0.5, -0.5,  1.0,  0.0, // bottom-right
     0.5,  0.5, -0.5,  1.0,  1.0, // top-right
    -0.5, -0.5, -0.5,  0.0,  0.0, // bottom-left
    -0.5,  0.5, -0.5,  0.0,  1.0, // top-left
     // front face
    -0.5, -0.5,  0.5,  0.0,  0.0, // bottom-left
     0.5, -0.5,  0.5,  1.0,  0.0, // bottom-right
     0.5,  0.5,  0.5,  1.0,  1.0, // top-right
     0.5,  0.5,  0.5,  1.0,  1.0, // top-right
    -0.5,  0.5,  0.5,  0.0,  1.0, // top-left
    -0.5, -0.5,  0.5,  0.0,  0.0, // bottom-left
     // left face
    -0.5,  0.5,  0.5, -1.0,  0.0, // top-right
    -0.5,  0.5, -0.5, -1.0,  1.0, // top-left
    -0.5, -0.5, -0.5, -0.0,  1.0, // bottom-left
    -0.5, -0.5, -0.5, -0.0,  1.0, // bottom-left
    -0.5, -0.5,  0.5, -0.0,  0.0, // bottom-right
    -0.5,  0.5,  0.5, -1.0,  0.0, // top-right
     // right face
     0.5,  0.5,  0.5,  1.0,  0.0, // top-left
     0.5, -0.5, -0.5,  0.0,  1.0, // bottom-right
     0.5,  0.5, -0.5,  1.0,  1.0, // top-right
     0.5, -0.5, -0.5,  0.0,  1.0, // bottom-right
     0.5,  0.5,  0.5,  1.0,  0.0, // top-left
     0.5, -0.5,  0.5,  0.0,  0.0, // bottom-left
     // bottom face
    -0.5, -0.5, -0.5,  0.0,  1.0, // top-right
     0.5, -0.5, -0.5,  1.0,  1.0, // top-left
     0.5, -0.5,  0.5,  1.0,  0.0, // bottom-left
     0.5, -0.5,  0.5,  1.0,  0.0, // bottom-left
    -0.5, -0.5,  0.5,  0.0,  0.0, // bottom-right
    -0.5, -0.5, -0.5,  0.0,  1.0, // top-right
     // top face
    -0.5,  0.5, -0.5,  0.0,  1.0, // top-left
     0.5,  0.5,  0.5,  1.0,  0.0, // bottom-right
     0.5,  0.5, -0.5,  1.0,  1.0, // top-right
     0.5,  0.5,  0.5,  1.0,  0.0, // bottom-right
    -0.5,  0.5, -0.5,  0.0,  1.0, // top-left
    -0.5,  0.5,  0.5,  0.0,  0.0  // bottom-left
    ]);

  glGenVertexArrays(1, @cubeVAO);
  glGenBuffers(1, @cubeVBO);

  // fill buffer
  glBindBuffer(GL_ARRAY_BUFFER, cubeVBO);
  glBufferData(GL_ARRAY_BUFFER, vertices.MemSize, @vertices[0], GL_STATIC_DRAW);

  // link vertex attributes
  glBindVertexArray(cubeVAO);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(0));
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  //═════════════════════════════════════════════════════════════════════════

  // load cube texture
  texture := Cardinal(0);
  glGenTextures(1, @texture);
  glBindTexture(GL_TEXTURE_2D, texture);

  if Imaging.LoadImageFromFile(img_wood, imgData) then
  begin
    Imaging.FlipImage(imgData);
    Imaging.ConvertImage(imgData, TImageFormat.ifA8R8G8B8);

    glTexImage2D(GL_FRAMEBUFFER, 0, GL_RGB, imgData.Width, imgData.Height, 0,
      GL_BGRA, GL_UNSIGNED_BYTE, imgData.Bits);
    glGenerateMipmap(GL_TEXTURE_2D);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  end
  else
  begin
    WriteLn('Failed to load texture');
  end;

  FreeImage(imgData);

  //═════════════════════════════════════════════════════════════════════════

  // set up projection matrix
  projection := TGLM.Perspective(TGLM.Radians(45.0), SCR_WIDTH / SCR_HEIGHT, 0.1, 10.0);
  glUniformMatrix4fv(glGetUniformLocation(shader.ID, 'projection'), 1, GL_FALSE,
    TGLM.ValuePtr(projection));
  glUniform1i(glGetUniformLocation(shader.ID, 'tex'), 0);

  //═════════════════════════════════════════════════════════════════════════

  // 渲染循环
  while not glfwWindowShouldClose(window).ToBoolean do
  begin
    // 输入
    ProcessInput(window);

    // render
    glClearColor(0.2, 0.3, 0.3, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    shader.UseProgram();
    rotationSpeed := float(10.0);
    angle := float(glfwGetTime * rotationSpeed);

    model := TGLM.Mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(0.0, 0.0, -2.5));
    model := TGLM.Rotate(model, TGLM.Radians(angle), TGLM.Vec3(1.0, 1.0, 1.0));
    glUniformMatrix4fv(glGetUniformLocation(shader.ID, 'model'), 1, GL_FALSE,
      TGLM.ValuePtr(model));

    glBindTexture(GL_TEXTURE_2D, texture);
    glBindVertexArray(cubeVAO);
    glDrawArrays(GL_TRIANGLES, 0, 36);
    glBindVertexArray(0);

    // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
    glfwSwapBuffers(window);
    glfwPollEvents;
  end;

  // 释放 / 删除之前的分配的所有资源
  glfwTerminate;
end;

function InitWindows: PGLFWwindow;
var
  window: PGLFWwindow = nil;
  flags: Integer;
begin
  if not glfwInit.ToBoolean then Exit(nil);

  // 设置主要版本和次要版本
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  // 在发布版本中注释这一行!
  glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, True.ToInteger);

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

  // 如果上下文允许调试上下文，则启用OpenGL调试上下文
  flags := 0;
  glGetIntegerv(GL_CONTEXT_FLAGS, @flags);

  if (flags and GL_CONTEXT_FLAG_DEBUG_BIT).ToBoolean then
  begin
    glEnable(GL_DEBUG_OUTPUT);
    glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);

    // 确保同步显示错误
    glDebugMessageCallback(@glDebugOutput, nil);
    glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nil, GL_TRUE);
  end;

  Result := window;
end;

procedure RenderQuad;
var
  quadVertices: TArr_GLfloat;
begin
  if quadVAO = 0 then
  begin
    quadVertices := TArr_GLfloat([
      // positions     // texture Coords
      -1.0,  1.0, 0.0, 0.0, 1.0,
      -1.0, -1.0, 0.0, 0.0, 0.0,
       1.0,  1.0, 0.0, 1.0, 1.0,
       1.0, -1.0, 0.0, 1.0, 0.0]);

    // setup plane VAO
    glGenVertexArrays(1, @quadVAO);
    glGenBuffers(1, @quadVBO);
    glBindVertexArray(quadVAO);
    glBindBuffer(GL_ARRAY_BUFFER, quadVBO);
    glBufferData(GL_ARRAY_BUFFER, quadVertices.MemSize, @quadVertices[0], GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));

  end;

  glBindVertexArray(quadVAO);
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  glBindVertexArray(0);
end;

function glCheckError(const fileName: string; line: integer): GLenum;
var
  errorCode: GLenum;
  error: String;
begin
  errorCode := GLenum(0);
  errorCode := glGetError();

  while errorCode <> GL_NO_ERROR do
  begin
     error := '';

     case errorCode of
       GL_INVALID_ENUM:                  error := 'INVALID_ENUM';
       GL_INVALID_VALUE:                 error := 'INVALID_VALUE';
       GL_INVALID_OPERATION:             error := 'INVALID_OPERATION';
       GL_STACK_OVERFLOW:                error := 'STACK_OVERFLOW';
       GL_STACK_UNDERFLOW:               error := 'STACK_UNDERFLOW';
       GL_OUT_OF_MEMORY:                 error := 'OUT_OF_MEMORY';
       GL_INVALID_FRAMEBUFFER_OPERATION: error := 'INVALID_FRAMEBUFFER_OPERATION';
     end;

     WriteLn(error, ' | ', fileName, ' (', line, ')');
  end;

  Result := errorCode;
end;

procedure glDebugOutput(source: GLenum; typ: GLenum; id: GLuint; severity: GLenum;
    length: GLsizei; message: PGLchar; userParam: pointer); stdcall;
begin
  // 忽略这些不重要的错误代码
  if (id = 131169) or (id = 131185) or (id = 131218) or (id = 131204) then
    Exit;

  WriteLn('---------------');
  WriteLn('Debug message (', id, '): ', message);

  case source of
    GL_DEBUG_SOURCE_API:             WriteLn('Source: API');
    GL_DEBUG_SOURCE_WINDOW_SYSTEM:   WriteLn('Source: Window System');
    GL_DEBUG_SOURCE_SHADER_COMPILER: WriteLn('Source: Shader Compiler');
    GL_DEBUG_SOURCE_THIRD_PARTY:     WriteLn('Source: Third Party');
    GL_DEBUG_SOURCE_APPLICATION:     WriteLn('Source: Application');
    GL_DEBUG_SOURCE_OTHER:           WriteLn('Source: Other');
  end;
  WriteLn;

  case typ of
    GL_DEBUG_TYPE_ERROR:               WriteLn('Type: Error');
    GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR: WriteLn('Type: Deprecated Behaviour');
    GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR:  WriteLn('Type: Undefined Behaviour');
    GL_DEBUG_TYPE_PORTABILITY:         WriteLn('Type: Portability');
    GL_DEBUG_TYPE_PERFORMANCE:         WriteLn('Type: Performance');
    GL_DEBUG_TYPE_MARKER:              WriteLn('Type: Marker');
    GL_DEBUG_TYPE_PUSH_GROUP:          WriteLn('Type: Push Group');
    GL_DEBUG_TYPE_POP_GROUP:           WriteLn('Type: Pop Group');
    GL_DEBUG_TYPE_OTHER:               WriteLn('Type: Other');
  end;
  WriteLn;

  case severity of
    GL_DEBUG_SEVERITY_HIGH:         WriteLn('Severity: high');
    GL_DEBUG_SEVERITY_MEDIUM:       WriteLn('Severity: medium');
    GL_DEBUG_SEVERITY_LOW:          WriteLn('Severity: low');
    GL_DEBUG_SEVERITY_NOTIFICATION: WriteLn('Severity: notification');
  end;
  WriteLn;

  WriteLn;
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
end;

end.
