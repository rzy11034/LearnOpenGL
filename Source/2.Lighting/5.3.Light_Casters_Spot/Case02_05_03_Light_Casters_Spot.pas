﻿unit Case02_05_03_Light_Casters_Spot;

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
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.Camera,
  DeepStar.OpenGL.Texture;

  // 每当窗口大小发生变化(由操作系统或用户调整大小)，这个回调函数就会执行
procedure Framebuffer_size_callback(window: PGLFWwindow; witdth, Height: integer); cdecl; forward;
// 每当鼠标滚轮滚动时，这个回调就被调用
procedure Scroll_callback(window: PGLFWwindow; xoffset, yoffset: double); cdecl; forward;
// 每当鼠标移动时，就调用这个回调
procedure Mouse_callback(window: PGLFWwindow; xpos, ypos: double); cdecl; forward;
// 处理所有输入:查询GLFW是否按下/释放了相关的键，并做出相应的反应
procedure ProcessInput(window: PGLFWwindow); forward;
// glfw & glad  初始化
function InitWindows: PGLFWwindow; forward;
// 加载贴图
function LoadTexture(fileName: string): cardinal; forward;

const
  SCR_WIDTH = 800;
  SCR_HEIGHT = 600;

var
  camera: TCamera;

  deltaTime: GLfloat = 0.0;  // time between current frame and last frame
  lastFrame: GLfloat = 0.0;

  firstMouse: boolean = true;
  //偏航被初始化为-90.0度，因为0.0的偏航导致一个指向右的方向矢量，所以我们最初
  //向左旋转一点。
  lastX: GLfloat = SCR_WIDTH / 2.0;
  lastY: GLfloat = SCR_HEIGHT / 2.0;

  lightPos: TVec3 = (v:(1.2, 1.0, 2.0));

procedure Main;
const
  fs = '..\Source\2.Lighting\5.3.Light_Casters_Spot\5.3.light_casters.fs';
  vs = '..\Source\2.Lighting\5.3.Light_Casters_Spot\5.3.light_casters.vs';
  light_cube_fs = '..\Source\2.Lighting\5.3.Light_Casters_Spot\5.3.light_cube.fs';
  light_cube_vs = '..\Source\2.Lighting\5.3.Light_Casters_Spot\5.3.light_cube.vs';
  diffuseTexture = '..\Resources\textures\container2.png';
  specularTexture = '..\Resources\textures\container2_specular.png';
var
  window: PGLFWwindow;
  lightingShader, lightCubeShader: TShaderProgram;
  vertices: TArr_GLfloat;
  cubeVAO, VBO, lightCubeVAO: GLuint;
  projection, view, model: TMat4;
  currentFrame, angle: GLfloat;
  diffuseMap, specularMap: Cardinal;
  cubePositions: TArr_TVec3;
  i: Integer;
begin
  window := InitWindows;
  if window = nil then
  begin
    glfwTerminate;
    Exit;
  end;

  lightingShader := TShaderProgram.Create;
  lightCubeShader := TShaderProgram.Create;
  camera := TCamera.Create(TGLM.Vec3(0, 0, 3));
  try
    lightingShader.LoadShaderFile(vs, fs);
    lightCubeShader.LoadShaderFile(light_cube_vs, light_cube_fs);

    vertices := TArr_GLfloat([
      // positions         // normals          // texture coords
      -0.5, -0.5, -0.5,    0.0,  0.0, -1.0,    0.0, 0.0,
       0.5, -0.5, -0.5,    0.0,  0.0, -1.0,    1.0, 0.0,
       0.5,  0.5, -0.5,    0.0,  0.0, -1.0,    1.0, 1.0,
       0.5,  0.5, -0.5,    0.0,  0.0, -1.0,    1.0, 1.0,
      -0.5,  0.5, -0.5,    0.0,  0.0, -1.0,    0.0, 1.0,
      -0.5, -0.5, -0.5,    0.0,  0.0, -1.0,    0.0, 0.0,

      -0.5, -0.5,  0.5,    0.0,  0.0,  1.0,    0.0, 0.0,
       0.5, -0.5,  0.5,    0.0,  0.0,  1.0,    1.0, 0.0,
       0.5,  0.5,  0.5,    0.0,  0.0,  1.0,    1.0, 1.0,
       0.5,  0.5,  0.5,    0.0,  0.0,  1.0,    1.0, 1.0,
      -0.5,  0.5,  0.5,    0.0,  0.0,  1.0,    0.0, 1.0,
      -0.5, -0.5,  0.5,    0.0,  0.0,  1.0,    0.0, 0.0,

      -0.5,  0.5,  0.5,   -1.0,  0.0,  0.0,    1.0, 0.0,
      -0.5,  0.5, -0.5,   -1.0,  0.0,  0.0,    1.0, 1.0,
      -0.5, -0.5, -0.5,   -1.0,  0.0,  0.0,    0.0, 1.0,
      -0.5, -0.5, -0.5,   -1.0,  0.0,  0.0,    0.0, 1.0,
      -0.5, -0.5,  0.5,   -1.0,  0.0,  0.0,    0.0, 0.0,
      -0.5,  0.5,  0.5,   -1.0,  0.0,  0.0,    1.0, 0.0,

       0.5,  0.5,  0.5,    1.0,  0.0,  0.0,    1.0, 0.0,
       0.5,  0.5, -0.5,    1.0,  0.0,  0.0,    1.0, 1.0,
       0.5, -0.5, -0.5,    1.0,  0.0,  0.0,    0.0, 1.0,
       0.5, -0.5, -0.5,    1.0,  0.0,  0.0,    0.0, 1.0,
       0.5, -0.5,  0.5,    1.0,  0.0,  0.0,    0.0, 0.0,
       0.5,  0.5,  0.5,    1.0,  0.0,  0.0,    1.0, 0.0,

      -0.5, -0.5, -0.5,    0.0, -1.0,  0.0,    0.0, 1.0,
       0.5, -0.5, -0.5,    0.0, -1.0,  0.0,    1.0, 1.0,
       0.5, -0.5,  0.5,    0.0, -1.0,  0.0,    1.0, 0.0,
       0.5, -0.5,  0.5,    0.0, -1.0,  0.0,    1.0, 0.0,
      -0.5, -0.5,  0.5,    0.0, -1.0,  0.0,    0.0, 0.0,
      -0.5, -0.5, -0.5,    0.0, -1.0,  0.0,    0.0, 1.0,

      -0.5,  0.5, -0.5,    0.0,  1.0,  0.0,    0.0, 1.0,
       0.5,  0.5, -0.5,    0.0,  1.0,  0.0,    1.0, 1.0,
       0.5,  0.5,  0.5,    0.0,  1.0,  0.0,    1.0, 0.0,
       0.5,  0.5,  0.5,    0.0,  1.0,  0.0,    1.0, 0.0,
      -0.5,  0.5,  0.5,    0.0,  1.0,  0.0,    0.0, 0.0,
      -0.5,  0.5, -0.5,    0.0,  1.0,  0.0,    0.0, 1.0]);

    cubePositions := TArr_TVec3([
      TGLM.Vec3( 0.0,  0.0,  0.0 ),
      TGLM.Vec3( 2.0,  5.0, -15.0),
      TGLM.Vec3(-1.5, -2.2, -2.5 ),
      TGLM.Vec3(-3.8, -2.0, -12.3),
      TGLM.Vec3( 2.4, -0.4, -3.5 ),
      TGLM.Vec3(-1.7,  3.0, -7.5 ),
      TGLM.Vec3( 1.3, -2.0, -2.5 ),
      TGLM.Vec3( 1.5,  2.0, -2.5 ),
      TGLM.Vec3( 1.5,  0.2, -1.5 ),
      TGLM.Vec3(-1.3,  1.0, -1.5 )]);

    cubeVAO := GLuint(0);
    VBO := GLuint(0);
    glGenVertexArrays(1, @cubeVAO);
    glGenBuffers(1, @VBO);

    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, vertices.MemSize, @vertices[0], GL_STATIC_DRAW);

    glBindVertexArray(cubeVAO);

    // 位置属性
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(0));
    glEnableVertexAttribArray(0);
    // 法向量
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));
    glEnableVertexAttribArray(1);
    // 漫反射贴图坐标
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(6 * SIZE_OF_F));
    glEnableVertexAttribArray(2);

    // 第二，配置灯的VAO (VBO保持不变;对于同样是3D立方体的光物体，顶点是相同的)
    lightCubeVAO := GLuint(0);
    glGenVertexArrays(1, @lightCubeVAO);
    glBindVertexArray(lightCubeVAO);

    glBindBuffer(GL_ARRAY_BUFFER, VBO);

    // 注意，我们更新了灯的位置属性的步幅来反映更新后的缓冲区数据
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(0));
    glEnableVertexAttribArray(0);

    // 加载纹理(现在使用一个实用函数来保持代码更有条理)
    diffuseMap := LoadTexture(diffuseTexture);
    specularMap := LoadTexture(specularTexture);

    lightingShader.UseProgram;
    lightingShader.SetUniformInt('material.diffuse', 0);
    lightingShader.SetUniformInt('material.specular', 1);

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
      glClearColor(0.1, 0.1, 0.1, 0.1);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      // 在设置uniforms/绘制对象时，请确保激活着色器
      lightingShader.UseProgram;
      lightingShader.SetUniformVec3('light.position', camera.Position);
      lightingShader.SetUniformVec3('light.direction', camera.Front);
      lightingShader.SetUniformFloat('light.cutOff', Cos(TGLM.Radians(12.5)));
      lightingShader.SetUniformVec3('viewPos', camera.Position);

      lightingShader.SetUniformFloat('light.ambient', 0.2, 0.2, 0.2);
      lightingShader.SetUniformFloat('light.diffuse', 0.5, 0.5, 0.5);
      lightingShader.SetUniformFloat('light.specular', 1.0, 1.0, 1.0);

      lightingShader.SetUniformFloat('light.constant',  1.0);
      lightingShader.SetUniformFloat('light.linear',    0.09);
      lightingShader.SetUniformFloat('light.quadratic', 0.032);

      // material properties
      lightingShader.SetUniformFloat('material.shininess', 32);

      // 视图/投影转换
      projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 100);
      view := camera.GetViewMatrix;
      lightingShader.SetUniformMatrix4fv('projection', projection);
      lightingShader.SetUniformMatrix4fv('view', view);

      // bind diffuse map
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, diffuseMap);
      // bind specular map
      glActiveTexture(GL_TEXTURE1);
      glBindTexture(GL_TEXTURE_2D, specularMap);

      glBindVertexArray(cubeVAO);
      glDrawArrays(GL_TRIANGLES, 0, 36);

      for i := 0 to High(cubePositions) do
      begin
        angle := GLfloat(20 * i);
        model := TGLM.Translate(TGLM.Mat4(1), cubePositions[i]);
        model := TGLM.Rotate(model, glfwGetTime * TGLM.Radians(angle), TGLM.Vec3(1, 1, 1));
        lightingShader.SetUniformMatrix4fv('model', model);
        glDrawArrays(GL_TRIANGLES, 0, 36);
      end;

      lightCubeShader.UseProgram;
      lightCubeShader.SetUniformMatrix4fv('projection', projection);
      lightCubeShader.SetUniformMatrix4fv('view', view);
      model := TGLM.Mat4_Identity;
      model := TGLM.Translate(model, lightPos);
      model := TGLM.Scale(model, TGLM.Vec3(0.2));
      lightCubeShader.SetUniformMatrix4fv('model', model);

      glBindVertexArray(lightCubeVAO);
      glDrawArrays(GL_TRIANGLES, 0, 36);

      // 交换缓冲区和轮询IO事件(键按/释放，鼠标移动等)。
      glfwSwapBuffers(window);
      glfwPollEvents;
    end;

    glDeleteVertexArrays(1, @cubeVAO);
    glDeleteVertexArrays(1, @lightCubeVAO);
    glDeleteBuffers(1, @VBO);
    glDeleteTextures(1, @diffuseMap);
    glDeleteTextures(1, @specularMap);
  finally
    camera.Free;
    lightCubeShader.Free;
    lightingShader.Free;

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
    WriteLn(' Failed to create GLFW window');
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

  glEnable(GL_DEPTH_TEST);

  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
  // 注册一个回调函数(Callback Function)，它会在每次窗口大小被调整的时候被调用
  glfwSetFramebufferSizeCallback(window, @Framebuffer_size_callback);
  glfwSetCursorPosCallback(window, @Mouse_callback);
  glfwSetScrollCallback(window, @Scroll_callback);

  Result := window;
end;

function LoadTexture(fileName: string): cardinal;
var
  texture_ID: GLuint;
  tx: TTexture;
begin
  texture_ID := GLuint(0);
  glGenTextures(1, @texture_ID);

  tx := TTexture.Create();
  try
    tx.LoadFormFile(fileName);

    glBindTexture(GL_TEXTURE_2D, texture_ID);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tx.Width, tx.Height, 0, GL_RGBA,
      GL_UNSIGNED_BYTE, tx.Pixels);
    glGenerateMipmap(GL_TEXTURE_2D);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    Result := texture_ID;
  finally
    tx.Destroy;
  end;
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

  if (glfwGetKey(window, GLFW_KEY_W) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.FORWARD, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_S) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.BACKWARD, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_A) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.LEFT, deltaTime);
  if (glfwGetKey(window, GLFW_KEY_D) = GLFW_PRESS) then
    camera.ProcessKeyboard(TCamera_Movement.RIGHT, deltaTime);
end;

procedure Mouse_callback(window: PGLFWwindow; xpos, ypos: double); cdecl;
var
  xoffset, yoffset: GLfloat;
begin
  if firstMouse then
  begin
    lastX := xpos;
    lastY := ypos;
    firstMouse := false;
  end;

  xoffset := GLfloat(xpos - lastX);
  yoffset := GLfloat(lastY - ypos);
  lastX := xpos;
  lastY := ypos;

  camera.ProcessMouseMovement(xoffset, yoffset);
end;

procedure Scroll_callback(window: PGLFWwindow; xoffset, yoffset: double); cdecl;
begin
  camera.ProcessMouseScroll(yoffset);
end;

end.

