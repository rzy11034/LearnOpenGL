unit Case06_02_01_01_IBL_Irradiance_Conversion;

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
  DeepStar.OpenGL.Camera,
  DeepStar.OpenGL.Model,

  Imaging,
  ImagingTypes;

procedure Main;

implementation

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

procedure RenderSphere; forward;
procedure RenderCube; forward;

const
  SCR_WIDTH  = 1280;
  SCR_HEIGHT = 720;

var
  camera: TCamera;

  deltaTime: float = 0.0;  // time between current frame and last frame
  lastFrame: float = 0.0;

  firstMouse: boolean = true;

  //偏航被初始化为-90.0度，因为0.0的偏航导致一个指向右的方向矢量，所以我们最初
  //向左旋转一点。
  lastX: float = 800 / 2.0;
  lastY: float = 600 / 2.0;

  sphereVAO: Cardinal = 0;
  indexCount: Cardinal;

  cubeVAO: Cardinal = 0;
  cubeVBO: Cardinal = 0;

procedure Main;
const
  shader_path = '..\Source\6.PBR\2.1.1.IBL_Irradiance_Conversion\';
  pbr_vs = shader_path + '2.1.1.pbr.vs';
  pbr_fs = shader_path + '2.1.1.pbr.fs';
  cubemap_vs = shader_path + '2.1.1.cubemap.vs';
  equirectangular_to_cubemap_fs = shader_path + '2.1.1.equirectangular_to_cubemap.fs';

  img_path      = '..\Resources\textures\hdr\';
  img_newport_loft = img_path + 'newport_loft.hdr';
var
  window: PGLFWwindow;
  camera_managed, pbrShader_managed, equirectangularToCubemapShader_managed, backgroundShader_managed: IInterface;
  pbrShader, equirectangularToCubemapShader, backgroundShader: TShaderProgram;
  lightColors, lightPositions: TArr_TVec3;
  nrRows, nrColumns, scrWidth, scrHeight, row, col, i: Integer;
  spacing: float;
  captureFBO, captureRBO, hdrTexture, envCubemap: Cardinal;
  hdrData: TImageData;
  captureProjection, model, view, projection: TMat4;
  currentFrame: GLfloat;
  newPos: TVec3;
  captureViews: TArr_TMat4;
begin
  window := InitWindows;
  if window = nil then
  begin
    glfwTerminate;
    Exit;
  end;

  //═════════════════════════════════════════════════════════════════════════'

  camera_managed := IInterface(TCamera.Create(TGLM.Vec3(0, 0, 3)));
  camera := camera_managed as TCamera;

  //═════════════════════════════════════════════════════════════════════════

  // configure global opengl state
  glEnable(GL_DEPTH_TEST);

  // 设置深度函数小于等于天空盒的深度技巧。
  glDepthFunc(GL_LEQUAL);

  //═════════════════════════════════════════════════════════════════════════

  pbrShader_managed := IInterface(TShaderProgram.Create);
  pbrShader := pbrShader_managed as TShaderProgram;
  pbrShader.LoadShaderFile(pbr_vs, pbr_fs);

  equirectangularToCubemapShader_managed := IInterface(TShaderProgram.Create);
  equirectangularToCubemapShader := equirectangularToCubemapShader_managed as TShaderProgram;
  equirectangularToCubemapShader.LoadShaderFile(cubemap_vs, equirectangular_to_cubemap_fs);

  backgroundShader_managed := IInterface(TShaderProgram.Create);
  backgroundShader := backgroundShader_managed as TShaderProgram;

  pbrShader.UseProgram;
  pbrShader.SetUniformVec3('albedo', TGLM.Vec3(0.5, 0.0, 0.0));
  pbrShader.SetUniformFloat('ao', 1.0);

  backgroundShader.UseProgram;
  backgroundShader.SetUniformInt('environmentMap', 0);

  //═════════════════════════════════════════════════════════════════════════

  // lights
  lightPositions := TArr_TVec3([
    TGLM.Vec3(-10.0,  10.0, 10.0),
    TGLM.Vec3( 10.0,  10.0, 10.0),
    TGLM.Vec3(-10.0, -10.0, 10.0),
    TGLM.Vec3( 10.0, -10.0, 10.0)]);


  lightColors := TArr_TVec3([
    TGLM.Vec3(300.0, 300.0, 300.0),
    TGLM.Vec3(300.0, 300.0, 300.0),
    TGLM.Vec3(300.0, 300.0, 300.0),
    TGLM.Vec3(300.0, 300.0, 300.0)]);

  nrRows    := integer(7);
  nrColumns := integer(7);
  spacing   := float(2.5);

  //═════════════════════════════════════════════════════════════════════════

  // pbr: setup framebuffer
  captureFBO := Cardinal(0);
  captureRBO := Cardinal(0);
  glGenFramebuffers(1, @captureFBO);
  glGenRenderbuffers(1, @captureRBO);

  glBindFramebuffer(GL_FRAMEBUFFER, captureFBO);
  glBindRenderbuffer(GL_RENDERBUFFER, captureRBO);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, 512, 512);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, captureRBO);

  //═════════════════════════════════════════════════════════════════════════

  // pbr: load the HDR environment map
  if Imaging.LoadImageFromFile(img_newport_loft, hdrData) then
  begin
    Imaging.FlipImage(hdrData);

    hdrTexture := Cardinal(0);
    glGenTextures(1, @hdrTexture);

    glBindTexture(GL_TEXTURE_2D, hdrTexture);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_BGR, hdrData.width, hdrData.height,
      0, GL_RGB, GL_FLOAT, hdrData.Bits); // 注意我们是如何将纹理的数据值指定为float的

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    FreeImage(hdrData);
  end
  else
  begin
    WriteLn('Failed to load HDR image.');
  end;

  //═════════════════════════════════════════════════════════════════════════

  // Pbr:设置立方体映射以渲染和附加到framebuffer
  envCubemap := Cardinal(0);
  glGenTextures(1, @envCubemap);
  glBindTexture(GL_TEXTURE_CUBE_MAP, envCubemap);

  for i := 0 to 5 do
  begin
    glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_RGB16F, 512, 512,
      0, GL_BGR, GL_FLOAT, nil);
  end;

  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  //═════════════════════════════════════════════════════════════════════════

  // Pbr:设置投影矩阵和视图矩阵，用于将数据捕获到6个立方体图面方向上
  captureProjection := TGLM.perspective(TGLM.Radians(90.0), 1.0, 0.1, 10.0);
  captureViews := TArr_TMat4([
    TGLM.LookAt(TGLM.Vec3(0.0, 0.0, 0.0), TGLM.Vec3( 1.0,  0.0,  0.0), TGLM.Vec3(0.0, -1.0,  0.0)),
    TGLM.LookAt(TGLM.Vec3(0.0, 0.0, 0.0), TGLM.Vec3(-1.0,  0.0,  0.0), TGLM.Vec3(0.0, -1.0,  0.0)),
    TGLM.LookAt(TGLM.Vec3(0.0, 0.0, 0.0), TGLM.Vec3( 0.0,  1.0,  0.0), TGLM.Vec3(0.0,  0.0,  1.0)),
    TGLM.LookAt(TGLM.Vec3(0.0, 0.0, 0.0), TGLM.Vec3( 0.0, -1.0,  0.0), TGLM.Vec3(0.0,  0.0, -1.0)),
    TGLM.LookAt(TGLM.Vec3(0.0, 0.0, 0.0), TGLM.Vec3( 0.0,  0.0,  1.0), TGLM.Vec3(0.0, -1.0,  0.0)),
    TGLM.LookAt(TGLM.Vec3(0.0, 0.0, 0.0), TGLM.Vec3( 0.0,  0.0, -1.0), TGLM.Vec3(0.0, -1.0,  0.0))
    ]);

  //═════════════════════════════════════════════════════════════════════════

  // 将HDR等矩形环境映射转换为等量立方体映射
  equirectangularToCubemapShader.UseProgram;
  equirectangularToCubemapShader.SetUniformInt('equirectangularMap', 0);
  equirectangularToCubemapShader.SetUniformMatrix4fv('projection', captureProjection);

  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, hdrTexture);

  glViewport(0, 0, 512, 512); // 不要忘记将视口配置为捕获维度。
  glBindFramebuffer(GL_FRAMEBUFFER, captureFBO);
  for i := 0 to 5 do
  begin
    equirectangularToCubemapShader.SetUniformMatrix4fv('view', captureViews[i]);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
      GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, envCubemap, 0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    RenderCube;
  end;

  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  //═════════════════════════════════════════════════════════════════════════

  // 在渲染前初始化静态着色器 uniforms
  projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT, 0.1, 100.0);
  pbrShader.UseProgram;
  pbrShader.SetUniformMatrix4fv('projection', projection);
  backgroundShader.UseProgram;
  backgroundShader.SetUniformMatrix4fv('projection', projection);

  // 然后在呈现之前，将视口配置为原始framebuffer的屏幕尺寸
  scrWidth := Integer(0);
  scrHeight := Integer(0);
  glfwGetFramebufferSize(window, scrWidth, scrHeight);
  glViewport(0, 0, scrWidth, scrHeight);

  //═════════════════════════════════════════════════════════════════════════

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
    glClearColor(0.2, 0.3, 0.3, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    // 渲染场景，为最终的着色器提供复杂的光照贴图。
    pbrShader.UseProgram;
    view := camera.GetViewMatrix();
    pbrShader.SetUniformMatrix4fv('view', view);
    pbrShader.SetUniformVec3('camPos', camera.Position);

    // 渲染 行*列 数的球体，不同的金属/粗糙度值分别按行和列缩放
    model := TGLM.mat4(1.0);
    for row := 0 to nrRows - 1 do
    begin
      pbrShader.SetUniformFloat('metallic', row / nrRows);
      for col := 0 to nrColumns - 1 do
      begin
        // we clamp the roughness to 0.025 - 1.0 as perfectly smooth surfaces (roughness of 0.0) tend to look a bit off
        // on direct lighting.
        pbrShader.SetUniformFloat('roughness', TGLM.Clamp(col / nrColumns, 0.05, 1.0));

        model := TGLM.mat4(1.0);
        model := TGLM.translate(model, TGLM.Vec3((col - (nrColumns / 2)) * spacing,
          (row - (nrRows / 2)) * spacing, -2.0));
        pbrShader.SetUniformMatrix4fv('model', model);
        pbrShader.SetUniformMatrix3fv('normalMatrix',
          TGLM.TransposeMat3(TGLM.InverseMat3(TGLM.Mat3(model))));
        RenderSphere;
      end;
    end;

    // 渲染光源(简单地在光源位置重新渲染球体)
    // 这看起来有点偏离，因为我们使用相同的着色器，但它会使他们的位置明显和
    // 保持代码小。
    for i := 0 to High(lightPositions) do
    begin
      newPos := lightPositions[i] + TGLM.Vec3(Sin(glfwGetTime * 5.0) * 5.0, 0.0, 0.0);
      newPos := lightPositions[i];
      pbrShader.SetUniformVec3('lightPositions[' + i.ToString + ']', newPos);
      pbrShader.SetUniformVec3('lightColors[' + i.ToString + ']', lightColors[i]);

      model := TGLM.mat4(1.0);
      model := TGLM.Translate(model, newPos);
      model := TGLM.Scale(model, TGLM.Vec3(0.5));
      pbrShader.SetUniformMatrix4fv('model', model);
      pbrShader.SetUniformMatrix3fv('normalMatrix',
        TGLM.TransposeMat3(TGLM.InverseMat3(TGLM.Mat3(model))));
      RenderSphere;
    end;

    // render skybox (render as last to prevent overdraw)
    backgroundShader.UseProgram;
    backgroundShader.SetUniformMatrix4fv('view', view);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_CUBE_MAP, envCubemap);
    RenderCube;

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
begin
  if not glfwInit.ToBoolean then Exit(nil);

  // 设置主要版本和次要版本
  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_SAMPLES, 4);
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

  // 设置窗口的维度(Dimension)
  glViewport(0, 0, SCR_WIDTH, SCR_HEIGHT);

  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

  // 注册一个回调函数(Callback Function)，它会在每次窗口大小被调整的时候被调用
  glfwSetFramebufferSizeCallback(window, @Framebuffer_size_callback);
  glfwSetCursorPosCallback(window, @Mouse_callback);
  glfwSetScrollCallback(window, @Scroll_callback);

  Result := window;
end;

procedure RenderSphere;
var
  vbo, ebo, X_SEGMENTS, Y_SEGMENTS, stride: Cardinal;
  positions_managed, uv_managed, normals_managed, indices_managed, data_managed: IInterface;
  positions, normals: TArrayList_TVec3;
  uv: TArrayList_TVec2;
  indices: TArrayList_Cardinal;
  x, y, i: Integer;
  xSegment, ySegment, xPos, yPos, zPos: float;
  oddRow: Boolean;
  data: TArrayList_Single;
  tempData: TArr_GLfloat;
  tempIndices: TArr_GLuint;
begin
  if sphereVAO = 0 then
  begin
    glGenVertexArrays(1, @sphereVAO);

    vbo := Cardinal(0);
    ebo := Cardinal(0);

    glGenBuffers(1, @vbo);
    glGenBuffers(1, @ebo);

    positions_managed := IInterface(TArrayList_TVec3.Create);
    positions := positions_managed as TArrayList_TVec3;

    uv_managed := IInterface(TArrayList_TVec2.Create);
    uv := uv_managed as TArrayList_TVec2;

    normals_managed := IInterface(TArrayList_TVec3.Create);
    normals := normals_managed as TArrayList_TVec3;

    indices_managed := IInterface(TArrayList_Cardinal.Create);
    indices := indices_managed as TArrayList_Cardinal;

    X_SEGMENTS := Cardinal(64);
    Y_SEGMENTS := Cardinal(64);

    for x := 0 to X_SEGMENTS do
    begin
      for y := 0 to Y_SEGMENTS do
      begin
        xSegment := x / X_SEGMENTS;
        ySegment := y / Y_SEGMENTS;
        xPos := float(Cos(xSegment * 2.0 * PI) * Sin(ySegment * PI));
        yPos := float(Cos(ySegment * PI));
        zPos := float(Sin(xSegment * 2.0 * PI) * Sin(ySegment * PI));

        positions.AddLast(TGLM.Vec3(xPos, yPos, zPos));
        uv.AddLast(TGLM.Vec2(xSegment, ySegment));
        normals.AddLast(TGLM.Vec3(xPos, yPos, zPos));
      end;
    end;

    oddRow := false;
    for y := 0 to Y_SEGMENTS - 1 do
    begin
      if not oddRow then // even rows: y = 0, y = 2; and so on
      begin
        for x := 0 to X_SEGMENTS do
        begin
          indices.AddLast(y       * (X_SEGMENTS + 1) + x);
          indices.AddLast((y + 1) * (X_SEGMENTS + 1) + x);
        end;
      end
      else
      begin
        for x := X_SEGMENTS downto  0 do
        begin
          indices.AddLast((y + 1) * (X_SEGMENTS + 1) + x);
          indices.AddLast(y       * (X_SEGMENTS + 1) + x);
        end;
      end;
      oddRow := not oddRow;
    end;
    indexCount := indices.Count;

    data_managed := IInterface(TArrayList_Single.Create);
    data := data_managed as TArrayList_Single;

    for i := 0 to positions.Count - 1 do
    begin
      data.AddLast(positions[i].x);
      data.AddLast(positions[i].y);
      data.AddLast(positions[i].z);

      if normals.Count > 0 then
      begin
        data.AddLast(normals[i].x);
        data.AddLast(normals[i].y);
        data.AddLast(normals[i].z);
      end;

      if uv.Count > 0 then
      begin
        data.AddLast(uv[i].x);
        data.AddLast(uv[i].y);
      end;
    end;

    glBindVertexArray(sphereVAO);

    tempData := TArr_GLfloat(nil);
    tempData := data.ToArray;
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, tempData.MemSize, @tempData[0], GL_STATIC_DRAW);

    tempIndices := TArr_GLuint(nil);
    tempIndices := indices.ToArray;
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, tempIndices.MemSize, @tempIndices[0], GL_STATIC_DRAW);

    stride := Cardinal((3 + 2 + 3) * SIZE_OF_F);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, stride, Pointer(0));
    glEnableVertexAttribArray(1);
	  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, stride, Pointer(3 * SIZE_OF_F));
    glEnableVertexAttribArray(2);
	  glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, stride, Pointer(6 * SIZE_OF_F));
  end;

  glBindVertexArray(sphereVAO);
  glDrawElements(GL_TRIANGLE_STRIP, indexCount, GL_UNSIGNED_INT, nil);
end;

procedure RenderCube;
var
  vertices: TArr_GLfloat;
begin
  // initialize (if necessary)
  if cubeVAO = 0 then
  begin
    vertices := TArr_GLfloat([
      // back face
      -1.0, -1.0, -1.0,  0.0,  0.0, -1.0, 0.0, 0.0, // bottom-left
       1.0,  1.0, -1.0,  0.0,  0.0, -1.0, 1.0, 1.0, // top-right
       1.0, -1.0, -1.0,  0.0,  0.0, -1.0, 1.0, 0.0, // bottom-right
       1.0,  1.0, -1.0,  0.0,  0.0, -1.0, 1.0, 1.0, // top-right
      -1.0, -1.0, -1.0,  0.0,  0.0, -1.0, 0.0, 0.0, // bottom-left
      -1.0,  1.0, -1.0,  0.0,  0.0, -1.0, 0.0, 1.0, // top-left
      // front face
      -1.0, -1.0,  1.0,  0.0,  0.0,  1.0, 0.0, 0.0, // bottom-left
       1.0, -1.0,  1.0,  0.0,  0.0,  1.0, 1.0, 0.0, // bottom-right
       1.0,  1.0,  1.0,  0.0,  0.0,  1.0, 1.0, 1.0, // top-right
       1.0,  1.0,  1.0,  0.0,  0.0,  1.0, 1.0, 1.0, // top-right
      -1.0,  1.0,  1.0,  0.0,  0.0,  1.0, 0.0, 1.0, // top-left
      -1.0, -1.0,  1.0,  0.0,  0.0,  1.0, 0.0, 0.0, // bottom-left
      // left face
      -1.0,  1.0,  1.0, -1.0,  0.0,  0.0, 1.0, 0.0, // top-right
      -1.0,  1.0, -1.0, -1.0,  0.0,  0.0, 1.0, 1.0, // top-left
      -1.0, -1.0, -1.0, -1.0,  0.0,  0.0, 0.0, 1.0, // bottom-left
      -1.0, -1.0, -1.0, -1.0,  0.0,  0.0, 0.0, 1.0, // bottom-left
      -1.0, -1.0,  1.0, -1.0,  0.0,  0.0, 0.0, 0.0, // bottom-right
      -1.0,  1.0,  1.0, -1.0,  0.0,  0.0, 1.0, 0.0, // top-right
      // right face
       1.0,  1.0,  1.0,  1.0,  0.0,  0.0, 1.0, 0.0, // top-left
       1.0, -1.0, -1.0,  1.0,  0.0,  0.0, 0.0, 1.0, // bottom-right
       1.0,  1.0, -1.0,  1.0,  0.0,  0.0, 1.0, 1.0, // top-right
       1.0, -1.0, -1.0,  1.0,  0.0,  0.0, 0.0, 1.0, // bottom-right
       1.0,  1.0,  1.0,  1.0,  0.0,  0.0, 1.0, 0.0, // top-left
       1.0, -1.0,  1.0,  1.0,  0.0,  0.0, 0.0, 0.0, // bottom-left
      // bottom face
      -1.0, -1.0, -1.0,  0.0, -1.0,  0.0, 0.0, 1.0, // top-right
       1.0, -1.0, -1.0,  0.0, -1.0,  0.0, 1.0, 1.0, // top-left
       1.0, -1.0,  1.0,  0.0, -1.0,  0.0, 1.0, 0.0, // bottom-left
       1.0, -1.0,  1.0,  0.0, -1.0,  0.0, 1.0, 0.0, // bottom-left
      -1.0, -1.0,  1.0,  0.0, -1.0,  0.0, 0.0, 0.0, // bottom-right
      -1.0, -1.0, -1.0,  0.0, -1.0,  0.0, 0.0, 1.0, // top-right
      // top face
      -1.0,  1.0, -1.0,  0.0,  1.0,  0.0, 0.0, 1.0, // top-left
       1.0,  1.0 , 1.0,  0.0,  1.0,  0.0, 1.0, 0.0, // bottom-right
       1.0,  1.0, -1.0,  0.0,  1.0,  0.0, 1.0, 1.0, // top-right
       1.0,  1.0,  1.0,  0.0,  1.0,  0.0, 1.0, 0.0, // bottom-right
      -1.0,  1.0, -1.0,  0.0,  1.0,  0.0, 0.0, 1.0, // top-left
      -1.0,  1.0,  1.0,  0.0,  1.0,  0.0, 0.0, 0.0  // bottom-left
    ]);

    glGenVertexArrays(1, @cubeVAO);
    glGenBuffers(1, @cubeVBO);

    // fill buffer
    glBindBuffer(GL_ARRAY_BUFFER, cubeVBO);
    glBufferData(GL_ARRAY_BUFFER, vertices.MemSize, @vertices[0], GL_STATIC_DRAW);

    // link vertex attributes
    glBindVertexArray(cubeVAO);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(3 * SIZE_OF_F));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * SIZE_OF_F, Pointer(6 * SIZE_OF_F));
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
  end;

  // render Cube
  glBindVertexArray(cubeVAO);
  glDrawArrays(GL_TRIANGLES, 0, 36);
  glBindVertexArray(0);
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

  if glfwGetKey(window, GLFW_KEY_W) = GLFW_PRESS then
    camera.ProcessKeyboard(TCamera_Movement.FORWARD, deltaTime);
  if glfwGetKey(window, GLFW_KEY_S) = GLFW_PRESS then
    camera.ProcessKeyboard(TCamera_Movement.BACKWARD, deltaTime);
  if glfwGetKey(window, GLFW_KEY_A) = GLFW_PRESS then
    camera.ProcessKeyboard(TCamera_Movement.LEFT, deltaTime);
  if glfwGetKey(window, GLFW_KEY_D) = GLFW_PRESS then
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
