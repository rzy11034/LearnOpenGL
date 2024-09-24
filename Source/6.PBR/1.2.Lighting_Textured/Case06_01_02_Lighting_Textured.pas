unit Case06_01_02_Lighting_Textured;

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
  DeepStar.OpenGL.Texture,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.Camera,
  DeepStar.OpenGL.Model;

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
// 加载贴图
function LoadTexture(fileName: string; gammaCorrection: boolean = false;
  inverse: boolean = true): cardinal; forward;

procedure RenderSphere; forward;

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
  lastX: float = SCR_WIDTH / 2.0;
  lastY: float = SCR_HEIGHT / 2.0;

  sphereVAO: Cardinal = 0;
  indexCount: Cardinal;

procedure Main;
const
  shader_path = '..\Source\6.PBR\1.2.Lighting_Textured\';
  pbr_vs = shader_path + '1.2.pbr.vs';
  pbr_fs = shader_path + '1.2.pbr.fs';

  img_path      = '..\Resources\textures\pbr\rusted_iron\';
  img_albedo    = img_path + 'albedo.png';
  img_normal    = img_path + 'normal.png';
  img_metallic  = img_path + 'metallic.png';
  img_roughness = img_path + 'roughness.png';
  img_ao        = img_path + 'ao.png';
var
  window: PGLFWwindow;
  lightPositions, lightColors: TArr_TVec3;
  nrRows, nrColumns, row, col, i: Integer;
  spacing: float;
  shader_managed, camera_managed: IInterface;
  shader: TShaderProgram;
  projection, view, model: TMat4;
  currentFrame: GLfloat;
  newPos: TVec3;
  albedo, normal, metallic, roughness, ao: Cardinal;
begin
  window := InitWindows;
  if window = nil then
  begin
    glfwTerminate;
    Exit;
  end;

  //═════════════════════════════════════════════════════════════════════════'

  camera_managed := IInterface(TCamera.Create(TGLM.Vec3(0, 0, 10)));
  camera := camera_managed as TCamera;

  //═════════════════════════════════════════════════════════════════════════

  // configure global opengl state
  glEnable(GL_DEPTH_TEST);

  //═════════════════════════════════════════════════════════════════════════

  shader_managed := IInterface(TShaderProgram.Create);
  shader := shader_managed as TShaderProgram;
  shader.LoadShaderFile(pbr_vs, pbr_fs);

  shader.SetUniformInt('albedoMap', 0);
  shader.SetUniformInt('normalMap', 1);
  shader.SetUniformInt('metallicMap', 2);
  shader.SetUniformInt('roughnessMap', 3);
  shader.SetUniformInt('aoMap', 4);

  //═════════════════════════════════════════════════════════════════════════4

  albedo    := LoadTexture(img_albedo);
  normal    := LoadTexture(img_normal);
  metallic  := LoadTexture(img_metallic);
  roughness := LoadTexture(img_roughness);
  ao        := LoadTexture(img_ao);

  //═════════════════════════════════════════════════════════════════════════

  // lights
  lightPositions := TArr_TVec3([TGLM.Vec3(0.0,  0.0, 10.0)]);
  lightColors := TArr_TVec3([TGLM.Vec3(150.0, 150.0, 150.0)]);

  nrRows    := integer(7);
  nrColumns := integer(7);
  spacing   := float(2.5);

  //═════════════════════════════════════════════════════════════════════════

  // 渲染前初始化静态着色器
  projection := TGLM.Perspective(TGLM.Radians(camera.Zoom), SCR_WIDTH / SCR_HEIGHT,
    0.1, 100.0);
  shader.UseProgram;
  shader.SetUniformMatrix4fv('projection', projection);

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
    glClearColor(0.1, 0.1, 0.1, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    shader.UseProgram;
    view := camera.GetViewMatrix;
    shader.SetUniformMatrix4fv('view', view);
    shader.SetUniformVec3('camPos', camera.Position);

    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, albedo);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_2D, normal);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, metallic);
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, roughness);
    glActiveTexture(GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_2D, ao);

    // 渲染 行*列 数球体的材质属性由纹理定义(他们都有相同的材质属性)
    model := TGLM.Mat4(1.0);
    for row := 0 to nrRows - 1 do
    begin
      for col := 0 to nrColumns - 1 do
      begin
          model := TGLM.Mat4(1.0);
          model := TGLM.Translate(model, TGLM.Vec3(
            (col - (nrColumns / 2)) * spacing,
            (row - (nrRows    / 2)) * spacing,
            0.0
            ));
          shader.SetUniformMatrix4fv('model', model);
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
        shader.SetUniformVec3('lightPositions[' + i.ToString + ']', newPos);
        shader.SetUniformVec3('lightColors[' + i.ToString + ']', lightColors[i]);

        model := TGLM.Mat4(1.0);
        model := TGLM.translate(model, newPos);
        model := TGLM.scale(model, TGLM.Vec3(0.5));
        shader.SetUniformMatrix4fv('model', model);
        //shader.setMat3("normalMatrix", TGLM.transpose(glm::inverse(glm::mat3(model))));
        renderSphere;
    end;

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

function LoadTexture(fileName: string; gammaCorrection: boolean; inverse: boolean): cardinal;
var
  texture_ID: GLuint;
  tx: TTexture;
  tx_managed: IInterface;
  internalFormat: GLenum;
begin
  texture_ID := GLuint(0);
  glGenTextures(1, @texture_ID);
  internalFormat := GLenum(0);

  tx_managed := IInterface(TTexture.Create);
  tx := tx_managed as TTexture;

  tx.LoadFormFile(fileName, inverse);

  if gammaCorrection then
    internalFormat := GL_SRGB
  else
    internalFormat := GL_RGBA;

  glBindTexture(GL_TEXTURE_2D, texture_ID);
  glTexImage2D(GL_TEXTURE_2D, 0, internalFormat, tx.Width, tx.Height, 0, GL_RGBA,
    GL_UNSIGNED_BYTE, tx.Pixels);
  glGenerateMipmap(GL_TEXTURE_2D);

  if tx.UseAlpha then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  end
  else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  end;

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  Result := texture_ID;
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
