﻿unit Case06_02_02_02_IBL_Specular_Textured;

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
  Math,
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
// 加载贴图
function LoadTexture(fileName: string): cardinal; forward;

procedure RenderSphere; forward;
procedure RenderCube; forward;
procedure RenderQuad; forward;

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

  quadVAO: Cardinal = 0;
  quadVBO: Cardinal = 0;

procedure Main;
const
  shader_path = '..\Source\6.PBR\2.2.2.IBL_Specular_Textured\';
  pbr_vs = shader_path + '2.2.2.pbr.vs';
  pbr_fs = shader_path + '2.2.2.pbr.fs';
  cubemap_vs = shader_path + '2.2.2.cubemap.vs';
  equirectangular_to_cubemap_fs = shader_path + '2.2.2.equirectangular_to_cubemap.fs';
  irradiance_convolution_fs = shader_path + '2.2.2.irradiance_convolution.fs';
  background_vs = shader_path + '2.2.2.background.vs';
  background_fs = shader_path + '2.2.2.background.fs';
  prefilter_fs = shader_path + '2.2.2.prefilter.fs';
  brdf_vs = shader_path + '2.2.2.brdf.vs';
  brdf_fs = shader_path + '2.2.2.brdf.fs';

  img_path = '..\Resources\textures\hdr\';
  img_newport_loft = img_path + 'newport_loft.hdr';

  tx_path = '..\Resources\textures\pbr\';
  img_rusted_iron_albedo = tx_path + 'rusted_iron\albedo.png';
  img_rusted_iron_normal = tx_path + 'rusted_iron\normal.png';
  img_rusted_iron_metallic = tx_path + 'rusted_iron\metallic.png';
  img_rusted_iron_roughness = tx_path + 'rusted_iron\roughness.png';
  img_rusted_iron_ao = tx_path + 'rusted_iron\ao.png';

  img_gold_albedo = tx_path + 'gold\albedo.png';
  img_gold_normal = tx_path + 'gold\normal.png';
  img_gold_metallic = tx_path + 'gold\metallic.png';
  img_gold_roughness = tx_path + 'gold\roughness.png';
  img_gold_ao = tx_path + 'gold\ao.png';

  img_grass_albedo = tx_path + 'grass\albedo.png';
  img_grass_normal = tx_path + 'grass\normal.png';
  img_grass_metallic = tx_path + 'grass\metallic.png';
  img_grass_roughness = tx_path + 'grass\roughness.png';
  img_grass_ao = tx_path + 'grass\ao.png';

  img_plastic_albedo = tx_path + 'plastic\albedo.png';
  img_plastic_normal = tx_path + 'plastic\normal.png';
  img_plastic_metallic = tx_path + 'plastic\metallic.png';
  img_plastic_roughness = tx_path + 'plastic\roughness.png';
  img_plastic_ao = tx_path + 'plastic\ao.png';

  img_wall_albedo = tx_path + 'wall\albedo.png';
  img_wall_normal = tx_path + 'wall\normal.png';
  img_wall_metallic = tx_path + 'wall\metallic.png';
  img_wall_roughness = tx_path + 'wall\roughness.png';
  img_wall_ao = tx_path + 'wall\ao.png';
var
  window: PGLFWwindow;
  camera_managed, pbrShader_managed, equirectangularToCubemapShader_managed,
    backgroundShader_managed, irradianceShader_managed, prefilterShader_managed,
    brdfShader_managed: IInterface;
  pbrShader, equirectangularToCubemapShader, backgroundShader,
    irradianceShader, prefilterShader, brdfShader: TShaderProgram;
  lightColors, lightPositions: TArr_TVec3;
  scrWidth, scrHeight, i, mip: Integer;
  roughness: float;
  captureFBO, captureRBO, hdrTexture, envCubemap, irradianceMap, prefilterMap,
    mipWidth, mipHeight, brdfLUTTexture, maxMipLevels, ironAlbedoMap, ironNormalMap,
    ironRoughnessMap, ironAOMap, goldAlbedoMap, goldNormalMap, goldMetallicMap,
    goldRoughnessMap, goldAOMap, grassAlbedoMap, grassNormalMap, grassMetallicMap,
    grassRoughnessMap, grassAOMap, plasticAlbedoMap, plasticNormalMap, plasticMetallicMap,
    plasticRoughnessMap, plasticAOMap, wallAlbedoMap, wallNormalMap, wallMetallicMap,
    wallRoughnessMap, wallAOMap, ironMetaCllicMap: Cardinal;
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

  // 在预滤波映射中，为较低的mip级别启用无缝立方体映射采样。
  glEnable(GL_TEXTURE_CUBE_MAP_SEAMLESS);

  //═════════════════════════════════════════════════════════════════════════

  pbrShader_managed := IInterface(TShaderProgram.Create);
  pbrShader := pbrShader_managed as TShaderProgram;
  pbrShader.LoadShaderFile(pbr_vs, pbr_fs);

  equirectangularToCubemapShader_managed := IInterface(TShaderProgram.Create);
  equirectangularToCubemapShader := equirectangularToCubemapShader_managed as TShaderProgram;
  equirectangularToCubemapShader.LoadShaderFile(cubemap_vs, equirectangular_to_cubemap_fs);

  irradianceShader_managed := IInterface(TShaderProgram.Create);
  irradianceShader := irradianceShader_managed as TShaderProgram;
  irradianceShader.LoadShaderFile(cubemap_vs, irradiance_convolution_fs);

  prefilterShader_managed := IInterface(TShaderProgram.Create);
  prefilterShader := prefilterShader_managed as TShaderProgram;
  prefilterShader.LoadShaderFile(cubemap_vs, prefilter_fs);

  brdfShader_managed := IInterface(TShaderProgram.Create);
  brdfShader := brdfShader_managed as TShaderProgram;
  brdfShader.LoadShaderFile(brdf_vs, brdf_fs);

  backgroundShader_managed := IInterface(TShaderProgram.Create);
  backgroundShader := backgroundShader_managed as TShaderProgram;
  backgroundShader.LoadShaderFile(background_vs, background_fs);

  pbrShader.UseProgram;
  pbrShader.SetUniformInt('irradianceMap', 0);
  pbrShader.SetUniformInt('prefilterMap', 1);
  pbrShader.SetUniformInt('brdfLUT', 2);
  pbrShader.SetUniformInt('albedoMap', 3);
  pbrShader.SetUniformInt('normalMap', 4);
  pbrShader.SetUniformInt('metallicMap', 5);
  pbrShader.SetUniformInt('roughnessMap', 6);
  pbrShader.SetUniformInt('aoMap', 7);

  backgroundShader.UseProgram;
  backgroundShader.SetUniformInt('environmentMap', 0);

  //═════════════════════════════════════════════════════════════════════════

  // 加载PBR材料纹理
  // rusted iron
  ironAlbedoMap := LoadTexture(img_rusted_iron_albedo);
  ironNormalMap := LoadTexture(img_rusted_iron_normal);
  ironMetaCllicMap := LoadTexture(img_rusted_iron_metallic);
  ironRoughnessMap := LoadTexture(img_rusted_iron_roughness);
  ironAOMap := LoadTexture(img_rusted_iron_ao);

  // gold
  goldAlbedoMap := LoadTexture(img_gold_albedo);
  goldNormalMap := LoadTexture(img_gold_normal);
  goldMetallicMap := LoadTexture(img_gold_metallic);
  goldRoughnessMap := LoadTexture(img_gold_roughness);
  goldAOMap := LoadTexture(img_gold_ao);

  // grass
  grassAlbedoMap := LoadTexture(img_grass_albedo);
  grassNormalMap := LoadTexture(img_grass_normal);
  grassMetallicMap := LoadTexture(img_grass_metallic);
  grassRoughnessMap := LoadTexture(img_grass_roughness);
  grassAOMap := LoadTexture(img_grass_ao);

  // plastic
  plasticAlbedoMap := LoadTexture(img_plastic_albedo);
  plasticNormalMap := LoadTexture(img_plastic_normal);
  plasticMetallicMap := LoadTexture(img_plastic_metallic);
  plasticRoughnessMap := LoadTexture(img_plastic_roughness);
  plasticAOMap := LoadTexture(img_plastic_ao);

  // wall
  wallAlbedoMap := LoadTexture(img_wall_albedo);
  wallNormalMap := LoadTexture(img_wall_normal);
  wallMetallicMap := LoadTexture(img_wall_metallic);
  wallRoughnessMap := LoadTexture(img_wall_roughness);
  wallAOMap := LoadTexture(img_wall_ao);
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
    Imaging.ConvertImage(hdrData, TImageFormat.ifA8R8G8B8);

    hdrTexture := Cardinal(0);
    glGenTextures(1, @hdrTexture);
    glBindTexture(GL_TEXTURE_2D, hdrTexture);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, hdrData.width, hdrData.height,
      0, GL_BGRA, GL_UNSIGNED_BYTE, hdrData.Bits); // 注意我们是如何将纹理的数据值指定为float的

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

  // 然后让 OpenGL 从第一个 mip 面生成 mipmaps (对抗可见点伪影)
  glBindTexture(GL_TEXTURE_CUBE_MAP, envCubemap);
  glGenerateMipmap(GL_TEXTURE_CUBE_MAP);


  //═════════════════════════════════════════════════════════════════════════

  // pbr: 创建一个辐照度立方体图，并重新缩放捕获FBO到辐照度尺度。
  irradianceMap := Cardinal(0);
  glGenTextures(1, @irradianceMap);
  glBindTexture(GL_TEXTURE_CUBE_MAP, irradianceMap);

  for i := 0 to 5 do
  begin
    glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_RGB16F, 32, 32, 0,
      GL_RGB, GL_FLOAT, nil);
  end;

  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glBindFramebuffer(GL_FRAMEBUFFER, captureFBO);
  glBindRenderbuffer(GL_RENDERBUFFER, captureRBO);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, 32, 32);

  //═════════════════════════════════════════════════════════════════════════

  // Pbr:通过卷积求解漫射积分来创建一个辐照度(立方体)图
  irradianceShader.UseProgram;
  irradianceShader.SetUniformInt('environmentMap', 0);
  irradianceShader.SetUniformMatrix4fv('projection', captureProjection);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_CUBE_MAP, envCubemap);

  glViewport(0, 0, 32, 32); // don't forget to configure the viewport to the capture dimensions.
  glBindFramebuffer(GL_FRAMEBUFFER, captureFBO);
  for i := 0 to 5 do
  begin
    irradianceShader.SetUniformMatrix4fv('view', captureViews[i]);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
      GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, irradianceMap, 0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    RenderCube;
  end;

  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  //═════════════════════════════════════════════════════════════════════════

  // pbr:创建一个预过滤器立方体映射，并重新缩放捕获FBO到预过滤器的比例。
  prefilterMap := Cardinal(0);
  glGenTextures(1, @prefilterMap);
  glBindTexture(GL_TEXTURE_CUBE_MAP, prefilterMap);
  for i := 0 to 5 do
  begin
    glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_RGB16F, 128, 128, 0,
      GL_BGR, GL_FLOAT, nil);
  end;

  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR); // 确保将缩小过滤器设置为mip_linear
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // 为立方体映射生成mipmap，这样OpenGL就会自动分配所需的内存。
  glGenerateMipmap(GL_TEXTURE_CUBE_MAP);

  //═════════════════════════════════════════════════════════════════════════

  // Pbr:在环境照明上运行一个准蒙特卡罗模拟来创建一个预滤波(立方体)贴图。
  prefilterShader.UseProgram;
  prefilterShader.SetUniformInt('environmentMap', 0);
  prefilterShader.SetUniformMatrix4fv('projection', captureProjection);
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_CUBE_MAP, envCubemap);

  glBindFramebuffer(GL_FRAMEBUFFER, captureFBO);

  maxMipLevels := Cardinal(5);
  for mip := 0 to maxMipLevels - 1 do
  begin
    // 根据mip级别大小调整framebuffer大小。
    mipWidth  := Cardinal(Round(128 * Power(0.5, mip)));
    mipHeight := Cardinal(Round(128 * Power(0.5, mip)));
    glBindRenderbuffer(GL_RENDERBUFFER, captureRBO);
    glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, mipWidth, mipHeight);
    glViewport(0, 0, mipWidth, mipHeight);

    roughness := float(mip / (maxMipLevels - 1));
    prefilterShader.SetUniformFloat('roughness', roughness);

    for i := 0 to 5 do
    begin
      prefilterShader.SetUniformMatrix4fv('view', captureViews[i]);
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, prefilterMap, mip);

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
      RenderCube;
    end;
  end;

  glBindFramebuffer(GL_FRAMEBUFFER, 0);

  //═════════════════════════════════════════════════════════════════════════

  // pbr: 从使用的BRDF方程生成二维LUT。
  brdfLUTTexture := Cardinal(0);
  glGenTextures(1, @brdfLUTTexture);

  // 为LUT纹理预先分配足够的内存。
  glBindTexture(GL_TEXTURE_2D, brdfLUTTexture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RG16F, 512, 512, 0, GL_RG, GL_FLOAT, nil);

  // 确保将包装模式设置为 GL_CLAMP_TO_EDGE
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // 然后重新配置捕获帧缓冲对象和渲染屏幕空间四边形与BRDF着色器。
  glBindFramebuffer(GL_FRAMEBUFFER, captureFBO);
  glBindRenderbuffer(GL_RENDERBUFFER, captureRBO);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, 512, 512);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, brdfLUTTexture, 0);

  glViewport(0, 0, 512, 512);

  brdfShader.UseProgram;

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  RenderQuad;

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
    model := TGLM.mat4(1.0);
    view := camera.GetViewMatrix();
    pbrShader.SetUniformMatrix4fv('view', view);
    pbrShader.SetUniformVec3('camPos', camera.Position);

    //绑定预先计算的IBL数据
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_CUBE_MAP, irradianceMap);
    glActiveTexture(GL_TEXTURE1);
    glBindTexture(GL_TEXTURE_CUBE_MAP, prefilterMap);
    glActiveTexture(GL_TEXTURE2);
    glBindTexture(GL_TEXTURE_2D, brdfLUTTexture);

    // rusted iron
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, ironAlbedoMap);
    glActiveTexture(GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_2D, ironNormalMap);
    glActiveTexture(GL_TEXTURE5);
    glBindTexture(GL_TEXTURE_2D, ironMetaCllicMap);
    glActiveTexture(GL_TEXTURE6);
    glBindTexture(GL_TEXTURE_2D, ironRoughnessMap);
    glActiveTexture(GL_TEXTURE7);
    glBindTexture(GL_TEXTURE_2D, ironAOMap);

    model := TGLM.Mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(-5.0, 0.0, 2.0));
    pbrShader.SetUniformMatrix4fv('model', model);
    pbrShader.SetUniformMatrix3fv('normalMatrix',
      TGLM.TransposeMat3(TGLM.InverseMat3(TGLM.Mat3(model))));
    RenderSphere;

    // gold
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, goldAlbedoMap);
    glActiveTexture(GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_2D, goldNormalMap);
    glActiveTexture(GL_TEXTURE5);
    glBindTexture(GL_TEXTURE_2D, goldMetallicMap);
    glActiveTexture(GL_TEXTURE6);
    glBindTexture(GL_TEXTURE_2D, goldRoughnessMap);
    glActiveTexture(GL_TEXTURE7);
    glBindTexture(GL_TEXTURE_2D, goldAOMap);

    model := TGLM.Mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(-3.0, 0.0, 2.0));
    pbrShader.SetUniformMatrix4fv('model', model);
    pbrShader.SetUniformMatrix3fv('normalMatrix',
      TGLM.TransposeMat3(TGLM.InverseMat3(TGLM.Mat3(model))));
    RenderSphere;

    // grass
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, grassAlbedoMap);
    glActiveTexture(GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_2D, grassNormalMap);
    glActiveTexture(GL_TEXTURE5);
    glBindTexture(GL_TEXTURE_2D, grassMetallicMap);
    glActiveTexture(GL_TEXTURE6);
    glBindTexture(GL_TEXTURE_2D, grassRoughnessMap);
    glActiveTexture(GL_TEXTURE7);
    glBindTexture(GL_TEXTURE_2D, grassAOMap);

    model := TGLM.Mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(-1.0, 0.0, 2.0));
    pbrShader.SetUniformMatrix4fv('model', model);
    pbrShader.SetUniformMatrix3fv('normalMatrix',
      TGLM.TransposeMat3(TGLM.InverseMat3(TGLM.Mat3(model))));
    RenderSphere;

    // plastic
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, plasticAlbedoMap);
    glActiveTexture(GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_2D, plasticNormalMap);
    glActiveTexture(GL_TEXTURE5);
    glBindTexture(GL_TEXTURE_2D, plasticMetallicMap);
    glActiveTexture(GL_TEXTURE6);
    glBindTexture(GL_TEXTURE_2D, plasticRoughnessMap);
    glActiveTexture(GL_TEXTURE7);
    glBindTexture(GL_TEXTURE_2D, plasticAOMap);

    model := TGLM.Mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(1.0, 0.0, 2.0));
    pbrShader.SetUniformMatrix4fv('model', model);
    pbrShader.SetUniformMatrix3fv('normalMatrix',
      TGLM.TransposeMat3(TGLM.InverseMat3(TGLM.Mat3(model))));
    RenderSphere;

    // wall
    glActiveTexture(GL_TEXTURE3);
    glBindTexture(GL_TEXTURE_2D, wallAlbedoMap);
    glActiveTexture(GL_TEXTURE4);
    glBindTexture(GL_TEXTURE_2D, wallNormalMap);
    glActiveTexture(GL_TEXTURE5);
    glBindTexture(GL_TEXTURE_2D, wallMetallicMap);
    glActiveTexture(GL_TEXTURE6);
    glBindTexture(GL_TEXTURE_2D, wallRoughnessMap);
    glActiveTexture(GL_TEXTURE7);
    glBindTexture(GL_TEXTURE_2D, wallAOMap);

    model := TGLM.Mat4(1.0);
    model := TGLM.Translate(model, TGLM.Vec3(3.0, 0.0, 2.0));
    pbrShader.SetUniformMatrix4fv('model', model);
    pbrShader.SetUniformMatrix3fv('normalMatrix',
      TGLM.TransposeMat3(TGLM.InverseMat3(TGLM.Mat3(model))));
    RenderSphere;

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

function LoadTexture(fileName: string): cardinal;
var
  img: TImageData;
  textureID: Cardinal;
begin
  textureID := Cardinal(0);
  glGenTextures(1, @textureID);

  if Imaging.LoadImageFromFile(fileName.ToAnsiString, img) then
  begin
    Imaging.FlipImage(img);
    Imaging.ConvertImage(img, TImageFormat.ifA8R8G8B8);

    glBindTexture(GL_TEXTURE_2D, textureID);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_BGRA, img.Width, img.Height, 0,
      GL_BGRA, GL_UNSIGNED_BYTE, img.Bits);

    glGenerateMipmap(GL_TEXTURE_2D);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    FreeImage(img);
  end
  else
  begin
    WriteLn('Texture failed to load at path: ', fileName);
  end;

  Result := textureID;
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

procedure RenderQuad;
var
  quadVertices: TArr_GLfloat;
begin
  if quadVAO = 0 then
  begin
    quadVertices := TArr_GLfloat([
      // positions    // texture Coords
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
