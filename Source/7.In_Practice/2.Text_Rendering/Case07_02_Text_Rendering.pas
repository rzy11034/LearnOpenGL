unit Case07_02_Text_Rendering;

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

  freetype, freetypeh,

  DeepStar.Utils,
  DeepStar.DSA.Tree.TreeMap,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.GLM,
  DeepStar.OpenGL.GLFW,
  DeepStar.OpenGL.Model;

procedure Main;

implementation

// 每当窗口大小发生变化(由操作系统或用户调整大小)，这个回调函数就会执行
procedure Framebuffer_size_callback(window: PGLFWwindow; witdth, Height: integer); cdecl; forward;
// 处理所有输入:查询GLFW是否按下/释放了相关的键，并做出相应的反应
procedure ProcessInput(window: PGLFWwindow); forward;
// glfw & glad  初始化
function InitWindows: PGLFWwindow; forward;

procedure RenderText(shader: TShaderProgram; Text: string;
  x, y, scale: float; color: TVec3); forward;

type
  // 保存与使用 FreeType 加载字符相关的所有状态信息
  TCharacter = Record
    TextureID: Cardinal; // ID handle of the glyph texture
    Size: TVec2; // Size of glyph
    Bearing: TVec2; // Offset from baseline to left/top of glyph
    Advance: Cardinal; // Horizontal offset to advance to next glyph
  end;

  TMap_Glchar_TCharacter = specialize TTreeMap<GLchar, TCharacter>;

const
  SCR_WIDTH  = 800;
  SCR_HEIGHT = 600;

var
  VAO, VBO: Cardinal;
  characters_managed: IInterface;
  characters: TMap_Glchar_TCharacter;

procedure Main;
const
  shader_path = '..\Source\7.In_Practice\2.Text_Rendering\';
  text_vs = shader_path + 'text.vs';
  text_fs = shader_path + 'text.fs';

  font_name = '..\Resources\fonts\Antonio-Bold.ttf';
var
  window: PGLFWwindow;
  shader_managed: IInterface;
  shader: TShaderProgram;
  projection: TMat4;
  c: Integer;
  texture: Cardinal;
  character: TCharacter;
  ft: PFT_Library;
  face: PFT_Face;
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
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  //═════════════════════════════════════════════════════════════════════════

  shader_managed := IInterface(TShaderProgram.Create);
  shader := shader_managed as TShaderProgram;
  shader.LoadShaderFile(text_vs, text_fs);

  projection := TGLM.Ortho2D(0.0, SCR_WIDTH, 0.0, SCR_HEIGHT);
  shader.UseProgram;
  shader.SetUniformMatrix4fv('projection', projection);

  //═════════════════════════════════════════════════════════════════════════

  characters_managed := IInterface(TMap_Glchar_TCharacter.Create);
  characters := characters_managed as TMap_Glchar_TCharacter;

  //═════════════════════════════════════════════════════════════════════════

  // All functions return a value different than 0 whenever an error occurred
  if FT_Init_FreeType(ft).ToBoolean then
  begin
    WriteLn('ERROR::FREETYPE: Could not init FreeType Library');
    Exit;
  end;

  // find path to font
  if FileExists(font_name) = false then
  begin
    WriteLn('ERROR::FREETYPE: Failed to load font_name');
    Exit;
  end;

  // load font as face_
  face := PFT_Face(nil);
  if FT_New_Face(ft, font_name.ToPAnsiChar, 0, face).ToBoolean then
  begin
    WriteLn('ERROR::FREETYPE: Failed to load font');
    Exit;
  end
  else
  begin
    // set size to load glyphs as
    FT_Set_Pixel_Sizes(face, 0, 48);

    // disable byte-alignment restriction
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

    // load first 128 characters of ASCII set
    for c := 0 to Pred(128) do
    begin
      // Load character glyph
      if FT_Load_Char(face, c, FT_LOAD_RENDER).ToBoolean then
      begin
        WriteLn('ERROR::FREETYTPE: Failed to load Glyph');
        Continue;
      end;

      // generate texture
      texture := Cardinal(0);
      glGenTextures(1, @texture);
      glBindTexture(GL_TEXTURE_2D, texture);
      glTexImage2D(
          GL_TEXTURE_2D,
          0,
          GL_RED,
          face^.glyph^.bitmap.width,
          face^.glyph^.bitmap.rows,
          0,
          GL_RED,
          GL_UNSIGNED_BYTE,
          face^.glyph^.bitmap.buffer
      );

      // set texture options
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

      character := Default(TCharacter);
      with character do
      begin
        TextureID := texture;
        Bearing := TGLM.Vec2(face^.glyph^.bitmap_left, face^.glyph^.bitmap_top);
        Size := TGLM.Vec2(face^.glyph^.bitmap.Width, face^.glyph^.bitmap.rows);
        Advance := face^.glyph^.advance.x;
      end;

      characters.Add(Chr(c), character);
    end;

    glBindTexture(GL_TEXTURE_2D, 0);
  end;

  // destroy FreeType once we're finished
  FT_Done_Face(face);
  FT_Done_FreeType(ft);

  //═════════════════════════════════════════════════════════════════════════

  // configure VAO/VBO for texture quads
  glGenVertexArrays(1, @VAO);
  glGenBuffers(1, @VBO);
  glBindVertexArray(VAO);
  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, SIZE_OF_F * 6 * 4, nil, GL_DYNAMIC_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * SIZE_OF_F, Pointer(0));
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);

  //═════════════════════════════════════════════════════════════════════════

  // 渲染循环
  while not glfwWindowShouldClose(window).ToBoolean do
  begin
    // 输入
    ProcessInput(window);

    // render
    glClearColor(0.2, 0.3, 0.3, 1.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    RenderText(shader, 'This is sample text', 25, 25, 1, TGLM.Vec3(0.5, 0.8, 0.2));
    RenderText(shader, '(C) LearnOpenGL.com', 540, 570, 0.5, TGLM.Vec3(0.3, 0.7, 0.9));

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

  Result := window;
end;

procedure RenderText(shader: TShaderProgram; Text: string; x, y, scale: float; color: TVec3);
var
  i: Integer;
  ch: TCharacter;
  xpos, ypos, w, h: Single;
  c: Char;
  vertices: array[0..5, 0..3] of GLfloat;
begin
  // 激活相应的呈现状态
  shader.UseProgram;
  shader.SetUniformVec3('textColor', color);
  glActiveTexture(GL_TEXTURE0);
  glBindVertexArray(VAO);

  for i := 0 to Length(text) - 1 do
  begin
    c := Text.Chars[i];
    ch := characters[c];

    xpos := x + ch.Bearing.x * scale;
    ypos := y - (ch.Size.y - ch.Bearing.y) * scale;

    w := ch.Size.x * scale;
    h := ch.Size.y * scale;

    // update VBO for each character
    vertices := [
      [ xpos,     ypos + h,   0.0, 0.0 ],
      [ xpos,     ypos,       0.0, 1.0 ],
      [ xpos + w, ypos,       1.0, 1.0 ],

      [ xpos,     ypos + h,   0.0, 0.0 ],
      [ xpos + w, ypos,       1.0, 1.0 ],
      [ xpos + w, ypos + h,   1.0, 0.0 ]];

    // render glyph texture over quad
    glBindTexture(GL_TEXTURE_2D, ch.TextureID);

    // update content of VBO memory
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferSubData(GL_ARRAY_BUFFER, 0, sizeof(vertices), @vertices); // be sure to use glBufferSubData and not glBufferData

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    // render quad
    glDrawArrays(GL_TRIANGLES, 0, 6);

    // 现在为下一个字形移动光标（注意，移动是1/64像素）
    // 位移6得到像素值（2^6 = 64（1/64像素的数量除以64得到像素的数量））
    x += (ch.Advance >> 6) * scale;
  end;

  glBindVertexArray(0);
  glBindTexture(GL_TEXTURE_2D, 0);
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
