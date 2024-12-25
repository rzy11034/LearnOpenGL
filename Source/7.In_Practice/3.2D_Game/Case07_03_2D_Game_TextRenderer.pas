unit Case07_03_2D_Game_TextRenderer;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,

  freetype,
  freetypeh,

  DeepStar.Utils,
  DeepStar.DSA.Tree.TreeMap,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.GLM,

  Case07_03_2D_Game_ResourceManager,
  Case07_03_2D_Game_Consts,
  Case07_03_2D_Game_Shader;

type
  // 保存与使用 FreeType 加载字符相关的所有状态信息
  TCharacter = Record
    TextureID: Cardinal; // ID handle of the glyph texture
    Size: TVec2; // Size of glyph
    Bearing: TVec2; // Offset from baseline to left/top of glyph
    Advance: Cardinal; // Horizontal offset to advance to next glyph
  end;

  TTextRenderer = class(TInterfacedObject)
  public type
    TMap_Glchar_TCharacter = specialize TTreeMap<GLchar, TCharacter>;

  private
    // Render state
    _VAO, _VBO: GLuint;

  public
    // Shader used for text rendering
    TextShader: TShader;
    // Holds a list of pre-compiled Characters
    Characters: TMap_Glchar_TCharacter;

    constructor Create(width, height: GLuint);
    destructor Destroy; override;

    // Pre-compiles a list of characters from the given font
    procedure Load(font: string; fontSize: GLuint);
    // Renders a string of text using the precompiled list of characters
    procedure RenderText(text: string; x, y, scale: GLfloat; pColor: PVec3 = nil);
  end;

implementation

{ TTextRenderer }

constructor TTextRenderer.Create(width, height: GLuint);
begin
  Self.Characters := TMap_Glchar_TCharacter.Create;

  // Load and configure shader
  TextShader := TResourceManager.LoadShader(TEXT_2D_NAME, TEXT_2D_VS, TEXT_2D_FS, '');
  TextShader.SetMatrix4('projection', TGLM.Ortho2D(0.0, width, height, 0), true);
  TextShader.SetInteger('text', 0);

  // Configure VAO/VBO for texture quads
  glGenVertexArrays(1, @_VAO);
  glGenBuffers(1, @_VBO);
  glBindVertexArray(_VAO);
  glBindBuffer(GL_ARRAY_BUFFER, _VBO);
  glBufferData(GL_ARRAY_BUFFER, SIZE_OF_F * 6 * 4, nil, GL_DYNAMIC_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * SIZE_OF_F, Pointer(0));
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
end;

destructor TTextRenderer.Destroy;
begin
  if Self.Characters <> nil then
    FreeAndNil(Self.Characters);

  inherited Destroy;
end;

procedure TTextRenderer.Load(font: string; fontSize: GLuint);
var
  ft: PFT_Library;
  face: PFT_Face;
  c: Integer;
  texture: Cardinal;
  character: TCharacter;

begin
  // All functions return a value different than 0 whenever an error occurred
  if FT_Init_FreeType(ft).ToBoolean then
  begin
    WriteLn('ERROR::FREETYPE: Could not init FreeType Library');
    Exit;
  end;

  // find path to font
  if FileExists(font) = false then
  begin
    WriteLn('ERROR::FREETYPE: Failed to load font_name');
    Exit;
  end;

  // load font as face_
  face := PFT_Face(nil);
  if FT_New_Face(ft, font.ToPAnsiChar, 0, face).ToBoolean then
  begin
    WriteLn('ERROR::FREETYPE: Failed to load font');
    Exit;
  end;

  // set size to load glyphs as
  FT_Set_Pixel_Sizes(face, 0, fontSize);

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

  // destroy FreeType once we're finished
  FT_Done_Face(face);
  FT_Done_FreeType(ft);
end;

procedure TTextRenderer.RenderText(text: string; x, y, scale: GLfloat; pColor: PVec3);
var
  color: TVec3;
  i: Integer;
  c: Char;
  ch: TCharacter;
  xpos, ypos, w, h: Single;
  vertices: array[0..5, 0..3] of GLfloat;
begin
  color := TGLM.Vec3(1);
  if pColor <> nil then color := pColor^;

  // Activate corresponding render state
  Self.TextShader.Use;
  Self.TextShader.SetVector3f('textColor', color);
  glActiveTexture(GL_TEXTURE0);
  glBindVertexArray(Self._VAO);

  // Iterate through all characters
  for i := 0 to text.Length - 1 do
  begin
    c := Text.Chars[i];
    ch := characters[c];

    xpos := x + ch.Bearing.x * scale;
    ypos := y + (Self.Characters['H'].Bearing.y - ch.Bearing.y) * scale;

    w := ch.Size.x * scale;
    h := ch.Size.y * scale;

    // Update VBO for each character
    vertices :=[
        [xpos,      ypos + h,   0.0,   1.0],
        [xpos + w,  ypos,       1.0,   0.0],
        [xpos,      ypos,       0.0,   0.0],

        [xpos,      ypos + h,   0.0,   1.0],
        [xpos + w,  ypos + h,   1.0,   1.0],
        [xpos + w,  ypos,       1.0,   0.0]];

    // Render glyph texture over quad
    glBindTexture(GL_TEXTURE_2D, ch.TextureID);
    // Update content of VBO memory
    glBindBuffer(GL_ARRAY_BUFFER, Self._VBO);
    glBufferSubData(GL_ARRAY_BUFFER, 0, SizeOf(vertices), @vertices); // Be sure to use glBufferSubData and not glBufferData
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    // Render quad
    glDrawArrays(GL_TRIANGLES, 0, 6);
    // Now advance cursors for next glyph
    x += (ch.Advance >> 6) * scale; // Bitshift by 6 to get value in pixels (1/64th times 2^6 = 64)
  end;

  glBindVertexArray(0);
  glBindTexture(GL_TEXTURE_2D, 0);
end;

end.

