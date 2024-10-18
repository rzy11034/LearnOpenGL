unit Case07_03_2D_Game_ResourceManager;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}
{$ModeSwitch implicitfunctionspecialization}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}

interface

uses
  Classes,
  SysUtils,
  DeepStar.DSA.Tree.TreeMap,
  DeepStar.DSA.Interfaces,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Texture,
  Case07_03_2D_Game_Shader,
  Case07_03_2D_Game_Texture2D;

type
  TResourceManager = class(TInterfacedObject)
  public type
    TTreeMap_str_TShader = specialize TTreeMap<string, TShader>;
    TTreeMap_str_TTexture2D = specialize TTreeMap<string, TTexture2D>;

  private
    // 从文件加载并生成一个着色器
    function __loadShaderFromFile(vFile, fFile: string; gFile:string = ''):TShader;

    //从文件中加载单个纹理
    function __loadTextureFromFile(aFile: string; alpha: Boolean): TTexture2D;

  public
    // Resource storage
    Shaders: TTreeMap_str_TShader;
    Textures: TTreeMap_str_TTexture2D;

    constructor Create;
    destructor Destroy; override;

    // 加载（并生成）一个着色器程序从文件加载顶点，片段（和几何）着色器的源代码。
    // 如果gShaderFile不是nullptr，它也会加载一个几何着色器
    function LoadShader(name, vFile, fFile: string; gFile:string = ''): TShader;

    // 检索存储的 shader
    function GetShader(name: string): TShader;

    // 从文件中加载（并生成）纹理
    function LoadTexture(name, fileName: string; alpha: Boolean): TTexture2D;

    // 检索存储的 texture
    function GetTexture(name: string): TTexture2D;

    // 正确地重新分配所有已加载的资源
    procedure Clear;
  end;

implementation

{ TResourceManager }

constructor TResourceManager.Create;
begin
  Shaders := TTreeMap_str_TShader.Create;
  Textures := TTreeMap_str_TTexture2D.Create;
end;

procedure TResourceManager.Clear;
var
  shader_keys: TTreeMap_str_TShader.TImpl_K.TArr;
  texture_keys: TTreeMap_str_TTexture2D.TImpl_K.TArr;
  i: Integer;
  tx2D: TTexture2D;
  Shader: TShader;
begin
  shader_keys := Shaders.Keys;
  for i := 0 to High(shader_keys) do
  begin
    Shader := Shaders[shader_keys[i]];
    glDeleteProgram(Shader.ID);
  end;

  texture_keys := Textures.Keys;
  for i := 0 to High(texture_keys) do
  begin
    tx2D := Textures[texture_keys[i]];
    glDeleteTextures(1, @tx2D.ID);
  end;
end;

destructor TResourceManager.Destroy;
var
  shader_value: TTreeMap_str_TShader.TImpl_V.TArr;
  texture_value: TTreeMap_str_TTexture2D.TImpl_V.TArr;
  i: Integer;
begin
  Self.Clear;

  shader_value := Shaders.Values;
  for i := 0 to High(shader_value) do
    shader_value[i].Free;
  Shaders.Free;

  texture_value := Textures.Values;
  for i := 0 to High(texture_value) do
    texture_value[i].Free;
  Textures.Free;

  inherited Destroy;
end;

function TResourceManager.GetShader(name: string): TShader;
begin
  Result := Shaders[name];
end;

function TResourceManager.GetTexture(name: string): TTexture2D;
begin
  Result := Textures[name];
end;

function TResourceManager.LoadShader(name, vFile, fFile: string; gFile: string): TShader;
begin
  Shaders.Add(name, __loadShaderFromFile(vFile, fFile, gFile));
  Result := Shaders[name];
end;

function TResourceManager.LoadTexture(name, fileName: string; alpha: Boolean): TTexture2D;
begin
  Textures.Add(name, __loadTextureFromFile(fileName, alpha));
  Result := Textures[name];
end;

function TResourceManager.__loadShaderFromFile(vFile, fFile: string; gFile: string): TShader;
var
  sd: TShader;
begin
  sd := TShader.Create;
  sd.LoadShaderFile(vFile, fFile, gFile);
  Result := sd;
end;

function TResourceManager.__loadTextureFromFile(aFile: string; alpha: Boolean): TTexture2D;
var
  tx2D: TTexture2D;
  data: TTexture;
begin
  tx2D:= TTexture2D.Create;
  data := TTexture.Create;

  try
    data.LoadFormFile(aFile);

    if alpha then
    begin
      tx2D.Internal_Format := GL_RGBA;
      tx2D.Image_Format := GL_RGBA;
    end;

    tx2D.Generate(data.Width, data.Height, data.Pixels);

  finally
    data.Free;
  end;

  Result := tx2D;
end;

end.

