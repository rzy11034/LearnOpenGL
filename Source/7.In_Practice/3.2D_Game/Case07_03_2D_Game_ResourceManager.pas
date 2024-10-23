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
  TTreeMap_str_TShader = specialize TTreeMap<string, TShader>;
  TTreeMap_str_TTexture2D = specialize TTreeMap<string, TTexture2D>;

  TResourceManager = class(TInterfacedObject)
  private
    // 从文件加载并生成一个着色器
    class function __LoadShaderFromFile(vFile, fFile: string; gFile:string = ''):TShader;

    //从文件中加载单个纹理
    class function __LoadTextureFromFile(aFile: string; alpha: Boolean): TTexture2D;

  public
    // Resource storage
    class var Shaders: TTreeMap_str_TShader;
    class var Textures: TTreeMap_str_TTexture2D;

    class constructor Create;
    class destructor Destroy;

    // 加载（并生成）一个着色器程序从文件加载顶点，片段（和几何）着色器的源代码。
    // 如果gShaderFile不是nullptr，它也会加载一个几何着色器
    class function LoadShader(name, vFile, fFile: string; gFile:string = ''): TShader;

    // 检索存储的 shader
    class function GetShader(name: string): TShader;

    // 从文件中加载（并生成）纹理
    class function LoadTexture(name, fileName: string; alpha: Boolean): TTexture2D;

    // 检索存储的 texture
    class function GetTexture(name: string): TTexture2D;

    // 正确地重新分配所有已加载的资源
    class procedure Clear;
  end;

implementation

{ TResourceManager }

class destructor TResourceManager.Destroy;
var
  shader_value: TTreeMap_str_TShader.TImpl_V.TArr;
  texture_value: TTreeMap_str_TTexture2D.TImpl_V.TArr;
  i: Integer;
begin
  Clear;

  shader_value := Shaders.Values;
  for i := 0 to High(shader_value) do
    FreeAndNil(shader_value[i]);
  FreeAndNil(Shaders);

  texture_value := Textures.Values;
  for i := 0 to High(texture_value) do
    FreeAndNil(texture_value[i]);
  FreeAndNil(Textures);
end;

class constructor TResourceManager.Create;
begin
  Shaders := TTreeMap_str_TShader.Create;
  Textures := TTreeMap_str_TTexture2D.Create;
end;

class procedure TResourceManager.Clear;
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

class function TResourceManager.GetShader(name: string): TShader;
begin
  Result := Shaders[name];
end;

class function TResourceManager.GetTexture(name: string): TTexture2D;
begin
  Result := Textures[name];
end;

class function TResourceManager.LoadShader(name, vFile, fFile: string; gFile: string): TShader;
begin
  Shaders.Add(name, __LoadShaderFromFile(vFile, fFile, gFile));
  Result := Shaders[name];
end;

class function TResourceManager.LoadTexture(name, fileName: string; alpha: Boolean): TTexture2D;
begin
  Textures.Add(name, __LoadTextureFromFile(fileName, alpha));
  Result := Textures[name];
end;

class function TResourceManager.__LoadShaderFromFile
  (vFile, fFile: string; gFile: string): TShader;
var
  sd: TShader;
begin
  sd := TShader.Create;
  sd.LoadShaderFile(vFile, fFile, gFile);
  Result := sd;
end;

class function TResourceManager.__LoadTextureFromFile
  (aFile: string; alpha: Boolean): TTexture2D;
var
  tx2D: TTexture2D;
  data: TTexture;
begin
  tx2D:= TTexture2D.Create;
  data := TTexture.Create;

  try
    data.LoadFormFile(aFile, false);

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

