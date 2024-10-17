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
  DeepStar.OpenGL.GLAD_GL,
  Case07_03_2D_Game_Shader,
  Case07_03_2D_Game_Texture2D;

type
  TResourceManager = class(TInterfacedObject)
  private
    // 从文件加载并生成一个着色器
    function __loadShaderFromFile(vFile, fFile: string; gFile:string = ''):TShader;

    //从文件中加载单个纹理
    function __loadTextureFromFile(aFile: string; alpha: Boolean): TTexture2D;

  public
    // Resource storage
    Shaders: specialize TTreeMap<string, TShader>;
    Textures: specialize TTreeMap<string, TTexture2D>;

    constructor Create;
    destructor Destroy; override;

    // 加载（并生成）一个着色器程序从文件加载顶点，片段（和几何）着色器的源代码。
    // 如果gShaderFile不是nullptr，它也会加载一个几何着色器
    function LoadShader(name, vFile, fFile: string; gFile:string = ''): TShader;

    // 检索存储的 shader
    function GetShader(name: string): TShader;

    // 从文件中加载（并生成）纹理
    function LoadTexture(name: string; alpha: Boolean): TTexture2D;

    // 检索存储的 texture
    function GetTexture(name: string): TTexture2D;

    // 正确地重新分配所有已加载的资源
    procedure Clear;
  end;

implementation

{ TResourceManager }

constructor TResourceManager.Create;
begin
  Shaders := specialize TTreeMap<string, TShader>.Create;
  Textures := specialize TTreeMap<string, TTexture2D>.Create;
end;

procedure TResourceManager.Clear;
begin

end;

destructor TResourceManager.Destroy;
begin
  inherited Destroy;
end;

function TResourceManager.GetShader(name: string): TShader;
begin

end;

function TResourceManager.GetTexture(name: string): TTexture2D;
begin

end;

function TResourceManager.LoadShader(name, vFile, fFile: string; gFile: string): TShader;
begin

end;

function TResourceManager.LoadTexture(name: string; alpha: Boolean): TTexture2D;
begin

end;

function TResourceManager.__loadShaderFromFile(vFile, fFile: string; gFile: string): TShader;
var
  sd: TShader;
begin
  sd := TShader.Create;
  i := sd.ID;
end;

function TResourceManager.__loadTextureFromFile(aFile: string; alpha: Boolean): TTexture2D;
begin

end;

end.

