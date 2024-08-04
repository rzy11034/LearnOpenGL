unit DeepStar.OpenGL.Model;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  ctypes,
  DeepStar.Utils,
  DeepStar.DSA.Linear.ArrayList,
  DeepStar.OpenGL.Utils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Texture,
  DeepStar.OpenGL.Shader,
  DeepStar.OpenGL.Mesh,
  DeepStar.OpenGL.Assimp;

type
  TList_TMesh = specialize TArrayList<TMesh>;

  TModel = class(TObject)
  private
    _directory: string;
    _gammaCorrection: boolean;
    _textures_loaded: TList_TTexture;
    _meshes: TList_TMesh;

    function __ProcessMesh(aMesh: PaiMesh; const scene: PaiScene):TMesh;

    procedure __LoadModel(fileName: string);

    // 以递归方式处理节点。处理位于节点上的每个单独的网格，
    // 并在其子节点上重复此过程(如果有的话)
    procedure __ProcessNode(node: PaiNode; const scene: PaiScene);

public
    constructor Create(fileName: string; gammaCorrection: boolean = false);
    destructor Destroy; override;

    // 绘制模型，从而绘制其所有网格
    procedure Draw(shader: TShaderProgram);

  end;

implementation

{ TModel }

constructor TModel.Create(fileName: string; gammaCorrection: boolean);
begin
  _gammaCorrection := gammaCorrection;
  __LoadModel(fileName);
end;

destructor TModel.Destroy;
begin
  inherited Destroy;
end;

procedure TModel.Draw(shader: TShaderProgram);
var
  i: cardinal;
begin
  for i := cardinal(0) to _meshes.Count - 1 do
  begin
    _meshes[i].Draw(shader)
  end;
end;

procedure TModel.__LoadModel(fileName: string);
var
  scene: PaiScene;
  pFlags: cuint;
  exceptionStr: String;
begin
  // read file via ASSIMP
  pFlags := aiProcess_Triangulate or aiProcess_GenSmoothNormals
    or aiProcess_FlipUVs or aiProcess_CalcTangentSpace;
  scene := aiImportFile(CrossFixFileName(fileName).ToPAnsiChar, pFlags);

  // check for errors
  if (scene = nil) or (scene^.mFlags and AI_SCENE_FLAGS_INCOMPLETE <> 0)
    or (scene^.mRootNode = nil) then
  begin
    exceptionStr := string('ERROR::ASSIMP::' + aiGetErrorString);
    raise Exception.Create(exceptionStr.ToAnsiString);
  end;

  _directory := fileName;

  // 递归地处理ASSIMP的根节点
  __ProcessNode(scene^.mRootNode, scene);
end;

function TModel.__ProcessMesh(aMesh: PaiMesh; const scene: PaiScene): TMesh;
var
  vertices: TList_TVertex;
  indices: TList_Gluint;
  textures: TList_TTexture;
  i: uint;
  vertex: TVertex;
begin
  // data to fill
  vertices := TList_TVertex.Create;
  indices := TList_Gluint.Create;
  textures := TList_TTexture.Create;

  // 遍历每个网格的顶点
  for i := uint(0) to aMesh^.mNumVertices-1 do
  begin
    vertex := Default(TVertex);
  end;

end;

procedure TModel.__ProcessNode(node: PaiNode; const scene: PaiScene);
var
  mesh: PaiMesh;
  i: uint;
begin
  // 处理位于当前节点的每个网格
  for i := uint(0) to node^.mNumChildren - 1 do
  begin
    // 节点对象只包含索引来索引场景中的实际对象。
    // 场景包含所有的数据，节点只是保持东西的组织(像节点之间的关系)。
    mesh := PaiMesh(nil);
    mesh := scene^.mMeshes[node^.mMeshes[i]];
    _meshes.AddLast(__ProcessMesh(mesh, scene));
  end;

  //在我们处理完所有的网格(如果有的话)之后，我们递归地处理每个子节点
  for i := uint(0) to node^.mNumChildren - 1 do
  begin

  end;
end;

end.
