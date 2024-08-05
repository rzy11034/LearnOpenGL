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
  DeepStar.OpenGL.GLM,
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

    function __ProcessMesh(mesh: PaiMesh; const scene: PaiScene):TMesh;

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

function TModel.__ProcessMesh(mesh: PaiMesh; const scene: PaiScene): TMesh;
var
  vertices: TList_TVertex;
  indices: TList_Gluint;
  textures: TList_TTexture;
  vertex: TVertex;
  i: cardinal;
  tempVec3: TVec3;
  tempVec2: TVec2;
  face: TaiFace;
begin
  // data to fill
  vertices := TList_TVertex.Create;
  indices := TList_Gluint.Create;
  textures := TList_TTexture.Create;

  // 遍历每个网格的顶点
  for i := 0 to mesh^.mNumVertices - 1 do
  begin
    vertex := Default(TVertex);
    // 我们声明一个占位符向量，因为assimp使用自己的vector类，
    // 不能直接转换为glm的vec3类，所以我们首先将数据传输到这个占位符 TGLM.vec3。
    tempVec3 := TGLM.Vec3(0);

    // position
    tempVec3.x := mesh^.mVertices[i].x;
    tempVec3.y := mesh^.mVertices[i].y;
    tempVec3.z := mesh^.mVertices[i].z;
    vertex.Position := tempVec3;

    // normals
    if mesh^.mNormals[i] <> nil then
    begin
      tempVec3.x := mesh^.mNormals[i].x;
      tempVec3.y := mesh^.mNormals[i].y;
      tempVec3.z := mesh^.mNormals[i].z;
      vertex.Normal := tempVec3;
    end;

    // texture coordinates
    if mesh^.mTextureCoords[0] <> nil then
    begin
      tempVec2 := TGLM.Vec2(0);

      //一个顶点最多可以包含8个不同的纹理坐标。因此，我们假设我们不会
      //使用顶点可以有多个纹理坐标的模型，所以我们总是取第一个集合(0)。
      tempVec2.x := mesh^.mTextureCoords[0][i].x;
      tempVec2.y := mesh^.mTextureCoords[0][i].y;
      vertex.TexCoords := tempVec2;

      // tangent
      tempVec3.x := mesh^.mTangents[i].x;
      tempVec3.y := mesh^.mTangents[i].y;
      tempVec3.z := mesh^.mTangents[i].z;
      vertex.Tangent := tempVec3;

      // bitangent
      tempVec3.x := mesh^.mBitangents[i].x;
      tempVec3.y := mesh^.mBitangents[i].y;
      tempVec3.z := mesh^.mBitangents[i].z;
      vertex.Bitangent := tempVec3;
    end
    else
      vertex.TexCoords := TGLM.Vec2(0);

    vertices.AddLast(vertex);
  end;

  //现在遍历每个网格的面 (面是一个网格的三角形) 并检索相应的顶点索引。
  for i := 0 to mesh^.mNumFaces -1 do
  begin
    face := mesh^.mFaces[i];

    // 检索脸的所有索引并将它们存储在索引向量中

  end;
end;

procedure TModel.__ProcessNode(node: PaiNode; const scene: PaiScene);
var
  mesh: PaiMesh;
  i: cardinal;
begin
  // 处理位于当前节点的每个网格
  for i := 0 to node^.mNumChildren - 1 do
  begin
    // 节点对象只包含索引来索引场景中的实际对象。
    // 场景包含所有的数据，节点只是保持东西的组织(像节点之间的关系)。
    mesh := PaiMesh(nil);
    mesh := scene^.mMeshes[node^.mMeshes[i]];
    _meshes.AddLast(__ProcessMesh(mesh, scene));
  end;

  //在我们处理完所有的网格(如果有的话)之后，我们递归地处理每个子节点
  for i := 0 to node^.mNumChildren - 1 do
  begin
    __ProcessNode(node^.mChildren[i], scene);
  end;
end;

end.
