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

    function __ProcessMesh(mesh: PaiMesh; const scene: PaiScene): TMesh;

    //检查给定类型的所有材质纹理，并加载尚未加载的纹理。
    //所需的信息作为一个纹理结构返回。
    function __LoadMaterialTextures(mat: PaiMaterial; type_: TaiTextureType;
      typeName: string): TList_TTexture;

    function __TextureFromFile(directory, fileName: string; gamma: Boolean = false): cardinal;

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

function TModel.__LoadMaterialTextures(mat: PaiMaterial; type_: TaiTextureType;
  typeName: string): TList_TTexture;
var
  res: TList_TTexture;
  i, j: Integer;
  str: TaiString;
  skip: Boolean;
  texture: TTexture;
begin
  res := TList_TTexture.Create;

  for i := 0 to aiGetMaterialTextureCount(mat, type_) - 1 do
  begin
    str := Default(TaiString);
    aiGetMaterialTexture(mat, type_, i, @str);

    // 检查之前是否加载了纹理，如果是，继续下一次迭代: 跳过加载新纹理
    skip := false;
    for j := 0 to _textures_loaded.Count - 1 do
    begin
      if CompareStr(_textures_loaded[j].Path.ToAnsiString, AnsiString(str.data)) = 0 then
      begin
        res.AddLast(_textures_loaded[j]);
        skip := true; //具有相同文件路径的纹理已经加载，继续下一个。(优化)
        Break;
      end;
    end;

    //如果纹理尚未加载，加载它
    if not skip then
    begin
      texture := Default(TTexture);
      texture.ID := __TextureFromFile(str.data, _directory);
      texture.Type_ := typeName;
      texture.Path := str.data;

      res.AddLast(texture);
      _textures_loaded.AddLast(texture);
    end;
  end;

  Result := res;
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
  textures, diffuseMaps, specularMap: TList_TTexture;
  vertex: TVertex;
  i, j: cardinal;
  tempVec3: TVec3;
  tempVec2: TVec2;
  face: TaiFace;
  materials: PaiMaterial;
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
    //if mesh^.mNormals[i] <> nil then
    //begin
    //  tempVec3.x := mesh^.mNormals[i].x;
    //  tempVec3.y := mesh^.mNormals[i].y;
    //  tempVec3.z := mesh^.mNormals[i].z;
    //  vertex.Normal := tempVec3;
    //end;

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

    // 检索脸的所有索引并将它们存储在索引 list 中
    for j := 0 to face.mNumIndices - 1 do
    begin
      indices.AddLast(face.mIndices[j]);
    end;
  end;

  // 处理材质  process materials
  materials := scene^.mMaterials[mesh^.mMaterialIndex];
  //我们假设在着色器中采样器名称有一个约定。每个漫反射纹理都应该命名
  //作为'texture_diffuseN'，其中N是一个从1到MAX_SAMPLER_NUMBER的序列号。
  //同样适用于其他纹理，如下所示:
  // diffuse: texture_diffuseN
  // specular: texture_specularN
  // normal: texture_normalN

  // 1. diffuse maps
  diffuseMaps := __LoadMaterialTextures(materials, aiTextureType_DIFFUSE, 'texture_diffuse');
  textures.AddRange(diffuseMaps.ToArray, 0, diffuseMaps.Count);

  // 2.specular maps
  specularMap := __LoadMaterialTextures(materials, aiTextureType_SPECULAR, 'texture_specular');
  extures.AddRange(specularMap.ToArray, 0, specularMap.Count);
end;

procedure TModel.__ProcessNode(node: PaiNode; const scene: PaiScene);
var
  mesh: PaiMesh;
  i: integer;
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
  for i := cardinal(0) to node^.mNumChildren - 1 do
  begin
    __ProcessNode(node^.mChildren[i], scene);
  end;
end;

function TModel.__TextureFromFile(directory, fileName: string; gamma: Boolean): cardinal;
var
  texture_ID: GLuint;
  tx: DeepStar.OpenGL.Texture.TTexture;
begin
  texture_ID := GLuint(0);
  glGenTextures(1, @texture_ID);

  tx := DeepStar.OpenGL.Texture.TTexture.Create;
  try
    tx.LoadFormFile(fileName);

    glBindTexture(GL_TEXTURE_2D, texture_ID);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tx.Width, tx.Height, 0, GL_RGBA,
      GL_UNSIGNED_BYTE, tx.Pixels);
    glGenerateMipmap(GL_TEXTURE_2D);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    Result := texture_ID;
  finally
    tx.Free;
  end;
end;

end.
