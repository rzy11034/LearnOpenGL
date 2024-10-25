unit Case07_03_2D_Game_GameLevel;

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
  Character,
  DeepStar.Utils,
  DeepStar.OpenGL.GLM,
  DeepStar.DSA.Linear.ArrayList,
  Case07_03_2D_Game_GameObject,
  Case07_03_2D_Game_ResourceManager,
  Case07_03_2D_Game_SpriteRenderer;

type
  TArrayList_TGameObject = specialize TArrayList<TGameObject>;
  TArrayList_TArrayList_Cardinal = specialize TArrayList<TArrayList_Cardinal>;

  TGameLevel = class(TInterfacedObject)
  private
    // initialize level from tile data
    procedure __Init(tileData: TArrayList_TArrayList_Cardinal;
      levelWidth, levelHeight: Cardinal);

  public
    // level state
    Bricks: TArrayList_TGameObject;

    // constructor
    constructor Create;
    destructor Destroy; override;

    // loads level from file
    procedure Load(fileName: string; levelWidth, levelHeight: Cardinal);

    // render level
    procedure Draw(renderer: TSpriteRenderer);

    // 检查关卡是否完成（所有非实体贴图被破坏）
    function IsCompleted: Boolean;

  end;

implementation

{ TGameLevel }

constructor TGameLevel.Create;
begin
  Bricks := TArrayList_TGameObject.Create;
end;

destructor TGameLevel.Destroy;
var
  i: Integer;
begin
  if not Bricks.IsEmpty then
  begin
    for i := 0 to Bricks.Count - 1 do
    begin
      Bricks[i].Free;
    end;
  end;

  Bricks.Free;

  inherited Destroy;
end;

procedure TGameLevel.Draw(renderer: TSpriteRenderer);
var
  i: Integer;
  tile: TGameObject;
begin
  for i := 0 to Bricks.Count - 1 do
  begin
    tile := Bricks[i];

    if not tile.Destroyed then
      tile.Draw(renderer);
  end;
end;

function TGameLevel.IsCompleted: Boolean;
var
  tile: TGameObject;
  i: Integer;
begin
  for i := 0 to Bricks.Count - 1 do
  begin
    tile := Bricks[i];

    if (not tile.IsSolid) and (not tile.Destroyed) then
      Exit(false);
  end;

  Result := true;
end;

procedure TGameLevel.Load(fileName: string; levelWidth, levelHeight: Cardinal);
var
  i, j: Integer;
  sl: TStringList;
  tileData_managed: IInterface;
  row: TArrayList_Cardinal;
  tileData: TArrayList_TArrayList_Cardinal;
  tempStr: String;
begin
  // clear Bricks
  if not Bricks.IsEmpty then
  begin
    for i := 0 to Bricks.Count - 1 do
    begin
      Bricks[i].Free;
    end;

    Bricks.Clear;
  end;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(CrossFixFileName(fileName).ToAnsiString);

    tileData_managed := IInterface(TArrayList_TArrayList_Cardinal.Create);
    tileData := tileData_managed as TArrayList_TArrayList_Cardinal;

    for i := 0 to sl.Count - 1 do
    begin
      row := TArrayList_Cardinal.Create;
      tempStr := sl.Strings[i].ToUString;

      for j := 0 to tempStr.Length - 1 do
      begin
        if tempStr.Chars[j] in ['0'..'9'] then
        begin
          row.AddLast(Round(tempStr.Chars[j].GetNumericValue));
        end;
      end;

      tileData.AddLast(row);
    end;

    if not tileData.IsEmpty then
      __Init(tileData, levelWidth, levelHeight);
  finally
    sl.Free;
  end;
end;

procedure TGameLevel.__Init(tileData: TArrayList_TArrayList_Cardinal;
  levelWidth, levelHeight: Cardinal);
var
  unit_width, unit_height: float;
  height, width, y, x, i: Integer;
  pos, size: TVec2;
  obj: TGameObject;
  color: TVec3;
begin
  // 尺寸计算
  height := tileData.Count;
  // 注意，我们可以在[0]处索引vector，因为这个函数只有在高度> 0时才被调用
  width := tileData[0].Count;

  unit_width := float(levelWidth / width);
  unit_height := float(levelHeight / height);

  // 基于 tileData 初始化关卡贴图
  for y := 0 to height - 1 do
  begin
    for x := 0 to width - 1 do
    begin
      // 从关卡数据中检查块类型（2D关卡数组）
      if (tileData[y][x] = 1) then // solid
      begin
        pos := TGLM.Vec2(unit_width * x, unit_height * y);
        size := TGLM.Vec2(unit_width, unit_height);
        color := TGLM.Vec3(0.8, 0.8, 0.7);
        obj := TGameObject.Create(pos, size,
          TResourceManager.GetTexture('block_solid'), @color);
        obj.IsSolid := true;
        Bricks.AddLast(obj);
      end
      else if (tileData[y][x] > 1) then	// 现在根据关卡数据确定它的颜色
      begin
        color := TGLM.Vec3(1.0); // 原始:白色
        if tileData[y][x] = 2 then
         color := TGLM.Vec3(0.2, 0.6, 1.0)
        else if tileData[y][x] = 3 then
         color := TGLM.Vec3(0.0, 0.7, 0.0)
        else if tileData[y][x] = 4 then
         color := TGLM.Vec3(0.8, 0.8, 0.4)
        else if tileData[y][x] = 5 then
         color := TGLM.Vec3(1.0, 0.5, 0.0);

        pos := TGLM.Vec2(unit_width * x, unit_height * y);
        size := TGLM.Vec2(unit_width, unit_height);
        Bricks.AddLast(TGameObject.Create(pos, size,
          TResourceManager.GetTexture('block'), @color));
      end;
    end;
  end;

  if not tileData.IsEmpty then
  begin
    for i := 0 to tileData.Count - 1 do
      tileData[i].Free;
  end;
end;

end.

