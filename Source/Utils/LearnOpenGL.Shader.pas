unit LearnOpenGL.Shader;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils,
  DeepStar.UString,
  GLAD_GL;

type
  TShader = class(TObject)
  private
    _id: GLuint;

  public
    procedure LoadShaderFile(const vertexFile, fragmentFile: string);

    constructor Create();
    constructor Create(const vertexFile, fragmentFile: string);
    destructor Destroy; override;

  end;

implementation

{ TShader }

constructor TShader.Create(const vertexFile, fragmentFile: string);
var
  sl: TStringList;
begin
  inherited Create;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(vertexFile);
    WriteLn(vertexFile);
    WriteLn('===================');
    sl.Clear;

    WriteLn(sl.Text);
  finally
    sl.Free;
  end;
end;

constructor TShader.Create();
begin
  inherited;
end;

destructor TShader.Destroy;
begin
  inherited Destroy;
end;

procedure TShader.LoadShaderFile(const vertexFile, fragmentFile: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(vertexFile);
    WriteLn(sl.Text);
    WriteLn('===================');
    sl.Clear;

    WriteLn(sl.Text);
  finally
    sl.Free;
  end;
end;

end.
