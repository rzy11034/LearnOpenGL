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
    constructor Create(vertexFile, fragmentFile: string);
    destructor Destroy; override;

  end;

implementation

{ TShader }

constructor TShader.Create(vertexFile, fragmentFile: string);
var
  sl: TStringList;
begin
  inherited;

  sl := TStringList.Create;
  try

  finally
    sl.Free;
  end;
end;

destructor TShader.Destroy;
begin
  inherited Destroy;
end;

end.
