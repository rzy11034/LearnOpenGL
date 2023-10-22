unit LearnOpenGL.Utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
interface

uses
  Classes,
  SysUtils,
  FpImage,
  FPReadJPEG,
  FPReadPNG,
  DeepStar.Utils,
  GLAD_GL;

const
  LE = LineEnding;

type
  TArr_GLboolean = array of GLboolean;
  TArr_GLfloat = array of GLfloat;
  TArr_GLint = array of GLint;
  TArr_GLuint = array of GLuint;
  TArr_GLchar = array of GLchar;
  TArr_GLdouble = array of GLdouble;

  TArrayUtils_GLfloat = specialize TArrayUtils<GLfloat>;
  TArrayUtils_GLint = specialize TArrayUtils<GLint>;

type
  TOpenGLTexture = class(TObject)
  private
    _pixels: array of GLuint;
    _width, _height: GLint;

    function __getDate: Pointer;
    function __FPColorToColor(const Value: TFPColor): GLuint;

  public
    constructor Create(const fileName: string);
    destructor Destroy; override;

    property Width: GLint read _width;
    property Height: GLint read _height;
    property Pixels: Pointer read __getDate;
  end;

implementation

{ TOpenGLTexture }

constructor TOpenGLTexture.Create(const fileName: string);
var
  img: TFPMemoryImage;
  readerClass: TFPCustomImageReaderClass;
  reader: TFPCustomImageReader;
  y: GLint;
  x: integer;
  c: TFPColor;
begin
  inherited Create;

  img := TFPMemoryImage.Create(0, 0);
  readerClass := img.FindReaderFromFileName(fileName);
  reader := readerClass.Create;

  try
    img.LoadFromFile(fileName, reader);

    _width := img.Width;
    _height := img.Height;

    SetLength(_pixels, Width * Height);

    for y := Height - 1 downto 0 do
      for x := 0 to Width - 1 do
      begin
        c := img.Colors[x, y];

        _pixels[x + ((Height - y - 1) * Width)] := __FPColorToColor(c);
      end;
  finally
    reader.Free;
    img.Free;
  end;
end;

destructor TOpenGLTexture.Destroy;
begin
  inherited Destroy;
end;

function TOpenGLTexture.__FPColorToColor(const Value: TFPColor): GLuint;
begin
  Result := ((Value.Red shr 8) and $FF)
    or (Value.Green and $FF00)
    or ((Value.Blue shl 8) and $FF0000);
end;

function TOpenGLTexture.__getDate: Pointer;
begin
  Result := @_pixels[0];
end;

end.
