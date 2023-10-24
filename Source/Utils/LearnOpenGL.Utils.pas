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

  TVec4f = array[0..3] of GLfloat;

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

function RGBAToOpenGLColor(red, green, blue: GLubyte; alpha: GLubyte = 0): TVec4f;
function HtmlRGBToOpenGLColor(HtmlColor: GLuint): TVec4f;

implementation

function RGBAToOpenGLColor(red, green, blue: GLubyte; alpha: GLubyte): TVec4f;
var
  res: TVec4f;
begin
  res[0] := red / 255;
  res[1] := green / 255;
  res[2] := blue / 255;
  res[3] := alpha / 255;
  Result := res;
end;

function HtmlRGBToOpenGLColor(HtmlColor: GLuint): TVec4f;
var
  r, g, b, a: GLubyte;
  p: PGLubyte;
begin
  p := @HtmlColor;
  b := p[0];
  g := p[1]; //Inc(p);
  r := p[2]; //Inc(p);
  a := p[3];

  Result := RGBAToOpenGLColor(r, g, b, a);
end;

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
