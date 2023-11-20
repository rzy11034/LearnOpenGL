unit LearnOpenGL.Texture;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  FpImage,
  FPReadJPEG,
  FPReadPNG,
  DeepStar.OpenGL.GLAD_GL;

type
  TTexture = class(TObject)
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

{ TTexture }

constructor TTexture.Create(const fileName: string);
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

destructor TTexture.Destroy;
begin
  inherited Destroy;
end;

function TTexture.__FPColorToColor(const Value: TFPColor): GLuint;
begin
  Result := ((Value.Red shr 8) and $FF)
    or (Value.Green and $FF00)
    or ((Value.Blue shl 8) and $FF0000);
end;

function TTexture.__getDate: Pointer;
begin
  Result := @_pixels[0];
end;

end.
