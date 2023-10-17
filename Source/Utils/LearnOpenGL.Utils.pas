unit LearnOpenGL.Utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GLAD_GL,
  FpImage,
  FPReadJPEG,
  FPReadBMP,
  FPReadPNG;

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
    procedure __loadTextureFormFile(fileName: string);

  public
    Width, Height: GLint;
    Data: PGLubyte;

    constructor Create(fileName: string);
    destructor Destroy; override;
  end;

implementation

{ TOpenGLTexture }

constructor TOpenGLTexture.Create(fileName: string);
begin
  inherited Create;

  __LoadTextureFormFile(fileName);
end;

destructor TOpenGLTexture.Destroy;
begin
  if Data <> nil then
    FreeMemAndNil(Self.Data);

  inherited Destroy;
end;

procedure TOpenGLTexture.__loadTextureFormFile(fileName: string);
var
  img: TFPMemoryImage;
  readerClass: TFPCustomImageReaderClass;
  Handler: TFPCustomImageReader;
  y, x: integer;
  p: PGLubyte;
  c: TFPColor;
  s: string;
begin
  img := TFPMemoryImage.Create(0, 0);
  readerClass := img.FindReaderFromFileName(fileName);

  if readerClass.ClassName = 'TFPReaderJPEG' then
  begin
    Handler := readerClass.Create;
    try
      try
        img.LoadFromFile(fileName, Handler);
      except
        on e: EFileNotFoundException do
          raise e;
      end;

      Self.Width := img.Width;
      Self.Height := img.Height;

      Self.Data := GetMem(img.Width * img.Height * 3);
      p := Self.Data;

      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
        begin
          c := img.Colors[x, y];
          p^ := c.Red shr 8;
          Inc(p);
          p^ := c.Green shr 8;
          Inc(p);
          p^ := c.Blue shr 8;
          Inc(p);
        end;
    finally
      Handler.Free;
      img.Free;
    end;
  end
  else if readerClass.ClassName = 'TFPReaderPNG' then
  begin
    Handler := readerClass.Create;
    try
      try
        img.LoadFromFile(fileName, Handler);
      except
        on e: EFileNotFoundException do
          raise e;
      end;

      Self.Width := img.Width;
      Self.Height := img.Height;

      Self.Data := GetMem(img.Width * img.Height * 4);
      p := Self.Data;

      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
        begin
          c := img.Colors[x, y];
          p^ := c.Red shr 8;
          Inc(p);
          p^ := c.Green shr 8;
          Inc(p);
          p^ := c.Blue shr 8;
          Inc(p);
          p^ := c.Alpha shr 8;
          Inc(p);
        end;
    finally
      Handler.Free;
      img.Free;
    end;
  end;
end;

end.
