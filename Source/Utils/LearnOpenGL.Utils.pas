unit LearnOpenGL.Utils;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
interface

uses
  Classes,
  SysUtils,
  Graphics,
  IntfGraphics,
  //IntfGraphics,
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
    _varSwapRB_PNG: boolean;
    _varSwapRB_TGA: boolean;
    _pixels: array of GLuint;
    _width, _height: GLint;

    function __getDate: Pointer;
    procedure __loadTextureFormFile(fileName: string);
    function __loadJPGTexture(Filename: string): boolean;
    function __loadPNGTexture(Filename: string): boolean;

  public
    constructor Create(fileName: string);
    destructor Destroy; override;

    property Width: GLint read _width;
    property Height: GLint read _height;
    property Pixels: Pointer read __getDate;
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
  inherited Destroy;
end;

function TOpenGLTexture.__getDate: Pointer;
begin
  Result := @_pixels[0];
end;

function TOpenGLTexture.__loadJPGTexture(Filename: string): boolean;
var
  JPG: TJPEGImage;
  img: TLazIntfImage;
  h, w: integer;
  Line: PCardinal;
  c: longword;
begin
  JPG := TJPEGImage.Create;
  try
    try
      JPG.LoadFromFile(Filename);
    except
      Result := false;
      raise Exception.Create('Couldn''t load JPG - "' + Filename + '"');
    end;

    Self._width := JPG.Width;
    Self._height := jpg.Height;
    SetLength(_pixels, _width * _height);

    img := JPG.CreateIntfImage;
    try
      for h := 0 to Height - 1 do
      begin
        Line := img.GetDataLineStart(Height - h - 1);   // flip JPEG
        for w := 0 to Width - 1 do
        begin   // No Transparent Color
          c := Line^ and $FFFFFF; // Need to do a color swap
          _pixels[w + (h * Width)] := (((c and $FF) shl 16) + (c shr 16) + (c and $FF00)) or $FF000000;
          Inc(Line);
        end;
      end;
    finally
      FreeAndNil(img);
    end;
  finally
    JPG.Free;
  end;
end;

function TOpenGLTexture.__loadPNGTexture(Filename: string): boolean;
var
  PNG: TPNGImage;
  img: TLazIntfImage;
  h, w: integer;
  Line: PCardinal;
  c: longword;
begin
  PNG := TPNGImage.Create;
  try
    try
      PNG.LoadFromFile(Filename);
    except
      Result := false;
      raise Exception.Create('Couldn''t load PNG - "' + Filename + '"');
    end;

    Self._width := PNG.Width;
    Self._height := PNG.Height;
    SetLength(_pixels, _width * _height);

    img := PNG.CreateIntfImage;
    try
      for h := 0 to Height - 1 do
      begin
        Line := img.GetDataLineStart(Height - h - 1);   // flip JPEG
        for w := 0 to Width - 1 do
        begin    // With Transparent Color
          c := Line^;

          if _varSwapRB_PNG then
            _pixels[w + (h * Width)] := ((c and $FF) shl 16) or ((c and $FF0000) shr 16) or c and $FF00FF00
          else
            _pixels[w + (h * Width)] := c;

          Inc(Line);
        end;
      end;
    finally
      FreeAndNil(img);
    end;
  finally
    PNG.Free;
  end;

  Result := true;
end;

procedure TOpenGLTexture.__loadTextureFormFile(fileName: string);
var
  xss: string;
begin
  {$IF DEFINED(LCLWin32) or DEFINED(LCLWin64)}
  _varSwapRB_PNG := true;
  _varSwapRB_TGA := true;
  {$elseif defined(LCLWinCE)}
  _varSwapRB_PNG := false;
  _varSwapRB_TGA := true;
  {$elseif defined(LCLGtk) or defined(LCLGtk2)}
  _varSwapRB_PNG := false;
  _varSwapRB_TGA := true;
  {$elseif defined(LCLGtk3)}
  _varSwapRB_PNG := false;
  _varSwapRB_TGA := false;
  {$elseif defined(LCLQt)}
  _varSwapRB_PNG := true;
  _varSwapRB_TGA := true;
  {$elseif defined(LCLQt5)}
  _varSwapRB_PNG := true;
  _varSwapRB_TGA := true;
  {$elseif defined(LCLCarbon)}
  _varSwapRB_PNG := false;
  _varSwapRB_TGA := false;
  {$elseif defined(LCLCocoa)}
  _varSwapRB_PNG := false;
  _varSwapRB_TGA := false;
  {$elseif defined(LCLCustomDrawn)}
  _varSwapRB_PNG := false;
  _varSwapRB_TGA := false;
  {$elseif defined(LCLFpGUI)}
  _varSwapRB_PNG := false;
  _varSwapRB_TGA := false;
  {$ELSE}
  _varSwapRB_PNG := false;
  _varSwapRB_TGA := false;
  {$ENDIF}

  //═════════════════════════════════════════════════════════════════════════

  xss := Copy(filename, length(filename) - 3, 4);

  if SameText(xss, '.bmp') then
  ;//LoadBMPTexture(Filename, Texture, LoadFromRes);

  if SameText(xss, '.png') then
    __loadPNGTexture(Filename);

  if SameText(xss, '.jpg') then
    __loadJPGTexture(Filename);

  if SameText(xss, '.gif') then
  ;  //LoadGIFTexture(Filename, Texture, LoadFromRes);

  if SameText(xss, '.tga') then;
  //adTGATexture(Filename, Texture, LoadFromRes);
end;

end.
