unit Case07_03_2D_Game_Sound;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  libSDL2_mixer,
  Case07_03_2D_Game_Consts;

type
  TSound = class(TInterfacedObject)
  private
    _Success: Boolean;
    _Breakout: PMix_Music;
    _Powerup: PMix_Chunk;
    _Bleep1: PMix_Chunk;
    _Bleep2: PMix_Chunk;
    _Solid: PMix_Chunk;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init;

    procedure Bleep1Play;
    procedure Bleep2Play;
    procedure BreakoutPlay;
    procedure PowerupPlay;
    procedure SolidPlay;
  end;

implementation

{ TSound }

constructor TSound.Create;
begin
  _Success := true;

  libSDL2_MIX_Load;

  if not libSDL2_MIX_IsLoaded then
    raise Exception.Create('libSDL2_MIX failed to load!')
  else
  begin
    if Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 2048) < 0 then
    begin
      raise Exception.Create('SDL_mixer could not initialize! SDL_mixer Error');
      _Success := false;
    end;
  end;
end;

procedure TSound.Bleep1Play;
begin
  Mix_PlayChannel(-1, _Bleep1, 0);
end;

procedure TSound.Bleep2Play;
begin
  Mix_PlayChannel(-1, _Bleep2, 0);
end;

procedure TSound.BreakoutPlay;
begin
  Mix_PlayMusic(_Breakout, -1);
end;

destructor TSound.Destroy;
begin
  Mix_CloseAudio;

  if _Powerup <> nil then
  begin
    Mix_FreeChunk(_Powerup);
    _Powerup := nil;
  end;

  if _Bleep1 <> nil then
  begin
    Mix_FreeChunk(_Bleep1);
    _Powerup := nil;
  end;

  if _Bleep2 <> nil then
  begin
    Mix_FreeChunk(_Bleep2);
    _Bleep2 := nil;
  end;

  if _Solid <> nil then
  begin
    Mix_FreeChunk(_Solid);
    _Solid := nil;
  end;

  if _Breakout <> nil then
  begin
    Mix_FreeMusic(_Breakout);
    _Breakout := nil;
  end;

  if libSDL2_MIX_IsLoaded then
    libSDL2_MIX_Free;

  inherited Destroy;
end;

procedure TSound.Init;
begin
  if not _Success then Exit;

  _Breakout := Mix_LoadMUS(CrossFixFileName(WAV_BREAKOUT).ToPAnsiChar);
  if _Breakout = nil then
    raise Exception.Create('Failed to load ''breakout.mp3''!');

  _Powerup := Mix_LoadWAV(CrossFixFileName(WAV_POWERUP).ToPAnsiChar);
  if _Powerup = nil then
    raise Exception.Create('Failed to load ''powerup.wav''!');

  _Bleep1 := Mix_LoadWAV(CrossFixFileName(WAV_BLEEP1).ToPAnsiChar);
  if _Bleep1 = nil then
    raise Exception.Create('Failed to load ''bleep.mp3''!');

  _Bleep2 := Mix_LoadWAV(CrossFixFileName(WAV_BLEEP2).ToPAnsiChar);
  if _Bleep2 = nil then
    raise Exception.Create('Failed to load ''bleep.wav''!');

  _Solid := Mix_LoadWAV(CrossFixFileName(WAV_SOLID).ToPAnsiChar);
  if _Solid = nil then
    raise Exception.Create('Failed to load ''solid.wav''!');
end;

procedure TSound.PowerupPlay;
begin
  Mix_PlayChannel(-1, _Powerup, 0);
end;

procedure TSound.SolidPlay;
begin
  Mix_PlayChannel(-1, _Solid, 0);
end;

end.

