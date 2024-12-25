unit Case07_03_2D_Game_Consts;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}

interface

uses
  Classes,
  SysUtils;

const
  SHADER_PATH = '..\Source\7.In_Practice\3.2D_Game\';

  PARTICLE_NAME = 'particle';
  PARTICLE_VS = SHADER_PATH + 'particle.vs';
  PARTICLE_FS = SHADER_PATH + 'particle.fs';

  POST_PROCESSING_NAME = 'post_processing';
  POST_PROCESSING_VS = SHADER_PATH + 'post_processing.vs';
  POST_PROCESSING_FS = SHADER_PATH + 'post_processing.fs';

  SPRITE_NAME = 'sprite';
  SPRITE_VS = SHADER_PATH + 'sprite.vs';
  SPRITE_FS = SHADER_PATH + 'sprite.fs';

  TEXT_2D_NAME = 'text_2d';
  TEXT_2D_VS = SHADER_PATH + 'text_2d.vs';
  TEXT_2D_FS = SHADER_PATH + 'text_2d.fs';

const
  IMG_PATH = '..\Resources\textures\';

  IMG_AWESOMEFACE_NAME = 'awesomeface';
  IMG_AWESOMEFACE = IMG_PATH + 'awesomeface.png';

  IMG_BLOCK_NAME = 'block';
  IMG_BLOCK = IMG_PATH + 'block.png';

  IMG_BLOCK_SOLID_NAME = 'block_solid';
  IMG_BLOCK_SOLID = IMG_PATH + 'block_solid.png';

  IMG_BACKGROUND_NAME = 'background';
  IMG_BACKGROUND = IMG_PATH + 'background.jpg';

  IMG_PADDLE_NAME ='paddle';
  IMG_PADDLE =  IMG_PATH + 'paddle.png';

  IMG_PARTICLE_NAME = 'particle';
  IMG_PARTICLE = IMG_PATH + 'particle.png';

  IMG_POWERUP_CHAOS_NAME = 'powerup_chaos';
  IMG_POWERUP_CHAOS = IMG_PATH + 'powerup_chaos.png';

  IMG_POWERUP_CONFUSE_NAME = 'powerup_confuse';
  IMG_POWERUP_CONFUSE = IMG_PATH + 'powerup_confuse.png';

  IMG_POWERUP_INCREASE_NAME = 'powerup_increase';
  IMG_POWERUP_INCREASE = IMG_PATH + 'powerup_increase.png';

  IMG_POWERUP_PASSTHROUGH_NAME = 'powerup_passthrough';
  IMG_POWERUP_PASSTHROUGH = IMG_PATH + 'powerup_passthrough.png';

  IMG_POWERUP_SPEED_NAME = 'powerup_speed';
  IMG_POWERUP_SPEED = IMG_PATH + 'powerup_speed.png';

  IMG_POWERUP_STICKY_NAME = 'powerup_sticky';
  IMG_POWERUP_STICKY = IMG_PATH + 'powerup_sticky.png';

const
  LEVELS_PATH = '..\Source\7.In_Practice\3.2D_Game\levels\';

  LEVEL_1 = LEVELS_PATH + 'one.lvl';
  LEVEL_2 = LEVELS_PATH + 'two.lvl';
  LEVEL_3 = LEVELS_PATH + 'three.lvl';
  LEVEL_4 = LEVELS_PATH + 'four.lvl';

const
  WAV_PATH = '..\Resources\audio\';

  WAV_BREAKOUT = WAV_PATH + 'breakout.mp3';
  WAV_POWERUP = WAV_PATH + 'powerup.wav';
  WAV_BLEEP_MP3 = WAV_PATH + 'bleep.mp3';
  WAV_BLEEP_WAV = WAV_PATH + 'bleep.wav';
  WAV_SOLID = WAV_PATH + 'solid.wav';

const
  FONT_OCRAEXT = '..\Resources\fonts\ocraext.TTF';

implementation

end.

