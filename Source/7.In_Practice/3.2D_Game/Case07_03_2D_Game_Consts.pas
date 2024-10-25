﻿unit Case07_03_2D_Game_Consts;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$ModeSwitch advancedrecords}
{$ModeSwitch implicitfunctionspecialization}
{$ModeSwitch anonymousfunctions}
{$ModeSwitch functionreferences}

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

const
  LEVELS_PATH = '..\Source\7.In_Practice\3.2D_Game\levels\';

  LEVEL_1 = LEVELS_PATH + 'one.lvl';
  LEVEL_2 = LEVELS_PATH + 'two.lvl';
  LEVEL_3 = LEVELS_PATH + 'three.lvl';
  LEVEL_4 = LEVELS_PATH + 'four.lvl';

implementation

end.

