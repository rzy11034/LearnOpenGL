unit Case07_03_2D_Game_Consts;

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

  PARTICLE_VS = SHADER_PATH + 'particle.vs';
  PARTICLE_FS = SHADER_PATH + 'particle.fs';

  POST_PROCESSING_VS = SHADER_PATH + 'post_processing.vs';
  POST_PROCESSING_FS = SHADER_PATH + 'post_processing.fs';

  SPRITE_VS = SHADER_PATH + 'sprite.vs';
  SPRITE_FS = SHADER_PATH + 'sprite.fs';

  TEXT_2D_VS = SHADER_PATH + 'text_2d.vs';
  TEXT_2D_FS = SHADER_PATH + 'text_2d.fs';

implementation

end.

