unit Case07_03_2D_Game_Texture2D;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.OpenGL.GLAD_GL,
  DeepStar.OpenGL.Texture;

type
  TTexture2D = class(TTexture)
  private

  public


    // 保存纹理对象的ID，用于所有纹理操作来引用这个特定的纹理
    ID: Cardinal;

    // 纹理图像尺寸 加载图像的宽度和高度（以像素为单位）
    Width, Height: Cardinal;

    // Texture Format
    Internal_Format: Cardinal; // Format of texture object
    Image_Format: Cardinal; // Format of loaded image

    // Texture configuration
    Wrap_S: Cardinal; // Wrapping mode on S axis
    Wrap_T: Cardinal; // Wrapping mode on T axis
    Filter_Min: Cardinal; // Filtering mode if texture pixels < screen pixels
    Filter_Max: Cardinal; // Filtering mode if texture pixels > screen pixels

    constructor Create;
    destructor Destroy; override;

    // 从图像数据生成纹理
    procedure Generate(width, height: GLuint; data: PByte);

    // 将纹理绑定为当前活动的GL_TEXTURE_2D纹理对象
    procedure Bind;
  end;

implementation

{ TTexture2D }

constructor TTexture2D.Create;
begin
  inherited;

  Internal_Format := GL_RGB;
  Image_Format := GL_RGB;
  Wrap_S := GL_REPEAT;
  Wrap_T := GL_REPEAT;
  Filter_Min := GL_LINEAR;
  Filter_Max := GL_LINEAR;
end;

destructor TTexture2D.Destroy;
begin
  inherited Destroy;
end;

end.

