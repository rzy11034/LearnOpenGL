﻿unit LearnOpenGL.Shader;

{$mode ObjFPC}{$H+}
{$ModeSwitch unicodestrings}{$J-}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
interface

uses
  Classes,
  SysUtils,
  DeepStar.OpenGL.GLAD_GL,
  LearnOpenGL.Utils;

type
  TShaderTypes = (vertexObj, fragmentObj, programObj);

  TShaderProgram = class(TObject)
  private
    _id: GLuint;

    procedure __checkShader(shaderID: GLubyte; shaderType: TShaderTypes);

  public
    procedure LoadShaderFile(const vertexFile, fragmentFile: string);
    // 使用/激活程序
    procedure UseProgram;

    // uniform工具函数
    procedure SetUniformInt(uniform: PGLchar; Value: TArr_GLint);
    procedure SetUniformFloat(uniform: PGLchar; Value: TArr_GLfloat);
    procedure SetUniformMatrix4fv(uniform: PGLchar; Value: PSingle);

    constructor Create();
    destructor Destroy; override;

    property ID: GLuint read _id;
  end;

implementation

{ TShaderProgram }

constructor TShaderProgram.Create();
begin
  inherited;
end;

destructor TShaderProgram.Destroy;
begin
  glDeleteProgram(_id);

  inherited Destroy;
end;

procedure TShaderProgram.LoadShaderFile(const vertexFile, fragmentFile: string);
var
  sl: TStringList;
  vertexStr, fragmentStr: PGLchar;
  vertexShader, fragmentShader, shaderProgram: GLuint;
begin
  sl := TStringList.Create();
  try
    try
      sl.LoadFromFile(vertexFile);
    except
      raise Exception.Create('Vertex File load Error!');
    end;
    vertexStr := PGLchar('');
    vertexStr := PGLchar(sl.Text);

    sl.Clear;

    try
      sl.LoadFromFile(fragmentFile);
    except
      raise Exception.Create('Fragment File load Error!');
    end;
    fragmentStr := PGLchar('');
    fragmentStr := PGLchar(sl.Text);
  finally
    sl.Free;
  end;

  vertexShader := GLuint(0);
  fragmentShader := GLuint(0);
  shaderProgram := GLuint(0);

  vertexShader := glCreateShader(GL_VERTEX_SHADER);
  glShaderSource(vertexShader, 1, @vertexStr, nil);
  glCompileShader(vertexShader);
  __checkShader(vertexShader, TShaderTypes.vertexObj);

  fragmentShader := glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(fragmentShader, 1, @fragmentStr, nil);
  glCompileShader(fragmentShader);
  __checkShader(fragmentShader, TShaderTypes.fragmentObj);

  shaderProgram := glCreateProgram();
  glAttachShader(shaderProgram, vertexShader);
  glAttachShader(shaderProgram, fragmentShader);
  glLinkProgram(shaderProgram);
  __checkShader(shaderProgram, TShaderTypes.programObj);

  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);

  _id := shaderProgram;
end;

procedure TShaderProgram.SetUniformFloat(uniform: PGLchar; Value: TArr_GLfloat);
var
  uniformLocation, len: GLint;
begin
  uniformLocation := GLint(0);
  uniformLocation := glGetUniformLocation(_id, uniform);

  len := GLint(0);
  len := Length(Value);
  if not (len in [1..4]) then
    raise Exception.Create('uniform is illegal!');

  case len of
    1: glUniform1f(uniformLocation, Value[0]);
    2: glUniform2f(uniformLocation, Value[0], Value[1]);
    3: glUniform3f(uniformLocation, Value[0], Value[1], Value[2]);
    4: glUniform4f(uniformLocation, Value[0], Value[1], Value[2], Value[3]);
  end;
end;

procedure TShaderProgram.SetUniformInt(uniform: PGLchar; Value: TArr_GLint);
var
  uniformLocation, len: GLint;
begin
  uniformLocation := GLint(0);
  uniformLocation := glGetUniformLocation(_id, uniform);

  len := GLint(0);
  len := Length(Value);
  if not (len in [1..4]) then
    raise Exception.Create('uniform is illegal!');

  case len of
    1: glUniform1i(uniformLocation, Value[0]);
    2: glUniform2i(uniformLocation, Value[0], Value[1]);
    3: glUniform3i(uniformLocation, Value[0], Value[1], Value[2]);
    4: glUniform4i(uniformLocation, Value[0], Value[1], Value[2], Value[3]);
  end;
end;

procedure TShaderProgram.SetUniformMatrix4fv(uniform: PGLchar; Value: PSingle);
var
  uniformLocation: GLint;
begin
  uniformLocation := GLint(0);
  uniformLocation := glGetUniformLocation(_id, uniform);
  glUniformMatrix4fv(uniformLocation, 1, GL_FALSE, Value);
end;

procedure TShaderProgram.UseProgram;
begin
  glUseProgram(_id);
end;

procedure TShaderProgram.__checkShader(shaderID: GLubyte; shaderType: TShaderTypes);
var
  success: GLint;
  infoLog: TArr_GLchar;
  s: string;
begin
  success := GLint(false.ToInteger);
  infoLog := TArr_GLchar(nil);
  SetLength(infoLog, 512);
  case shaderType of
    vertexObj, fragmentObj:
    begin
      if shaderType = TShaderTypes.vertexObj then
        s := 'VERTEX'
      else
        s := 'FRAGMENT';

      glGetShaderiv(shaderID, GL_COMPILE_STATUS, @success);
      if not success.ToBoolean then
      begin
        glGetShaderInfoLog(shaderID, 512, nil, @infoLog[0]);
        WriteLn('ERROR::SHADER::' + s + '::COMPILATION_FAILED' + LE, PGLchar(infoLog));
      end;
    end;

    programObj:
    begin
      s := 'PROGRAM';

      glGetProgramiv(shaderID, GL_LINK_STATUS, @success);
      if not success.ToBoolean then
      begin
        glGetProgramInfoLog(shaderID, 512, nil, @infoLog[0]);
        WriteLn('ERROR::SHADER::' + s + '::COMPILATION_FAILED' + LE, PAnsiChar(infoLog));
      end;
    end;
  end;
end;

end.
