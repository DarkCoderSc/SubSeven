{******************************************************************************}
{                                                                              }
{         ____             _     ____          _           ____                }
{        |  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___          }
{        | | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|         }
{        | |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__          }
{        |____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|         }
{                                                                              }
{                           +++++++++++++++++++++                              }
{                           +       +           +                              }
{                           +       +           +                              }
{                           +    +++++++++      +                              }
{                           +            +      +                              }
{                           +            +      +                              }
{                           +++++++      +      +                              }
{                           +            +      +                              }
{                           +            +      +                              }
{                           +++++++++++++++++++++                              }
{                                 SubSeven Legacy                              }
{                                                                              }
{                                                                              }
{                   Author: DarkCoderSc (Jean-Pierre LESUEUR)                  }
{                   https://www.twitter.com/                                   }
{                   https://github.com/darkcodersc                             }
{                   License: Apache License 2.0                                }
{                                                                              }
{                                                                              }
{  Disclaimer:                                                                 }
{  -----------                                                                 }
{    We are doing our best to prepare the content of this app and/or code.     }
{    However, The author cannot warranty the expressions and suggestions       }
{    of the contents, as well as its accuracy. In addition, to the extent      }
{    permitted by the law, author shall not be responsible for any losses      }
{    and/or damages due to the usage of the information on our app and/or      }
{    code.                                                                     }
{                                                                              }
{    By using our app and/or code, you hereby consent to our disclaimer        }
{    and agree to its terms.                                                   }
{                                                                              }
{    Any links contained in our app may lead to external sites are provided    }
{    for convenience only.                                                     }
{    Any information or statements that appeared in these sites or app or      }
{    files are not sponsored, endorsed, or otherwise approved by the author.   }
{    For these external sites, the author cannot be held liable for the        }
{    availability of, or the content located on or through it.                 }
{    Plus, any losses or damages occurred from using these contents or the     }
{    internet generally.                                                       }
{                                                                              }
{                                                                              }
{                                                                              }
{    I dedicate this work to my daughter.                                      }
{                                                                              }
{******************************************************************************}

unit Sub7.Core.Diagnostic;

interface

uses WinAPI.Windows, System.SysUtils, System.SyncObjs;

procedure Log(const AString : String); overload;
procedure Log(const AInteger : Int64); overload;
procedure Log(const ABoolean : Boolean); overload;
procedure Log(pBuffer : Pointer; ABufferSize : Cardinal); overload;

procedure CrashLog(const AException : Exception; AClass : TObject);
procedure LastErrorLog(AClass : TObject; const AFuncName : String; const AWinAPI : String);

function Ternary(ABoolean : Boolean; const APassed, AFailed : String) : String;
procedure odbs(const AString : String);

var G_LogLock        : TCriticalSection = nil;
    G_LogFilePath    : String           = '';
    G_LogFileOpenned : Boolean          = False;
    G_LogFile        : TextFile;

implementation

uses Sub7.Core.Application.Env, Sub7.Core.FileSystem.Utils;

procedure odbs(const AString : String);
begin
  OutputDebugString(PWideChar(AString));
end;

function Ternary(ABoolean : Boolean; const APassed, AFailed : String) : String;
begin
  if ABoolean then
    result := APassed
  else
    result := AFailed;
end;

procedure Log(const AString : String);
begin
  if not G_LogFileOpenned or
     not Assigned(G_LogLock) then
    Exit();
  ///

  try
    G_LogLock.Acquire();
    try
      WriteLn(G_LogFile, Format('%s/(%s) | %s', [DateTimeToStr(Now), APP_ENV_FileName, AString]));

      Flush(G_LogFile);
    finally
      G_LogLock.Leave();
    end;
  except

  end;
end;

procedure Log(const AInteger : Int64);
begin
  Log(IntToStr(AInteger));
end;

procedure Log(const ABoolean : Boolean);
begin
  if ABoolean then
    Log('True')
  else
    Log('False');
end;

procedure Log(pBuffer : Pointer; ABufferSize : Cardinal);
var AOutput : String;
    I       : Cardinal;
begin
  AOutput := '';
  ///

  for I := 0 to ABufferSize -1 do begin
    AOutput := AOutput + IntToHex(PByte(NativeUInt(pBuffer) + I)^);
  end;

  if (Length(AOutput) > 0) then
    Log(AOutput);
end;

procedure CrashLog(const AException : Exception; AClass : TObject);
var AClassName : String;
begin
  if Assigned(AClass) then
    AClassName := AClass.ClassName
  else
    AClassName := 'NULL';
  ///

  Log(Format('%s___%s: %s.', [
                                AException.ClassName,
                                AClassName,
                                AException.Message
  ]));
end;

procedure LastErrorLog(AClass : TObject; const AFuncName : String; const AWinAPI : String);
begin
  Log(Format('%s->%s: WinAPI=[%s], LastError=[%d] ErrorMessage=[%s]', [
                                            AClass.ClassName,
                                            AFuncName,
                                            AWinAPI,
                                            GetLastError(),
                                            SysErrorMessage(GetLastError())

  ]));
end;

initialization
  G_LogLock := TCriticalSection.Create();
  try
    G_LogFilePath := Format('%s%s.log', [APP_ENV_CurrentDirectory, APP_ENV_FileNameNoExt]);
    ///

    AssignFile(
                G_LogFile,
                G_LogFilePath
    );

    // If log file is > 10 MiB then we flush it
    if GetFileSize(G_LogFilePath) > (1024 * 1024) * 10 then
      DeleteFile(G_LogFilePath);

    if FileExists(G_LogFilePath) then
      Append(G_LogFile)
    else
      Rewrite(G_LogFile);

    ///
    G_LogFileOpenned := True;
  except
    on E : Exception do begin
      OutputDebugString(PWideChar(Format('Log file initialization failed: "%s".', [E.Message])));
      ///

      if G_LogFileOpenned then
        CloseFile(G_LogFile);
    end;
  end;

finalization
  if G_LogFileOpenned then begin
    if Assigned(G_LogLock) then
      G_LogLock.Acquire();
    try
      CloseFile(G_LogFile);
    finally
      if Assigned(G_LogLock) then begin
        G_LogLock.Leave();

        FreeAndNil(G_LogLock);
      end;
    end;
  end;

end.
