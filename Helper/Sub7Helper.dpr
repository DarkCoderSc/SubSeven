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

program Sub7Helper;

{$APPTYPE CONSOLE}

{$R *.res}

{$R version.res}

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.ShellAPI,
  Winapi.ActiveX,
  Sub7.Core.Bundle in '..\Shared\Sub7.Core.Bundle.pas',
  Sub7.Core.Protocol in '..\Shared\Sub7.Core.Protocol.pas',
  Sub7.Core.Exceptions in '..\Shared\Sub7.Core.Exceptions.pas',
  XSuperObject in '..\Shared\XSuperObject.pas',
  XSuperJson in '..\Shared\XSuperJson.pas';

{-------------------------------------------------------------------------------
  Test Functions
-------------------------------------------------------------------------------}

function TestSuccess(const AData : ISuperObject) : ISuperObject;
begin
  result := TSuperObject.Create();

  if Assigned(AData) then
    result.O['data'] := AData;
end;

procedure TestException();
begin
  raise Exception.Create('Error Message');
end;

{-------------------------------------------------------------------------------
  Lock current session
-------------------------------------------------------------------------------}
procedure LockSession();
begin
  if not Winapi.Windows.LockWorkStation() then
    raise ES7WindowsException.Create('LockWorkStation');
end;

{-------------------------------------------------------------------------------
  Logoff from current session
-------------------------------------------------------------------------------}
procedure Logoff();
begin
  if not Winapi.Windows.ExitWindowsEx((EWX_LOGOFF or EWX_FORCEIFHUNG), 0) then
    raise ES7WindowsException.Create('ExitWindowsEx');
end;

{-------------------------------------------------------------------------------
  ShellExecute(open)
-------------------------------------------------------------------------------}
procedure Open(const AData : ISuperObject);
var ACommand    : String;
    ARet        : Integer;
    AErrMessage : String;
    AVerb       : String;
begin
  if not AData.Contains('command') then
    raise Exception.Create(Format(ERR_MISSING_PARAMETER, ['command']));

  ACommand := AData.S['command'];


  if AData.Contains('runas') then
    AVerb := 'runas'
  else
    AVerb := 'open';

  ARet := ShellExecuteW(0, PWideChar(AVerb), PWideChar(ACommand), nil, nil, SW_SHOWNORMAL);

  AErrMessage := '';
  case ARet of
    0                      : AErrMessage := 'The operating system is out of memory or resources.';
    ERROR_FILE_NOT_FOUND   : AErrMessage := 'The specified file was not found.';
    ERROR_PATH_NOT_FOUND   : AErrMessage := 'The specified path was not found.';
    ERROR_BAD_FORMAT       : AErrMessage := 'The .exe file is invalid (non-Win32 .exe or error in .exe image).';
    SE_ERR_ACCESSDENIED    : AErrMessage := 'The operating system denied access to the specified file.';
    SE_ERR_ASSOCINCOMPLETE : AErrMessage := 'The file name association is incomplete or invalid.';
    SE_ERR_DDEBUSY         : AErrMessage := 'The DDE transaction could not be completed because other DDE transactions were being processed.';
    SE_ERR_DDEFAIL         : AErrMessage := 'The DDE transaction failed.';
    SE_ERR_DDETIMEOUT      : AErrMessage := 'The DDE transaction could not be completed because the request timed out.';
    SE_ERR_DLLNOTFOUND     : AErrMessage := 'The specified DLL was not found.';
    SE_ERR_NOASSOC         : AErrMessage := 'There is no application associated with the given file name extension. This error will also be returned if you attempt to print a file that is not printable.';
    SE_ERR_OOM             : AErrMessage := 'There was not enough memory to complete the operation.';
    SE_ERR_SHARE           : AErrMessage := 'A sharing violation occurred.';
  end;

  if AErrMessage <> '' then
    raise ES7SystemException.Create(AErrMessage);

end;

{-------------------------------------------------------------------------------
  ___entrypoint
-------------------------------------------------------------------------------}
var FInput      : ISuperObject;
    FOutput     : ISuperObject;
    FCommand    : TS7HelperCommand;
    FSuccess    : Boolean;
    FInputData  : ISuperObject;
    FOutputData : ISuperObject;

    function uEscape(const AParam : String) : String;
    begin
      result := StringReplace(AParam, '[-*qt]', '"', [rfReplaceAll]);
    end;

begin
  FOutputData := nil;
  FSuccess := False;
  try
    FOutput := TSuperObject.Create();
    try
      if (ParamCount <> 1) then
        raise Exception.Create('Application must have a super object in input.');
      ///

      FInput := TSuperObject.Create(uEscape(ParamStr(1)));

      if not FInput.Contains('command') then
        raise Exception.Create(Format(ERR_MISSING_PARAMETER, ['command']));

      FCommand := TS7HelperCommand(FInput.I['command']);

      FInputData := nil;
      if FInput.Contains('data') then
        FInputData := FInput.O['data'];
      ///

      case FCommand of
        hcTestSuccess   : FOutputData := TestSuccess(FInputData);
        hcTestException : TestException();
        hcLockComputer  : LockSession();
        hcLogoff        : Logoff();
        hcOpen          : Open(FInputData);
      end;

      ///
      FSuccess := True;
    except
      on E: Exception do begin
        FOutput.S['reason'] := E.Message;
      end;
    end;
  finally
    FOutput.B['success'] := FSuccess;

    if Assigned(FOutputData) then
      FOutput.O['data'] := FOutputData;

    ///
    Write(FOutput.AsJson());
  end;
end.
