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

unit Sub7.Service.External.Helper;

interface

uses Winapi.Windows, Sub7.Core.Protocol, XSuperObject;

function RunHelperCommand(const ACommand : TS7HelperCommand; const AData : ISuperObject; ASessionId : Integer = -1 {Current}) : ISuperObject;

implementation

uses Sub7.Core.Exceptions, Sub7.Core.Windows.Sessions.Enum, Sub7.Core.Bundle, System.SysUtils,
     Sub7.Core.Windows.Process, Sub7.Core.Diagnostic, Sub7.Core.Utils,
     Sub7.Core.Application.Env;

{-------------------------------------------------------------------------------
  Run subseven code through Sub7Helper in defined desktop context
-------------------------------------------------------------------------------}
function RunHelperCommand(const ACommand : TS7HelperCommand; const AData : ISuperObject; ASessionId : Integer = -1 {Current}) : ISuperObject;
var AProcess          : TS7SpawnProcessHelper;
    AParam            : ISuperObject;
    AResponse         : ISuperObject;
    AExceptionMessage : String;
    AResponseString   : String;

    function Escape(const AParameter : String) : String;
    begin
      result := StringReplace(AParameter, '"', '[-*qt]', [rfReplaceAll]);
    end;

begin
  result := nil;
  ///

  if ASessionId = -1 then
    ASessionId := GetActiveTerminalSessionId();
  ///

  if not FileExists(APP_ENV_HelperFile) then
    raise ES7FileException.Create(Format(ERR_FILE_MISSING, [APP_ENV_HelperFile]));
  ///

  AParam := TSuperObject.Create();

  AParam.I['command'] := Integer(ACommand);

  if Assigned(AData) then
    AParam.O['data'] := AData;

  AProcess := TS7SpawnProcessHelper.Create(Format('%s "%s"', [APP_ENV_HelperFile, Escape(AParam.AsJson())]));
  try
    AProcess.TermSessionId          := ASessionId;
    AProcess.SpawnMode              := spmCreateInTermSession;
    AProcess.ShowWindow             := False;
    AProcess.AttachToCurrentProcess := False;
    ///

    AResponseString := AProcess.SpawnCaptureOutput(4000);
    try
      AResponse := TSuperObject.Create(AResponseString);

      if not AResponse.Contains('success') then
        raise Exception.Create('');
    except
      on E : Exception do
        raise ES7ParseException.Create('Helper likely failed to execute. Could not handle result.');
    end;

    if not AResponse.B['success'] then begin
      if AResponse.Contains('reason') then
        AExceptionMessage := AResponse.S['reason'];

      raise ES7HelperException.Create(AExceptionMessage);
    end;

    {
      On Success
    }
    if AResponse.Contains('data') then
      result := AResponse.O['data'];
  finally
    if Assigned(AProcess) then
      FreeAndNil(AProcess);
  end;
end;

end.
