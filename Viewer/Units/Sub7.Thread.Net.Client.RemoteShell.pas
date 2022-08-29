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

unit Sub7.Thread.Net.Client.RemoteShell;

interface

uses System.Classes, Sub7.Thread.Net.Client.NonBlockingCmd, Sub7.Core.Protocol,
     XSuperObject, ___S7DockFrame;

type
  TSub7ClientRemoteShell = class(TSub7NonBlockingCmd)
  protected
    F : TS7FrameDock;

    {@M}
    procedure RegisterShellSession(const AData : ISuperObject; const ARegister : Boolean);
    procedure RenderShellOutput(const AData : ISuperObject);
  public
    {@M}
    procedure OnBeforeCommandLoop(); override;
    procedure OnCommand(const ACommand : TS7Command; AData : ISuperObject = nil); override;
    procedure OnAfterCommandLoop(); override;
  end;

implementation

uses uFormMain, System.SysUtils, Sub7.Core.Bundle, Sub7.Core.Types, Sub7.Viewer.Types,
     uFrameShell, Sub7.Core.Exceptions, Winapi.Windows;

{-------------------------------------------------------------------------------
  On Before Command Loop
-------------------------------------------------------------------------------}
procedure TSub7ClientRemoteShell.OnBeforeCommandLoop();
begin
  F := nil;
  ///

  SafeSynchronize(procedure begin
    F := FormMain.GetDockedFrame(nmTerminal);
  end);

  if Assigned(F) then begin
    SafeSynchronize(procedure begin
      TFrameShell(F).OnShellHandlerStart(self);
    end);
  end;
end;

{-------------------------------------------------------------------------------
  On After Command Loop
-------------------------------------------------------------------------------}
procedure TSub7ClientRemoteShell.OnAfterCommandLoop();
begin
  if Assigned(F) then begin
    SafeSynchronize(procedure begin
      TFrameShell(F).OnShellHandlerStop();
    end);
  end;
end;

{-------------------------------------------------------------------------------
  On Receive Data
-------------------------------------------------------------------------------}
procedure TSub7ClientRemoteShell.RenderShellOutput(const AData : ISuperObject);
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('session_id') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['session_id']));

  if not AData.Contains('data') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['data']));

  SafeSynchronize(procedure begin
    TFrameShell(F).RenderShellOutput(AData.S['session_id'], AData.S['data']);
  end);
end;

{-------------------------------------------------------------------------------
  Register new shell session
-------------------------------------------------------------------------------}
procedure TSub7ClientRemoteShell.RegisterShellSession(const AData : ISuperObject; const ARegister : Boolean);
var ASessionId : String;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('session_id') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['session_id']));

  ASessionId := AData.S['session_id'];

  SafeSynchronize(procedure begin
    TFrameShell(F).RegisterShellSession(ASessionId, ARegister);
  end);
end;

{-------------------------------------------------------------------------------
  On Command Received
-------------------------------------------------------------------------------}
procedure TSub7ClientRemoteShell.OnCommand(const ACommand : TS7Command; AData : ISuperObject = nil);
begin
  case ACommand of
    rtNew : begin
      self.RegisterShellSession(AData, True);
    end;

    rtData : begin
      self.RenderShellOutput(AData);
    end;

    rtStopped : begin
      self.RegisterShellSession(AData, False);
    end;
  end;
end;

end.
