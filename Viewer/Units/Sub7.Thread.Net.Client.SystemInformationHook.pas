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

unit Sub7.Thread.Net.Client.SystemInformationHook;

interface

uses System.Classes, Sub7.Thread.Net.Client.NonBlockingCmd, XSuperObject, Sub7.Core.Protocol,
     uFrameSystemInformationHook;

type
  TSub7SystemInformationHook = class(TSub7NonBlockingCmd)
  private
    F : TFrameSystemInformationHook;
  protected
    {@M}
    procedure OnBeforeCommandLoop(); override;
    procedure OnAfterCommandLoop(); override;
    procedure OnCommand(const ACommand : TS7Command; AData : ISuperObject = nil); override;
  end;

implementation

uses uFormMain, Sub7.Core.Types, Sub7.Viewer.Types;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7SystemInformationHook.OnBeforeCommandLoop();
begin
  F := nil;

  SafeSynchronize(procedure begin
    F := TFrameSystemInformationHook(FormMain.GetDockedFrame(nmMachineActions));
  end);

  if Assigned(F) then begin
    SafeSynchronize(procedure begin
      F.OnSessionListenerStatusChange(True);
    end);
  end;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7SystemInformationHook.OnAfterCommandLoop();
begin
  if Assigned(F) then begin
    SafeSynchronize(procedure begin
      F.OnSessionListenerStatusChange(False);
    end);
  end;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7SystemInformationHook.OnCommand(const ACommand : TS7Command; AData : ISuperObject = nil);
begin
  if not Assigned(F) then
    Exit();
  ///

  case ACommand of
    {
      Refresh Terminal Sessions
    }
    tshEnumSessions : begin
      SafeSynchronize(procedure begin
        F.RenderSessions(AData);
      end);
    end;

    {
      Windows User Information
    }
    sihUser : begin
      SafeSynchronize(procedure begin
        F.WinUserUpdate(AData);
      end);
    end;
  end;
end;

end.
