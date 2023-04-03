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

unit Sub7.Thread.Net.Server.Peer.NonBlockingCmd;

interface

uses System.Classes, Sub7.Thread.Net.Server.Peer.Base, System.SyncObjs, Sub7.Core.Protocol,
     XSuperObject, Generics.Collections, Sub7.Core.Thread;

type
  TSub7PeerNonBlockingCmd = class(TSub7ServerPeerBase)
  private
    {@M}
    procedure OnPeerExecute(); override;
  protected
    procedure OnIdle(); virtual;
    procedure OnBeforeCommandLoop(); virtual; abstract;
    procedure OnCommand(const ACommand : TS7Command; const AData : ISuperObject = nil); virtual; abstract;
    procedure OnAfterCommandLoop(); virtual; abstract;
  public
  end;

implementation

uses Winapi.Windows, System.SysUtils, Sub7.OpenSSL.TLS.Exceptions, Sub7.Core.Diagnostic,
     Sub7.Core.Exceptions;

{-------------------------------------------------------------------------------
  ___idle
-------------------------------------------------------------------------------}
procedure TSub7PeerNonBlockingCmd.OnIdle();
begin
  Sleep(10);
end;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TSub7PeerNonBlockingCmd.OnPeerExecute();
var ACommand : TS7Command;
    AData    : ISuperObject;
begin
  try
    self.OnBeforeCommandLoop();
    ///

    while not Terminated do begin
      try
        self.OnIdle();
        ///

        if not FIOHandler.DataAvailable then
          continue;

        FIOHandler.GetCommand(ACommand, AData);

        self.OnCommand(ACommand, AData);
      except
        on E : ETLSSocketException do
          raise;

        on E : EOpenSSLBaseException do
          raise;

        on E : ES7Exception do
          FIOHandler.SendException(E);
      end;
    end;
  finally
    self.OnAfterCommandLoop();
  end;
end;

end.
