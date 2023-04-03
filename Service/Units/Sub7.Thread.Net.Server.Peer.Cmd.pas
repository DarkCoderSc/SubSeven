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

unit Sub7.Thread.Net.Server.Peer.Cmd;

interface

uses System.Classes, S7Protocol, XSuperObject, Sub7.Thread.Net.Server.Peer.Base;

type
  TSub7PeerCmd = class(TSub7ServerPeerBase)
  protected
    {@M}
    procedure OnPeerExecute(); override;

    procedure OnBeforeCommandLoop(); virtual; abstract;
    procedure OnCommand(const ACommand : TS7Command; const AData : ISuperObject = nil); virtual; abstract;
    procedure OnAfterCommandLoop(); virtual; abstract;
  end;

implementation

uses Winapi.Windows, S7Exceptions, S7Consts, UntDebug, System.SysUtils,
     Sub7.OpenSSL.TLS.Exceptions;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TSub7PeerCmd.OnPeerExecute();
var ACommand : TS7Command;
    AData    : ISuperObject;
begin
  try
    self.OnBeforeCommandLoop();
    ///

    while not Terminated do begin
      try
        FIOHandler.GetCommand(ACommand, AData);

        ///
        self.OnCommand(ACommand, AData);
      except
        on E : ETLSSocketException do
          raise;

        on E : EOpenSSLException do
          raise;

        on E : Exception do
          FIOHandler.SendException(E);
      end;
    end;
  finally
    self.OnAfterCommandLoop();
  end;
end;

end.
