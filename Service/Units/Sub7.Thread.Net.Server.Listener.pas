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

unit Sub7.Thread.Net.Server.Listener;

interface

uses System.Classes, Sub7.OpenSSL.TLS.Socket, Generics.Collections,
     Sub7.Thread.Net.Server.Peer.Session.Cmd, Sub7.Core.Thread, Sub7.OpenSSL.TLS.Context,
     Sub7.TLS.IOHandler, Sub7.Service.Types, Sub7.Core.Thread.Watcher,
     Sub7.Service.Config;

type
  TSub7ServerListener = class(TCoreThread)
  private
    FServer       : TServerSocket;
    FContext      : TOpenSSLContext;
    FServerConfig : TServerConfig;
  protected
    {@M}
    procedure TerminatedSet(); override;
    procedure OnThreadExecute(); override;
    procedure OnDestroyObjects(); override;
  public
    {@C}
    constructor Create(AServerConfig : TServerConfig); overload;

    {@G}
    property ServerConfig : TServerConfig   read FServerConfig;
    property Context      : TOpenSSLContext read FContext;
  end;

implementation

uses Winapi.Windows, System.SysUtils, Sub7.OpenSSL.TLS.Exceptions, Sub7.OpenSSL.TLS.IOHandler,
     Sub7.OpenSSL.Headers, Sub7.Thread.Net.Server.Peer.Base, Sub7.Core.Protocol,
     Sub7.Core.Exceptions, Sub7.Core.Bundle, XSuperObject, Sub7.Core.Diagnostic,
     Winapi.Winsock2, Sub7.Core.Utils, Sub7.Core.Input.Validators, Sub7.Core.Types,
     Sub7.Thread.Net.Server.Session.Registrar, Sub7.Core.Magic;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSub7ServerListener.Create(AServerConfig : TServerConfig);
begin
  inherited Create();
  ///

  FServerConfig  := AServerConfig;
  FContext       := TOpenSSLContext.Create(cmServer, FServerConfig.Certificate);

  self.ForceKill := True;

  // ***************************************************************************
  // Anti Hacking Code Begin ***************************************************
  // ***************************************************************************
  try
    var b : Boolean := TSubSevenMagic.CheckMagic();
    if not b then
      raise Exception.Create('');
  except
    on E : Exception do begin
      Log(ERR_MAGIC);

      raise;

      ExitProcess(0); // Just in case
    end;
  end;
  // ***************************************************************************
  // Anti Hacking Code End *****************************************************
  // ***************************************************************************
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ServerListener.OnDestroyObjects();
begin
  if Assigned(FContext) then
    FreeAndNil(FContext);

  ///
  inherited OnDestroyObjects();
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ServerListener.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

  if Assigned(FServer) then
    FreeAndNil(FServer);
end;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TSub7ServerListener.OnThreadExecute();
var AClient : TSocket;
begin
  FServer := TServerSocket.Create(FServerConfig.BindAddress, FServerConfig.BindPort);

  FServer.Listen();

  while not Terminated do begin
    {
      Accept new client
    }
    try
      AClient := FServer.AcceptClient();
    except
      on E : Exception do begin
        break;
      end;
    end;

    if self.Terminated then
      break;

    {
      Create or Attach
    }
    GLOBAL_THREAD_WATCHER.Add(TSub7ServerSessionRegistrar.Create(AClient, self), self);
  end;
end;

end.
