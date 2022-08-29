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

unit Sub7.Thread.Net.Client.Base;

interface

uses System.Classes, Sub7.OpenSSL.TLS.Socket, Sub7.TLS.IOHandler, XSuperObject, Sub7.Core.Protocol,
     System.SyncObjs, Sub7.Core.Thread, Winapi.Winsock2, Sub7.Net.Client.Context,
     System.SysUtils;

type
  TSub7ClientBase = class(TCoreThread)
  private
    FSocketFd : TSocket;

    {@M}
    procedure CreateNewSession();
    procedure AttachToSession();
    procedure PasswordAuthentication(const AData : ISuperObject);
  protected
    FContext   : TSub7ClientContext;
    FIOHandler : TSub7TLSIOHandler;

    {@M}
    procedure OnThreadExecute(); override;
    procedure OnDestroyObjects(); override;
    procedure OnClientExecute(); virtual; abstract;
    procedure OnConnected(const ASocket : TSocket); virtual;
    procedure OnConnect(); virtual;
    procedure OnDisconnected(); virtual;
    procedure OnInitiateTLSConnection(); virtual;
    procedure OnTLSConnected(); virtual;
    procedure ExceptionHandler(const E : Exception); override;
    procedure TerminatedSet(); override;

    procedure OnSessionCreated(const AMachineName : String); virtual; abstract;
    procedure OnPasswordRequest(var APassword : String; var ACanceled : Boolean); virtual; abstract;
    procedure OnVerifyPeer(const AFingerprint : String; var ASuccess : Boolean); virtual; abstract;
  public
    {@C}
    constructor Create(const AContext : TSub7ClientContext); overload; virtual;

    {@M}
    function GetCurrentSocket() : TSocket;
  end;

implementation

uses Winapi.Windows, Sub7.OpenSSL.TLS.Exceptions, Sub7.OpenSSL.TLS.IOHandler,
     uFormExceptions, Sub7.Core.Bundle, Sub7.Core.Exceptions, Sub7.Core.Types,
     uFormSockets, Sub7.Viewer.Types, Sub7.Core.Diagnostic, System.Hash,
     Sub7.Core.Windows.PE.Version;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientBase.ExceptionHandler(const E : Exception);
begin
  if (E is ETLSSocketException)        or
     (E is EOpenSSLBaseException)      or
     (E is ES7AuthenticationException) or
     (E is ES7VersionException)
  then
    Exit();
  ///

  SafeSynchronize(procedure begin
    FormExceptions.AddItem(E);
  end);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientBase.AttachToSession();
var AData    : ISuperObject;
    ACommand : TS7Command;
    AKind    : TWorkerKind;
begin
  AData := TSuperObject.Create();

  {
    Provide Registered Session
  }
  AData.S['session_id'] := FContext.SessionId;

  {
    Detect Worker Kind by it Class Type
  }
  AKind := WorkerClassToKind(self);

  if AKind = wkUnknown then
    raise ES7ProtocolException.Create('Could not resolve worker kind.');

  AData.I['kind'] := Integer(AKind);

  {
    Tell server to attach our new worker to existing session
  }
  FIOHandler.SendCommand(s7cAttachToSession, AData);


  {
    Wait for acknoledgement
  }
  FIOHandler.GetCommand(ACommand);

  if ACommand <> s7cSessionAttached then
    raise ES7ProtocolException.Create(ERR_SESSION);

  /// OK
end;

{-------------------------------------------------------------------------------
  Server request client authentication.

  Notice "OnRequestPassword" callback must be set, otherwise it will raise an
  exception that will close client.
-------------------------------------------------------------------------------}
procedure TSub7ClientBase.PasswordAuthentication(const AData : ISuperObject);
var APassword  : String;
    AChallenge : String;
    ADataOut   : ISuperObject;
    ACanceled  : Boolean;
begin
  if not Assigned(AData) then
    raise Exception.Create(ERR_MISSING_INSTANCE);
  ///

  // Read challenge
  if not AData.Contains('challenge') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['challenge']));

  // @GUI Request Password
  if FContext.Password = '' then begin
    self.OnPasswordRequest(APassword, ACanceled);
    if ACanceled then
      raise ES7AuthenticationException.Create(aeCanceled);
  end else begin
    APassword := FContext.Password;

    { We set context password to nil in case the pre registered password was wrong
    in this case we will show the regular password prompt for user to enter password
    manually }
    FContext.Password := '';
  end;

  // Resolve Server Challenge
  AChallenge := System.Hash.THashSHA2.GetHashString(
                                                      Format('%s.%s', [
                                                                        AData.S['challenge'],
                                                                        APassword
                                                      ]),

                                                      SHA512
  );

  ADataOut := TSuperObject.Create();

  ADataOut.S['challenge'] := AChallenge;

  // Send back challenge answer
  FIOHandler.SendCommand(s7cPasswordAuthentication, ADataOut);
end;

{ TSub7ClientBase.CreateNewSession }

procedure TSub7ClientBase.CreateNewSession();
var ACommand  : TS7Command;
    AData     : ISuperObject;
    AMessage  : String;
    AVersion1 : TVersion;
    AVersion2 : TVersion;
begin
  AData := TSuperObject.Create();

  // Version must be identical between Viewer and Server.
  AVersion1 := TVersion.Create(GetModuleName(0));
  try
    AData.O['version'] := AVersion1.Serialize();

    FIOHandler.SendCommand(s7cCreateSession, AData);
    ///

    // Authentication & Session Registration Process & Registration Error Handling
    while not Terminated do begin
      FIOHandler.GetCommand(ACommand, AData);

      case ACommand of
        // Server version is not compatible with this viewer
        s7cVersionMismatch : begin
          if not Assigned(AData) then
            raise ES7ProtocolException.Create(ERR_MISSING_DATA_NODE);

          if not AData.Contains('version') then
            raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['version']));

          AVersion2 := TVersion.Create(AData.O['version']);
          try
            if AVersion1.CompareTo(AVersion2) = -1 then
              AMessage := Format(
                'Server version is higher than viewer. Upgrade SubSeven Viewer to version "%s" and try again.',
                  [
                    AVersion2.ToString
                  ]
                )
            else
              AMessage := Format(
                'Server version is lower than viewer. Downgrade SubSeven Viewer to version "%s" or update server to version "%s".',
                  [
                    AVersion2.ToString,
                    AVersion1.ToString
                  ]
                );
          finally
            if Assigned(AVersion2) then
              FreeAndNil(AVersion2);
          end;

          ///
          raise ES7VersionException.Create(AMessage);
        end;

        // Server request authentication (password-auth)
        s7cPasswordAuthentication : begin
          if not Assigned(AData) then
            raise ES7ProtocolException.Create(ERR_MISSING_DATA_NODE);
          ///

          self.PasswordAuthentication(AData);
        end;

        // Server could not validate our certificate fingerprint
        s7cPeerKeyVerificationFailed : begin
          FIOHandler.SendAck();

          ///
          raise ES7AuthenticationException.Create(aePeerCertificate, 'Remote server could not validate current certificate fingerprint.');
        end;

        // Server doesn't support concurrency
        s7cNoConcurrent : begin
          FIOHandler.SendAck();
          ///

          raise ES7ProtocolException.Create('A viewer has already established a session with remote server. Server session concurrency is disabled.');
        end;

        // Create and register new subseven session
        s7cCreateSession : begin
          if not Assigned(AData) then
            raise ES7ProtocolException.Create(ERR_MISSING_DATA_NODE);
          ///

          if not AData.Contains('session_id') then
            raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['session_id']));

          if not AData.Contains('machine_name') then
            raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['machine_name']));

          FContext.SessionId := AData.S['session_id'];

          self.OnSessionCreated(AData.S['machine_name']);

          ///
          break;
        end;

        else
          raise ES7ProtocolException.Create(ERR_UNEXPECTED_CMD);
      end;
    end;
  finally
    if Assigned(AVersion1) then
      FreeAndNil(AVersion1);
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSub7ClientBase.Create(const AContext : TSub7ClientContext);
begin
  inherited Create();
  ///

  FContext   := AContext;
  FIOHandler := nil;
  FSocketFd  := INVALID_SOCKET;
end;

{-------------------------------------------------------------------------------
  ___free
-------------------------------------------------------------------------------}
procedure TSub7ClientBase.OnDestroyObjects();
begin
  if Assigned(FIOHandler) then
    FreeAndNil(FIOHandler);

  ///
  inherited OnDestroyObjects();
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientBase.OnConnected(const ASocket : TSocket);
begin
  ///
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientBase.OnConnect();
begin
  ///
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientBase.OnInitiateTLSConnection();
begin
  ///
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientBase.OnTLSConnected();
begin
  SafeSynchronize(procedure begin
    FormSockets.AddClient(self);
  end);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TSub7ClientBase.OnDisconnected();
begin
  SafeSynchronize(procedure begin
    FormSockets.RemoveClient(self.ThreadID);
  end);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function TSub7ClientBase.GetCurrentSocket() : TSocket;
begin
  result := INVALID_SOCKET;
  ///

  if Assigned(FIOHandler) then
    result := FIOHandler.SocketFd;
end;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TSub7ClientBase.OnThreadExecute();
var AClient        : TClientSocket;
    ASocketFd      : TSocket;
    APeerValidated : Boolean;
begin
  {
    Create new Socket
  }
  AClient := TClientSocket.Create(FContext.RemoteAddress, FContext.RemotePort);
  try
    self.OnConnect();
    ///

    {
      Connect to remote server
    }
    ASocketFd := AClient.Connect();
  finally
    if Assigned(AClient) then
      FreeAndNil(AClient);
  end;

  FSocketFd := ASocketFd;
  try
    self.OnConnected(ASocketFd);

    self.OnInitiateTLSConnection();

    {
      Create TLS Handler (Instanciate TLS Connection with Peer)
    }
    FIOHandler := TSub7TLSIOHandler.Create(FContext, ASocketFd);
    try
      FIOHandler.Connect();

      { Check if we already know remote peer certificate fingerprint, if no we
        show the fingerprint to user in order to validate }
      if FContext.SessionId = '' then begin
        APeerValidated := False;

        self.OnVerifyPeer(FIOHandler.PeerFingerprint, APeerValidated);

        if not APeerValidated then
          raise EOpenSSLBaseException.Create('Peer certificate validation failed.');
      end;

      self.OnTLSConnected();
      ///

      if FContext.SessionId = '' then begin
        {
          Create new SubSeven session
        }
        self.CreateNewSession();
      end else begin
        {
          Attach client to an existing SubSeven session
        }
        self.AttachToSession();
      end;

      {
        Client can execute normally
      }
      try
        self.OnClientExecute();
      except
        on E : Exception do
          self.ExceptionHandler(E);
      end;
    finally
      if Assigned(FIOHandler) then
        FreeAndNil(FIOHandler);
    end;
  finally
    self.OnDisconnected();
  end;
end;

{ TSub7ClientBase.OnTerminatedSet

  It is important to free the socket from this method }
procedure TSub7ClientBase.TerminatedSet();
begin
  inherited;
  ///

  if FSocketFd <> INVALID_SOCKET then begin
    Winapi.Winsock2.closesocket(FSocketFd);

    FSocketFd := INVALID_SOCKET;
  end;
end;

end.

