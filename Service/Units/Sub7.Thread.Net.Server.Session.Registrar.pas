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

unit Sub7.Thread.Net.Server.Session.Registrar;

interface

uses System.Classes, Sub7.Core.Thread, Winapi.Winsock2,
     Winapi.Windows, Sub7.Thread.Net.Server.Peer.Session.Cmd, Sub7.TLS.IOHandler,
     Sub7.Thread.Net.Server.Listener;

type
  TSub7ServerSessionRegistrar = class(TCoreThread)
  private
    FSocketFd : TSocket;
    FListener : TSub7ServerListener;
    FSuccess  : Boolean;

    {@M}
    function Authenticate(const AIOHandler : TSub7TLSIOHandler) : Boolean;
  protected
    {@M}
    procedure OnThreadExecute(); override;
    procedure TerminatedSet(); override;
  public
    {@C}
    constructor Create(const ASocketFd : TSocket; const AListener : TSub7ServerListener); overload;
  end;

implementation

uses Sub7.Core.Protocol, XSuperObject, Sub7.Core.Types, Sub7.Core.Exceptions, Sub7.Core.Bundle,
     Sub7.Core.Utils, System.SysUtils, Sub7.Core.Diagnostic, Sub7.Core.Thread.Watcher,
     System.Hash, Sub7.Core.Input.Validators, Sub7.Service.Config, Sub7.Core.Windows.PE.Version;

{-------------------------------------------------------------------------------
  Authenticate Client to Server
-------------------------------------------------------------------------------}
function TSub7ServerSessionRegistrar.Authenticate(const AIOHandler : TSub7TLSIOHandler) : Boolean;
var ASalt      : String;
    ADataOut   : ISuperObject;
    ADataIn    : ISuperObject;
    AChallenge : String;
    ACommand   : TS7Command;
begin
  { This variable is used to ensure that at least one path was used to validate
    that client have access granted to this system }
  result := False;
  ///

  if not Assigned(AIOHandler) then
    raise Exception.Create(ERR_MISSING_INSTANCE);
  ///

  { Public Key Authentication }
  if ((FListener.ServerConfig.AuthenticationMode = amPubKey) or
     (FListener.ServerConfig.AuthenticationMode = amBoth)) and
     (IsValidSha512Fingerprint(AIOHandler.PeerFingerprint))
  then begin
    if not FListener.ServerConfig.FingerprintExists(AIOHandler.PeerFingerprint) then begin
      AIOHandler.SendCommand(s7cPeerKeyVerificationFailed);

      AIOHandler.WaitForAck();

      ///
      raise ES7AuthenticationException.Create(aePeerCertificate, Format('"%s" peer certificate was not recognized.', [AIOHandler.PeerFingerprint]));
    end;

    ///
    result := True;
  end;

  { Password Authentication }
  if ((FListener.ServerConfig.AuthenticationMode = amPassword) or
     (FListener.ServerConfig.AuthenticationMode = amBoth)) and
     (IsValidSha512(FListener.ServerConfig.Password))
  then begin
    ASalt := RandomString(32); // 256 Bit Random Salt
    ///

    ADataOut := TSuperObject.Create();

    ADataOut.S['challenge'] := ASalt;

    AChallenge := System.Hash.THashSHA2.GetHashString(
                                                        Format('%s.%s', [
                                                                          ASalt,
                                                                          FListener.ServerConfig.Password
                                                        ]),

                                                        SHA512
    );

    while not Terminated do begin
      // Ask for password authentication
      AIOHandler.SendCommand(s7cPasswordAuthentication, ADataOut);

      // Retreive Client Challenge Response
      AIOHandler.GetCommand(ACommand, ADataIn);

      if ACommand <> s7cPasswordAuthentication then
        raise ES7ProtocolException.Create(ERR_UNEXPECTED_CMD);
      ///

      if not ADataIn.Contains('challenge') then
        raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['challenge']));

      if String.Compare(ADataIn.S['challenge'], AChallenge) = 0 then begin
        // Authentication Successfull
        result := True;

        break;
      end;

      // Authentication Failed

      // Re-Send s7cAuthentify Request

      {TODO -oJPL -cFeature : Implement peer lockout after a certain amount of failed attempt}
    end;
  end;
end;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TSub7ServerSessionRegistrar.OnThreadExecute();
var ACommand        : TS7Command;
    AData           : ISuperObject;
    ASessionUID     : String;
    AIOHandler      : TSub7TLSIOHandler;
    ASession        : TSub7ServerPeerSessionCmd;
    AWorkerKind     : TWorkerKind;
    AViewerVersion  : TVersion;
    AServiceVersion : TVersion;
begin
  try
    // Establish a new SSL/TLS connection with peer
    AIOHandler := TSub7TLSIOHandler.Create(FListener.Context, FSocketFd);
    AIOHandler.Connect();

    // Create a new session or assign client to an existing session
    AIOHandler.GetCommand(ACommand, AData);
    ///

    case ACommand of
      // Register a new Sub7 Session
      s7cCreateSession : begin
        // Check if session concurrency is enabled
        if (GLOBAL_THREAD_WATCHER.Count(TSub7ServerPeerSessionCmd) > 0) and (not FListener.ServerConfig.Concurrency) then begin
          AIOHandler.SendCommand(s7cNoConcurrent);

          AIOHandler.WaitForAck();

          raise ES7ProtocolException.Create(
            'A peer attempt to open a new session while another session was already active. ' +
            'Concurrency is currently disabled so session creation was refused.'
          );
        end;

        if not AData.Contains('version') then
          raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['version']));
        ///


        AViewerVersion  := nil;
        AServiceVersion := nil;
        try
          AViewerVersion := TVersion.Create(AData.O['version']);
          AServiceVersion := TVersion.Create(GetModuleName(0));
          ///

          // Check if viewer and server version match.
          if AServiceVersion.CompareTo(AViewerVersion) <> 0 then begin
            AData.O['version'] := AServiceVersion.Serialize();

            AIOHandler.SendCommand(s7cVersionMismatch, AData);

            // Prevent session to be established
            raise ES7VersionException.Create(Format(ERR_VERSION, [AServiceVersion.ToString, AViewerVersion.ToString]));
          end;
        finally
          if Assigned(AViewerVersion) then
            FreeAndNil(AViewerVersion);

          if Assigned(AServiceVersion) then
            FreeAndNil(AServiceVersion);
        end;

        // Authentify Peer with Server (pubkey and/or password)
        if not self.Authenticate(AIOHandler) then
          raise ES7AuthenticationException.Create(aeError, 'Could not securely authenticate remote peer. Connection aborted.')
        else
          // Create and assign new session to our thread pool
          GLOBAL_THREAD_WATCHER.Add(TSub7ServerPeerSessionCmd.Create(AIOHandler), FListener);
      end;

      // Attach client ao an existing Sub7 Session
      s7cAttachToSession : begin
        if not AData.Contains('session_id') then
          raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['session_id']));

        if not AData.Contains('kind') then
          raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['kind']));
        ///

        ASessionUID := AData.S['session_id'];
        AWorkerKind := TWorkerKind(AData.I['kind']);

        // Get session thread using its unique id as session id
        ASession := TSub7ServerPeerSessionCmd(GLOBAL_THREAD_WATCHER.Get(ASessionUID));

        if not Assigned(ASession) then
          raise ES7ProtocolException.Create(ERR_INVALID_SESSION);

        // Tell Client that provided session was correct
        AIOHandler.SendCommand(s7cSessionAttached);

        // Register new worker on session
        ASession.AddWorker(AWorkerKind, AIOHandler);
      end;

      else
        raise ES7ProtocolException.Create(ERR_UNEXPECTED_CMD);
    end;

    ///
    FSuccess := True;
  except
    on E : Exception do begin
      CrashLog(E, self);
      ///

      // Destroy TLS/IOHandler
      if Assigned(AIOHandler) then
        FreeAndNil(AIOHandler);

      // Close possible residual socket file descriptor
      if FSocketFd <> INVALID_SOCKET then
        closesocket(FSocketFd);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSub7ServerSessionRegistrar.Create(const ASocketFd : TSocket; const AListener : TSub7ServerListener);
begin
  inherited Create();
  ///

  FSocketFd := ASocketFd;
  FListener := AListener;
  FSuccess  := False;
end;

{-------------------------------------------------------------------------------
  Close connection (in case of timeout)
-------------------------------------------------------------------------------}
procedure TSub7ServerSessionRegistrar.TerminatedSet();
begin
  inherited TerminatedSet();
  ///

  if not FSuccess then
    Winapi.Winsock2.closesocket(FSocketFd);
end;

end.
