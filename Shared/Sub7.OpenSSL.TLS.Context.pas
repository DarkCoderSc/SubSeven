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

unit Sub7.OpenSSL.TLS.Context;

interface

uses System.Classes, Sub7.OpenSSL.Headers, System.SyncObjs;

type
  TContextMode = (cmClient, cmServer);

  TOpenSSLContext = class
  private
    FCertFile : AnsiString;
    FContext  : Pointer;
    FMode     : TContextMode;


    {@M}
    procedure LoadCertificates();
    procedure InitializeContext();
  public
    {@C}
    constructor Create(const AMode : TContextMode; const ACertFile : AnsiString); virtual;
    destructor Destroy(); override;

    {@G}
    property Context : Pointer      read FContext;
    property Mode    : TContextMode read FMode;
  end;

  var GLOBAL_VerifyCallBackLock : TCriticalSection;

implementation

uses System.SysUtils, Winapi.Windows, Sub7.OpenSSL.TLS.Exceptions;

{ OpenSSL_VerifyCallback

  The trick here is to always validate certificate even if its wrong, we don't
  want to rely on CA but validate peer fingerprint ourself. If we don't use this
  callback, we will receive an error during TLS Handshake : "certificate verify failed"}

function OpenSSL_VerifyCallback(AOk: Integer; AContext: Pointer): Integer; cdecl;
begin
  GLOBAL_VerifyCallBackLock.Acquire();
  try
    result := 1; // Always valid
  finally
    GLOBAL_VerifyCallBackLock.Leave();
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TOpenSSLContext.Create(const AMode : TContextMode; const ACertFile : AnsiString);
begin
  CheckOpenSSL();
  ///

  FContext  := nil;
  FCertFile := ACertFile;
  FMode     := AMode;
  ///

  {
    Create a new SSL Context
  }
  self.InitializeContext();

  {
    Load and assign both private and public key to the new context
  }
  self.LoadCertificates();
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TOpenSSLContext.Destroy();
begin
  if Assigned(FContext) and Assigned(SSL_CTX_free) then begin
    SSL_CTX_free(FContext);

    ///
    FContext := nil;
  end;
  ///

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TOpenSSLContext.InitializeContext();
var pSSLMethod : Pointer;
begin
  CheckOpenSSL();
  ///

  case FMode of
    cmClient : pSSLMethod := TLS_client_method;
    cmServer : pSSLMethod := TLS_server_method;
  end;

  FContext := SSL_CTX_new(pSSLMethod);
  if not Assigned(FContext) then
    raise EOpenSSLBaseException.Create();

  {
    Force usage of TLSv1.3 AES 256bit GCM with SHA384 as digest
  }
  if SSL_CTX_set_ciphersuites(FContext, 'TLS_AES_256_GCM_SHA384') <> 1 then
    raise EOpenSSLLibraryException.Create('Could not configure context cipher suites.');

  // Configure context to verify peer certificate
  SSL_CTX_set_verify(FContext, SSL_VERIFY_PEER or SSL_VERIFY_FAIL_IF_NO_PEER_CERT, @OpenSSL_VerifyCallback);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
procedure TOpenSSLContext.LoadCertificates();
begin
  CheckOpenSSL();
  ///

  if not Assigned(FContext) then
    raise Exception.Create('Context is missing to load certificates.');

  if not FileExists(FCertFile) then
    raise Exception.Create(Format('"%s" certificate file doesn''t exists.', [FCertFile]));

  // Load Public Key
  if SSL_CTX_use_certificate_file(FContext, PAnsiChar(FCertFile), SSL_FILETYPE_PEM) <> 1 then
    raise EOpenSSLBaseException.Create();

  // Load Private Key
  if SSL_CTX_use_PrivateKey_file(FContext, PAnsiChar(FCertFile), SSL_FILETYPE_PEM) <> 1 then
    raise EOpenSSLBaseException.Create();

  // Check if key pair match
  if SSL_CTX_check_private_key(FContext) <> 1 then
    raise EOpenSSLBaseException.Create();
end;

initialization
  GLOBAL_VerifyCallBackLock := TCriticalSection.Create();

finalization
  if Assigned(GLOBAL_VerifyCallBackLock) then
    FreeAndNil(GLOBAL_VerifyCallBackLock);

end.
