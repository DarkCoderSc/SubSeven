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

unit Sub7.OpenSSL.TLS.IOHandler;

interface

uses Winapi.Windows, Winapi.Winsock2, System.Classes, Sub7.OpenSSL.Headers,
     Sub7.OpenSSL.TLS.Context, System.ZLib;

type
  TTransferDirection = (
    tdIn,
    tdOut
  );

  TOpenSSL_TLSIOHandler = class
  private
    const
      PACKET_SIZE = 4096;

    var
      FSSL      : Pointer;
      FSocketFd : TSocket;
      FContext  : TOpenSSLContext;

    {@M}
    procedure TransmitData(const buf: Pointer; num: Integer; const ADirection : TTransferDirection);
    procedure SetBlocking(const ABlocking : Boolean);
    function GetPeerCertificateFingerprint() : String;
  public
    {@C}
    constructor Create(AContext : TOpenSSLContext; const ASocketFd : TSocket);
    destructor Destroy(); override;

    {@M}
    procedure Connect();

    procedure Send(const buf: Pointer; num: Integer); overload;
    procedure Send(const AInteger : Int64); overload;

    procedure Recv(buf: Pointer; num: Integer); overload;
    procedure Recv(var AInteger : Int64); overload;

    procedure SendBuffer(pBuffer : PVOID; ABufferSize : Int64);
    procedure ReceiveBuffer(var AStream : TMemoryStream);

    procedure SendString(const AString : String);
    procedure ReceiveString(var AString : String);

    function DataAvailable() : Boolean;
    procedure CheckConnected();

    {@G}
    property SSL      : Pointer read FSSL       default nil;
    property SocketFd : TSocket read FSocketFd  default INVALID_SOCKET;

    property PeerFingerprint : String read GetPeerCertificateFingerprint;
  end;

implementation

uses System.SysUtils, Sub7.OpenSSL.TLS.Exceptions, Sub7.Core.Diagnostic, Sub7.OpenSSL.TLS.Utils;

{ TOpenSSL_TLSIOHandler.GetPeerCertificateFingerprint()

  Retrieve peer certificate fingerprint for authentication purpose }

function TOpenSSL_TLSIOHandler.GetPeerCertificateFingerprint() : String;
begin
  if not Assigned(FSSL) then
    raise Exception.Create('Missing object instance.');

  result := Peer_SHA512FingerPrint(FSSL);
end;

{ TOpenSSL_TLSIOHandler.Connect

  Instanciate a TLS connection between peers }

procedure TOpenSSL_TLSIOHandler.Connect();
var ARet : Integer;
begin
  CheckOpenSSL();

  if not Assigned(FContext) then
    raise Exception.Create(''); // TODO

  if not Assigned(FContext.Context) then
    raise Exception.Create(''); // TODO

  FSocketFd := FSocketFd;
  FSSL      := nil;

  {
    Get new SSL State with SSL Context
  }
  FSSL := SSL_new(FContext.Context);
  if not Assigned(FSSL) then
    raise Exception.Create(''); // TODO

  {
    Assign Client Socket to SSL State
  }
  if SSL_set_fd(FSSL, FSocketFd) <> 1 then
    raise Exception.Create(''); // TODO

  {
    Check SSL Protocol TODO in separate object
  }
  case FContext.Mode of
    cmClient : ARet := SSL_connect(FSSL);
    cmServer : ARet := SSL_accept(FSSL);
  end;

  if ARet <> 1 then
    raise EOpenSSLBaseException.Create(FSSL);
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TOpenSSL_TLSIOHandler.Create(AContext : TOpenSSLContext; const ASocketFd : TSocket);
begin
  inherited Create();
  ///

  FContext  := AContext;
  FSocketFd := ASocketFd;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TOpenSSL_TLSIOHandler.Destroy();
begin
  {
    Close SSL Connection
  }
  if Assigned(FSSL) then
    SSL_free(FSSL);

  {
    Close Socket Connection and Invalidate Socket
  }
  if FSocketFd <> INVALID_SOCKET then begin
    Winapi.Winsock2.closesocket(FSocketFd);

    FSocketFd := INVALID_SOCKET;
  end;

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Check if some data are available for reading on socket file descriptor
-------------------------------------------------------------------------------}
function TOpenSSL_TLSIOHandler.DataAvailable() : Boolean;
var ABytesAvailable : Cardinal;
    ADummy          : Byte;
begin
  result := False;
  ///

  self.CheckConnected();

  CheckOpenSSL();

  self.SetBlocking(False);
  try
    result := SSL_peek(FSSL, @ADummy, SizeOf(Byte)) = SizeOf(Byte);
  finally
    self.SetBlocking(True);
  end;
end;

{-------------------------------------------------------------------------------
  Set Socket in Blocking / Non-Blocking Mode (Use with caution)
-------------------------------------------------------------------------------}
procedure TOpenSSL_TLSIOHandler.SetBlocking(const ABlocking : Boolean);
var AMode : U_LONG;
begin
  if ABlocking then
    AMode := 0
  else
    AMode := 1;
  ///

  if ioctlsocket(FSocketFd, FIONBIO, AMode) = SOCKET_ERROR then
    raise ETLSSocketException.Create();
end;

{-------------------------------------------------------------------------------
  Check if we are connected with socket.
-------------------------------------------------------------------------------}
procedure TOpenSSL_TLSIOHandler.CheckConnected();
var bt   : Byte;
    ARet : Integer;
begin
  self.SetBlocking(False);
  try
    ARet := WinAPI.Winsock2.recv(FSocketFd, bt, SizeOf(Byte), MSG_PEEK);
    if ARet <= 0 then begin
      if (WSAGetLastError <> WSAEWOULDBLOCK) then
        raise ETLSSocketException.Create();
    end;
  finally
    self.SetBlocking(True);
  end;
end;

{-------------------------------------------------------------------------------
  Transfer Data In / Out
-------------------------------------------------------------------------------}
procedure TOpenSSL_TLSIOHandler.TransmitData(const buf: Pointer; num: Integer; const ADirection : TTransferDirection);
var ARet      : Integer;
    ASSLError : Integer;
begin
  CheckOpenSSL();
  ///

  ARet := 0;

  if not Assigned(FSSL) then
    raise Exception.Create(''); // TODO

  while True do begin
    case ADirection of
      tdOut : ARet := SSL_Write(FSSL, buf, num);
      tdIn  : ARet := SSL_Read(FSSL, buf, num);
    end;

    if ARet > 0 then
      break;

    ASSLError := SSL_get_error(FSSL, ARet);

    case ASSLError of
      SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE :
        continue;

      else
        raise EOpenSSLBaseException.Create(FSSL);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  OpenSSL Send data securely to peer
-------------------------------------------------------------------------------}
procedure TOpenSSL_TLSIOHandler.Send(const buf: Pointer; num: Integer);
begin
  self.TransmitData(buf, num, tdOut);
end;

{-------------------------------------------------------------------------------
  OpenSSL Receive data securely from peer
-------------------------------------------------------------------------------}
procedure TOpenSSL_TLSIOHandler.Recv(buf: Pointer; num: Integer);
begin
  self.TransmitData(buf, num, tdIn);
end;


{-------------------------------------------------------------------------------
  Send/Receive Integer 64 to/from peer
-------------------------------------------------------------------------------}

procedure TOpenSSL_TLSIOHandler.Send(const AInteger : Int64);
begin
  self.Send(@AInteger, SizeOf(Int64));
end;

procedure TOpenSSL_TLSIOHandler.Recv(var AInteger : Int64);
begin
  self.Recv(@AInteger, SizeOf(Int64));
end;

{-------------------------------------------------------------------------------
  Send/Receive String to/from peer
-------------------------------------------------------------------------------}

procedure TOpenSSL_TLSIOHandler.SendString(const AString : String);
var AStream : TMemoryStream;
    zStream : TMemoryStream;
begin
  AStream := TStringStream.Create();
  zStream := TMemoryStream.Create();
  try
    AStream.Write(AString[1], Length(AString) * SizeOf(WideChar));

    AStream.Position := 0;

    ZCompressStream(AStream, zStream);

    self.SendBuffer(zStream.Memory, zStream.Size);
  finally
    FreeAndNil(zStream);
    FreeAndNil(AStream);
  end;
end;

procedure TOpenSSL_TLSIOHandler.ReceiveString(var AString : String);
var AStream : TMemoryStream;
    zStream : TMemoryStream;
begin
  AString := '';
  ///

  zStream := TMemoryStream.Create();
  AStream := TStringStream.Create();
  try
    self.ReceiveBuffer(zStream);
    ///

    zStream.Position := 0;

    ZDecompressStream(zStream, AStream);

    SetString(AString, PWideChar(AStream.Memory), AStream.Size div SizeOf(WideChar));
  finally
    FreeAndNil(AStream);
    FreeAndNil(zStream);
  end;
end;

{-------------------------------------------------------------------------------
  Send Data to peer
-------------------------------------------------------------------------------}
procedure TOpenSSL_TLSIOHandler.SendBuffer(pBuffer : PVOID; ABufferSize : Int64);
var ABytesWritten : Int64;
    AChunkSize    : Integer;
    ACompleted    : Boolean;
    pOffset       : PByte;
begin
  self.Send(ABufferSize);
  ///

  ABytesWritten := 0;
  repeat
    AChunkSize := (ABufferSize - ABytesWritten);

    if AChunkSize > PACKET_SIZE then
      AChunkSize := PACKET_SIZE;

    pOffset := PByte(NativeUInt(pBuffer) + ABytesWritten);

    self.Send(pOffset, AChunkSize);

    Inc(ABytesWritten, AChunkSize);

    ACompleted := (ABytesWritten >= ABufferSize);
  until ACompleted;
end;

{-------------------------------------------------------------------------------
  Receive Data from peer
-------------------------------------------------------------------------------}
procedure TOpenSSL_TLSIOHandler.ReceiveBuffer(var AStream : TMemoryStream);
var ABufferSize : Int64;
    ABytesRead  : Int64;
    ACompleted  : Boolean;
    AChunkSize  : Integer;
begin
  if not Assigned(AStream) then
    raise Exception.Create('');
  ///

  self.Recv(ABufferSize);

  if (ABufferSize <= 0) then
    raise Exception.Create(''); // TODO

  AStream.SetSize(ABufferSize);

  AStream.Position := 0;

  ABytesRead := 0;
  repeat
    AChunkSize := (ABufferSize - ABytesRead);

    if AChunkSize >= PACKET_SIZE then
      AChunkSize := PACKET_SIZE;

    self.Recv(PByte(NativeUInt(AStream.Memory) + ABytesRead), AChunkSize);

    Inc(ABytesRead, AChunkSize);

    ACompleted := (ABytesRead >= ABufferSize);
  until ACompleted;
end;

end.
