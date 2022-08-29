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

// Jade <3 28012021

unit Sub7.OpenSSL.TLS.Socket;

interface

uses Winapi.Windows, Winapi.Winsock2, System.Classes;

type
  TSocketBase = class(TPersistent)
  protected
    function CreateSocket() : TSocket;
  end;

  TServerSocket = class(TSocketBase)
  private
    FBindAddress : String;
    FBindPort    : Word;
    FSocketFd   : TSocket;
  public
    {@C}
    constructor Create(const ABindAddress : String; const ABindPort : Word); overload;
    destructor Destroy(); override;

    {@M}
    procedure Listen();
    function AcceptClient() : TSocket;
    procedure Close();

    {@G}
    property SocketFd : TSocket read FSocketFd;
  end;

  TClientSocket = class(TSocketBase)
  private
    FRemoteAddress : String;
    FRemotePort    : word;
  public
    {@C}
    constructor Create(const ARemoteAddress : String; const ARemotePort : word); overload;

    {@M}
    function Connect() : TSocket;
  end;

implementation

uses System.SysUtils, Sub7.OpenSSL.TLS.Exceptions, Sub7.Core.Diagnostic;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TSocketBase


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  Create socket
-------------------------------------------------------------------------------}
function TSocketBase.CreateSocket() : TSocket;
var ASocket : TSocket;
    b       : Longbool;
    dw      : DWORD;
begin
  result := INVALID_SOCKET;
  ///

  { Create Socket }
  ASocket := WinAPI.Winsock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (ASocket = INVALID_SOCKET) then
    raise ETLSSocketException.Create();
  ///

  { Configure Socket }
  b := True;
  if (setsockopt(ASocket, IPPROTO_TCP, TCP_NODELAY, @b, SizeOf(LongBool)) = SOCKET_ERROR) then
    raise ETLSSocketException.Create();

  dw := 2048;

  if (setsockopt(ASocket, SOL_SOCKET, SO_RCVBUF, @dw, SizeOf(DWORD)) = SOCKET_ERROR) then
    raise ETLSSocketException.Create();

  if (setsockopt(ASocket, SOL_SOCKET, SO_SNDBUF, @dw, SizeOf(DWORD)) = SOCKET_ERROR) then
    raise ETLSSocketException.Create();

  { Return Socket }
  result := ASocket;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TClientSocket


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TClientSocket.Create(const ARemoteAddress : String; const ARemotePort : word);
begin
  inherited Create();
  ///

  FRemoteAddress := ARemoteAddress;
  FRemotePort    := ARemotePort;
end;

{-------------------------------------------------------------------------------
  ___connect
-------------------------------------------------------------------------------}
function TClientSocket.Connect() : TSocket;
var AHostEnt    : PHostEnt;
    ASockAddrIn : TSockAddrIn;
    ASocket     : TSocket;
begin
  {
    Create Socket
  }
  ASocket := self.CreateSocket();

  ZeroMemory(@ASockAddrIn, SizeOf(TSockAddrIn));

  ASockAddrIn.sin_port        := WinAPI.Winsock2.htons(FRemotePort);
  ASockAddrIn.sin_family      := AF_INET;
  ASockAddrIn.sin_addr.S_addr := WinAPI.Winsock2.inet_addr(PAnsiChar(AnsiString(FRemoteAddress)));

  {
    Resolve Host if any
  }
  if ASockAddrIn.sin_addr.S_addr = INADDR_NONE then begin
    AHostEnt := GetHostByName(PAnsiChar(AnsiString(FRemoteAddress)));
    if AHostEnt <> nil then
      ASockAddrIn.sin_addr.S_addr := Integer(Pointer(AHostEnt^.h_addr^)^);
  end;

  {
    Attempt to connect to remote server
  }
  if (WinAPI.Winsock2.connect(ASocket, TSockAddr(ASockAddrIn), SizeOf(TSockAddrIn)) = SOCKET_ERROR) then
    raise ETLSSocketException.Create();

  ///
  result := ASocket;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TServerSocket


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TServerSocket.Create(const ABindAddress : String; const ABindPort : Word);
begin
  inherited Create();
  ///

  FBindAddress := ABindAddress;
  FBindPort    := ABindPort;
  FSocketFd    := INVALID_SOCKET;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TServerSocket.Destroy();
begin
  self.Close();

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Gracefully Close Socket and its dependencies
-------------------------------------------------------------------------------}
procedure TServerSocket.Close();
begin
  {
    Close Socket Connection and Invalidate Socket
  }
  if FSocketFd <> INVALID_SOCKET then begin
    Winapi.Winsock2.shutdown(FSocketFd, SD_BOTH);
    Winapi.Winsock2.closesocket(FSocketFd);

    FSocketFd := INVALID_SOCKET;
  end;
end;

{-------------------------------------------------------------------------------
  Create Socket and Listen for connection on Port
-------------------------------------------------------------------------------}
procedure TServerSocket.Listen();
var ASockAddrIn : TSockAddrIn;
begin
  try
    {
      Create Socket
    }
    FSocketFd := self.CreateSocket();

    ZeroMemory(@ASockAddrIn, SizeOf(TSockAddrIn));

    ASockAddrIn.sin_port   := WinAPI.Winsock2.htons(FBindPort);
    ASockAddrIn.sin_family := AF_INET;

    if (FBindAddress = '0.0.0.0') or (FBindAddress = '') then
      ASockAddrIn.sin_addr.S_addr := INADDR_ANY
    else
      ASockAddrIn.sin_addr.S_addr := WinAPI.Winsock2.inet_addr(PAnsiChar(AnsiString(FBindAddress)));

    {
      Bind Socket
    }
    if WinAPI.Winsock2.bind(FSocketFd, TSockAddr(ASockAddrIn), SizeOf(TSockAddrIn)) = SOCKET_ERROR then
      raise ETLSSocketException.Create();

    {
      Listen on Socket
    }
    if WinAPI.WinSock2.listen(FSocketFd, SOMAXCONN) = SOCKET_ERROR then
      raise ETLSSocketException.Create();
  except
    on E: Exception do begin
      if (FSocketFd <> INVALID_SOCKET) then
        WinAPI.Winsock2.closesocket(FSocketFd);

      FSocketFd := INVALID_SOCKET;

      ///
      raise;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Waiting for a new client to connect to server and setup SSL to secure this
  connection.
-------------------------------------------------------------------------------}
function TServerSocket.AcceptClient() : TSocket;
var AClient     : TSocket;
    ASockAddrIn : TSockAddrIn;
    ALen        : Integer;
begin
  result := INVALID_SOCKET;
  ///

  {
    Wait until a new client connects
  }
  ZeroMemory(@ASockAddrIn, SizeOf(TSockAddrIn));

  ALen := SizeOf(TSockAddrIn);

  AClient := WinAPI.WinSock2.accept(FSocketFd, @ASockAddrIn, @ALen);
  if AClient = INVALID_SOCKET then
    raise ETLSSocketException.Create();

  ///
  result := AClient;
end;

{-------------------------------------------------------------------------------

  Initialize Required Libraries
    - Winsock2
    - OpenSSL

-------------------------------------------------------------------------------}
var __WSAData : TWSAData;

initialization
  {
    Load Winsock
  }
  WSAStartup($0202, __WSAData);

finalization
  WSACleanup();

end.
