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

unit S7SocketBuilder;

interface

uses WinAPI.Winsock2;

type
  TNetworkProtocol = (npTCP, npUDP);

  TBuilder = class
  private

  public

  end;

  TClientBuilder = class
  private
    FProtocol      : TNetworkProtocol;
    FRemoteAddress : String;
    FRemotePort    : Word;
  public
    {@C}
    constructor Create(ARemoteAddress : String; ARemotePort : Word; AProtocol : TNetworkProtocol = npTCP);

    {@M}
    function Build(var ASockAddrIn : TSockAddrIn) : TSocket;
  end;

  TServerBuilder = class
  private
    FProtocol     : TNetworkProtocol;
    FLocalAddress : String;
    FLocalPort    : Word;
  public
    {@C}
    constructor Create(ALocalAddress : String; ALocalPort : Word; AProtocol : TNetworkProtocol = npTCP);

    {@M}
    function Build() : TSocket;
  end;

implementation

uses WinAPI.Windows, S7Exceptions, System.SysUtils, UntDebug;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TClientBuilder


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TClientBuilder.Create(ARemoteAddress : String; ARemotePort : Word; AProtocol : TNetworkProtocol = npTCP);
begin
  FRemoteAddress := ARemoteAddress;
  FRemotePort    := ARemotePort;
  FProtocol      := AProtocol;
end;

{-------------------------------------------------------------------------------
  ___build
-------------------------------------------------------------------------------}
function TClientBuilder.Build(var ASockAddrIn : TSockAddrIn) : TSocket;
var ASocket  : TSocket;
    AHostEnt : PHostEnt;
begin
  result := INVALID_SOCKET;
  ///

  ASocket := WinAPI.Winsock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if (ASocket = INVALID_SOCKET) then
    raise ES7SocketException.Create();

  ZeroMemory(@ASockAddrIn, SizeOf(TSockAddrIn));

  ASockAddrIn.sin_port   := WinAPI.Winsock2.htons(FRemotePort);
  ASockAddrIn.sin_family := AF_INET;
  ASockAddrIn.sin_addr.S_addr := WinAPI.Winsock2.inet_addr(PAnsiChar(AnsiString(FRemoteAddress)));

  if ASockAddrIn.sin_addr.S_addr = INADDR_NONE then begin
    AHostEnt := GetHostByName(PAnsiChar(AnsiString(FRemoteAddress)));
    if AHostEnt <> nil then
      ASockAddrIn.sin_addr.S_addr := Integer(Pointer(AHostEnt^.h_addr^)^);
  end;

  ///
  result := ASocket;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TServerBuilder


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TServerBuilder.Create(ALocalAddress : String; ALocalPort : Word; AProtocol : TNetworkProtocol = npTCP);
begin
  FLocalAddress := ALocalAddress;
  FLocalPort    := ALocalPort;
  FProtocol     := AProtocol;
end;

{-------------------------------------------------------------------------------
  ___build
-------------------------------------------------------------------------------}
function TServerBuilder.Build() : TSocket;
var ASocket      : TSocket;
    ASockAddrIn  : TSockAddrIn;
begin
  result  := INVALID_SOCKET;
  ASocket := INVALID_SOCKET;
  ///

  try
    ASocket := WinAPI.Winsock2.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (ASocket = INVALID_SOCKET) then
      raise ES7SocketException.Create();

    ZeroMemory(@ASockAddrIn, SizeOf(TSockAddrIn));

    ASockAddrIn.sin_port   := WinAPI.Winsock2.htons(FLocalPort);
    ASockAddrIn.sin_family := AF_INET;

    if (FLocalAddress = '0.0.0.0') or (FLocalAddress = '') then
      ASockAddrIn.sin_addr.S_addr := INADDR_ANY
    else
      ASockAddrIn.sin_addr.S_addr := WinAPI.Winsock2.inet_addr(PAnsiChar(AnsiString(FLocalAddress)));

    if WinAPI.Winsock2.bind(ASocket, TSockAddr(ASockAddrIn), SizeOf(TSockAddrIn)) = SOCKET_ERROR then
      raise ES7SocketException.Create();

    ///
    result := ASocket;
  except
    on E: Exception do begin
      if (ASocket <> INVALID_SOCKET) then
        WinAPI.Winsock2.closesocket(ASocket);

      raise;
    end;
  end;
end;

var __WSAData : TWSAData;

initialization
  WSAStartup($0202, __WSAData);

finalization
  WSACleanup();

end.
