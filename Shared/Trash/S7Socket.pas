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

unit S7Socket;

interface

uses WinAPI.Windows, WinAPI.Winsock2, System.Classes, XSuperObject, UntRC4,
     S7Protocol, System.SysUtils, S7Exceptions;

type
  TS7Socket = class(TPersistent)
  private
    const PACKET_SIZE = 4096;

    var
      FSocket     : TSocket;
      FPassphrase : String;
      FCipher     : TRC4;

    {@M}
    procedure SetPassphrase(const AValue : String);
  protected
    {@M}
    procedure Assign(ASource: TPersistent); override;
  public
    {@C}
    constructor Create(ASocket : TSocket; APassphrase : String = ''); overload;
    constructor Create(AS7Socket : TS7Socket); overload;

    {@M}
    procedure Send(const buf; len : Integer); overload;
    procedure Recv(var buf; len: Integer); overload;
    procedure Send(AValue : Int64); overload;
    procedure Send(AValue : Int32); overload;
    procedure Recv(var AValue : Int64); overload;
    procedure Recv(var AValue : Int32); overload;
    procedure Send(AString : String); overload;
    procedure Recv(var AString : String); overload;

    procedure SendCommand(ACommand : TS7Command; AJsonData : ISuperObject = nil);
    procedure GetCommand(var ACommand : TS7Command; var AJsonData : ISuperObject); overload;
    procedure GetCommand(var ACommand : TS7Command); overload;

    procedure SendException(const AException : Exception);

    procedure SendBuffer(pBuffer : PVOID; ABufferSize : Int64);
    procedure ReceiveBuffer(var AStream : TMemoryStream);

    function DataAvailable() : Boolean;
    procedure CheckConnected();

    procedure Close();

    {@G/S}
    property socket     : TSocket read FSocket     write FSocket;
    property Passphrase : String  read FPassphrase write SetPassphrase;
  end;

implementation

uses System.Math, UntDebug, UntFunctions, S7Consts;

{-------------------------------------------------------------------------------
  ___assign
-------------------------------------------------------------------------------}
procedure TS7Socket.Assign(ASource: TPersistent);
begin
  if (ASource is TS7Socket) then begin
    FSocket := TS7Socket(ASource).FSocket;

    SetPassphrase(TS7Socket(ASource).FPassphrase);
  end else
    inherited Assign(ASource);
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}

constructor TS7Socket.Create(ASocket : TSocket; APassphrase : String = '');
begin
  FSocket := ASocket;

  if (FSocket <= 0) then begin
    WSASetLastError(WSA_INVALID_HANDLE);

    raise ES7SocketException.Create();
  end;

  FCipher     := nil;
  FPassphrase := '';

  SetPassphrase(APassphrase);
end;

constructor TS7Socket.Create(AS7Socket : TS7Socket);
begin
  if Assigned(AS7Socket) then begin
    self.Assign(AS7Socket);
  end;
end;

{-------------------------------------------------------------------------------
  __winsock2_send()
-------------------------------------------------------------------------------}
procedure TS7Socket.Send(const buf; len : Integer);
begin
  if Assigned(FCipher) then
    FCipher.Encrypt(@buf, len);
  ///

  if WinAPI.Winsock2.send(FSocket, buf, len, 0) <> len then
    raise ES7SocketException.Create();
end;

{-------------------------------------------------------------------------------
  __winsock2_recv()
-------------------------------------------------------------------------------}
procedure TS7Socket.Recv(var buf; len: Integer);
begin
  if WinAPI.Winsock2.recv(FSocket, buf, len, 0) <> len then
    raise ES7SocketException.Create();

  if Assigned(FCipher) then
    FCipher.Decrypt(@buf, len);
end;

{-------------------------------------------------------------------------------
  Send / Recv in different format
-------------------------------------------------------------------------------}

procedure TS7Socket.Send(AValue : Int64);
begin
  self.Send(AValue, SizeOf(Int64));
end;

procedure TS7Socket.Send(AValue : Int32);
begin
  self.Send(AValue, SizeOf(Int32));
end;

procedure TS7Socket.Recv(var AValue : Int64);
begin
  self.Recv(AValue, SizeOf(Int64));
end;

procedure TS7Socket.Recv(var AValue : Int32);
begin
  self.Recv(AValue, SizeOf(Int32));
end;

procedure TS7Socket.Send(AString : String);
var AStringStream : TStringStream;
begin
  if Length(Trim(AString)) = 0 then
    raise ES7InvalidDataException.Create('Cannot send an empty string.');

  AStringStream := TStringStream.Create();
  try
    AStringStream.WriteString(AString);

    AStringStream.Position := 0;

    self.SendBuffer(AStringStream.Memory, AStringStream.Size);
  finally
    FreeAndNil(AStringStream);
  end;
end;

procedure TS7Socket.Recv(var AString : String);
var AStringStream : TStringStream;
begin
  AString := '';

  AStringStream := TStringStream.Create();
  try
    self.ReceiveBuffer(TMemoryStream(AStringStream));

    if AStringStream.Size = 0 then
      raise ES7InvalidDataException.Create('Empty string received.');

    AString := AStringStream.ReadString(AStringStream.Size);
  finally
    FreeAndNil(AStringStream);
  end;
end;

{-------------------------------------------------------------------------------
  Sender Buffer
-------------------------------------------------------------------------------}
procedure TS7Socket.SendBuffer(pBuffer : PVOID; ABufferSize : Int64) ;
var ABytesWritten : Int64;
    AChunkSize    : Integer;
    ACompleted    : Boolean;
    pOffset       : PByte;
begin
  self.Send(ABufferSize);
  ///

  if (ABufferSize <= PACKET_SIZE) then
    self.Send(PByte(pBuffer)^, ABufferSize)
  else begin
    ABytesWritten := 0;
    repeat
      AChunkSize := (ABufferSize - ABytesWritten);

      if AChunkSize > PACKET_SIZE then
        AChunkSize := PACKET_SIZE;

      pOffset := PByte(NativeUInt(pBuffer) + ABytesWritten);

      self.Send(pOffset^, AChunkSize);

      Inc(ABytesWritten, AChunkSize);

      ACompleted := (ABytesWritten >= ABufferSize);
    until ACompleted;

    ///
    if not ACompleted then
      raise ES7IncompleteDataException.Create('Data stream was not completely sent.');
  end;
end;

{-------------------------------------------------------------------------------
  Receive Buffer
-------------------------------------------------------------------------------}
procedure TS7Socket.ReceiveBuffer(var AStream : TMemoryStream);
var ABufferSize : Int64;
    ABytesRead  : Int64;
    ACompleted  : Boolean;
    AChunkSize  : Integer;
begin
  if not Assigned(AStream) then
    raise ES7InvalidDataException.Create('Target stream is not assigned.');

  self.Recv(ABufferSize);

  if (ABufferSize <= 0) then
    raise ES7InvalidDataException.Create('Invalid data length.');

  AStream.SetSize(ABufferSize);

  AStream.Position := 0;

  if (ABufferSize <= PACKET_SIZE) then begin
    {
      Only one packet required
    }
    self.Recv(PByte(AStream.Memory)^, ABufferSize);
  end else begin
    {
      More than 1 Packet is required
    }
    ABytesRead := 0;
    repeat
      AChunkSize := (ABufferSize - ABytesRead);

      if AChunkSize >= PACKET_SIZE then
        AChunkSize := PACKET_SIZE;

      self.Recv(PByte(NativeUInt(AStream.Memory) + ABytesRead)^, AChunkSize);

      Inc(ABytesRead, AChunkSize);

      ACompleted := (ABytesRead >= ABufferSize);
    until ACompleted;

    if not ACompleted then begin
      AStream.Clear();

      raise ES7IncompleteDataException.Create('Data stream was not completely received.');
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Gracefully Close Socket
-------------------------------------------------------------------------------}
procedure TS7Socket.Close();
begin
  WinAPI.Winsock2.shutdown(FSocket, SD_BOTH);

  WinAPI.Winsock2.closesocket(FSocket);

  FSocket := INVALID_SOCKET;
end;

{-------------------------------------------------------------------------------
  Return true if some data is available
-------------------------------------------------------------------------------}
function TS7Socket.DataAvailable() : Boolean;
var ABytesAvailable : Cardinal;
begin
  result := False;
  ///

  self.CheckConnected();

  ABytesAvailable := 0;

  if ioctlsocket(self.Socket, FIONREAD, ABytesAvailable) = SOCKET_ERROR then
    raise ES7SocketException.Create();

  ///
  result := (ABytesAvailable > 0);
end;

{-------------------------------------------------------------------------------
  Little trick to check whether or not our socket is disconnected without blocking
-------------------------------------------------------------------------------}
procedure TS7Socket.CheckConnected();

  procedure SetMode(AMode : u_long);
  begin
    if ioctlsocket(FSocket, FIONBIO, AMode) = SOCKET_ERROR then
      raise ES7SocketException.Create();
  end;

  var bt   : Byte;
      ARet : Integer;
begin
  SetMode(1); // Set Socket in Non-Blocking Mode
  try
    ARet := WinAPI.Winsock2.recv(FSocket, bt, SizeOf(Byte), MSG_PEEK);
    if ARet <= 0 then begin
      if WSAGetLastError <> WSAEWOULDBLOCK then
        raise ES7SocketException.Create();
    end;
  finally
    SetMode(0); // Set Socket in Blocking Mode
  end;
end;

{-------------------------------------------------------------------------------
  Send and receive S7 Commands (JSON)
-------------------------------------------------------------------------------}

procedure TS7Socket.SendCommand(ACommand : TS7Command; AJsonData : ISuperObject = nil);
var AJsonCommand : ISuperObject;
begin
  Randomize();
  ///

  AJsonCommand := TSuperObject.Create();

  {Add entropie to package N°1}
  if Assigned(FCipher) then
    AJsonCommand.S[RandomString(Random(128))] := RandomString(Random(128));

  AJsonCommand.V['command'] := Integer(ACommand);

  if (AJsonData = nil) then
    AJsonCommand.O['data'] := SO()
  else
    AJsonCommand.O['data'] := AJsonData;

  {Add entropie to package N°2}
  if Assigned(FCipher) then
    AJsonCommand.S[RandomString(Random(128))] := RandomString(Random(128));

  ///
  self.Send(AJsonCommand.AsJSON());
end;

procedure TS7Socket.GetCommand(var ACommand : TS7Command; var AJsonData : ISuperObject);
var AJsonCommand : ISuperObject;
    AJsonString  : String;
    AMessage     : String;
begin
  AJsonData := nil;
  ACommand  := s7cUnknown;
  ///

  self.Recv(AJsonString);

  AJsonCommand := SO(AJsonString);

  if not AJsonCommand.Contains('command') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['command']));
  ///

  ACommand  := TS7Command(AJsonCommand.V['command']);

  if AJsonCommand.Contains('data') then
    AJsonData := AJsonCommand.O['data'];

  {
    Handle Action Exception
  }
  if (ACommand = s7cServerException) then begin
    if AJsonData.Contains('message') then
      AMessage := AJsonData.V['message'];

    raise ES7ServerException.Create(AMessage);
  end;
end;

procedure TS7Socket.GetCommand(var ACommand : TS7Command);
var AData : ISuperObject;
begin
  AData := nil;

  self.GetCommand(ACommand, AData);
end;

procedure TS7Socket.SendException(const AException : Exception);
var AJsonData : ISuperObject;
begin
  if not Assigned(AException) then
    Exit();
  ///

  AJsonData := TSuperObject.Create();

  AJsonData.V['message'] := AException.Message;

  ///
  SendCommand(s7cServerException, AJsonData);
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7Socket.SetPassphrase(const AValue : String);
begin
  if FPassphrase = AValue then
    Exit();
  ///

  FPassphrase := AValue;

  if Assigned(FCipher) then
    FreeAndNil(FCipher);

  if (FPassphrase <> '') then
    FCipher := TRC4.Create(FPassphrase);
end;

end.
