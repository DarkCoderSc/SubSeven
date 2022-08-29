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

{
  Créer un record qui décrit la communication exemple

  TS7_IPC_Header = record
    DataSize : Int64;
    Success : Boolean; // Ou un magic token pour valider que c'est bon
  end;
}

unit S7InterProcessCommunication;

interface

uses System.Classes, Winapi.Windows;

type
  TCommunicationMode = (
    cmServer,
    cmClient
  );

  TS7InterProcessCommunication = class
  private
    FMode      : TCommunicationMode;
    FGUID      : String;
    FMaxSize   : Int64;
    FMapHandle : THandle;
    FEventIn   : THandle;
    FEventOut  : THandle;
    FMapOffset : Pointer;
  protected
    {@C}
    constructor Create(const AGUID : String; const AMaxSize : Int64; const AMode : TCommunicationMode);
    destructor Destroy(); override;
  public
    {@M}
    function ReadBuffer() : TMemoryStream;
    procedure WriteBuffer(pBuffer : PVOID; const ABufferSize : Int64);
    function GetDataOffset() : Pointer;
    function GetDataSize() : Int64;
    procedure SetDataSize(const ASize : Int64);
  end;

  TS7InterProcessCommunicationServer = class(TS7InterProcessCommunication)
  public
    {@M}
    function WaitForQuery(const ATimeout : Integer = INFINITE) : Integer;
    procedure QueryDone();

    {@C}
    constructor Create(const AGUID : String; const AMaxSize : Int64); overload;
  end;

  TS7InterProcessCommunicationClient = class(TS7InterProcessCommunication)
  private

  protected
    {@M}
    function WaitForResult(const ATimeout : Integer = INFINITE) : Integer;
    procedure Query();

    {@C}
    constructor Create(const AGUID : String; const AMaxSize : Int64); overload;
  end;

implementation

uses S7Exceptions, System.SysUtils, UntFunctions;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


     TS7InterProcessCommunication


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7InterProcessCommunication.Create(const AGUID : String; const AMaxSize : Int64; const AMode : TCommunicationMode);
var AEventInName  : String;
    AEventOutName : String;
    AMapName      : String;
begin
  FMode    := AMode;
  FGUID    := AGUID;
  FMaxSize := AMaxSize;
  ///

  FMapHandle := INVALID_HANDLE_VALUE;
  FMapOffset := nil;
  FEventIn   := 0;
  FEventOut  := 0;
  ///

  NTSetPrivilege('SeCreateGlobalPrivilege', True);


  AMapName      := Format('Global\%s_MAP', [AGUID]);
  AEventInName  := Format('Global\%s_EVTi', [AGUID]);
  AEventOutName := Format('Global\%s_EVTo', [AGUID]);

  case FMode of
    cmServer : begin
      {
        Create File Mapping
      }
      FMapHandle := CreateFileMappingW(
                                        INVALID_HANDLE_VALUE,
                                        nil,
                                        PAGE_READWRITE,
                                        0,
                                        FMaxSize,
                                        PWideChar(AMapName)
      );
      if FMapHandle = 0 then
        raise ES7WindowsException.Create('CreateFileMappingW');

      {
        Create Communication Events
      }
      FEventIn := CreateEventW(nil, True, True, PWideChar(AEventInName));
      if FEventIn = 0 then
        raise ES7WindowsException.Create('(1)CreateEventW');

      FEventOut := CreateEventW(nil, True, True, PWideChar(AEventOutName));
      if FEventOut = 0 then
        raise ES7WindowsException.Create('(2)CreateEventW');
    end;

    cmClient : begin
      {
        Open File Mapping
      }
      FMapHandle := OpenFileMappingW(
                                    FILE_MAP_ALL_ACCESS,
                                    False,
                                    PWideChar(AMapName)
      );

      if FMapHandle = 0 then
        raise ES7WindowsException.Create('OpenFileMappingW');

      {
        Open Communication Events
      }
      FEventIn := OpenEventW(EVENT_ALL_ACCESS, False, PWideChar(AEventInName));
      if FEventIn = 0 then
        raise ES7WindowsException.Create('(1)OpenEventW');

      FEventOut := OpenEventW(EVENT_ALL_ACCESS, False, PWideChar(AEventOutName));
      if FEventOut = 0 then
        raise ES7WindowsException.Create('(2)OpenEventW');
    end;
  end;

  {
    Acquire Memory Address
  }
  FMapOffset := MapViewOfFile(
                                FMapHandle,
                                FILE_MAP_ALL_ACCESS,
                                0,
                                0,
                                FMaxSize
  );
  if not Assigned(FMapOffset) then
    raise ES7WindowsException.Create('MapViewOfFile');
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7InterProcessCommunication.Destroy();
begin
  if Assigned(FMapOffset) then
    UnmapViewOfFile(FMapOffset);

  if FMapHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FMapHandle);

  if FEventIn <> 0 then
    CloseHandle(FEventIn);

  if FEventOut <> 0 then
    CloseHandle(FEventOut);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  read/write buffer to memory
-------------------------------------------------------------------------------}

function TS7InterProcessCommunication.ReadBuffer() : TMemoryStream;
var ABufferSize : Int64;
begin
  result := nil;
  ///

  ABufferSize := self.GetDataSize();

  if ABufferSize <= 0 then
    Exit();
  ///

  result := TMemoryStream.Create();

  result.SetSize(ABufferSize);

  result.Position := 0;

  result.Write(PByte(self.GetDataOffset())^, ABufferSize);

  result.position := 0;
end;

procedure TS7InterProcessCommunication.WriteBuffer(pBuffer : PVOID; const ABufferSize : Int64);
begin
  if not Assigned(pBuffer) then
    Exit();
  ///

  if ABufferSize <= 0 then
    Exit();

  self.SetDataSize(ABufferSize);

  CopyMemory(self.GetDataOffset(), pBuffer, ABufferSize);
end;

{-------------------------------------------------------------------------------
  Retrieve offset of available data (not including data size)
-------------------------------------------------------------------------------}
function TS7InterProcessCommunication.GetDataOffset() : Pointer;
begin
  result := Pointer(NativeUint(FMapOffset) + SizeOf(Int64));
end;

{-------------------------------------------------------------------------------
  Get/ set  size of available data
-------------------------------------------------------------------------------}

function TS7InterProcessCommunication.GetDataSize() : Int64;
begin
  result := PInt64(FMapOffset)^;
end;

procedure TS7InterProcessCommunication.SetDataSize(const ASize : Int64);
begin
  PInt64(FMapOffset)^ := ASize;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


     TS7InterProcessCommunicationServer


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7InterProcessCommunicationServer.Create(const AGUID : String; const AMaxSize : Int64);
begin
  inherited Create(AGUID, AMaxSize, cmServer);
  ///
end;

{-------------------------------------------------------------------------------
  ___wait_for_query
-------------------------------------------------------------------------------}
function TS7InterProcessCommunicationServer.WaitForQuery(const ATimeout : Integer = INFINITE) : Integer;
begin
  ResetEvent(FEventIn);

  result := WaitForSingleObject(FEventIn, ATimeout);
end;

{-------------------------------------------------------------------------------
  ___ready
-------------------------------------------------------------------------------}
procedure TS7InterProcessCommunicationServer.QueryDone();
begin
  SetEvent(FEventOut);
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


     TS7InterProcessCommunicationClient


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7InterProcessCommunicationClient.Create(const AGUID : String; const AMaxSize : Int64);
begin
  inherited Create(AGUID, AMaxSize, cmClient);
  ///


end;

{-------------------------------------------------------------------------------
  ___query
-------------------------------------------------------------------------------}
procedure TS7InterProcessCommunicationClient.Query();
begin
  ResetEvent(FEventOut);
  SetEvent(FEventIn);
end;

{-------------------------------------------------------------------------------
  ___wait_for_result
-------------------------------------------------------------------------------}
function TS7InterProcessCommunicationClient.WaitForResult(const ATimeout : Integer = INFINITE) : Integer;
begin
  result := WaitForSingleObject(FEventOut, ATimeout);
end;

end.
