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

unit Sub7.Core.Windows.Process.Enum;

interface

uses System.Classes, WinAPI.Windows, System.SysUtils, Generics.Collections,
     XSuperObject, Sub7.Core.OOP.Interfaces;

type
  TS7ProcessInformation = class(TInterfacedObject, IS7Serializable)
  private
    FImagePath : String;
    FUser      : String;
    FDomain    : String;
    FProcessId : Cardinal;
  public
    {@C}
    constructor Create(const ASerializedObject : ISuperObject = nil);

    {@M}
    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);

    {@G/S}
    property ImagePath : String   read FImagePath write FImagePath;
    property User      : String   read FUser      write FUser;
    property Domain    : String   read FDomain    write FDomain;
    property ProcessId : Cardinal read FProcessId write FProcessId;
  end;

  TS7EnumProcess = class(TInterfacedObject, IS7Serializable, IS7Enumerator)
  private
    FItems : TObjectList<TS7ProcessInformation>;

    {@M}
    procedure EnumProcess_TLHELP32();
  public
    {@C}
    constructor Create(const AOwnsObject : Boolean; const ARefresh : Boolean = True);
    destructor Destroy(); override;

    {@M}
    function Refresh() : Integer;
    procedure Clear();

    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);

    {@G}
    property Items : TObjectList<TS7ProcessInformation> read FItems;
  end;

  const PROCESS_QUERY_LIMITED_INFORMATION = $00001000;

  function GetProcessImagePath(const AProcessID : Cardinal) : String;
  procedure GetProcessUser(const AProcessId : Cardinal; var AUserName, ADomain : String);
  procedure TerminateProcess(const AProcessID : Cardinal; var AProcessName : String);
  function ProcessIdExists(const AProcessID : Cardinal) : Boolean;

implementation

uses Winapi.tlHelp32, Sub7.Core.Exceptions, System.IOUtils, Winapi.PsAPI, Sub7.Core.Windows.Information,
     Sub7.Core.Diagnostic, Sub7.Core.Bundle;

var QueryFullProcessImageNameW : function(
                                            AProcess  : THandle;
                                            AFlags    : DWORD;
                                            AFileName : PWideChar;
                                            var ASize : DWORD
    ) : BOOL; stdcall;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  Local


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  Check if process id exists
-------------------------------------------------------------------------------}
function ProcessIdExists(const AProcessID : Cardinal) : Boolean;
var ASnap  : THandle;
    AEntry : TProcessEntry32;

    function Check() : Boolean;
    begin
      result := (AEntry.th32ProcessID = AProcessID);
    end;

begin
  result := False;
  ///

  ASnap := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if ASnap = INVALID_HANDLE_VALUE then
    raise ES7WindowsException.Create('CreateToolHelp32Snapshot');
  try
    ZeroMemory(@AEntry, SizeOf(TProcessEntry32));
    AEntry.dwSize := SizeOf(TProcessEntry32);

    if NOT Process32First(ASnap, AEntry) then
      raise ES7WindowsException.Create('Process32First');

    if Check() then
      Exit(True);

    while True do begin
      ZeroMemory(@AEntry, SizeOf(TProcessEntry32));
      AEntry.dwSize := SizeOf(TProcessEntry32);
      ///

      if NOT Process32Next(ASnap, AEntry) then
        break;

      if Check() then
        Exit(True);
    end;
  finally
    CloseHandle(ASnap);
  end;
end;

{-------------------------------------------------------------------------------
  Terminate Process id
-------------------------------------------------------------------------------}
procedure TerminateProcess(const AProcessID : Cardinal; var AProcessName : String);
var hProc : THandle;
begin
  AProcessName := '';
  ///

  if not ProcessIdExists(AProcessID) then
    raise ES7MissingProcess.Create(Format('pid "%d" is missing.', [AProcessID]));
  ///

  AProcessName := ExtractFileName(GetProcessImagePath(AProcessId));

  hProc := OpenProcess(PROCESS_TERMINATE, False, AProcessID);
  if hProc = 0 then
    raise ES7WindowsException.Create('OpenProcess');
  try
    if not WinAPI.Windows.TerminateProcess(hProc, 0) then
      raise ES7WindowsException.Create('TerminateProcess');
  finally
    CloseHandle(hProc);
  end;
end;

{-------------------------------------------------------------------------------
  Get Process User
-------------------------------------------------------------------------------}
procedure GetProcessUser(const AProcessId : Cardinal; var AUserName, ADomain : String);
var ATokenHandle  : THandle;
    ALength       : Cardinal;
    ptrTokenUser  : PTokenUser;
    ASidNameUser  : SID_NAME_USE;
    hProc         : THandle;
    AUserLength   : Cardinal;
    ADomainLength : Cardinal;
    AFlags        : Integer;
    AUserBuffer   : array of WideChar;
    ADomainBuffer : array of WideChar;
    AMemSize      : Integer;

const MAX_TRY = 50;
begin
  AUserName    := '';
  ADomain      := '';
  ALength      := 0;
  AMemSize     := 0;
  ptrTokenUser := nil;

  if TOSVersion.Major < 6 then
    AFlags := PROCESS_QUERY_INFORMATION
  else
    AFlags := PROCESS_QUERY_LIMITED_INFORMATION;

  hProc := OpenProcess(AFlags, False, AProcessId);
  if hProc = 0 then
    raise ES7WindowsException.Create('OpenProcess');
  try
    if not OpenProcessToken(hProc, TOKEN_QUERY, ATokenHandle) then
      raise ES7WindowsException.Create('OpenProcessToken');

    try
      if not GetTokenInformation(ATokenHandle, TokenUser, nil, 0, ALength) then
        if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
          raise ES7WindowsException.Create('GetTokenInformation');

      AMemSize := ALength;
      GetMem(ptrTokenUser, AMemSize);

      if not GetTokenInformation(ATokenHandle, TokenUser, ptrTokenUser, ALength, ALength) then
        raise ES7WindowsException.Create('GetTokenInformation');
    finally
      CloseHandle(ATokenHandle);
    end;

    AUserLength   := 0;
    ADomainLength := 0;
    ///

    if not LookupAccountSid(
                            nil,
                            ptrTokenUser.User.Sid,
                            nil,
                            AUserLength,
                            nil,
                            ADomainLength,
                            ASidNameUser
    ) then
      if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
        raise ES7WindowsException.Create('LookupAccountSid');

    if (AUserLength <> 0) and (ADomainLength <> 0) then begin

      SetLength(AUserBuffer, AUserLength);
      SetLength(ADomainBuffer, ADomainLength);
      try
        if LookupAccountSid(
                              nil,
                              ptrTokenUser.User.Sid,
                              @AUserBuffer[0],
                              AUserLength,
                              @ADomainBuffer[0],
                              ADomainLength,
                              ASidNameUser
        ) then begin
          SetString(AUserName, PWideChar(AUserBuffer), AUserLength);
          SetString(ADomain, PWideChar(ADomainBuffer), ADomainLength);
        end else
          raise ES7WindowsException.Create('LookupAccountSid');
      finally
        SetLength(AUserBuffer, 0);
        SetLength(ADomainBuffer, 0);
      end;
    end;
  finally
    if AMemSize > 0 then
      FreeMem(ptrTokenUser, AMemSize);
    ///

    CloseHandle(hProc);
  end;
end;

{-------------------------------------------------------------------------------
  Retrieve Process Image Path
-------------------------------------------------------------------------------}
function GetProcessImagePath(const AProcessID : Cardinal) : String;
var hProc      : THandle;
    ABuffer    : array of WideChar;
    ALength    : DWORD;

    AOldMethod : Boolean;
    AFlags     : Integer;
begin
  result := '';
  ///

  AOldMethod := (TOSVersion.Major < 6) or (not Assigned(QueryFullProcessImageNameW));

  if AOldMethod then
    AFlags := PROCESS_QUERY_INFORMATION
  else
    AFlags := PROCESS_QUERY_LIMITED_INFORMATION;

  hProc := OpenProcess(AFlags, false, AProcessID);
  if hProc = 0 then
    raise ES7WindowsException.Create('OpenProcess');
  try
    ALength := (MAX_PATH * 4) +1;
    SetLength(ABuffer, ALength);
    ///

    if not AOldMethod then begin
      if not QueryFullProcessImageNameW(hProc, 0, @ABuffer[0], ALength) then
        raise ES7WindowsException.Create('QueryFullProcessImageNameW');
    end else begin
      ALength := GetModuleFileNameExW(hProc, 0, @ABuffer[0], ALength);
      if ALength = 0 then
        raise ES7WindowsException.Create('GetModuleFileNameExW');
    end;

    ///
    SetString(result, PWideChar(ABuffer), ALength);

    { implement few fixes in path }
    if Copy(result, 1, 4) = '\??\' then
      Delete(result, 1, 4);

    if Copy(result, 1, 12) = '\SystemRoot\' then
      Delete(result, 1, 12);
  finally
    SetLength(ABuffer, 0);

    CloseHandle(hProc);
  end;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7EnumProcess


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  Enumerate Process through CreateToolHelp32Snapshot(...)
-------------------------------------------------------------------------------}
procedure TS7EnumProcess.EnumProcess_TLHELP32();
var ASnap         : THandle;
    AProcessEntry : TProcessEntry32;

    procedure PushProcess();
    var AProcessInformation : TS7ProcessInformation;
        AUser, ADomain      : String;
    begin
      AProcessInformation := TS7ProcessInformation.Create();
      ///

      AProcessInformation.ProcessId := AProcessEntry.th32ProcessID;

      { Image Path }
      try
        AProcessInformation.ImagePath := GetProcessImagePath(AProcessEntry.th32ProcessID);
      except
        on E : Exception do begin
          AProcessInformation.ImagePath := AProcessEntry.szExeFile;

          CrashLog(E, self);
        end;
      end;

      { User / Domain }
      try
        GetProcessUser(
                        AProcessEntry.th32ProcessID,
                        AUser,
                        ADomain
        );

        ///
        AProcessInformation.User   := AUser;
        AProcessInformation.Domain := ADomain;
      except
        on E : Exception do
          CrashLog(E, self);
      end;

      ///
      FItems.Add(AProcessInformation);
    end;

begin
  ASnap := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if ASnap = INVALID_HANDLE_VALUE then
    raise ES7WindowsException.Create('CreateToolHelp32Snapshot');
  try
    ZeroMemory(@AProcessEntry, SizeOf(TProcessEntry32));

    AProcessEntry.dwSize := SizeOf(TProcessEntry32);
    ///

    if NOT Process32First(ASnap, AProcessEntry) then
      raise ES7WindowsException.Create('Process32First');

    PushProcess();

    while True do begin
      ZeroMemory(@AProcessEntry, SizeOf(TProcessEntry32));

      AProcessEntry.dwSize := SizeOf(TProcessEntry32);
      ///

      if NOT Process32Next(ASnap, AProcessEntry) then
        break;

      PushProcess();
    end;
  finally
    CloseHandle(ASnap);
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7EnumProcess.Create(const AOwnsObject : Boolean; const ARefresh : Boolean = True);
begin
  inherited Create();
  ///

  FItems := TObjectList<TS7ProcessInformation>.Create(AOwnsObject);

  if ARefresh then
    self.Refresh();
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7EnumProcess.Destroy();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___refresh
-------------------------------------------------------------------------------}
function TS7EnumProcess.Refresh() : Integer;
begin
  FItems.Clear();
  ///

  self.EnumProcess_TLHELP32();

  ///
  result := FItems.Count;
end;

{-------------------------------------------------------------------------------
  ___clear
-------------------------------------------------------------------------------}
procedure TS7EnumProcess.Clear();
begin
  FItems.Clear();
end;

{-------------------------------------------------------------------------------
  ___serialize
-------------------------------------------------------------------------------}
function TS7EnumProcess.Serialize() : ISuperObject;
var I                   : Integer;
    AProcessInformation : TS7ProcessInformation;
    ANodes              : ISuperArray;
    ANode               : ISuperObject;
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

  for I := 0 to FItems.Count -1 do begin
    AProcessInformation := FItems.Items[I];
    ///

    try
      ANode := AProcessInformation.Serialize();
    except
      {Ignore}
    end;

    ///
    ANodes.Add(ANode);
  end;

  result := TSuperObject.Create();

  result.A['process'] := ANodes;
end;

{-------------------------------------------------------------------------------
  ___deserialize
-------------------------------------------------------------------------------}
procedure TS7EnumProcess.DeSerialize(const ASerializedObject : ISuperObject);
var ANodes              : ISuperArray;
    I                   : Integer;
    AProcessInformation : TS7ProcessInformation;
begin
  FItems.Clear();

  if not Assigned(ASerializedObject) then
    raise ES7DeserializationError.Create(ERR_MISSING_INSTANCE);

  if not ASerializedObject.Contains('process') then
    raise ES7DeserializationError.Create(ERR_INVALID_DATA);

  ANodes := ASerializedObject.A['process'];

  for I := 0 to ANodes.Length -1 do begin
    try
      AProcessInformation := TS7ProcessInformation.Create(ANodes.O[I]);

      ///
      FItems.Add(AProcessInformation);
    except
      { Ignore }
    end;
  end;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7ProcessInformation


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7ProcessInformation.Create(const ASerializedObject : ISuperObject = nil);
begin
  FImagePath := '';
  FUser      := '';
  FDomain    := '';
  FProcessid := 0;

  if Assigned(ASerializedObject) then
    self.DeSerialize(ASerializedObject);
end;

{-------------------------------------------------------------------------------
  ___serialize
-------------------------------------------------------------------------------}
function TS7ProcessInformation.Serialize() : ISuperObject;
begin
  result := TSuperObject.Create();
  ///

  result.V['image_path'] := FImagePath;
  result.I['pid']        := FProcessId;

  if FDomain <> '' then
    result.V['domain'] := FDomain;

  if FUser <> '' then
    result.V['user'] := FUser;
end;

{-------------------------------------------------------------------------------
  ___deserialize
-------------------------------------------------------------------------------}
procedure TS7ProcessInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    raise ES7DeserializationError.Create(ERR_MISSING_INSTANCE);

  if (not ASerializedObject.Contains('image_path')) or (not ASerializedObject.Contains('pid')) then
    raise ES7DeserializationError.Create(ERR_INVALID_DATA);
  ///

  FImagePath := ASerializedObject.V['image_path'];
  FProcessId := ASerializedObject.I['pid'];

  if ASerializedObject.Contains('user') then
    FUser := ASerializedObject.S['user'];

  if ASerializedObject.Contains('domain') then
    FDomain := ASerializedObject.S['domain'];
end;

// +++

initialization
  @QueryFullProcessImageNameW := GetProcAddress(LoadLibrary('kernel32.dll'), 'QueryFullProcessImageNameW');

end.
