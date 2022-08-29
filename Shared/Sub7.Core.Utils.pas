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

unit Sub7.Core.Utils;

interface

uses System.Classes, System.SysUtils, Winapi.Windows;

function FillFree(const AString : String) : String;
function BoolToStr(const b : Boolean) : String;
function BoolToInt(const b : Boolean) : word;

procedure IsValidPEFile(const AFileName : String);

procedure FancyTerminateThread(AThread : TThread);
function GetComputerName() : String;
function IsThreadActive(AThread : TThread) : Boolean;
function RandomString(const ALen : Integer) : String;
function BrowseForFolder(const ADialogTitle : String; const AInitialFolder : String = ''; ACanCreateFolder: Boolean = False) : String;
function TimeSince(const ASeconds : Int64) : String;
function GetHostFolderFile(const AFileName : String) : String;
function iCompare(const AValue : String; const ACompareTo : String) : Boolean;

procedure NTSetPrivilege(const APrivilegeName: string; const AEnabled: Boolean);
function NTGetPrivilegeStatus(APrivilegeName : String; hProcess : THandle = 0) : Boolean;

procedure Open(const ACommand : String);
procedure CheckFileExists(const AFileName : String);

function ReadStringResource(const AResourceName, AResourceType : PWideChar) : String;
function HashSubSevenPassword(const AClearTextPassword : String) : String;

implementation

uses Sub7.Core.Exceptions, Sub7.Core.Bundle, WinAPI.ShlObj, WinAPI.ShellAPI,
     IOUtils, System.DateUtils,  System.Math, System.RegularExpressions,
     System.Hash;

{ _.HashSubSevenPassword }

function HashSubSevenPassword(const AClearTextPassword : String) : String;
begin
  result := System.Hash.THashSHA2.GetHashString(Format('%ssub7legacy_salt', [
    System.Hash.THashSHA2.GetHashString(AClearTextPassword, SHA512)
  ]), SHA512);
end;

{ _.ReadStringResource }

function ReadStringResource(const AResourceName, AResourceType : PWideChar) : String;
var AResourceStream : TResourceStream;
begin
  result := '';
  ///

  AResourceStream := TResourceStream.Create(hInstance, AResourceName, AResourceType);
  try
    AResourceStream.Position := 0;
    ///

    if AResourceStream.Size > 0 then
      SetString(result, PAnsiChar(AResourceStream.Memory), AResourceStream.Size);
  finally
    if Assigned(AResourceStream) then
      FreeAndNil(AResourceStream);
  end;
end;

{ _.CheckFileExists }

procedure CheckFileExists(const AFileName : String);
begin
  if not FileExists(AFileName) then
    raise ES7FileDoesNotExistsException.Create(AFileName);
end;

{ _.Open }

procedure Open(const ACommand : String);
begin
  ShellExecute(0, 'open', PWideChar(ACommand), nil, nil, SW_SHOW);
end;

{-------------------------------------------------------------------------------
  Insensitive Compare String
-------------------------------------------------------------------------------}
function iCompare(const AValue : String; const ACompareTo : String) : Boolean;
begin
  result := String.Compare(Trim(AValue), Trim(ACompareTo), True) = 0;
end;

{-------------------------------------------------------------------------------
  Get File Path in current running process folder
-------------------------------------------------------------------------------}
function GetHostFolderFile(const AFileName : String) : String;
begin
  result := IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(0))) + AFileName;
end;

{-------------------------------------------------------------------------------
  Get Privilege Status, Check if a privilege is activated or not for local
  or target process
-------------------------------------------------------------------------------}
function NTGetPrivilegeStatus(APrivilegeName : String; hProcess : THandle = 0) : Boolean;
var AProcessToken        : THandle;
    pTokenInformation    : PTOKENPRIVILEGES;
    ALength              : Cardinal;
    lpName               : PChar;
    i                    : integer;
    ACurentPrivilegeName : String;

const TokenBufferSize = 1024;
begin
  result := false;
  ///

  GetMem(pTokenInformation, TokenBufferSize);
  try
    if hProcess = 0 then
      hProcess := GetCurrentProcess();

    if NOT OpenProcessToken(hProcess, TOKEN_QUERY, AProcessToken) then
      Exit;
    try
      if NOT GetTokenInformation(AProcessToken, TokenPrivileges, pTokenInformation, TokenBufferSize, ALength) then
        Exit;

      for i := 0 to pTokenInformation^.PrivilegeCount -1 do begin
        GetMem(lpName, MAX_PATH);
        try
          ALength := MAX_PATH;
          ///

          LookupPrivilegeName(nil, pTokenInformation^.Privileges[i].Luid, lpName, ALength);

          ACurentPrivilegeName := StrPas(lpName);

          if String.Compare(ACurentPrivilegeName, APrivilegeName, True) = 0 then begin
            result := (
                       ((pTokenInformation^.Privileges[i].Attributes and SE_PRIVILEGE_ENABLED)= SE_PRIVILEGE_ENABLED) or
                       ((pTokenInformation^.Privileges[i].Attributes and SE_PRIVILEGE_ENABLED_BY_DEFAULT = SE_PRIVILEGE_ENABLED_BY_DEFAULT))
                       );
            Break;
          end;
        finally
          FreeMem(lpName);
        end;
      end;
    finally
      CloseHandle(AProcessToken);
    end;
  finally
    FreeMem(pTokenInformation);
  end;
end;

{-------------------------------------------------------------------------------
  Set Privilege to Enabled or Disabled
-------------------------------------------------------------------------------}

procedure NTSetPrivilege(const APrivilegeName: string; const AEnabled: Boolean);
var AProcessToken   : THandle;
    ATokenPrivilege : TOKEN_PRIVILEGES;
begin
  if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, AProcessToken) then
    raise ES7WindowsException.Create('OpenProcessToken');

  try
    if not LookupPrivilegeValue(nil, PChar(APrivilegeName), ATokenPrivilege.Privileges[0].Luid) then
      raise ES7WindowsException.Create('LookupPrivilegeValue');

    ATokenPrivilege.PrivilegeCount := 1;

    case AEnabled of
      True  : ATokenPrivilege.Privileges[0].Attributes  := SE_PRIVILEGE_ENABLED;
      False : ATokenPrivilege.Privileges[0].Attributes  := 0;
    end;

    if not AdjustTokenPrivileges(
                                  AProcessToken,
                                  False,
                                  ATokenPrivilege,
                                  SizeOf(TOKEN_PRIVILEGES),
                                  PTokenPrivileges(nil)^,
                                  PDWORD(nil)^
    ) then
      raise ES7WindowsException.Create('AdjustTokenPrivileges');
  finally
    CloseHandle(AProcessToken);
  end;
end;

{-------------------------------------------------------------------------------
  Timesince.
-------------------------------------------------------------------------------}
function TimeSince(const ASeconds : Int64) : String;
var m : Integer;

const SECONDS_IN_MINUTES = 60;
      SECONDS_IN_HOUR    = 60 * SECONDS_IN_MINUTES;
      SECONDS_IN_DAY     = 24 * SECONDS_IN_HOUR;
begin
  result := '';
  ///

  if ASeconds < SECONDS_IN_MINUTES then begin
    { Seconds }
    result := Format('%d Seconds', [ASeconds]);
  end else if (ASeconds >= SECONDS_IN_MINUTES) and (ASeconds < SECONDS_IN_HOUR) then begin
    { Minutes }
    result := Format('%d Minutes', [ceil(ASeconds / SECONDS_IN_MINUTES)]);
  end else if (ASeconds >= SECONDS_IN_HOUR) and (ASeconds < SECONDS_IN_DAY) then begin
    { Hours }
    result := Format('%d Hours', [ceil(ASeconds / SECONDS_IN_HOUR)]);
  end else begin
    { Days }
    result := Format('%d Days', [ceil(ASeconds / SECONDS_IN_DAY)]);
  end;
end;

{-------------------------------------------------------------------------------
  Show native Windows Dialog to select an existing folder.
-------------------------------------------------------------------------------}

function BrowseForFolderCallBack(hwnd : HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) then begin
    SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData);
  end;

  ///
  result := 0;
end;

function BrowseForFolder(const ADialogTitle : String; const AInitialFolder : String = ''; ACanCreateFolder: Boolean = False) : String;
var ABrowseInfo : TBrowseInfo;
    AFolder  : array[0..MAX_PATH-1] of Char;
    pItem  : PItemIDList;
begin
  ZeroMemory(@ABrowseInfo, SizeOf(TBrowseInfo));
  ///

  ABrowseInfo.pszDisplayName := @AFolder[0];
  ABrowseInfo.lpszTitle := PChar(ADialogTitle);
  ABrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE;


  if NOT ACanCreateFolder then
    ABrowseInfo.ulFlags := ABrowseInfo.ulFlags or BIF_NONEWFOLDERBUTTON;

  ABrowseInfo.hwndOwner := 0;

  if AInitialFolder <> '' then begin
    ABrowseInfo.lpfn   := BrowseForFolderCallBack;
    ABrowseInfo.lParam := NativeUInt(@AInitialFolder[1]);
  end;

  pItem := SHBrowseForFolder(ABrowseInfo);
  if Assigned(pItem) then begin
    if SHGetPathFromIDList(pItem, AFolder) then
      result := IncludeTrailingPathDelimiter(AFolder)
    else
      result := '';

    GlobalFreePtr(pItem);
  end else
    result := '';
end;

{-------------------------------------------------------------------------------
  Is Thread Active
-------------------------------------------------------------------------------}
function IsThreadActive(AThread : TThread) : Boolean;
var AExitCode : Cardinal;
begin
  result := False;

  if not Assigned(AThread) then
    Exit();

  GetExitCodeThread(AThread.handle, AExitCode);

  result := (AExitCode = STILL_ACTIVE);
end;

{-------------------------------------------------------------------------------
  Get Computer Name
-------------------------------------------------------------------------------}
function GetComputerName() : String;
var ABufferSize : Cardinal;
begin
  result := '';
  ///

  ABufferSize := 0;

  GetComputerNameExW(ComputerNameDnsFullyQualified, nil, ABufferSize);

  if ABufferSize > 1 then begin
    SetLength(result, ABufferSize);

    GetComputerNameExW(ComputerNameDnsFullyQualified, PWideChar(result), ABufferSize);

    SetLength(result, ABufferSize);
  end else begin
    ABufferSize := MAX_COMPUTERNAME_LENGTH +1;

    SetLength(result, ABufferSize);

    WinAPI.Windows.GetComputerName(PWideChar(result), ABufferSize);

    SetLength(result, ABufferSize);
  end;
end;

{-------------------------------------------------------------------------------
  GenerateRandom String
-------------------------------------------------------------------------------}
function RandomString(const ALen : Integer) : String;
const AChars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@#%$*-+&(_)={}][|\/;.!';
var i : integer;
begin
  randomize();
  ///

  result := '';
  for i := 1 to ALen -1 do begin
    result := result + AChars[random(length(AChars))+1];
  end;
end;

{-------------------------------------------------------------------------------
  Terminate a VCL Thread Correctly
-------------------------------------------------------------------------------}
procedure FancyTerminateThread(AThread : TThread);
var AExitCode        : Cardinal;
    AFreeOnTerminate : Boolean;
begin
  if Assigned(AThread) then begin
    try
      AFreeOnTerminate := AThread.FreeOnTerminate;
    except
      AFreeOnTerminate := False;
    end;

    GetExitCodeThread(AThread.Handle, AExitCode);
    if (AExitCode = STILL_ACTIVE) then begin
      AThread.Terminate();

      AThread.WaitFor();
    end;

    if not AFreeOnTerminate then
      FreeAndNil(AThread)
    else
      AThread := nil;
  end;
end;

{ _.IsValidPEFile }

procedure IsValidPEFile(const AFileName : String);
var hFile                   : THandle;
    AImageDosHeader         : TImageDosHeader;
    dwBytesRead             : DWORD;
    AImageFileHeader        : TImageFileHeader;
    AImageNtHeaderSignature : DWORD;
begin
  hFile := CreateFile(
                        PWideChar(AFileName),
                        GENERIC_READ,
                        FILE_SHARE_READ,
                        nil,
                        OPEN_EXISTING,
                        0,
                        0
  );
  if hFile = INVALID_HANDLE_VALUE then
    raise ES7FileException.Create(Format(ERR_FILE_IO, [AFileName]));

  try
    SetFilePointer(hFile, 0, nil, FILE_BEGIN);

    // Read the Image Dos Header
    if NOT ReadFile(
                      hFile,
                      AImageDosHeader,
                      SizeOf(TImageDosHeader),
                      dwBytesRead,
                      nil
    ) then
      raise ES7WindowsException.Create('(1)ReadFile');

    // To be considered as a valid PE file, e_magic must be $5A4D (MZ)
    if (AImageDosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then
      raise ES7Exception.Create(Format(ERR_PE, [AFileName]));

    // Move the cursor to Image NT Signature
    SetFilePointer(hFile, AImageDosHeader._lfanew, nil, FILE_BEGIN);

    // Read the Image NT Signature
    if NOT ReadFile(
                      hFile,
                      AImageNtHeaderSignature,
                      SizeOf(DWORD),
                      dwBytesRead,
                      nil
    ) then
      raise ES7WindowsException.Create('(2)ReadFile');

    // To be considered as a valid PE file, Image NT Signature must be $00004550 (PE00)
    if (AImageNtHeaderSignature <> IMAGE_NT_SIGNATURE) then
      raise ES7Exception.Create(Format(ERR_PE, [AFileName]));
  finally
    CloseHandle(hFile);
  end;
end;

function FillFree(const AString : String) : String;
begin
  if Length(Trim(AString)) = 0 then
    result := '-'
  else
    result := AString;
end;

function BoolToStr(const b : Boolean) : String;
begin
  if b then
    result := 'True'
  else
    result := 'False';
end;

function BoolToInt(const b : Boolean) : word;
begin
  if b then
    result := 1
  else
    result := 0;
end;

end.
