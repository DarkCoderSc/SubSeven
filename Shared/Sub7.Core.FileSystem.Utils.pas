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

unit Sub7.Core.FileSystem.Utils;

interface

uses System.Classes, Winapi.Windows, Winapi.ShellAPI, VCL.Controls, System.SysUtils,
     Sub7.Core.FileSystem.Enum;

procedure TouchFile(const AFileName : String; const AResetFile : Boolean = False);
procedure WriteTextToFile(const AString, AFileName : String);
procedure ReadTextFile(const AFileName : String; var AString : String);
function SecurePath(const APath : String) : String;
function GetFileSize(AFileName : String) : Int64;
function FormatSize(const ASize : Int64) : string;
procedure InitializeSystemIcons(var AImages : TImageList; var AFileInfo : TSHFileInfo; ALargeIcon : Boolean = False);
function SystemFolderIcon() : Integer;
function SystemFileIcon(const AFileName : string) : Integer;
function IsDir(const APath : String) : Boolean;
function UniqueFileName(const AFileName : String; const AIgnore : Boolean = True) : String;
function GetFileInfo(const AFileName : String; var AInfo : TFileInformation) : Boolean; overload;
procedure GetFileInfo(const ABaseDirectory : String; const ASearchRec : System.SysUtils.TSearchRec; var AInfo : TFileInformation); overload;

implementation

uses Sub7.Core.Exceptions, System.IOUtils, System.Math, Sub7.Core.Windows.Information;

{ _. }

procedure GetFileInfo(const ABaseDirectory : String; const ASearchRec : System.SysUtils.TSearchRec; var AInfo : TFileInformation);
var AFullPath : String;
begin
  AInfo := TFileInformation.Create();
  ///

  AInfo.FileName := ASearchRec.Name;

  if ASearchRec.Name = '..' then
    AInfo.FileType := ftParentFolder
  else begin
    AFullPath := Format('%s%s', [IncludeTrailingBackSlash(ABaseDirectory), ASearchRec.Name]);

    if (ASearchRec.Attr and faDirectory) = faDirectory then
      AInfo.FileType := ftFolder
    else
      AInfo.FileType := ftFile;

    if AInfo.FileType = ftFile then
      AInfo.Size := GetFileSize(AFullPath);
  end;
end;

{ _. }

function GetFileInfo(const AFileName : String; var AInfo : TFileInformation) : Boolean;
var ASearchRec : System.SysUtils.TSearchRec;
begin
  result := False;
  ///

  if (System.SysUtils.FindFirst(AFileName, faAnyFile, ASearchRec) = 0) then begin
    try
      GetFileInfo(ExtractFilePath(AFileName), ASearchRec, AInfo);

      ///
      result := True;
    finally
      System.SysUtils.FindClose(ASearchRec);
    end;
  end;
end;

{ _. }

function UniqueFileName(const AFileName : String; const AIgnore : Boolean = True) : String;
var i               : integer;
    AUniqueFileName : String;
begin
  if AIgnore then
    Exit(AFileName);
  ///

  if not FileExists(AFileName) then
    Exit(AFileName);

  i := 1;
  repeat
    AUniqueFileName := Format('%s%s(%d)%s', [
                                            IncludeTrailingPathDelimiter(ExtractFilePath(AFileName)),
                                            TPath.GetFileNameWithoutExtension(AFileName),
                                            i,
                                            ExtractFileExt(AFileName)
                                          ]
                              );

    Inc(i);
  until (NOT FileExists(AUniqueFileName));

  ///
  result := AUniqueFileName;
end;

{ _. }

function IsDir(const APath : String) : Boolean;
var ASearchRec : System.SysUtils.TSearchRec;
begin
  result := False;
  ///

  if (System.SysUtils.FindFirst(APath, faAnyFile, ASearchRec) = 0) then begin
    try
      result := ((ASearchRec.Attr and faDirectory) <> 0);
    finally
      System.SysUtils.FindClose(ASearchRec);
    end;
  end;
end;

{ _. }

procedure InitializeSystemIcons(var AImages : TImageList; var AFileInfo : TSHFileInfo; ALargeIcon : Boolean = False);
var AFlags : Integer;
begin
  ZeroMemory(@AFileInfo, SizeOf(TSHFileInfo));
  ///

  if ALargeIcon then
    AFlags := SHGFI_LARGEICON
  else
    AFlags := SHGFI_SMALLICON;

  AImages.Handle := SHGetFileInfo(
                                    PChar(TPath.GetPathRoot(GetWindowsDirectory())),
                                    0,
                                    AFileInfo,
                                    SizeOf(AFileInfo),
                                    AFlags or (SHGFI_SYSICONINDEX)
  );
end;

{ _. }

function SystemFolderIcon() : Integer;
var AFileInfo : TSHFileInfo;
    AFileName : String;
    AFlags    : Integer;
begin
  ZeroMemory(@AFileInfo, sizeof(AFileInfo));
  ///

  AFileName := GetWindowsDirectory();

  AFlags := SHGFI_SYSICONINDEX;

  SHGetFileInfo(PChar(AFileName), 0, AFileInfo, SizeOf(AFileInfo), AFlags);

  Result := AFileInfo.iIcon;
end;

{ _. }

function SystemFileIcon(const AFileName : string) : Integer;
var AFileInfo  : TSHFileInfo;
    AFlags     : Integer;
    AExtension : String;
begin
  ZeroMemory(@AFileInfo, sizeof(AFileInfo));
  ///

  if ExtractFileExt(AFileName) = '.' then
    AExtension := '.dummy_ext'
  else
    AExtension := ExtractFileExt(AFileName);

  AFlags := SHGFI_SMALLICON or SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES;

  SHGetFileInfo(PWideChar(AExtension), 0, AFileInfo, SizeOf(AFileInfo), AFlags);

  Result := AFileInfo.iIcon;
end;

{ _. }

function FormatSize(const ASize : Int64) : string;
const AByteDescription : Array[0..8] of string = ('Bytes', 'KiB', 'MB', 'GiB', 'TB', 'PB', 'EB', 'ZB', 'YB');

var ACount : Integer;
begin
  ACount := 0;

  while ASize > Power(1024, ACount +1) do
    Inc(ACount);

  result := Format('%s %s', [FormatFloat('###0.00', ASize / Power(1024, ACount)), AByteDescription[ACount]]);
end;

{ _. }

function SecurePath(const APath : String) : String;
begin
  result := APath;
  ///

  result := result.Replace('*', '', [rfReplaceAll]);
  result := result.Replace('?', '', [rfReplaceAll]);
  result := result.Replace('"', '', [rfReplaceAll]);
  result := result.Replace('<', '', [rfReplaceAll]);
  result := result.Replace('>', '', [rfReplaceAll]);
  result := result.Replace('|', '', [rfReplaceAll]);
end;

{ _. }

function GetFileSize(AFileName : String) : Int64;
var AFileInfo : TWin32FileAttributeData;
begin
  result := 0;
  ///

  if NOT FileExists(AFileName) then
    Exit();

  if NOT GetFileAttributesEx(PWideChar(AFileName), GetFileExInfoStandard, @AFileInfo) then
    Exit();

  ///
  result := Int64(AFileInfo.nFileSizeLow) or Int64(AFileInfo.nFileSizeHigh shl 32);
end;

{ _.TouchFile
  If AResetFile param is set to true and target filename already exists with
  data, file will be reset as an empty file}

procedure TouchFile(const AFileName : String; const AResetFile : Boolean = False);
var AFlags : Cardinal;
    hFile  : THandle;
begin
  if AResetFile then
    AFlags := CREATE_ALWAYS
  else
    AFlags := CREATE_NEW;
  ///

  hFile := CreateFileW(
                        PWideChar(AFileName),
                        (GENERIC_READ or GENERIC_WRITE),
                        FILE_SHARE_DELETE or FILE_SHARE_READ or FILE_SHARE_WRITE,
                        nil,
                        AFlags,
                        FILE_ATTRIBUTE_NORMAL,
                        0
  );

  if hFile = INVALID_HANDLE_VALUE then begin
    if (GetLastError() = ERROR_ALREADY_EXISTS) and (AFlags = CREATE_ALWAYS) then
      SetLastError(0);

    if (GetLastError() = ERROR_FILE_EXISTS) and (AFlags = CREATE_NEW) then
      SetLastError(0);

    if (GetLastError() <> 0) then
      raise ES7WindowsException.Create('CreateFileW')
  end else
    CloseHandle(hFile);
end;

{ _.WriteTextToFile }

procedure WriteTextToFile(const AString, AFileName : String);
var AStringList : TStringList;
begin
  AStringList := TStringList.Create();
  try
    AStringList.Text := AString;
    ///

    AStringList.SaveToFile(AFileName);
  finally
    if Assigned(AStringList) then
      FreeAndNil(AStringList);
  end;
end;

{ _.ReadTextFile }

procedure ReadTextFile(const AFileName : String; var AString : String);
var AStringList : TStringList;
begin
  AString := '';
  ///

  AStringList := TStringList.Create();
  try
    AStringList.LoadFromFile(AFileName);

    ///
    AString := AStringList.Text;
  finally
    if Assigned(AStringList) then
      FreeAndNil(AStringList);
  end;
end;

end.
