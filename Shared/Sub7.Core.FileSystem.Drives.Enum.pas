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

unit Sub7.Core.FileSystem.Drives.Enum;

interface

uses System.Classes, WinAPI.Windows, System.SysUtils, Generics.Collections,
     XSuperObject, Sub7.Core.OOP.Interfaces;

type
  TDriveType = (
                  dtUnknown,
                  dtNoRootDir,
                  dtRemovable,
                  dtFixed,
                  dtRemote,
                  dtCDROM,
                  dtRAMDisk
  );

  TDriveInformation = class(TInterfacedObject, IS7Serializable)
  private
    FLetter        : String;
    FPartitionName : String;
    FPartitionType : String;
    FDriveType     : TDriveType;
    FDiskSize      : Int64;
    FFreeSize      : Int64;
  public
    {@C}
    constructor Create(const ASerializedObject : ISuperObject = nil);

    {@M}
    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);

    {@G/S}
    property Letter        : String     read FLetter        write FLetter;
    property PartitionName : String     read FPartitionName write FPartitionName;
    property PartitionType : String     read FPartitionType write FPartitionType;
    property DriveType     : TDriveType read FDriveType     write FDriveType;
    property DiskSize      : Int64      read FDiskSize      write FDiskSize;
    property FreeSize      : Int64      read FFreeSize      write FFreeSize;
  end;

  TS7EnumHardDrives = class(TInterfacedObject, IS7Serializable, IS7Enumerator)
  private
    FItems : TList<TDriveInformation>;

    {@M}
    function GetDriveInformation(APath : String; var APartitionName : String; var APartitionType : String; var ADriveType : TDriveType) : Boolean;
  public
    {@C}
    constructor Create(const AOwnsObject : Boolean; const ARefresh : Boolean = True);
    destructor Destroy(); override;

    {@M}
    function DriveTypeToString(ADriveType : TDriveType) : String;

    function Refresh() : Integer;
    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);
    procedure Clear();

    {@G}
    property Items : TList<TDriveInformation> read FItems;
  end;

implementation

uses Sub7.Core.Exceptions, Sub7.Core.Bundle;

(*******************************************************************************


    TDriveInformation


*******************************************************************************)

{ TDriveInformation.Create }

constructor TDriveInformation.Create(const ASerializedObject : ISuperObject = nil);
begin
  FLetter        := '';
  FPartitionName := '';
  FPartitionType := '';
  FDriveType     := dtUnknown;
  FDiskSize      := 0;
  FFreeSize      := 0;

  if Assigned(ASerializedObject) then
    self.DeSerialize(ASerializedObject);
end;

{ TDriveInformation.Serialize }

function TDriveInformation.Serialize() : ISuperObject;
begin
  result := TSuperObject.Create();
  ///

  result.V['letter']         := FLetter;
  result.V['partition_name'] := FPartitionName;
  result.V['partition_type'] := FPartitionType;
  result.I['drive_type']     := Integer(FDriveType);
  result.I['disk_size']      := FDiskSize;
  result.I['free_size']      := FFreeSize;
end;

{ TDriveInformation.DeSerialize }

procedure TDriveInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    raise ES7DeserializationError.Create(ERR_MISSING_INSTANCE);

  if not ASerializedObject.Contains('letter')
  then
    raise ES7DeserializationError.Create(ERR_INVALID_DATA);
  ///

  if ASerializedObject.Contains('letter') then
    FLetter := ASerializedObject.S['letter'];

  if ASerializedObject.Contains('partition_name') then
    FPartitionName := ASerializedObject.S['partition_name'];

  if ASerializedObject.Contains('partition_type') then
    FPartitionType := ASerializedObject.S['partition_type'];

  if ASerializedObject.Contains('drive_type') then
    FDriveType := TDriveType(ASerializedObject.I['drive_type']);

  if ASerializedObject.Contains('disk_size') then
    FDiskSize := ASerializedObject.I['disk_size'];

  if ASerializedObject.Contains('free_size') then
    FFreeSize := ASerializedObject.I['free_sze'];
end;

(*******************************************************************************


    TS7EnumHardDrives


*******************************************************************************)

{ TS7EnumHardDrives.Clear }

procedure TS7EnumHardDrives.Clear();
begin
  if Assigned(FItems) then
    FItems.Clear();
end;

{ TS7EnumHardDrives.Create }

constructor TS7EnumHardDrives.Create(const AOwnsObject : Boolean; const ARefresh : Boolean = True);
begin
  FItems := TObjectList<TDriveInformation>.Create(AOwnsObject);

  if ARefresh then
    self.Refresh();
end;

{ TS7EnumHardDrives.Destroy }

destructor TS7EnumHardDrives.Destroy();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);

  ///
  inherited Destroy();
end;

{ TS7EnumHardDrives.GetDriveInformation }

function TS7EnumHardDrives.GetDriveInformation(APath : String; var APartitionName : String; var APartitionType : String; var ADriveType : TDriveType) : Boolean;
var
  ADummy         : DWORD;
  ABufferType    : array[0..MAX_PATH-1] of Char;
  ABufferName    : array[0..MAX_PATH-1] of Char;
  AOldErrorMode  : LongInt;
begin
  AOldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    APath := IncludeTrailingPathDelimiter(ExtractFileDrive(APath));
    ///

    {
      Get Drive Name
      Get Partition Type Name
    }
    FillChar(ABufferName, MAX_PATH, #0);
    FillChar(ABufferType, MAX_PATH, #0);

    result := GetVolumeInformation(
                                    PChar(APath),
                                    ABufferName,
                                    MAX_PATH,
                                    nil,
                                    ADummy,
                                    ADummy,
                                    ABufferType,
                                    MAX_PATH
    );

    {
      Conv to String
    }
    APartitionName := StrPas(ABufferName);
    APartitionType := StrPas(ABufferType);

    {
      Get Drive Type
    }
    case GetDriveType(PChar(APath)) of
      1 : ADriveType := dtNoRootDir; // DRIVE_NO_ROOT_DIR
      2 : ADriveType := dtRemovable; // DRIVE_REMOVABLE
      3 : ADriveType := dtFixed;     // DRIVE_FIXED
      4 : ADriveType := dtRemote;    // DRIVE_REMOTE
      5 : ADriveType := dtCDROM;     // DRIVE_CDROM
      6 : ADriveType := dtRAMDisk;   // DRIVE_RAMDISK
      else
        ADriveType := dtUnknown;
    end;
  finally
    SetErrorMode(AOldErrorMode);
  end;
end;

{ TS7EnumHardDrives.Refresh }

function TS7EnumHardDrives.Refresh() : Integer;
var ADriveType        : TDriveType;
    ALetter           : String;
    I                 : Integer;
    ALogicalDrives    : Integer;

    APartitionType    : String;
    APartitionName    : String;
    ADiskSize         : Int64;
    AFreeSize         : Int64;

    ADrive            : String;

    ADriveInformation : TDriveInformation;
begin
  FItems.Clear();
  try
    ALogicalDrives := GetLogicalDrives();
    ///

    I := 0;
    for ALetter in ['a'..'z'] do begin
      if (ALogicalDrives and (1 shl I)) = 0 then begin
        Inc(I);

        continue;
      end;
      ///

      Inc(I);

      ADrive := Format('%s:', [ALetter.ToUpper]);

      {
        Get More Information
      }
      GetDriveInformation(
                            ADrive,
                            APartitionName,
                            APartitionType,
                            ADriveType
      );

      {
        Get Disk Size
      }
      ADiskSize := DiskSize(I);
      AFreeSize := DiskFree(I);

      {
        Append new information to our Items List
      }
      ADriveInformation := TDriveInformation.Create();

      ADriveInformation.Letter        := ADrive;
      ADriveInformation.PartitionName := APartitionName;
      ADriveInformation.PartitionType := APartitionType;
      ADriveInformation.DriveType     := ADriveType;
      ADriveInformation.DiskSize      := ADiskSize;
      ADriveInformation.FreeSize      := AFreeSize;

      ///
      FItems.Add(ADriveInformation);
    end;
  finally
    result := FItems.Count;
  end;
end;

{ TS7EnumHardDrives.DriveTypeToString }

function TS7EnumHardDrives.DriveTypeToString(ADriveType : TDriveType) : String;
begin
  case ADriveType of
    dtUnknown   : result := 'Unknown';
    dtNoRootDir : result := 'No Root Dir';
    dtRemovable : result := 'Removable';
    dtFixed     : result := 'Fixed';
    dtRemote    : result := 'Network';
    dtCDROM     : result := 'CD-ROM';
    dtRAMDisk   : result := 'RAM Disk';
  end;
end;

{ TS7EnumHardDrives.Serialize }

function TS7EnumHardDrives.Serialize() : ISuperObject;
var I      : Integer;
    ANodes : ISuperArray;
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
    try
      ANodes.Add(FItems.Items[I].Serialize());
    except

    end;
  end;

  result := TSuperObject.Create();

  result.A['drives'] := ANodes;
end;

{ TS7EnumHardDrives.FromJson }

procedure TS7EnumHardDrives.DeSerialize(const ASerializedObject : ISuperObject);
var ANodes            : ISuperArray;
    ANode             : ISuperObject;
    I                 : Integer;
    ADriveInformation : TDriveInformation;
begin
  self.Clear();
  ///

  if not Assigned(ASerializedObject) then
    raise ES7DeserializationError.Create(ERR_MISSING_INSTANCE);

  if not ASerializedObject.Contains('drives') then
    raise ES7DeserializationError.Create(ERR_INVALID_DATA);

  ANodes := ASerializedObject.A['drives'];

  for I := 0 to ANodes.Length -1 do begin
    try
      FItems.Add(TDriveInformation.Create(ANodes.O[I]));
    except

    end;
  end;
end;

end.
