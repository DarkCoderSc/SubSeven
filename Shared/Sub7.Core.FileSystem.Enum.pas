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

unit Sub7.Core.FileSystem.Enum;

interface

uses System.Classes, Generics.Collections, XSuperObject, Sub7.Core.OOP.Interfaces;

type
  TFileType = (
                  ftFile,
                  ftFolder,
                  ftParentFolder,
                  ftUndefined
  );

  TFileInformation = class(TInterfacedObject, IS7Serializable)
  private
    FFileName : String;
    FSize     : Int64;
    FType     : TFileType;
  public
    {@C}
    constructor Create(const ASerializedObject : ISuperObject = nil);

    {@M}
    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);

    {@G/S}
    property FileName : String    read FFileName write FFileName;
    property Size     : Int64     read FSize     write FSize;
    property FileType : TFileType read FType     write FType;
  end;

  TS7EnumFolder = class(TInterfacedObject, IS7Serializable, IS7Enumerator)
  private
    FCurrentPath : String;
    FItems       : TObjectList<TFileInformation>;
  public
    {@C}
    constructor Create(const AOwnsObject : Boolean);
    destructor Destroy(); override;

    {@M}
    function Browse(const APath : String) : Integer;
    function Refresh() : Integer;
    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);
    procedure Clear();

    {@G}
    property Items       : TObjectList<TFileInformation> read FItems;
    property CurrentPath : String                        read FCurrentPath;
  end;

implementation

uses WinAPI.Windows, System.SysUtils, System.Math, Sub7.Core.Diagnostic,
     Sub7.Core.Exceptions, Sub7.Core.Bundle, Sub7.Core.FileSystem.Utils;

(*******************************************************************************


    TFileInformation


*******************************************************************************)

{ TFileInformation.Create }

constructor TFileInformation.Create(const ASerializedObject : ISuperObject = nil);
begin
  FFileName := '';
  FSize     := 0;
  FType     := ftUndefined;

  if Assigned(ASerializedObject) then
    self.DeSerialize(ASerializedObject);
end;

{ TFileInformation.Serialize }

function TFileInformation.Serialize() : ISuperObject;
begin
  result := TSuperObject.Create();

  result.V['name'] := FFileName;
  result.I['type'] := Integer(FType);

  if FType = ftFile then
    result.I['size'] := FSize;
end;

{ TFileInformation.DeSerialize }

procedure TFileInformation.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    raise ES7DeserializationError.Create(ERR_MISSING_INSTANCE);

  if not ASerializedObject.Contains('name') or not ASerializedObject.Contains('type')
  then
    raise ES7DeserializationError.Create(ERR_INVALID_DATA);

  FFileName := ASerializedObject.S['name'];
  FType     := TFileType(ASerializedObject.I['type']);

  if ASerializedObject.Contains('size') then
    FSize := ASerializedObject.I['size'];
end;

(*******************************************************************************


    TS7EnumFolder


*******************************************************************************)

{ TS7EnumFolder.Create }

constructor TS7EnumFolder.Create(const AOwnsObject : Boolean);
begin
  FItems       := TObjectList<TFileInformation>.Create(AOwnsObject);
  FCurrentPath := '';
end;

{ TS7EnumFolder.Destroy }

destructor TS7EnumFolder.Destroy();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);

  ///
  inherited Destroy();
end;

{ TS7EnumFolder.Browse }

function TS7EnumFolder.Browse(const APath : String) : Integer;
var ASearchRec : TSearchRec;
    AFileInfo  : TFileInformation;
begin
  result := 0;
  ///

  FCurrentPath := IncludeTrailingPathDelimiter(SecurePath(APath));

  self.Clear();

  ZeroMemory(@ASearchRec, SizeOf(TSearchRec));

  if System.SysUtils.FindFirst(Format('%s*.*', [FCurrentPath]), faAnyFile, ASearchRec) <> 0 then
    raise ES7BrowseDirectoryException.Create(Format('Could not enumerate content of "%s", %s', [FCurrentPath, SysErrorMessage(GetLastError)]));
  try
    repeat
      if (ASearchRec.Name = '.') then
        continue;

      GetFileInfo(FCurrentPath, ASearchRec, AFileInfo);

      FItems.Add(AFileInfo);
    until System.SysUtils.FindNext(ASearchRec) <> 0;
  finally
    System.SysUtils.FindClose(ASearchRec);
  end;

  ///
  result := FItems.Count;
end;

{ TS7EnumFolder.Refresh }

function TS7EnumFolder.Refresh() : Integer;
begin
  if DirectoryExists(FCurrentPath) and IsDir(FCurrentPath) then
    result := self.Browse(FCurrentPath);
end;

{ TS7EnumFolder.Serialize }

function TS7EnumFolder.Serialize() : ISuperObject;
var I         : Integer;
    ANodes    : ISuperArray;
begin
  result := TSuperObject.Create();
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

  result.V['path']  := FCurrentPath;
  result.A['files'] := ANodes;
end;

{ TS7EnumFolder.DeSerialize }

procedure TS7EnumFolder.DeSerialize(const ASerializedObject : ISuperObject);
var ANodes : ISuperArray;
    I      : Integer;
begin
  self.Clear();
  ///

  if not Assigned(ASerializedObject) then
    raise ES7DeserializationError.Create(ERR_MISSING_INSTANCE);

  if not ASerializedObject.Contains('files') or not ASerializedObject.Contains('path') then
    raise ES7DeserializationError.Create(ERR_INVALID_DATA);

  FCurrentPath := IncludeTrailingPathDelimiter(SecurePath(ASerializedObject.V['path']));

  ANodes := ASerializedObject.A['files'];

  for I := 0 to ANodes.Length -1 do begin
    try
      FItems.Add(TFileInformation.Create(ANodes.O[I]));
    except

    end;
  end;
end;

{ TS7EnumFolder.Clear }

procedure TS7EnumFolder.Clear();
begin
  if Assigned(FItems) then
    FItems.Clear();
end;

end.

