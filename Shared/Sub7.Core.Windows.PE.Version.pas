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
unit Sub7.Core.Windows.PE.Version;

interface

uses Winapi.Windows, System.Classes, Sub7.Core.OOP.Interfaces, XSuperObject;

type
  TVersion = class(TInterfacedObject, IS7Serializable)
  private
    FMajor   : Cardinal;
    FMinor   : Cardinal;
    FRelease : Cardinal;
    FBuild   : Cardinal;

    {@M}
    function GetStringVersion() : String;
  public
    {@C}
    constructor Create(const AFileName : String); overload;
    constructor Create(const ASerializedObject : ISuperObject); overload;

    {@M}
    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);
    function CompareTo(const AVersion : TVersion) : Integer;

    {@S}
    class procedure GetFileVersion(const AFileName : String; var AMajor, AMinor, ARelease, ABuild : Cardinal); overload; static;
    class function GetFileVersion(const AFileName : String; const AIncludeBuild : Boolean = False) : String; overload; static;
    class function CompareFileVersion(const AVersion1, AVersion2 : TVersion) : Integer; static;

    {@G}
    property Major   : Cardinal read FMajor   default 0;
    property Minor   : Cardinal read FMinor   default 0;
    property Release : Cardinal read FRelease default 0;
    property Build   : Cardinal read FBuild   default 0;

    property ToString : String read GetStringVersion;
  end;

implementation

uses System.SysUtils, Sub7.Core.Exceptions, Sub7.Core.Bundle, System.Math;

{ TVersion.Create }

constructor TVersion.Create(const AFileName : String);
begin
  inherited Create();
  ///

  self.GetFileVersion(AFileName, self.FMajor, self.FMinor, self.FRelease, self.FBuild);
end;

constructor TVersion.Create(const ASerializedObject : ISuperObject);
begin
  inherited Create();
  ///

  DeSerialize(ASerializedObject);
end;

{ TVersion.Serialize }

function TVersion.Serialize() : ISuperObject;
begin
  result := TSuperObject.Create();
  ///

  result.I['major']   := FMajor;
  result.I['minor']   := FMinor;
  result.I['release'] := FRelease;
  result.I['build']   := FBuild;
end;

{ TVersion.GetStringVersion }

function TVersion.GetStringVersion() : String;
begin
  result := Format('%d.%d.%d.%d', [FMajor, FMinor, FRelease, FBuild]);
end;

{ TVersion.DeSerialize }

procedure TVersion.DeSerialize(const ASerializedObject : ISuperObject);
begin
  if not Assigned(ASerializedObject) then
    raise ES7DeserializationError.Create(ERR_MISSING_INSTANCE);
  ///

  if not ASerializedObject.Contains('major') or
     not ASerializedObject.Contains('minor') or
     not ASerializedObject.Contains('release') or
     not ASerializedObject.Contains('build')
  then
    raise ES7DeserializationError.Create(ERR_INVALID_DATA);
  ///

  FMajor   := ASerializedObject.I['major'];
  FMinor   := ASerializedObject.I['minor'];
  FRelease := ASerializedObject.I['release'];
  FBuild   := ASerializedObject.I['build'];
end;

{ TVersion.CompareTo }

function TVersion.CompareTo(const AVersion : TVersion) : Integer;
begin
  result := self.CompareFileVersion(self, AVersion);
end;

{ TVersion.CompareFileVersion }

class function TVersion.CompareFileVersion(const AVersion1, AVersion2 : TVersion) : Integer;
begin
  // Compare Major
  result := CompareValue(AVersion1.Major, AVersion2.Major);
  if result <> 0 then
    Exit();

  // Compare Minor
  result := CompareValue(AVersion1.Minor, AVersion2.Minor);
  if result <> 0 then
    Exit();

  // Compare Release
  result := CompareValue(AVersion1.Release, AVersion2.Release);
end;

{ TVersion.GetFileVersion }

class procedure TVersion.GetFileVersion(const AFileName : String; var AMajor, AMinor, ARelease, ABuild : Cardinal);
var AValueSize : Cardinal;
    ASize      : Cardinal;
    ADummy     : Cardinal;
    pVerInfo   : Pointer;
    pVerValue  : PVSFixedFileInfo;
begin
  AMajor   := 0;
  AMinor   := 0;
  ARelease := 0;
  ABuild   := 0;
  ///

  ASize := GetFileVersionInfoSize(PWideChar(AFileName), ADummy);
  GetMem(pVerInfo, ASize);
  try
    if not GetFileVersionInfo(PWideChar(AFileName), 0, ASize, pVerInfo) then
      Exit();

    if not VerQueryValue(pVerInfo, '\', Pointer(pVerValue), AValueSize) then
      Exit();

    AMajor   := HiWord(pVerValue^.dwFileVersionMS);
    AMinor   := LoWord(pVerValue^.dwFileVersionMS);
    ARelease := HiWord(pVerValue^.dwFileVersionLS);
    ABuild   := LoWord(pVerValue^.dwFileVersionLS);
  finally
    FreeMem(pVerInfo, ASize);
  end;
end;

class function TVersion.GetFileVersion(const AFileName : String; const AIncludeBuild : Boolean = False) : String;
var AMajor   : Cardinal;
    AMinor   : Cardinal;
    ARelease : Cardinal;
    ABuild   : Cardinal;
begin
  GetFileVersion(AFileName, AMajor, AMinor, ARelease, ABuild);

  result := Format('%d.%d.%d', [AMajor, AMinor, ARelease]);

  if AIncludeBuild then
    result := Format('%s build %d', [result, ABuild]);
end;

end.
