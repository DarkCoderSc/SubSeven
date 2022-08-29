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

// TODO: Use variants instead of overloading Read / Write methods.

unit S7SettingHandler;

interface

uses System.Classes, XSuperObject, Generics.Collections;

type
  TSettingHandlerEvent = (
    sheLoaded,
    sheSaved
  );

  TOnSettingHandlerNotify = procedure(Sender : TObject; const AEvent : TSettingHandlerEvent) of object;

  TS7SettingHandler = class(TComponent)
  private
    FOutputPath  : String;
    FJsonSetting : ISuperObject;
    FNotifiers   : TList<TOnSettingHandlerNotify>;

    {@M}
    procedure SetupJson();
    function GetOrCreateMainGroup(const AGroupName : String) : ISuperObject;
    procedure BroadcastEvent(const AEvent : TSettingHandlerEvent);
  public
    {@M}
    procedure Save();
    procedure Load();
    procedure Loaded(); override;

    procedure UpdateOrCreateNode(const ANodeName : String; ANode : ISuperObject);
    procedure UpdateOrCreateArray(const AArrayName : String; AArray : ISuperArray);

    function GetNode(const ANodeName : String) : ISuperObject;
    function GetArray(const AArrayName : String) : ISuperArray;

    procedure Write(const AName, AValue : String; const AGroupName : String = ''); overload;
    procedure Write(const AName : String; AValue : Int64; const AGroupName : String = ''); overload;
    procedure Write(const AName : String; AValue : Boolean; const AGroupName : String = ''); overload;

    function Read(const AName, ADefault : String; const AGroupName : String = '') : String; overload;
    function Read(const AName : String; ADefault : Int64; const AGroupName : String = '') : Int64; overload;
    function Read(const AName : String; ADefault : Boolean; const AGroupName : String = '') : Boolean; overload;

    procedure RegisterCallback(const ACallback : TOnSettingHandlerNotify);
    procedure UnregisterCallback(const ACallback : TOnSettingHandlerNotify);

    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property OutputPath : String read FOutputPath write FOutputPath;
  end;

implementation

uses System.SysUtils, Winapi.Windows;

{ TS7SettingHandler.BroadcastEvent }

procedure TS7SettingHandler.BroadcastEvent(const AEvent : TSettingHandlerEvent);
var ACallback : TOnSettingHandlerNotify;
begin
  for ACallback in FNotifiers do
    ACallback(self, AEvent);
end;

{ TS7SettingHandler.RegisterCallback }

procedure TS7SettingHandler.RegisterCallback(const ACallback : TOnSettingHandlerNotify);
begin
  if Assigned(ACallback) then
    FNotifiers.Add(ACallback);
end;

{ TS7SettingHandler.UnregisterCallback }

procedure TS7SettingHandler.UnregisterCallback(const ACallback : TOnSettingHandlerNotify);
begin
  if Assigned(ACallback) then
    FNotifiers.Remove(ACallback);
end;

{ TS7SettingHandler.SetupJson }

procedure TS7SettingHandler.SetupJson();
begin
  if not Assigned(FJsonSetting) then
    FJsonSetting := TSuperObject.Create();

  if not FJsonSetting.Contains('main') then
    FJsonSetting.O['main'] := SO();
end;

{ TS7SettingHandler.GetOrCreateMainGroup }

function TS7SettingHandler.GetOrCreateMainGroup(const AGroupName : String) : ISuperObject;
var AMain : ISuperObject;
begin
  self.SetupJson();
  ///

  AMain := FJsonSetting.O['main'];

  if (AGroupName <> '') then begin
    if AMain.Contains(AGroupName) then
      result := AMain.O[AGroupName]
    else begin
      result := TSuperObject.Create();

      AMain.O[AGroupName] := result;
    end;
  end else
    result := AMain;
end;

{ TS7SettingHandler.Write }

procedure TS7SettingHandler.Write(const AName, AValue : String; const AGroupName : String = '');
var AGroup : ISuperObject;
begin
  AGroup := self.GetOrCreateMainGroup(AGroupName);

  AGroup.S[AName] := AValue;
end;

{ TS7SettingHandler.Write }

procedure TS7SettingHandler.Write(const AName : String; AValue : Int64; const AGroupName : String = '');
var AGroup : ISuperObject;
begin
  AGroup := self.GetOrCreateMainGroup(AGroupName);

  AGroup.I[AName] := AValue;
end;

{ TS7SettingHandler.Write }

procedure TS7SettingHandler.Write(const AName : String; AValue : Boolean; const AGroupName : String = '');
var AGroup : ISuperObject;
begin
  AGroup := self.GetOrCreateMainGroup(AGroupName);

  AGroup.B[AName] := AValue;
end;

{ TS7SettingHandler.Read }

function TS7SettingHandler.Read(const AName, ADefault : String; const AGroupName : String = '') : String;
var AGroup : ISuperObject;
begin
  result := ADefault;
  ///

  AGroup := self.GetOrCreateMainGroup(AGroupName);

  if AGroup.Contains(AName) then
    try
      result := AGroup.S[AName];
    except

    end;
end;

{ TS7SettingHandler.Read }

function TS7SettingHandler.Read(const AName : String; ADefault : Int64; const AGroupName : String = '') : Int64;
var AGroup : ISuperObject;
begin
  result := ADefault;
  ///

  AGroup := self.GetOrCreateMainGroup(AGroupName);

  if AGroup.Contains(AName) then
    try
      result := AGroup.I[AName];
    except

    end;
end;

{ TS7SettingHandler.Read }

function TS7SettingHandler.Read(const AName : String; ADefault : Boolean; const AGroupName : String = '') : Boolean;
var AGroup : ISuperObject;
begin
  result := ADefault;
  ///

  AGroup := self.GetOrCreateMainGroup(AGroupName);

  if AGroup.Contains(AName) then
    try
      result := AGroup.B[AName];
    except

    end;
end;


{ TS7SettingHandler.Loaded }

procedure TS7SettingHandler.Loaded();
begin
  inherited Loaded();
  ///

  if (csDesigning in ComponentState) then
    Exit;

  if FOutputPath <> '' then
    self.Load();
end;

{ TS7SettingHandler.Save }

procedure TS7SettingHandler.Load();
var AFileStream : TFileStream;
    ABytes      : TBytes;
begin
  self.SetupJson();
  ///

  if not FileExists(FOutputPath) then
    Exit();

  AFileStream := TFileStream.Create(FOutputPath, fmOpenRead);
  try
    if AFileStream.Size = 0 then
      Exit();
    ///

    AFileStream.Position := 0;

    SetLength(ABytes, AFileStream.Size);
    try
      AFileStream.Read(ABytes[0], AFileStream.Size);
      try
        FJsonSetting := TSuperObject.Create(TEncoding.Unicode.GetString(ABytes));
      except

      end;
    finally
      SetLength(ABytes, 0);
    end;
  finally
    if Assigned(AFileStream) then
      FreeAndNil(AFileStream);
  end;

  ///
  self.BroadcastEvent(sheLoaded);
end;

{ TS7SettingHandler.Save }

procedure TS7SettingHandler.Save();
var AFileStream : TFileStream;
    AString     : String;
begin
  self.SetupJson();
  ///

  AFileStream := TFileStream.Create(
    FOutputPath,
    fmCreate or
    fmShareDenyWrite
  );
  try
    AString := FJsonSetting.AsJson(True);

    AFileStream.Write(AString[1], Length(AString) * SizeOf(WideChar));
  finally
    if Assigned(AFileStream) then
      FreeAndNil(AFileStream);
  end;

  ///
  self.BroadcastEvent(sheSaved);
end;

{ TS7SettingHandler.Create }

constructor TS7SettingHandler.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FOutputPath  := '';
  FJsonSetting := TSuperObject.Create();

  FNotifiers   := TList<TOnSettingHandlerNotify>.Create();
end;

destructor TS7SettingHandler.Destroy();
begin
  if not (csDesigning in ComponentState) then
    self.Save();

  if Assigned(FNotifiers) then
    FreeAndNil(FNotifiers);

  ///
  inherited Destroy();
end;

{ TS7SettingHandler.UpdateOrCreateNode }

procedure TS7SettingHandler.UpdateOrCreateNode(const ANodeName : String; ANode : ISuperObject);
begin
  if not Assigned(ANode) then
    ANode := TSuperObject.Create();

  self.SetupJson();

  FJsonSetting.O[ANodeName] := ANode;
end;

{ TS7SettingHandler.UpdateOrCreateArray }

procedure TS7SettingHandler.UpdateOrCreateArray(const AArrayName : String; AArray : ISuperArray);
begin
  if not Assigned(AArray) then
    AArray := TSuperArray.Create();

  self.SetupJson();

  FJsonSetting.A[AArrayName] := AArray;
end;

{ TS7SettingHandler.GetNode }

function TS7SettingHandler.GetNode(const ANodeName : String) : ISuperObject;
begin
  result := nil;

  self.SetupJson();

  if FJsonSetting.Contains(ANodeName) then
    result := FJsonSetting.O[ANodeName];
end;

{ TS7SettingHandler.GetArray }

function TS7SettingHandler.GetArray(const AArrayName : String) : ISuperArray;
begin
  result := nil;

  self.SetupJson();

  if FJsonSetting.Contains(AArrayName) then
    result := FJsonSetting.A[AArrayName];
end;

end.
