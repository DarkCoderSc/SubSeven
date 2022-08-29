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
unit Sub7.Core.Setting.Registry;

interface

uses System.Classes, System.Win.Registry, System.SysUtils, WinAPI.Windows,
     Sub7.Core.Crypto.RC4, Generics.Collections, Messages,
     Vcl.StdCtrls, Vcl.Samples.Spin;

type 
  TSettings = class
  private
    FRegistry    : TRegistry;
    FKeyPath     : String;
    FCipher      : TRC4;
    FProjectName : String;
    FCategory    : String;

    {@M}
    function Setup() : Boolean;
  public
    {@C}
    constructor Create(const AHIVE : HKEY; const AProjectName : String);
    destructor Destroy(); override;

    {@M}
    function Write(const AName : String; AValue : String) : Boolean; overload;
    procedure Write(const AName : String; const AValue : Integer); overload;
    procedure Write(const AName : String; const AValue : Boolean); overload;

    procedure Write(AComboBox : TComboBox); overload;
    procedure Write(AEdit : TEdit); overload;
    procedure Write(ASpinEdit : TSpinEdit); overload;
    procedure Write(ACheckBox : TCheckBox); overload;

    function Read(const AName, ADefault : String) : String; overload;
    function Read(const AName : String; const ADefault : Integer) : Integer; overload;
    function Read(const AName : String; const ADefault : Boolean) : Boolean; overload;

    procedure Read(AComboBox : TComboBox; const ADefault : String); overload;
    procedure Read(AComboBox : TComboBox; const ADefault : Integer); overload;
    procedure Read(AEdit : TEdit; const ADefault : String); overload;
    procedure Read(ASpinEdit : TSpinEdit; const ADefault : Integer); overload;
    procedure Read(ACheckBox : TCheckBox; const ADefault : Boolean); overload;

    function Exists(AName : String) : Boolean;
    function Delete(AName : String) : Boolean;
    function GetWorkPath() : String;

    {@G/S}
    property Category : String read FCategory write FCategory;
  end;

  const ROOT_KEY = '\SOFTWARE';

implementation

uses Sub7.Core.Utils, System.Hash;

{-------------------------------------------------------------------------------
  Get Cryptography Key (Unic Windows Identifier)
-------------------------------------------------------------------------------}
function GetMachineGuid() : String;
var ARegistry  : TRegistry;
    I          : Integer;
    AGUID      : String;
    ACandidate : Char;
begin
  result := '';
  ///

  ARegistry := TRegistry.Create(
                                  KEY_READ  or
                                  KEY_WRITE or
                                  KEY_WOW64_64KEY
  );
  try
    ARegistry.RootKey := HKEY_LOCAL_MACHINE;
    ///

    {
      Machine GUID Obfuscation (Dirty)
    }
    AGUID := '{C0F51C52-BC15-4A6B-BF02-D1717F367F66}';
    for I := 1 to Length(AGUID) do
      if odd(ord(AGUID[I])) then begin
        ACandidate := chr((ord(AGUID[I]) + I) mod 126);

        if (ord(ACandidate) >= 33) and (ord(ACandidate) <= 126) then
          result := result + ACandidate
        else
          result := result + chr(64);
      end;

    {
      Read Machine GUID
    }
    if not ARegistry.OpenKeyReadOnly('\SOFTWARE\Microsoft\Cryptography') then
      Exit();

    ///
    result := System.Hash.THashSHA2.GetHashString(result + ARegistry.ReadString('MachineGuid'), SHA512);
  finally
    if Assigned(ARegistry) then
      FreeAndNil(ARegistry);
  end;
end;

{-------------------------------------------------------------------------------
  Get Work Key Path
-------------------------------------------------------------------------------}
function TSettings.GetWorkPath() : String;
begin
  if Length(Trim(FCategory)) > 0 then
    result := Format('%s\%s\%s', [ROOT_KEY, FProjectName, FCategory])
  else
    result := Format('%s\%s', [ROOT_KEY, FProjectName]);
end;

{-------------------------------------------------------------------------------
  Initialize Registry Context
-------------------------------------------------------------------------------}
function TSettings.Setup() : Boolean;
var AWorkPath : String;
begin
  result := False;
  ///

  if NOT Assigned(FRegistry) then
    Exit();
  ///

  AWorkPath := self.GetWorkPath();

  if String.Compare(AWorkPath, FRegistry.CurrentPath, True) <> 0 then
    FRegistry.CloseKey();

  ///
  result := FRegistry.OpenKey(AWorkPath, True);
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSettings.Create(const AHIVE : HKEY; const AProjectName : String);
var ASeed     : String;
    APassword : String;
    i         : Integer;
    AGUID     : String;
begin
  FRegistry := TRegistry.Create(
                                  KEY_READ  or
                                  KEY_WRITE or
                                  KEY_WOW64_64KEY
  );

  FRegistry.RootKey := AHIVE;

  FProjectName := AProjectName;
  FCategory    := '';
  FCipher      := nil;

  {
    Setup Registry Work Directory
  }
  self.Setup();

  {
    Setup Encryption
  }
  if not FRegistry.ValueExists('seed') then begin
    ASeed := RandomString(255);

    FRegistry.WriteString('seed', ASeed);
  end else
    ASeed := FRegistry.ReadString('seed');

  AGUID := GetMachineGuid();
  for i := 0 to 5 do
    APassword := System.Hash.THashSHA2.GetHashString(AGUID + ASeed, SHA512);

  FCipher := TRC4.Create(APassword);

  FillChar(APassword[1], (Length(APassword) * SizeOf(WideChar)), #0);
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TSettings.Destroy();
begin
  if Assigned(FRegistry) then
    FreeAndNil(FRegistry);

  if Assigned(FCipher) then
    FreeAndNil(FCipher);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Write to registry.
-------------------------------------------------------------------------------}

function TSettings.Write(const AName : String; AValue : String) : Boolean;
begin
  result := false;
  ///

  if NOT self.Setup() then
    Exit();
  ///

  if Length(AValue) = 0 then
    self.Delete(AName)
  else begin
    if Assigned(FCipher) then begin
      AValue := FCipher.Encrypt(AValue, True);

      if (Length(AValue) <= 0) then
        Exit();
    end;

    FRegistry.WriteString(AName, AValue);
  end;

  ///
  result := True;
end;

procedure TSettings.Write(const AName : String; const AValue : Integer);
begin
  self.Write(AName, IntToStr(AValue));
end;

procedure TSettings.Write(const AName : String; const AValue : Boolean);
var ABoolean : String;
begin
  if AValue then
    ABoolean := 'Y'
  else
    ABoolean := 'N';
  ///

  self.Write(AName, ABoolean);
end;

{-------------------------------------------------------------------------------
  Write (VCL Wrappes)
-------------------------------------------------------------------------------}

procedure TSettings.Write(AComboBox : TComboBox);
begin
  if not Assigned(AComboBox) then
    Exit();

  if AComboBox.Style = csDropDownList then
    self.Write(AComboBox.Name, AComboBox.ItemIndex)
  else
    self.Write(AComboBox.Name, AComboBox.Text);
end;

procedure TSettings.Write(AEdit : TEdit);
begin
  if not Assigned(AEdit) then
    Exit();

  self.Write(AEdit.Name, AEdit.Text);
end;

procedure TSettings.Write(ASpinEdit : TSpinEdit);
begin
  if not Assigned(ASpinEdit) then
    Exit();

  self.Write(ASpinEdit.Name, ASpinEdit.Value);
end;

procedure TSettings.Write(ACheckBox : TCheckBox);
begin
  if not Assigned(ACheckBox) then
    Exit();

  self.Write(ACheckBox.Name, ACheckBox.Checked);
end;

{-------------------------------------------------------------------------------
  Read value from registry.
-------------------------------------------------------------------------------}

function TSettings.Read(const AName, ADefault : String) : String;
var AValue : String;
begin
  result := ADefault;
  ///

  if NOT Self.Setup() then
    Exit();

  AValue := FRegistry.ReadString(AName);

  if NOT AValue.IsEmpty then begin
    AValue := FCipher.Decrypt(AValue);
    if (Length(AValue) > 0) then
      result := AValue;
  end;
end;

function TSettings.Read(const AName : String; const ADefault : Integer) : Integer;
var AValue   : String;
    AInteger : Integer;
begin
  result := ADefault;
  ///

  AValue := self.Read(AName, '');

  if TryStrToInt(AValue, AInteger) then
    result := AInteger;
end;

function TSettings.Read(const AName : String; const ADefault : Boolean) : Boolean;
var AValue : String;
begin
  result := ADefault;
  ///

  AValue := self.Read(AName, '');

  if AValue <> '' then
    if AValue = 'Y' then
      result := True
    else
      result := False;
end;

{-------------------------------------------------------------------------------
  Read (VCL Wrappes)
-------------------------------------------------------------------------------}

procedure TSettings.Read(AComboBox : TComboBox; const ADefault : String);
begin
  if not Assigned(AComboBox) then
    Exit();
  ///

  AComboBox.Text := self.Read(AComboBox.Name, ADefault);
end;

procedure TSettings.Read(AComboBox : TComboBox; const ADefault : Integer);
var AValue : Integer;
begin
  if not Assigned(AComboBox) then
    Exit();
  ///

  AValue := self.Read(AComboBox.Name, ADefault);

  if AValue > AComboBox.Items.Count-1 then
    AValue := -1;

  AComboBox.ItemIndex := AValue;
end;

procedure TSettings.Read(AEdit : TEdit; const ADefault : String);
begin
  if not Assigned(AEdit) then
    Exit();
  ///

  AEdit.Text := self.Read(AEdit.Name, ADefault);
end;

procedure TSettings.Read(ASpinEdit : TSpinEdit; const ADefault : Integer);
begin
  if not Assigned(ASpinEdit) then
    Exit();
  ///

  ASpinEdit.Value := self.Read(ASpinEdit.Name, ADefault);
end;

procedure TSettings.Read(ACheckBox : TCheckBox; const ADefault : Boolean);
begin
  if not Assigned(ACheckBox) then
    Exit();
  ///

  ACheckBox.Checked := self.Read(ACheckBox.Name, ADefault);
end;

{-------------------------------------------------------------------------------
  Do Value Exists
-------------------------------------------------------------------------------}
function TSettings.Exists(AName : String) : Boolean;
begin
  result := FRegistry.ValueExists(AName);
end;

{-------------------------------------------------------------------------------
  Delete value from registry
-------------------------------------------------------------------------------}
function TSettings.Delete(AName : String) : Boolean;
begin
  result := False;
  ///

  if NOT self.Setup() then
    Exit();

  if NOT self.Exists(AName) then
    Exit();

  ///
  result := FRegistry.DeleteValue(AName);
end;

end.
