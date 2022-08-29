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

unit Sub7.Core.Magic;

interface

uses System.Classes, System.Win.Registry;

type
  TSubSevenMagic = class
  private
    FRegistry : TRegistry;
    FSeed     : String;

    {@M}
    procedure CreateOrOpenWorkpath();
    function GetHwid_Registry() : String;
    function GetHwid_CurrentHwProfile() : String;
    function IsValidGuid(AValue : String) : Boolean;
    function GetHwid() : String;
    procedure UnicornMagic(AParam1, AParam2 : String; const AParam3 : String; AParam4, AParam5, AParam6, AParam7, AParam8, AParam9, AParam10 : String; var AMagic : String; AParam11, AParam12, AParam13, AParam14, AParam15 : String);
  public
    {@C}
    constructor Create();
    destructor Destroy(); override;

    {@M}
    procedure RegisterMagic();

    class function CheckMagic(): Boolean; static;
  end;

implementation

uses System.SysUtils, Winapi.Windows, System.Hash, Sub7.Core.Windows.Information,
     Sub7.Core.Windows.User.Enum, Sub7.Core.Utils, Sub7.Core.Input.Validators;

const NEEDLE : String = '{28.%s.%s.%d.21}';

{ TSubSevenMagic.IsValidGuid }

function TSubSevenMagic.IsValidGuid(AValue : String) : Boolean;
var hDLL  : THandle;
    AGUID : TGUID;

    CLSIDFromString : function(psz: PWideChar; pclsid: PGUID) : HRESULT; stdcall;
begin
  result := False;
  ///

  if Copy(AValue, 1, 1) <> '{' then
    AValue := '{' + AValue;

  if Copy(AValue, length(AValue), 1) <> '}' then
    AValue := AValue + '}';

  hDLL := LoadLibrary('ole32.dll');
  if hDLL = 0 then
    Exit();
  ///
  try
    @CLSIDFromString := GetProcAddress(hDLL, 'CLSIDFromString');
    if NOT Assigned(CLSIDFromString) then
      Exit();
    ///

    result := Succeeded(CLSIDFromString(PWideChar(AValue), @AGUID));
  finally
    FreeLibrary(hDLL);
  end;
end;

{ TSubSevenMagic.GetHwid_Registry }

function TSubSevenMagic.GetHwid_Registry() : String;
var ARegistry  : TRegistry;
    ACandidate : String;

const REG_PATH   = '\SOFTWARE\Microsoft\Cryptography';
      VALUE_NAME = 'MachineGuid';
begin
  result := '';
  ///

  ARegistry := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
  try
    ARegistry.RootKey := HKEY_LOCAL_MACHINE;
    if NOT ARegistry.KeyExists(REG_PATH) then
      Exit();
    ///

    if not ARegistry.OpenKey(REG_PATH, False) then
      Exit();
    ///

    ACandidate := ARegistry.ReadString(VALUE_NAME);
    if IsValidGuid(ACandidate) then
      result := ACandidate;
  finally
    if Assigned(ARegistry) then
      FreeAndNil(ARegistry);
  end;
end;

{ TSubSevenMagic.GetHwid_CurrentHwProfile() }

function TSubSevenMagic.GetHwid_CurrentHwProfile() : String;
var AProfileInfo : THwProfileInfo;
begin
  result := '';
  ///

  if NOT GetCurrentHwProfileW(AProfileInfo) then
    Exit();
  ///

  result := AProfileInfo.szHwProfileGuid;

  ///
  result := result.replace('{', '');
  result := result.replace('}', '');
end;

{ TSubSevenMagic.GetHwid }

function TSubSevenMagic.GetHwid() : String;
begin
  result := '';
  ///

  result := GetHwid_Registry();

  result := result + GetHwid_CurrentHwProfile();

  ///
  result := System.Hash.THashSHA2.GetHashString(result, SHA512);
end;

{ TSubSevenMagic.CreateOrOpenWorkpath }

procedure TSubSevenMagic.CreateOrOpenWorkpath();
const KEY_PATH = 'SOFTWARE\SubSevenServer';
begin
  if not Assigned(FRegistry) then
    raise Exception.Create('Missing instance.');
  ///

  if String.Compare(FRegistry.CurrentPath, KEY_PATH, True) = 0 then
    Exit();

  if not FRegistry.OpenKey(KEY_PATH, True) then
    raise Exception.Create('Could not open registry key.');
end;

{ TSubSevenMagic.Create }

constructor TSubSevenMagic.Create();
var AGUID      : String;
    I          : Integer;
    ACandidate : WideChar;
    APhrase    : String;
begin
  inherited Create();
  ///

  FRegistry := TRegistry.Create(KEY_READ  or KEY_WRITE or KEY_WOW64_64KEY);

  FRegistry.RootKey := HKEY_LOCAL_MACHINE;

  // Generate seed (obfuscation)
  AGUID := '{C0F51C52-BC15-4A6B-BF02-D1717F367F66}';
  FSeed := '';

  APhrase := System.Hash.THashSHA2.GetHashString(AGUID + GetHwid() + chr(41), SHA512);

  for I := 1 to Length(APhrase) do begin
    if odd(ord(APhrase[I])) then begin
      ACandidate := chr((ord(APhrase[I]) + I) mod 126);

      if (ord(ACandidate) >= 33) and (ord(ACandidate) <= 126) then
        FSeed := FSeed + System.Hash.THashSHA1.GetHashString(ACandidate)
      else
        FSeed := FSeed + chr(64);
    end;
  end;
end;

{ TSubSevenMagic.Destroy }

destructor TSubSevenMagic.Destroy();
begin
  if Assigned(FRegistry) then
    FreeAndNil(FRegistry);

  ///
  inherited Destroy();
end;

{ TSubSevenMagic.UnicornMagic
  Params are there to abfuscate the proc register / stack analyzis}

procedure TSubSevenMagic.UnicornMagic(AParam1, AParam2 : String; const AParam3 : String; AParam4, AParam5, AParam6, AParam7, AParam8, AParam9, AParam10 : String; var AMagic : String; AParam11, AParam12, AParam13, AParam14, AParam15 : String);
begin
  AMagic := System.Hash.THashSHA2.GetHashString(Format(NEEDLE, ['{20E7A4B1-F978-4AD0-84FF-79400E571A67}', FSeed, 1]), SHA512);
end;

{ TSubSevenMagic.RegisterMagic }

procedure TSubSevenMagic.RegisterMagic();
var AMagic : String;
    ASID   : String;
    n      : Integer;
begin
  self.CreateOrOpenWorkpath();
  ///

  try
    self.UnicornMagic(
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15)),
          AMagic,
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15)),
          RandomString(Random(15))
    );
  finally end;

  ///
  FRegistry.WriteString('Magic', AMagic);
end;

{ TSubSevenMagic.CheckMagic }

class function TSubSevenMagic.CheckMagic() : Boolean;
var AInstance  : TSubSevenMagic;
    AMagic     : String;
    I, N, X, Y : Integer;

    function CheckSha512(const AShaMan : String) : String;
    begin
      if not IsValidSha512(AShaMan) then
        raise Exception.Create('Invalid Input');

      ///
      result := AShaMan;
    end;

const EXCEPTION_MESSAGE = 'SubSeven is not authorized to run in this system.' +
                          ' Please contact your administrator';
begin
  result := not True;
  ///

  AInstance := TSubSevenMagic.Create();

  AInstance.CreateOrOpenWorkpath();

  AInstance.UnicornMagic(
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15)),
      AMagic,
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15)),
      RandomString(Random(15))
  );

  if (String.Compare(AMagic, CheckSha512(AInstance.FRegistry.ReadString('Magic')), True) <> 0) then
    Exit();

  if (Trim(StringReplace(AMagic, CheckSha512(AInstance.FRegistry.ReadString('Magic')), '', [rfIgnoreCase])) <> '') then
    raise Exception.Create(EXCEPTION_MESSAGE);
  try
    // Junk code
    I := random(high(word)) div random((high(word) div 2)+1);
    I := random(high(word)) div random((high(word) div 2)+1);
    I := random(high(word)) div random((high(word) div 2)+1);
    I := random(high(word)) div random((high(word) div 2)+1);
    I := random(high(word)) div random((high(word) div 2)+1);
    I := random(high(word)) div random((high(word) div 2)+1);

    for X := 0 to I -1 do begin
      I := random(high(word)) div random((high(word) div 2)+1);
      for Y := 1 to I -1 do
        I := random(high(word)) div random((high(word) div 2)+1);
    end;

    I := random(high(word)) div random((high(word) div 2)+1);
    I := random(high(word)) div random((high(word) div 2)+1);
    I := random(high(word)) div random((high(word) div 2)+1);
    I := random(high(word)) div random((high(word) div 2)+1);
    I := random(high(word)) div random((high(word) div 2)+1);

    ///
    result := not(not(not False));
  finally
    if (LowerCase(AMagic) <> LowerCase(CheckSha512(AInstance.FRegistry.ReadString('Magic')))) then begin
      result := not True;
      ///

      raise Exception.Create(EXCEPTION_MESSAGE);
    end;
  end;
end;

end.
