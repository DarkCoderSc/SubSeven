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


unit Sub7.Core.Input.Validators;

interface

type
  TPasswordCriteria = (
                        pcMinimum10Characters,
                        pcOneUpperOrLower,
                        pcOneSymbol,
                        pcOneNumber
  );

  TPasswordCriterias = set of TPasswordCriteria;

function IsValidIpAddress(const AIP : String) : Boolean;
function IsValidHost(const AHost : String) : Boolean;
function IsValidNetworkAddress(const AValue : String) : Boolean;

function IsValidPort(const APort : Integer) : Boolean; overload;
function IsValidPort(const APort : String) : Boolean; overload

function IsValidSha512(const ASha512 : String) : Boolean;
function IsValidSha512Fingerprint(const AFingerprint : String) : Boolean;

procedure CalcPasswordComplexity(const APassword : String; var AResult : TPasswordCriterias);
function CheckPassword(const APassword : String; const AValidators : TPasswordCriterias) : Boolean;

implementation

uses System.RegularExpressions, System.SysUtils, Winapi.Windows;

{ _.IsValidSha512 }

function IsValidSha512(const ASha512 : String) : Boolean;
begin
  result := TRegEx.IsMatch(ASha512, '^([0-9a-fA-F]{128})$');
end;

{ _.IsValidSha512Fingerprint }

function IsValidSha512Fingerprint(const AFingerprint : String) : Boolean;
begin
  result := TRegEx.IsMatch(AFingerprint, '^([0-9a-fA-F]{2}:){63}[0-9a-fA-F]{2}$');
end;

{-------------------------------------------------------------------------------
  Check password complexity
-------------------------------------------------------------------------------}
procedure CalcPasswordComplexity(const APassword : String; var AResult : TPasswordCriterias);
begin
  AResult := [];
  ///

  if TRegEx.IsMatch(APassword, '[0-9]') then
    AResult := AResult + [pcOneNumber];

  if (TRegEx.IsMatch(APassword, '[A-Z]')) and (TRegEx.IsMatch(APassword, '[a-z]')) then
    AResult := AResult + [pcOneUpperOrLower];

  if TRegEx.IsMatch(APassword, '[-!$%^&*()_+|~=`{}\[\]:";''<>?,.\/@€#²]') then
    AResult := AResult + [pcOneSymbol];

  if TRegEx.IsMatch(APassword, '^.{10,}$') then
    AResult := AResult + [pcMinimum10Characters];
end;

{-------------------------------------------------------------------------------
  Validate Password
-------------------------------------------------------------------------------}
function CheckPassword(const APassword : String; const AValidators : TPasswordCriterias) : Boolean;
var AResult : TPasswordCriterias;
begin
  CalcPasswordComplexity(APassword, AResult);

  ///
  result := (AValidators = AResult);
end;

{-------------------------------------------------------------------------------
  Validate Ip Address
-------------------------------------------------------------------------------}
function IsValidIpAddress(const AIP : String) : Boolean;
begin
  result := TRegEx.IsMatch(Trim(AIP), '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$');
end;

{-------------------------------------------------------------------------------
  Validate Host
-------------------------------------------------------------------------------}
function IsValidHost(const AHost : String) : Boolean;
begin
  result := TRegEx.IsMatch(Trim(AHost), '^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])$');
end;

{-------------------------------------------------------------------------------
  Validate Network Address (IP / Host)
-------------------------------------------------------------------------------}
function IsValidNetworkAddress(const AValue : String) : Boolean;
begin
  result := IsValidIpAddress(AValue) or
            IsValidHost(AValue);
end;

{-------------------------------------------------------------------------------
  Validate TCP/IP Port
-------------------------------------------------------------------------------}

function IsValidPort(const APort : Integer) : Boolean;
begin
  result := (APort >= Low(word)) and (APort <= High(word));
end;

function IsValidPort(const APort : String) : Boolean;
var AValue : Integer;
begin
  result := False;
  if not TryStrToInt(Trim(APort), AValue) then
    Exit();
  ///

  result := IsValidPort(AValue);
end;

end.
