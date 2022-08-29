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

unit S7Validators;

interface

uses S7Types, VCL.Controls, System.Classes, S7Panel;

function IsValidIpAddress(const AIP : String) : Boolean;
function IsValidHost(const AHost : String) : Boolean;
function IsValidNetworkAddress(const AValue : String) : Boolean;

function IsValidPort(const APort : Integer) : Boolean; overload;
function IsValidPort(const APort : String) : Boolean; overload

function Validate(const AInput : String; const AValidators : TValidators) : Boolean;

implementation

uses System.RegularExpressions, System.SysUtils, S7Edit, S7ComboBox, Winapi.Windows;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function Validate(const AInput : String; const AValidators : TValidators) : Boolean;
begin
  result := False;
  ///

  { Filled }
  if reqFilled in AValidators then
    if Length(Trim(AInput)) = 0 then
      Exit();

  { Ip Address }
  if reqIpAddress in AValidators then
    if not IsValidIpAddress(AInput) then
      Exit();

  { Host }
  if reqHost in AValidators then
    if not IsValidHost(AInput) then
      Exit();

  { TCP / UDP Port }
  if reqNetPort in AValidators then
    if not IsValidPort(AInput) then
      Exit();

  ///
  result := True;
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function IsValidIpAddress(const AIP : String) : Boolean;
begin
  result := TRegEx.IsMatch(AIP, '^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$');
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function IsValidHost(const AHost : String) : Boolean;
begin
  result := TRegEx.IsMatch(AHost, '^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])$');
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function IsValidNetworkAddress(const AValue : String) : Boolean;
begin
  result := IsValidIpAddress(AValue) or
            IsValidHost(AValue);
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function IsValidPort(const APort : Integer) : Boolean;
begin
  result := (APort >= Low(word)) and (APort <= High(word));
end;

{-------------------------------------------------------------------------------

-------------------------------------------------------------------------------}
function IsValidPort(const APort : String) : Boolean;
var AValue : Integer;
begin
  result := False;
  if not TryStrToInt(APort, AValue) then
    Exit();
  ///

  result := IsValidPort(AValue);
end;

end.
