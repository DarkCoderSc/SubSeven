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

unit Sub7.Core.Windows.Information;

interface

uses WinAPI.Windows, System.SysUtils;

type
  TSidNameUse = (
                    SidTypeUser,
                    SidTypeGroup,
                    SidTypeDomain,
                    SidTypeAlias,
                    SidTypeWellKnownGroup,
                    SidTypeDeletedAccount,
                    SidTypeInvalid,
                    SidTypeUnknown,
                    SidTypeComputer,
                    SidTypeLabel,
                    SidTypeLogonSession
  );

function GetWindowsDirectory() : string;
function IsDir(const APath : String) : Boolean;
function GetUserSidType(const AUserName : String; ASidType : TSidNameUse = SidTypeUser) : String;

implementation

{ GetUserSidType }

function GetUserSidType(const AUserName : String; ASidType : TSidNameUse = SidTypeUser) : String;
var ptrSID         : PSID;
    ASidSize       : Cardinal;
    ARefDomainSize : Cardinal;
    ASidNameUse    : SID_NAME_USE;
    ARefDomain     : String;
    ARet           : Boolean;
    ASid           : PWideChar;
begin
  result := '';
  ///

  ASidSize       := 0;
  ARefDomainSize := 0;

  ASidNameUse := Cardinal(ASidType);

  LookupAccountNameW(nil, PWideChar(AUserName), nil, ASidSize, nil, ARefDomainSize, ASidNameUse);

  GetMem(ptrSID, ASidSize);
  try
    SetLength(ARefDomain, ARefDomainSize);

    ARet := LookupAccountNameW(
                                  nil,
                                  PWideChar(AUserName),
                                  ptrSID,
                                  ASidSize,
                                  PWideChar(ARefDomain),
                                  ARefDomainSize,
                                  ASidNameUse
    );

    if ARet then begin
      ConvertSidToStringSidW(ptrSID, ASid);
      try
        result := String(ASid);
      finally
        LocalFree(ASid);
      end;
    end;
  finally
    FreeMem(ptrSID, ASidSize);
  end;
end;

{-------------------------------------------------------------------------------
  Retrieve Windows Directory
-------------------------------------------------------------------------------}
function GetWindowsDirectory() : string;
var ALen  : Cardinal;
begin
  SetLength(result, MAX_PATH);

  ALen := WinAPI.Windows.GetWindowsDirectory(@result[1], MAX_PATH);

  SetLength(result, ALen);
  if ALen > MAX_PATH then
    WinAPI.Windows.GetWindowsDirectory(@result[1], ALen);

  ///
  result := IncludeTrailingPathDelimiter(result);
end;

{-------------------------------------------------------------------------------
  Best way to know if a file is a directory
-------------------------------------------------------------------------------}
function IsDir(const APath : String) : Boolean;
var ASearchRec : TSearchRec;
begin
  result := False;
  ///

  if (FindFirst(APath, faAnyFile, ASearchRec) = 0) then begin
    try
      result := ((ASearchRec.Attr and faDirectory) <> 0);
    finally
      FindClose(ASearchRec);
    end;
  end;
end;

end.
