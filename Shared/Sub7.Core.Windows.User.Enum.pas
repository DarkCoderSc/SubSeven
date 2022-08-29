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

// TODO: Faire un Objet User Information avec bcp plus d'infos sur chaque user

unit Sub7.Core.Windows.User.Enum;

interface

uses Winapi.Windows, System.Classes, Sub7.Core.OOP.Interfaces, Generics.Collections,
     XSuperObject;

type
  TS7EnumUser = class(TInterfacedObject, IS7Serializable)
  private
    FItems : TList<String>;

    {@M}
    procedure Enum();
  public
    {@C}
    constructor Create(const ARefresh : Boolean = False); overload;
    constructor Create(const ASerializedObject : ISuperObject = nil); overload;
    destructor Destroy(); override;

    {@M}
    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);
    procedure Clear();

    {@G}
    property Items : TList<String> read FItems;
  end;

const USER_PRIV_ADMIN       = 2;
      MAX_PREFERRED_LENGTH  = $FFFFFFFF;
      NERR_Success          = 0;
      FILTER_NORMAL_ACCOUNT = 2;
      NERR_BufTooSmall      = 2123;
      NERR_InvalidComputer  = 2351;

type
  NetApiStatus = DWORD;

  USER_INFO_2 = record
    usri2_name           : LPWSTR;
    usri2_password       : LPWSTR;
    usri2_password_age   : DWORD;
    usri2_priv           : DWORD;
    usri2_home_dir       : LPWSTR;
    usri2_comment        : LPWSTR;
    usri2_flags          : DWORD;
    usri2_script_path    : LPWSTR;
    usri2_auth_flags     : DWORD;
    usri2_full_name      : LPWSTR;
    usri2_usr_comment    : LPWSTR;
    usri2_parms          : LPWSTR;
    usri2_workstations   : LPWSTR;
    usri2_last_logon     : DWORD;
    usri2_last_logoff    : DWORD;
    usri2_acct_expires   : DWORD;
    usri2_max_storage    : DWORD;
    usri2_units_per_week : DWORD;
    usri2_logon_hours    : Pointer;
    usri2_bad_pw_count   : DWORD;
    usri2_num_logons     : DWORD;
    usri2_logon_server   : LPWSTR;
    usri2_country_code   : DWORD;
    usri2_code_page      : DWORD;
  end;
  TUserInfo2 = USER_INFO_2;
  PUserInfo2 = ^TUserInfo2;

var
  hNetApi32 : THandle = 0;

  NetUserEnum : function(
                          servername       : LPWSTR;
                          level            : DWORD;
                          filter           : DWORD;
                          var bufptr       : Pointer;
                          prefmaxlen       : DWORD;
                          var entriesread  : DWORD;
                          var totalentries : DWORD;
                          resume_handle    : Pointer
  ) : NetApiStatus; stdcall;

  NetUserGetInfo : function(
                              servername : LPWSTR;
                              username   : LPWSTR;
                              level      : DWORD;
                              var bufptr : Pointer
  ) : NetApiStatus; stdcall;

  NetApiBufferFree : function(Buffer: Pointer) : NetApiStatus; stdcall;

implementation

uses Sub7.Core.Exceptions, Sub7.Core.Bundle, System.SysUtils;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7EnumUser


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___enum
-------------------------------------------------------------------------------}
procedure TS7EnumUser.Enum();
var AStatus       : NetApiStatus;
    pBuffer       : Pointer;
    AEntriesRead  : DWORD;
    ATotalEntries : DWORD;
    AResume       : DWORD;
    pUserInfo     : PUserInfo2;
    I             : Cardinal;
begin
  FItems.Clear();
  repeat
    AResume := 0;
    ///

    AStatus := NetUserEnum(
                            nil,
                            2 {USER_INFO_2},
                            FILTER_NORMAL_ACCOUNT,
                            pBuffer,
                            MAX_PREFERRED_LENGTH,
                            AEntriesRead,
                            ATotalEntries,
                            @AResume
    );

    case AStatus of
      NERR_SUCCESS, ERROR_MORE_DATA : begin
        try
          pUserInfo := pBuffer;
          ///

          for I := 0 to AEntriesRead - 1 do begin
            try
              FItems.Add(WideCharToString(pUserInfo^.usri2_name));

            finally
              Inc(pUserInfo);
            end;
          end;
        finally
          NetApiBufferFree(pBuffer);
        end;
      end;

      ERROR_ACCESS_DENIED  : raise Exception.Create('The user does not have access to the requested information.');
      NERR_BufTooSmall     : raise Exception.Create('The buffer is too small to contain an entry. No information has been written to the buffer.');
      NERR_InvalidComputer : raise Exception.Create('The computer name is invalid.');
    end;
  until AStatus <> ERROR_MORE_DATA;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}

constructor TS7EnumUser.Create(const ARefresh : Boolean = False);
begin
  if not Assigned(NetUserEnum) then
    raise ES7SystemException.Create(Format(ERR_MISSING_WINAPI, ['NetUserEnum']));

  if not Assigned(NetApiBufferFree) then
    raise ES7SystemException.Create(Format(ERR_MISSING_WINAPI, ['NetApiBufferFree']));
  ///

  FItems := TList<String>.Create();

  if ARefresh then
    self.Enum();
end;

constructor TS7EnumUser.Create(const ASerializedObject : ISuperObject = nil);
begin
  self.Create(False);
  ///

  if not Assigned(ASerializedObject) then
    Exit();
  ///

  self.DeSerialize(ASerializedObject);
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7EnumUser.Destroy();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___clear
-------------------------------------------------------------------------------}
procedure TS7EnumUser.Clear();
begin
  if Assigned(FItems) then
    FItems.Clear();
end;

{-------------------------------------------------------------------------------
  ___serialize
-------------------------------------------------------------------------------}
function TS7EnumUser.Serialize() : ISuperObject;
var AUsers : ISuperArray;
    AUser  : String;
begin
  result := nil;
  ///

  if not Assigned(FItems) then
    Exit();

  if FItems.Count = 0 then
    Exit();

  result := TSuperObject.Create();

  AUsers := TSuperArray.Create();

  for AUser in FItems do
    AUsers.Add(AUser);

  result.A['users'] := AUsers;
end;

{-------------------------------------------------------------------------------
  ___deserialize
-------------------------------------------------------------------------------}
procedure TS7EnumUser.DeSerialize(const ASerializedObject : ISuperObject);
var ANodes : ISuperArray;
    I      : Integer;
begin
  if not Assigned(ASerializedObject) or not Assigned(FItems) then
    Exit();
  ///

  FItems.Clear();

  if not ASerializedObject.Contains('users') then
    raise ES7DeserializationError.Create(ERR_INVALID_DATA);

  ANodes := ASerializedObject.A['users'];

  for I := 0 to ANodes.Length -1 do
    FItems.Add(ANodes.S[I]);
end;

{-------------------------------------------------------------------------------
  ___init
-------------------------------------------------------------------------------}
initialization
  NetUserEnum      := nil;
  NetApiBufferFree := nil;
  NetUserGetInfo   := nil;
  ///

  hNetApi32 := LoadLibrary('Netapi32.dll');
  if hNetApi32 <> 0 then begin
    @NetUserEnum      := GetProcAddress(hNetApi32, 'NetUserEnum');
    @NetApiBufferFree := GetProcAddress(hNetApi32, 'NetApiBufferFree');
    @NetUserGetInfo   := GetProcAddress(hNetApi32, 'NetUserGetInfo');
  end;

finalization
  if hNetApi32 <> 0 then
    FreeLibrary(hNetApi32);

end.
