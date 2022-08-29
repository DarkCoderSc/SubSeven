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

unit Sub7.Core.SafeSocketList;

interface

uses Winapi.Winsock2, System.Classes, System.SyncObjs, Generics.Collections;

type
  TSub7SafeSocketList = class
  private
    FSockets : TList<TSocket>;
    FLock    : TCriticalSection;
  public
    {@C}
    constructor Create();
    destructor Destroy(); override;

    {@M}
    procedure Add(const ASocket : TSocket);
    procedure Delete(const ASocket : TSocket);
    procedure Clear();
  end;

  var GLOBAL_SafeSocketList : TSub7SafeSocketList;

implementation

uses System.SysUtils, Sub7.Core.Diagnostic;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSub7SafeSocketList.Create();
begin
  inherited Create();
  ///

  FSockets := TList<TSocket>.Create();
  FLock    := TCriticalSection.Create();
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TSub7SafeSocketList.Destroy();
begin
  self.Clear();
  ///

  FLock.Acquire();
  try
    if Assigned(FSockets) then
      FreeAndNil(FSockets);
  finally
    FLock.Leave();
  end;

  if Assigned(FLock) then
    FreeAndNil(FLock);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___add
-------------------------------------------------------------------------------}
procedure TSub7SafeSocketList.Add(const ASocket : TSocket);
begin
  FLock.Acquire();
  try
    if not Assigned(FSockets) then
      Exit();
    ///

    FSockets.Add(ASocket);
  finally
    FLock.Leave();
  end;
end;

{-------------------------------------------------------------------------------
  ___delete
-------------------------------------------------------------------------------}
procedure TSub7SafeSocketList.Delete(const ASocket : TSocket);
begin
  FLock.Acquire();
  try
    if not Assigned(FSockets) then
      Exit();
    ///

    Winapi.Winsock2.closesocket(ASocket);

    FSockets.Remove(ASocket);
  finally
    FLock.Leave();
  end;
end;

{-------------------------------------------------------------------------------
  ___clear
-------------------------------------------------------------------------------}
procedure TSub7SafeSocketList.Clear();
var ASocket : TSocket;
begin
  FLock.Acquire();
  try
    if not Assigned(FSockets) then
      Exit();
    ///

    for ASocket in FSockets do begin
      Winapi.Winsock2.Shutdown(ASocket, SD_BOTH);

      Winapi.Winsock2.closesocket(ASocket);
    end;

    ///
    FSockets.Clear();
  finally
    FLock.Leave();
  end;
end;

initialization
  GLOBAL_SafeSocketList := TSub7SafeSocketList.Create();

finalization
  if Assigned(GLOBAL_SafeSocketList) then
    FreeAndNil(GLOBAL_SafeSocketList);

end.
