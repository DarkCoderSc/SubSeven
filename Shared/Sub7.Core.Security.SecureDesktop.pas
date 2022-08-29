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

unit Sub7.Core.Security.SecureDesktop;

interface

uses Winapi.Windows, System.Classes, Sub7.Core.Thread, VCL.Graphics;

type
  TCallBack = procedure(Sender : TObject; const ASecureDesktopSuccess : Boolean) of object;

  TSpawnSecureDesktopThread = class(TCoreThread)
  private
    FCallBack : TCallBack;

    {@M}
    procedure OnThreadExecute(); override;
  public
    {@C}
    constructor Create(ACallback : TCallback); overload;
  end;

const
    DESKTOP_NONE            = $0000000;
    DESKTOP_READOBJECTS     = $0000001;
    DESKTOP_CREATEWINDOW    = $0000002;
    DESKTOP_CREATEMENU      = $0000004;
    DESKTOP_HOOKCONTROL     = $0000008;
    DESKTOP_JOURNALRECORD   = $0000010;
    DESKTOP_JOURNALPLAYBACK = $0000020;
    DESKTOP_ENUMERATE       = $0000040;
    DESKTOP_WRITEOBJECTS    = $0000080;
    DESKTOP_SWITCHDESKTOP   = $0000100;

    DESKTOP_GENERIC_ALL : Cardinal = DESKTOP_READOBJECTS     or
                                     DESKTOP_CREATEWINDOW    or
                                     DESKTOP_CREATEMENU      or
                                     DESKTOP_HOOKCONTROL     or
                                     DESKTOP_JOURNALRECORD   or
                                     DESKTOP_JOURNALPLAYBACK or
                                     DESKTOP_ENUMERATE       or
                                     DESKTOP_WRITEOBJECTS    or
                                     DESKTOP_SWITCHDESKTOP;

implementation

uses System.SysUtils, Sub7.Core.Utils, Sub7.Core.Windows.Userland.Desktop,
     Sub7.Core.Application.Env, Sub7.Core.Exceptions, Sub7.Core.Bundle;

{ TSpawnSecureDesktopThread.Create }

constructor TSpawnSecureDesktopThread.Create(ACallback : TCallback);
begin
  inherited Create();
  ///

  FCallback := ACallback;
end;

{ TSpawnSecureDesktopThread.OnThreadExecute }

procedure TSpawnSecureDesktopThread.OnThreadExecute();
var hSecureDesktop : THandle;
    AOldDesktop    : HDESK;
    ANewDesktop    : HDESK;
    ADesktopImage  : TBitmap;
    ASuccess       : Boolean;

    ApproveSubSevenUsage : function(AOldDesktop, ANewDesktop : HDESK; ADesktopImage : TBitmap): BOOL; stdcall;
begin
  AOldDesktop   := 0;
  ANewDesktop   := 0;
  ADesktopImage := nil;
  ///

  AOldDesktop := GetThreadDesktop(GetCurrentThreadId());

  ANewDesktop := CreateDesktop(
                                PWideChar(RandomString(32)),
                                nil,
                                nil,
                                0,
                                DESKTOP_GENERIC_ALL,
                                nil
  );

  ADesktopImage := GetDesktopImage();
  try
   SwitchDesktop(ANewDesktop);
   ///

    SetThreadDesktop(ANewDesktop);

    hSecureDesktop := LoadLibrary(PWideChar(APP_ENV_SecureDesktopFile));
    if hSecureDesktop = 0 then
      raise ES7Exception.Create(Format(ERR_LIBRARY, [APP_ENV_SecureDesktopFile]));
    try
      @ApproveSubSevenUsage := GetProcAddress(hSecureDesktop, 'ApproveSubSevenUsage');
      if Assigned(ApproveSubSevenUsage) then begin
        try
          ASuccess := ApproveSubSevenUsage(AOldDesktop, ANewDesktop, ADesktopImage);

          if Assigned(FCallBack) then
            SafeSynchronize(procedure begin
              FCallback(self, ASuccess);
            end);
        except

        end;
      end;
    finally
      if hSecureDesktop <> 0 then
        FreeLibrary(hSecureDesktop);
    end;
  finally
    if AOldDesktop <> 0 then
      SwitchDesktop(AOldDesktop);

    if ANewDesktop <> 0 then
      CloseDesktop(ANewDesktop);

    if Assigned(ADesktopImage) then
      FreeAndNil(ADesktopImage);
  end;
end;

end.
