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

unit Sub7.Core.Messages.Listener;

interface

uses Winapi.Windows, System.Classes, Winapi.Messages, Generics.Collections;

type
  TS7MessageEvent = procedure(Sender : TObject; AMessage : Cardinal) of object;

  TS7MessageListener = class(TObject)
  private
    FMsgHandler         : THandle;
    FRegisteredMessages : TList<Cardinal>;
    FOnMessage          : TS7MessageEvent;

    {@M}
    procedure WndMethod(var AMessage : TMessage);
  public
    {@M}
    procedure Add(const AMessage : Cardinal);
    procedure Remove(const AMessage : Cardinal);
    procedure Clear();

    {@C}
    constructor Create();
    destructor Destroy(); override;

    {@G/S}
    property OnMessage : TS7MessageEvent read FOnMessage write FOnMessage;
  end;

  {
    @Exported
  }
  procedure BroadcastMessage(const AMessage : UINT; const AIsolate : Boolean = True);

implementation

uses System.SysUtils;

{-------------------------------------------------------------------------------
  ___add
-------------------------------------------------------------------------------}
procedure TS7MessageListener.Add(const AMessage : Cardinal);
begin
  if not Assigned(FRegisteredMessages) or (AMessage = 0) then
    Exit();

  if FRegisteredMessages.Contains(AMessage) then
    Exit();

  FRegisteredMessages.Add(AMessage);
end;

{-------------------------------------------------------------------------------
  ___remove
-------------------------------------------------------------------------------}
procedure TS7MessageListener.Remove(const AMessage : Cardinal);
begin
  if not Assigned(FRegisteredMessages) then
    Exit();

  if not FRegisteredMessages.Contains(AMessage) then
    Exit();

  FRegisteredMessages.Remove(AMessage);
end;

{-------------------------------------------------------------------------------
  ___clear
-------------------------------------------------------------------------------}
procedure TS7MessageListener.Clear();
begin
  if Assigned(FRegisteredMessages) then
    FRegisteredMessages.Clear();
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7MessageListener.Create();
begin
  inherited Create();
  ///

  FMsgHandler := AllocateHWnd(WndMethod);
  FOnMessage  := nil;

  FRegisteredMessages := TList<Cardinal>.Create();
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7MessageListener.Destroy();
begin
  DeallocateHWnd(FMsgHandler);

  if Assigned(FRegisteredMessages) then
    FreeAndNil(FRegisteredMessages);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___message
-------------------------------------------------------------------------------}
procedure TS7MessageListener.WndMethod(var AMessage : TMessage);
var ADefault : Boolean;
begin
  ADefault := True;
  try
    if Assigned(FOnMessage) and Assigned(FRegisteredMessages) then begin
      if (FRegisteredMessages.Contains(AMessage.Msg)) then begin
        if (AMessage.wParam <> 0) and (AMessage.wParam <> GetCurrentProcessId) then
          Exit();
        ///

        FOnMessage(self, AMessage.Msg);
      end;
    end;
  finally
    if ADefault then
      AMessage.Result := DefWindowProc(
                                        FMsgHandler,
                                        AMessage.Msg,
                                        AMessage.wParam,
                                        AMessage.lParam
      );
  end;
end;

{-------------------------------------------------------------------------------
  ___broadcast
-------------------------------------------------------------------------------}
procedure BroadcastMessage(const AMessage : UINT; const AIsolate : Boolean = True);
var wParam : NativeUInt;
begin
  if AMessage = 0 then
    Exit();
  ///

  wParam := 0;
  if AIsolate then
    wParam := GetCurrentProcessId();

  ///
  PostMessage(HWND_BROADCAST, AMessage, wParam, 0);
end;

end.
