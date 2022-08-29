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

unit ___S7Frame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Core.Messages.Listener;

type
  TS7Frame = class(TFrame)
  private
    FMessageListener : TS7MessageListener;

    {@M}
    procedure OnReceiveSubSevenMessage(Sender : TObject; AMessage : Cardinal);
  protected
    {@M}
    procedure Resize; override;

    ///

    procedure CMEnabledChanged(var AMessage: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var AMessage: TMessage); message CM_VISIBLECHANGED;

    procedure OnConnectionStateUpdated(const AConnected : Boolean); virtual;
    procedure OnTerminalSessionUpdate(); virtual;
    procedure OnWindowsUserUpdate(); virtual;
    procedure OnSub7Conected(); virtual;
    procedure OnSub7Disconnected(); virtual;

    procedure DoResize(); virtual;

    procedure OnShow(); virtual;
    procedure OnHide(); virtual;
    procedure OnEnabled(); virtual;
    procedure OnDisabled(); virtual;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;

implementation

uses Sub7.Viewer.Messages;

{$R *.dfm}

procedure TS7Frame.Resize();
begin
  inherited;
  ///

  self.DoResize();
end;

constructor TS7Frame.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.DoubleBuffered := True;
  self.Color          := clBlack;

  FMessageListener := TS7MessageListener.Create();

  FMessageListener.OnMessage := OnReceiveSubSevenMessage;

  FMessageListener.Add(SUB7_TERMINAL_SESSION_UPDATED);
  FMessageListener.Add(SUB7_CONNECTED);
  FMessageListener.Add(SUB7_DISCONNECTED);
  FMessageListener.Add(SUB7_WINUSER_UPDATED);
end;

destructor TS7Frame.Destroy();
begin
  if Assigned(FMessageListener) then
    FreeAndNil(FMessageListener);

  ///
  inherited Destroy();
end;

procedure TS7Frame.OnReceiveSubSevenMessage(Sender : TObject; AMessage : Cardinal);
begin
  if AMessage = SUB7_TERMINAL_SESSION_UPDATED then
    self.OnTerminalSessionUpdate()
  else if AMessage = SUB7_CONNECTED then begin
    self.OnSub7Conected();

    self.OnConnectionStateUpdated(True);
  end else if AMessage = SUB7_DISCONNECTED then begin
    self.OnSub7Disconnected();

    self.OnConnectionStateUpdated(False);
  end else if AMessage = SUB7_WINUSER_UPDATED then
    self.OnWindowsUserUpdate();
end;

procedure TS7Frame.CMEnabledChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  if self.Enabled then
    self.OnEnabled()
  else
    self.OnDisabled();
end;

procedure TS7Frame.CMVisibleChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  if self.Visible then begin
    self.OnShow();

    self.DoResize();
  end else
    self.OnHide();
end;

{-------------------------------------------------------------------------------
  ___override_me
-------------------------------------------------------------------------------}

procedure TS7Frame.DoResize();
begin
  ///
end;

procedure TS7Frame.OnShow();
begin
  ///
end;

procedure TS7Frame.OnHide();
begin
  ///
end;

procedure TS7Frame.OnEnabled();
begin
  ///
end;

procedure TS7Frame.OnDisabled();
begin
  ///
end;

procedure TS7Frame.OnTerminalSessionUpdate();
begin
  ///
end;

procedure TS7Frame.OnWindowsUserUpdate();
begin
  ///
end;

procedure TS7Frame.OnConnectionStateUpdated(const AConnected : Boolean);
begin
  ///
end;

procedure TS7Frame.OnSub7Conected();
begin
  ///
end;

procedure TS7Frame.OnSub7Disconnected();
begin
  ///
end;

end.
