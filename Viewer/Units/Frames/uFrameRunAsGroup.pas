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

unit uFrameRunAsGroup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, S7Edit,
  Sub7.Viewer.VCL.CheckBox, S7GroupBox, S7ComboBox, XSuperObject, uFrameComboSessions,
  ___S7Frame, uFrameComboUser;

type
  TFrameRunAsGroup = class(TFrame)
    GroupRunAs: TS7GroupBox;
    RadioSession: TS7CheckBox;
    RadioWinUser: TS7CheckBox;
    GroupCredential: TS7GroupBox;
    Label3: TLabel;
    Label4: TLabel;
    EditPassword: TS7Edit;
    RadioNTSYS: TS7CheckBox;
    FrameComboSessions1: TFrameComboSessions;
    FrameComboUser1: TFrameComboUser;
    procedure RadioSessionStateChanged(Sender: TObject);
    procedure RadioWinUserStateChanged(Sender: TObject);
  public
    {@M}
    function Serialize() : TSuperObject;

    function ValidateForm() : Boolean;

    {@C}
    constructor Create(AOwner : TComponent); override;
  end;

implementation

uses S7Types, uFrameSystemInformationHook, uFormMain, Sub7.Core.Types, Sub7.Core.Protocol,
     Sub7.Core.Exceptions, Sub7.Core.Bundle;

{$R *.dfm}

function TFrameRunAsGroup.ValidateForm() : Boolean;
begin
  result := True;
  ///

  if self.RadioSession.Checked and
    (not self.FrameComboSessions1.ComboSessions.IsValid) then
    result := False;

  if self.RadioWinUser.Checked and (
    not FrameComboUser1.ComboUser.IsValid
  ) then
    result := False;
end;

function TFrameRunAsGroup.Serialize() : TSuperObject;
var AMode      : TS7RunAsMode;
    ASessionId : Integer;
begin
  result := TSuperObject.Create();
  ///

  if RadioNTSYS.Checked then
    AMode := ramNT
  else if RadioSession.Checked then begin
    AMode := ramSession;

    ASessionId := self.FrameComboSessions1.SessionId;

    result.I['session_id'] := ASessionId;
  end else if RadioWinUser.Checked then begin
    AMode := ramUser;

    result.S['user']     := FrameComboUser1.ComboUser.Text;
    result.S['password'] := EditPassword.Text;
  end;

  ///
  result.I['mode'] := Integer(AMode);
end;

constructor TFrameRunAsGroup.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

end;

procedure TFrameRunAsGroup.RadioSessionStateChanged(Sender: TObject);
begin
  if Assigned(self.FrameComboSessions1) then
    self.FrameComboSessions1.Enabled := TS7CheckBox(Sender).Checked;
end;

procedure TFrameRunAsGroup.RadioWinUserStateChanged(Sender: TObject);
begin
  FrameComboUser1.Enabled := TS7CheckBox(Sender).Checked;
  EditPassword.Enabled    := TS7CheckBox(Sender).Checked;
end;

end.
