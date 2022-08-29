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

unit uFrameOpen;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ___S7DockFrame, S7DockCaption,
  S7Panel, Sub7.Viewer.VCL.Button, Vcl.StdCtrls, S7Edit, S7ComboBox, S7ImageButton, Sub7.Core.Protocol,
  Sub7.Core.Types, XSuperObject, Sub7.Viewer.VCL.CheckBox, S7GroupBox, uFrameComboSessions,
  ___S7Frame;

type
  TFrameOpen = class(TS7FrameDock)
    PanelForm: TS7Panel;
    LabelOpenCommand: TLabel;
    EditCommand: TS7Edit;
    LabelSession: TLabel;
    ButtonOpen: TS7Button;
    FrameComboSessions1: TFrameComboSessions;
    procedure ButtonOpenClick(Sender: TObject);
  private
    {@M}
    function CheckForm() : Boolean;
  protected
    {@M}
    procedure DoResize(); override;
    procedure OnConnectionStateUpdated(const AConnected : Boolean); override;
  public
    {@M}
    procedure Render(const ACommand : TS7Command; const AData : ISuperObject); override;
  end;

implementation

uses uFormMain, Sub7.Core.Exceptions, Sub7.Core.Bundle;

{$R *.dfm}

procedure TFrameOpen.ButtonOpenClick(Sender: TObject);
var AData : ISuperObject;
begin
  if not self.CheckForm() then
    raise ES7FormException.Create(ERR_FORM);
  ///

  AData := TSuperObject.Create();

  AData.S['command']    := EditCommand.Text;
  AData.I['session_id'] := self.FrameComboSessions1.SessionId;

  ///
  FormMain.Session.SendCommand(mhcOpen, AData);
end;

function TFrameOpen.CheckForm() : Boolean;
begin
  result := True;
  ///

  if not EditCommand.IsValid then
    result := False;

  if not self.FrameComboSessions1.ComboSessions.IsValid then
    result := False;
end;

procedure TFrameOpen.DoResize();
var ADelta : Integer;
begin
  if not self.DockCaption.Docked then
    ADelta := 0
  else
    ADelta := self.DockCaption.Height;
  ///

  ButtonOpen.Left := (PanelForm.Width div 2) - (ButtonOpen.Width div 2);
  PanelForm.Left  := (ClientWidth div 2) - (PanelForm.Width div 2);
  PanelForm.Top   := ADelta + ((ClientHeight - ADelta) div 2) - (PanelForm.Height div 2);
end;

procedure TFrameOpen.OnConnectionStateUpdated(const AConnected : Boolean);
begin
  inherited;
  ///

  ButtonOpen.Enabled          := AConnected;
  EditCommand.Enabled         := AConnected;
  FrameComboSessions1.Enabled := AConnected;

  ///
  EditCommand.Clear();
end;

procedure TFrameOpen.Render(const ACommand : TS7Command; const AData : ISuperObject);
begin
  {}
end;

end.
