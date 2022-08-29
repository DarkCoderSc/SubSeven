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

unit S7InputQueryForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.Button, Vcl.StdCtrls,
  Vcl.ExtCtrls, S7Panel, Sub7.Viewer.VCL.CaptionBar, Vcl.Imaging.pngimage, S7Edit;

type
  TSub7FormInputQuery = class(TForm)
    CaptionBar: TS7CaptionBar;
    PanelClient: TS7Panel;
    ImageIcon: TImage;
    LabelMessage: TLabel;
    PanelFooter: TS7Panel;
    ButtonRight: TS7Button;
    ButtonLeft: TS7Button;
    SubSevenForms: TS7Form;
    EditResponse: TS7Edit;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonLeftClick(Sender: TObject);
    procedure ButtonRightClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FResult : String;

    {@M}
    procedure DoResize();
  public
    {@G}
    property Result : String read FResult;
  end;

var
  Sub7FormInputQuery: TSub7FormInputQuery;

implementation

uses VCL.Consts;

{$R *.dfm}

procedure TSub7FormInputQuery.ButtonLeftClick(Sender: TObject);
begin
  self.Close();
end;

procedure TSub7FormInputQuery.ButtonRightClick(Sender: TObject);
begin
  FResult := EditResponse.text;

  self.Close();
end;

procedure TSub7FormInputQuery.DoResize();
var AHeight : Integer;
const START_TOP = 16;
begin
  self.Constraints.MinHeight := 0;
  self.Constraints.MinWidth  := 0;

  ImageIcon.Left    := 16;

  LabelMessage.Left     := ImageIcon.Left + ImageIcon.Width + 16;
  LabelMessage.Top      := START_TOP;
  LabelMessage.Width    := PanelClient.Width - LabelMessage.Left - 16;

  AHeight               := LabelMessage.top + LabelMessage.Height + 8 + CaptionBar.Height;

  EditResponse.top      := LabelMessage.top + LabelMessage.Height + 8;
  EditResponse.left     := LabelMessage.Left;
  EditResponse.Width    := LabelMessage.Width;

  ImageIcon.Top         := START_TOP + (((LabelMessage.top + LabelMessage.BoundsRect.Bottom) div 2) - (Icon.Height div 2));

  Inc(AHeight, PanelFooter.Height + EditResponse.Height + 8);

  if AHeight < 130 then
    AHeight := 130;

  ClientHeight := AHeight;

  ButtonLeft.Left  := (PanelFooter.Width div 2) - 4 - ButtonLeft.Width;
  ButtonRight.Left := ButtonLeft.Left + ButtonLeft.Width + 8;

  ButtonRight.Top := (PanelFooter.Height div 2) - (ButtonRight.Height div 2);
  ButtonLeft.Top  := ButtonRight.Top;

  self.Constraints.MinHeight := ClientHeight;
  self.Constraints.MinWidth  := ClientWidth;
end;

procedure TSub7FormInputQuery.FormCreate(Sender: TObject);
begin
  FResult := '';

  ButtonLeft.Caption  := SMsgDlgCancel;
  ButtonRight.Caption := SMsgDlgConfirm;
end;

procedure TSub7FormInputQuery.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27 : self.Close;
    13 : ButtonRightClick(ButtonRight);
  end;
end;

procedure TSub7FormInputQuery.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TSub7FormInputQuery.FormShow(Sender: TObject);
begin
  DoResize();
end;

end.
