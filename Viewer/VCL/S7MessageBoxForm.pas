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

unit S7MessageBoxForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, S7Panel, Sub7.Viewer.VCL.CaptionBar, Sub7.Viewer.VCL.SubSevenForm,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls, Sub7.Viewer.VCL.Button, Vcl.StdCtrls, VCL.Consts,
  System.ImageList, Vcl.ImgList;

type
  TSub7FormMessageBox = class(TForm)
    SubSevenForms: TS7Form;
    CaptionBar: TS7CaptionBar;
    ImageIcon: TImage;
    PanelClient: TS7Panel;
    LabelMessage: TLabel;
    PanelFooter: TS7Panel;
    ButtonRight: TS7Button;
    ButtonLeft: TS7Button;
    ImagesIcon: TImageList;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonRightClick(Sender: TObject);
    procedure ButtonLeftClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FResult : Integer;

    {@M}
    procedure DoResize();
  public
    procedure MessageBox(const AMessage, ATitle : String; const AFlags : Integer);

    {@G}
    property Result : Integer read FResult;
  end;

var
  Sub7FormMessageBox: TSub7FormMessageBox;

{$R *.dfm}

implementation

procedure TSub7FormMessageBox.DoResize();
var AHeight : Integer;
const START_TOP = 16;
begin
  self.Constraints.MinHeight := 0;
  self.Constraints.MinWidth  := 0;

  if ImageIcon.Visible then begin
    ImageIcon.Left := 16;
    ImageIcon.Top  := START_TOP;
    LabelMessage.Left  := ImageIcon.Left + ImageIcon.Width + 16;
  end else
    LabelMessage.Left := 8;

  LabelMessage.Top   := START_TOP;
  LabelMessage.Width := PanelClient.Width - LabelMessage.Left - 8;

  AHeight := LabelMessage.top + LabelMessage.Height + 8 + CaptionBar.Height;
  if PanelFooter.Visible then
    Inc(AHeight, PanelFooter.Height);

  if AHeight < 130 then
    AHeight := 130;

  ClientHeight := AHeight;

  if PanelFooter.Visible then begin
    if ButtonRight.Visible and ButtonLeft.Visible then begin
      ButtonLeft.Left  := (PanelFooter.Width div 2) - 4 - ButtonLeft.Width;
      ButtonRight.Left := ButtonLeft.Left + ButtonLeft.Width + 8;
    end else if ButtonRight.Visible then begin
      ButtonRight.Left := (PanelFooter.Width div 2) - (ButtonRight.Width div 2);
    end else if ButtonLeft.Visible then begin
      ButtonLeft.Left := (PanelFooter.Width div 2) - (ButtonLeft.Width div 2);
    end;
  end;

  ButtonRight.Top := (PanelFooter.Height div 2) - (ButtonRight.Height div 2);
  ButtonLeft.Top  := ButtonRight.Top;

  self.Constraints.MinHeight := ClientHeight;
  self.Constraints.MinWidth  := ClientWidth;
end;

procedure TSub7FormMessageBox.ButtonLeftClick(Sender: TObject);
begin
  FResult := TS7Button(Sender).Value;

  self.Close();
end;

procedure TSub7FormMessageBox.ButtonRightClick(Sender: TObject);
begin
  FResult := TS7Button(Sender).Value;

  self.Close();
end;

procedure TSub7FormMessageBox.FormCreate(Sender: TObject);
begin
  FResult := 0;
end;

procedure TSub7FormMessageBox.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27 : self.Close;
    13 : ButtonRightClick(ButtonRight);
  end;
end;

procedure TSub7FormMessageBox.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TSub7FormMessageBox.FormShow(Sender: TObject);
begin
  DoResize();
end;

procedure TSub7FormMessageBox.MessageBox(const AMessage, ATitle : String; const AFlags : Integer);
var AIconIndex : Integer;
begin
  {
    Buttons
  }
  ButtonLeft.Visible  := False;
  ButtonLeft.Value    := 0;
  ButtonRight.Value   := 0;

  if (AFlags and MB_YESNO = MB_YESNO) then begin
    ButtonLeft.Visible  := True;
    ButtonLeft.Caption  := SMsgDlgNo;
    ButtonRight.Caption := SMsgDlgYes;
    ButtonLeft.Value    := ID_NO;
    ButtonRight.Value   := ID_YES;
    FResult             := ButtonLeft.Value; // Default
  end else
    ButtonRight.Caption := SMsgDlgOk;

  {
    Icon
  }
  AIconIndex := -1;

  if (AFlags and MB_ICONINFORMATION) = MB_ICONINFORMATION then
    AIconIndex := 0
  else
  if (AFlags and MB_ICONWARNING) = MB_ICONWARNING then
    AIconIndex := 1
  else
  if (AFlags and MB_ICONQUESTION) = MB_ICONQUESTION then
    AIconIndex := 2
  else
  if (AFlags and MB_ICONERROR) = MB_ICONERROR then
    AIconIndex := 3
  else
  if (AFlags and MB_USERICON) = MB_USERICON then
    AIconIndex := 4;

  ImageIcon.Visible := (AIconIndex <> -1);
  if ImageIcon.Visible then begin
    ImagesIcon.Draw(ImageIcon.Canvas, 0, 0, AIconIndex);
  end;

  {
    Texts
  }
  LabelMessage.Caption := AMessage;
  CaptionBar.Caption   := ATitle;
end;

end.
