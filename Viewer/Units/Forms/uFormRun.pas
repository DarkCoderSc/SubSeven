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

unit uFormRun;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, S7Panel, Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.CaptionBar,
  Sub7.Viewer.VCL.CheckBox, S7GroupBox, Vcl.StdCtrls, S7Edit, Sub7.Viewer.VCL.Button, uFrameRunAsGroup,
  ___S7ContextWindow, Sub7.Core.Protocol, uFrameSystemInformationHook;

type
  TFormRun = class(TS7ContextWindow)
    CaptionBar: TS7CaptionBar;
    SubSevenForms: TS7Form;
    PanelClient: TS7Panel;
    PanelForm: TS7Panel;
    LabelProgram: TLabel;
    LabelArguments: TLabel;
    EditProgram: TS7Edit;
    EditArguments: TS7Edit;
    PanelFooter: TS7Panel;
    ButtonCancel: TS7Button;
    ButtonRun: TS7Button;
    FrameRunAsGroup1: TFrameRunAsGroup;
    procedure ButtonCancelClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    {@M}
    procedure DoResize(); override;
    function ValidateForm() : Boolean;
  public

  end;

var
  FormRun: TFormRun;

implementation

uses uFormMain, S7Types, XSuperObject, Sub7.Core.Exceptions, Sub7.Core.Bundle, S7Validators;

{$R *.dfm}

function TFormRun.ValidateForm() : Boolean;
begin
  result := True;
  ///

  if not EditProgram.IsValid then
    result := False;

  if not self.FrameRunAsGroup1.ValidateForm() then
    result := False;
end;

procedure TFormRun.ButtonCancelClick(Sender: TObject);
begin
  self.close();
end;

procedure TFormRun.ButtonRunClick(Sender: TObject);
var AData : ISuperObject;
begin
  if not ValidateForm() then
    raise ES7FormException.Create(ERR_FORM);
  ///

  AData := TSuperObject.Create();

  AData.S['program'] := EditProgram.Text;

  if not EditArguments.IsEmpty then
    Adata.S['argv'] := EditArguments.Text;

  AData.O['runas'] := self.FrameRunAsGroup1.Serialize();

  ///
  FormMain.Session.SendCommand(mhcRun, AData);
end;

procedure TFormRun.DoResize();
begin
  inherited;
  ///

  ButtonCancel.Left := (PanelFooter.Width div 2) - ButtonCancel.Width - 4;
  ButtonRun.Left    := (PanelFooter.Width div 2) + 4;

  ButtonCancel.Top  := (PanelFooter.Height div 2) - (ButtonCancel.Height div 2);
  ButtonRun.Top     := ButtonCancel.Top;
end;

procedure TFormRun.FormCreate(Sender: TObject);
begin
  FReleaseOnClose := True;
end;

procedure TFormRun.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13 : begin
      if ButtonRun.Enabled then
        ButtonRunClick(ButtonRun);
    end;

    27 : begin
      self.Close();
    end;
  end;
end;

end.
