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

unit uFormPassword;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.CaptionBar, S7Panel,
  Vcl.StdCtrls, Sub7.Viewer.VCL.Button, S7Edit, S7StatusBar;

type
  TFormPassword = class(TForm)
    SubSevenForms: TS7Form;
    CaptionBar: TS7CaptionBar;
    PanelClient: TS7Panel;
    LabelPassword: TLabel;
    EditPassword: TS7Edit;
    ButtonConnect: TS7Button;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonConnectClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure CaptionBarClose(Sender: TObject; var AHandled: Boolean);
    procedure EditPasswordChange(Sender: TObject);
  private
    FCanceled : Boolean;

    {@M}
    procedure DoClose(ACanceled : Boolean);
  public
    {@M}
    function GetPassword() : String;

    {@G}
    property Canceled : Boolean read FCanceled;
  end;

var
  FormPassword: TFormPassword;

implementation

{$R *.dfm}

uses uFormMain, S7Types, Sub7.Core.Utils;

procedure TFormPassword.DoClose(ACanceled : Boolean);
begin
  FCanceled := ACanceled;

  self.Close();
end;

procedure TFormPassword.EditPasswordChange(Sender: TObject);
begin
  if TS7Edit(Sender).IsEmpty then
    self.EditPassword.Status := csError
  else
    self.EditPassword.Status := csNormal;
end;

function TFormPassword.GetPassword() : String;
begin
  result := HashSubSevenPassword(self.EditPassword.text);
end;

procedure TFormPassword.CaptionBarClose(Sender: TObject;
  var AHandled: Boolean);
begin
  AHandled := True;

  self.DoClose(True);
end;

procedure TFormPassword.ButtonConnectClick(Sender: TObject);
begin
  if EditPassword.IsEmpty then begin
    self.EditPassword.Status := csError;

    if FormMain.MessageBox.MessageBox(self, 'Please enter server password in order to start your remote control session. Password authentication is not mandatory so it cannot be left blank.' + #13+#10#13+#10 + 'Do you wish to abort connection?', 'Invalid Input', MB_ICONERROR + MB_YESNO) = ID_YES then
      self.DoClose(True);
  end else
    self.DoClose(False);
end;

procedure TFormPassword.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;

  FormMain.AlphaBlend := False;
end;

procedure TFormPassword.FormCreate(Sender: TObject);
begin
  FCanceled := False;
end;

procedure TFormPassword.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13 : begin
      ButtonConnectClick(ButtonConnect);
    end;

    27 : begin
      self.DoClose(True);
    end;
  end;
end;

procedure TFormPassword.FormShow(Sender: TObject);
begin
  EditPassword.Width := PanelClient.Width - (LabelPassword.left + LabelPassword.Width + 16);
  ButtonConnect.Left := (PanelClient.Width div 2) - (ButtonConnect.Width div 2);

  FormMain.AlphaBlend := True;

  self.EditPassword.SetFocus();
end;

end.
