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

unit uFormHashPassword;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, S7CButton, Vcl.ExtCtrls;

type
  TFormHashPassword = class(TForm)
    LabelTitle: TLabel;
    EditPassword: TEdit;
    LabelResult: TLabel;
    EditResult: TEdit;
    ButtonOk: TS7CButton;
    ButtonCopy: TS7CButton;
    Shape: TShape;
    procedure EditResultChange(Sender: TObject);
    procedure EditPasswordChange(Sender: TObject);
    procedure ButtonCopyClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHashPassword: TFormHashPassword;

implementation

uses System.Hash, VCL.Clipbrd, Sub7.Core.Utils;

{$R *.dfm}

procedure TFormHashPassword.ButtonCopyClick(Sender: TObject);
begin
  Clipboard.AsText := self.EditResult.Text;
end;

procedure TFormHashPassword.ButtonOkClick(Sender: TObject);
begin
  self.Close();
end;

procedure TFormHashPassword.EditPasswordChange(Sender: TObject);
begin
  if TEdit(Sender).Text = '' then
    EditResult.Clear()
  else
    EditResult.Text := HashSubSevenPassword(EditPassword.Text);
end;

procedure TFormHashPassword.EditResultChange(Sender: TObject);
begin
  self.ButtonCopy.Enabled := Length(TEdit(Sender).Text) > 0;
end;

end.
