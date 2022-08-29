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

unit uFormAddContact;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.CaptionBar, Sub7.Viewer.VCL.SubSevenForm, S7Panel,
  Vcl.StdCtrls, S7Edit, S7GroupBox, Vcl.ComCtrls, Sub7.Viewer.VCL.Button, VirtualTrees,
  ___S7BaseForm;

type
  TFormAddContact = class(TS7BaseForm)
    SubSevenForms: TS7Form;
    CaptionBar: TS7CaptionBar;
    PanelClient: TS7Panel;
    PanelForm: TS7Panel;
    LabelDisplayName: TLabel;
    EditDisplayName: TS7Edit;
    GroupBoxConnection: TS7GroupBox;
    LabelRemoteAddress: TLabel;
    EditRemoteAddress: TS7Edit;
    LabelRemotePort: TLabel;
    EditRemotePort: TS7Edit;
    LabelPassword: TLabel;
    EditPassword: TS7Edit;
    LabelDescription: TLabel;
    PanelMemo: TS7Panel;
    RichEditDescription: TRichEdit;
    PanelFooter: TS7Panel;
    ButtonAdd: TS7Button;
    ButtonCancel: TS7Button;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FNode : PVirtualNode;
  protected
    {@M}
    procedure DoResize(); override;
  public
    {@M}
    function ValidateForm() : Boolean;

    {@C}
    constructor Create(AOwner : TComponent; const ANode : PVirtualNode); overload;

    {@G}
    property Node : PVirtualNode read FNode;
  end;

var
  FormAddContact: TFormAddContact;

implementation

uses uFormAddressBook, uFormMain;

{$R *.dfm}

constructor TFormAddContact.Create(AOwner : TComponent; const ANode : PVirtualNode);
var pData : uFormAddressBook.PTreeData;
begin
  inherited Create(AOwner);
  ///

  FReleaseOnClose := True;

  FNode := ANode;

  if Assigned(FNode) then begin
    pData := FNode.GetData;

    EditDisplayName.Text     := pData^.DisplayName;
    RichEditDescription.Text := pData^.Description;
    EditRemoteAddress.Text   := pData^.RemoteAddress;
    EditRemotePort.Text      := IntToStr(pData^.RemotePort);
    EditPassword.Text        := pData^.Password;

    ButtonAdd.Caption := 'save';
    ButtonAdd.Hint    := 'save contact detail.';

    CaptionBar.Caption := 'edit existing contact';
  end;
end;

function TFormAddContact.ValidateForm() : Boolean;
begin
  result := True;
  ///

  if not EditDisplayName.IsValid then
    result := False;

  if not EditRemoteAddress.IsValid then
    result := False;

  if not EditRemotePort.IsValid then
    result := False;
end;

procedure TFormAddContact.ButtonAddClick(Sender: TObject);
begin
  if not self.ValidateForm() then
    Exit();
  ///

  if not Assigned(FNode) and FormAddressBook.ContactExists(EditRemoteAddress.Text, StrToInt(EditRemotePort.Text)) then begin
    if FormMain.MessageBox.MessageBox(
      self,
      'Target address:port is already present in address book. Do you want to ' +
      'update existing contact with current information?',
      'duplicate contact',
      MB_ICONQUESTION + MB_YESNO
    ) = ID_NO then
      Exit();

    FNode := FormAddressBook.GetContact(EditRemoteAddress.Text, StrToInt(EditRemotePort.Text));
  end;

  FormAddressBook.AddItem(
    FNode,
    EditDisplayName.Text,
    RichEditDescription.Text,
    EditRemoteAddress.Text,
    StrToInt(EditRemotePort.Text),
    EditPassword.Text
  );

  ///
  self.Close();
end;

procedure TFormAddContact.ButtonCancelClick(Sender: TObject);
begin
  self.Close();
end;

procedure TFormAddContact.DoResize();
begin
  ButtonCancel.Left := (PanelFooter.Width div 2) - ButtonCancel.Width - 4;
  ButtonAdd.Left    := (PanelFooter.Width div 2) + 4;

  ButtonCancel.Top  := (PanelFooter.Height div 2) - (ButtonCancel.Height div 2);
  ButtonAdd.Top     := ButtonCancel.Top;
end;

procedure TFormAddContact.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13 : ButtonAddClick(ButtonAdd);

    27 : self.Close();
  end;
end;

end.
