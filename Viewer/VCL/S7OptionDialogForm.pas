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

unit S7OptionDialogForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, S7OptionDialog, S7Panel, Sub7.Viewer.VCL.CaptionBar,
  Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.CheckBox, Sub7.Viewer.VCL.Button, Generics.Collections;

type
  TSub7FormOptionDialog = class(TForm)
    PanelClient: TS7Panel;
    SubSevenForms: TS7Form;
    CaptionBar: TS7CaptionBar;
    PanelFooter: TS7Panel;
    ButtonOk: TS7Button;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    {@M}
    FOptionDialog : TS7OptionDialog;

    FComponents : TObjectDictionary<TS7OptionItem, TS7CheckBox>;

    {@M}
    procedure ApplyChanges();
  public
    {@M}
    procedure Render();
    procedure DoResize();

    {@C}
    constructor Create(AOwner : TComponent; AOptionDialog : TS7OptionDialog); overload;
    destructor Destroy(); override;
  end;

var
  Sub7FormOptionDialog: TSub7FormOptionDialog;

implementation

{$R *.dfm}

procedure TSub7FormOptionDialog.ApplyChanges();
var AOption : TS7OptionItem;
    C       : TS7CheckBox;
begin
  if not Assigned(FOptionDialog) then
    Exit();
  ///

  for AOption in FComponents.keys do begin
    if not FComponents.TryGetValue(AOption, C) then
      continue;
    ///

    AOption.Checked := C.Checked;
  end;
end;

procedure TSub7FormOptionDialog.DoResize();
begin
  ButtonOk.Top  := (PanelFooter.Height div 2) - (ButtonOk.Height div 2);
  ButtonOk.Left := (PanelFooter.Width div 2) - (ButtonOk.Width div 2);
end;

procedure TSub7FormOptionDialog.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    27 : begin
      self.Close();
    end;

    13 : begin
      ButtonOkClick(ButtonOk);
    end;
  end;
end;

procedure TSub7FormOptionDialog.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TSub7FormOptionDialog.FormShow(Sender: TObject);
begin
  DoResize();
end;

procedure TSub7FormOptionDialog.ButtonOkClick(Sender: TObject);
begin
  self.ApplyChanges();

  self.Close();
end;

constructor TSub7FormOptionDialog.Create(AOwner : TComponent; AOptionDialog : TS7OptionDialog);
begin
  inherited Create(AOwner);
  ///

  FOptionDialog := AOptionDialog;

  FComponents := TObjectDictionary<TS7OptionItem, TS7CheckBox>.Create([doOwnsValues]);
end;

destructor TSub7FormOptionDialog.Destroy();
begin
  if Assigned(FComponents) then
    FreeAndNil(FComponents);

  ///
  inherited Destroy();
end;

procedure TSub7FormOptionDialog.Render();
var i       : integer;
    AOption : TS7OptionItem;
    C       : TS7CheckBox;
    APosY   : Integer;
const
  C_HEIGHT = 19;
begin
  self.Constraints.MinHeight := 0;
  self.Constraints.MinWidth  := 0;

  FComponents.Clear();
  ///

  if not Assigned(FOptionDialog) then
    Exit();
  ///

  CaptionBar.Caption := FOptionDialog.Caption;
  ///

  APosY := 16;

  for i := 0 to FOptionDialog.Options.Count -1 do begin
    AOption := FOptionDialog.Options.Items[i];
    ///

    C := TS7CheckBox.Create(PanelClient);

    FComponents.Add(AOption, C);

    C.Parent := PanelClient;

    C.Caption := AOption.Caption;
    C.Hint    := AOption.Hint;
    C.Checked := AOption.Checked;
    C.Enabled := AOption.Enabled;
    C.Name    := AOption.Name;

    C.Width   := (ClientWidth - 32);
    C.Top     := APosY;
    C.Left    := 16;
    C.Height  := 19;

    Inc(APosY, C.Height);


    if AOption.Kind = okCheckBox then
      TS7CheckBox(C).Mode := cbmCheckBox
    else
      TS7CheckBox(C).Mode := cbmRadioBox;
  end;

  ClientWidth  := FOptionDialog.Width;
  ClientHeight := CaptionBar.Height + APosY + C_HEIGHT + PanelFooter.Height;

  self.Constraints.MinHeight := ClientHeight;
  self.Constraints.MinWidth  := ClientWidth;
end;

end.
