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

unit uFrameComboUser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, S7ComboBox,
  Sub7.Core.Protocol, Sub7.Core.Messages.Listener, ___S7Frame;

type
  TFrameComboUser = class(TS7Frame)
    ComboUser: TS7ComboBox;
  private
    {@M}
    procedure RefreshUser();
  protected
    {@M}
    procedure OnEnabled(); override;
    procedure OnDisabled(); override;

    procedure OnWindowsUserUpdate(); override;
  public

    {@C}
    constructor Create(AOwner : TComponent); override;
  end;

implementation

uses uFrameSystemInformationHook, Sub7.Core.Exceptions, Sub7.Core.Bundle, uFormMain, Sub7.Core.Types,
     S7Types, Sub7.Viewer.Types;

{$R *.dfm}

procedure TFrameComboUser.RefreshUser();
var AUser        : String;
    AFrameAction : TFrameSystemInformationHook;
    AOldText     : String;
begin
  AOldText := ComboUser.Text;
  try
    ComboUser.Clear();
    ///

    AFrameAction := TFrameSystemInformationHook(FormMain.GetDockedFrame(nmMachineActions));
    if not Assigned(AFrameAction) then
      Exit();

    for AUser in AFrameAction.Users.Items do begin
      ComboUser.Items.Add(AUser);
    end;
  finally
    ComboUser.Text := AOldText;
  end;
end;

constructor TFrameComboUser.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.RefreshUser();
end;

procedure TFrameComboUser.OnWindowsUserUpdate();
begin
  inherited;
  ///

  self.RefreshUser();
end;

procedure TFrameComboUser.OnEnabled();
begin
  inherited;
  ///

  self.ComboUser.Enabled := True;
end;

procedure TFrameComboUser.OnDisabled();
begin
  inherited;
  ///

  self.ComboUser.Enabled := False;
end;

end.
