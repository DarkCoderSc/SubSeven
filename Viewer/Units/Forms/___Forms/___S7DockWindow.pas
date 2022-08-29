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

unit ___S7DockWindow;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.CaptionBar,
  ___S7BaseForm;

type
  TS7WindowDock = class(TS7BaseForm)
    CaptionBar: TS7CaptionBar;
    S7Form1: TS7Form;
    procedure CaptionBarDock(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FOwnerFrame : TFrame;
  protected
    {@M}
    procedure CreateParams(var Params: TCreateParams); override;
  public
    {@C}
    constructor Create(AOwner : TComponent; AOwnerFrame : TFrame; const ANameEx : String = ''); overload;
  end;

var
  S7WindowDock: TS7WindowDock;

implementation

uses ___S7DockFrame, uFormMain;

{$R *.dfm}

procedure TS7WindowDock.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ///

  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;

  Params.WndParent := FormMain.Handle;
end;

procedure TS7WindowDock.FormCreate(Sender: TObject);
begin
  self.Constraints.MinWidth  := 301;
  self.Constraints.MinHeight := 249;
end;

procedure TS7WindowDock.CaptionBarDock(Sender: TObject);
begin
  if not Assigned(FOwnerFrame) then
    Exit();
  ///

  TS7FrameDock(FOwnerFrame).DockCaption.Docked := True;
end;

constructor TS7WindowDock.Create(AOwner : TComponent; AOwnerFrame : TFrame; const ANameEx : String = '');
begin
  inherited Create(AOwner);
  ///

  FNameEx := ANameEx;

  FOwnerFrame := AOwnerFrame;
end;

end.
