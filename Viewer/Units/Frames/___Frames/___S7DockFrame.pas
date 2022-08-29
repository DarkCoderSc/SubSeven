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

(*
    To fix TFrame inheritance:

    /!\
    "In addition to changing base class of TMyFrameTreeView to TMyBaseFrame
    change the first word in the dfm file for TMyFrameTreeView from object to inherited."
    We must also declare this base Frame Class in project properties as this:
      - S7DockFrame in 'Units\Frames\S7DockFrame.pas' {S7FrameDock: TFrame},
    /!\
*)

unit ___S7DockFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ___S7DockWindow, S7ImageButton,
  System.ImageList, Vcl.ImgList, Sub7.Core.Types, S7Panel, Vcl.StdCtrls, S7DockCaption,
  Sub7.Core.Protocol, XSuperObject, ___S7Frame, Sub7.Viewer.Types;

type
  TS7FrameDock = class(TS7Frame)
    DockCaption: TS7DockCaption;
    procedure DockCaptionDockStatusChange(Sender: TObject; ADocked: Boolean);
  private
    FDockForm : TS7WindowDock;
    FNavMenu  : TNavMenu;
  protected
    {@M}
    function GetOwnerForm() : TForm;
  public
    {@C}
    constructor Create(AOwner: TComponent; const ANavMenu : TNavMenu); overload; virtual;
    destructor Destroy(); override;

    {@M}
    procedure Open();
    procedure Render(const ACommand : TS7Command; const AData : ISuperObject); virtual; abstract;
  end;

implementation

uses uFormMain;

{$R *.dfm}

function TS7FrameDock.GetOwnerForm() : TForm;
begin
  if self.DockCaption.Docked then
    result := FormMain
  else
    result := FDockForm;
end;

procedure TS7FrameDock.Open();
begin
  if not self.Visible then
    self.Visible := True;
  ///

  if (not self.DockCaption.Docked) and Assigned(FDockForm) then
    FDockForm.Show();
end;

procedure TS7FrameDock.DockCaptionDockStatusChange(Sender: TObject;
  ADocked: Boolean);
begin
  if ADocked then begin
    {
      Attached to Main Form
    }
    self.Parent := TWinControl(self.GetOwner);

    self.Visible := (FNavMenu = FormMain.ActiveMenu);

    FDockForm.Hide();
  end else begin
    {
      Dettached from Main Form
    }
    self.Parent := FDockForm;

    FDockForm.Show();
  end;

  ///
  TS7DockCaption(Sender).Visible := ADocked;

  FormMain.SettingHandler.Write('docked', ADocked, self.ClassName);
end;

constructor TS7FrameDock.Create(AOwner: TComponent; const ANavMenu : TNavMenu);
var ADocked : Boolean;
begin
  inherited Create(AOwner);

  self.Parent  := TWinControl(AOwner);
  self.Visible := False;
  self.Align   := alClient;
  FNavMenu     := ANavMenu;

  ///
  FDockForm := TS7WindowDock.Create(Application.MainForm, self, self.ClassName);
  FDockForm.CaptionBar.Caption := self.DockCaption.Caption;

  ADocked := FormMain.SettingHandler.Read('docked', True, self.ClassName);

  // Change default behaviour
  self.DockCaption.DefaultDocked := ADocked;
  if not ADocked then begin
    self.Parent              := FDockForm;
    self.DockCaption.Visible := False;
  end;
end;

destructor TS7FrameDock.Destroy();
begin
  if Assigned(FDockForm) then
    FDockForm.Release();

  ///
  inherited Destroy();
end;

end.
