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

unit ___S7BaseForm;

interface

uses VCL.Forms, System.Classes, Winapi.Windows, System.SysUtils, Winapi.Messages,
     VCL.Controls, Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.CaptionBar;

type
  TS7BaseForm = class(TForm)
  private
    FFirstShow    : Boolean;
    FDefaultBound : TRect;

    {@M}
    function GetCaptionBarInstance() : TS7CaptionBar;
    function IsFormMaximizable() : Boolean;
    function IsFormMaximized() : Boolean;
  protected
    FReleaseOnClose : Boolean;
    FNameEx         : String;

    {@M}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure Resize; override;
    function IsFormResizable() : Boolean;
    function GetFullIdentifier() : String;

    ///

    procedure CMEnabledChanged(var AMessage: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var AMessage: TMessage); message CM_VISIBLECHANGED;

    procedure DoResize(); virtual;

    procedure OnShow(); virtual;
    procedure OnHide(); virtual;
    procedure OnEnabled(); virtual;
    procedure OnDisabled(); virtual;

    procedure LoadFromSettings(); virtual;
    procedure SaveToSettings(); virtual;

    procedure AfterConstruction(); override;
    procedure DoDestroy(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
  end;

implementation

uses uFormMain;

{ TS7BaseForm.Loaded }

procedure TS7BaseForm.AfterConstruction();
begin
  inherited AfterConstruction();
  ///


end;

{ TS7BaseForm.GetFullIdentifier }

function TS7BaseForm.GetFullIdentifier() : String;
begin
  if FNameEx <> '' then
    result := Format('%s_%s', [self.ClassName, FNameEx])
  else
    result := self.ClassName;
end;

{ TS7BaseForm.Create }

constructor TS7BaseForm.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.Position              := poOwnerFormCenter;
  FReleaseOnClose            := False;
  self.Constraints.MinWidth  := Width;
  self.Constraints.MinHeight := Height;
  FFirstShow                 := True;

  FDefaultBound              := self.BoundsRect;
  FNameEx                    := '';

  ///
  self.LoadFromSettings();
end;


{ TS7BaseForm.Resize }

procedure TS7BaseForm.Resize();
begin
  inherited;
  ///

  self.DoResize();
end;

{ TS7BaseForm.CMEnabledChanged }

procedure TS7BaseForm.CMEnabledChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  if self.Enabled then
    self.OnEnabled()
  else
    self.OnDisabled();
end;

{ TS7BaseForm.CMVisibleChanged }

procedure TS7BaseForm.CMVisibleChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  if self.Visible then begin
    self.OnShow();

    self.DoResize();
  end else
    self.OnHide();
end;

{ TS7BaseForm.CreateParams }

procedure TS7BaseForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  ///

  if FormMain <> self then begin
    Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;

    Params.WndParent := FormMain.Handle;
  end;
end;

{ TS7BaseForm.BeforeDestruction }

procedure TS7BaseForm.DoDestroy();
begin
  self.SaveToSettings();
  ///

  inherited;
end;

{ TS7BaseForm.DoClose }

procedure TS7BaseForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  ///

  if FReleaseOnClose then
    Action := caFree;
end;

{-------------------------------------------------------------------------------
  ___override_me
-------------------------------------------------------------------------------}

{ TS7BaseForm.DoResize }

procedure TS7BaseForm.DoResize();
begin
  ///
end;

{ TS7BaseForm.OnShow }

procedure TS7BaseForm.OnShow();
begin
  ///


end;

{ TS7BaseForm.OnHide }

procedure TS7BaseForm.OnHide();
begin
  ///
end;

{ TS7BaseForm.OnEnabled }

procedure TS7BaseForm.OnEnabled();
begin
  ///
end;

{ TS7BaseForm.OnDisabled }

procedure TS7BaseForm.OnDisabled();
begin
  ///
end;

{ TS7BaseForm.IsFormResizable }

function TS7BaseForm.IsFormResizable() : Boolean;
var I : Integer;
begin
  result := False;
  for I := 0 to self.ComponentCount -1 do begin
    if self.Components[I] is TS7Form then begin
      result := TS7Form(self.Components[i]).Resizable;

      break;
    end;
  end;
end;

{ TS7BaseForm.GetCaptionBarInstance }

function TS7BaseForm.GetCaptionBarInstance() : TS7CaptionBar;
var I : Integer;
begin
  result := nil;
  ///

  for I := 0 to self.ComponentCount -1 do begin
    if self.Components[i] is TS7CaptionBar then begin
      result := TS7CaptionBar(self.Components[i]);

      break;
    end;
  end;
end;

{ TS7BaseForm.IsFormMaximizable }

function TS7BaseForm.IsFormMaximizable() : Boolean;
var ACaptionBar : TS7CaptionBar;
begin
  ACaptionBar := self.GetCaptionBarInstance();
  if not Assigned(ACaptionBar) then
    Exit(False);
  ///

  result := biMaximize in ACaptionBar.BorderIcons;
end;

{ TS7BaseForm.IsFormMaximized }

function TS7BaseForm.IsFormMaximized() : Boolean;
var ACaptionBar : TS7CaptionBar;
begin
  ACaptionBar := self.GetCaptionBarInstance();
  if not Assigned(ACaptionBar) then
    Exit(False);
  ///

  result := ACaptionBar.Maximized;
end;

{ TS7BaseForm.LoadFromSettings }

procedure TS7BaseForm.LoadFromSettings();
var ARect       : TRect;
    ACaptionBar : TS7CaptionBar;
begin
  ARect.Left   := FormMain.SettingHandler.Read('x', FDefaultBound.Left, self.GetFullIdentifier);
  ARect.Top    := FormMain.SettingHandler.Read('y', FDefaultBound.Top, self.GetFullIdentifier);
  ARect.Width  := FDefaultBound.Width;
  ARect.Height := FDefaultBound.Height;


  if self.IsFormResizable() then begin
    ARect.Width  := FormMain.SettingHandler.Read('w', FDefaultBound.Width, self.GetFullIdentifier);
    ARect.Height := FormMain.SettingHandler.Read('h', FDefaultBound.Height, self.GetFullIdentifier);
  end;

  if not Winapi.Windows.EqualRect(ARect, FDefaultBound) then begin
    self.Position := poDesigned;

    self.BoundsRect := ARect;
  end;

  ///

  if FormMain.SettingHandler.Read('maximized', False, self.GetFullIdentifier) and self.IsFormMaximizable then begin
    ACaptionBar := self.GetCaptionBarInstance();
    if Assigned(ACaptionBar) then
      ACaptionBar.Maximize();
  end;
end;

{ TS7BaseForm.SaveToSettings }

procedure TS7BaseForm.SaveToSettings();
var AMaximized  : Boolean;
    ACaptionBar : TS7CaptionBar;
    ARect       : TRect;
begin
  AMaximized := self.IsFormMaximized and self.IsFormMaximizable;
  ///

  if self.IsFormMaximizable then
    FormMain.SettingHandler.Write('maximized', AMaximized, self.GetFullIdentifier);

  ACaptionBar := self.GetCaptionBarInstance();

  if AMaximized and Assigned(ACaptionBar) then
    ARect := ACaptionBar.OldBoundRect
  else
    ARect := self.BoundsRect;

  FormMain.SettingHandler.Write('x', ARect.Left, self.GetFullIdentifier);
  FormMain.SettingHandler.Write('y', ARect.Top, self.GetFullIdentifier);
  ///

  if self.IsFormResizable() then begin
    FormMain.SettingHandler.Write('w', ARect.Width, self.GetFullIdentifier);
    FormMain.SettingHandler.Write('h', ARect.Height, self.GetFullIdentifier);
  end;
end;

end.
