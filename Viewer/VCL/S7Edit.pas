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

// Strongly Inspired by TFlatEditUnit.pas from FlatStyle Components (Like Orignal Sub7)

unit S7Edit;

interface

uses Winapi.Windows, System.Classes, VCL.Controls, VCL.StdCtrls, Winapi.Messages, VCL.Graphics,
     VCL.Forms, S7Classes, S7Types;

type
  TS7Edit = class(TEdit)
  private
    FMouseHover       : Boolean;

    FBackground       : TS7StateColors;
    FInnerBorder      : TS7StateColors;
    FOuterBorder      : TS7StateColors;

    FEditStatus       : TControlStatus;

    FValidators       : TValidators;

    FAlternativeTheme : Boolean;

    {@M}
    procedure WMNCPaint(var AMessage: TWMNCPaint);      message WM_NCPAINT;
    procedure CMMouseEnter(var AMessage: TMessage);     message CM_MOUSEENTER;
    procedure CMMouseLeave(var AMessage: TMessage);     message CM_MOUSELEAVE;
    procedure WMSetFocus(var AMessage: TWMSetFocus);    message WM_SETFOCUS;
    procedure WMKillFocus(var AMessage: TWMKillFocus);  message WM_KILLFOCUS;
    procedure CMEnabledChanged(var AMessage: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var AMessage: TMessage);    message CM_FONTCHANGED;

    procedure DrawFlatBorder(ARegion : HRGN);

    function IsDesigning() : Boolean;

    procedure AdjustBound();

    procedure SetAlternativeTheme(const AValue : Boolean);
    procedure SetEditStatus(const AValue : TControlStatus);

    procedure SetEnabled(const AValue : Boolean);
    function GetEnabled() : Boolean;
    function GetIsEmpty() : Boolean;

    procedure SetValidators(const AValue : TValidators);
    function GetIsValid() : Boolean;

    procedure DoValidate();
  protected
    {@M}
    procedure Loaded(); override;
    procedure Change(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@G}
    property IsValid : Boolean read GetIsValid;
  published
    {@G/S}
    property AlternativeTheme : Boolean        read FAlternativeTheme write SetAlternativeTheme;
    property Enabled          : Boolean        read GetEnabled        write SetEnabled;
    property Status           : TControlStatus read FEditStatus       write SetEditStatus;
    property Validators       : TValidators    read FValidators       write SetValidators;

    {@G}
    property IsEmpty : Boolean read GetIsEmpty;
  end;

implementation

uses S7Theme, System.SysUtils, S7Validators;

{-------------------------------------------------------------------------------
  Validate Input
-------------------------------------------------------------------------------}
procedure TS7Edit.DoValidate();
begin
  if Validate(self.Text, FValidators) then
    self.Status := csNormal
  else
    self.Status := csError;
end;

{-------------------------------------------------------------------------------
  Ajust Client Height following Font Settings
-------------------------------------------------------------------------------}
procedure TS7Edit.AdjustBound();
var ADC       : HDC;
    ASaveFont : HFONT;
    AMetrics  : TTextMetric;
begin
  ADC := GetDC(0);
  try
    ASaveFont := SelectObject(ADC, self.Font.Handle);
    GetTextMetrics(ADC, AMetrics);
    SelectObject(ADC, ASaveFont);
  finally
    ReleaseDC(0, ADC);
  end;

  ///
  Height := (AMetrics.tmHeight + 6);
end;

{-------------------------------------------------------------------------------
  Override Loaded Component Event
-------------------------------------------------------------------------------}
procedure TS7Edit.Loaded();
begin
  inherited;
  ///

  if NOT self.IsDesigning() then
    AdjustBound();
end;

{-------------------------------------------------------------------------------
  ___change
-------------------------------------------------------------------------------}
procedure TS7Edit.Change();
begin
  inherited;
  ///

  if (self.Status = csError) then
    self.DoValidate();
end;

{-------------------------------------------------------------------------------
  Are we currently designing application form ?
-------------------------------------------------------------------------------}
function TS7Edit.IsDesigning() : Boolean;
begin
  result := (csDesigning in ComponentState);
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7Edit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  self.Color      := clBlack;
  self.Font.Color := clWhite;
  self.Font.Name  := FONT_2;

  FEditStatus := csNormal;

  self.ShowHint := True;

  //self.BorderStyle  := bsNone;
  self.ControlStyle := ControlStyle - [csFramed];
  self.Ctl3D        := False;
  self.AutoSize     := False;

  FMouseHover := False;

  FBackground  := TS7StateColors.Create(self);
  FInnerBorder := TS7StateColors.Create(self);
  FOuterBorder := TS7StateColors.Create(self);

  FValidators := [];

  SetAlternativeTheme(False);
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7Edit.Destroy();
begin
  if Assigned(FBackground) then
    FreeAndNil(FBackground);

  if Assigned(FInnerBorder) then
    FreeAndNil(FInnerBorder);

  if Assigned(FOuterBorder) then
    FreeAndNil(FOuterBorder);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Draw Flat Border
-------------------------------------------------------------------------------}
procedure TS7Edit.DrawFlatBorder(ARegion : HRGN);
var ADC         : HDC;
    ARect       : TRect;
    AOuterBrush : HBRUSH;
    AInnerBrush : HBRUSH;
    AInnerColor : LongInt;
    AOuterColor : LongInt;
    ABackground : TColor;
begin
  ADC := GetWindowDC(self.Handle);
  try
    GetWindowRect(self.Handle, ARect);
    ///

    OffsetRect(ARect, -ARect.Left, -ARect.Top);

    {
      Define Colors
    }
    if self.Focused then begin
      AOuterColor := ColorToRGB(FOuterBorder.Focus);
      AInnerColor := ColorToRGB(FInnerBorder.Focus);
      ABackground := FBackground.Focus;
    end else begin
      if FMouseHover then begin
        AOuterColor := ColorToRGB(FOuterBorder.Hover);
        AInnerColor := ColorToRGB(FInnerBorder.Hover);
        ABackground := FBackground.Hover;
      end else begin
        AOuterColor := ColorToRGB(FOuterBorder.Normal);
        AInnerColor := ColorToRGB(FInnerBorder.Normal);
        ABackground := FBackground.Normal;
      end;
    end;

    if AOuterColor = clNone then
      AOuterColor := ABackground;

    if AInnerColor = clNone then
      AInnerColor := ABackground;

    // Override Outer Color if status is <> Normal
    if (FEditStatus <> csNormal) and enabled then begin
      case FEditStatus of
        csError : AOuterColor := MAIN_RED;
      end;
    end;

    self.Color := ABackground; // bgcolor

    AOuterBrush := CreateSolidBrush(AOuterColor);
    AInnerBrush := CreateSolidBrush(AInnerColor);
    try
      FrameRect(ADC, ARect, AOuterBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(ADC, ARect, AInnerBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(ADC, ARect, AInnerBrush);
    finally
      DeleteObject(AOuterBrush);
      DeleteObject(AInnerBrush);
    end;
  finally
    ReleaseDC(self.Handle, ADC);
  end;
end;

{-------------------------------------------------------------------------------
  Handle and Override Messages.
-------------------------------------------------------------------------------}

procedure TS7Edit.WMNCPaint(var AMessage: TWMNCPaint);
begin
  inherited;
  ///

  DrawFlatBorder(AMessage.RGN);
end;

procedure TS7Edit.CMMouseEnter(var AMessage: TMessage);
begin
  inherited;
  ///

  if (GetActiveWindow = 0) then
    Exit();
  ///

  FMouseHover := True;

  DrawFlatBorder(0);
end;

procedure TS7Edit.CMMouseLeave(var AMessage: TMessage);
begin
  inherited;
  ///

  FMouseHover := false;

  DrawFlatBorder(0);
end;

procedure TS7Edit.WMSetFocus(var AMessage: TWMSetFocus);
begin
  inherited;
  if NOT self.IsDesigning() then
    DrawFlatBorder(0);
end;

procedure TS7Edit.WMKillFocus(var AMessage: TWMKillFocus);
begin
  inherited;
  ///

  if NOT self.IsDesigning() then
    DrawFlatBorder(0);
end;

procedure TS7Edit.CMEnabledChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  DrawFlatBorder(0);
end;

procedure TS7Edit.CMFontChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  if NOT self.IsDesigning() and (csLoading in ComponentState) then
    AdjustBound();
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7Edit.SetAlternativeTheme(const AValue : Boolean);
begin
  FAlternativeTheme := AValue;

  if FAlternativeTheme then begin
    FBackground.Normal := DARK_BLUE;
    FBackground.Hover  := DARK_BLUE;
    FBackground.Focus  := DARK_BLUE;

    FInnerBorder.Normal := DARK_BLUE;
    FInnerBorder.Hover  := DARK_BLUE;
    FInnerBorder.Focus  := DARK_BLUE;

    FOuterBorder.Normal := DARK_BLUE;
    FOuterBorder.Hover  := DARK_BLUE;
    FOuterBorder.Focus  := DARK_BLUE;
  end else begin
    FBackground.Normal := clBlack;
    FBackground.Hover  := clBlack;
    FBackground.Focus  := DARK_BLUE;

    FInnerBorder.Normal := clBlack;
    FInnerBorder.Hover  := clBlack;
    FInnerBorder.Focus  := DARK_BLUE;

    FOuterBorder.Normal := MAIN_GRAY;
    FOuterBorder.Hover  := MAIN_GRAY;
    FOuterBorder.Focus  := MAIN_GRAY;
  end;

  ///
  Invalidate();
end;

function TS7Edit.GetEnabled() : Boolean;
begin
  result := inherited Enabled;
end;

function TS7Edit.GetIsEmpty() : Boolean;
begin
  result := (Length(Trim(self.text)) = 0);
end;

procedure TS7Edit.SetEnabled(const AValue : Boolean);
begin
  if AValue = inherited Enabled then
    Exit();

  inherited Enabled := AValue;

  if AValue then
    self.Font.Color := clWhite
  else
    self.Font.Color := clGray;
end;

procedure TS7Edit.SetValidators(const AValue : TValidators);
begin
  if AValue = FValidators then
    Exit();
  ///

  FValidators := AValue;

  ///
  Invalidate();
end;

procedure TS7Edit.SetEditStatus(const AValue : TControlStatus);
begin
  if AValue = FEditStatus then
    Exit();
  ///

  FEditStatus := AValue;

  ///
  DrawFlatBorder(0);
  Invalidate();
end;

function TS7Edit.GetIsValid() : Boolean;
begin
  self.DoValidate();
  ///

  result := (self.Status = csNormal);
end;

end.
