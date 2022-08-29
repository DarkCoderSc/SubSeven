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

unit S7CButton;

interface

uses Windows, Classes, VCL.Controls, Messages, VCL.Graphics, S7Classes;

type
  TOnValueChanged = procedure(Sender : TObject; ANewValue : Integer) of object;

  TS7CButton = class(TGraphicControl)
  private
    FOldWindowProc    : TWndMethod;
    FButtonState      : TS7ControlState;
    FOldButtonState   : TS7ControlState;
    FMouseHover       : Boolean;
    FOnClick          : TNotifyEvent;
    FOnValueChanged   : TOnValueChanged;

    FBackground       : TS7StateColors;
    FBorder           : TS7StateColors;

    FValue            : Integer;
    FBusy             : Boolean;
    FOldEnabledValue  : Boolean;

    {@M}
    procedure OnCustomWindowProc(var AMessage : TMessage);

    procedure SetButtonState(AState : TS7ControlState);

    function GetEnabled() : Boolean;
    procedure SetEnabled(AValue : Boolean);

    function GetCaption() : String;
    procedure SetCaption(AValue : String);
    procedure SetValue(AValue : Integer);
    procedure SetBusy(const AValue : Boolean);
    procedure DrawBackground();
    procedure DrawBorder();
    procedure DrawText();
  protected
    {@M}
    procedure Paint(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;


  published
    {@G/S}
    property Visible;
    property Align;
    property Font;

    property Enabled        : Boolean         read GetEnabled      write SetEnabled;
    property Caption        : String          read GetCaption      write SetCaption;
    property Value          : Integer         read FValue          write SetValue;
    property OnClick        : TNotifyEvent    read FOnClick        write FOnClick;
    property OnValueChanged : TOnValueChanged read FOnValueChanged write FOnValueChanged;
    property Busy           : Boolean         read FBusy           write SetBusy;
  end;

implementation

uses System.IniFiles, System.SysUtils;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7CButton


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-----------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7CButton.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  ShowHint := True;

  FOldWindowProc := self.WindowProc;
  self.WindowProc := OnCustomWindowProc;

  ControlStyle := ControlStyle;

  FButtonState := csNormal;
  FMouseHover  := False;

  FOnClick := nil;

  FValue := 0;
  FOnValueChanged := nil;

  FBackground  := TS7StateColors.Create(self);
  FBorder      := TS7StateColors.Create(self);

  FBackground.Normal   := clNone;
  FBackground.Hover    := RGB(233, 241, 251);
  FBackground.Focus    := clNone;
  FBackground.Active   := RGB(201, 224, 247);
  FBackground.Disabled := RGB(240, 240, 240);

  FBorder.Normal   := RGB(218, 219, 220);
  FBorder.Hover    := RGB(157, 201, 252);
  FBorder.Focus    := RGB(157, 201, 252);
  FBorder.Active   := RGB(98, 162, 228);
  FBorder.Disabled := RGB(218, 219, 220);

  FOldEnabledValue := inherited Enabled;
  FBusy            := False;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
destructor TS7CButton.Destroy();
begin
  if Assigned(FBackground) then
    FreeAndNil(FBackground);

  if Assigned(FBorder) then
    FreeAndNil(FBorder);

  if Assigned(FOldWindowProc) then
    self.WindowProc := FOldWindowProc;

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Draw Background
-------------------------------------------------------------------------------}
procedure TS7CButton.DrawBackground();
var AColor         : TColor;
    ARect          : TRect;
    AOldBrushStyle : TBrushStyle;
begin
  AColor := FBackground.GetStateColor(FButtonState);

  if AColor <> clNone then begin
    ARect := self.ClientRect;

    AOldBrushStyle := Canvas.Brush.Style;

    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := AColor;

    {
      Render Background
    }
    Canvas.FillRect(ARect);

    Canvas.Brush.Style := AOldBrushStyle;
  end;
end;

{-------------------------------------------------------------------------------
  Draw Border
-------------------------------------------------------------------------------}
procedure TS7CButton.DrawBorder();
var AColor         : TColor;

    ARect          : TRect;
    AOldBrushStyle : TBrushStyle;
begin
  ARect := Rect(0, 0, self.ClientWidth, self.ClientHeight);
  ///

  AColor := FBorder.GetStateColor(FButtonState);

  if (AColor <> clNone) then begin
    AOldBrushStyle := Canvas.Brush.Style;

    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := AColor;

    Canvas.FrameRect(ARect);

    Canvas.Brush.Style := AOldBrushStyle;
  end;
end;

{-------------------------------------------------------------------------------
  Draw Text
-------------------------------------------------------------------------------}
procedure TS7CButton.DrawText();
var ARect          : TRect;
    ATextFormat    : TTextFormat;
    ACaption       : String;
    AChevron       : String;
begin
  Canvas.Font.Assign(self.Font);

  if FButtonState = csDisabled then
    Canvas.Font.Color := RGB(205, 205, 205)
  else
    Canvas.Font.Color := RGB(57, 57, 57);

  ARect.Left   := 2;
  ARect.Top    := 0;
  ARect.Width  := (ClientWidth - 4);
  ARect.Height := ClientHeight;

  ACaption := inherited Caption;

  Canvas.Brush.Style := bsClear;

  ATextFormat := [tfEndEllipsis, tfVerticalCenter, tfSingleLine];

  ATextFormat := ATextFormat + [tfCenter];

  Canvas.TextRect(ARect, ACaption, ATextFormat);
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TS7CButton.Paint();
begin
  inherited Paint();
  ///

  Canvas.Lock();
  try
    {
      Draw Background
    }
    self.DrawBackground();

    {
      Draw Borders
    }
    self.DrawBorder();

    {
      Draw Text & Chevron
    }
    self.DrawText();
  finally
    Canvas.Unlock();
  end;
end;

{-------------------------------------------------------------------------------
  Override Messages Received to Component
-------------------------------------------------------------------------------}
procedure TS7CButton.OnCustomWindowProc(var AMessage : TMessage);
var APoint : TPoint;
begin
  FOldWindowProc(AMessage);
  ///

  if (csDesigning in ComponentState) then
    Exit;

  if FButtonState = csDisabled then
    Exit();

  case AMessage.Msg of
    {
      Button Click (Down)
    }
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK : begin
      SetButtonState(csActive);
      ///

    end;

    {
      Button Click (Up)
    }
    WM_LBUTTONUP : begin
      APoint.X := TWMLButtonUp(AMessage).XPos;
      APoint.Y := TWMLButtonUp(AMessage).YPos;

      FMouseHover := ptinrect(self.ClientRect, APoint);

      if FMouseHover then
        SetButtonState(csHover)
      else
        SetButtonState(csNormal);
      ///

      {
        Trigger Event
      }
      if Assigned(FOnClick) and FMouseHover then
        FOnClick(self);
    end;

    {
      Surface Move (Enter)
    }
    WM_MOUSEMOVE : begin
      FMouseHover := True;
      ///

      if (FButtonState = csActive) then
        Exit();

      SetButtonState(csHover);
    end;

    {
      Surface Leave
    }
    WM_MOUSELEAVE, {VCL ->} CM_MOUSELEAVE : begin
      FMouseHover := False;
      ///

      if (FButtonState <> csActive) then
        SetButtonState(csNormal);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7CButton.SetButtonState(AState : TS7ControlState);
begin
  if (AState = FButtonState) then
    Exit();
  ///

  FButtonState := AState;

  case FButtonState of
    csNormal: ;
    csHover: ;
    csActive: ;
    csFocus: ;
    csDisabled: ;
  end;

  ///
  Invalidate();
end;

function TS7CButton.GetCaption() : String;
begin
  result := inherited Caption;
end;

procedure TS7CButton.SetCaption(AValue : String);
begin
  inherited Caption := AValue;

  ///
  Invalidate();
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7CButton.SetValue(AValue : Integer);
begin
  if FValue = AValue then
    Exit();
  ///

  FValue := AValue;

  if Assigned(FOnValueChanged) then
    FOnValueChanged(self, AValue);
end;

function TS7CButton.GetEnabled() : Boolean;
begin
  result := inherited Enabled;
end;

procedure TS7CButton.SetEnabled(AValue : Boolean);
begin
  inherited Enabled := AValue;

  if FBusy then
    FOldEnabledValue := AValue;

  if not AValue then
    FButtonState := csDisabled
  else
    FButtonState := csNormal;

  ///
  Invalidate();
end;

procedure TS7CButton.SetBusy(const AValue : Boolean);
begin
  if AValue = FBusy then
    Exit();

  FBusy := AValue;

  if FBusy then begin
    FOldEnabledValue := inherited Enabled;

    inherited Enabled := False;
    FButtonState      := csDisabled
  end else begin
    inherited Enabled := FOldEnabledValue;
    FButtonState      := csNormal;
  end;

  ///
  Invalidate();
end;
end.
