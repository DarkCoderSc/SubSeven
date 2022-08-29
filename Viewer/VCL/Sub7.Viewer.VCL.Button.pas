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

unit Sub7.Viewer.VCL.Button;

interface

uses Winapi.Windows, System.Classes, VCL.Controls, Winapi.Messages, VCL.Graphics, S7Theme,
     S7Classes;

type
  TS7Button = class;

  TTextAlign   = (taLeft, taCenter, taRight);

  TOnValueChanged = procedure(Sender : TObject; ANewValue : Integer) of object;

  TS7Button = class(TGraphicControl)
  private
    FOldWindowProc       : TWndMethod;
    FButtonState         : TS7ControlState;
    FOldButtonState      : TS7ControlState;
    FMouseHover          : Boolean;
    FTextAlign           : TTextAlign;
    FDown                : Boolean;
    FChevron             : Boolean;
    FOnClick             : TNotifyEvent;
    FOnValueChanged      : TOnValueChanged;

    FBackground          : TS7StateColors;
    FOuterBorder         : TS7StateColors;

    FOldEnabledValue     : Boolean;
    FBusy                : Boolean;

    FValue               : Integer;

    {@M}
    procedure OnCustomWindowProc(var AMessage : TMessage);

    procedure SetButtonState(AState : TS7ControlState);
    procedure SetTextAlign(AValue : TTextAlign);
    procedure SetDown(AValue : Boolean);
    procedure SetChevron(AValue : Boolean);

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
    property TextAlign      : TTextAlign      read FTextAlign      write SetTextAlign;
    property Down           : Boolean         read FDown           write SetDown;
    property Chevron        : Boolean         read FChevron        write SetChevron;
    property Caption        : String          read GetCaption      write SetCaption;
    property Value          : Integer         read FValue          write SetValue;
    property OnClick        : TNotifyEvent    read FOnClick        write FOnClick;
    property OnValueChanged : TOnValueChanged read FOnValueChanged write FOnValueChanged;
    property Busy           : Boolean         read FBusy           write SetBusy;
  end;

implementation

uses System.IniFiles, System.SysUtils;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7Button


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-----------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7Button.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  ShowHint := True;

  FOldWindowProc := self.WindowProc;
  self.WindowProc := OnCustomWindowProc;

  ControlStyle := ControlStyle;

  FButtonState := csNormal;
  FMouseHover  := False;
  FTextAlign   := taCenter;
  FDown        := False;
  FChevron     := False;

  FOnClick := nil;

  FValue := 0;
  FOnValueChanged := nil;

  FBackground  := TS7StateColors.Create(self);
  FOuterBorder := TS7StateColors.Create(self);

  FBackground.Normal   := clNone;
  FBackground.Hover    := RGB(0, 0, 128);
  FBackground.Focus    := clNone;
  FBackground.Active   := MAIN_BLUE;
  FBackground.Disabled := clNone;

  FOuterBorder.Normal   := MAIN_BLUE;
  FOuterBorder.Hover    := MAIN_BLUE;
  FOuterBorder.Focus    := MAIN_BLUE;
  FOuterBorder.Active   := MAIN_BLUE;
  FOuterBorder.Disabled := clGray;

  self.Font.Size        := 9;
  self.Font.Name        := FONT_2;

  FOldEnabledValue      := inherited Enabled;
  FBusy                 := False;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
destructor TS7Button.Destroy();
begin
  if Assigned(FBackground) then
    FreeAndNil(FBackground);

  if Assigned(FOuterBorder) then
    FreeAndNil(FOuterBorder);

  if Assigned(FOldWindowProc) then
    self.WindowProc := FOldWindowProc;

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Draw Background
-------------------------------------------------------------------------------}
procedure TS7Button.DrawBackground();
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
      Render Transparent Innerborder
    }
    Canvas.FrameRect(ARect);

    InflateRect(ARect, -2, -2);

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
procedure TS7Button.DrawBorder();
var AColor         : TColor;

    ARect          : TRect;
    AOldBrushStyle : TBrushStyle;
begin
  ARect := Rect(0, 0, self.ClientWidth, self.ClientHeight);
  ///

  AColor := FOuterBorder.GetStateColor(FButtonState);

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
procedure TS7Button.DrawText();
var ARect          : TRect;
    ATextFormat    : TTextFormat;
    ACaption       : String;
    AChevron       : String;

    ATextDownDelta : Integer;
begin
  ATextDownDelta := 0;
  if (FButtonState = csActive) then
    ATextDownDelta := 1;
  ///

  Canvas.Font.Assign(self.Font);

  if FButtonState = csDisabled then
    Canvas.Font.Color := clGray
  else
    Canvas.Font.Color := clWhite;

  ARect.Left   := 2;
  ARect.Top    := (ATextDownDelta);
  ARect.Width  := (ClientWidth - 4);
  ARect.Height := ClientHeight;

  if FChevron then
    ARect.Width := (ARect.Width - 16);

  ACaption := inherited Caption;

  Canvas.Brush.Style := bsClear;

  ATextFormat := [tfEndEllipsis, tfVerticalCenter, tfSingleLine];

  case FTextAlign of
    taLeft   : ATextFormat := ATextFormat + [tfLeft];
    taCenter : ATextFormat := ATextFormat + [tfCenter];
    taRight  : ATextFormat := ATextFormat + [tfRight];
  end;

  Canvas.TextRect(ARect, ACaption, ATextFormat);

  // @Chevron
  if FChevron then begin
    ARect.Left   := (ClientWidth - 16);
    ARect.Top    := (ATextDownDelta);
    ARect.Width  := 16;
    ARect.Height := ClientHeight;

    AChevron := '>';

    Canvas.TextRect(ARect, AChevron, [tfSingleLine, tfVerticalCenter]);
  end;

  {
    If we are down, we strike out
  }
  if FDown then begin
    Canvas.Pen.Width := 1;

    Canvas.Pen.Color := Canvas.Font.Color;

    Canvas.MoveTo(8, ((ClientHeight div 2) + 1) + ATextDownDelta);

    Canvas.LineTo((ClientWidth - 8), ((ClientHeight div 2) + 1) + ATextDownDelta);
  end;
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TS7Button.Paint();
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
procedure TS7Button.OnCustomWindowProc(var AMessage : TMessage);
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

procedure TS7Button.SetButtonState(AState : TS7ControlState);
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

procedure TS7Button.SetTextAlign(AValue : TTextAlign);
begin
  if AValue = FTextAlign then
    Exit();
  ///

  FTextAlign := AValue;

  Invalidate();
end;

procedure TS7Button.SetDown(AValue : Boolean);
begin
  if AValue = FDown then
    Exit();
  ///

  if AValue then
    self.Font.Style := [fsItalic]
  else
    self.Font.Style := [];

  FDown := AValue;

  Invalidate();
end;

procedure TS7Button.SetChevron(AValue : Boolean);
begin
  if AValue = FChevron then
    Exit();
  ///

  FChevron := AValue;

  Invalidate();
end;

function TS7Button.GetCaption() : String;
begin
  result := inherited Caption;
end;

procedure TS7Button.SetCaption(AValue : String);
begin
  inherited Caption := AValue;

  ///
  Invalidate();
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7Button.SetValue(AValue : Integer);
begin
  if FValue = AValue then
    Exit();
  ///

  FValue := AValue;

  if Assigned(FOnValueChanged) then
    FOnValueChanged(self, AValue);
end;

function TS7Button.GetEnabled() : Boolean;
begin
  result := inherited Enabled;
end;

procedure TS7Button.SetEnabled(AValue : Boolean);
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

procedure TS7Button.SetBusy(const AValue : Boolean);
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
