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

unit Sub7.Viewer.VCL.CheckBox;

interface

uses Winapi.Windows, VCL.Controls, System.Classes, VCL.Graphics, Winapi.Messages, S7Theme;

type
  TByteArrayArray = array of array of Byte;

  TCheckBoxMode = (cbmCheckBox, cbmRadioBox);

  TControlState = (csNormal, csHover, csActive);

  TS7CheckBox = class(TCustomControl)
  private
    FMode           : TCheckBoxMode;
    FControlState   : TControlState;

    FMouseHover     : Boolean;

    FOldWindowProc  : TWndMethod;

    FButtonIsDown   : Boolean;

    FChecked        : Boolean;

    FColor          : TColor;

    FHoverColor     : TColor;
    FActiveColor    : TColor;

    FOnStateChanged : TNotifyEvent;

    {@M}
    procedure SetCaption(AValue : String);
    function GetCaption() : String;
    procedure SetColor(AIndex : Integer; AValue : TColor);

    procedure SetMode(AValue : TCheckBoxMode);

    procedure AdjustBound();

    function IsDesigning() : Boolean;

    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure OnCustomWindowProc(var AMessage : TMessage);

    procedure SetControlState(AValue : TControlState);
    procedure SetChecked(AValue : Boolean);

    function GetButtonRect() : TRect;
  protected
    {@M}
    procedure Paint(); override;

    procedure Loaded(); override;
  public
    {@M}
    procedure _SetChecked(const AValue : Boolean);

    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Caption        : String        read GetCaption      write SetCaption;
    property Mode           : TCheckBoxMode read FMode           write SetMode;
    property Checked        : Boolean       read FChecked        write SetChecked;
    property OnStateChanged : TNotifyEvent  read FOnStateChanged write FOnStateChanged;

    property Color       : TColor index 0 read FColor          write SetColor;
    property HoverColor  : TColor index 1 read FHoverColor     write SetColor;
    property ActiveColor : TColor index 2 read FActiveColor    write SetColor;

    property Align;
    property AlignWithMargins;
    property Margins;
    property Visible;
    property Enabled;
    property Font;
  end;

  {
    Glyphs
  }
  const CHECKBOX_GLYPH : TByteArrayArray = [
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $1, $0],
                                              [$0, $0, $0, $0, $0, $0, $1, $1, $0],
                                              [$0, $1, $0, $0, $0, $1, $1, $1, $0],
                                              [$0, $1, $1, $0, $1, $1, $1, $0, $0],
                                              [$0, $1, $1, $1, $1, $1, $0, $0, $0],
                                              [$0, $0, $1, $1, $1, $0, $0, $0, $0],
                                              [$0, $0, $0, $1, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];

  RADIOBOX_GLYPH : TByteArrayArray = [
                                        [$0, $1, $1, $0],
                                        [$1, $1, $1, $1],
                                        [$1, $1, $1, $1],
                                        [$0, $1, $1, $0]
  ];

implementation

{-------------------------------------------------------------------------------
  Update Check / Radio box rect
-------------------------------------------------------------------------------}
function TS7CheckBox.GetButtonRect() : TRect;
begin
  if FMode = cbmCheckBox then begin
    result.Left   := 0;
    result.Top    := (ClientHeight div 2) - 6;
    result.Width  := 11;
    result.Height := 11;
  end else begin
    result.Left   := 0;
    result.Top    := (ClientHeight div 2) - 5;
    result.Width  := 10;
    result.Height := 10;
  end;
end;

{-------------------------------------------------------------------------------
  Mouse Movement Control
-------------------------------------------------------------------------------}

procedure TS7CheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ///

  FButtonIsDown := True;
end;

procedure TS7CheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var APoint : TPoint;
begin
  inherited;
  ///

  if FButtonIsDown then begin
    APoint.X := X;
    APoint.Y := Y;

    if ptinrect(self.ClientRect, APoint) then begin
      self.SetChecked(not FChecked);
    end;
  end;

  FButtonIsDown := False;
end;

{-------------------------------------------------------------------------------
  Hook Messages
-------------------------------------------------------------------------------}
procedure TS7CheckBox.OnCustomWindowProc(var AMessage : TMessage);
var APoint : TPoint;
begin
  FOldWindowProc(AMessage);
  ///

  if (csDesigning in ComponentState) then
    Exit;

  case AMessage.Msg of
    {
      Button Click (Down)
    }
    WM_LBUTTONDOWN : begin
      SetControlState(csActive);
      ///

//      if Assigned(FOnClick) then
//        FOnClick(self);
    end;

    {
      Button Click (Up)
    }
    WM_LBUTTONUP : begin
      APoint.X := TWMLButtonUp(AMessage).XPos;
      APoint.Y := TWMLButtonUp(AMessage).YPos;

      FMouseHover := ptinrect(self.ClientRect, APoint);

      if FMouseHover then
        SetControlState(csHover)
      else
        SetControlState(csNormal);
      ///

      {
        Trigger Event
      }
    end;

    {
      Surface Move (Enter)
    }
    WM_MOUSEMOVE : begin
      FMouseHover := True;
      ///

      if (FControlState = csActive) then
        Exit();

      SetControlState(csHover);
    end;

    {
      Surface Leave
    }
    WM_MOUSELEAVE, {VCL ->} CM_MOUSELEAVE : begin
      FMouseHover := False;
      ///

      if (FControlState <> csActive) then
        SetControlState(csNormal);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Are we currently designing application form ?
-------------------------------------------------------------------------------}
function TS7CheckBox.IsDesigning() : Boolean;
begin
  result := (csDesigning in ComponentState);
end;


{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7CheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  self.Font.Height  := -11;
  self.Font.Name    := FONT_2;
  self.Font.Color   := clWhite;

  ShowHint := True;

  FMode := cbmRadioBox;

  FControlState := csNormal;
  FMouseHover   := False;
  FChecked      := False;

  FOldWindowProc  := self.WindowProc;
  self.WindowProc := OnCustomWindowProc;

  FColor := clBlack;
  FButtonIsDown := False;
  FOnStateChanged := nil;

  FHoverColor  := DARK_BLUE;
  FActiveColor := DARK_BLUE;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7CheckBox.Destroy();
begin
  if Assigned(FOldWindowProc) then
    self.WindowProc := FOldWindowProc;

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Override Loaded Component Event
-------------------------------------------------------------------------------}
procedure TS7CheckBox.Loaded();
begin
  inherited;
  ///

  if NOT self.IsDesigning() then
    AdjustBound();
end;

{-------------------------------------------------------------------------------
  Ajust Client Height following Font Settings
-------------------------------------------------------------------------------}
procedure TS7CheckBox.AdjustBound();
var
  ADC: HDC;
  ASaveFont: HFONT;
  AMetrics: TTextMetric;
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
  ___paint
-------------------------------------------------------------------------------}
procedure TS7CheckBox.Paint();
var ARect       : TRect;
    X, Y        : Integer;
    ALeft       : Integer;
    ACaption    : String;
    ABackground : TColor;
    ABorder     : TColor;

  {
    Draw the CheckBox Glyph in Canvas
  }
  procedure DrawAGlyph(const AGlyphMatrix : TByteArrayArray; const X, Y : Integer; const AGlyphColor : TColor);
  var I, N         : Integer;
      AGlyphWidth  : Integer;
      AGlyphHeight : Integer;
  begin
    AGlyphWidth  := High(AGlyphMatrix[1]) +1;
    AGlyphHeight := High(AGlyphMatrix) +1;
    ///

    for I := 0 to AGlyphWidth -1 do begin
      for N := 0 to AGlyphHeight -1 do begin
        if AGlyphMatrix[N][I] <> 0 then begin
          Canvas.Pixels[(X + I), (Y + N)] := AGlyphColor;
        end;
      end;
    end;
  end;

begin
  Canvas.Lock();
  try
    {
      Draw Background
    }
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FColor;

    Canvas.FillRect(Rect(0, 0, self.ClientWidth, self.ClientHeight));

    {
      Draw Glyph
    }
    Canvas.Pen.Style   := psClear;
    Canvas.Brush.Style := bsClear;
    case FMode of
      cbmCheckBox : begin
        {
          Draw CheckBox Border
        }
        ABorder     := clNone;
        ABackground := clNone;

        case FControlState of
          csNormal : begin
            ABorder     := MAIN_GRAY;
            ABackground := clNone;
          end;

          csHover : begin
            ABorder     := MAIN_GRAY;
            ABackground := FHoverColor;
          end;

          csActive : begin
            ABorder     := MAIN_GRAY;
            ABackground := FActiveColor;
          end;
        end;

        ARect := self.GetButtonRect();

//        ARect.Left   := 0;
//        ARect.Top    := (ClientHeight div 2) - 6;
//        ARect.Width  := 11;
//        ARect.Height := 11;


        if (ABorder <> clNone) then begin
          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := ABorder;

          Canvas.FrameRect(ARect);
        end;

        if (ABackground <> clNone) then begin
          InflateRect(ARect, -1, -1);

          Canvas.Brush.Style := bsSolid;
          Canvas.Brush.Color := ABackground;

          Canvas.FillRect(ARect);

          InflateRect(ARect, 1, 1);
        end;

        {
          Draw Glyph
        }
        if FChecked then begin
          X := (ARect.Left + 1);
          Y := (ARect.Top + 1);

          DrawAGlyph(CHECKBOX_GLYPH, X, Y, MAIN_GRAY);
        end;
      end;

      cbmRadioBox : begin
        {
          Draw RadioBox Border
        }

        ABorder     := clNone;
        ABackground := clNone;

        case FControlState of
          csNormal : begin
            ABorder     := MAIN_GRAY;
            ABackground := clNone;
          end;

          csHover : begin
            ABorder     := MAIN_GRAY;
            ABackground := FHoverColor;
          end;

          csActive : begin
            ABorder     := MAIN_GRAY;
            ABackground := FActiveColor;
          end;
        end;

        if (ABackground <> clNone) then begin
          Canvas.Brush.Color := ABackground;
          Canvas.Brush.Style := bsSolid;
        end;

        if (ABorder <> clNone) then begin
          Canvas.Pen.Color := ABorder;
          Canvas.Pen.Style := psSolid;
        end;                                                   //continuer et faire le check etc... radio quand sur meme panel etc.Self.

        ARect := self.GetButtonRect();
//        ARect.Left   := 0;
//        ARect.Top    := (ClientHeight div 2) - 5;
//        ARect.Width  := 10;
//        ARect.Height := 10;

        Canvas.Ellipse(ARect);

        X := (ARect.Left + 3);
        Y := (ARect.Top + 3);

        if FChecked then begin
          DrawAGlyph(RADIOBOX_GLYPH, X, Y, MAIN_BLUE);
        end;
      end;
    end;

    {
      Draw Caption / Text
    }
    Canvas.Brush.Style := bsClear;

    ACaption := inherited Caption;

    Canvas.Font.Assign(inherited Font);

    ALeft := (ARect.Left + ARect.Width + 6);

    // Calc Required Height for Text
    ARect.Top    := 0;
    ARect.Height := ClientHeight - 2;
    ARect.Left   := ALeft;
    ARect.Width  := (ClientWidth - 6 - ALeft);

    // Then Draw Text
    Canvas.TextRect(ARect, ACaption, [tfSingleLine, tfVerticalCenter, tfEndEllipsis]);
  finally
    Canvas.Unlock();
  end;
end;

{-------------------------------------------------------------------------------
  Get Notified when Font Settings Has Changed
-------------------------------------------------------------------------------}
procedure TS7CheckBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ///

  if NOT self.IsDesigning() and (csLoading in ComponentState) then
    AdjustBound();
end;

{-------------------------------------------------------------------------------
  ___getters___setters
-------------------------------------------------------------------------------}

procedure TS7CheckBox.SetCaption(AValue : String);
begin
  if (AValue = inherited Caption) then
    Exit();
  ///

  inherited Caption := AValue;

  ///
  Invalidate();
end;

procedure TS7CheckBox.SetColor(AIndex : Integer; AValue : TColor);
begin
  case AIndex of
    0 : FColor       := AValue;
    1 : FHoverColor  := AValue;
    2 : FActiveColor := AValue;
  end;

  ///
  Invalidate();
end;

function TS7CheckBox.GetCaption() : String;
begin
  result := inherited Caption;
end;

procedure TS7CheckBox.SetMode(AValue : TCheckBoxMode);
begin
  if (AValue = FMode) then
    Exit();
  ///

  FMode := AValue;

  ///
  Invalidate();
end;

procedure TS7CheckBox.SetControlState(AValue : TControlState);
begin
  if (AValue = FControlState) then
    Exit();
  ///

  FControlState := AValue;

  ///
  Invalidate();
end;

procedure TS7CheckBox.SetChecked(AValue : Boolean);

  procedure UncheckGroupRadio();
  var I : Integer;
      C : TComponent;
  begin
    for i := 0 to self.Owner.ComponentCount -1 do begin
      C := self.Owner.Components[i];

      if C = self then
        continue;

      if not (C is TS7CheckBox) then
        continue;

      if (TS7CheckBox(C).Mode <> cbmRadioBox) then
        continue;

      TS7CheckBox(C)._SetChecked(False);
    end;
  end;

begin
  if AValue = FChecked then
    Exit();

  if (self.Mode = cbmRadioBox) then begin
    UncheckGroupRadio();

    AValue := True; // always for radio
  end;

  _SetChecked(AValue);
end;

procedure TS7CheckBox._SetChecked(const AValue : Boolean);
begin
  FChecked := AValue;

  if Assigned(FOnStateChanged) then
    FOnStateChanged(self);

  ///
  Invalidate();
end;

end.
