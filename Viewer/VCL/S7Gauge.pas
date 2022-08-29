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
unit S7Gauge;

interface

uses System.Classes, VCL.Graphics, VCL.Controls, Winapi.Windows, VCL.ExtCtrls;

type
  TS7GaugeMode = (
                    gmProgressBar,
                    gmMarquee
  );

  TS7GaugeTextMode = (
                        gtmNone,
                        gtmProgress,
                        gtmCustom
  );

  TS7MarqueeDirection = (
                          mdLeftToRight,
                          mdRightToLeft
  );

  TS7GaugeState = (
                      gsNormal,
                      gsError
  );

  TS7Gauge = class(TGraphicControl)
  private
    FBackground       : TColor;
    FBorder           : TColor;
    FForeground       : TColor;
    FBorderWidth      : Integer;

    FMax              : Integer;
    FProgress         : Integer;

    FTextMode         : TS7GaugeTextMode;
    FText             : String;

    FMode             : TS7GaugeMode;

    FState            : TS7GaugeState;

    FMarqueeTimer     : TTimer;
    FMarqueeProgress  : Integer;
    FMarqueeDirection : TS7MarqueeDirection;

    {@M}
    procedure SetColor(const AIndex : Integer; const AColor : TColor);
    procedure SetInteger(const AIndex, AValue : Integer);

    procedure DrawMarquee(const AClientRect : TRect);
    procedure DrawProgress(const AClientRect : TRect);
    procedure DrawText(const AClientRect : TRect);

    procedure SetMode(const AValue : TS7GaugeMode);
    procedure SetText(const AValue : String);
    procedure SetTextMode(const AValue : TS7GaugeTextMode);
    procedure SetState(const AValue : TS7GaugeState);

    procedure OnTimerMarquee(Sender : TObject);
  protected
    {@M}
    procedure paint(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Background  : TColor index 0  read FBackground  write SetColor;
    property Border      : TColor index 1  read FBorder      write SetColor;
    property Foreground  : TColor index 2  read FForeground  write SetColor;

    property BorderWidth : Integer index 0 read FBorderWidth write SetInteger;
    property Max         : Integer index 1 read FMax         write SetInteger;
    property Progress    : Integer index 2 read FProgress    write SetInteger;

    property State    : TS7GaugeState    read FState    write SetState;
    property Mode     : TS7GaugeMode     read FMode     write SetMode;
    property Text     : String           read FText     write SetText;
    property TextMode : TS7GaugeTextMode read FTextMode write SetTextMode;

    property Align;
    property AlignWithMargins;
    property Margins;
    property Visible;
    property Enabled;
    property Font;
  end;

implementation

uses S7Theme, Winapi.GDIPAPI, Winapi.GDIPOBJ, System.SysUtils, System.Math;

{-------------------------------------------------------------------------------
  When Marquee Timer get triggered
-------------------------------------------------------------------------------}
procedure TS7Gauge.OnTimerMarquee(Sender : TObject);
begin
  Inc(FMarqueeProgress);

  if FMarqueeProgress >= 100 then begin
    FMarqueeProgress := 0;

    if FMarqueeDirection = mdLeftToRight then
      FMarqueeDirection := mdRightToLeft
    else
      FMarqueeDirection := mdLeftToRight;
  end;

  ///
  self.Invalidate();
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7Gauge.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FBackground       := clBlack;
  FBorder           := MAIN_GRAY;
  FForeground       := DARK_BLUE;
  FBorderWidth      := 1;
  FMax              := 100;
  FProgress         := 50;

  self.Width        := 300;
  self.Height       := 21;

  Font.Name         := FONT_2;
  Font.Size         := 8;
  Font.Color        := clWhite;

  FText             := '';

  FMode             := gmProgressBar;
  FTextMode         := gtmProgress;
  FState            := gsNormal;

  FMarqueeProgress  := 0;
  FMarqueeDirection := mdLeftToRight;

  self.ShowHint     := True;

  FMarqueeTimer          := TTimer.Create(self);
  FMarqueeTimer.Interval := 10;
  FMarqueeTimer.Enabled  := False;
  FMarqueeTimer.OnTimer  := OnTimerMarquee;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7Gauge.Destroy();
begin
  if Assigned(FMarqueeTimer) then
    FreeAndNil(FMarqueeTimer);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Draw Marquee Animation
-------------------------------------------------------------------------------}
procedure TS7Gauge.DrawMarquee(const AClientRect : TRect);
var AGraphics      : TGPGraphics;
    ABrush         : TGPLinearGradientBrush;
    AGPRect        : TGPRect;
    AProgressWidth : Integer;
    AColor1        : LongInt;
    AColor2        : LongInt;

    function GetColor(AColor : TColor) : Longint;
    begin
      result := MakeColor(
                            GetRValue(AColor),
                            GetGValue(AColor),
                            GetBValue(AColor)
      );
    end;

begin
  AGraphics := TGPGraphics.Create(Canvas.Handle);
  try
    AGPRect.X      := AClientRect.Left;
    AGPRect.Y      := AClientRect.Top;
    AGPRect.Width  := AClientRect.Width;
    AGPRect.Height := AClientRect.Height;

    AProgressWidth := ceil((FMarqueeProgress * AClientRect.Width) div 100);

    if FMarqueeDirection = mdLeftToRight then begin
      AColor1 := GetColor(FBackground);
      AColor2 := GetColor(FForeground);
    end else begin
      AColor1 := GetColor(FForeground);
      AColor2 := GetColor(FBackground);
    end;

    AGPRect.Y      := AClientRect.Top;
    AGPRect.Height := AClientRect.Height;

    if FMarqueeDirection = mdLeftToRight then
      AGPRect.X := AClientRect.Left
    else
      AGPRect.X := AClientRect.Right - AProgressWidth;

    AGPRect.Width := AProgressWidth;

    ABrush := TGPLinearGradientBrush.Create(
                                              AGPRect,
                                              AColor1,
                                              AColor2,
                                              LinearGradientModeHorizontal
    );
    try
      AGraphics.FillRectangle(
                                ABrush,
                                AGPRect
      );
    finally
      if Assigned(ABrush) then
        FreeAndNil(ABrush);
    end;
  finally
    if Assigned(AGraphics) then
      FreeAndNil(AGraphics);
  end;
end;

{-------------------------------------------------------------------------------
  Draw Progress Bar
-------------------------------------------------------------------------------}
procedure TS7Gauge.DrawProgress(const AClientRect : TRect);
var ARect  : TRect;
    AWidth : Integer;
begin
  Canvas.Brush.Color := FForeground;

  ARect.Left  := AClientRect.Left;
  ARect.Top   := AClientRect.Top;

  AWidth := ceil((FProgress * AClientRect.Width) div 100);

  if AWidth > AClientRect.Width then
    AWidth := AClientRect.Width;

  ARect.Height := AClientRect.Height;
  ARect.Width  := AWidth;

  Canvas.FillRect(ARect);
end;

{-------------------------------------------------------------------------------
  Draw Text
-------------------------------------------------------------------------------}
procedure TS7Gauge.DrawText(const AClientRect : TRect);
var AText       : String;
    ATextFormat : TTextFormat;
    ATextRect   : TRect;
begin
  if (FTextMode = gtmNone) then
    Exit();
  ///

  case FTextMode of
    gtmProgress : AText := Format('%d%%', [FProgress]);
    gtmCustom   : AText := FText;
  end;

  ATextRect := AClientRect;

  Canvas.Brush.Style := bsClear;

  ATextFormat := [
                    tfCenter,
                    tfEndEllipsis,
                    tfSingleLine,
                    tfVerticalCenter
  ];

  Canvas.TextRect(ATextRect, AText, ATextFormat);
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TS7Gauge.paint();
var ARect       : TRect;
    AClientRect : TRect;
    ABorder     : TColor;
    AFont       : TFont;
begin
  Canvas.Lock();
  try
    AFont := TFont.Create();
    try
      AFont.Assign(Font);
      ///

      AClientRect.Left   := 0;
      AClientRect.Top    := 0;
      AClientRect.Width  := ClientWidth;
      AClientRect.Height := ClientHeight;

      Canvas.Brush.Style := bsSolid;

      case FState of
        gsNormal : begin
          ABorder := FBorder;
        end;

        gsError : begin
          ABorder     := MAIN_RED;
          AFont.Color := ABorder;
        end;
      end;

      Canvas.Font.Assign(AFont);

      {
        Draw Bg
      }
      if FBackground <> clNone then begin
        Canvas.Brush.Color := FBackground;

        Canvas.FillRect(AClientRect);
      end;

      {
        Draw Border
      }
      if (FBorderWidth > 0) and (ABorder <> clNone) then begin
        Canvas.Brush.Color := ABorder;
        ///

        ARect.Left   := 0;
        ARect.Top    := 0;
        ARect.Width  := ClientWidth;
        ARect.Height := FBorderWidth;

        Canvas.FillRect(ARect);

        ARect.Left   := (ClientWidth - FBorderWidth);
        ARect.Top    := 0;
        ARect.Width  := FBorderWidth;
        ARect.Height := ClientHeight;

        Canvas.FillRect(ARect);

        ARect.Left   := 0;
        ARect.Top    := (ClientHeight - FBorderWidth);
        ARect.Width  := ClientWidth;
        ARect.Height := FBorderWidth;

        Canvas.FillRect(ARect);

        ARect.Left   := 0;
        ARect.Top    := 0;
        ARect.Width  := FBorderWidth;
        ARect.Height := ClientHeight;

        Canvas.FillRect(ARect);

        ///
        InflateRect(AClientRect, -FBorderWidth, -FBorderWidth);
      end;


      {
        Draw Marquee or Progress
      }
      case FMode of
        gmProgressBar : self.DrawProgress(AClientRect);
        gmMarquee     : self.DrawMarquee(AClientRect);
      end;

      {
        Draw Text
      }
      if not ((FMode = gmMarquee) and (FTextMode = gtmProgress)) then
        self.DrawText(AClientRect);
    finally
      if Assigned(AFont) then
        FreeAndNil(AFont);
    end;
  finally
    Canvas.Unlock();
  end;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7Gauge.SetColor(const AIndex : Integer; const AColor : TColor);
begin
  case AIndex of
    0 : FBackground := AColor;
    1 : FBorder     := AColor;
    2 : FForeground := AColor;
  end;

  ///
  self.Invalidate();
end;

procedure TS7Gauge.SetInteger(const AIndex, AValue : Integer);
begin
  case AIndex of
    0 : FBorderWidth := AValue;
    1 : FMax         := AValue;
    2 : FProgress    := AValue;
  end;

  ///
  self.Invalidate();
end;

procedure TS7Gauge.SetMode(const AValue : TS7GaugeMode);
begin
  if FMode = AValue then
    Exit();

  FMode := AValue;

  FMarqueeTimer.Enabled := (FMode = gmMarquee);

  if FMarqueeTimer.Enabled then begin
    FMarqueeProgress  := 0;
    FMarqueeDirection := mdLeftToRight;
  end;

  ///
  self.Invalidate();
end;

procedure TS7Gauge.SetText(const AValue : String);
begin
  if AValue = FText then
    Exit();

  FText := AValue;

  ///
  self.Invalidate();
end;

procedure TS7Gauge.SetTextMode(const AValue : TS7GaugeTextMode);
begin
  if AValue = FTextMode then
    Exit();

  FTextMode := AValue;

  ///
  self.Invalidate();
end;

procedure TS7Gauge.SetState(const AValue : TS7GaugeState);
begin
  if AValue = FState then
    Exit();

  FState := AValue;

  ///
  self.Invalidate();
end;

end.
