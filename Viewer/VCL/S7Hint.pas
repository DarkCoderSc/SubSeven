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

// Jade <3

// Inspired by TFlatStyle -> TFlatHintUnit.pas

unit S7Hint;

interface

uses System.Classes, VCL.Controls, Winapi.Windows, VCl.Graphics;

type
  TByteArrayArray = array of array of Byte;

  TArrowPos = (apBottomRight, apBottomLeft, apTopRight, apTopLeft);

  THintInfo = record
    HintShow       : Boolean;
    HintShortPause : Integer;
    HintPause      : Integer;
    HintHidePause  : Integer;
  end;

  TS7Hint = class(TComponent)
  private
    FActive             : Boolean;

    FOldHintWindowClass : THintWindowClass;
    FOldHintInfo        : THintInfo;

    {@M}
    procedure SetActive(const AValue : Boolean);
  public
    {@C}
    constructor Create(AOwner : TComponent);
  published
    {@G/S}
    property Active : Boolean read FActive write SetActive;
  end;

  TS7HintWindow = class(THintWindow)
  private
    FArrowPos: TArrowPos;
  protected
    {@M}
    procedure CreateParams(var AParams: TCreateParams); override;

    procedure Paint(); override;
  public
    {@M}
    procedure ActivateHint(AHintRect: TRect; const AHint: String); override;
  end;

implementation

uses VCL.Forms, S7Theme, System.Types;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


   TS7Hint


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7Hint.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FActive := False;

  ZeroMemory(@FOldHintInfo, SizeOf(THintInfo));

  FOldHintWindowClass := nil;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}
procedure TS7Hint.SetActive(const AValue : Boolean);
begin
  if FActive = AValue then
    Exit();

  FActive := AValue;

  if (csDesigning in ComponentState) then
    Exit;

  if FActive then begin
    FOldHintWindowClass := HintWindowClass;
    HintWindowClass := TS7HintWindow;

    FOldHintInfo.HintShow       := Application.ShowHint;
    FOldHintInfo.HintShortPause := Application.HintShortPause;
    FOldHintInfo.HintPause      := Application.HintPause;
    FOldHintInfo.HintHidePause  := Application.HintHidePause;

    Application.ShowHint       := FActive;
    Application.HintShortPause := 25;
    Application.HintPause      := 500;
    Application.HintHidePause  := 5000;
  end else begin
    Application.ShowHint       := FOldHintInfo.HintShow;
    Application.HintShortPause := FOldHintInfo.HintShortPause;
    Application.HintPause      := FOldHintInfo.HintPause;
    Application.HintHidePause  := FOldHintInfo.HintHidePause;

    ZeroMemory(@FOldHintInfo, SizeOf(THintInfo));
  end;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


   TS7HintWindow


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  Modify Window Attributes
-------------------------------------------------------------------------------}
procedure TS7HintWindow.CreateParams(var AParams: TCreateParams);
begin
  inherited CreateParams(AParams);
  ///

  AParams.Style := AParams.Style - WS_BORDER;

  AParams.WindowClass.Style := AParams.WindowClass.Style - CS_DROPSHADOW;
end;

{-------------------------------------------------------------------------------
  Modify Window Attributes
-------------------------------------------------------------------------------}
procedure TS7HintWindow.ActivateHint(AHintRect: TRect; const AHint: String);
var ACurWidth       : Integer;
    APoint          : TPoint;
    AHintHeight     : Integer;
    AHintWidth      : Integer;

    ATopLeftRect    : TRect;
    ATopRightRect   : TRect;
    ABottomLeftRect : TRect;

const HINT_WIDTH = 200;
begin
  Caption := AHint;

  Canvas.Font.Name  := FONT_2;
  Canvas.Font.Size  := 9;
  Canvas.Font.Color := clWhite;

  AHintRect.Right := AHintRect.Left + HINT_WIDTH - 22;

  DrawText(Canvas.Handle, @AHint[1], Length(AHint), AHintRect, DT_CALCRECT or DT_WORDBREAK or DT_NOPREFIX);

  Inc(AHintRect.Right, 22);
  Inc(AHintRect.Bottom, 6);

  ATopLeftRect    :=  Rect(0, 0, Screen.Width div 2, Screen.Height div 2);
  ATopRightRect   :=  Rect(Screen.Width div 2, 0, Screen.Width, Screen.Height div 2);
  ABottomLeftRect := Rect(0, Screen.Height div 2, Screen.Width div 2, Screen.Height);

  GetCursorPos(APoint);

  if PtInRect(ATopLeftRect, APoint) then
    FArrowPos := apTopLeft
  else if PtInRect(ATopRightRect, APoint) then
    FArrowPos := apTopRight
  else if PtInRect(ABottomLeftRect, APoint) then
    FArrowPos := apBottomLeft
  else
    FArrowPos := apBottomRight;

  if FArrowPos = apTopLeft then
    ACurWidth := 12
  else
    ACurWidth := 5;

  AHintHeight := AHintRect.Bottom - AHintRect.Top;
  AHintWidth  := AHintRect.Right - AHintRect.Left;

  case FArrowPos of
    apTopLeft     : AHintRect := Rect(APoint.x + ACurWidth, APoint.y + ACurWidth, APoint.x + AHintWidth + ACurWidth, APoint.y + AHintHeight + ACurWidth);
    apTopRight    : AHintRect := Rect(APoint.x - AHintWidth - ACurWidth, APoint.y + ACurWidth, APoint.x - ACurWidth, APoint.y + AHintHeight + ACurWidth);
    apBottomLeft  : AHintRect := Rect(APoint.x + ACurWidth, APoint.y - AHintHeight - ACurWidth, APoint.x + AHintWidth + ACurWidth, APoint.y - ACurWidth);
    apBottomRight : AHintRect := Rect(APoint.x - AHintWidth - ACurWidth, APoint.y - AHintHeight - ACurWidth, APoint.x - ACurWidth, APoint.y - ACurWidth);
  end;

  BoundsRect := AHintRect;

  APoint := ClientToScreen(Point(0, 0));

  {
    Spawn and Place Window
  }
  SetWindowPos(
                self.Handle,
                HWND_TOPMOST,
                APoint.X,
                APoint.Y,
                0,
                0,
                (SWP_SHOWWINDOW or SWP_NOACTIVATE or SWP_NOSIZE)
  );
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TS7HintWindow.Paint();
var AArrowAreaRect : TRect;
    ATextRect      : TRect;
    AArrowPos      : TPoint;
begin
  case FArrowPos of
    apTopLeft, apBottomLeft:
      begin
        AArrowAreaRect := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Left + 15, ClientRect.Bottom - 1);
        ATextRect      := Rect(ClientRect.Left + 15, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 1);
      end;

    apTopRight, apBottomRight:
      begin
        AArrowAreaRect := Rect(ClientRect.Right - 15, ClientRect.Top + 1, ClientRect.Right - 1, ClientRect.Bottom - 1);
        ATextRect      := Rect(ClientRect.Left + 1, ClientRect.Top + 1, ClientRect.Right - 15, ClientRect.Bottom - 1);
      end;
  end;

  self.Canvas.Lock();
  try
    Canvas.Brush.Style := bsSolid;

    {
      Draw Backgrounds
    }

    // Text Background
    Canvas.Brush.Color := clBlack;
    Canvas.FillRect(ATextRect);

    // Arrow Area Background
    Canvas.Brush.Color := MAIN_BLUE;
    Canvas.FillRect(AArrowAreaRect);

    {
      Draw Border
    }
    Canvas.Brush.Color := MAIN_GRAY;
    Canvas.FrameRect(self.ClientRect);

    {
      Draw Arrow
    }
    Canvas.Pen.Color := clWhite;
    case FArrowPos of
      apTopLeft : begin
        AArrowPos := Point(AArrowAreaRect.Left + 2, AArrowAreaRect.Top + 2);
        ///

        Canvas.Polyline([
                           Point(AArrowPos.x,     AArrowPos.y),     Point(AArrowPos.x, AArrowPos.y + 6),
                           Point(AArrowPos.x + 1, AArrowPos.y + 6), Point(AArrowPos.x + 1, AArrowPos.y),
                           Point(AArrowPos.x + 6, AArrowPos.y),     Point(AArrowPos.x + 6, AArrowPos.y + 1),
                           Point(AArrowPos.x + 2, AArrowPos.y + 1), Point(AArrowPos.x + 2, AArrowPos.y + 4),
                           Point(AArrowPos.x + 5, AArrowPos.y + 7), Point(AArrowPos.x + 6, AArrowPos.y + 7),
                           Point(AArrowPos.x + 3, AArrowPos.y + 4), Point(AArrowPos.x + 3, AArrowPos.y + 3),
                           Point(AArrowPos.x + 6, AArrowPos.y + 6), Point(AArrowPos.x + 7, AArrowPos.y + 6),
                           Point(AArrowPos.x + 3, AArrowPos.y + 2), Point(AArrowPos.x + 4, AArrowPos.y + 2),
                           Point(AArrowPos.x + 7, AArrowPos.y + 5), Point(AArrowPos.x + 7, AArrowPos.y + 6)
        ]);
      end;

      apTopRight : begin
        AArrowPos := Point(AArrowAreaRect.Right - 3, AArrowAreaRect.Top + 2);
        ///

        Canvas.Polyline([
                           Point(AArrowPos.x,     AArrowPos.y),     Point(AArrowPos.x, AArrowPos.y + 6),
                           Point(AArrowPos.x - 1, AArrowPos.y + 6), Point(AArrowPos.x - 1, AArrowPos.y),
                           Point(AArrowPos.x - 6, AArrowPos.y),     Point(AArrowPos.x - 6, AArrowPos.y + 1),
                           Point(AArrowPos.x - 2, AArrowPos.y + 1), Point(AArrowPos.x - 2, AArrowPos.y + 4),
                           Point(AArrowPos.x - 5, AArrowPos.y + 7), Point(AArrowPos.x - 6, AArrowPos.y + 7),
                           Point(AArrowPos.x - 3, AArrowPos.y + 4), Point(AArrowPos.x - 3, AArrowPos.y + 3),
                           Point(AArrowPos.x - 6, AArrowPos.y + 6), Point(AArrowPos.x - 7, AArrowPos.y + 6),
                           Point(AArrowPos.x - 3, AArrowPos.y + 2), Point(AArrowPos.x - 4, AArrowPos.y + 2),
                           Point(AArrowPos.x - 7, AArrowPos.y + 5), Point(AArrowPos.x - 7, AArrowPos.y + 6)
        ]);
      end;

      apBottomLeft : begin
        AArrowPos := Point(AArrowAreaRect.Left + 2, AArrowAreaRect.Bottom - 3);
        ///

        Canvas.Polyline([
                          Point(AArrowPos.x,     AArrowPos.y),     Point(AArrowPos.x, AArrowPos.y - 6),
                          Point(AArrowPos.x + 1, AArrowPos.y - 6), Point(AArrowPos.x + 1, AArrowPos.y),
                          Point(AArrowPos.x + 6, AArrowPos.y),     Point(AArrowPos.x + 6, AArrowPos.y - 1),
                          Point(AArrowPos.x + 2, AArrowPos.y - 1), Point(AArrowPos.x + 2, AArrowPos.y - 4),
                          Point(AArrowPos.x + 5, AArrowPos.y - 7), Point(AArrowPos.x + 6, AArrowPos.y - 7),
                          Point(AArrowPos.x + 3, AArrowPos.y - 4), Point(AArrowPos.x + 3, AArrowPos.y - 3),
                          Point(AArrowPos.x + 6, AArrowPos.y - 6), Point(AArrowPos.x + 7, AArrowPos.y - 6),
                          Point(AArrowPos.x + 3, AArrowPos.y - 2), Point(AArrowPos.x + 4, AArrowPos.y - 2),
                          Point(AArrowPos.x + 7, AArrowPos.y - 5), Point(AArrowPos.x + 7, AArrowPos.y - 6)
        ]);
      end;

      apBottomRight : begin
        AArrowPos := Point(AArrowAreaRect.Right - 3, AArrowAreaRect.Bottom - 3);
        ///

        Canvas.Polyline([
                           Point(AArrowPos.x,     AArrowPos.y),     Point(AArrowPos.x, AArrowPos.y - 6),
                           Point(AArrowPos.x - 1, AArrowPos.y - 6), Point(AArrowPos.x - 1, AArrowPos.y),
                           Point(AArrowPos.x - 6, AArrowPos.y),     Point(AArrowPos.x - 6, AArrowPos.y - 1),
                           Point(AArrowPos.x - 2, AArrowPos.y - 1), Point(AArrowPos.x - 2, AArrowPos.y - 4),
                           Point(AArrowPos.x - 5, AArrowPos.y - 7), Point(AArrowPos.x - 6, AArrowPos.y - 7),
                           Point(AArrowPos.x - 3, AArrowPos.y - 4), Point(AArrowPos.x - 3, AArrowPos.y - 3),
                           Point(AArrowPos.x - 6, AArrowPos.y - 6), Point(AArrowPos.x - 7, AArrowPos.y - 6),
                           Point(AArrowPos.x - 3, AArrowPos.y - 2), Point(AArrowPos.x - 4, AArrowPos.y - 2),
                           Point(AArrowPos.x - 7, AArrowPos.y - 5), Point(AArrowPos.x - 7, AArrowPos.y - 6)
        ]);
      end;
    end;

    {
      Draw Text
    }
    Canvas.brush.Style := bsClear;
    InflateRect(ATextRect, -3, -1);

    DrawText(
              Canvas.handle,
              PWideChar(Caption),
              Length(Caption),
              ATextRect,
              (DT_WORDBREAK or DT_NOPREFIX)
    );
  finally
    self.Canvas.Unlock();
  end;
end;

end.
