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

unit S7ScrollingCredit;

interface

uses System.Classes, Winapi.Windows, VCL.Controls, VCL.Graphics, S7Timer;

type
  TFadeMode = (
    fmIn,
    fmOut
  );

  TS7ScrollingCredit = class(TGraphicControl)
  private
    FActive        : Boolean;
    FText          : TStringList;
    FScene         : TBitmap;
    FTimer         : TS7Timer;
    FScrollPos     : Integer;

    FFadeOutBitmap : TBitmap;
    FFadeInBitmap  : TBitmap;

    FLogoBegin     : TPicture;
    FLogoEnd       : TPicture;

    {@M}
    procedure SetActive(const AValue : Boolean);
    procedure SetText(const AValue : TStringList);
    procedure SetPicture(const AIndex : Integer; const AValue : TPicture);

    procedure OnTimer(Sender : TObject);

    procedure RenderFadeBitmap(var ABitmap : TBitmap; const AFadeMode : TFadeMode);
    procedure RenderFadeBitmaps();
  protected
    {@M}
    procedure Paint(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    procedure RefreshScene();
    procedure NextFrame();
  published
    {@G/S}
    property Active    : Boolean     read FActive write SetActive;
    property Text      : TStringList read FText   write SetText;

    property LogoBegin : TPicture index 0 read FLogoBegin write SetPicture;
    property LogoEnd   : TPicture index 1 read FLogoEnd   write SetPicture;

    property Align;
    property AlignWithMargins;
    property Margins;
    property Visible;
    property Enabled;
    property Font;
  end;

implementation

uses System.SysUtils, S7Common, Winapi.GDIPOBJ, Winapi.GDIPAPI;

procedure TS7ScrollingCredit.NextFrame();
begin
  Inc(FScrollPos);

  if FScrollPos >= (FScene.Height + ClientHeight) then
    FScrollPos := 0;

  ///
  self.Invalidate();
end;

{ TS7ScrollingCredit.OnTimer }

procedure TS7ScrollingCredit.OnTimer(Sender : TObject);
begin
  self.NextFrame();
end;

{ TS7ScrollingCredit.Create }

constructor TS7ScrollingCredit.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FActive    := False;
  FText      := TStringList.Create();
  FScene     := TBitmap.Create();
  FScrollPos := 0;

  FTimer          := TS7Timer.Create(self);
  FTimer.Enabled  := False;
  FTimer.Period   := 80;
  FTimer.DueTime  := 80;
  FTimer.OnTimer  := self.OnTimer;

  FFadeOutBitmap  := nil;
  FFadeInBitmap   := nil;

  FLogoBegin      := TPicture.Create();
  FLogoEnd        := TPicture.Create();
end;

{ TS7ScrollingCredit.Destroy }

destructor TS7ScrollingCredit.Destroy();
begin
  if Assigned(FTimer) then
    FreeAndNil(FTimer);

  if Assigned(FText) then
    FreeAndNil(FText);

  if Assigned(FScene) then
    FreeAndNil(FScene);

  if Assigned(FLogoBegin) then
    FreeAndNil(FLogoBegin);

  if Assigned(FLogoEnd) then
    FreeAndNil(FLogoEnd);

  if Assigned(FFadeOutBitmap) then
    FreeAndNil(FFadeOutBitmap);

  if Assigned(FFadeInBitmap) then
    FreeAndNil(FFadeInBitmap);

  ///
  inherited Destroy();
end;

{ TS7ScrollingCredit.Paint }

procedure TS7ScrollingCredit.Paint();
begin
  if not Assigned(FScene) then
    Exit();

  if (FScene.Width <= 0) or (FScene.Height <= 0) then
    Exit();

  Canvas.Lock();
  try
    // Draw Text Scene
    Canvas.Draw(0, ClientHeight - FScrollPos, FScene);

    // Draw Fade Scenes
    Canvas.Draw(0, 0, FFadeOutBitmap);
    Canvas.Draw(0, ClientHeight - FFadeInBitmap.Height, FFadeInBitmap);
  finally
    Canvas.Unlock();
  end;
end;

{ TS7ScrollingCredit.RenderFadeBitmap }

procedure TS7ScrollingCredit.RenderFadeBitmap(var ABitmap : TBitmap; const AFadeMode : TFadeMode);
var ABrush    : TGPLinearGradientBrush;
    ARect     : TGPRect;
    AGraphics : TGPGraphics;
    AColorA   : Longint;
    AColorB   : Longint;
begin
  if Assigned(ABitmap) then
    FreeAndNil(ABitmap);

  ABitmap := TBitmap.Create();

  InitializeBitmap32(ABitmap, ClientWidth, 40);

  // Draw Text Out Scene
  ARect.X      := 0;
  ARect.Y      := 0;
  ARect.Width  := ClientWidth;
  ARect.Height := 40;

  AGraphics := TGPGraphics.Create(ABitmap.Canvas.Handle);
  try
    case AFadeMode of
      fmIn: begin
        AColorA := MakeColor(0, 0, 0, 0);
        AColorB := MakeColor(0, 0, 0);
      end;

      fmOut: begin
        AColorA := MakeColor(0, 0, 0);
        AColorB := MakeColor(0, 0, 0, 0);
      end;
    end;
    ABrush := TGPLinearGradientBrush.Create(
      ARect,
      AColorA,
      AColorB,
      LinearGradientModeVertical
    );
    try
      AGraphics.FillRectangle(ABrush, ARect);
    finally
      if Assigned(ABrush) then
        FreeAndNil(ABrush);
    end;
  finally
    if Assigned(AGraphics) then
      FreeAndNil(AGraphics);
  end;
end;

{ TS7ScrollingCredit.RenderFadeBitmaps }

procedure TS7ScrollingCredit.RenderFadeBitmaps();
begin
  self.RenderFadeBitmap(FFadeInBitmap, fmIn);
  self.RenderFadeBitmap(FFadeOutBitmap, fmOut);
end;

{ TS7ScrollingCredit.LoadScene }

procedure TS7ScrollingCredit.SetActive(const AValue : Boolean);
begin
  if AValue = FActive then
    Exit();

  FActive := AValue;

  if FActive then begin
    self.RefreshScene();

    FScrollPos := 0;
  end;

  FTimer.Enabled := FActive;
end;

{ TS7ScrollingCredit.SetText }

procedure TS7ScrollingCredit.SetText(const AValue : TStringList);
begin
  FText.Assign(AValue);

  self.RefreshScene();
end;

{ TS7ScrollingCredit.RefreshScene }

procedure TS7ScrollingCredit.RefreshScene();
var i           : Integer;
    ALine       : String;
    APosY       : Integer;
    ATextRect   : TRect;
    ATextFormat : TTextFormat;
    ACount      : Integer;
    n           : Integer;
    ANormalSize : Integer;
begin
  if Assigned(FScene) then
    FreeAndNil(FScene);

  FScene := TBitmap.Create();
  APosY  := 0;

  FScrollPos := 0;

  FScene.Canvas.Font.Assign(Font);
  FScene.Canvas.Brush.Color := clBlack;
  FScene.Canvas.Brush.Style := bsSolid;

  FScene.SetSize(ClientWidth, 1);

  FScene.Canvas.FillRect(Rect(0, 0, FScene.Width, FScene.Height));

  { Render Begin Logo }

  if Assigned(FLogoBegin) then begin
    FScene.SetSize(ClientWidth, FLogoBegin.Height + 16);

    FScene.Canvas.Draw((FScene.Width div 2) - (FLogoBegin.Width div 2), APosY, FLogoBegin.Graphic);

    Inc(APosY, FLogoBegin.Height + 16);
  end;

  { Render Scrolling Text }

  ATextFormat := [tfCenter, tfWordBreak];

  ANormalSize := FScene.Canvas.Font.Size;

  for I := 0 to FText.Count -1 do begin
    ALine := FText.Strings[i];

    // Calc Size
    if ALine = '' then begin
      // Separator

      Inc(APosY, 16);

      FScene.SetSize(FScene.Width, APosY);
    end else begin
      ATextRect.Left   := 0;
      ATextRect.Top    := APosY;
      ATextRect.Width  := ClientWidth;
      ATextRect.Height := 0;

      if Copy(ALine, 1, 1) = '#' then begin
        // Handle Headings

        ACount := 0;
        for n := 1 to Length(ALine) do
          if ALine[n] = '#' then
            Inc(ACount)
          else
            break;

        Delete(ALine, 1, ACount);

        case ACount of
          1 : FScene.Canvas.Font.Size := ANormalSize + 7;
          2 : FScene.Canvas.Font.Size := ANormalSize + 6;
          3 : FScene.Canvas.Font.Size := ANormalSize + 5;
          4 : FScene.Canvas.Font.Size := ANormalSize + 4;
          5 : FScene.Canvas.Font.Size := ANormalSize + 3;
          6 : FScene.Canvas.Font.Size := ANormalSize + 2;
        end;
      end else
        FScene.Canvas.Font.Size := ANormalSize;

      if Copy(ALine, 1, 1) = '_' then begin
        FScene.Canvas.Font.Style := [fsUnderline];
        Delete(ALine, 1, 1);
      end else
        FScene.Canvas.Font.Style := [];

      FScene.Canvas.TextRect(ATextRect, ALine, ATextFormat + [tfCalcRect]);

      Inc(APosY, ATextRect.height);

      FScene.SetSize(FScene.Width, APosY);

      ATextRect.Width := ClientWidth;

      FScene.Canvas.TextRect(ATextRect, ALine, ATextFormat);
    end;
  end;

  { Render End Logo }

  if Assigned(FLogoEnd) then begin
    FScene.SetSize(ClientWidth, APosY + FLogoEnd.Height + 64);

    FScene.Canvas.Draw((FScene.Width div 2) - (FLogoEnd.Width div 2), APosY + 32, FLogoEnd.Graphic);

    Inc(APosY, FLogoEnd.Height + 64);
  end;

  { Render Fading Bitmaps }

  self.RenderFadeBitmaps();
end;

{ TS7ScrollingCredit.SetPicture }

procedure TS7ScrollingCredit.SetPicture(const AIndex : Integer; const AValue : TPicture);
begin
  case AIndex of
    0 : FLogoBegin.Assign(AValue);
    1 : FLogoEnd.Assign(AValue);
  end;

  ///
  self.Invalidate();
end;

end.
