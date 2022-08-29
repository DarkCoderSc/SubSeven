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

unit Sub7.Core.VCL.Captcha;

interface

uses System.Classes, Winapi.Windows, System.SysUtils, VCL.Controls, VCL.Graphics;

type
  TS7CCaptcha = class(TCustomControl)
  private
    FCandidate             : String;
    FCandidateLength       : Cardinal;
    FErrorThreshold        : Cardinal;
    FCurrentErrorThreshold : Cardinal;

    {@M}
    procedure DrawCandidate();
    procedure ArmorCandidate();
    function RandomColor() : TColor;

    procedure SetCandidateLength(const AValue : Cardinal);
  protected
    {@M}
    procedure Paint(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    procedure Generate();
    function Validate(const ACandidate : String) : Boolean;

  published
    {@G/S}
    property CandidateLength : Cardinal read FCandidateLength write SetCandidateLength;
    property ErrorThreshold  : Cardinal read FErrorThreshold  write FErrorThreshold;

    property Align;
    property AlignWithMargins;
    property Margins;
  end;

implementation

uses System.Math;

{ TS7CCaptcha.SetCandidateLength }

procedure TS7CCaptcha.SetCandidateLength(const AValue : Cardinal);
begin
  if AValue = FCandidateLength then
    Exit();

  FCandidateLength := AValue;

  self.Generate();
end;

{ TS7CCaptcha.RandomColor }

function TS7CCaptcha.RandomColor() : TColor;
begin
  result := RGB(
                  RandomRange(100, 222),
                  RandomRange(100, 222),
                  RandomRange(100, 222)
  );
end;

{ TS7CCaptcha.Generate }

procedure TS7CCaptcha.Generate();
const AChars = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
var i : integer;
begin
  randomize();
  ///

  FCandidate := '';

  FCurrentErrorThreshold := 0;

  for i := 1 to FCandidateLength do
    FCandidate := FCandidate + AChars[random(length(AChars))+1];

  ///
  self.Invalidate();
end;

{ TS7CCaptcha.Create }

constructor TS7CCaptcha.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.DoubleBuffered    := True;
  Width                  := 250;
  Height                 := 70;
  FCandidateLength       := 4;
  FErrorThreshold        := 3;
  FCurrentErrorThreshold := 0;

  ///
  self.Generate();
end;

{ TS7CCaptcha.Destroy }

destructor TS7CCaptcha.Destroy();
begin

  ///
  inherited Destroy();
end;

{ TS7CCaptcha.DrawCandidate }

procedure TS7CCaptcha.DrawCandidate();
var ALogFont : TLogFont;
    AFont    : THandle;
    I        : Integer;
begin
  ALogFont.lfweight         := 900;

  ALogFont.lfcharset        := 1;
  ALogFont.lfoutprecision   := OUT_TT_ONLY_PRECIS;
  ALogFont.lfquality        := DEFAULT_QUALITY;
  ALogFont.lfpitchandfamily := FF_SWISS;
  ALogFont.lfUnderline      := 0;
  ALogFont.lfStrikeOut      := 0;

  ALogFont.lfFaceName       := 'Arial';

  for I := 1 to Length(FCandidate) do begin
    ALogFont.lfheight     := RandomRange(30, 60);
    ALogFont.lfwidth      := RandomRange(10, 25);
    ALogFont.lfEscapement := RandomRange(0, 800);

    AFont := CreateFontIndirect(ALogFont);

    Selectobject(Canvas.handle, AFont);
    try
      SetTextColor(
                      Canvas.handle,
                      RandomColor()
      );

      SetBKmode(Canvas.handle, Transparent);

      Canvas.TextOut(50 * (I-1), (Height div 3), FCandidate[I]);
    finally
      DeleteObject(AFont);
    end;
  end;
end;

{ TS7CCaptcha.ArmorCandidate }

procedure TS7CCaptcha.ArmorCandidate();
var I         : Integer;
    APenStyle : TPenStyle;
begin
  Canvas.Pen.Color   := RandomColor();
  Canvas.Brush.Style := bsClear;

  case Random(5) of
    0, 8, 9       : APenStyle := TPenStyle.psSolid;
    1             : APenStyle := TPenStyle.psDash;
    2, 5, 6, 7    : APenStyle := TPenStyle.psDot;
    3             : APenStyle := TPenStyle.psDashDot;
    4             : APenStyle := TPenStyle.psDashDotDot;
  end;
  Canvas.Pen.Style := APenStyle;

  for I := 0 to (Height div 4) do begin
    Canvas.MoveTo(0, (I * 4));
    Canvas.LineTo(Width, (I * 4));
  end;
end;

{ TS7CCaptcha.Paint }

procedure TS7CCaptcha.Paint();
begin
  Canvas.Lock();
  try
    self.Brush.Style := bsSolid;
    self.Brush.Color := clWhite;

    Canvas.FillRect(self.BoundsRect);

    self.DrawCandidate();

    self.ArmorCandidate();
  finally
    Canvas.Unlock();
  end;
end;

{ TS7CCaptcha.Validate }

function TS7CCaptcha.Validate(const ACandidate : String) : Boolean;
begin
  result := String.Compare(ACandidate, FCandidate, True) = 0;

  if not result then begin
    Inc(FCurrentErrorThreshold);

    if FCurrentErrorThreshold >= FErrorThreshold then
      self.Generate();
  end;
end;

end.
