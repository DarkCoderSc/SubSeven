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

unit Sub7.Core.UX.Utils;

interface

uses Winapi.Windows, VCL.Graphics;

procedure InitializeBitmap32(var ABmp : TBitmap; AWidth, AHeight : Integer);
procedure FillBitmap32(var ABitmap : TBitmap; AOpacity : Integer = 255);
procedure GrayScaleBitmap32(var ABitmap : TBitmap);

implementation

uses System.SysUtils;

{ _.InitializeBitmap32 }

procedure InitializeBitmap32(var ABmp : TBitmap; AWidth, AHeight : Integer);
var p : pointer;
begin
  if NOT Assigned(ABmp) then exit;

  ABmp.PixelFormat   := pf32Bit;
  ABmp.Width         := AWidth;
  ABmp.Height        := AHeight;
  ABmp.HandleType    := bmDIB;
  ABmp.ignorepalette := true;
  ABmp.alphaformat   := afPremultiplied;

  p := ABmp.ScanLine[AHeight - 1];
  ZeroMemory(p, AWidth * AHeight * 4);
end;

{ _.FillBitmap32 }

procedure FillBitmap32(var ABitmap : TBitmap; AOpacity : Integer = 255);
var x, y    : Integer;
    pPixels : PRGBQuad;
begin
  for y := 0 to ABitmap.Height-1 do begin
    pPixels := PRGBQuad(ABitmap.ScanLine[y]);

    for x := 0 to ABitmap.Width-1 do begin
      try
        with pPixels^ do begin
          if rgbReserved = 0 then
            continue;

          rgbRed   := (rgbRed * AOpacity) div $FF;
          rgbGreen := (rgbGreen * AOpacity) div $FF;
          rgbBlue  := (rgbBlue * AOpacity) div $FF;
        end;
      finally
        Inc(pPixels);
      end;
    end;
  end;
end;

{ _.GrayScaleBitmap32 }

procedure GrayScaleBitmap32(var ABitmap : TBitmap);
var x, y       : integer;
    pPixels    : PRGBQuad;
    vGrayValue : Byte;
begin
  if NOT Assigned(ABitmap)          then exit;
  if ABitmap.PixelFormat <> pf32Bit then exit;

  for y := 0 to ABitmap.Height - 1 do begin
    pPixels := ABitmap.ScanLine[y];

    for x := 0 to ABitmap.Width - 1 do begin
      try
        vGrayValue := (pPixels^.rgbRed + pPixels^.rgbGreen + pPixels^.rgbBlue) div 3;
        pPixels^.rgbRed   := vGrayValue;
        pPixels^.rgbGreen := vGrayValue;
        pPixels^.rgbBlue  := vGrayValue;
      finally
        Inc(pPixels);
      end;
    end;
  end;
end;

end.
