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

unit S7Common;

interface

uses WinAPI.Windows, VCL.Graphics, System.Classes;

function FadeBitmap32Opacity(var ABitmap : TBitmap; AOpacity : word = 200) : Boolean;
procedure InitializeBitmap32(var ABmp : TBitmap; AWidth, AHeight : Integer);
function GetParentChain(ABase : TComponent) : String;

implementation

uses System.SysUtils;

{ _.GetParentChain

  TODO: Improve this code with recursion instead of using TStringList like a lazy
  guy }

function GetParentChain(ABase : TComponent) : String;
var AList : TStringList;
    I     : Integer;
begin
  result := '';

  if not Assigned(ABase) then
    Exit();

  AList := TStringList.Create();
  try
    AList.Add(ABase.Name);

    while True do begin
      if ABase.Owner = nil then
        break;

      ABase := ABase.Owner;

      if ABase.Name <> '' then
        AList.Add(ABase.Name);
    end;

    for I := AList.Count -1 downto 0 do begin
      if I <> AList.Count -1 then
        result := result + '.';

      result := result + AList.Strings[I];
    end;
  finally
    if Assigned(AList) then
      FreeAndNil(AList);
  end;
end;

{-------------------------------------------------------------------------------
  Initialize Transparent Bitmap
-------------------------------------------------------------------------------}
procedure InitializeBitmap32(var ABmp : TBitmap; AWidth, AHeight : Integer);
var p : pointer;
begin
  if NOT Assigned(ABmp) then
    Exit;

  ABmp.PixelFormat   := pf32Bit;
  ABmp.Width         := AWidth;
  ABmp.Height        := AHeight;
  ABmp.HandleType    := bmDIB;
  ABmp.ignorepalette := true;
  ABmp.alphaformat   := afPremultiplied;

  p := ABmp.ScanLine[AHeight - 1];
  ZeroMemory(p, AWidth * AHeight * 4);
end;

{-------------------------------------------------------------------------------
  Check if Bitmap is well formed
-------------------------------------------------------------------------------}
function ValidGraphic(ABitmap : TBitmap) : Boolean;
begin
  result := false;
  ///

  if NOT Assigned(ABitmap) then
    Exit;

  if ABitmap.Width  <= 0 then exit;
  if ABitmap.Height <= 0 then exit;

  ///
  result := true;
end;

{-------------------------------------------------------------------------------
  Fade 32Bit bitmap (Change opacity level)
-------------------------------------------------------------------------------}
function FadeBitmap32Opacity(var ABitmap : TBitmap; AOpacity : word = 200) : Boolean;
var x, y    : Integer;
    pPixels : PRGBQuad;
    AColor  : LongInt;

    APixelOpacity : word;
begin
  result := false;
  ///

  if NOT ValidGraphic(ABitmap) then exit;

  {
    START FADING BITMAP PIXEL
  }
  for y := 0 to ABitmap.Height-1 do begin
    pPixels := PRGBQuad(ABitmap.ScanLine[y]);

    {WORK ON THE LINE}
    for x := 0 to ABitmap.Width-1 do begin
      try
        if pPixels^.rgbReserved = 0 then continue;

        {
          WE NEED TO TAKE CARE OF CURRENT PIXEL TRANSPARENCY LEVEL TO BE SMOOTH
        }
        APixelOpacity := (pPixels^.rgbReserved * AOpacity) div high(byte);

        {
          UPDATE PIXEL COLOR USING NEW OPACITY LEVEL
        }
        pPixels^.rgbRed   := (pPixels^.rgbRed   * APixelOpacity) div $FF;
        pPixels^.rgbGreen := (pPixels^.rgbGreen * APixelOpacity) div $FF;
        pPixels^.rgbBlue  := (pPixels^.rgbBlue  * APixelOpacity) div $FF;

        {
          THEN SET THE OPACITY LEVEL OF THE CURRENT PIXEL
        }
        pPixels^.rgbReserved := APixelOpacity;
      finally
        Inc(pPixels);
      end;
    end;
  end;
end;

end.
