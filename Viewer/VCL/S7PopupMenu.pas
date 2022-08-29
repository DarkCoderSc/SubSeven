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

unit S7PopupMenu;

interface

uses VCL.Menus, System.Classes, VCL.Graphics, Winapi.Windows, System.SysUtils, VCL.Controls;

type
  TS7PopupMenu = class(TPopupMenu)
  private
    FMenuBrushHandle: THandle;

    {@M}
    procedure MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
    procedure DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure InitializeMenu();
  protected
    {@M}
    procedure DoPopup(Sender: TObject); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  end;

  const PADDING = 8;

implementation

uses S7Theme;

{-------------------------------------------------------------------------------
  Initialize PopupMenu Style
-------------------------------------------------------------------------------}
procedure TS7PopupMenu.InitializeMenu();
var AMenuInfo  : TMenuInfo;
begin
  DeleteObject(FMenuBrushHandle);

  FMenuBrushHandle := CreateSolidBrush(ColorToRGB(MAIN_GRAY));

  FillChar(AMenuInfo, SizeOf(TMenuInfo), #0);

  AMenuInfo.cbSize  := SizeOf(TMenuInfo);
  AMenuInfo.hbrBack := fMenuBrushHandle;
  AMenuInfo.fMask   := MIM_BACKGROUND or
                       MIM_APPLYTOSUBMENUS;

  self.OwnerDraw := True;

  ///
  SetMenuInfo(self.Handle, AMenuInfo);
end;

{-------------------------------------------------------------------------------
  On Mesure Popup Menu Item
-------------------------------------------------------------------------------}
procedure TS7PopupMenu.MeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
var M : TMenuItem;
begin
  M := TMenuItem(Sender);
  ///

  if M.Caption = '-' then
    Height := 10
  else begin
    Height := 19;

    if (M.MenuIndex = 0) or (M.MenuIndex = self.Items.Count-1) then
      Inc(Height, PADDING);
  end;
end;

{-------------------------------------------------------------------------------
  On Draw Item
-------------------------------------------------------------------------------}
procedure TS7PopupMenu.DrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var AColor         : TColor;
    ACaption       : String;
    ASeparator     : Boolean;
    ASeparatorRect : TRect;
    M              : TMenuItem;
    AFontColor     : TColor;
begin
  M := TMenuItem(Sender);
  ACaption := M.Caption;

  ASeparator := (ACaption = '-');

  ACanvas.Brush.Style := bsSolid;

  if ASeparator then begin
    {
      Draw Separator Menu
    }
    ACanvas.Brush.Color := clBlack;

    ACanvas.FillRect(ARect);

    ASeparatorRect.Left   := ARect.Left + 8;
    ASeparatorRect.Top    := ARect.Top + (ARect.Height div 2) - 1;
    ASeparatorRect.Height := 2;
    ASeparatorRect.Width  := ARect.Width - 16;

    ACanvas.Brush.Color := DARK_BLUE;

    ACanvas.FillRect(ASeparatorRect);
  end else begin
    {
      Draw Menu Item
    }
    if Selected and M.Enabled then
      AColor := MAIN_BLUE
    else
      AColor := clBlack;

    if (M.MenuIndex = 0) or (M.MenuIndex = self.Items.Count-1) then begin
      ACanvas.Brush.Color := clBlack;

      ACanvas.FillRect(ARect);

      if (M.MenuIndex = 0) then
        ARect.Top := ARect.Top + PADDING
      else if (M.MenuIndex = self.Items.Count-1) then
        ARect.Bottom := ARect.Bottom - PADDING
    end;

    ACanvas.Brush.Color := AColor;

    if M.Enabled then
      AFontColor := clWhite
    else
      AFontColor := clGray;

    ACanvas.Font.Color := AFontColor;
    ACanvas.Font.Name  := FONT_2;
    ACanvas.Font.Size  := 9;


    ACanvas.FillRect(ARect);

    Inc(ARect.Left, 16);

    ACanvas.TextRect(ARect, ACaption, [tfSingleLine, tfVerticalCenter]);
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7PopupMenu.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.InitializeMenu();
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7PopupMenu.Destroy();
begin
  DeleteObject(FMenuBrushHandle);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Triggered when popup shows, used to initialize custom item draw
-------------------------------------------------------------------------------}
procedure TS7PopupMenu.DoPopup(Sender: TObject);
var I : Integer;
    M : TMenuItem;
begin
  inherited;
  ///

  for I := 0 to self.Items.Count -1 do begin
    M := self.Items.Items[i];
    ///

    if not Assigned(M.OnDrawItem) then
      M.OnDrawItem := DrawItem;

    if not Assigned(M.OnMeasureItem) then
      M.OnMeasureItem := MeasureItem;
  end;
end;

end.
