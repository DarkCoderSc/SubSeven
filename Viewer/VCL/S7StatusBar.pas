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

unit S7StatusBar;

interface

uses Winapi.Windows, System.Classes, VCL.Controls, VCL.Forms, VCL.Graphics, S7Theme;

type
  TStatusPanel = class(TCollectionItem)
  private
    FText      : String;
    FWidth     : Integer;
    FHint      : String;
    FAlignment : TAlignment;

    FRect      : TRect;

    {@M}
    procedure Invalidate();

    procedure SetText(AValue : String);
    procedure SetWidth(AValue : Integer);
    procedure SetAlignment(AValue : TAlignment);
  public
    {@C}
    constructor Create(ACollection : TCollection); override;
    destructor Destroy(); override;

    {@G/S}
    property Rect : TRect read FRect write FRect;
  published
    {@G/S}
    property Text      : String     read FText      write SetText;
    property Width     : Integer    read FWidth     write SetWidth;
    property Hint      : String     read FHint      write FHint;
    property Alignment : TAlignment read FAlignment write SetAlignment;
  end;

  TStatusPanels = class(TOwnedCollection)
  private
    {@M}
    procedure Invalidate();
  protected
    {@M}
    function GetItem(Index: Integer): TStatusPanel;
    procedure SetItem(Index: Integer; Value: TStatusPanel);
  public
    {@C}
    constructor Create(AOwner: TPersistent);

    {@M}
    function Add: TStatusPanel;

    {@G/S}
    property Items[Index: Integer]: TStatusPanel read GetItem write SetItem; default;
  end;

  TS7StatusBar = class(TCustomControl)
  private
    FPanels : TStatusPanels;

    {@M}
    procedure CMHintShow(var AMessage: TCMHintShow); message CM_HINTSHOW;
    procedure RefreshPanelsRect();
    function GetPanelByPoint(APoint : TPoint) : TStatusPanel;
  protected
    {@M}
    procedure Paint(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Font;
    ///

    property Panels : TStatusPanels read FPanels write FPanels;
  end;

implementation

uses SysUtils, SYstem.Types;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7StatusBar


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  Get panel by cursor pos
-------------------------------------------------------------------------------}
function TS7StatusBar.GetPanelByPoint(APoint : TPoint) : TStatusPanel;
var I : Integer;
    P : TStatusPanel;
begin
  result := nil;
  ///

  for I := 0 to self.Panels.Count -1 do begin
    P := self.Panels.Items[i];
    ///

    if ptinrect(P.Rect, APoint) then begin
      result := P;

      break;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Refresh panel rects
-------------------------------------------------------------------------------}
procedure TS7StatusBar.RefreshPanelsRect();
var I     : Integer;
    P     : TStatusPanel;
    ARect : TRect;
begin
  ARect        := TRect.Empty;

  ARect.Top    := 1;
  ARect.Height := (ClientHeight - 2);

  for I := 0 to self.Panels.Count -1 do begin
    P := self.Panels.Items[i];
    ///

    ARect.Left  := (ARect.Left + ARect.Width + 1);

    if I = (self.Panels.Count -1) then
      ARect.Width := (ClientWidth - ARect.Left - 1)
    else
      ARect.Width := self.Panels[i].Width;

    ///
    P.Rect := ARect;
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7StatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  self.Align := alBottom;

  FPanels := TStatusPanels.Create(self);

  self.Font.Name  := FONT_1;
  self.Font.Color := clWhite;

  self.ShowHint := True;

  self.Height := 19;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7StatusBar.Destroy();
begin
  if Assigned(FPanels) then
    FreeAndNil(FPanels);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TS7StatusBar.Paint();
var I           : integer;
    ARect       : TRect;
    ACaption    : String;
    ATextRect   : TRect;
    ATextFormat : TTextFormat;
begin
  Canvas.Lock();
  try
    Canvas.Brush.Style := bsSolid;
    Canvas.Font.Assign(self.Font);
    ///

    {
      Draw Background
    }
    ARect.Left   := 0;
    ARect.Top    := 0;
    ARect.Width  := ClientWidth;
    ARect.Height := ClientHeight;

    Canvas.Brush.Color := clBlack;

    Canvas.FillRect(ARect);

    {
      Draw Panels
    }
    Canvas.Brush.Color := DARK_BLUE;

    self.RefreshPanelsRect();

    for I := 0 to (self.Panels.Count -1) do begin
      ARect := self.Panels.Items[i].Rect;
      ///

      {
        Draw Panel Background
      }
      Canvas.FillRect(ARect);

      {
        Draw Panel Text
      }
      ACaption := self.Panels[I].Text;

      ATextRect.Top    := ARect.Top;
      ATextRect.Height := ARect.Height;
      ATextRect.Left   := (ARect.Left + 2);
      ATextRect.Width  := (ARect.Width - 4);

      ATextFormat := [tfEndEllipsis, tfSingleLine, tfVerticalCenter];

      case self.Panels[I].Alignment of
        taLeftJustify  : ATextFormat := ATextFormat + [tfLeft];
        taRightJustify : ATextFormat := ATextFormat + [tfRight];
        taCenter       : ATextFormat := ATextFormat + [tfCenter];
      end;

      Canvas.TextRect(ATextRect, ACaption, ATextFormat);
    end;
  finally
    Canvas.Unlock();
  end;
end;

{-------------------------------------------------------------------------------
  Detect when hint is being shown
-------------------------------------------------------------------------------}
procedure TS7StatusBar.CMHintShow(var AMessage: TCMHintShow);
var P : TStatusPanel;
begin
  P := self.GetPanelByPoint(AMessage.HintInfo.CursorPos);

  if Assigned(P) then begin
    AMessage.HintInfo.HintStr := P.Hint;
    AMessage.HintInfo.CursorRect := P.Rect;
  end;

  ///
  inherited;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TStatusPanel


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

constructor TStatusPanel.Create(ACollection : TCollection);
begin
  inherited Create(ACollection);
  ///

  FText      := '';
  FWidth     := 50;
  FHint      := '';
  FAlignment := taLeftJustify;
  FRect      := TRect.Empty;

  ///
  self.Invalidate();
end;

destructor TStatusPanel.Destroy();
begin
  inherited Destroy();
  ///

  self.Invalidate();
end;

procedure TStatusPanel.Invalidate();
begin
  if Assigned(GetOwner()) then begin
    if Assigned(TStatusPanels(GetOwner())) then begin
      TS7StatusBar(TStatusPanels(GetOwner()).GetOwner).Invalidate();
    end;
  end;
end;

procedure TStatusPanel.SetText(AValue : String);
begin
  if (FText = AValue) then
    Exit();
  ///

  FText := AValue;

  ///
  Invalidate();
end;

procedure TStatusPanel.SetWidth(AValue : Integer);
begin
  if (FWidth = AValue) then
    Exit();
  ///

  FWidth := AValue;

  ///
  Invalidate();
end;

procedure TStatusPanel.SetAlignment(AValue : TAlignment);
begin
  if (AValue = FAlignment) then
    Exit();
  ///

  FAlignment := AValue;

  ///
  Invalidate();
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TStatusPanels


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

function TStatusPanels.Add: TStatusPanel;
begin
  Result := TStatusPanel(inherited Add);
end;

constructor TStatusPanels.create(AOwner : TPersistent);
begin
  inherited Create(AOwner, TStatusPanel);
  ///

end;

function TStatusPanels.GetItem(Index: Integer): TStatusPanel;
begin
  Result := TStatusPanel(inherited GetItem(Index));
end;

procedure TStatusPanels.SetItem(Index: Integer; Value: TStatusPanel);
begin
  inherited SetItem(Index, Value);
  ///

  Invalidate();
end;

procedure TStatusPanels.Invalidate();
begin
  if Assigned(self.GetOwner()) then
    TS7StatusBar(self.GetOwner()).Invalidate();
end;

end.
