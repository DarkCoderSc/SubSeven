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

unit S7Panel;

interface

uses Winapi.Windows, System.Classes, VCL.Controls, VCL.Graphics, S7Theme;

type
  TS7Panel = class(TCustomControl)
  private
    FBorderTop    : Integer;
    FBorderLeft   : Integer;
    FBorderRight  : Integer;
    FBorderBottom : Integer;

    FColor        : TColor;
    FBorderColor  : TColor;

    {@M}
    procedure SetBorder(AIndex : Integer; AValue : Integer);
    procedure SetColor(AIndex : Integer; AValue : TColor);
  protected
    {@M}
    procedure Paint(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property BorderTop    : Integer index 0 read FBorderTop    write SetBorder;
    property BorderLeft   : Integer index 1 read FBorderLeft   write SetBorder;
    property BorderRight  : Integer index 2 read FBorderRight  write SetBorder;
    property BorderBottom : Integer index 3 read FBorderBottom write SetBorder;

    property Color        : TColor  index 0 read FColor        write SetColor;
    property BorderColor  : TColor  index 1 read FBorderColor  write SetColor;

    property Align;
    property Cursor;
    property Caption;
    property Font;
    property ParentFont;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property ParentShowHint;
    property Enabled;
    property Visible;
    property TabOrder;
    property TabStop;
    property Hint;
    property HelpContext;
    property Anchors;
    property Constraints;
    property DragKind;
    property DragMode;
    property DragCursor;
    property DockSite;
    property OnEndDock;
    property OnStartDock;
    property OnDockDrop;
    property OnDockOver;
    property OnGetSiteInfo;
    property OnUnDock;
    property OnContextPopup;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

uses S7Classes;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7Panel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ///

  FBorderTop    := 0;
  FBorderLeft   := 0;
  FBorderRight  := 0;
  FBorderBottom := 0;

  ControlStyle  := ControlStyle + [csAcceptsControls, csOpaque];

  DoubleBuffered := True;

  FColor := DARK_BLUE;
  FBorderColor := clBlack;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7Panel.Destroy();
begin

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TS7Panel.Paint();
var ARect : TRect;
begin
  Canvas.Lock();
  try
    Canvas.Brush.Style := bsSolid;

    {
      Draw Background
    }
    Canvas.Brush.Color := FColor;
    Canvas.FillRect(self.ClientRect);

    {
      Draw Borders
    }
    Canvas.Brush.Color := FBorderColor;
    ///

    if (FBorderTop > 0) then begin
      ARect.Left   := 0;
      ARect.Top    := 0;
      ARect.Width  := ClientWidth;
      ARect.Height := FBorderTop;

      Canvas.FillRect(ARect);
    end;

    if (FBorderRight > 0) then begin
      ARect.Left   := (ClientWidth - FBorderRight);
      ARect.Top    := 0;
      ARect.Width  := FBorderRight;
      ARect.Height := ClientHeight;

      Canvas.FillRect(ARect);
    end;

    if (FBorderBottom > 0) then begin
      ARect.Left   := 0;
      ARect.Top    := (ClientHeight - FBorderBottom);
      ARect.Width  := ClientWidth;
      ARect.Height := FBorderBottom;

      Canvas.FillRect(ARect);
    end;

    if (FBorderleft > 0) then begin
      ARect.Left   := 0;
      ARect.Top    := 0;
      ARect.Width  := FBorderleft;
      ARect.Height := ClientHeight;

      Canvas.FillRect(ARect);
    end;
  finally
    Canvas.UnLock();
  end;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7Panel.SetBorder(AIndex : Integer; AValue : Integer);
begin
  case AIndex of
    0 : FBorderTop    := AValue;
    1 : FBorderLeft   := AValue;
    2 : FBorderRight  := AValue;
    3 : FBorderBottom := AValue;
  end;

  ///
  Invalidate();
end;

procedure TS7Panel.SetColor(AIndex : Integer; AValue : TColor);
begin
  case AIndex of
    0 : FColor       := AValue;
    1 : FBorderColor := AValue;
  end;

  ///
  Invalidate();
end;

end.
