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

unit S7CTaskList;

interface

uses System.Classes, VCL.Controls, VCL.Graphics, VCl.ImgList;

type
  TS7CItem = class(TCollectionItem)
  private
    FCaption : String;
    FDone    : Boolean;

    {@M}
    procedure SetCaption(const AValue : String);
    procedure SetDone(const AValue : Boolean);

    procedure Invalidate();
  public
    {@C}
    constructor Create(ACollection : TCollection); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Caption : String  read FCaption write SetCaption;
    property Done    : Boolean read FDone    write SetDone;
  end;

  TS7CItems = class(TOwnedCollection)
  private
  protected
    {@M}
    function GetItem(Index: Integer): TS7CItem;
    procedure SetItem(Index: Integer; Value: TS7CItem);

    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    {@C}
    constructor Create(AOwner: TPersistent);

    {@M}
    function Add: TS7CItem;

    {@G/S}
    property Items[Index: Integer]: TS7CItem read GetItem write SetItem; default;
  end;

  TS7CTaskList = class(TGraphicControl)
  private
    FColor         : TColor;
    FItemHeight    : Integer;
    FImageIndexOff : Integer;
    FImageIndexOn  : Integer;
    FItems         : TS7CItems;
    FImageList     : TCustomImageList;

    {@M}
    procedure SetColor(const AValue : TColor);
    procedure SetItemHeight(const AValue : Integer);
    procedure SetImageIndex(const AIndex : Integer; const AValue : Integer);
  protected
    {@M}
    procedure Paint(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Color      : TColor           read FColor      write SetColor;
    property ItemHeight : Integer          read FItemHeight write SetItemHeight;
    property Items      : TS7CItems        read FItems      write FItems;
    property ImageList  : TCustomImageList read FImageList  write FImageList;

    property ImageIndexOn  : Integer index 1 read FImageIndexOn  write SetImageIndex;
    property ImageIndexOff : Integer index 0 read FImageIndexOff write SetImageIndex;

    //
    property Align;
    property AutoSize;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Touch;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses System.SysUtils, Winapi.Windows, S7Common;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    TS7CTaskList


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7CTaskList.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FImageIndexOff := -1;
  FImageIndexOn  := -1;
  FImageList     := nil;

  FColor         := clNone;
  FItemHeight    := 35;
  FItems         := TS7CItems.Create(self);

  Font.Color     := RGB(57, 57, 57);
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7CTaskList.Destroy();
begin
  if Assigned(FItems) then
    FreeAndNil(FItems);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TS7CTaskList.Paint();
var ARowRect    : TRect;
    ATextRect   : TRect;
    X, Y        : Integer;

    AGlyphOn    : VCL.Graphics.TBitmap;
    AGlyphOff   : VCL.Graphics.TBitmap;

    ACaption    : String;
    I           : Integer;
    ATextFormat : TTextFormat;
begin
  AGlyphOn  := nil;
  AGlyphOff := nil;
  ///

  if Assigned(FImageList) then begin
    if (FImageIndexOn >= 0) and (FImageIndexOn <= FImageList.Count -1) then begin
      AGlyphOn := VCL.Graphics.TBitmap.Create();

      InitializeBitmap32(AGlyphOn, FImageList.Width, FImageList.Height);

      FImageList.GetBitmap(FImageIndexOn, AGlyphOn);
    end;

    if (FImageIndexOff >= 0) and (FImageIndexOff <= FImageList.Count -1) then begin
      AGlyphOff := VCL.Graphics.TBitmap.Create();

      InitializeBitmap32(AGlyphOff, FImageList.Width, FImageList.Height);

      FImageList.GetBitmap(FImageIndexOff, AGlyphOff);
    end;
  end;

  {
    Define Row Rect
  }
  ARowRect.Left   := 0;
  ARowRect.Width  := ClientWidth;
  ARowRect.Top    := 0;
  ARowRect.Height := FItemHeight;

  {
    Define Text Rect
  }
  if Assigned(FImageList) then
    ATextRect.Left := FImageList.Width + 8
  else
    ATextRect.Left := 0;

  ATextRect.Width  := ClientWidth - ATextRect.Left;


  ATextFormat := [
                    tfEndEllipsis,
                    tfLeft,
                    tfSingleLine,
                    tfVerticalCenter
  ];

  Canvas.Lock();
  try
    Canvas.Brush.Style := bsSolid;
    ///

    {
      Draw Background
    }
    if FColor <> clNone then begin
      Canvas.Brush.Color := FColor;

      Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));
    end;

    Canvas.Font.Assign(Font);
    Canvas.Brush.Style := bsClear;

    {
      Draw Items
    }
    for I := 0 to FItems.Count -1 do begin
      {
        Draw Caption
      }
      ATextRect.Top    := ARowRect.Height * I;
      ATextRect.Height := ARowRect.Height;
      ///

      ACaption := FItems.Items[i].Caption;

      Canvas.TextRect(ATextRect, ACaption, ATextFormat);

      {
        Draw Picture
      }
      if Assigned(FImageList) then begin
        X := 0;
        Y := ATextRect.Top + (ARowRect.Height div 2) - (FImageList.Height div 2);

        if FItems.Items[I].Done then begin
          if Assigned(AGlyphOn) then
            Canvas.Draw(X, Y, AGlyphOn);
        end else begin
          if Assigned(AGlyphOff) then
            Canvas.Draw(X, Y, AGlyphOff);
        end;
      end;
    end;
  finally
    Canvas.Unlock();
  end;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7CTaskList.SetColor(const AValue : TColor);
begin
  if AValue = FColor then
    Exit();
  ///

  FColor := AValue;

  ///
  Invalidate();
end;

procedure TS7CTaskList.SetItemHeight(const AValue : Integer);
begin
  if FItemHeight = AValue then
    Exit();
  ///

  FItemHeight := AValue;

  ///
  Invalidate();
end;

procedure TS7CTaskList.SetImageIndex(const AIndex : Integer; const AValue : Integer);
begin
  case AIndex of
    0 : FImageIndexOff := AValue;
    1 : FImageIndexOn  := AValue;
  end;

  ///
  Invalidate();
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7CItem


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

constructor TS7CItem.Create(ACollection : TCollection);
begin
  inherited Create(ACollection);
  ///

  FCaption := Format('Items n°%d', [self.Index]);
  FDone    := False;
end;

destructor TS7CItem.Destroy();
begin
  inherited Destroy();
  ///

end;

procedure TS7CItem.SetCaption(const AValue : String);
begin
  if FCaption = AValue then
    Exit();

  FCaption := AValue;

  ///
  Invalidate();
end;

procedure TS7CItem.SetDone(const AValue : Boolean);
begin
  if FDone = AValue then
    Exit();

  FDone := AValue;

  ///
  Invalidate();
end;

procedure TS7CItem.Invalidate();
begin
  TGraphicControl(TCollection(self.GetOwner).Owner).Invalidate();
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7CItems


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

function TS7CItems.Add: TS7CItem;
begin
  Result := TS7CItem(inherited Add);
end;

procedure TS7CItems.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  case Action of
    cnAdded, cnRemoved : begin
      TGraphicControl(self.GetOwner).Invalidate();
    end;
  end;
end;

constructor TS7CItems.create(AOwner : TPersistent);
begin
  inherited Create(AOwner, TS7CItem);
  ///

end;

function TS7CItems.GetItem(Index: Integer): TS7CItem;
begin
  Result := TS7CItem(inherited GetItem(Index));
end;

procedure TS7CItems.SetItem(Index: Integer; Value: TS7CItem);
begin
  inherited SetItem(Index, Value);
  ///

end;

end.
