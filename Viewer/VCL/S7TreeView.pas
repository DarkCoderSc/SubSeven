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

unit S7TreeView;

interface

uses System.Classes, VirtualTrees, VCL.Graphics, WinAPI.Windows;

type
  TTreeData = record
    ItemName : String;
    Index    : Integer;
  end;
  PTreeData = ^TTreeData;

  TOnItemClick = procedure(Sender : TObject; AIndex : Integer; AItemName : String) of object;

  TS7VirtualStringTree = class(TCustomVirtualStringTree)
  private
    FDottedBrush : HBRUSH;
    FOnItemClick : TOnItemClick;

    {@M}
    function GenerateDottedBrush() : HBRUSH;
  protected
    procedure DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType); override;
    procedure DrawDottedHLine(const PaintInfo: TVTPaintInfo; Left, Right, Top: Integer); override;
    procedure DrawDottedVLine(const PaintInfo: TVTPaintInfo; Top, Bottom, Left: Integer; UseSelectedBkColor: Boolean = False); override;
    procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); override;
    procedure DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal); override;
    procedure DoNodeClick(const HitInfo: THitInfo); override;
    procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); override;
  public
    {@M}
    function AddItem(ACaption : String; AIndex : Cardinal; AParent : PVirtualNode = nil) : PVirtualNode;

    {@C}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  published
    property Align;
    property Margins;
    property Alignwithmargins;
    property Enabled;
    property Visible;

    {@G/S}
    property OnItemClick : TOnItemClick read FOnItemClick write FOnItemClick;
  end;

implementation

uses S7Theme, math, VCL.Forms;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7VirtualStringTree.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.Color := clBlack;
  self.Colors.TreeLineColor := MAIN_BLUE;

  self.Header.Options := self.Header.Options + [hoAutoResize];

  self.TreeOptions.SelectionOptions := self.TreeOptions.SelectionOptions + [toFullRowSelect];

  self.TreeOptions.PaintOptions := self.TreeOptions.PaintOptions - [
                                                                      toShowRoot,
                                                                      toShowButtons,
                                                                      toUseBlendedSelection,
                                                                      toThemeAware
  ] + [
          toAlwaysHideSelection,
          toHideSelection,
          toHideFocusRect
  ];

  self.Font.Name   := FONT_1;
  self.Font.Height := -11;

  self.Header.Font.Name   := FONT_1;
  self.Header.Font.Height := -11;

  FDottedBrush := GenerateDottedBrush();

  self.BorderStyle := bsNone;

  self.Header.Columns.Add();

  self.NodeDataSize := SizeOf(TTreeData);

  self.Font.name := 'Arial';

  FOnItemClick := nil;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7VirtualStringTree.Destroy();
begin

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Add item to Tree
-------------------------------------------------------------------------------}
function TS7VirtualStringTree.AddItem(ACaption : String; AIndex : Cardinal; AParent : PVirtualNode = nil) : PVirtualNode;
var AData : PTreeData;
    ANode : PVirtualNode;
begin
  ANode := self.AddChild(AParent);
  AData := self.GetNodeData(ANode);

  AData^.ItemName := ACaption;
  AData^.Index    := AIndex;

  ///
  result := ANode;
end;

{-------------------------------------------------------------------------------
  Override Virtual String Tree Methods
-------------------------------------------------------------------------------}

procedure TS7VirtualStringTree.DoPaintText(Node: PVirtualNode; const Canvas: TCanvas; Column: TColumnIndex; TextType: TVSTTextType);
begin
  inherited DoPaintText(Node, Canvas, Column, TextType);
  ///

  if (Node.ChildCount > 0) then
    Canvas.Font.Color := MAIN_BLUE
  else
    Canvas.Font.Color := clWhite;
end;

function TS7VirtualStringTree.GenerateDottedBrush() : HBRUSH;
const LineBitsDotted: array [0..8] of Word = (
                                                $55,
                                                $AA,
                                                $55,
                                                $AA,
                                                $55,
                                                $AA,
                                                $55,
                                                $AA,
                                                $55
);
var APatternBitmap : HBITMAP;
begin
  APatternBitmap := WinAPI.Windows.CreateBitmap(8, 8, 1, 1, @LineBitsDotted);
  try
    result := WinAPI.Windows.CreatePatternBrush(APatternBitmap);
  finally
    DeleteObject(APatternBitmap);
  end;
end;

procedure TS7VirtualStringTree.DrawDottedHLine(const PaintInfo: TVTPaintInfo; Left, Right, Top: Integer);
var ARect : TRect;
begin
  ARect := Rect(
                  Min(Left, Right),
                  Top + 1,
                  Max(Left, Right) + 1,
                  Top + 2
  );

  Brush.Color := clBlack;

  PaintInfo.Canvas.Font.Color := MAIN_BLUE;

  WinAPI.Windows.FillRect(
                            PaintInfo.Canvas.Handle,
                            ARect,
                            FDottedBrush
  );
end;

procedure TS7VirtualStringTree.DrawDottedVLine(const PaintInfo: TVTPaintInfo; Top, Bottom, Left: Integer; UseSelectedBkColor: Boolean = False);
var ARect : TRect;
begin
  Brush.Color := clBlack;

  ARect := Rect(
                  Left,
                  Min(Top, Bottom),
                  Left + 1,
                  Max(Top, Bottom) + 1
  );

  PaintInfo.Canvas.Font.Color := MAIN_BLUE;

  Winapi.Windows.FillRect(PaintInfo.Canvas.Handle, ARect, FDottedBrush);
end;

procedure TS7VirtualStringTree.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var ASelected : Boolean;
    Y         : Integer;
begin
  inherited DoBeforeCellPaint(Canvas, Node, Column, CellPaintMode, CellRect, ContentRect);
  ///

  {
    Draw Selected Background
  }
  CellPaintMode := TVTCellPaintMode.cpmPaint;

  ASelected := (vsSelected in Node.States);
  if ASelected then begin
    Canvas.Brush.Color := DARK_BLUE;

    Canvas.FillRect(CellRect);
  end;

  {
    Draw "-" on root node
  }
  if self.GetNodeLevel(Node) = 0 then begin
    if Node.ChildCount > 0 then
      Canvas.Pen.Color := MAIN_BLUE
    else
      Canvas.Pen.Color := clWhite;

    Y := CellRect.Top + ((CellRect.Height div 2) + 1);
    Canvas.MoveTo(CellRect.Left + 13, Y);

    Canvas.LineTo(CellRect.Left + 15, Y);
  end;
end;

procedure TS7VirtualStringTree.DoTextDrawing(var PaintInfo: TVTPaintInfo; const Text: string; CellRect: TRect; DrawFormat: Cardinal);
begin
  if self.GetNodeLevel(PaintInfo.Node) = 0 then
    CellRect.Left := CellRect.Left + 12;

  inherited;
end;

procedure TS7VirtualStringTree.DoNodeClick(const HitInfo: THitInfo);
var AData : PTreeData;
begin
  inherited;

  if HitInfo.HitNode.ChildCount > 0 then
    self.Expanded[HitInfo.HitNode] := not (vsExpanded in HitInfo.HitNode.States)
  else if Assigned(FOnItemClick) then begin
    AData := self.GetNodeData(HitInfo.HitNode);

    if Assigned(AData) then
      FOnItemClick(self, AData^.Index, AData^.ItemName);
  end;
end;

procedure TS7VirtualStringTree.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
var AData : PTreeData;
begin
  inherited;
  ///

  AData := self.GetNodeData(pEventArgs.Node);
  if Assigned(AData) then begin
    if pEventArgs.Column = 0 then
      pEventArgs.CellText := AData^.ItemName;
  end;
end;

end.
