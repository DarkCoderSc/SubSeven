unit Sub7.Core.VCL.MultiPanel;

{
    OMultiPanel

    Version 1.3

    Copyright (C) 2012 Ondrej Pokorny
      http://www.kluug.net


  *** BEGIN LICENSE BLOCK *****

  OMultiPanel is licensed under MPL 1.1/GPL 3/LGPL 3 tri-license.

  MPL: http://www.mozilla.org/MPL/1.1/
  GPL: http://www.gnu.org/licenses/gpl-3.0.html
  LGPL: http://www.gnu.org/licenses/lgpl-3.0.de.html

  ***** END LICENSE BLOCK *****

  // Optimized & Edited by Jean-Pierre LESUEUR

}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  VCL.Graphics, VCL.Controls, VCL.Dialogs, VCL.ExtCtrls, System.Types, System.Win.Registry,
  System.IniFiles, System.Contnrs, VCL.StdCtrls;

type
  TPanelOrientation = (poHorizontal, poVertical);

  TS7CCustomMultiPanel = class;

  TOnPaintSizingBar = procedure(Sender: TS7CCustomMultiPanel; aCanvas: TCanvas; aBarRect: TRect; aHover: Boolean) of Object;
  TOnPaintSizingBarMethod = procedure(Sender: TS7CCustomMultiPanel; aCanvas: TCanvas; aBarRect: TRect; aHover: Boolean);

  TSplitterListItem = class(TObject)
  private
    fPxPosition: Integer;
    fPanelIndex: Integer;
  public
    constructor CreateAdd(aAddToList: TList);
  public
    property PxPosition: Integer read fPxPosition;
    property PanelIndex: Integer read fPanelIndex;
  end;

  TS7CMultiPanelCollection = class;
  TS7CMultiPanelItem = class(TCollectionItem)
  private
    FPosition : Double;
    FControl  : TControl;
    FVisible  : Boolean;
    procedure SetControl(const Value: TControl);
    procedure SetPosition(const Value: Double);
    procedure SetPositionOnlyWithCheck(const Value: Double);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure SetIndex(Value: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure CheckPosition(var Value: Double); virtual;
  public
    constructor Create(ACollection: TCollection); override;
  public
    function MPOwner: TS7CCustomMultiPanel;
    function MPCollection: TS7CMultiPanelCollection;
  published
    property Control: TControl read fControl write SetControl;
    property Position: Double read fPosition write SetPosition;
    property Visible: Boolean read fVisible write SetVisible;

    property Index;
  end;

  TS7CMultiPanelCollection = class(TOwnedCollection)
  protected
    function GetAttrCount: Integer; override;
    function GetItem(Index: Integer): TS7CMultiPanelItem;
    procedure SetItem(Index: Integer; Value: TS7CMultiPanelItem);

    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TPersistent);
  public
    function MPOwner: TS7CCustomMultiPanel;

    function Add: TS7CMultiPanelItem;

    procedure AddControl(AControl: TControl; AIndex: Integer = -1);
    procedure RemoveControl(AControl: TControl);
    function IndexOf(AControl: TControl): Integer;

    procedure SetDefaultPositions;

    property Items[Index: Integer]: TS7CMultiPanelItem read GetItem write SetItem; default;
  end;

  TConstraintSize = Integer;

  TS7CCustomMultiPanel = class(TCustomPanel)
  private
    FOnSplitterMoved      : TNotifyEvent;
    FOnPaintSizingBar     : TOnPaintSizingBar;

    FSplitterColor        : TColor;
    FSplitterHoverColor   : TColor;

    FHover                : Boolean;
    FHoverIndex           : Integer;
    FSizing               : Boolean;
    FSizingIndex          : Integer;

    FLastSizingLinePx     : Integer;
    FPrevBrush            : HBrush;

    FHitTest              : TPoint;
    FLineDC               : HDC;
    FBrush                : TBrush;

    FInResizeControls     : Boolean;
    FDoubleBufferedBitmap : TBitmap;

    FMinPosition          : Double;
    FSplitterWidth        : Integer;

    FPanels               : TS7CMultiPanelCollection;
    FSplittersList        : TObjectList;
    FOrientation          : TPanelOrientation;

    procedure SetPanelOrientation(const Value: TPanelOrientation);

    procedure SetMinPosition(const Value: Double);
    procedure SetSplitterWidth(const Value: Integer);

    procedure AllocateLineDC;
    procedure ReleaseLineDC;

    procedure DrawSizingLine({%H-}X, {%H-}Y: Integer);

    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SetCursor;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;


    procedure SetPanels(const Value: TS7CMultiPanelCollection);
    function GetSplitterColor: TColor;
    procedure SetSplitterColor(const Value: TColor);
    function GetSplitterHoverColor: TColor;
    procedure SetSplitterHoverColor(const Value: TColor);
    function GetSplitterPosition(Index: Integer): Integer;
    function MinPositionStored: Boolean;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
  protected
    function GetSettingsSection: String; virtual;
    function GetSettingsIdent: String; virtual;

    procedure CalcSizing(X, Y: Integer; var aSizing: Boolean; var aSizingIndex: Integer);
    function GetSizingRect(SplitterIndex: Integer): TRect;

    procedure Paint; override;
    procedure DoPaintSizingBar(aCanvas: TCanvas; aIndex: Integer; aRect: TRect); virtual;
    procedure InvalidateSplitter(aIndex: Integer);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    procedure AlignControls({%H-}AControl: TControl; var {%H-}Rect: TRect); override;
    procedure Loaded; override;
    procedure Resize; override;

    procedure ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight: TConstraintSize); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure InvalidateSplitters;
    procedure ResizeControls; virtual;

    procedure LoadPositionsFromRegistry(aReg: TRegIniFile); overload;
    procedure SavePositionsToRegistry(aReg: TRegIniFile); overload;
    procedure LoadPositionsFromRegistry(aReg: TRegIniFile; const aSection, aIdent: String); overload; virtual;
    procedure SavePositionsToRegistry(aReg: TRegIniFile; const aSection, aIdent: String); overload; virtual;

    procedure LoadPositionsFromIniFile(aIni: TCustomIniFile); overload;
    procedure SavePositionsToIniFile(aIni: TCustomIniFile); overload;
    procedure LoadPositionsFromIniFile(aIni: TCustomIniFile; const aSection, aIdent: String); overload; virtual;
    procedure SavePositionsToIniFile(aIni: TCustomIniFile; const aSection, aIdent: String); overload; virtual;

    procedure DoSplitterMoved;

    function FindPanel(aControl: TControl): TS7CMultiPanelItem;

    property SplitterPosition[Index: Integer]: Integer read GetSplitterPosition;

    property Panels             : TS7CMultiPanelCollection read FPanels               write SetPanels;
    property Orientation        : TPanelOrientation        read FOrientation          write SetPanelOrientation   default poHorizontal;
    property MinPosition        : Double                   read fMinPosition          write SetMinPosition        stored MinPositionStored;
    property SplitterWidth      : Integer                  read FSplitterWidth        write SetSplitterWidth      default 3;
    property SplitterColor      : TColor                   read GetSplitterColor      write SetSplitterColor      default clNone;
    property SplitterHoverColor : TColor                   read GetSplitterHoverColor write SetSplitterHoverColor default clNone;
    property OnSplitterMoved    : TNotifyEvent             read fOnSplitterMoved      write fOnSplitterMoved;
    property OnPaintSizingBar   : TOnPaintSizingBar        read fOnPaintSizingBar     write fOnPaintSizingBar;

    property BevelOuter  default bvNone;
    property BevelInner  default bvNone;
    property ParentColor default True;
  end;

  TS7CMultiPanel = class(TS7CCustomMultiPanel)
  published
    property Orientation;
    property Panels;
    property MinPosition;
    property SplitterWidth;
    property SplitterColor;
    property SplitterHoverColor;
    property OnSplitterMoved;
    property OnPaintSizingBar;
    property Align;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelKind;
    property Ctl3D;
    property ParentCtl3D;
    property ParentBackground;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property Color;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

//DEFAULTS
var
  OMP_SplitterColor      : TColor = clNone;
  OMP_SplitterHoverColor : TColor = clNone;
  OMP_SplitterSize       : Integer = 3;
  OMP_OnPaintSizingBar   : TOnPaintSizingBarMethod = nil;

implementation

uses System.Math, VCL.Themes, Winapi.ShellAPI;

{ TS7CCustomMultiPanel }

procedure TS7CCustomMultiPanel.AllocateLineDC;
begin
  FLineDC := GetDCEx(Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE);

  if FBrush = nil then
  begin
    FBrush := TBrush.Create;

    FBrush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  end;
  fPrevBrush := SelectObject(FLineDC, FBrush.Handle);
end;

procedure TS7CCustomMultiPanel.CalcSizing(X, Y: Integer; var aSizing: Boolean;
  var aSizingIndex: Integer);

  function NextTo(Pos1, Pos2: Double): Boolean;
  begin
    result := (Abs(Pos1 - Pos2) < FSplitterWidth);
  end;

var
  I: Integer;
begin
  aSizing := False;
  aSizingIndex := -1;

  if csDesigning in ComponentState then
    Exit;

  for I := 0 to fSplittersList.Count-1 do
  if PtInRect(GetSizingRect(I), Point(X, Y)) then
  begin
    aSizing := true;
    aSizingIndex := TSplitterListItem(fSplittersList[I]).PanelIndex;
    Break;
  end;
end;

procedure TS7CCustomMultiPanel.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  if not (csLoading in ComponentState) then
    if Message.Inserting and (Message.Control.Parent = Self) then
    begin
      DisableAlign;
      try
        Message.Control.Anchors := [];
        FPanels.AddControl(Message.Control);
      finally
        EnableAlign;
      end;
    end else
      FPanels.RemoveControl(Message.Control);
end;

procedure TS7CCustomMultiPanel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
end;

procedure TS7CCustomMultiPanel.CMMouseLeave(var Message: TMessage);
var
  xHover: Boolean;
begin
  xHover := fHover;
  fHover := False;

  inherited;

  if (xHover <> fHover) and (fHoverIndex >= 0) then
    InvalidateSplitter(fHoverIndex);
end;

type
  TMyControl = class(TControl);

procedure TS7CCustomMultiPanel.ConstrainedResize(var MinWidth, MinHeight, MaxWidth,
  MaxHeight: TConstraintSize);
  function GetControlsConstrainedHeight: Integer;
  var nW, nH, xW, xH: TConstraintSize;
    I: Integer;
  begin
    Result := 0;
    nW := 0; xW := 0; xH := 0;
    for I := 0 to ControlCount-1 do
    begin
      nH := Controls[I].Constraints.MinHeight;
      TMyControl(Controls[I]).ConstrainedResize(nW, nH, xW, xH);
      Inc(Result, nH);
    end;
  end;
begin
  inherited ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);

  if FOrientation = poHorizontal then
  begin
    MinWidth := Max(MinWidth, FPanels.Count*(FSplitterWidth*2) + GetControlsConstrainedHeight);
  end else
  begin
    MinHeight := Max(MinHeight, FPanels.Count*(FSplitterWidth*2) + GetControlsConstrainedHeight);
  end;
end;

constructor TS7CCustomMultiPanel.Create(AOwner: TComponent);
begin
  FPanels := TS7CMultiPanelCollection.Create(Self);
  fSplittersList := TObjectList.Create(True);
  fDoubleBufferedBitmap := TBitmap.Create;

  inherited;

  Caption := '';
  BevelOuter := bvNone;
  BevelInner := bvNone;
  ParentColor := True;

  fHover := True;
  fHoverIndex := -1;
  fSizing := False;
  fSizingIndex := -1;

  FOrientation := poHorizontal;
  fMinPosition := 0.02;
  fOnSplitterMoved := nil;

  fSplitterColor := OMP_SplitterColor;
  fSplitterHoverColor := OMP_SplitterHoverColor;
  FSplitterWidth := OMP_SplitterSize;

  self.DoubleBuffered := True;
end;

destructor TS7CCustomMultiPanel.Destroy;
begin
  FPanels.Free;
  fSplittersList.Free;
  fDoubleBufferedBitmap.Free;

  inherited;
end;

procedure TS7CCustomMultiPanel.DoPaintSizingBar(aCanvas: TCanvas; aIndex: Integer; aRect: TRect);
var
  xColor: TColor;
begin
  if Assigned(fOnPaintSizingBar) then
  begin
    fOnPaintSizingBar(Self, aCanvas, aRect, fHover and (fHoverIndex = aIndex));
  end else
  if Assigned(OMP_OnPaintSizingBar) then
  begin
    OMP_OnPaintSizingBar(Self, aCanvas, aRect, fHover and (fHoverIndex = aIndex));
  end else
  begin
    aCanvas.Brush.Style := bsSolid;
    if fHover and (fHoverIndex = aIndex) then
      xColor := SplitterHoverColor
    else
      xColor := SplitterColor;
    if xColor <> clNone then
    begin
      aCanvas.Brush.Color := xColor;
      aCanvas.FillRect(aRect);
    end;
  end;
end;

procedure TS7CCustomMultiPanel.DoSplitterMoved;
begin
  if Assigned(fOnSplitterMoved) then
    fOnSplitterMoved(Self);
end;

procedure TS7CCustomMultiPanel.DrawSizingLine(X, Y: Integer);
var
  xRect: TRect;
begin
  if fSizing and (FOrientation = poHorizontal) then
  begin
    xRect := Rect(Left + X-FSplitterWidth div 2, Top, FSplitterWidth, Height);
    fLastSizingLinePx := X;
  end else
  if fSizing and (FOrientation = poVertical) then
  begin
    xRect := Rect(Left, Top + Y-FSplitterWidth div 2, Width, FSplitterWidth);
    fLastSizingLinePx := Y;
  end;

  PatBlt(FLineDC, xRect.Left, xRect.Top, xRect.Right, xRect.Bottom, PATINVERT);
end;

function TS7CCustomMultiPanel.FindPanel(aControl: TControl): TS7CMultiPanelItem;
var
  I: Integer;
begin
  for I := 0 to FPanels.Count-1 do
  if FPanels[I].Control = aControl then
  begin
    Result := FPanels[I];
    Exit;
  end;
  Result := nil;
end;

function TS7CCustomMultiPanel.GetSplitterHoverColor: TColor;
begin
  Result := fSplitterHoverColor;
end;

function TS7CCustomMultiPanel.GetSettingsIdent: String;
var
  xControl: TWinControl;
begin
  Result := Self.Name;
  xControl := Self.Parent;
  while Assigned(xControl) do
  begin
    Result := xControl.Name+'.'+Result;
    xControl := xControl.Parent;
  end;
end;

function TS7CCustomMultiPanel.GetSettingsSection: String;
begin
  Result := 'S7CMultiPanel';
end;

function TS7CCustomMultiPanel.GetSizingRect(SplitterIndex: Integer): TRect;
var Px: Integer;
begin
  Px := TSplitterListItem(fSplittersList[SplitterIndex]).PxPosition;
  if FOrientation = poHorizontal then
    Result := Rect(Px, 0, Px+FSplitterWidth, ClientHeight)
  else
    Result := Rect(0, Px, ClientWidth, Px+FSplitterWidth);
end;

function TS7CCustomMultiPanel.GetSplitterColor: TColor;
begin
  Result := fSplitterColor;
end;

function TS7CCustomMultiPanel.GetSplitterPosition(Index: Integer): Integer;
begin
  if Index < fSplittersList.Count then
  begin
    Result := TSplitterListItem(fSplittersList[Index]).PxPosition;
  end else
  begin
    Result := 0;
  end;
end;

type
  TAccessWinControl = class(TWinControl);

procedure TS7CCustomMultiPanel.InvalidateSplitter(aIndex: Integer);
  procedure InvalidateWithParent(bControl: TWinControl; bRect: TRect);
  begin
    if not bControl.HandleAllocated then
      Exit;

    if TAccessWinControl(bControl).ParentBackground and (bControl.Parent <> nil)
    then begin
      OffsetRect(bRect, bControl.Left, bControl.Top);
      InvalidateWithParent(bControl.Parent, bRect);
      OffsetRect(bRect, -bControl.Left, -bControl.Top);
    end;

    InvalidateRect(bControl.Handle, @bRect, True);
  end;
var xR: TRect;
begin
  if aIndex < 0 then
    Exit;

  {$WARNINGS OFF}
  if HandleAllocated then
  begin
    xR := GetSizingRect(aIndex);
    if ThemeServices.ThemesEnabled then
    begin
      //Repaint parent under splitter if splitter is hidden (clNone etc)!!!
      InvalidateWithParent(Self, xR);
    end else
    begin
      InvalidateRect(Handle, @xR, False);
    end;
  end;
  {$WARNINGS ON}
end;

procedure TS7CCustomMultiPanel.InvalidateSplitters;
var
  I: Integer;
begin
  for I := 0 to fSplittersList.Count-1 do
    InvalidateSplitter(I);
end;

procedure TS7CCustomMultiPanel.Loaded;
begin
  inherited;

  ResizeControls;
end;

procedure TS7CCustomMultiPanel.LoadPositionsFromIniFile(aIni: TCustomIniFile;
  const aSection, aIdent: String);
var
  I: Integer;
begin
  for I := 0 to FPanels.Count-2 do
    FPanels[I].Position :=
      StrToFloatDef(
        aIni.ReadString(aSection, aIdent+'.Position'+IntToStr(I), ''),
        FPanels[I].Position);
end;

procedure TS7CCustomMultiPanel.LoadPositionsFromIniFile(aIni: TCustomIniFile);
begin
  LoadPositionsFromIniFile(aIni, GetSettingsSection, GetSettingsIdent);
end;

procedure TS7CCustomMultiPanel.LoadPositionsFromRegistry(aReg: TRegIniFile;
  const aSection, aIdent: String);
var I: Integer;
begin
  for I := 0 to FPanels.Count-2 do
    FPanels[I].Position :=
      StrToFloatDef(
        aReg.ReadString(aSection, aIdent+'.Position'+IntToStr(I), ''),
        FPanels[I].Position);
end;

procedure TS7CCustomMultiPanel.LoadPositionsFromRegistry(aReg: TRegIniFile);
begin
  LoadPositionsFromRegistry(aReg, GetSettingsSection, GetSettingsIdent);
end;

function TS7CCustomMultiPanel.MinPositionStored: Boolean;
begin
  Result := MinPosition <> 0.02;
end;

procedure TS7CCustomMultiPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (Button = mbLeft) and not (ssDouble in Shift) then
  begin
    CalcSizing(X, Y, fSizing, fSizingIndex);

    if fSizing then
      AllocateLineDC;

    DrawSizingLine(X, Y);
  end else
    inherited;
end;

procedure TS7CCustomMultiPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pos: Double;
  xHover: Boolean;
  xHoverIndex: Integer;
begin
  inherited;

  if fSizing and (fSizingIndex >= 0) and (ClientWidth > 0) and (ClientHeight > 0) then
  begin
    DrawSizingLine(fLastSizingLinePx, fLastSizingLinePx);

    if FOrientation = poHorizontal then
      Pos := X/ClientWidth
    else
      Pos := Y/ClientHeight;

    FPanels[fSizingIndex].CheckPosition(Pos);

    if FOrientation = poHorizontal then
      Pos := Pos*ClientWidth
    else
      Pos := Pos*ClientHeight;

    DrawSizingLine(Round(Pos), Round(Pos));
  end else
  begin
    CalcSizing(X, Y, {%H-}xHover, {%H-}xHoverIndex);
    if (xHoverIndex <> fHoverIndex) or (xHover <> fHover) then
    begin
      fHover := xHover;
      if fHoverIndex <> xHoverIndex then
        InvalidateSplitter(fHoverIndex);
      fHoverIndex := xHoverIndex;
      InvalidateSplitter(fHoverIndex);
    end;
  end;
end;

procedure TS7CCustomMultiPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  try
    if (fSizing) and (fSizingIndex >= 0) and (ClientWidth > 0) and (ClientHeight > 0) then
    begin
      Dec(X, FSplitterWidth div 2);
      Dec(Y, FSplitterWidth div 2);

      DrawSizingLine(fLastSizingLinePx, fLastSizingLinePx);

      SendMessage(Handle, WM_SetRedraw, 0, 0);
      try

        if FOrientation = poHorizontal then
          FPanels[fSizingIndex].Position := X/ClientWidth
        else
          FPanels[fSizingIndex].Position := Y/ClientHeight;
      finally
        ReleaseLineDC;

        SendMessage(Handle, WM_SetRedraw, 1, 0);
        RedrawWindow(Handle, nil, 0,
          RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN);

      end;
    end;
  finally
    fSizing := False;
    CalcSizing(X, Y, fHover, fHoverIndex);
    InvalidateSplitter(fHoverIndex);
  end;
end;

procedure TS7CCustomMultiPanel.Paint;
var
  I: Integer;
  xR: TRect;
  xC: TCanvas;
begin
  {$WARNINGS OFF}
  if not Assigned(fDoubleBufferedBitmap) then
  begin
    inherited;
    Exit;
  end;

  if not ThemeServices.ThemesEnabled then
  begin
    fDoubleBufferedBitmap.SetSize(Width, Height);

    xC := fDoubleBufferedBitmap.Canvas;
    if Color = clDefault then
      xC.Brush.Color := clBtnFace
    else
      xC.Brush.Color := Color;
    xC.FillRect(Rect(0, 0, Width, Height));
  end else
  begin
    xC := Canvas;

  end;

  xC.Lock;

//  if (csDesigning in ComponentState) and (FPanels.Count = 0) then
//  begin
//    xC.Pen.Color := clBlue;
//    xC.Brush.Style := bsClear;
//    xC.Rectangle(0, 0, ClientWidth, ClientHeight);
//    xC.MoveTo(0, 0);
//    xC.LineTo(ClientWidth, ClientHeight);
//    xC.MoveTo(ClientWidth, 0);
//    xC.LineTo(0, ClientHeight);
//    if Name <> '' then
//    begin
//      xR := Rect(0, 0, xC.TextWidth(Name) + 4, xC.TextHeight(Name) + 2);
//      OffsetRect(xR, (Width - xR.Right) div 2, (Height - xR.Bottom) div 2);
//      xC.Brush.Color := clWindow;
//      xC.Font.Color := clWindowText;
//      xC.Rectangle(xR);
//      xC.TextOut(xR.Left+2, xR.Top+1, Name);
//    end;
//  end;

  for I := 0 to fSplittersList.Count-1 do
  begin
    xR := GetSizingRect(I);
    DoPaintSizingBar(xC, I, xR);
  end;

  xC.Unlock;

  if not ThemeServices.ThemesEnabled then
  begin
    Canvas.Draw(0, 0, fDoubleBufferedBitmap);

    Brush.Style := bsClear;//Disable flicker
  end;
  {$WARNINGS ON}
end;

procedure TS7CCustomMultiPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  ResizeControls;
end;

procedure TS7CCustomMultiPanel.ReleaseLineDC;
begin
  if FPrevBrush <> 0 then
    SelectObject(FLineDC, FPrevBrush);
  ReleaseDC(Parent.Handle, FLineDC);
  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;

procedure TS7CCustomMultiPanel.Resize;
begin
  inherited;

  DoSplitterMoved;
end;

procedure TS7CCustomMultiPanel.ResizeControls;
var
  I, L: Integer;
  xPanel: TS7CMultiPanelItem;
  xPosAbs, xDefaultSize: Integer;
  xSplitterItem: TSplitterListItem;
begin
  if fInResizeControls then
    Exit;

  fInResizeControls := True;
  try
    if not(
       not (csLoading in ComponentState) and
       not (csUpdating in ComponentState) and
       not (csDestroying in ComponentState))
    then
      Exit;

    fSplittersList.Clear;
    if csLoading in ComponentState then
      Exit;

    xPosAbs := 0;
    if (FPanels.Count > 0) then
      FPanels[FPanels.Count-1].FPosition := 1;//use fPosition => do not check!!

    for I := 0 to FPanels.Count-1 do
    begin
      xPanel := FPanels[I];
      if not Assigned(xPanel.Control) then
        Continue;
      if xPanel.Control.Parent <> Self then
      begin
        xPanel.Control := nil;
        Continue;
      end;

      //CHECK TAB STOP!!! -> BEFORE visible check
      if xPanel.Control is TWinControl then
        TWinControl(xPanel.Control).TabOrder := I;

      if (not xPanel.Visible and not (csDesigning in ComponentState)) then
      begin
        xPanel.Control.Visible := False;
        Continue;
      end;

      if FOrientation = poHorizontal then
      begin
        xDefaultSize := Round(xPanel.Position*ClientWidth)-xPosAbs;//MUST NOT BE CLIENT WIDTH!!!
        if not(csDesigning in ComponentState) then
        for L := I+1 to FPanels.Count-1 do
        begin
          if not FPanels[L].Visible then
            xDefaultSize := Round(FPanels[L].Position*ClientWidth)-xPosAbs
          else
            Break;
        end;

        xPanel.Control.SetBounds(xPosAbs, 0, xDefaultSize, ClientHeight);
        if xPanel.Control.Width <> xDefaultSize then
          xPanel.SetPositionOnlyWithCheck((xPosAbs+xPanel.Control.Width)/Max(ClientWidth, 1));//  <- Causes Stact Ovreflow when Position := ...; used

        Inc(xPosAbs, xPanel.Control.Width);
        if xPosAbs < ClientWidth then
        begin
          xSplitterItem := TSplitterListItem.CreateAdd(fSplittersList);
          xSplitterItem.fPxPosition := xPosAbs;
          xSplitterItem.fPanelIndex := I;
        end;
      end else
      begin
        xDefaultSize := Round(xPanel.Position*ClientHeight)-xPosAbs;
        if not(csDesigning in ComponentState) then
        for L := I+1 to FPanels.Count-1 do
        begin
          if not FPanels[L].Visible then
            xDefaultSize := Round(FPanels[L].Position*ClientHeight)-xPosAbs
          else
            Break;
        end;

        xPanel.Control.SetBounds(0, xPosAbs, ClientWidth, xDefaultSize);
        if xPanel.Control.Height <> xDefaultSize then
          xPanel.SetPositionOnlyWithCheck((xPosAbs+xPanel.Control.Height)/Max(ClientHeight, 1));//  <- Causes Stact Ovreflow when Position := ...; used

        Inc(xPosAbs, xPanel.Control.Height);
        if xPosAbs < ClientHeight then
        begin
          xSplitterItem := TSplitterListItem.CreateAdd(fSplittersList);
          xSplitterItem.fPxPosition := xPosAbs;
          xSplitterItem.fPanelIndex := I;
        end;
      end;
      Inc(xPosAbs, FSplitterWidth);
    end;
  finally
    fInResizeControls := False;
  end;
end;

procedure TS7CCustomMultiPanel.SavePositionsToRegistry(aReg: TRegIniFile);
begin
  SavePositionsToRegistry(aReg, GetSettingsSection, GetSettingsIdent);
end;

procedure TS7CCustomMultiPanel.SavePositionsToIniFile(aIni: TCustomIniFile;
  const aSection, aIdent: String);
var I: Integer;
begin
  for I := 0 to FPanels.Count-2 do
    aIni.WriteString(aSection, aIdent+'.Position'+IntToStr(I), FloatToStr(FPanels[I].Position));
end;

procedure TS7CCustomMultiPanel.SavePositionsToIniFile(aIni: TCustomIniFile);
begin
  SavePositionsToIniFile(aIni, GetSettingsSection, GetSettingsIdent);
end;

procedure TS7CCustomMultiPanel.SavePositionsToRegistry(aReg: TRegIniFile;
  const aSection, aIdent: String);
var I: Integer;
begin
  for I := 0 to FPanels.Count-2 do
    aReg.WriteString(aSection, aIdent+'.Position'+IntToStr(I), FloatToStr(FPanels[I].Position));
end;

procedure TS7CCustomMultiPanel.SetSplitterHoverColor(const Value: TColor);
begin
  if fSplitterHoverColor <> Value then
  begin
    fSplitterHoverColor := Value;
    Repaint;
  end;
end;

procedure TS7CCustomMultiPanel.SetMinPosition(const Value: Double);
begin
  if fMinPosition <> Value then
  begin
    fMinPosition := Value;
    if (fMinPosition < 0) then
     fMinPosition := 0;
    if (fMinPosition > 0.4) then
      fMinPosition := 0.4;
  end;
end;

procedure TS7CCustomMultiPanel.SetPanels(
  const Value: TS7CMultiPanelCollection);
begin
  FPanels.Assign(Value);
end;

procedure TS7CCustomMultiPanel.SetPanelOrientation(const Value: TPanelOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    ResizeControls;
  end;
end;

procedure TS7CCustomMultiPanel.SetSplitterColor(const Value: TColor);
begin
  if fSplitterColor <> Value then
  begin
    fSplitterColor := Value;
    Repaint;
  end;
end;

procedure TS7CCustomMultiPanel.SetSplitterWidth(const Value: Integer);
begin
  if FSplitterWidth <> Value then
  begin
    FSplitterWidth := Value;
    ResizeControls;
  end;
end;

procedure TS7CCustomMultiPanel.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  DefaultHandler(Msg);
  FHitTest := ScreenToClient(SmallPointToPoint(Msg.Pos));
end;

procedure TS7CCustomMultiPanel.WMSetCursor(var Message: TWMSetCursor);
var
  Cur: HCURSOR;
  SizeState: Boolean;
  I: Integer;
begin
  Cur := 0;
  if (Message.CursorWnd = Handle) and not (csDesigning in ComponentState) and (Message.HitTest = HTCLIENT) then
  begin
    if not fSizing then
      CalcSizing(Self.FHitTest.X, Self.FHitTest.Y, {%H-}SizeState, {%H-}I);

    if fSizing or SizeState then
    begin
      {$WARNINGS OFF}
      if (FOrientation = poHorizontal) then
        Cur := Winapi.Windows.LoadCursor(0, IDC_SIZEWE)
      else
      if (FOrientation = poVertical) then
        Cur := Winapi.Windows.LoadCursor(0, IDC_SIZENS);
      {$WARNINGS ON}
    end;
  end;

  if Cur <> 0 then
  begin
    Winapi.Windows.SetCursor(Cur);
    Message.Result := 1;
  end else
    inherited;
end;

{ TS7CMultiPanelItem }

procedure TS7CMultiPanelItem.AssignTo(Dest: TPersistent);
var
  xDest: TS7CMultiPanelItem;
begin
  if Dest is TS7CMultiPanelItem then
  begin
    xDest := TS7CMultiPanelItem(Dest);
    xDest.fPosition := Self.fPosition;
    xDest.fControl := Self.fControl;
    xDest.fVisible := Self.Visible;
  end;
end;

procedure TS7CMultiPanelItem.CheckPosition(var Value: Double);
begin
  if not(
     not (csLoading in MPOwner.ComponentState) and
     not (csUpdating in MPOwner.ComponentState) and
     not (csDestroying in MPOwner.ComponentState))
  then
    Exit;

  if (MPOwner = nil) or (csLoading in MPOwner.ComponentState) then//Component is loading
    Exit;

  if (Index = Collection.Count-1) then
    Value := 1;

  if (Index = 0) and (Value < MPOwner.MinPosition) then
    Value := MPOwner.MinPosition;
  if (Index = Collection.Count-2) and (Value > 1-MPOwner.MinPosition) then
    Value := 1-MPOwner.MinPosition;

  if(Index > 0) and (MPCollection[Index-1].Position+MPOwner.MinPosition > Value) then
    Value := MPCollection[Index-1].Position + MPOwner.MinPosition;
  if(Index < Collection.Count-1) and (MPCollection[Index+1].Position-MPOwner.MinPosition < Value) then
    Value := MPCollection[Index+1].Position - MPOwner.MinPosition;
end;

constructor TS7CMultiPanelItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  MPCollection.SetDefaultPositions;

  fControl := nil;
  fVisible := true;
end;

function TS7CMultiPanelItem.MPCollection: TS7CMultiPanelCollection;
begin
  if Collection is TS7CMultiPanelCollection then
    Result := TS7CMultiPanelCollection(Collection)
  else
    Result := nil;
end;

function TS7CMultiPanelItem.MPOwner: TS7CCustomMultiPanel;
begin
  Result := MPCollection.MPOwner;
end;

procedure TS7CMultiPanelItem.SetControl(const Value: TControl);
begin
  fControl := Value;
  
  if not (csLoading in MPOwner.ComponentState) and
     not (csUpdating in MPOwner.ComponentState) and
     not (csDestroying in MPOwner.ComponentState) then
  begin
    if Assigned(fControl) then
      fControl.Align := alNone;

    MPOwner.ResizeControls;
  end;
end;

procedure TS7CMultiPanelItem.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);

  if not (csLoading in MPOwner.ComponentState) and
     not (csUpdating in MPOwner.ComponentState) and
     not (csDestroying in MPOwner.ComponentState) then
  begin
    MPCollection.SetDefaultPositions;
    MPOwner.ResizeControls;
  end;
end;

procedure TS7CMultiPanelItem.SetPosition(const Value: Double);
begin
  if fPosition <> Value then
  begin
    fPosition  := Value;

    if not (csLoading in MPOwner.ComponentState) and
       not (csUpdating in MPOwner.ComponentState) and
       not (csDestroying in MPOwner.ComponentState) then
    begin
      CheckPosition(fPosition);

      MPOwner.ResizeControls;
      MPOwner.DoSplitterMoved;
    end;
  end;
end;

procedure TS7CMultiPanelItem.SetPositionOnlyWithCheck(const Value: Double);
begin
  if fPosition <> Value then
  begin
    fPosition  := Value;

    if not (csLoading in MPOwner.ComponentState) and
       not (csUpdating in MPOwner.ComponentState) and
       not (csDestroying in MPOwner.ComponentState) then
    begin
      CheckPosition(fPosition);
    end;
  end;
end;

procedure TS7CMultiPanelItem.SetVisible(const Value: Boolean);
begin
  if fVisible <> Value then
  begin
    fVisible := Value;

    if not (csLoading in MPOwner.ComponentState) and
       not (csUpdating in MPOwner.ComponentState) and
       not (csDestroying in MPOwner.ComponentState) then
    begin
      if Assigned(fControl) then
        fControl.Visible := fVisible;

      MPOwner.ResizeControls;
      MPOwner.DoSplitterMoved;
    end;
  end;
end;

{ TOMultiPanelCollection }

function TS7CMultiPanelCollection.Add: TS7CMultiPanelItem;
begin
  Result := TS7CMultiPanelItem(inherited Add);
end;

procedure TS7CMultiPanelCollection.AddControl(AControl: TControl; AIndex: Integer);
var I: Integer;
  Item: TS7CMultiPanelItem;
begin
  if IndexOf(AControl) >= 0 then
    Exit;

  if AIndex = -1 then
  for I := 0 to Count-1 do
  if not Assigned(Items[I].Control) then
  begin
    AIndex := I;
    Break;
  end;
  if AIndex = -1 then
    Item := Add
  else
    Item := Items[AIndex];

  Item.Control := AControl;
end;

constructor TS7CMultiPanelCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TS7CMultiPanelItem);
end;

function TS7CMultiPanelCollection.GetAttrCount: Integer;
begin
  Result := 0;
end;

function TS7CMultiPanelCollection.GetItem(Index: Integer): TS7CMultiPanelItem;
begin
  Result := TS7CMultiPanelItem(inherited GetItem(Index));
end;

function TS7CMultiPanelCollection.IndexOf(AControl: TControl): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Control = AControl then
      Exit;
  Result := -1;
end;

procedure TS7CMultiPanelCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;

end;

procedure TS7CMultiPanelCollection.RemoveControl(AControl: TControl);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].Control = AControl then
    begin
      Items[I].Control := nil;
      Delete(I);
      Exit;
    end;
end;

function TS7CMultiPanelCollection.MPOwner: TS7CCustomMultiPanel;
begin
  Result := GetOwner as TS7CCustomMultiPanel;
end;

procedure TS7CMultiPanelCollection.SetDefaultPositions;
var I: Integer;
begin
  if (MPOwner <> nil) and not (csLoading in MPOwner.ComponentState) then
  for I := 0 to Count-1 do
    TS7CMultiPanelItem(Items[I]).FPosition := (I+1)/Count;
end;

procedure TS7CMultiPanelCollection.SetItem(Index: Integer; Value: TS7CMultiPanelItem);
begin
  inherited SetItem(Index, Value);
end;

{ TSplitterListItem }

constructor TSplitterListItem.CreateAdd(aAddToList: TList);
begin
  inherited Create;

  fPxPosition := -1;
  fPanelIndex := -1;

  aAddToList.Add(Self);
end;

end.

