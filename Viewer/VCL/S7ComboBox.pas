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

// Built from TFlatComboBoxUnit.pas (Simplified & Optimized)

unit S7ComboBox;

interface

uses System.Classes, VCL.Controls, VCL.StdCtrls, VCL.Graphics, WinAPI.Messages,
     WinAPI.Windows, S7Types;

type
  TS7ComboBox = class(TCustomComboBox)
  private
    FArrowColor        : TColor;
    FBorderColor       : TColor;
    FButtonBackground  : TColor;

    FListInstance     : Pointer;
    FSolidBorder      : Boolean;
    FButtonWidth      : Integer;
    FSysBtnWidth      : Integer;
    FChildHandle      : HWND;

    FValidators       : TValidators;

    FStatus           : TControlStatus;

    {@M}
    procedure WMSetFocus(var AMessage: TMessage);              message WM_SETFOCUS;
    procedure WMKillFocus(var AMessage: TMessage);             message WM_KILLFOCUS;
    procedure WMKeyDown(var AMessage: TMessage);               message WM_KEYDOWN;
    procedure WMPaint(var AMessage: TWMPaint);                 message WM_PAINT;
    procedure WMNCPaint(var AMessage: TMessage);               message WM_NCPAINT;
    procedure CMEnabledChanged(var AMessage: TMessage);        message CM_ENABLEDCHANGED;
    procedure CNCommand(var AMessage: TWMCommand);             message CN_COMMAND;
    procedure CMFontChanged(var AMessage: TMessage);           message CM_FONTCHANGED;
    procedure CMSysColorChange(var AMessage: TMessage);        message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var AMessage: TWMNoParams); message CM_PARENTCOLORCHANGED;

    function GetSolidBorder(): Boolean;
    procedure SetSolidBorder();
    procedure RedrawBorders();
    procedure PaintBorder();

    procedure PaintButton();
    function GetButtonRect(): TRect;

    procedure InvalidateSelection();

    procedure ListWndProc(var AMessage: TMessage);

    procedure SetColors(AIndex : Integer; AValue : TColor);

    function GetEnabled() : Boolean;
    function GetIsValid() : Boolean;
    procedure SetEnabled(const AValue : Boolean);

    procedure SetStatus(const AStatus : TControlStatus);
    procedure DoValidate();
    procedure SetValidators(const AValue : TValidators);
  protected
    {@M}
    procedure WndProc(var AMessage: TMessage); override;
    procedure ComboWndProc(var AMessage: TMessage; ComboWnd: HWnd; ComboProc: Pointer); override;
    procedure Loaded(); override;
    procedure Change; override;

    {@G}
    property SolidBorder : Boolean read FSolidBorder;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    function HasSelectedItem() : Boolean;

    {@G}
    property IsValid : Boolean read GetIsValid;
  published
    {@G/S}
    property ArrowColor            : TColor index 0 read FArrowColor        write SetColors;
    property ButtonBackgroundColor : TColor index 1 read FButtonBackground  write SetColors;
    property BorderColor           : TColor index 2 read FBorderColor       write SetColors;

    property Enabled    : Boolean        read GetEnabled  write SetEnabled;
    property Status     : TControlStatus read FStatus     write SetStatus;
    property Validators : TValidators    read FValidators write SetValidators;

    property Align;
    property Style;
    property Color;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Font;
    property ItemHeight;
    property Items;
    property MaxLength;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property ItemIndex;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

uses Math, S7Theme, S7Validators, System.Types;

{-------------------------------------------------------------------------------
  ___change
-------------------------------------------------------------------------------}
procedure TS7ComboBox.Change();
begin
  inherited;
  ///

  if (self.Status = csError) then
    self.DoValidate();
end;


{-------------------------------------------------------------------------------
  Override Loaded Component Event
-------------------------------------------------------------------------------}
procedure TS7ComboBox.Loaded();
begin
  inherited;
  ///

end;


{-------------------------------------------------------------------------------
  Validate Input
-------------------------------------------------------------------------------}
procedure TS7ComboBox.DoValidate();
begin
  if Validate(self.Text, FValidators) then
    self.Status := csNormal
  else
    self.Status := csError;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7ComboBox.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  ControlStyle := ControlStyle -  [csOpaque];

  TControlCanvas(Canvas).Control := self;

  FListInstance    := MakeObjectInstance(ListWndProc);

  FSysBtnWidth     := GetSystemMetrics(SM_CXVSCROLL);

  FArrowColor           := MAIN_GRAY;
  FBorderColor          := MAIN_GRAY;
  self.Color            := clBlack;
  FButtonBackground     := clBlack;

  self.Font.Color       := clWhite;
  self.Font.Name        := FONT_1;
  self.Font.Height      := -11;

  FValidators := [];

  self.DoubleBuffered := True;

  FButtonWidth     := 11;
  FChildHandle     := 0;

  FStatus := csNormal;

  ShowHint := True;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7ComboBox.Destroy();
begin
  FreeObjectInstance(FListInstance);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Custom Control Window Proc
-------------------------------------------------------------------------------}

procedure TS7ComboBox.WndProc(var AMessage: TMessage);
begin
  if (AMessage.Msg = WM_PARENTNOTIFY) then begin
    case LoWord(AMessage.wParam) of
      WM_CREATE:
        if FDefListProc <> nil then
        begin
          SetWindowLong(FListHandle, GWL_WNDPROC, Longint(FDefListProc));
          FDefListProc := nil;
          FChildHandle := AMessage.lParam;
        end
        else
          if FChildHandle = 0 then
            FChildHandle := AMessage.lParam
          else
            FListHandle := AMessage.lParam;
      end
  end else begin
    if (AMessage.Msg = WM_WINDOWPOSCHANGING) then begin
      if Style in [csDropDown, csSimple] then
        SetWindowPos(
                        EditHandle,
                        0,
                        0,
                        0,
                        (ClientWidth - FButtonWidth - 2 * 2 - 4),
                        (Height - 2 * 2 - 2),
                        SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_NOREDRAW
        );
    end;
  end;

  inherited;
  ///

  if (AMessage.Msg = WM_CTLCOLORLISTBOX) then begin
    SetBkColor(AMessage.wParam, ColorToRGB(Color));

    AMessage.Result := CreateSolidBrush(ColorToRGB(Color));
  end;
end;

procedure TS7ComboBox.ListWndProc(var AMessage: TMessage);

  function GetFontHeight(Font: TFont): Integer;
  var DC          : HDC;
      ASaveFont   : HFont;
      ATextMetric : TTextMetric;
  begin
    DC := GetDC(0);
    try
      ASaveFont := SelectObject(DC, Font.Handle);

      GetTextMetrics(DC, ATextMetric);

      SelectObject(DC, ASaveFont);
    finally
      ReleaseDC(0, DC);
    end;

    ///
    result := round(ATextMetric.tmHeight + ATextMetric.tmHeight / 8);
  end;

begin
  case AMessage.Msg of
    WM_WINDOWPOSCHANGING:
      with TWMWindowPosMsg(AMessage).WindowPos^ do begin
        if Style in [csDropDown, csDropDownList] then
          cy := (GetFontHeight(Font)-2) * Min(DropDownCount, Items.Count) + 4
        else
          cy := (ItemHeight) * Min(DropDownCount, Items.Count) + 4;

        if cy <= 4  then
          cy := 10;
      end;
    else
      AMessage.Result := CallWindowProc(
                                          FDefListProc,
                                          FListHandle,
                                          AMessage.Msg,
                                          AMessage.WParam,
                                          AMessage.LParam
      );
  end;
end;

procedure TS7ComboBox.ComboWndProc(var AMessage: TMessage; ComboWnd: HWnd; ComboProc: Pointer);
begin
  inherited;
  ///

  if (ComboWnd <> EditHandle) then
    Exit();

  case AMessage.Msg of
    WM_SETFOCUS, WM_KILLFOCUS:
      SetSolidBorder();
  end;
end;

{-------------------------------------------------------------------------------
  Border Methods
-------------------------------------------------------------------------------}

procedure TS7ComboBox.SetSolidBorder();
var ASolidBorder: Boolean;
begin
  ASolidBorder := GetSolidBorder();
  if ASolidBorder <> FSolidBorder then begin
    FSolidBorder := ASolidBorder;

    ///
    RedrawBorders();
  end;
end;

function TS7ComboBox.GetSolidBorder(): Boolean;
begin
  result := ((csDesigning in ComponentState) and Enabled) or
    (not(csDesigning in ComponentState) and
    (DroppedDown or (GetFocus = Handle) or (GetFocus = EditHandle)));
end;

procedure TS7ComboBox.RedrawBorders();
begin
  PaintBorder();
  ///

  if Style <> csSimple then
    PaintButton();
end;

procedure TS7ComboBox.PaintBorder();
var DC             : HDC;
    ARect          : TRect;
    ABtnFaceBrush  : HBRUSH;
    AWindowBrush   : HBRUSH;
    ABorderColor   : TColor;
begin
  DC := GetWindowDC(Handle);
  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    Dec(ARect.Right, FButtonWidth + 1);

    if (self.Status = csError) and enabled then
      ABorderColor := MAIN_RED
    else
      ABorderColor := FBorderColor;

    ABtnFaceBrush := CreateSolidBrush(ColorToRGB(ABorderColor));
    AWindowBrush := CreateSolidBrush(ColorToRGB(Color));
    try
      FrameRect(DC, ARect, ABtnFaceBrush);
      InflateRect(ARect, -1, -1);

      FrameRect(DC, ARect, AWindowBrush);
      InflateRect(ARect, -1, -1);

      FrameRect(DC, ARect, AWindowBrush);
    finally
      DeleteObject(ABtnFaceBrush);
      DeleteObject(AWindowBrush);
    end;
  finally
    ReleaseDC(Handle, DC);
  end;
end;

{-------------------------------------------------------------------------------
  Buttons Methods
-------------------------------------------------------------------------------}

function TS7ComboBox.GetButtonRect(): TRect;
begin
  GetWindowRect(Handle, Result);
  OffsetRect(Result, -Result.Left, -Result.Top);

  Inc(Result.Left, ClientWidth - FButtonWidth);
  OffsetRect(Result, -1, 0);
end;

procedure TS7ComboBox.PaintButton();
var ARect        : TRect;
    X, Y         : Integer;
    AArrowColor  : TColor;
    ABorderColor : TColor;
begin
  ARect := GetButtonRect();
  InflateRect(ARect, 1, 0);

  if (self.Status = csError) and enabled then begin
    AArrowColor  := MAIN_RED;
    ABorderColor := MAIN_RED;
  end else begin
    AArrowColor  := FArrowColor;
    ABorderColor := FBorderColor;
  end;

  Canvas.Brush.Color := FButtonBackground;
  Canvas.FillRect(ARect);
  Canvas.Brush.Color := ABorderColor;
  Canvas.FrameRect(ARect);

  x := (ARect.Right - ARect.Left) div 2 - 6 + ARect.Left;

  if DroppedDown then
    y := (ARect.Bottom - ARect.Top) div 2 - 1 + ARect.Top
  else
    y := (ARect.Bottom - ARect.Top) div 2 - 1 + ARect.Top;

  if Enabled then begin
    {
      Control is Enabled
    }
    canvas.Brush.Color := AArrowColor;
    canvas.Pen.Color   := AArrowColor;

    if DroppedDown then
      canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])
    else
      canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);
  end else begin
    {
      Control is Disabled (TODO: Colors)
    }
    canvas.Brush.Color := clWhite;
    canvas.Pen.Color   := clWhite;

    Inc(x);
    Inc(y);

    if DroppedDown then
      canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])
    else
      canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);

    Dec(x);
    Dec(y);

    canvas.Brush.Color := clGray;
    canvas.Pen.Color   := clGray;

    if DroppedDown then
      canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])
    else
      canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);
  end;

  ExcludeClipRect(
                    Canvas.Handle,
                    (ClientWidth - FSysBtnWidth),
                    0,
                    ClientWidth,
                    ClientHeight
  );
end;

{-------------------------------------------------------------------------------
  Invalidate Selection
-------------------------------------------------------------------------------}
procedure TS7ComboBox.InvalidateSelection();
var ARect: TRect;
begin
  ARect := ClientRect;
  InflateRect(ARect, -2, -3);

  ARect.Left := ARect.Right - FButtonWidth - 8;
  Dec(ARect.Right, FButtonWidth + 3);

  if (GetFocus = Handle) and not DroppedDown then
    Canvas.Brush.Color := clHighlight
  else
    Canvas.Brush.Color := Color;

  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ARect);

  if (GetFocus = Handle) and not DroppedDown then begin
    ARect := ClientRect;
    InflateRect(ARect, -3, -3);
    Dec(ARect.Right, FButtonWidth + 2);

    Canvas.FrameRect(ARect);
    Canvas.Brush.Color := clWindow;
  end;

  ExcludeClipRect(
                    Canvas.Handle,
                    (ClientWidth - FSysBtnWidth - 2),
                    0,
                    ClientWidth,
                    ClientHeight
  );
end;

{-------------------------------------------------------------------------------
  ____messages
-------------------------------------------------------------------------------}

procedure TS7ComboBox.CMSysColorChange (var AMessage: TMessage);
begin
  Invalidate();
end;

procedure TS7ComboBox.CMParentColorChanged(var AMessage: TWMNoParams);
begin
  Invalidate();
end;

procedure TS7ComboBox.WMSetFocus(var AMessage: TMessage);
begin
  inherited;
  ///

  if (csDesigning in ComponentState) then
    Exit();

  SetSolidBorder();

  if not (Style in [csSimple, csDropDown]) then
    InvalidateSelection();
end;

procedure TS7ComboBox.WMKillFocus(var AMessage: TMessage);
begin
  inherited;
  ///

  if (csDesigning in ComponentState) then
    Exit();

  SetSolidBorder;

  if not (Style in [csSimple, csDropDown]) then
    InvalidateSelection();
end;

procedure TS7ComboBox.CMEnabledChanged(var AMessage : TMessage);
begin
  inherited;
  ///

  Invalidate();
end;

procedure TS7ComboBox.CNCommand(var AMessage: TWMCommand);
var ARect: TRect;
begin
  inherited;
  ///

  if AMessage.NotifyCode in [1, 9, CBN_DROPDOWN, CBN_SELCHANGE] then begin
    if not (Style in [csSimple, csDropDown]) then
      InvalidateSelection();
  end;

  if (AMessage.NotifyCode in [CBN_CLOSEUP]) then begin
    ARect := GetButtonRect;
    Dec(ARect.Left, 2);
    InvalidateRect(Handle, @ARect, FALSE);
  end;
end;

procedure TS7ComboBox.WMKeyDown(var AMessage: TMessage);
var AString : String;
begin
  AString := Text;

  inherited;
  ///

  if not (Style in [csSimple, csDropDown]) and (Text <> AString) then
    InvalidateSelection();
end;

procedure TS7ComboBox.WMPaint(var AMessage: TWMPaint);
var ARect        : TRect;
    DC           : HDC;
    APaintStruct : TPaintStruct;

    function RectInRect(R1, R2: TRect): Boolean;
    begin
      Result := IntersectRect(R1, R1, R2);
    end;

begin
  DC := BeginPaint(Handle, APaintStruct);
  try
    ARect := APaintStruct.rcPaint;
    if ARect.Right > Width - FButtonWidth - 4 then
      ARect.Right := Width - FButtonWidth - 4;

    FillRect(DC, ARect, Brush.Handle);
    if RectInRect(GetButtonRect, APaintStruct.rcPaint) then
      PaintButton();

    ExcludeClipRect(DC, ClientWidth - FSysBtnWidth - 2, 0, ClientWidth, ClientHeight);

    PaintWindow(DC);

    if (Style = csDropDown) and DroppedDown then begin
      ARect := ClientRect;
      InflateRect(ARect, -2, -2);
      ARect.Right := Width - FButtonWidth - 3;
      Canvas.Brush.Color := clWindow;
      Canvas.FrameRect(ARect);
    end else begin
      if Style <> csDropDown then
        InvalidateSelection;
    end;
  finally
    EndPaint(Handle, APaintStruct);
  end;
  RedrawBorders();

  ///
  AMessage.Result := 0;
end;

procedure TS7ComboBox.WMNCPaint(var AMessage: TMessage);
begin
  inherited;
  ///

  RedrawBorders();
end;

procedure TS7ComboBox.CMFontChanged(var AMessage: TMessage);
begin
  inherited;
  ///

  //ItemHeight := 13;

  RecreateWnd();
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7ComboBox.SetColors(AIndex : Integer; AValue : TColor);
begin
  case AIndex of
    0 : begin
      FArrowColor := AValue;
    end;

    1 : begin
      FButtonBackground := AValue;
    end;

    2 : begin
      FBorderColor := AValue;
    end;
  end;

  ///
  Invalidate();
end;

function TS7ComboBox.GetEnabled() : Boolean;
begin
  result := inherited Enabled;
end;

procedure TS7ComboBox.SetEnabled(const AValue : Boolean);
begin
  if AValue = inherited Enabled then
    Exit();

  inherited Enabled := AValue;

  if AValue then
    self.Font.Color := clWhite
  else
    self.Font.Color := clGray;
end;

procedure TS7ComboBox.SetStatus(const AStatus : TControlStatus);
begin
  if AStatus = FStatus then
    Exit();

  FStatus := AStatus;

  ///
  Invalidate();
end;

procedure TS7ComboBox.SetValidators(const AValue : TValidators);
begin
  if AValue = FValidators then
    Exit();
  ///

  FValidators := AValue;

  ///
  Invalidate();
end;

function TS7ComboBox.HasSelectedItem() : Boolean;
begin
  result := (self.ItemIndex >= 0) and (self.ItemIndex <= self.Items.count -1);
end;

function TS7ComboBox.GetIsValid() : Boolean;
begin
  self.DoValidate();
  ///

  result := (self.Status = csNormal);
end;

end.
