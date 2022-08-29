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

unit Sub7.Viewer.VCL.CaptionBar;

interface

uses Winapi.Windows, System.Classes, VCL.Graphics, VCL.Controls, Winapi.Messages, VCL.Forms, 
     System.SysUtils, System.UITypes, Sub7.Viewer.VCL.SubSevenForm, S7Theme;

type
  TS7CaptionBar = class;

  TByteArrayArray = array of array of Byte;

  TCaptionButtonState = (cbsNormal, cbsHover, cbsActive);

  TOnHandleSystemButton = procedure(Sender : TObject; var AHandled : Boolean) of object;

  TCaptionButton = class
  private
    FRect        : TRect;
    FVisible     : Boolean;
    FOwner       : TS7CaptionBar;
    FState       : TCaptionButtonState;

    {@M}
    function GetBackgroundColor() : TColor;
    function GetGlyphColor() : TColor;
    procedure SetState(AValue : TCaptionButtonState);
  public
    {@C}
    constructor Create(AOwner : TS7CaptionBar);

    {@G/S}
    property Rect    : TRect               read FRect    write FRect;
    property Visible : Boolean             read FVisible write FVisible;
    property State   : TCaptionButtonState read FState   write SetState;

    {@G}
    property BackgroundColor : TColor read GetBackgroundColor;
    property GlyphColor      : TColor read GetGlyphColor;
  end;

  TCloseButton    = class(TCaptionButton);
  TMaximizeButton = class(TCaptionButton);
  TMinimizeButton = class(TCaptionButton);
  TDockButton     = class(TCaptionButton);

  TS7CaptionBar = class(TGraphicControl)
  private
    FOldWindowProc   : TWndMethod;

    FOwnerForm       : TForm;

    FOldBoundRect    : TRect;

    FS7Form          : TS7Form;

    FCloseButton     : TCloseButton;
    FMaximizeButton  : TMaximizeButton;
    FMinimizeButton  : TMinimizeButton;
    FDockButton      : TDockButton;

    FMainColor       : TColor;
    FSecondaryColor  : TColor;

    FButtonDown      : TCaptionButton;
    FButtonHover     : TCaptionButton;

    FCollapsed       : Boolean;
    FOwnerOldClientH : Integer;
    FOldConstraintH  : Integer;
    FOldConstraintW  : Integer;

    FCollapsible     : Boolean;

    FMaximized       : Boolean;

    FBorderIcons     : TBorderIcons;

    FTransparent     : Boolean;

    FCaptionRect     : TRect;

    FSubCaption      : String;

    FTextCenter      : Boolean;

    FDockable        : Boolean;

    FOnMaximize      : TOnHandleSystemButton;
    FOnMinimize      : TOnHandleSystemButton;
    FOnRestore       : TOnHandleSystemButton;
    FOnClose         : TOnHandleSystemButton;
    FOnDock          : TNotifyEvent;
    FOnClickCaption  : TNotifyEvent;

    {@M}
    procedure OnCustomWindowProc(var AMessage : TMessage);
    function GetCaptionButtonFromCoord(X, Y : Integer) : TCaptionButton;

    procedure DrawLogo();

    function DrawCaptionButtons() : Integer;

    procedure doRestore();
    procedure doMaximize();
    procedure doMinimize();
    procedure doMaximizeRestore();
    procedure doCollapseRestore();
    procedure doClose();
    procedure doDock();

    procedure SetCaption(AValue : String);
    function GetCaption() : String;

    procedure SetDockable(const AValue : Boolean);

    procedure SetTransparent(const AValue : Boolean);

    procedure SetBorderIcons(AValue : TBorderIcons);
    procedure SetSubCaption(const AValue : String);

    procedure SetTextCenter(const AValue : Boolean);

    procedure PrepareCaptionButtons();

    procedure SetColor(const AIndex : Integer; const AColor : TColor);
  protected
    {@M}
    procedure Paint(); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    procedure Maximize();

    {@G}
    property Maximized    : Boolean read FMaximized;
    property OldBoundRect : TRect   read FOldBoundRect;
  published
    {@G/S}
    property Caption     : String       read GetCaption   write SetCaption;
    property BorderIcons : TBorderIcons read FBordericons write SetBorderIcons;
    property S7Form      : TS7Form      read FS7Form      write FS7Form;
    property SubCaption  : String       read FSubCaption  write SetSubCaption;
    property Dockable    : Boolean      read FDockable    write SetDockable;
    property Transparent : Boolean      read FTransparent write SetTransparent;
    property Collapsible : Boolean      read FCollapsible write FCollapsible;
    property TextCenter  : Boolean      read FTextCenter  write SetTextCenter;

    property MainColor      : TColor index 0 read FMainColor      write SetColor;
    property SecondaryColor : TColor index 1 read FSecondaryColor write SetColor;

    property OnMaximize     : TOnHandleSystemButton read FOnMaximize     write FOnMaximize;
    property OnMinimize     : TOnHandleSystemButton read FOnMinimize     write FOnMinimize;
    property OnRestore      : TOnHandleSystemButton read FOnRestore      write FOnRestore;
    property OnClose        : TOnHandleSystemButton read FOnClose        write FOnClose;
    property OnDock         : TNotifyEvent          read FOnDock         write FOnDock;
    property OnClickCaption : TNotifyEvent          read FOnClickCaption write FOnClickCaption;

    property Align;
    property AlignWithMargins;
    property Margins;
    property Visible;
    property Enabled;
    property Font;
  end;

  {
    Glyphs
  }
  const CLOSE_GLYPH : TByteArrayArray = [
                                          [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                          [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                          [$0, $1, $0, $0, $0, $0, $0, $0, $1, $0],
                                          [$0, $0, $1, $0, $0, $0, $0, $1, $0, $0],
                                          [$0, $0, $0, $1, $0, $0, $1, $0, $0, $0],
                                          [$0, $0, $0, $0, $1, $1, $0, $0, $0, $0],
                                          [$0, $0, $0, $0, $1, $1, $0, $0, $0, $0],
                                          [$0, $0, $0, $0, $1, $1, $0, $0, $0, $0],
                                          [$0, $0, $0, $1, $0, $0, $1, $0, $0, $0],
                                          [$0, $0, $1, $0, $0, $0, $0, $1, $0, $0],
                                          [$0, $1, $0, $0, $0, $0, $0, $0, $1, $0],
                                          [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                          [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];

  const MINIMIZE_GLYPH : TByteArrayArray = [
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$1, $1, $1, $1, $1, $1, $1, $1, $1, $1]
];

  const RESTORE_GLYPH : TByteArrayArray = [
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $1, $1, $1, $1, $1, $1, $0],
                                            [$0, $0, $0, $1, $0, $0, $0, $0, $1, $0],
                                            [$0, $1, $1, $1, $1, $1, $1, $0, $1, $0],
                                            [$0, $1, $0, $0, $0, $0, $1, $0, $1, $0],
                                            [$0, $1, $0, $0, $0, $0, $1, $0, $1, $0],
                                            [$0, $1, $0, $0, $0, $0, $1, $1, $1, $0],
                                            [$0, $1, $0, $0, $0, $0, $1, $0, $0, $0],
                                            [$0, $1, $1, $1, $1, $1, $1, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                            [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];


  const MAXIMIZE_GLYPH : TByteArrayArray = [
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $1, $1, $1, $1, $1, $1, $1, $1, $1],
                                              [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $1, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $1, $1, $1, $1, $1, $1, $1, $1, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];

  const DOCK_GLYPH : TByteArrayArray = [
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0],
                                              [$0, $0, $0, $0, $1, $1, $1, $1, $1, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $1, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $1, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $1, $1, $1, $1, $1, $1, $1, $1],
                                              [$0, $0, $0, $1, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $1, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $1],
                                              [$0, $0, $0, $0, $1, $1, $1, $1, $1, $1],
                                              [$0, $0, $0, $0, $0, $0, $0, $0, $0, $0]
  ];

implementation

uses System.Types;

{ TS7Caption.Maximize }

procedure TS7CaptionBar.Maximize();
begin
  if (biMaximize in self.BorderIcons) and (not FMaximized) then
    self.doMaximize();
end;

{ TS7Caption.MouseMove

  Spy on mouse position over the control }
procedure TS7CaptionBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ///

  if not Assigned(FOnClickCaption) then
    Exit();


end;

{-------------------------------------------------------------------------------
  System Button Actions
-------------------------------------------------------------------------------}

procedure TS7CaptionBar.doRestore();
var AHandled : Boolean;
begin
  if not FMaximized then
    Exit();
  ///

  AHandled := False;
  if Assigned(FOnRestore) then
    FOnRestore(self, AHandled);

  FMaximized := False;

  if Assigned(FS7Form) then
    FS7Form.ShowBorder := True;

  FOwnerForm.BoundsRect := FOldBoundRect;

  ///
  Invalidate();
end;

procedure TS7CaptionBar.doMaximize();
var AHandled : Boolean;
begin
  if FMaximized then
    Exit;
  ///

  AHandled := False;
  if Assigned(FOnMaximize) then
    FOnMaximize(self, AHandled);

  FOldBoundRect := FOwnerForm.BoundsRect;

  if Assigned(FS7Form) then
    FS7Form.ShowBorder := False;

  {
    Fit window to the correct screen monitor (depending on the position of our window)
  }
  with Screen.MonitorFromRect(FOwnerForm.BoundsRect).workAreaRect do begin
    FOwnerForm.SetBounds(Left, Top, Right - Left, Bottom - Top);
  end;

  FMaximized := True;

  ///
  Invalidate();
end;

procedure TS7CaptionBar.doMinimize();
var AHandled : Boolean;
begin
  AHandled := False;
  if Assigned(FOnMinimize) then
    FOnMinimize(self, AHandled);

  if AHandled then
    Exit();

  if FOwnerForm = Application.MainForm then
    Application.Minimize
  else
    ShowWindow(FOwnerForm.Handle, SW_MINIMIZE);
end;

procedure TS7CaptionBar.doClose();
var AHandled : Boolean;
begin
  AHandled := False;
  if Assigned(FOnClose) then
    FOnClose(self, AHandled);

  if AHandled then
    Exit();

  FOwnerForm.Close();
end;

procedure TS7CaptionBar.doDock();
begin
  if Assigned(FOnDOck) then
    FOnDock(self);
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7CaptionBar.Create(AOwner : TComponent);
var AObject : TComponent;
begin
  inherited Create(AOwner);
  ///

  //ControlStyle  := ControlStyle + [csAcceptsControls, csOpaque];

  FOldWindowProc := self.WindowProc;
  self.WindowProc := OnCustomWindowProc;

  //self.Align  := alTop;
  self.Height := 25;

  FOwnerForm := nil;

  FS7Form := nil;

  FButtonDown := nil;
  FButtonHover := nil;

  FTransparent := False;

  FCollapsed   := False;
  FCollapsible := True;

  FMaximized := False;
  FDockable  := False;

  FTextCenter := False;

  FOldConstraintH := 0;
  FOldConstraintW := 0;

  FMainColor      := MAIN_BLUE;
  FSecondaryColor := clBlack;

  FSubCaption := '';

  FBorderIcons := [biSystemMenu, biMinimize, biMaximize];

  {
    Get Owner Form
  }
  if Assigned(AOwner) then begin
     AObject := self;
     while true do begin
      AObject := AObject.Owner;

      if (AObject = nil) then
        break;

      if (AObject is TForm) then begin
        FOwnerForm := TForm(AObject);

        break;
      end;
    end;
  end;

  FOnMaximize     := nil;
  FOnMinimize     := nil;
  FOnRestore      := nil;
  FOnClose        := nil;
  FOnDock         := nil;
  FOnClickCaption := nil;

  {
    Create Caption Buttons Classes
  }
  FCloseButton    := TCloseButton.Create(self);
  FMaximizeButton := TMaximizeButton.Create(self);
  FMinimizeButton := TMinimizeButton.Create(self);
  FDockButton     := TDockButton.Create(self);
end;


{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
destructor TS7CaptionBar.Destroy();
begin
  if Assigned(FOldWindowProc) then
    self.WindowProc := FOldWindowProc;

  if Assigned(FCloseButton) then
    FreeAndNil(FCloseButton);

  if Assigned(FMaximizeButton) then
    FreeAndNil(FMaximizeButton);

  if Assigned(FMinimizeButton) then
    FreeAndNil(FMinimizeButton);

  if Assigned(FDockButton) then
    FreeAndNil(FDockButton);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Refresh Caption Button Location
-------------------------------------------------------------------------------}
procedure TS7CaptionBar.PrepareCaptionButtons();
var AButtonWidth : Integer;
    ARect        : TRect;
begin
  AButtonWidth := (self.ClientHeight - 2); // Minus two borders pixels

  ARect := TRect.Empty();
  ///

  {
    Close Button
  }
  if FCloseButton.Visible then begin
    ARect.Left   := (self.ClientWidth - 1 - AButtonWidth);
    ARect.Top    := 1;
    ARect.Width  := AButtonWidth;
    ARect.Height := AButtonWidth;

    FCloseButton.Rect := ARect;
  end;

  {
    Maximize Restore
  }
  if FMaximizeButton.Visible then begin
    ARect.Left   := (ARect.Left - 1) - AButtonWidth;
    ARect.Top    := 1;
    ARect.Width  := AButtonWidth;
    ARect.Height := AButtonWidth;

    FMaximizeButton.Rect := ARect;
  end;

  {
    Minimize
  }
  if FMinimizeButton.Visible then begin
    ARect.Left   := (ARect.Left - 1) - AButtonWidth;
    ARect.Top    := 1;
    ARect.Width  := AButtonWidth;
    ARect.Height := AButtonWidth;

    FMinimizeButton.Rect := ARect;
  end;

  {
    Dock Button
  }
  if FDockable then begin
    ARect.Left   := (ARect.Left - 1) - AButtonWidth;
    ARect.Top    := 1;
    ARect.Width  := AButtonWidth;
    ARect.Height := AButtonWidth;

    FDockButton.Rect := ARect;
  end;

  ///
  if not ARect.IsEmpty then
    Dec(FCaptionRect.Right, (Width - ARect.Left));
end;

{-------------------------------------------------------------------------------
  Draw Caption Buttons
-------------------------------------------------------------------------------}
function TS7CaptionBar.DrawCaptionButtons() : Integer;

    {
      Then we don't need to do the same thing multiple time
    }
    function DrawAButton(AButton : TCaptionButton; const AGlyphMatrix : TByteArrayArray) : TRect;
    var I, N         : Integer;
        X, Y         : Integer;
        AGlyphWidth  : Integer;
        AGlyphHeight : Integer;
    begin
      result := TRect.Empty();
      ///

      if NOT Assigned(AButton) then
        Exit();
      ///

      {
        Draw Background
      }
      Canvas.Brush.Color := AButton.BackgroundColor;
      Canvas.FillRect(AButton.Rect);

      Canvas.Brush.Color := FSecondaryColor;

      Canvas.MoveTo((AButton.Rect.Left - 1), AButton.Rect.Top);
      Canvas.LineTo((AButton.Rect.Left - 1), AButton.Rect.Bottom);

      {
        Draw Glyph
      }
      AGlyphWidth  := High(AGlyphMatrix[1]) +1;
      AGlyphHeight := High(AGlyphMatrix) +1;

      X := AButton.Rect.Left + ((FCloseButton.Rect.Width div 2) - (AGlyphWidth div 2));
      Y := AButton.Rect.Top + ((FCloseButton.Rect.Height div 2) - (AGlyphHeight div 2));

      for I := 0 to AGlyphWidth -1 do begin
        for N := 0 to AGlyphHeight -1 do begin
          if AGlyphMatrix[N][I] <> 0 then begin
            Canvas.Pixels[(X + I), (Y + N)] := AButton.GlyphColor;
          end;
        end;
      end;
    end;

begin
  result := 0;
  ///

  PrepareCaptionButtons();
  ///

  Canvas.Brush.Style := bsSolid;

  {
    Draw Close Button
  }
  if FCloseButton.Visible then
    DrawAButton(FCloseButton, CLOSE_GLYPH);

  {
    Draw Maximize / Restore Button
  }
  if FMaximizeButton.Visible then begin
    if FMaximized then
      DrawAButton(FMaximizeButton, RESTORE_GLYPH)
    else
      DrawAButton(FMaximizeButton, MAXIMIZE_GLYPH)
  end;

  {
    Draw Minimize Button
  }
  if FMinimizeButton.Visible then
    DrawAButton(FMinimizeButton, MINIMIZE_GLYPH);

  {
    Draw Dock Button
  }
  if FDockable then
    DrawAButton(FDockButton, DOCK_GLYPH);
end;

{-------------------------------------------------------------------------------
  Draw S7 Logo
-------------------------------------------------------------------------------}
procedure TS7CaptionBar.DrawLogo();
var X, Y    : Integer;
    AHeight : Integer;
    AWidth  : Integer;

    ARect   : TRect;
begin
  AHeight := ClientHeight;
  AWidth  := (AHeight + round((AHeight * 13) / 100));

  Canvas.Brush.Color := FSecondaryColor;
  Canvas.Pen.Color   := FSecondaryColor;

  ARect.Left   := 0;
  ARect.Top    := 0;
  ARect.Width  := AWidth;
  ARect.Height := AHeight;

  Canvas.FrameRect(ARect);

  Y := 0;
  X := round((AWidth * 50) / 100);

  Canvas.MoveTo(X, Y);

  Y := round((AHeight * 35) / 100);

  Canvas.LineTo(X, Y);

  X := round((AWidth * 25) / 100);

  Canvas.LineTo(X, Y);

  X := AWidth - round((AWidth * 35) / 100);

  Canvas.LineTo(X, Y);

  Y := AHeight;

  Canvas.LineTo(X, Y);

  X := 0;

  Y := round((AHeight * 65) / 100);

  Canvas.MoveTo(X, Y);

  X := round((AWidth * 50) / 100);

  Canvas.LineTo(X, Y);

  ///
  Inc(FCaptionRect.Left, AWidth + 8);
end;

{-------------------------------------------------------------------------------
  ___paint
-------------------------------------------------------------------------------}
procedure TS7CaptionBar.Paint();
var ARect       : TRect;
    ACaption    : String;
    ATextFormat : TTextFormat;
begin
  FCaptionRect := Rect(0, 0, Width, Height);
  ///

  Canvas.Lock();
  try
    {
      Prepare Canvas
    }
    self.Canvas.Pen.Width := 0;
    self.Canvas.Pen.Color := clNone;

    self.Canvas.Brush.Style := bsSolid;
    self.Canvas.Brush.Color := clNone;

    Canvas.Font.Name   := FONT_3;
    Canvas.Font.Color  := clWhite;
    Canvas.Font.Height := -11;

    Canvas.Brush.Style := bsSolid;

    ARect.Left   := 0;
    ARect.Top    := 0;
    ARect.Height := ClientHeight;
    ARect.Width  := ClientWidth;

    {
      Draw Background
    }
    if not FTransparent then begin
      Canvas.Brush.Color := FMainColor;

      Canvas.FillRect(ARect);
    end;

    {
      Draw Border
    }
    Canvas.Brush.Color := FSecondaryColor;

    Canvas.FrameRect(ARect);

    {
      Draw S7 Logo
    }
    DrawLogo();

    {
      Draw System Buttons (Caption Buttons)
    }
    DrawCaptionButtons();

    {
      Draw Caption Text
    }
    Canvas.Brush.Style := bsClear;

    ACaption := Lowercase(inherited Caption);

    if FSubCaption <> '' then
      ACaption := Format('%s / %s', [ACaption, FSubCaption]);

    ATextFormat := [tfLeft, tfVerticalCenter, tfEndEllipsis, tfSingleLine];
    if FTextCenter then
      ATextFormat := ATextFormat + [tfCenter];

    Canvas.TextRect(FCaptionRect, ACaption, ATextFormat);
  finally
    Canvas.Unlock();
  end;
end;

{-------------------------------------------------------------------------------
  Get Caption Button From Mouse Coord
-------------------------------------------------------------------------------}
function TS7Captionbar.GetCaptionButtonFromCoord(X, Y : Integer) : TCaptionButton;
begin
  result := nil;
  ///

  if ptinrect(FCloseButton.Rect, Point(X, Y)) then
    result := FCloseButton
  else if ptinrect(FMaximizeButton.Rect, Point(X, Y)) then
    result := FMaximizeButton
  else if ptinrect(FMinimizeButton.Rect, Point(X, Y)) then
    result := FMinimizeButton
  else if ptinrect(FDockButton.Rect, Point(X, Y)) then
    result := FDockButton;
end;

{-------------------------------------------------------------------------------
  Do Collapse / Restore
-------------------------------------------------------------------------------}
procedure TS7CaptionBar.doCollapseRestore();
begin
  if FMaximized or not FCollapsible then
    Exit();
  ///

  FCollapsed := not FCollapsed;

  if Assigned(FS7Form) then
    FS7Form.Resizable := not FCollapsed;
  ///

  if FCollapsed then begin
    FOwnerOldClientH := FOwnerForm.ClientHeight;
    FOldConstraintH  := FOwnerForm.Constraints.MinHeight;
    FOldConstraintW  := FOwnerForm.Constraints.MinWidth;

    FOwnerForm.Constraints.MinHeight := 0;
    FOwnerForm.Constraints.MinWidth  := 0;

    FOwnerForm.ClientHeight := self.ClientHeight;
  end else begin
    FOwnerForm.ClientHeight := FOwnerOldClientH;

    FOwnerForm.Constraints.MinHeight := FOldConstraintH;
    FOwnerForm.Constraints.MinWidth  := FOldConstraintW;
  end;
end;

{-------------------------------------------------------------------------------
  Depending on state, maximize or restore window
-------------------------------------------------------------------------------}
procedure TS7CaptionBar.doMaximizeRestore();
begin
  if (not (biMaximize in FBorderIcons)) or (FCollapsed) then
    Exit();
  ///

  if FMaximized then
    self.doRestore()
  else
    self.doMaximize();
end;

{-------------------------------------------------------------------------------
  Override Messages Received to Component
-------------------------------------------------------------------------------}
procedure TS7CaptionBar.OnCustomWindowProc(var AMessage : TMessage);
var //APoint  : TPoint;
    AButton : TCaptionButton;
    //ARect   : TRect;
begin
  FOldWindowProc(AMessage);
  ///

  if (csDesigning in ComponentState) then
    Exit;

  case AMessage.Msg of
    {
      Handle mouse double click
    }
    WM_LBUTTONDBLCLK : begin
      self.doMaximizeRestore();
    end;

    {
      On mouse left button down
    }
    WM_LBUTTONDOWN : begin
      AButton := GetCaptionButtonFromCoord(TWMLButtonDown(AMessage).XPos, TWMLButtonDown(AMessage).YPos);
      if Assigned(AButton) then begin
        AButton.State := cbsActive;

        FButtonDown := AButton;
      end;
      ///

      {
        Move Owner Form
      }
      if (NOT Assigned(AButton)) and Assigned(FOwnerForm) then begin
        ReleaseCapture();
        SendMessage(FOwnerForm.Handle, WM_SYSCOMMAND, $F012, 0);
      end;
    end;

    {
      Button Click (Up)
    }
    WM_LBUTTONUP : begin
      AButton := GetCaptionButtonFromCoord(TWMLButtonUp(AMessage).XPos, TWMLButtonUp(AMessage).YPos);
      if Assigned(AButton) then begin
        AButton.State := cbsHover;

        if (AButton = FButtonDown) then begin
          if AButton is TCloseButton then
            self.DoClose()
          else if AButton is TMaximizeButton then
            self.doMaximizeRestore()
          else if AButton is TMinimizeButton then
            self.DoMinimize()
          else if AButton is TDockButton then
            self.DoDock();
        end;
      end;

      if Assigned(FButtonDown) then begin
        FButtonDown.State := cbsNormal;

        FButtonDown := nil;
      end;
    end;

    {
      Surface Move (Enter)
    }
    WM_MOUSEMOVE : begin
      if Assigned(FButtonDown) then
        Exit();
      ///

      AButton := GetCaptionButtonFromCoord(TWMMouseMove(AMessage).XPos, TWMMouseMove(AMessage).YPos);
      if Assigned(AButton) then begin
        if (AButton <> FButtonHover) then begin

          if Assigned(FButtonHover) then
            FButtonHover.State := cbsNormal;

          FButtonHover := AButton;
        end;

        if (AButton.State <> cbsActive) then begin
          AButton.State := cbsHover;
        end;
      end else begin
        if Assigned(FButtonHover) then begin
          FButtonHover.State := cbsNormal;

          FButtonHover := nil;
        end;
      end;
    end;

    {
      Surface Leave
    }
    WM_MOUSELEAVE, {VCL ->} CM_MOUSELEAVE : begin
      if Assigned(FButtonDown) then
        Exit();
      ///

      if Assigned(FButtonHover) then begin
        if (FButtonHover.State <> cbsActive) then
          FButtonHover.State := cbsNormal;
      end;
    end;

    {
      Right Mouse Click
    }
    WM_RBUTTONDOWN : begin
      self.doCollapseRestore();
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7CaptionBar.SetCaption(AValue : String);
begin
  if AValue = inherited Caption then
    Exit();

  inherited Caption := AValue;

  FOwnerForm.Caption := AValue;

  ///
  Invalidate();
end;

procedure TS7CaptionBar.SetSubCaption(const AValue : String);
begin
  if AValue = FSubCaption then
    Exit();

  FSubCaption := AValue;

  ///
  Invalidate();
end;

function TS7CaptionBar.GetCaption() : String;
begin
  result := inherited Caption;
end;

procedure TS7CaptionBar.SetBorderIcons(AValue : TBorderIcons);
begin
  if (AValue = FBorderIcons) then
    Exit();

  FBorderIcons := AValue;

  {
    Update Border Icons visibility
  }
  self.FCloseButton.Visible    := (biSystemMenu in FBorderIcons);
  self.FMaximizeButton.Visible := (biMaximize in FBorderIcons);
  self.FMinimizeButton.Visible := (biMinimize in FBorderIcons);

  ///
  Invalidate();
end;

procedure TS7CaptionBar.SetDockable(const AValue : Boolean);
begin
  if AValue = FDockable then
    Exit();

  FDockable := AValue;

  ///
  Invalidate();
end;

{ TS7CaptionBar.SetTransparent }

procedure TS7CaptionBar.SetTransparent(const AValue : Boolean);
begin
  if AValue = FTransparent then
    Exit();

  FTransparent := AValue;

  ///
  Invalidate();
end;

{ TS7CaptionBar.SetTransparent }

procedure TS7CaptionBar.SetTextCenter(const AValue : Boolean);
begin
  if AValue = FTextCenter then
    Exit();

  FTextCenter := AValue;

  ///
  Invalidate();
end;

{ TS7CaptionBar.SetColor }

procedure TS7CaptionBar.SetColor(const AIndex : Integer; const AColor : TColor);
begin
  case AIndex of
    0 : begin
      FMainColor := AColor;
    end;

    1 : begin
      FSecondaryColor := AColor;
    end;
  end;

  ///
  Invalidate();
end;

{-------------------------------------------------------------------------------
  TCaptionButton.___constructor
-------------------------------------------------------------------------------}
constructor TCaptionButton.Create(AOwner : TS7CaptionBar);
begin
  inherited Create();

  if Assigned(AOwner) then
    FOwner := AOwner;

  FRect        := TRect.Empty;
  FVisible     := True;
  FState       := cbsNormal;
end;

{-------------------------------------------------------------------------------
  TCaptionButton.___getters___setters
-------------------------------------------------------------------------------}
function TCaptionButton.GetBackgroundColor() : TColor;
begin
  result := clNone;
  ///

  case FState of
    cbsNormal : begin
      result := FOwner.MainColor;
    end;

    cbsHover : begin
      result := FOwner.MainColor;
    end;

    cbsActive : begin
      result := FOwner.SecondaryColor;
    end;
  end;
end;

function TCaptionButton.GetGlyphColor() : TColor;
begin
  result := clNone;
  ///

  case FState of
    cbsNormal: result := FOwner.SecondaryColor;
    cbsHover:  result := FOwner.SecondaryColor;
    cbsActive: result := FOwner.MainColor;
  end;
end;

procedure TCaptionButton.SetState(AValue : TCaptionButtonState);
begin
  if AValue = FState then
    Exit();

  FState := AValue;

  ///
  if Assigned(FOwner) then
    FOwner.Invalidate();
end;

end.
