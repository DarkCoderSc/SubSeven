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

unit Sub7.Viewer.VCL.SubSevenForm;

interface

uses Winapi.Windows, System.Classes, VCL.Controls, VCL.Forms, Winapi.Messages,
     VCL.Graphics;

type
  TS7Form = class(TComponent)
  private
    FOldWindowProc : TWndMethod;
    FOwnerForm     : TForm;
    FResizable     : Boolean;
    FShowBorder    : Boolean;
    FColor         : TColor;

    {@M}
    procedure OnCustomWindowProc(var AMessage : TMessage);
    procedure SetResizable(AValue : Boolean);
    procedure SetShowBorder(const AValue : Boolean);
    procedure SetColor(const AValue : TColor);
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Resizable  : Boolean read FResizable  write SetResizable;
    property ShowBorder : Boolean read FShowBorder write SetShowBorder;
    property Color      : TColor  read FColor      write SetColor;
  end;

implementation

uses S7Theme;

{-------------------------------------------------------------------------------
  Handle Form Messages
-------------------------------------------------------------------------------}
procedure TS7Form.OnCustomWindowProc(var AMessage : TMessage);
var ADoCallBase : Boolean;
    ARect       : TRect;
    AFrameEdges : TRect;
begin
  ADoCallBase := True;
  try
    if (csDesigning in ComponentState) then
      Exit();
    ///

    case AMessage.Msg of
      {
        Form Border Resize
      }
      WM_NCHITTEST : begin
        if NOT FResizable then
          Exit();
        ///

        AFrameEdges.Left   := 8;
        AFrameEdges.Right  := 8;
        AFrameEdges.Bottom := 8;
        AFrameEdges.Top    := 8;

        {
          Define corners
        }
        ARect.Left   := TWMNCHitTest(AMessage).XPos  - FOwnerForm.BoundsRect.Left;
        ARect.Top    := TWMNCHitTest(AMessage).YPos  - FOwnerForm.BoundsRect.Top;
        ARect.Bottom := FOwnerForm.BoundsRect.Bottom - TWMNCHitTest(AMessage).YPos;
        ARect.Right  := FOwnerForm.BoundsRect.Right  - TWMNCHitTest(AMessage).XPos;

        if (ARect.Top < AFrameEdges.Top) and (ARect.Left < AFrameEdges.Left) then begin
          TWMNCHitTest(AMessage).Result := HTTOPLEFT;
        end else if (ARect.Top < AFrameEdges.Top) and (ARect.Right < AFrameEdges.Right) then begin
          TWMNCHitTest(AMessage).Result := HTTOPRIGHT;
        end else if (ARect.Bottom < AFrameEdges.Bottom) and (ARect.Left < AFrameEdges.Left) then begin
          TWMNCHitTest(AMessage).Result := HTBOTTOMLEFT;
        end else if (ARect.Bottom < AFrameEdges.Bottom) and (ARect.Right < AFrameEdges.Right) then begin
          TWMNCHitTest(AMessage).Result := HTBOTTOMRIGHT;
        end else if (ARect.Top < AFrameEdges.Top) then begin
          TWMNCHitTest(AMessage).Result := HTTOP;
        end else if (ARect.Left < AFrameEdges.Left) then begin
          TWMNCHitTest(AMessage).Result := HTLEFT;
        end else if (ARect.Bottom < AFrameEdges.Bottom) then begin
          TWMNCHitTest(AMessage).Result := HTBOTTOM;
        end else if (ARect.Right < AFrameEdges.Right) then begin
          TWMNCHitTest(AMessage).Result := HTRIGHT;
        end else begin
          TWMNCHitTest(AMessage).Result := HTCLIENT;
        end;

        ///
        ADoCallBase := False;
      end;
    end;
  finally
    if ADoCallBase then
      FOldWindowProc(AMessage);
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7Form.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FOwnerForm := nil;

  if NOT Assigned(AOwner) then
    Exit();

  if NOT (AOwner is TForm) then
    Exit();
  ///

  FOwnerForm := TForm(AOwner);

  SetColor(MAIN_GRAY);

  FOwnerForm.DoubleBuffered := True;
  FOwnerForm.BorderStyle    := bsNone;
  FOwnerForm.Font.Name      := FONT_1;
  FOwnerForm.Font.Height    := -11;

  SetShowBorder(True);

  //SetClassLong(FOwnerForm.Handle, GCL_STYLE, CS_DROPSHADOW); // Drop Shadow

  FOldWindowProc := FOwnerForm.WindowProc;
  FOwnerForm.WindowProc := OnCustomWindowProc;

  FResizable := True;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7Form.Destroy();
begin
  if Assigned(FOldWindowProc) then
    FOwnerForm.WindowProc := FOldWindowProc;

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7Form.SetResizable(AValue : Boolean);
begin
  if (FResizable = AValue) then
    Exit();
  ///

  FResizable := AValue;
end;

procedure TS7Form.SetShowBorder(const AValue : Boolean);
begin
  if not Assigned(FOwnerForm) then
    Exit();
  ///

  if AValue then
    FOwnerForm.BorderWidth := 2
  else
    FOwnerForm.BorderWidth := 0;

  FShowBorder := AValue;
end;

{ TS7Form.SetColor }

procedure TS7Form.SetColor(const AValue : TColor);
begin
  if not Assigned(FOwnerForm) then
    Exit();
  ///

  FColor           := AValue;
  FOwnerForm.Color := FColor;
end;

end.
