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

unit S7Classes;

interface

uses System.Classes, VCL.Graphics, VCL.Controls;

type
  TS7ControlState = (
                      csNormal,
                      csHover,
                      csActive,
                      csFocus,
                      csDisabled
  );

  TS7StateColors = class(TPersistent)
  private
    FOwner    : TControl;

    FNormal   : TColor;
    FHover    : TColor;
    FActive   : TColor;
    FFocus    : TColor;
    FDisabled : TColor;

    {@M}
    procedure SetColor(const AIndex : Integer; const AColor : TColor);
  public
    {@C}
    constructor Create(AOwner : TControl);
    destructor Destroy(); override;

    {@M}
    procedure Assign(ASource : TPersistent); override;
  published
    {@M}
    function GetStateColor(AState : TS7ControlState) : TColor;

    {@G/S}
    property Normal   : TColor index 0 read FNormal   write SetColor;
    property Hover    : TColor index 1 read FHover    write SetColor;
    property Focus    : TColor index 2 read FFocus    write SetColor;
    property Active   : TColor index 3 read FActive   write SetColor;
    property Disabled : TColor index 4 read FDisabled write SetColor;
  end;

implementation

uses S7Theme, System.SysUtils, WinAPI.Windows;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    TS7StateColors


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7StateColors.Create(AOwner : TControl);
begin
  inherited Create();
  ///

  FOwner    := AOwner;

  FNormal   := clNone;
  FHover    := clNone;
  FFocus    := clNone;
  FActive   := clNone;
  FDisabled := clNone;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7StateColors.Destroy();
begin

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Get State Color
-------------------------------------------------------------------------------}
function TS7StateColors.GetStateColor(AState : TS7ControlState) : TColor;
begin
  case AState of
    csNormal   : result := FNormal;
    csHover    : result := FHover;
    csActive   : result := FActive;
    csFocus    : result := FFocus;
    csDisabled : result := FDisabled;

    else
      result := clNone;
  end;
end;

{-------------------------------------------------------------------------------
  ___assign
-------------------------------------------------------------------------------}
procedure TS7StateColors.Assign(ASource : TPersistent);
begin
  if ASource is TS7StateColors then begin
    FNormal   := TS7StateColors(ASource).Normal;
    FHover    := TS7StateColors(ASource).Hover;
    FFocus    := TS7StateColors(ASource).Focus;
    FActive   := TS7StateColors(ASource).Active;
    FDisabled := TS7StateColors(ASource).Disabled;
  end else
    inherited;
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

procedure TS7StateColors.SetColor(const AIndex : Integer; const AColor : TColor);
begin
  case AIndex of
    {Normal}
    0 : begin
      FNormal := AColor;
    end;

    {Hover}
    1 : begin
      FHover := AColor;
    end;

    {Focus}
    2 : begin
      FFocus := AColor;
    end;

    {Active}
    3 : begin
      FActive := AColor;
    end;

    {Disabled}
    4 : begin
      FDisabled := AColor;
    end;
  end;

  ///
  FOwner.Invalidate();
end;

end.
