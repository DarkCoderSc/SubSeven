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
unit uFrameComboSessions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, S7ComboBox,
  Sub7.Core.Protocol, Sub7.Core.Messages.Listener, ___S7Frame;

type
  TFrameComboSessions = class(TS7Frame)
    ComboSessions: TS7ComboBox;
  private
    {@M}
    function GetSelectedSessionId() : Integer;
    function GetSessionIndex(const AValue : String) : Integer;
  protected
    {@M}
    procedure OnEnabled(); override;
    procedure OnDisabled(); override;
    procedure OnTerminalSessionUpdate(); override;
    procedure RefreshSessions();
  public
    {@G}
    property SessionId : Integer read GetSelectedSessionId;

    {@C}
    constructor Create(AOwner : TComponent); override;
  end;

implementation

uses uFrameSystemInformationHook, Sub7.Core.Exceptions, Sub7.Core.Bundle, uFormMain, Sub7.Core.Types,
     S7Types, Sub7.Viewer.Types;

{$R *.dfm}

constructor TFrameComboSessions.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  self.RefreshSessions();
end;

procedure TFrameComboSessions.OnTerminalSessionUpdate();
begin
  inherited;
  ///

  self.RefreshSessions();
end;

procedure TFrameComboSessions.OnEnabled();
begin
  inherited;
  ///

  self.ComboSessions.Enabled := True;
end;

procedure TFrameComboSessions.OnDisabled();
begin
  inherited;
  ///

  self.ComboSessions.Enabled := False;
end;

function TFrameComboSessions.GetSessionIndex(const AValue : String) : Integer;
var I : Integer;
begin
  result := -1;
  ///

  for I := 0 to ComboSessions.Items.Count -1 do begin
    if String.Compare(ComboSessions.Items.Strings[I], AValue, True) = 0 then begin
      result := I;

      break;
    end;
  end;
end;

function TFrameComboSessions.GetSelectedSessionId() : Integer;
var ASessionId : Integer;
    AValue     : String;
begin
  AValue := Trim(Copy(ComboSessions.Text, 1, Pos('-', ComboSessions.Text)-1));

  if not TryStrToInt(AValue, ASessionId) then
    raise ES7FormException.Create(ERR_FORM);

  ///
  result := ASessionId;
end;

procedure TFrameComboSessions.RefreshSessions();
var ASessionId    : Integer;
    ASessionUser  : String;
    AFrameAction  : TFrameSystemInformationHook;
    AIndex        : Integer;
    AOldValue     : String;
begin
  AOldValue := '';
  if ComboSessions.HasSelectedItem then
    AOldValue := ComboSessions.Text;
  ///

  ComboSessions.Clear();
  ///

  AFrameAction := TFrameSystemInformationHook(FormMain.GetDockedFrame(nmMachineActions));
  ///

  if not Assigned(AFrameAction) then
    Exit();

  for ASessionId in AFrameAction.ActiveSessions.Keys do begin
    if not AFrameAction.ActiveSessions.TryGetValue(ASessionId, ASessionUser) then
      continue;
    ///

    ComboSessions.Items.Add(Format('%d - %s', [ASessionId, ASessionUser]));
  end;
  ///

  { Restore }
  if AOldValue <> '' then begin
    AIndex := GetSessionIndex(AOldValue);
    ///

    if AIndex <> -1 then
      ComboSessions.ItemIndex := AIndex;
  end;
end;

end.
