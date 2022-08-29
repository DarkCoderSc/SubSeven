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

unit uFormRemoteShell;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ___S7ContextWindow, Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.CaptionBar,
  Vcl.StdCtrls, Vcl.ComCtrls, S7Panel, S7ComboBox, Generics.Collections,
  S7Edit, S7ImageButton, Vcl.ExtCtrls, Sub7.Thread.Net.Client.RemoteShell;

type
  THistoryCursorDirection = (
    hcdForward,
    hcdBackward
  );

  TFormRemoteShell = class(TS7ContextWindow)
    CaptionBar: TS7CaptionBar;
    SubSevenForms: TS7Form;
    PanelClient: TS7Panel;
    PanelCommand: TS7Panel;
    EditCommand: TS7Edit;
    ButtonRun: TS7ImageButton;
    Terminal: TMemo;
    procedure ButtonRunClick(Sender: TObject);
    procedure EditCommandKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure TerminalMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CaptionBarClickCaption(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FRemoteShellHandler : TSub7ClientRemoteShell;
    FSessionId          : String;
    FHistory            : TStringList;
    FHistoryCursor      : Integer;
    FHistoryCommand     : String;
    FFirstArrowUp       : Boolean;


    {@M}
    procedure SendCommand(const ACommand : String);
    function LastHistoryRecord() : String;
    procedure AppendToHistory(const ACommand : String);
    procedure MoveHistoryCursor(const ADirection : THistoryCursorDirection);
    procedure MoveEditCommandCursorToEnd();
  public
    {@M}
    procedure RenderShellOutput(const AData : String);
    procedure SetActive(const AActive : Boolean);
    procedure CloseSession();

    {@C}
    constructor Create(AOwner : TComponent; const ASessionId : String; ARemoteShellHandler : TSub7ClientRemoteShell); overload;
    destructor Destroy(); override;
  end;

var
  FormRemoteShell: TFormRemoteShell;

implementation

uses System.SyncObjs, uFormMain, XSuperObject, Sub7.Core.Protocol,
     Sub7.Core.Utils;

{$R *.dfm}

procedure TFormRemoteShell.MoveEditCommandCursorToEnd();
begin
  EditCommand.SelStart  := EditCommand.GetTextLen;
  EditCommand.SelLength := 1;
end;

procedure TFormRemoteShell.MoveHistoryCursor(const ADirection : THistoryCursorDirection);
begin
  if FHistory.Count = 0 then
    Exit();
  ///

  case ADirection of
    // Move cursor back in history (previous records)
    hcdBackward : begin
      if FFirstArrowUp then
        FFirstArrowUp := False
      else
        if (FHistoryCursor > 0) then
          Dec(FHistoryCursor);
    end;

    // Move cursor up in historyh (newest records)
    hcdForward : begin
      if (FHistoryCursor < (FHistory.Count -1)) then
        Inc(FHistoryCursor);
    end;
  end;

  ///
  FHistoryCommand := FHistory.Strings[FHistoryCursor];
  EditCommand.Text := FHistoryCommand;
  self.MoveEditCommandCursorToEnd();
end;

function TFormRemoteShell.LastHistoryRecord() : String;
begin
  result := '';

  if FHistory.Count = 0 then
    Exit();
  ///

  result := FHistory.Strings[FHistory.Count -1];
end;

procedure TFormRemoteShell.AppendToHistory(const ACommand : String);
begin
  if self.LastHistoryRecord() <> ACommand then
    FHistory.Add(ACommand);

  if FHistoryCommand <> ACommand then
    FHistoryCursor := FHistory.Count -1;
end;

procedure TFormRemoteShell.CaptionBarClickCaption(Sender: TObject);
begin
  ///
end;

procedure TFormRemoteShell.CloseSession();
var AData : ISuperObject;
begin
  AData := TSuperObject.Create();

  AData.S['session_id'] := FSessionId;

  FRemoteShellHandler.SendCommand(rtClose, AData);
end;

procedure TFormRemoteShell.SetActive(const AActive : Boolean);
begin
  EditCommand.Enabled := AActive;
  ButtonRun.Enabled   := AActive;
  ///

  if not AActive then
  begin
    Terminal.Font.Color := $00BEBEBE;
    FRemoteShellHandler := nil;
  end else
    Terminal.Font.Color := clWhite;
end;

procedure TFormRemoteShell.TerminalMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var AOldStart, AOldLen : Integer;
begin
  if EditCommand.Enabled then
  begin
    AOldStart := EditCommand.SelStart;
    AOldLen   := EditCommand.SelLength;

    EditCommand.SetFocus();

    EditCommand.SelStart  := AOldStart;
    EditCommand.SelLength := AOldLen;
  end;
end;

constructor TFormRemoteShell.Create(AOwner : TComponent; const ASessionId : String; ARemoteShellHandler : TSub7ClientRemoteShell);
begin
  inherited Create(AOwner);
  ///

  FRemoteShellHandler := ARemoteShellHandler;
  FSessionId          := ASessionId;
  FHistory            := TStringList.Create();
  FHistoryCursor      := 0;
  FHistoryCommand     := '';
  FFirstArrowUp       := True;
end;

destructor TFormRemoteShell.Destroy();
begin
  if Assigned(FHistory) then
    FreeAndNil(FHistory);

  ///
  inherited Destroy();
end;

procedure TFormRemoteShell.RenderShellOutput(const AData : String);
begin
  Terminal.Lines.Append(AData);

  ///
  SendMessage(
                Terminal.Handle,
                WM_VSCROLL,
                SB_BOTTOM,
                0
  );
end;

procedure TFormRemoteShell.SendCommand(const ACommand : String);
var AData : ISuperObject;
begin
  if not Assigned(FRemoteShellHandler) then
    Exit();
  ///

  if iCompare(ACommand, 'cls') or
     iCompare(ACommand, 'clear') then begin
    ///
    Terminal.Clear();
  end else begin
    if not Assigned(FRemoteShellHandler) then
      Exit();

    AData := TSuperObject.Create();

    // Send command
    AData.S['session_id'] := FSessionId;
    AData.S['command']    := EditCommand.Text;

    FRemoteShellHandler.SendCommand(rtCommand, AData);

    // Append to local command history
    self.AppendToHistory(EditCommand.Text);

    FFirstArrowUp := True;
  end;

  ///
  EditCommand.Clear();
end;

procedure TFormRemoteShell.EditCommandKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_DOWN, VK_UP : Key := 0;
  end;
end;

procedure TFormRemoteShell.EditCommandKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    13 : begin
      ButtonRunClick(ButtonRun);
    end;
  end;
end;

procedure TFormRemoteShell.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_UP   : self.MoveHistoryCursor(hcdBackward);
    VK_DOWN : self.MoveHistoryCursor(hcdForward);
  end;
end;

procedure TFormRemoteShell.FormKeyPress(Sender: TObject; var Key: Char);
var AData : ISuperObject;
begin
  if not Assigned(FRemoteShellHandler) then
    Exit();
  ///

  if Key = ^C then begin
    AData := TSuperObject.Create();

    AData.S['session_id'] := FSessionId;

    FRemoteShellHandler.SendCommand(rtBreak, AData);
  end;
end;

procedure TFormRemoteShell.ButtonRunClick(Sender: TObject);
begin
  SendCommand(EditCommand.Text);
end;

end.
