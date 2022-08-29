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

unit uFormSockets;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.CaptionBar, S7Panel,
  VirtualTrees, WinAPI.Winsock2, S7ImageButton, Sub7.Viewer.VCL.CheckBox,
  Vcl.ExtCtrls, Sub7.Core.Protocol, Vcl.Menus, S7PopupMenu, Sub7.Core.Types,
  System.Win.TaskbarCore, Vcl.Taskbar, ___S7BaseForm, Sub7.Thread.Net.Client.Base;

type
  TTreeData = record
    Date       : TDateTime;
    WorkerKind : TWorkerKind;
    SocketFd   : TSocket;
    ThreadId   : Cardinal;
  end;
  PTreeData = ^TTreeData;

  TFormSockets = class(TS7BaseForm)
    SubSevenForms: TS7Form;
    CaptionBar: TS7CaptionBar;
    PanelClient: TS7Panel;
    VST: TVirtualStringTree;
    PanelHeader: TS7Panel;
    ButtonCloseAllSockets: TS7ImageButton;
    ButtonCloseSocket: TS7ImageButton;
    CheckBoxAutoScroll: TS7CheckBox;
    PopupMenuAction: TS7PopupMenu;
    closeselectedsocket1: TMenuItem;
    N1: TMenuItem;
    closeallsockets1: TMenuItem;
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure ButtonCloseAllSocketsClick(Sender: TObject);
    procedure ButtonCloseSocketClick(Sender: TObject);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure FormCreate(Sender: TObject);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure closeselectedsocket1Click(Sender: TObject);
    procedure closeallsockets1Click(Sender: TObject);
  private
    {@M}
    function GetNodeFromThreadId(const AThreadId : Cardinal) : PVirtualNode;
    procedure RefreshIconsStatus();
  protected

  public
    {@M}
    procedure AddClient(const AClient : TSub7ClientBase);
    procedure RemoveClient(const AThreadID : Cardinal);

    procedure CloseAllConnections();
  end;

var
  FormSockets: TFormSockets;

implementation

uses System.Math, System.DateUtils, uFormMain, Generics.Collections,
     Sub7.Viewer.Types;

{$R *.dfm}

procedure TFormSockets.RefreshIconsStatus();
begin
  ButtonCloseSocket.Enabled := VST.FocusedNode <> nil;

  self.closeselectedsocket1.Enabled := ButtonCloseSocket.Enabled;
end;

procedure TFormSockets.RemoveClient(const AThreadID : Cardinal);
var ANode : PVirtualNode;
begin
  ANode := self.GetNodeFromThreadId(AThreadID);
  if not Assigned(ANode) then
    Exit();
  ///

  VST.BeginUpdate();
  try
    VST.DeleteNode(ANode);
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormSockets.CloseAllConnections();
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  for ANode in VST.Nodes do begin
    pData := ANode.GetData;

    Winapi.Winsock2.closesocket(pData^.SocketFd);
  end;

  ///
  RefreshIconsStatus();
end;

procedure TFormSockets.closeallsockets1Click(Sender: TObject);
begin
  ButtonCloseAllSocketsClick(ButtonCloseAllSockets);
end;

procedure TFormSockets.closeselectedsocket1Click(Sender: TObject);
begin
  ButtonCloseSocketClick(ButtonCloseSocket);
end;

function TFormSockets.GetNodeFromThreadId(const AThreadId : Cardinal) : PVirtualNode;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  result := nil;
  ///

  for ANode in VST.Nodes do begin
    pData := ANode.GetData;

    if pData^.ThreadId = AThreadId then begin
      result := ANode;

      break;
    end;
  end;
end;

procedure TFormSockets.AddClient(const AClient : TSub7ClientBase);
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  VST.BeginUpdate();
  try
    ANode := VST.AddChild(nil);
    pData := ANode.GetData;

    pData^.SocketFd   := AClient.GetCurrentSocket();
    pData^.Date       := Now();
    pData^.WorkerKind := WorkerClassToKind(AClient);
    pData^.ThreadId   := AClient.ThreadID;
  finally
    VST.EndUpdate();
  end;

  ///
  if CheckBoxAutoScroll.Checked then
    VST.Perform(WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TFormSockets.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
  var AData1, AData2 : PTreeData;
begin
  AData1 := VST.GetNodeData(Node1);
  AData2 := VST.GetNodeData(Node2);
  ///

  if not Assigned(AData1) or not Assigned(AData2) then
    Exit();

  case column of
    0 : begin
      {
        Int
      }
      result := CompareValue(AData1^.SocketFd, AData2^.SocketFd);
    end;

    2 : begin
      {
        Int
      }
      result := CompareValue(Integer(AData1^.WorkerKind), Integer(AData2^.WorkerKind));
    end;

    3 : begin
      {
        Int
      }
      result := CompareValue(AData1^.ThreadID, AData2^.ThreadID);
    end;

    4 : begin
      {
        Date
      }
      result := CompareDateTime(AData1^.Date, AData2^.Date);
    end;
  end;
end;

procedure TFormSockets.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  RefreshIconsStatus();

  VST.Refresh();
end;

procedure TFormSockets.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  ///
end;

procedure TFormSockets.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormSockets.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var AData : PTreeData;
begin
  AData := VST.GetNodeData(Node);
  if not Assigned(AData) then
    Exit();

  case Column of
    0 : begin
      CellText := IntToStr(AData^.SocketFd);
    end;

    1 : begin
      CellText := WorkerKindToString(AData^.WorkerKind);
    end;

    2 : begin
      CellText := IntToStr(AData^.ThreadID)
    end;

    3 : begin
      CellText := DateTimeToStr(AData^.Date);
    end;
  end;
end;

procedure TFormSockets.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  RefreshIconsStatus();
end;

procedure TFormSockets.ButtonCloseAllSocketsClick(Sender: TObject);
begin
  self.CloseAllConnections();
end;

procedure TFormSockets.ButtonCloseSocketClick(Sender: TObject);
var pData : PTreeData;
begin
  if VST.FocusedNode <> nil then begin
    VST.BeginUpdate();
    try
      pData := VST.FocusedNode.GetData;

      Winapi.Winsock2.closesocket(pData^.SocketFd);
    finally
      VST.EndUpdate();
    end;
  end;

  ///
  RefreshIconsStatus();
end;

procedure TFormSockets.FormCreate(Sender: TObject);
begin
  self.RefreshIconsStatus();
end;

end.
