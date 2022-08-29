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

unit uFormServerHive;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.CaptionBar, Sub7.Viewer.VCL.SubSevenForm, S7ImageButton,
  VirtualTrees, S7Panel, Vcl.Menus, S7PopupMenu, Sub7.Core.Messages.Listener,
  ___S7BaseForm;

type
  TTreeData = record
    Fingerprint  : String;
    DisplayName  : String;
    Name         : String;
    TrustedSince : String;
    Probing      : Boolean;
  end;
  PTreeData = ^TTreeData;

  TFormServerHive = class(TS7BaseForm)
    SubSevenForms: TS7Form;
    CaptionBar: TS7CaptionBar;
    PanelClient: TS7Panel;
    VST: TVirtualStringTree;
    PanelHeader: TS7Panel;
    ButtonDeleteAll: TS7ImageButton;
    ButtonDelete: TS7ImageButton;
    ButtonRename: TS7ImageButton;
    PopupMenuAction: TS7PopupMenu;
    delectselectedservertrust1: TMenuItem;
    deletealltrustedservers1: TMenuItem;
    N1: TMenuItem;
    renameselectedserverdisplayname1: TMenuItem;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);

    procedure FormShow(Sender: TObject);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonDeleteAllClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonRenameClick(Sender: TObject);
    procedure delectselectedservertrust1Click(Sender: TObject);
    procedure deletealltrustedservers1Click(Sender: TObject);
    procedure renameselectedserverdisplayname1Click(Sender: TObject);
  private
    FMessageListener : TS7MessageListener;
    FConnected       : Boolean;

    {@M}
    procedure RefreshIconsStatus();
    procedure OnReceiveSubSevenMessage(Sender : TObject; AMessage : Cardinal);
  protected
    {@M}
    procedure SaveToSettings(); override;
    procedure LoadFromSettings(); override;
  public
    {@M}
    procedure TrustNewServer(const AFingerprint : String);
    function GetServerNode(const AFingerprint : String) : PVirtualNode;
    procedure UpdateProbingInformation(const AFingerprint, AMachineName : String);
  end;

var
  FormServerHive: TFormServerHive;

implementation

uses System.StrUtils, Sub7.Viewer.Messages, uFormMain, XSuperObject;

{$R *.dfm}

procedure TFormServerHive.SaveToSettings();
var ANode       : PVirtualNode;
    pData       : PTreeData;
    AJsonArray  : ISuperArray;
    AJsonObject : ISuperObject;
begin
  inherited;
  ///

  AJsonArray := TSuperArray.Create();

  for ANode in VST.Nodes do begin
    pData := ANode.GetData;

    AJsonObject := TSuperObject.Create();

    AJsonObject.S['name']          := pData^.Name;
    AJsonObject.S['display_name']  := pData^.DisplayName;
    AJsonObject.S['fingerprint']   := pData^.Fingerprint;
    AJsonObject.S['trusted_since'] := pData^.TrustedSince;

    AJsonArray.Add(AJsonObject);
  end;

  ///
  FormMain.SettingHandler.UpdateOrCreateArray('trusted_server', AJsonArray);
end;

procedure TFormServerHive.LoadFromSettings();
var AJsonObject : ISuperObject;
    AJsonArray  : ISuperArray;
    ANode       : PVirtualNode;
    pData       : PTreeData;
    I           : Integer;
begin
  inherited;
  ///

  VST.Clear();

  AJsonArray := FormMain.SettingHandler.GetArray('trusted_server');
  if not Assigned(AJsonArray) then
    Exit();

  VST.BeginUpdate();
  try
    for I := 0 to AJsonArray.Length -1 do begin
      AJsonObject := AJsonArray.O[I];
      ///

      if not AJsonObject.Contains('name') or
         not AJsonObject.Contains('display_name') or
         not AJsonObject.Contains('fingerprint') or
         not AJsonObject.Contains('trusted_since') then
        continue;


      if GetServerNode(AJsonObject.S['fingerprint']) <> nil then
        continue;
      ///

      ANode := VST.AddChild(nil);
      pData := ANode.GetData;

      try
        pData^.Name         := AJsonObject.S['name'];
        pData^.DisplayName  := AJsonObject.S['display_name'];
        pData^.Fingerprint  := AJsonObject.S['fingerprint'];
        pData^.TrustedSince := AJsonObject.S['trusted_since'];
        pData^.Probing      := False;
      except
        VST.DeleteNode(ANode);
      end;
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormServerHive.OnReceiveSubSevenMessage(Sender : TObject; AMessage : Cardinal);
begin
  if AMessage = SUB7_CONNECTED then begin
    FConnected := True;
  end else if AMessage = SUB7_DISCONNECTED then begin
    FConnected := False;
  end;

  ///
  self.RefreshIconsStatus();
end;

procedure TFormServerHive.RefreshIconsStatus();
begin
  ButtonDelete.Enabled    := (VST.FocusedNode <> nil) and not FConnected;
  ButtonDeleteAll.Enabled := (VST.RootNodeCount > 0) and not FConnected;
  ButtonRename.Enabled    := VST.FocusedNode <> nil;

  delectselectedservertrust1.Enabled       := ButtonDelete.Enabled;
  deletealltrustedservers1.Enabled         := ButtonDeleteAll.Enabled;
  renameselectedserverdisplayname1.Enabled := ButtonRename.Enabled;
end;

procedure TFormServerHive.renameselectedserverdisplayname1Click(
  Sender: TObject);
begin
  ButtonRenameClick(ButtonRename);
end;

procedure TFormServerHive.UpdateProbingInformation(const AFingerprint, AMachineName : String);
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  ANode := GetServerNode(AFingerprint);
  if not Assigned(ANode) then
    Exit();
  ///

  pData := ANode.GetData;

  if pData^.Probing = false then
    Exit();

  VST.BeginUpdate();
  try
    pData^.Probing     := False;
    pData^.Name        := AMachineName;
    pData^.DisplayName := AMachineName;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormServerHive.ButtonDeleteAllClick(Sender: TObject);
begin
  VST.Clear();
end;

procedure TFormServerHive.ButtonDeleteClick(Sender: TObject);
begin
  VST.DeleteSelectedNodes();
end;

procedure TFormServerHive.ButtonRenameClick(Sender: TObject);
var AQuery : String;
    ANode  : PVirtualNode;
    pData  : PTreeData;
begin
  ANode := VST.FocusedNode;

  if ANode = nil then
    Exit();

  AQuery := FormMain.MessageBox.InputQuery(FormMain, 'please enter new server display name:', 'rename');

  if AQuery = '' then
    Exit();

  pData := ANode.GetData;

  VST.BeginUpdate();
  try
    pData^.DisplayName := AQuery;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormServerHive.delectselectedservertrust1Click(Sender: TObject);
begin
  ButtonDeleteClick(ButtonDelete);
end;

procedure TFormServerHive.deletealltrustedservers1Click(Sender: TObject);
begin
  ButtonDeleteAllClick(ButtonDeleteAll);
end;

procedure TFormServerHive.FormCreate(Sender: TObject);
begin
  FMessageListener := TS7MessageListener.Create();

  FMessageListener.OnMessage := OnReceiveSubSevenMessage;

  FMessageListener.Add(SUB7_CONNECTED);
  FMessageListener.Add(SUB7_DISCONNECTED);

  FConnected := False;
end;

procedure TFormServerHive.FormDestroy(Sender: TObject);
begin
  { It is very important to clear the VST before destruction of form to avoid
  Access Violation. For some reason nodes are freed after form destruction whcih
  can cause issues when we are hooking some nodes even like "OnFreeNode" }
  VST.Clear();

  if Assigned(FMessageListener) then
    FreeAndNil(FMessageListener);
end;

procedure TFormServerHive.FormShow(Sender: TObject);
begin
  RefreshIconsStatus();
end;

function TFormServerHive.GetServerNode(const AFingerprint : String) : PVirtualNode;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  result := nil;

  for ANode in VST.Nodes do begin
    pData := ANode.GetData;

    if String.Compare(pData^.Fingerprint, AFingerprint, True) = 0 then begin
      result := ANode;

      break;
    end;
  end;
end;

procedure TFormServerHive.TrustNewServer(const AFingerprint : String);
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  if GetServerNode(AFingerprint) <> nil then
    Exit();
  ///

  VST.BeginUpdate();
  try
    ANode := VST.AddChild(nil);
    pData := ANode.GetData;

    pData^.Name         := 'Probing...';
    pData^.DisplayName  := 'Probing...';
    pData^.Fingerprint  := AFingerprint;
    pData^.TrustedSince := DateTimeToStr(Now);
    pData^.Probing      := True;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormServerHive.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFormServerHive.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
  var pData1, pData2 : PTreeData;
begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;
  ///

  case Column of
    0 : result := CompareText(pData1^.Name, pData2^.Name);
    1 : result := CompareText(pData1^.DisplayName, pData2^.DisplayName);
    2 : result := CompareText(pData1^.Fingerprint, pData2^.Fingerprint);
    3 : result := CompareText(pData1^.TrustedSince, pData2^.TrustedSince);
  end;
end;

procedure TFormServerHive.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();

  RefreshIconsStatus();
end;

procedure TFormServerHive.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  RefreshIconsStatus();
end;

procedure TFormServerHive.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormServerHive.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
  var pData : PTreeData;
begin
  pData := Node.GetData;
  ///

  case Column of
    0 : CellText := pData^.Name;
    1 : CellText := pData^.DisplayName;
    2 : CellText := pData^.Fingerprint;
    3 : CellText := pData^.TrustedSince;
  end;
end;

procedure TFormServerHive.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  RefreshIconsStatus();
end;

end.
