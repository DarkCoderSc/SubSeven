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

unit uFormMain;

interface

uses  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
      Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Sub7.Viewer.VCL.SubSevenForm,
      S7StatusBar, Vcl.Imaging.pngimage, Vcl.ExtCtrls, S7Edit, Sub7.Viewer.VCL.Button, S7Panel,
      Sub7.Viewer.VCL.CaptionBar, VirtualTrees, S7TreeView, System.ImageList, Vcl.ImgList,
      S7ImageButton, XSuperObject, Sub7.Core.Protocol, Winapi.ShellAPI, S7MessageBox,
      S7Hint, Generics.Collections, ___S7DockFrame, Sub7.Viewer.Types,
      Sub7.Core.Types, S7ComboBox, ___S7ControlWindow, Sub7.Thread.Net.Client.Session.Cmd,
      Sub7.Net.Client.Context, S7SettingHandler, ___S7BaseForm;

type
  TSessionStatus = (
                      sessDisconnected,
                      sessInit,
                      sessConnected
  );

  TFormMain = class(TS7BaseForm)
    CaptionBar: TS7CaptionBar;
    PanelClient: TPanel;
    PanelHeader: TPanel;
    PanelHeaderControls: TPanel;
    PanelHeaderControlNet: TS7Panel;
    LabelRemotePort: TLabel;
    LabelRemoteAddress: TLabel;
    ButtonAddressBook: TS7Button;
    ButtonConnect: TS7Button;
    EditRemotePort: TS7Edit;
    PanelHeaderControlShortcuts: TS7Panel;
    PanelHeaderLogo: TS7Panel;
    ImgLogo: TImage;
    StatusFooter: TS7StatusBar;
    PanelMenu: TS7Panel;
    PanelDock: TS7Panel;
    VST: TS7VirtualStringTree;
    ImageSubSeven: TImageList;
    ButtonFileManager: TS7ImageButton;
    btnSockets: TS7ImageButton;
    ImageSystem: TImageList;
    MessageBox: TS7MessageBox;
    btnExceptions: TS7ImageButton;
    SubSevenHints: TS7Hint;
    btnQueue: TS7ImageButton;
    ImageSystemBig: TImageList;
    SubSevenForms: TS7Form;
    DockBackground: TImage;
    ComboRemoteAddress: TS7ComboBox;
    Shape1: TShape;
    ButtonProcess: TS7ImageButton;
    ButtonTerminal: TS7ImageButton;
    ButtonRegistry: TS7ImageButton;
    ButtonRemoteDesktop: TS7ImageButton;
    ButtonRun: TS7ImageButton;
    ButtonOpen: TS7ImageButton;
    ButtonPing: TS7ImageButton;
    ButtonSessions: TS7ImageButton;
    ButtonKnownServers: TS7ImageButton;
    SettingHandler: TS7SettingHandler;
    ButtonClientCert: TS7ImageButton;
    procedure FormCreate(Sender: TObject);
    procedure VSTItemClick(Sender: TObject; AIndex: Integer; AItemName: string);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonConnectValueChanged(Sender: TObject; ANewValue: Integer);
    procedure ButtonFileManagerClick(Sender: TObject);
    procedure btnSocketsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ImgLogoClick(Sender: TObject);
    procedure EditRemotePortChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnExceptionsClick(Sender: TObject);
    procedure btnQueueClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboRemoteAddressChange(Sender: TObject);
    procedure ButtonProcessClick(Sender: TObject);
    procedure ButtonTerminalClick(Sender: TObject);
    procedure ButtonRegistryClick(Sender: TObject);
    procedure ButtonRemoteDesktopClick(Sender: TObject);
    procedure ComboRemoteAddressKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditRemotePortKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ButtonRunClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonPingClick(Sender: TObject);
    procedure ButtonSessionsClick(Sender: TObject);
    procedure ButtonAddressBookClick(Sender: TObject);
    procedure ButtonKnownServersClick(Sender: TObject);
    procedure ButtonClientCertClick(Sender: TObject);
  private
    FFileInfo     : TSHFileInfo;
    FSession      : TSub7ClientSessionCmd;
    FActiveMenu   : TNavMenu;
    FFirstShow    : Boolean;
    FContext      : TSub7ClientContext;

    FDockedFrames : TObjectDictionary<TNavMenu, TS7FrameDock>;

    {@M}
    procedure InitializeTreeMenu();

    procedure OpenFileManager();
    procedure OpenRegistryEditor();
    procedure OpenRemoteDesktop();

    procedure CloseSession();

    procedure CheckForms();

    procedure CreateFrames();
    procedure HideDockedFrames();

    procedure NotConnectedError();

    procedure DoPing();

    procedure ExceptionHandler(Sender : TObject; E : Exception);

    procedure SetVersion(const AVersion : String);
  protected
    {@M}
    procedure Loaded(); override;
    procedure SaveToSettings(); override;
    procedure LoadFromSettings(); override;
  public
    {@M}
    procedure SetStatus(AStatus : String);

    procedure OpenSession(const ARemoteAddress : String; const ARemotePort : Word; ACertFile : AnsiString = ''; APassword : String = '');

    procedure ReactControlWindows(const ACommand : TS7Command; const AData : ISuperObject);

    procedure BrowseMenu(const AMenu : TNavMenu);

    function GetDockedFrame(const AType : TNavMenu) : TS7FrameDock;
    function GetControlForm(const AGUID : String) : TS7ControlWindow;

    function CheckConnected(const ASilent : Boolean = False) : Boolean;

    procedure RegisterServerAddress(const AServerAddress : String);

    procedure ShowSuccessMessage(const AMessage : String);

    procedure RegisterServerInformation(const AMachineName, AFingerprint : String);

    procedure UpdateSessionStatus(const AStatus : TSessionStatus);

    procedure OnRequestPassword(Sender : TObject; var APassword : String; var ACanceled : Boolean);
    procedure OnVerifyPeer(const AFingerprint : String; var ASuccess : Boolean);

    {@G}
    property Session    : TSub7ClientSessionCmd read FSession;
    property ActiveMenu : TNavMenu              read FActiveMenu;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}
{$R sub7.res}

uses uFormPassword, Sub7.Viewer.Messages, Sub7.Core.Utils,
     uFormAbout, Sub7.Core.FileSystem.Utils, uFormSockets, uFormExceptions, uFormQueue,
     uFormFileManager, Sub7.Core.Windows.Process.Enum, uFrameProcess, uFrameSystemInformationHook, S7Types,
     Sub7.Core.Windows.Sessions.Enum, uFrameOpen, uFormRun, Sub7.Core.Input.Validators,
     uFormRemoteShell, uFrameShell, Sub7.Core.Messages.Listener, Sub7.Core.Diagnostic,
     uFormServerHive, Sub7.OpenSSL.TLS.Utils, Sub7.OpenSSL.Cert.Utils,
     uFormClientCertificate, Sub7.Core.Application.Env, Sub7.Core.Exceptions,
     uFormAddressBook, Sub7.Core.Windows.PE.Version, Sub7.Core.Windows.Process,
     Sub7.Core.FileSystem.Enum, Sub7.Viewer.Singletons, System.IOUtils;


procedure TFormMain.RegisterServerAddress(const AServerAddress : String);
var AItem  : String;
    i      : Integer;
    AFound : Boolean;
begin
  AFound := False;
  ///

  if not IsValidNetworkAddress(AServerAddress) then
    Exit();

  for i := 0 to self.ComboRemoteAddress.Items.Count -1 do begin
    AItem := self.ComboRemoteAddress.Items.Strings[i];

    if String.Compare(AItem, AServerAddress, True) = 0 then begin
      AFound := True;

      break;
    end;
  end;

  if not AFound then
    self.ComboRemoteAddress.Items.add(AServerAddress);
end;

procedure TFormMain.SaveToSettings();
var AJsonArray     : ISuperArray;
    AJsonObject    : ISuperObject;
    i              : Integer;
    ARemoteAddress : String;
begin
  inherited;
  ///

  AJsonArray := TSuperArray.Create();

  for i := 0 to self.ComboRemoteAddress.Items.Count -1 do begin
    ARemoteAddress := self.ComboRemoteAddress.Items.Strings[i];

    AJsonObject := TSuperObject.Create();

    AJsonArray.Add(ARemoteAddress);
  end;

  ///
  SettingHandler.UpdateOrCreateArray('server_addresses', AJsonArray);
end;

procedure TFormMain.LoadFromSettings();
var AJsonArray  : ISuperArray;
    I           : Integer;
begin
  inherited;
  ///

  AJsonArray := SettingHandler.GetArray('server_addresses');
  if not Assigned(AJsonArray) then
    Exit();

  for I := 0 to AJsonArray.Length -1 do begin
    try
      self.RegisterServerAddress(AJsonArray.S[i]);
    except

    end;
  end;
end;

procedure TFormMain.OpenRemoteDesktop();
begin
  MessageBox.MessageBox(self, 'This function is disabled for alpha version of SubSeven 1.0. Before implementing this function ' +
                              'to public, I want to be sure the rest of the program and protocol is stable enough.', 'Remote Desktop', MB_USERICON);
end;

procedure TFormMain.SetVersion(const AVersion : String);
begin
  self.StatusFooter.Panels.Items[1].Text := AVersion;
end;

procedure TFormMain.RegisterServerInformation(const AMachineName, AFingerprint : String);
begin
  CaptionBar.SubCaption := AMachineName;

  // Update fingerprint information if it is still probing
  FormServerHive.UpdateProbingInformation(AFingerprint, AMachineName);
end;

procedure TFormMain.OnVerifyPeer(const AFingerprint : String; var ASuccess : Boolean);
begin
  // Verify if this fingerprint is already known
  if FormServerHive.GetServerNode(AFingerprint) <> nil then begin
    ASuccess := True;
  end else begin
    // Display messagebox
    ASuccess := self.MessageBox.MessageBox(self, Format(
      'You are attempting to connect to an unknown server.' +
      'Please take care of verifying bellow server certificate ' +
      'fingerprint before connecting:%s%s%s Do you want to connect anyway ' +
      'and hence trust this server?',
        [
          #13#10#13#10,
          PadFingerPrint(AFingerprint, 16),
          #13#10#13#10
        ]
    ), 'unknown server', MB_ICONWARNING + MB_YESNO) = ID_YES;

    if not ASuccess then
      Exit();

    // Add fingerprint to known server list
    FormServerHive.TrustNewServer(AFingerprint);
  end;
end;

procedure TFormMain.OpenSession(const ARemoteAddress : String; const ARemotePort : Word; ACertFile : AnsiString = ''; APassword : String = '');
begin
  {
    Destroy residual session object
  }
  if Assigned(FSession) then
    FreeAndNil(FSession);

  if Assigned(FContext) then
    FreeAndNil(FContext);

  if (ACertFile = '') then begin
    CheckFileExists(APP_ENV_ClientCertificateFile);

    ACertFile := APP_ENV_ClientCertificateFile;
  end;

  { If no password provided, we check our contact list. If a password is found for
    address:port then we use it to avoid fastidious input query }
  if APassword = '' then
    APassword := FormAddressBook.GetContactPassword(ARemoteAddress, ARemotePort);

  FContext := TSub7ClientContext.Create(
                                          ARemoteAddress,
                                          ARemotePort,
                                          ACertFile,
                                          APassword
  );

  FSession := TSub7ClientSessionCmd.Create(FContext);

  ///
  RegisterServerAddress(ARemoteAddress);
end;

procedure TFormMain.OnRequestPassword(Sender : TObject; var APassword : String; var ACanceled : Boolean);
var AFormPassword : TFormPassword;
begin
  AFormPassword := TFormPassword.Create(self);
  try
    AFormPassword.ShowModal();
    ///

    ACanceled := AFormPassword.Canceled;

    if ACanceled then
      Exit();
    ///

    APassword := AFormPassword.GetPassword();
  finally
    AFormPassword.Release();
  end;
end;

procedure TFormMain.ReactControlWindows(const ACommand : TS7Command; const AData : ISuperObject);
var I : Integer;
begin
  for I := 0 to Screen.FormCount -1 do begin
    if not (Screen.Forms[I] is TS7ControlWindow) then
      continue;
    ///

    TS7ControlWindow(Screen.Forms[I]).React(ACommand, AData);
  end;
end;

function TFormMain.GetControlForm(const AGUID : String) : TS7ControlWindow;
var I : Integer;
    F : TS7ControlWindow;
begin
  result := nil;
  ///

  for I := 0 to Screen.FormCount -1 do begin
    if not (Screen.Forms[I] is TS7ControlWindow) then
      continue;
    ///

    F := TS7ControlWindow(Screen.Forms[I]);

    if String.Compare(F.GUID, AGUID, True) = 0 then begin
      result := F;

      break;
    end;
  end;
end;

procedure TFormMain.Loaded();
begin
  inherited;
  ///

  self.UpdateSessionStatus(sessDisconnected);
end;

procedure TFormMain.UpdateSessionStatus(const AStatus : TSessionStatus);
var AStatusText : String;
begin
  {
    Refresh Components States
  }
  ComboRemoteAddress.Enabled := (AStatus = sessDisconnected);
  EditRemotePort.Enabled     := ComboRemoteAddress.Enabled;
  ///

  {
    Refresh Icons Shortcut
  }
  ButtonFileManager.Enabled   := (AStatus = sessConnected);
  ButtonRun.Enabled           := (AStatus = sessConnected);
  ButtonRegistry.Enabled      := (AStatus = sessConnected);
  ButtonRemoteDesktop.Enabled := (AStatus = sessConnected);
  ButtonPing.Enabled          := (AStatus = sessConnected);

  {
    Update Connect Button and Status Footer
  }
  case AStatus of
    sessDisconnected : begin
      ButtonConnect.Value   := 0;
      CaptionBar.SubCaption := '';
      AStatusText           := 'ready.';
      CaptionBar.SubCaption := '';

      if Assigned(SGLT_ExplorerClipboard) then
        SGLT_ExplorerClipboard.Clear();

      ///
      if Assigned(FormQueue) then
        FormQueue.VST.Clear();

      ///
      BroadcastMessage(SUB7_DISCONNECTED);

      FSession := nil;
    end;

    sessInit : begin
      ButtonConnect.Value   := 1;
      AStatusText           := 'connecting...';
      CaptionBar.SubCaption := '';
    end;

    sessConnected : begin
      ButtonConnect.Value := 2;
      AStatusText         := 'connected.';

      ///
      BroadcastMessage(SUB7_CONNECTED);
    end;
  end;

  ///
  StatusFooter.Panels.Items[0].Text := AStatusText;
end;

procedure TFormMain.ShowSuccessMessage(const AMessage : String);
begin
  self.MessageBox.MessageBox(Screen.ActiveForm, AMessage, 'success', MB_USERICON);
end;

function TFormMain.GetDockedFrame(const AType : TNavMenu) : TS7FrameDock;
var F : TS7FrameDock;
begin
  result := nil;
  ///

  if not FDockedFrames.TryGetValue(AType, F) then
    Exit();
  ///

  result := F;
end;

function TFormMain.CheckConnected(const ASilent : Boolean = False) : Boolean;
begin
  result := False;
  ///

  if Assigned(FSession) then
    result := True
  else if not ASilent then
    self.NotConnectedError();
end;

procedure TFormMain.NotConnectedError();
begin
  self.MessageBox.MessageBox(self, 'You must be connected to a subseven server to use this specific function.', 'offline', MB_ICONWARNING);
end;

procedure TFormMain.BrowseMenu(const AMenu : TNavMenu);
var AFrame : TS7FrameDock;
    AForm  : TForm;
begin
  if not self.FDockedFrames.TryGetValue(AMenu, AFrame) then
    AFrame := nil;
  ///

  if Assigned(AFrame) then begin
    FActiveMenu := AMenu;

    if AFrame.DockCaption.Docked then
      self.HideDockedFrames();
  end;

  case AMenu of
    {
      Open Docked Frames / Forms
    }
    nmProcessManager,
    nmMachineActions,
    nmOpen,
    nmTerminal : begin
      if Assigned(AFrame) then
        AFrame.Open();
    end;

    {
      Run
    }
    nmRun : begin
      if not self.CheckConnected() then
        Exit();
      ///

      AForm := TFormRun.Create(self);
      AForm.Show();
    end;

    {
      Registry Editor
    }
    nmRegistryEditor : begin
      OpenRegistryEditor();
    end;

    {
      Transfer Queue
    }
    nmQueue : begin
      FormQueue.Show();
    end;

    {
      File Manager
    }
    nmFileManager : begin
      self.OpenFileManager();
    end;

    {
      Ping target Machine
    }
    nmMachinePing : begin
      self.DoPing();
    end;

    // Remote Desktop
    nmRemoteDesktop : self.OpenRemoteDesktop();
  end;
end;

procedure TFormMain.ExceptionHandler(Sender : TObject; E : Exception);
begin
  FormExceptions.AddItem(E);
end;

procedure TFormMain.CreateFrames();
begin
  FDockedFrames.Add(nmProcessManager, TFrameProcess.Create(PanelDock, nmProcessManager));
  FDockedFrames.Add(nmMachineActions, TFrameSystemInformationHook.Create(PanelDock, nmMachineActions));
  FDockedFrames.Add(nmOpen, TFrameOpen.Create(PanelDock, nmOpen));
  FDockedFrames.Add(nmTerminal, TFrameShell.Create(PanelDock, nmTerminal));
end;

procedure TFormMain.CheckForms();
begin
  if IsValidHost(ComboRemoteAddress.Text) then
    ComboRemoteAddress.Status := csNormal
  else
    ComboRemoteAddress.Status := csError;

  if IsValidPort(EditRemotePort.Text) then
    EditRemotePort.Status := csNormal
  else
    EditRemotePort.Status := csError;
  ///

  ButtonConnect.Enabled := (ComboRemoteAddress.Status <> csError) and
                           (EditRemotePort.Status     <> csError);
end;

procedure TFormMain.CloseSession();
begin
  if Assigned(FSession) then
    FreeAndNil(FSession);
end;

procedure TFormMain.ComboRemoteAddressChange(Sender: TObject);
begin
  self.CheckForms();
end;

procedure TFormMain.ComboRemoteAddressKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) and ButtonConnect.Enabled then
    ButtonConnectClick(ButtonConnect);
end;

procedure TFormMain.EditRemotePortChange(Sender: TObject);
var AValue : Integer;
    AFixed : Boolean;
begin
  AFixed := False;

  if not TryStrToInt(TS7Edit(Sender).Text, AValue) then
    Exit();

  if AValue < 0 then begin
    AValue := 0;

    AFixed := True;
  end;

  if AValue > High(word) then begin
    AValue := High(word);

    AFixed := True;
  end;

  if AFixed then begin
    TS7Edit(Sender).Text := IntToStr(AValue);
    TS7Edit(Sender).SelStart := Length(TS7Edit(Sender).Text);
    TS7Edit(Sender).SelLength := TS7Edit(Sender).SelStart;
  end;
end;

procedure TFormMain.EditRemotePortKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 13) and ButtonConnect.Enabled then
    ButtonConnectClick(ButtonConnect);
end;

procedure TFormMain.ButtonProcessClick(Sender: TObject);
begin
  self.BrowseMenu(nmProcessManager);
end;

procedure TFormMain.ButtonTerminalClick(Sender: TObject);
begin
  self.BrowseMenu(nmTerminal);
end;

procedure TFormMain.ButtonRegistryClick(Sender: TObject);
begin
  ///
end;

procedure TFormMain.ButtonRemoteDesktopClick(Sender: TObject);
begin
  self.OpenRemoteDesktop();
end;

procedure TFormMain.ButtonRunClick(Sender: TObject);
begin
  self.BrowseMenu(nmRun);
end;

procedure TFormMain.ButtonSessionsClick(Sender: TObject);
begin
  self.BrowseMenu(nmMachineActions);
end;

procedure TFormMain.ButtonPingClick(Sender: TObject);
begin
  self.DoPing();
end;

procedure TFormMain.OpenFileManager();
var F : TFormFileManager;
begin
  if not self.CheckConnected() then
    Exit();
  ///

  F := TFormFileManager.Create(nil);
  F.Show();
end;

procedure TFormMain.OpenRegistryEditor();
begin
  ///
end;

procedure TFormMain.DoPing();
var AData : ISuperObject;
begin
  if Assigned(FSession) then begin
    AData := TSuperObject.Create();

    AData.I['tick'] := GetTickCount();

    FSession.SendCommand(mhcPing, AData);
  end else
    self.NotConnectedError();
end;

procedure TFormMain.ButtonFileManagerClick(Sender: TObject);
begin
  self.OpenFileManager();
end;

procedure TFormMain.ButtonKnownServersClick(Sender: TObject);
begin
  FormServerHive.Show();
end;

procedure TFormMain.ButtonOpenClick(Sender: TObject);
begin
  self.BrowseMenu(nmOpen);
end;

procedure TFormMain.btnQueueClick(Sender: TObject);
begin
  FormQueue.Show();
end;

procedure TFormMain.btnSocketsClick(Sender: TObject);
begin
  FormSockets.Show();
end;

procedure TFormMain.ButtonAddressBookClick(Sender: TObject);
begin
  FormAddressBook.Show();
end;

procedure TFormMain.SetStatus(AStatus : String);
begin
  StatusFooter.Panels.Items[2].Text := AStatus;
  StatusFooter.Panels.Items[2].Hint := AStatus;
end;

procedure TFormMain.ButtonConnectValueChanged(Sender: TObject; ANewValue: Integer);
var ACaption : String;
    AHint    : String;
begin
  case ANewValue of
    0 : begin
      ACaption := 'Connect';
      AHint    := 'attempt to connect to a valid sub7 server';
    end;

    1 : begin
      ACaption := 'Cancel';
      AHint    := 'cancel connection attempt to server';
    end;

    2 : begin
      ACaption := 'Disconnect';
      AHint    := 'disconnect from server';
    end;
  end;

  ///
  TS7Button(Sender).Caption := ACaption;
  TS7Button(Sender).Hint    := AHint;
end;

procedure TFormMain.btnExceptionsClick(Sender: TObject);
begin
  FormExceptions.Show();
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
var F : TForm;
    I : Integer;
begin
  ///
  self.CloseSession();

  for I := 0 to Screen.FormCount -1 do begin
    F := Screen.Forms[i];

    if F.Visible and (F <> self) then
      F.Close();
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var AVersion : String;
begin
  InitializeTreeMenu();

  FSession    := nil;

  FActiveMenu := nmMain;
  FFirstShow  := True;

  InitializeSystemIcons(self.ImageSystem, FFileInfo);
  InitializeSystemIcons(self.ImageSystemBig, FFileInfo, True);

  ChangeWindowMessageFilter(WM_DROPFILES, MSGFLT_ADD);
  ChangeWindowMessageFilter(WM_COPYGLOBALDATA, MSGFLT_ADD);

  FDockedFrames := TObjectDictionary<TNavMenu, TS7FrameDock>.Create([doOwnsValues]);
  self.CreateFrames();

  self.SetStatus('Welcome to SubSeven Legacy.');

  AVersion := TVersion.GetFileVersion(GetModuleName(0));

  self.SetVersion(AVersion);

  CaptionBar.Caption := Format('%s %s %s', [
    CaptionBar.Caption,
    AVersion,
    APP_ENV_ReleaseName
  ]);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FDockedFrames) then
    FreeAndNil(FDockedFrames);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if FFirstShow then begin
    { We only redirect Exception handler when our main form instance was
     successfully created }
    Application.OnException := ExceptionHandler;

    self.BrowseMenu(nmMain);

    FFirstShow := False;
  end;

  ///
  self.CheckForms();
end;

procedure TFormMain.ImgLogoClick(Sender: TObject);
begin
  FormAbout.Show();
end;

procedure TFormMain.InitializeTreeMenu();
var ASystemNode  : PVirtualNode;
    AControlNode : PVirtualNode;
    ACommands    : PVirtualNode;
    AFiles       : PVirtualNode;
    AMachine     : PVirtualNode;

    function AddNavItem(AName : String; ANavMenu : TNavMenu; AParentNode : PVirtualNode = nil) : PVirtualNode;
    begin
      result := VST.AddItem(AName, Integer(ANavMenu), AParentNode);
    end;

begin
  ASystemNode := AddNavItem('system', nmCategory);

  //AddNavItem('registry editor', nmRegistryEditor, ASystemNode);
  AddNavItem('process manager', nmProcessManager, ASystemNode);
  //AddNavItem('service manager', nmServiceManager, ASystemNode);

  AControlNode := AddNavItem('control', nmCategory);
  AddNavItem('remote desktop', nmRemoteDesktop, AControlNode);

  AFiles := AddNavItem('files', nmCategory);
  AddNavItem('file manager', nmFileManager, AFiles);
  //AddNavItem('search files', nmSearchForFiles, AFiles);
  AddNavItem('queue', nmQueue, AFiles);

  ACommands := AddNavItem('commands', nmCategory);
  AddNavItem('run', nmRun, ACommands);
  AddNavItem('open', nmOpen, ACommands);
  AddNavItem('remote shell', nmTerminal, ACommands);

  AMachine := AddNavItem('machine', nmCategory);
  AddNavItem('sessions', nmMachineActions, AMachine);
  AddNavItem('ping', nmMachinePing, AMachine);
end;

procedure TFormMain.HideDockedFrames();
var F : TS7FrameDock;
    I : TNavMenu;
begin
  for I in FDockedFrames.Keys do begin
    if not FDockedFrames.TryGetValue(I, F) then
      continue;
    ///

    if F.DockCaption.Docked then
      F.Visible := False;
  end;
end;

procedure TFormMain.VSTItemClick(Sender: TObject; AIndex: Integer;
  AItemName: string);
begin
  self.BrowseMenu(TNavMenu(AIndex));
end;

procedure TFormMain.ButtonClientCertClick(Sender: TObject);
begin
  FormClientCertificate.Show();
end;

procedure TFormMain.ButtonConnectClick(Sender: TObject);
var APort : Integer;
begin
  if TS7Button(Sender).Value = 0 then begin
    if not TryStrToInt(EditRemotePort.text, APort) then
      Exit();
    ///

    try
      self.OpenSession(ComboRemoteAddress.Text, APort);
    except
      on E : ES7FileDoesNotExistsException do begin
        if self.MessageBox.MessageBox(
              self,
              'Client certificate is missing. To connect to a remote server you must generate or import ' +
              'a valid OpenSSL Certificate. Do you wan''t to open "certificate manager" now?',
              'missing certificate',
              MB_ICONWARNING + MB_YESNO
            ) = ID_YES then
              FormClientCertificate.Show();
      end;
    end;
  end else
    self.CloseSession();
end;

end.
