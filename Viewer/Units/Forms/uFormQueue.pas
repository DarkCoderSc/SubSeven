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

unit uFormQueue;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Sub7.Viewer.VCL.CheckBox, S7ImageButton,
  S7Panel, Sub7.Viewer.VCL.CaptionBar, Sub7.Viewer.VCL.SubSevenForm, Vcl.StdCtrls, Vcl.Samples.Gauges, S7Edit,
  Sub7.Core.Protocol, Vcl.ExtCtrls, Sub7.Core.Exceptions, S7OptionDialog, Sub7.Thread.Net.Client.Transfer,
  System.Win.TaskbarCore, Vcl.Taskbar, Vcl.Menus, S7PopupMenu, uFormTransfer,
  ___S7BaseForm;

type
  TTreeData = record
    Origin      : String;
    Destination : String;

    WorkCount   : Int64;
    Progress    : Integer;
    Overwrite   : Boolean;
    Size        : Int64;
    bps         : Int64;

    LastError   : String;
    Status      : TS7TransferStatus;
    Client      : TThreadTransfer;

    Lock        : TFileStream;
    Form        : TFormTransfer;
    Direction   : TS7TransferDirection;
  end;
  PTreeData = ^TTreeData;

  TFormQueue = class(TS7BaseForm)
    CaptionBar: TS7CaptionBar;
    PanelHeader: TS7Panel;
    ButtonStartStopTransfer: TS7ImageButton;
    VST: TVirtualStringTree;
    SubSevenForms: TS7Form;
    ButtonDelete: TS7ImageButton;
    PanelClient: TS7Panel;
    PanelFooter: TS7Panel;
    LabelGaugeSelected: TLabel;
    GaugeSelected: TGauge;
    LabelGaugeTotal: TLabel;
    GaugeTotal: TGauge;
    Timer: TTimer;
    ButtonRetryTransfer: TS7ImageButton;
    ButtonOpenTransferWindow: TS7ImageButton;
    ButtonOptions: TS7ImageButton;
    Options: TS7OptionDialog;
    PopupMenuAction: TS7PopupMenu;
    starttransfer1: TMenuItem;
    deletetransfer1: TMenuItem;
    retry1: TMenuItem;
    N1: TMenuItem;
    opentransferwindow1: TMenuItem;
    WindowsTaskbar: TTaskbar;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure ButtonStartStopTransferValueChanged(Sender: TObject;
      ANewValue: Integer);
    procedure ButtonStartStopTransferClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonRetryTransferClick(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure VSTDblClick(Sender: TObject);
    procedure ButtonOpenTransferWindowClick(Sender: TObject);
    procedure starttransfer1Click(Sender: TObject);
    procedure deletetransfer1Click(Sender: TObject);
    procedure retry1Click(Sender: TObject);
    procedure opentransferwindow1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected

  private
    {@M}
    procedure AddTransfer(const AOrigin, ADestination : String; const ADirection : TS7TransferDirection);
    procedure OpenSelectedTransferWindow();

    procedure ResetQueueData(const pData : PTreeData);
  public
    {@M}
    procedure UploadFile(const AFilePath, ADestinationPath : String);
    procedure DownloadFile(const AOrigin, ADestinationPath : String);
    procedure OnTransferException(const AException : Exception; const ANode : PVirtualNode);
    procedure RefreshIconsStatus();
    procedure ReQueue(const ANode : PVirtualNode);
    procedure SetProgress(const ANode : PVirtualNode; const AWorkCount, ABytesPerSecond : Int64);
    procedure SetFileSize(const ANode : PVirtualNode; const ASize : Int64);
    procedure SetStatus(const ANode : PVirtualNode; const AStatus : TS7TransferStatus); overload;
    procedure SetStatus(const pData : PTreeData; const AStatus : TS7TransferStatus); overload;

    procedure UpdateDestination(const ANode : PVirtualNode; const ADestination : String);

    procedure DeleteTransfer(const ANode : PVirtualNode);
    procedure StartTransfer(const ANode : PVirtualNode);
    procedure StopTransfer(const ANode : PVirtualNode);
    procedure RetryTransfer(const ANode : PVirtualNode);
    procedure TransferEnded(const ANode : PVirtualNode);
  end;

var
  FormQueue: TFormQueue;

implementation

{$R *.dfm}

uses uFormMain, Sub7.Core.FileSystem.Utils, System.Math, uFormExceptions, Sub7.Core.Types;

procedure TFormQueue.TransferEnded(const ANode : PVirtualNode);
begin
  VST.BeginUpdate();
  try
    VST.DeleteNode(ANode);
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormQueue.UpdateDestination(const ANode : PVirtualNode; const ADestination : String);
var pData : PTreeData;
begin
  if not Assigned(ANode) then
    Exit();

  pData := ANode.GetData();

  VST.BeginUpdate();
  try
    pData^.Destination := ADestination;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormQueue.SetFileSize(const ANode : PVirtualNode; const ASize : Int64);
var pData : PTreeData;
begin
  if not Assigned(ANode) then
    Exit();

  pData := ANode.GetData();

  VST.BeginUpdate();
  try
    pData^.Size := ASize;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormQueue.ResetQueueData(const pData : PTreeData);
begin
  if not Assigned(pData) then
    Exit();
  ///

  VST.BeginUpdate();
  try
    pData^.WorkCount := 0;
    pData^.Progress  := 0;
    pData^.Overwrite := self.Options.Get('overwrite', True);
    pData^.Size      := 0;
    pData^.bps       := 0;
    pData^.LastError := '';

    if Assigned(pData^.Lock) then
      pData^.Size := pData^.Lock.Size;
  finally
    VST.EndUpdate();
  end;

  pData^.Form.UpdateAll();
end;

procedure TFormQueue.retry1Click(Sender: TObject);
begin
  ButtonRetryTransferClick(ButtonRetryTransfer);
end;

procedure TFormQueue.StartTransfer(const ANode : PVirtualNode);
var pData : PTreeData;
begin
  if not Assigned(ANode) then
    Exit();
  ///

  pData := ANode.GetData;

  if (pData^.Status <> tsQueue) then
    Exit();

  self.ResetQueueData(pData);

  try
    if pData^.Direction = tdUpload then
      FormMain.Session.CreateSessionWorker(wkUploadHandler, ANode)
    else
      FormMain.Session.CreateSessionWorker(wkDownloadHandler, ANode);
  except
    on E : Exception do
      self.OnTransferException(E, ANode);
  end;

  ///
  self.RefreshIconsStatus();
end;

procedure TFormQueue.starttransfer1Click(Sender: TObject);
begin
  ButtonStartStopTransferClick(ButtonStartStopTransfer);
end;

procedure TFormQueue.StopTransfer(const ANode : PVirtualNode);
var pData : PTreeData;
begin
  if not Assigned(ANode) then
    Exit();
  ///

  pData := ANode.GetData;

  if pData^.Status <> tsProgress then
    Exit();

  pData^.Client.Close(True);

  self.ResetQueueData(pData);

  ///
  self.RefreshIconsStatus();
end;

procedure TFormQueue.RetryTransfer(const ANode : PVirtualNode);
var pData : PTreeData;
begin
  if not Assigned(ANode) then
    Exit();
  ///

  pData := ANode.GetData;

  if pData^.Status <> tsError then
    Exit();

  self.ResetQueueData(pData);

  self.SetStatus(pData, tsQueue);

  self.StartTransfer(ANode);
end;

procedure TFormQueue.DeleteTransfer(const ANode : PVirtualNode);
var pData : PTreeData;
begin
  if not Assigned(ANode) then
    Exit();

  pData := ANode.GetData;

  if pData^.Status = tsProgress then
    Exit();

  VST.BeginUpdate();
  try
    VST.DeleteNode(ANode);
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormQueue.deletetransfer1Click(Sender: TObject);
begin
  ButtonDeleteClick(ButtonDelete);
end;

procedure TFormQueue.SetStatus(const pData : PTreeData; const AStatus : TS7TransferStatus);
begin
  VST.BeginUpdate();
  try
    pData^.Status := AStatus;
  finally
    VST.EndUpdate();
  end;

  pData^.Form.UpdateStatus();

  ///
  self.RefreshIconsStatus();
end;

procedure TFormQueue.SetStatus(const ANode : PVirtualNode; const AStatus : TS7TransferStatus);
var pData : PTreeData;
begin
  pData := ANode.GetData;

  self.SetStatus(pData, AStatus);
end;

procedure TFormQueue.OpenSelectedTransferWindow();
var pData : PTreeData;
begin
  if not Assigned(VST.FocusedNode) then
    Exit();

  pData := VST.FocusedNode.GetData;

  ///
  pData^.Form.Show();
end;

procedure TFormQueue.opentransferwindow1Click(Sender: TObject);
begin
  ButtonOpenTransferWindowClick(ButtonOpenTransferWindow);
end;

procedure TFormQueue.SetProgress(const ANode : PVirtualNode; const AWorkCount, ABytesPerSecond : Int64);
var pData     : PTreeData;
    AProgress : Integer;
begin
  if not Assigned(ANode) then
    Exit();

  pData := ANode.GetData();

  AProgress := ceil((AWorkCount * 100) / pData.Size);

  VST.BeginUpdate();
  try
    pData^.WorkCount := AWorkCount;
    pData^.Progress  := AProgress;
    pData^.bps       := ABytesPerSecond;
  finally
    VST.EndUpdate();
  end;

  ///
  pData^.Form.UpdateProgress();
end;

procedure TFormQueue.ReQueue(const ANode : PVirtualNode);
var pData : PTreeData;
begin
  if not Assigned(ANode) then
    Exit();
  ///

  pData := ANode.GetData();

  pData^.Progress  := 0;
  pData^.LastError := '';
  pData^.Client    := nil;

  self.SetStatus(pData, tsQueue);
end;

procedure TFormQueue.OnTransferException(const AException : Exception; const ANode : PVirtualNode);
var pData : PTreeData;
begin
  if not Assigned(ANode) then
    Exit();

  pData := ANode.GetData;

  if not Assigned(pData) then
    Exit();

  pData^.LastError := AException.Message;

  self.SetStatus(pData, tsError);

  { register this exception to exception handler form }
  FormExceptions.AddItem(AException);
end;

procedure TFormQueue.AddTransfer(const AOrigin, ADestination : String; const ADirection : TS7TransferDirection);
var ANode  : PVirtualNode;
    pData  : PTreeData;
    AFound : Boolean;
begin
  if not Assigned(FormMain.Session) then
    Exit();
  ///

  { Anti Transfer Doublons }
  AFound := False;
  for ANode in VST.Nodes do begin
    pData := ANode.GetData;

    if (pData^.Origin = AOrigin)           and
       (pData^.Destination = ADestination) and
       (pData^.Direction = ADirection)     then begin
        AFound := True;
        break;
    end;
  end;

  if AFound then begin
    if (pData^.Status = tsError) then
      self.RetryTransfer(ANode);

    ///
    Exit();
  end;

  { Append Transfer }
  VST.BeginUpdate();
  try
    ANode := VST.AddChild(nil);
    pData := ANode.GetData;
    ///

    if (ADirection = tdUpload) then
      pData^.Lock := TFileStream.Create(AOrigin, fmShareDenyWrite)
    else
      pData^.Lock := nil;

    pData^.Origin      := AOrigin;
    pData^.Destination := ADestination;
    pData^.Direction   := ADirection;
    pData^.Form        := TFormTransfer.Create(nil, ANode);

    self.ResetQueueData(pData);

    self.SetStatus(pData, tsQueue);

    if self.Options.Get('autotransfer', True) then
      self.StartTransfer(ANode)
    else
      pData^.Client := nil;
  finally
    VST.EndUpdate();
  end;

  if self.Options.Get('opentranswindow', True) then
    pData^.Form.Show();
end;

procedure TFormQueue.RefreshIconsStatus();
var ASelected  : Boolean;
    ANode      : PVirtualNode;
    pData      : PTreeData;
begin
  ANode := VST.FocusedNode;
  ///

  self.ButtonDelete.Enabled             := False;
  self.ButtonRetryTransfer.Enabled      := False;
  self.ButtonStartStopTransfer.Enabled  := False;
  self.ButtonOpenTransferWindow.Enabled := Assigned(ANode);

  if Assigned(ANode) then begin
    pData := ANode.GetData;
    ///

    self.ButtonStartStopTransfer.Enabled  := (pData^.Status = tsProgress) or (pData^.Status = tsQueue);

    if self.ButtonStartStopTransfer.Enabled then begin
      if pData^.Status = tsProgress then
        self.ButtonStartStopTransfer.Value := 1
      else
        self.ButtonStartStopTransfer.Value := 0;
    end;

    ///
    self.ButtonRetryTransfer.Enabled  := (pData^.Status = tsError);
    self.ButtonDelete.Enabled := Assigned(ANode) and (pData^.Status <> tsProgress);
  end;


  ///
  self.starttransfer1.Enabled := self.ButtonStartStopTransfer.Enabled;
  if self.ButtonStartStopTransfer.Value = 1 then
    self.starttransfer1.Caption := 'Stop'
  else
    self.starttransfer1.Caption := 'Start';

  self.deletetransfer1.Enabled     := self.ButtonDelete.Enabled;
  self.retry1.Enabled              := self.ButtonRetryTransfer.Enabled;
  self.opentransferwindow1.Enabled := self.ButtonOpenTransferWindow.Enabled;
end;

procedure TFormQueue.ButtonDeleteClick(Sender: TObject);
begin
  self.DeleteTransfer(VST.FocusedNode);
end;

procedure TFormQueue.ButtonOpenTransferWindowClick(Sender: TObject);
begin
  self.OpenSelectedTransferWindow();
end;

procedure TFormQueue.ButtonOptionsClick(Sender: TObject);
begin
  self.Options.Show(self);
end;

procedure TFormQueue.ButtonRetryTransferClick(Sender: TObject);
begin
  self.RetryTransfer(VST.FocusedNode);
end;

procedure TFormQueue.ButtonStartStopTransferClick(Sender: TObject);
begin
  case TS7ImageButton(Sender).Value of
    0 : self.StartTransfer(VST.FocusedNode);
    1 : self.StopTransfer(VST.FocusedNode);
  end;
end;

procedure TFormQueue.ButtonStartStopTransferValueChanged(Sender: TObject;
  ANewValue: Integer);
begin
  case ANewValue of
    0 : begin
      TS7ImageButton(Sender).ImageIndex := 13;
    end;

    1 : begin
      TS7ImageButton(Sender).ImageIndex := 12;
    end;
  end;
end;

procedure TFormQueue.FormCreate(Sender: TObject);
begin
  WindowsTaskbar.ProgressMaxValue := 100;
end;

procedure TFormQueue.FormHide(Sender: TObject);
begin
  Timer.Enabled := False;

  WindowsTaskbar.ProgressState := TTaskBarProgressState.None;
end;

procedure TFormQueue.FormShow(Sender: TObject);
begin
  Timer.Enabled := True;

  WindowsTaskbar.ProgressState := TTaskBarProgressState.Normal;
  WindowsTaskbar.ProgressValue := 0;

  self.RefreshIconsStatus();

  VST.Refresh();
end;

procedure TFormQueue.TimerTimer(Sender: TObject);
var ANode            : PVirtualNode;
    pData            : PTreeData;
    ACurrentProgress : Word;
    ATotalProgress   : Word;
    AProgressCount   : Integer;
begin
  {
    Current Progress
  }
  ANode := VST.FocusedNode;

  ACurrentProgress := 0;

  if Assigned(ANode) then begin
    pData := ANode.GetData;

    if pData^.Status = tsProgress then
      ACurrentProgress := pData^.Progress;
  end;

  {
    Total Gauges
  }
  ATotalProgress := 0;
  AProgressCount := 0;
  for ANode in VST.Nodes do begin
    pData := ANode.GetData;

    if pData^.Status <> tsProgress then
      continue;

    Inc(ATotalProgress, pData^.Progress);
    Inc(AProgressCount);
  end;

  if AProgressCount > 0 then
    ATotalProgress := ceil(ATotalProgress div AProgressCount);

  {
    Update Gauges
  }
  self.GaugeSelected.Progress := ACurrentProgress;
  self.GaugeTotal.Progress    := ATotalProgress;

  {
    Update Taskbar
  }
  WindowsTaskbar.ProgressValue := ATotalProgress;
end;

procedure TFormQueue.UploadFile(const AFilePath, ADestinationPath : String);
begin
  self.AddTransfer(AFilePath, IncludeTrailingBackSlash(ADestinationPath) + ExtractFileName(AFilePath), tdUpload);
end;

procedure TFormQueue.DownloadFile(const AOrigin, ADestinationPath : String);
begin
  self.AddTransfer(AOrigin, ADestinationPath, tdDownload);
end;

procedure TFormQueue.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  self.RefreshIconsStatus();

  VST.Refresh();
end;

procedure TFormQueue.VSTDblClick(Sender: TObject);
begin
  self.OpenSelectedTransferWindow();
end;

procedure TFormQueue.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  self.RefreshIconsStatus();

  VST.Refresh();
end;

procedure TFormQueue.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var pData : PTreeData;
begin
  pData := Node.GetData();

  pData^.Client := nil;
  if Assigned(pData^.Lock) then
    FreeAndNil(pData^.Lock);

  if Assigned(pData^.Form) then
    FreeAndNil(pData^.Form);

  ///
  self.RefreshIconsStatus();

  VST.Refresh();
end;

procedure TFormQueue.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormQueue.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData();

  case Column of
    0 : begin
      CellText := ExtractFileName(pData^.Origin);
    end;

    1 : begin
      CellText := Format('%s (%s)', [ExtractFileName(pData^.Destination), ExtractFilePath(pData^.Destination)]);
    end;

    2 : begin
      case pData^.Direction of
        tdDownload : CellText := 'Download';
        tdUpload   : CellText := 'Upload';
      end;
    end;

    3 : begin
      CellText := FormatSize(pData^.Size);
    end;

    4 : begin
      CellText := 'N/A';
      ///

      case pData^.Status of
        tsQueue : begin
          CellText := 'Queue';
        end;

        tsError : begin
          CellText := Format('Error: %s', [pData^.LastError]);
        end;

        tsProgress : begin
          CellText := Format('%d%%', [pData^.Progress]);
        end;
      end;
    end;
  end;
end;

end.
