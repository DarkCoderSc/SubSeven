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

unit uFormFileManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.CaptionBar, Sub7.Viewer.VCL.SubSevenForm, S7Panel,
  Vcl.StdCtrls, S7Edit, Vcl.ExtCtrls, S7ImageButton, VirtualTrees, S7ComboBox,
  XSuperObject, ___S7ControlWindow, Sub7.Core.FileSystem.Enum, Vcl.Menus,
  S7PopupMenu, Sub7.Core.Protocol, Sub7.Viewer.Clipboard;

type
  TTreeData = record
    FileInformation : TFileInformation;
    ImageIndex      : Integer;
  end;
  PTreeData = ^TTreeData;

  TSelectedBenchmarkResult = record
    SelectedCount : Integer;
    FilesCount    : Integer;
    FoldersCount  : Integer;
  end;

  TFormFileManager = class(TS7ControlWindow)
    SubSevenForms: TS7Form;
    CaptionBar: TS7CaptionBar;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    PopupMenuAction: TS7PopupMenu;
    refreshdrives1: TMenuItem;
    N1: TMenuItem;
    refreshcurrentfolder1: TMenuItem;
    gotorootfolder1: TMenuItem;
    N2: TMenuItem;
    downloadselectedfiles1: TMenuItem;
    uploadfilestocurrentfolder1: TMenuItem;
    N3: TMenuItem;
    createdirectory1: TMenuItem;
    renameselectedfilefolder1: TMenuItem;
    deleteselectedfilesfolders1: TMenuItem;
    N4: TMenuItem;
    PanelClient: TS7Panel;
    PanelRight: TS7Panel;
    ButtonRefreshCurrentDirectory: TS7ImageButton;
    ButtonGotoRoot: TS7ImageButton;
    ButtonDownload: TS7ImageButton;
    ButtonUpload: TS7ImageButton;
    ButtonCreateFolder: TS7ImageButton;
    ButtonDeleteFile: TS7ImageButton;
    ButtonRenameFile: TS7ImageButton;
    ButtonSearchForFile: TS7ImageButton;
    PanelBody: TS7Panel;
    PanelBodyHeader: TS7Panel;
    LabelDrive: TLabel;
    ButtonRefreshDrives: TS7ImageButton;
    ComboDrive: TS7ComboBox;
    PanelPath: TS7Panel;
    EditPath: TS7Edit;
    VST: TVirtualStringTree;
    ButtonLocalClipboard: TS7ImageButton;
    PopupMenuClipboard: TS7PopupMenu;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    N5: TMenuItem;
    Paste1: TMenuItem;
    Pastetocurrentfolder1: TMenuItem;
    N6: TMenuItem;
    Clipboard1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ButtonRefreshDrivesClick(Sender: TObject);
    procedure ComboDriveChange(Sender: TObject);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: TImageIndex);
    procedure VSTNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ButtonRefreshCurrentDirectoryClick(Sender: TObject);
    procedure ButtonGotoRootClick(Sender: TObject);
    procedure ButtonCreateFolderClick(Sender: TObject);
    procedure EditPathChange(Sender: TObject);
    procedure ButtonDeleteFileClick(Sender: TObject);
    procedure ButtonRenameFileClick(Sender: TObject);
    procedure VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure ButtonUploadClick(Sender: TObject);
    procedure VSTRemoveFromSelection(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VSTAddToSelection(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonDownloadClick(Sender: TObject);
    procedure refreshdrives1Click(Sender: TObject);
    procedure gotorootfolder1Click(Sender: TObject);
    procedure refreshcurrentfolder1Click(Sender: TObject);
    procedure downloadselectedfiles1Click(Sender: TObject);
    procedure uploadfilestocurrentfolder1Click(Sender: TObject);
    procedure createdirectory1Click(Sender: TObject);
    procedure deleteselectedfilesfolders1Click(Sender: TObject);
    procedure renameselectedfilefolder1Click(Sender: TObject);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ButtonLocalClipboardClick(Sender: TObject);
    procedure PopupMenuClipboardPopup(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Pastetocurrentfolder1Click(Sender: TObject);
    procedure PopupMenuActionPopup(Sender: TObject);
  private
    {@M}
    procedure BrowsePath(const APath : String);
    procedure RefreshDrives();

    procedure DeleteSelectedFiles();
    procedure CreateFolder();

    procedure RefreshCurrentDirectory();

    function GetItem(const AName : String) : PVirtualNode;
    procedure SelectItem(const AName : String);

    function CompareWithCurrentPath(const APath : String) : Boolean;

    function GetSelectedFiles() : ISuperObject;

    procedure RefreshIconsStatus();

    procedure AppendFileToList(const AFileInfo : TFileInformation; ACheckIfExists : Boolean = False);
    function FileExists(const AName : String; const AType : TFileType) : Boolean;
    function DriveExists(const ALetter : String) : Boolean;
    procedure CopySelectedFileToClipboard(const ACopyMode : TClipboardCopyMode);
    procedure PasteToFolder(const ADestFolder : String);
    function BenchmarkSelectedItems() : TSelectedBenchmarkResult;
    function GetSelectedDirectory() : String;
    function GetItemFullPath(const ANode : PVirtualNode) : String;
  protected
    {@M}
    procedure WMDropFiles(var AMessage: TMessage); message WM_DROPFILES;
    procedure DoResize(); override;
  public
    {@M}
    procedure RenderHardDrives(const AData : ISuperObject);
    procedure RenderFolder(const AData : ISuperObject);
    procedure RenderCreateFolder(const AData : ISuperObject);
    procedure RenderDeleteFile(const AData : ISuperObject);
    procedure RenderRenameFile(const AData : ISuperObject);
    procedure RenderFileUpload(const AData : ISuperObject);

    procedure React(const ACommand : TS7Command; const AData : ISuperObject); override;
  end;

var
  FormFileManager: TFormFileManager;

implementation

uses uFormMain, Sub7.Core.Utils, Sub7.Core.FileSystem.Drives.Enum, IOUtils, math,
     uFormExceptions, Sub7.Core.Exceptions, uFormQueue, WinAPI.ShellAPI, Sub7.Core.Diagnostic,
     Sub7.Core.FileSystem.Utils, Sub7.Viewer.Singletons, Sub7.Core.Serializers.TStringList,
     Sub7.Core.FileSystem.Types;

{$R *.dfm}

function TFormFileManager.GetItemFullPath(const ANode : PVirtualNode) : String;
var pData : PTreeData;
begin
  result := '';
  ///

  if not Assigned(ANode) then
    Exit();
  ///

  pData := ANode.GetData;

  ///
  result := IncludeTrailingBackSlash(EditPath.Text) + pData^.FileInformation.FileName;
end;

function TFormFileManager.GetSelectedDirectory() : String;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  result := '';
  ///

  if VST.SelectedCount <> 1 then
    Exit();

  ANode := VST.FocusedNode;
  pData := ANode.GetData;

  if pData.FileInformation.FileType <> ftFolder then
    Exit();

  ///
  result := self.GetItemFullPath(ANode);
end;

function TFormFileManager.BenchmarkSelectedItems() : TSelectedBenchmarkResult;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  ZeroMemory(@result, SizeOf(TSelectedBenchmarkResult));

  for ANode in VST.SelectedNodes do begin
    pData := ANode.GetData;

    if (pData^.FileInformation.FileType = ftParentFolder) then
      continue;

    Inc(result.SelectedCount);

    case pData^.FileInformation.FileType of
      ftFile   : Inc(result.FilesCount);
      ftFolder : Inc(result.FoldersCount);
    end;
  end;
end;

procedure TFormFileManager.PasteToFolder(const ADestFolder : String);
var AFiles  : TStringList;
    AData   : ISuperObject;
    AAction : TFileAction;
begin
  if not Assigned(SGLT_ExplorerClipboard) then
    Exit();
  ///

  AFiles := TStringList.Create();
  try
    case SGLT_ExplorerClipboard.CopyMode of
      ccmCopy : AAction := faCopy;
      ccmCut  : AAction := faMove;
    end;

    SGLT_ExplorerClipboard.Paste(AFiles);

    if AFiles.Count > 0 then begin
      AData := TSuperObject.Create();

      AData.A['src_files']   := TStringList_Serializer.FlatSerialize(AFiles);
      AData.S['destination'] := ADestFolder;
      AData.I['action']      := Integer(AAction);
    end;

    ///
    self.SendCommand(fmcFileAction, AData);
  finally
    if Assigned(AFiles) then
      FreeAndNil(AFiles);
  end;
end;

procedure TFormFileManager.CopySelectedFileToClipboard(const ACopyMode : TClipboardCopyMode);
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  if not Assigned(SGLT_ExplorerClipboard) then
    Exit();
  ///

  for ANode in VST.SelectedNodes do begin
    pData := ANode.GetData;
    ///

    if pData^.FileInformation.FileType = ftParentFolder then
      continue;

    SGLT_ExplorerClipboard.Add(Format('%s%s', [
      IncludeTrailingPathDelimiter(self.EditPath.Text),
      pData^.FileInformation.FileName
    ]));
  end;

  SGLT_ExplorerClipboard.Copy(ACopyMode);
end;

procedure TFormFileManager.WMDropFiles(var AMessage: TMessage);
var i      : Integer;
    ACount : Integer;
    ALen   : Integer;
    AFile  : String;
begin
  try
    ACount := DragQueryFile(AMessage.WParam, $FFFFFFFF, nil, 0);

    for i := 0 to ACount -1 do begin
      ALen := DragQueryFile(AMessage.WParam, I, nil, 0) +1;

      SetLength(AFile, ALen);

      DragQueryFile(AMessage.WParam, I, PWideChar(AFile), ALen);

      FormQueue.UploadFile(AFile, EditPath.text);
    end;
  finally
    DragFinish(AMessage.WParam);
  end;
end;

function TFormFileManager.FileExists(const AName : String; const AType : TFileType) : Boolean;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  result := False;
  ///

  for ANode in VST.Nodes do begin
    pData := ANode.GetData;

    if (String.Compare(pData^.FileInformation.FileName, AName, True) = 0) and
       (pData^.FileInformation.FileType = AType) then begin
      result := True;

      break;
    end;
  end;
end;

procedure TFormFileManager.AppendFileToList(const AFileInfo : TFileInformation; ACheckIfExists : Boolean = False);
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  if ACheckIfExists then begin
    if FileExists(AFileInfo.FileName, AFileInfo.FileType) then begin
      if Assigned(AFileInfo) then
        FreeAndNil(AFileInfo);

      Exit();
    end;
  end;
  ///

  ANode := VST.AddChild(nil);
  pData := ANode.GetData;

  pData^.FileInformation := AFileInfo;

  if pData^.FileInformation.FileType = ftFile then
    pData^.ImageIndex := SystemFileIcon('.' + ExtractFileExt(pData^.FileInformation.FileName))
  else
    pData^.ImageIndex := SystemFolderIcon();
end;

procedure TFormFileManager.React(const ACommand : TS7Command; const AData : ISuperObject);
begin
  case ACommand of
    {
      @render: Display Uploaded File
    }
    uscUpload : begin
      self.RenderFileUpload(AData);
    end;

    {
      @render: Create Directory
    }
    fmcCreateFolder : begin
      self.RenderCreateFolder(AData);
    end;

    {
      @render: Delete File(s) / Folder(s)
    }
    fmcDeleteFile : begin
      self.RenderDeleteFile(AData);
    end;

    {
      @render: Rename File / Directory
    }
    fmcRenameFile : begin
      self.RenderRenameFile(AData);
    end;
  end;
end;

procedure TFormFileManager.RenderFileUpload(const AData : ISuperObject);
var APath     : String;
    AFileInfo : TFileInformation;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('path') then
    Exit();
  ///

  APath := AData.S['path'];

  if not self.CompareWithCurrentPath(APath) then
    Exit(); // Ignore

  AFileInfo := TFileInformation.Create(AData);

  VST.BeginUpdate();
  try
    self.AppendFileToList(AFileInfo, True);
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormFileManager.RenderRenameFile(const AData : ISuperObject);
var APath : String;
    AOld  : String;
    ANew  : String;
    ANode : PVirtualNode;
    pData : PTreeData;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if AData.Contains('path') and AData.Contains('old_name') and AData.Contains('new_name') then begin
    APath := AData.S['path'];
    AOld  := AData.S['old_name'];
    ANew  := AData.S['new_name'];
    ///

    if not self.CompareWithCurrentPath(APath) then
      Exit();
    ///

    ANode := self.GetItem(AOld);
    if not Assigned(ANode) then
      Exit();
    ///

    VST.BeginUpdate();
    try
      pData := ANode.GetData;

      pData^.FileInformation.FileName := ANew;

      if pData^.FileInformation.FileType = ftFile then
        pData^.ImageIndex := SystemFileIcon(ANew);
    finally
      VST.EndUpdate();
    end;
  end else
    self.RefreshCurrentDirectory();
end;

procedure TFormFileManager.RenderDeleteFile(const AData : ISuperObject);
var APath            : String;
    ASuccessFiles    : ISuperArray;
    AFailedFiles     : ISuperArray;
    I                : Integer;
    ANode            : PVirtualNode;
    AServerException : ES7ServerException;
    AReason          : String;
    AName            : String;
    AFailedFile      : ISuperObject;
    AErrList         : String;
begin
  if not Assigned(AData) then
    Exit();
  ///

  ASuccessFiles := nil;
  AFailedFiles  := nil;

  if AData.Contains('path') and (AData.Contains('success') or AData.Contains('failed')) then begin

    APath := AData.S['path'];

    if AData.Contains('success') then
      ASuccessFiles := AData.A['success'];

    if AData.Contains('failed') then
      AFailedFiles := AData.A['failed'];
    ///

    {
      Render Files that has succeed
    }
    if Assigned(ASuccessFiles) and self.CompareWithCurrentPath(APath) then begin
      for i := 0 to ASuccessFiles.Length -1 do begin
        ANode := self.GetItem(ASuccessFiles.S[i]);
        ///

        if Assigned(ANode) then begin
          VST.BeginUpdate();
          try
            VST.DeleteNode(ANode);
          finally
            VST.EndUpdate();
          end;
        end;
      end;
    end;

    {
      Render files that has failed
    }
    if Assigned(AFailedFiles) then begin
      AErrList := '';

      for i := 0 to AFailedFiles.Length -1 do begin
        AFailedFile := AFailedFiles.O[i];

        AReason := 'Undefined';
        if AFailedFile.Contains('reason') then
          AReason := AFailedFile.S['reason'];

        AName := 'Undefined';
        if AFailedFile.Contains('name') then
          AName := AFailedFile.S['name'];

        AServerException := ES7ServerException.Create(AReason);

        FormExceptions.AddItem(AServerException, True {Silent});

        AErrList := Format('* %s%s', [AName, #13#10]);
      end;

      { Only display on forground file manager }
      if self.Handle = GetForegroundWindow() then
        FormMain.MessageBox.MessageBox(
          self,
          Format('Could not delete bellow file(s)/folder(s):%s%s', [#13#10, AErrList]),
          'Delete File',
          MB_ICONERROR
        );
    end;
  end else
    self.RefreshCurrentDirectory();
end;

procedure TFormFileManager.RefreshIconsStatus();
var AValidPath : Boolean;
    ABench     : TSelectedBenchmarkResult;
begin
  AValidPath := (EditPath.Text <> '');

  ABench := self.BenchmarkSelectedItems();

  self.ButtonRefreshCurrentDirectory.Enabled := AValidPath;
  self.ButtonGotoRoot.Enabled                := AValidPath;
  self.ButtonDownload.Enabled                := AValidPath and (ABench.FilesCount > 0);
  self.ButtonUpload.Enabled                  := AValidPath;
  self.ButtonCreateFolder.Enabled            := AValidPath;
  self.ButtonDeleteFile.Enabled              := AValidPath and (ABench.SelectedCount > 0);
  self.ButtonRenameFile.Enabled              := AValidPath and (ABench.SelectedCount = 1);
  self.ButtonLocalClipboard.Enabled          := AValidPath;


  { Apply to Popup Menu }
  self.refreshdrives1.Enabled              := True;
  self.refreshcurrentfolder1.Enabled       := self.ButtonRefreshCurrentDirectory.Enabled;
  self.gotorootfolder1.Enabled             := self.ButtonGotoRoot.Enabled;
  self.downloadselectedfiles1.Enabled      := self.ButtonDownload.Enabled;
  self.uploadfilestocurrentfolder1.Enabled := self.ButtonUpload.Enabled;
  self.createdirectory1.Enabled            := self.ButtonCreateFolder.Enabled;
  self.deleteselectedfilesfolders1.Enabled := self.ButtonDeleteFile.Enabled;
  self.renameselectedfilefolder1.Enabled   := self.ButtonRenameFile.Enabled;
end;

procedure TFormFileManager.renameselectedfilefolder1Click(Sender: TObject);
begin
  ButtonRenameFileClick(ButtonRenameFile);
end;

function TFormFileManager.GetSelectedFiles() : ISuperObject;
var ANode  : PVirtualNode;
    pData  : PTreeData;
    AFiles : ISuperArray;
begin
  result := nil;
  AFiles := nil;
  ///

  for ANode in VST.SelectedNodes do begin
    pData := ANode.GetData;
    ///

    if not Assigned(AFiles) then
      AFiles := TSuperArray.Create();

    AFiles.Add(pData^.FileInformation.FileName);
  end;

  if Assigned(AFiles) then begin
    result := TSuperObject.Create();

    result.V['path']  := EditPath.Text;
    result.A['files'] := AFiles;
  end;
end;

procedure TFormFileManager.gotorootfolder1Click(Sender: TObject);
begin
  ButtonGotoRootClick(ButtonGotoRoot);
end;

procedure TFormFileManager.Paste1Click(Sender: TObject);
begin
  self.PasteToFolder(self.GetSelectedDirectory());
end;

procedure TFormFileManager.Pastetocurrentfolder1Click(Sender: TObject);
begin
  self.PasteToFolder(EditPath.Text);
end;

procedure TFormFileManager.PopupMenuActionPopup(Sender: TObject);
begin
  ///
  ///
  ///

  // TODO continuer, déclencher le OnPopupMenu de l'autre popup clipboard et copier ces valuers et enabled dans celui la.
end;

procedure TFormFileManager.PopupMenuClipboardPopup(Sender: TObject);
var b      : Boolean;
    ABench : TSelectedBenchmarkResult;
begin
  b := Assigned(SGLT_ExplorerClipboard);
  if b then
    b := SGLT_ExplorerClipboard.HasContent;
  ///

  ABench := self.BenchmarkSelectedItems();

  Paste1.Enabled                := b and (ABench.FoldersCount = 1);
  Pastetocurrentfolder1.Enabled := b and (EditPath.Text <> '');
end;

function TFormFileManager.CompareWithCurrentPath(const APath : String) : Boolean;
begin
  result := (String.Compare(APath, EditPath.Text, True) = 0);
end;

procedure TFormFileManager.Copy1Click(Sender: TObject);
begin
  self.CopySelectedFileToClipboard(ccmCopy);
end;

procedure TFormFileManager.createdirectory1Click(Sender: TObject);
begin
  ButtonCreateFolderClick(ButtonCreateFolder);
end;

procedure TFormFileManager.RefreshCurrentDirectory();
begin
  self.BrowsePath(EditPath.text);
end;

procedure TFormFileManager.refreshcurrentfolder1Click(Sender: TObject);
begin
  ButtonRefreshCurrentDirectoryClick(ButtonRefreshCurrentDirectory);
end;

function TFormFileManager.GetItem(const AName : String) : PVirtualNode;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  result := nil;
  ///

  for ANode in VST.Nodes do begin
    pData := ANode.GetData;
    ///

    if String.Compare(AName, pData^.FileInformation.FileName, True) = 0 then begin
      result := ANode;
      break;
    end;
  end;
end;

procedure TFormFileManager.SelectItem(const AName : String);
var ANode : PVirtualNode;
begin
  ANode := self.GetItem(AName);
  if Assigned(ANode) then
    VST.FocusedNode := ANode;
end;

procedure TFormFileManager.uploadfilestocurrentfolder1Click(Sender: TObject);
begin
  ButtonUploadClick(ButtonUpload);
end;

procedure TFormFileManager.RenderCreateFolder(const AData : ISuperObject);
var AName     : String;
    APath     : String;
    ANode     : PVirtualNode;
    pData     : PTreeData;
    AFileInfo : TFileInformation;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if AData.Contains('name') and AData.Contains('path') then begin
    AName := AData.V['name'];
    APath := AData.V['path'];
    ///

    if not self.CompareWithCurrentPath(APath) then
      Exit();
    ///

    if Assigned(self.GetItem(AName)) then
      Exit();
    ///

    VST.BeginUpdate();
    try
      ANode := VST.AddChild(nil);
      pData := ANode.GetData;

      AFileInfo := TFileInformation.Create();

      AFileInfo.FileName     := AName;
      AFileInfo.Size         := 0;
      AFileInfo.FileType     := ftFolder;

      pData^.FileInformation := AFileInfo;
      pData^.ImageIndex      := SystemFolderIcon();

      ///
      VST.FocusedNode := ANode;
    finally
      VST.EndUpdate();
    end;
  end else
    self.RefreshCurrentDirectory();
end;

procedure TFormFileManager.DeleteSelectedFiles();
var AData : ISuperObject;
begin
  AData := self.GetSelectedFiles();

  if not Assigned(AData) then
    Exit();
  ///

  if FormMain.MessageBox.MessageBox(self, Format('You are about to permanently delete %d file(s)/folder(s). This action can''t be undone. Notice: selected directories are deleted recursively. Are you sure ?', [VST.SelectedCount]), 'delete file(s)/folder(s)', MB_ICONQUESTION + MB_YESNO) = ID_NO then
    Exit();

  ///
  self.SendCommand(fmcDeleteFile, AData);
end;

procedure TFormFileManager.deleteselectedfilesfolders1Click(Sender: TObject);
begin
  ButtonDeleteFileClick(ButtonDeleteFile);
end;

procedure TFormFileManager.CreateFolder();
var AQuery : String;
    AData  : ISuperObject;
begin
  AQuery := FormMain.MessageBox.InputQuery(self, 'folder name:', 'create folder');

  if AQuery = '' then
    Exit();

  AData := TSuperObject.Create();

  AData.V['name'] := AQuery;
  AData.V['path'] := EditPath.Text;

  ///
  self.SendCommand(fmcCreateFolder, AData);
end;

procedure TFormFileManager.Cut1Click(Sender: TObject);
begin
  self.CopySelectedFileToClipboard(ccmCut);
end;

procedure TFormFileManager.RenderFolder(const AData : ISuperObject);
var AFolder   : TS7EnumFolder;
    I         : Integer;
    AFileInfo : TFileInformation;
begin
  VST.Clear();
  ///

  if not Assigned(AData) then
    Exit();

  AFolder := TS7EnumFolder.Create(False);
  try
    AFolder.DeSerialize(AData);
    ///

    EditPath.text := AFolder.CurrentPath;

    if AFolder.Items.Count < 0 then
      Exit();

    VST.BeginUpdate();
    try
      for I := 0 to AFolder.Items.Count -1 do begin
        AFileInfo := AFolder.Items[i];

        AppendFileToList(AFileInfo, False);
      end;
    finally
      VST.EndUpdate();
    end;
  finally
    if Assigned(AFolder) then
      FreeAndNil(AFolder);
  end;
end;

procedure TFormFileManager.BrowsePath(const APath : String);
var AData : ISuperObject;
begin
  AData := TSuperObject.Create();

  AData.V['path'] := IncludeTrailingPathDelimiter(APath);

  self.SendCommand(fmcBrowseFolder, AData);
end;

procedure TFormFileManager.RefreshDrives();
begin
  self.SendCommand(fmcRefreshDrives);
end;

procedure TFormFileManager.refreshdrives1Click(Sender: TObject);
begin
  ButtonRefreshDrivesClick(ButtonRefreshDrives);
end;

function TFormFileManager.DriveExists(const ALetter : String) : Boolean;
var I      : Integer;
    AValue : String;
begin
  result := False;
  ///

  for I := 0 to self.combodrive.Items.count -1 do begin
    AValue := Copy(self.combodrive.Items.Strings[i], 0, Length(ALetter));

    if String.Compare(AValue, ALetter, True) = 0 then begin
      result := True;

      break;
    end;
  end;
end;

procedure TFormFileManager.RenderHardDrives(const AData : ISuperObject);
var ADrives        : TS7EnumHardDrives;
    ADrive         : TDriveInformation;
    I              : Integer;
    AName          : String;
    ASelectedIndex : Integer;
begin
  if not Assigned(AData) then
    Exit();
  ///

  self.combodrive.Clear();

  ADrives := TS7EnumHardDrives.Create(True);
  try
    ADrives.DeSerialize(AData);

    ASelectedIndex := -1;

    for I := 0 to ADrives.Items.Count -1 do begin
      ADrive := ADrives.Items[I];

      AName := Format('%s [%s]', [
                                    ADrive.Letter,
                                    ADrives.DriveTypeToString(ADrive.DriveType),
                                    ADrive.PartitionName
      ]);

      if Length(Trim(ADrive.PartitionName)) > 0 then
        AName := Format('%s - %s', [AName, ADrive.PartitionName]);

      self.combodrive.Items.Add(AName);

      if not EditPath.IsEmpty then
        if String.Compare(Copy(EditPath.text, 0, Length(ADrive.Letter)), ADrive.Letter, True) = 0 then
          ASelectedIndex := I;
    end;

    if ASelectedIndex <> -1 then
      self.combodrive.ItemIndex := ASelectedIndex;
  finally
    if Assigned(ADrives) then
      FreeAndNil(ADrives);
  end;
end;

procedure TFormFileManager.ButtonRefreshCurrentDirectoryClick(Sender: TObject);
begin
  self.RefreshCurrentDirectory();
end;

procedure TFormFileManager.ButtonGotoRootClick(Sender: TObject);
begin
  self.BrowsePath(TPath.GetPathRoot(EditPath.text));
end;

procedure TFormFileManager.ButtonLocalClipboardClick(Sender: TObject);
var APoint : TPoint;
begin
  APoint := self.ButtonLocalClipboard.ClientToScreen(Point(0, 0));

  self.PopupMenuClipboard.Popup(APoint.X, APoint.Y + TS7ImageButton(Sender).Height);
end;

procedure TFormFileManager.ButtonCreateFolderClick(Sender: TObject);
begin
  self.CreateFolder();
end;

procedure TFormFileManager.ButtonDeleteFileClick(Sender: TObject);
begin
  self.DeleteSelectedFiles();
end;

procedure TFormFileManager.ButtonDownloadClick(Sender: TObject);
var pData     : PTreeData;
    ANode     : PVirtualNode;
    ADestPath : String;
    ADestFile : String;
    ASrcFile  : String;
begin
  if VST.SelectedCount = 0 then
    Exit();
  ///

  if VST.SelectedCount = 1 then begin
    {
      Save Dialog
    }
    pData := VST.FocusedNode.GetData;

    SaveDialog.DefaultExt := ExtractFileExt(pData^.FileInformation.FileName);
    SaveDialog.FileName   :=pData^.FileInformation.FileName;

    if not SaveDialog.Execute() then
      Exit();
    ///

    FormQueue.DownloadFile(IncludeTrailingPathDelimiter(EditPath.text) + pData^.FileInformation.FileName, SaveDialog.FileName);
  end else begin
    {
      Select Destination Path
    }
    ADestPath := BrowseForFolder('Destination path:', '', True);

    for ANode in VST.Nodes do begin
      if not (vsSelected in ANode.States) then
        continue;

      pData := ANode.GetData;

      ASrcFile  := IncludeTrailingPathDelimiter(EditPath.text) + pData^.FileInformation.FileName;

      if pData^.FileInformation.FileType <> ftFile then
        continue;

      ADestFile := ADestPath + pData^.FileInformation.FileName;

      FormQueue.DownloadFile(ASrcFile, ADestFile);
    end;
  end;
end;

procedure TFormFileManager.VSTAddToSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  self.RefreshIconsStatus();
end;

procedure TFormFileManager.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
  var AData1, AData2 : PTreeData;
begin
  AData1 := VST.GetNodeData(Node1);
  AData2 := VST.GetNodeData(Node2);

  if not Assigned(AData1) or not Assigned(AData2) then
    Exit();

  if AData1^.FileInformation.FileType = ftParentFolder then begin
    result := 0;
  end else begin
    {
      Separate Folders and Files
    }
    if (AData1^.FileInformation.FileType = ftFolder) and (AData2^.FileInformation.FileType <> ftFolder) then
      result := -1
    else if (AData1^.FileInformation.FileType <> ftFolder) and (AData2^.FileInformation.FileType = ftFolder) then
      result := 1
    else
      result := 0;

    case column of
      0 : begin
        {
          Sort alphabetically by name
        }
        if (result = 0) then
          result := CompareText(AData1^.FileInformation.FileName, AData2^.FileInformation.FileName);
      end;

      1 : begin
        {
          Sort by size
        }
        if (result = 0) then
          result := CompareValue(AData1^.FileInformation.Size, AData2^.FileInformation.Size);
      end;
    end;
  end;
end;

procedure TFormFileManager.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFormFileManager.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var pData : PTreeData;
begin
  pData := Node.GetData;

  if Assigned(pData) then begin
    if Assigned(pData^.FileInformation) then
      FreeAndNil(pData^.FileInformation);
  end;
end;

procedure TFormFileManager.VSTGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
  var AData : PTreeData;
begin
  AData := VST.GetNodeData(Node);

  if column <> 0 then
    Exit();

  if (Kind = ikNormal) or (Kind = ikSelected) then begin
    ImageIndex := AData^.ImageIndex;
  end;
end;

procedure TFormFileManager.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormFileManager.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
  var AData : PTreeData;
begin
  AData := VST.GetNodeData(Node);

  case column of
    0 : begin
      CellText := AData^.FileInformation.FileName;
    end;

    1 : begin
      if AData^.FileInformation.Size = 0 then
        CellText := ''
      else
        CellText := FormatSize(AData^.FileInformation.Size);
    end;
  end;
end;

procedure TFormFileManager.VSTNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var pData : PTreeData;
    AData : ISuperObject;
begin
  pData := Node.GetData();

  if NewText <> pData^.FileInformation.FileName then begin
    AData := TSuperObject.Create();

    AData.S['path']     := EditPath.Text;
    AData.S['old_name'] := pData^.FileInformation.FileName;
    AData.S['new_name'] := NewText;

    ///
    self.SendCommand(fmcRenameFile, AData);
  end;
end;

procedure TFormFileManager.VSTNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
  var pData : PTreeData;
      APath : String;
begin
  pData := HitInfo.HitNode.GetData();
  if not Assigned(pData) then
    Exit();

  APath := '';
  if pData^.FileInformation.FileType = ftFolder then begin
    APath := Format('%s%s', [IncludeTrailingPathDelimiter(EditPath.text), pData^.FileInformation.FileName])
  end else if pData^.FileInformation.FileType = ftParentFolder then begin
    APath := TDirectory.GetParent(ExcludeTrailingPathDelimiter(EditPath.text));
  end;

  if APath <> '' then
    self.BrowsePath(APath);
end;

procedure TFormFileManager.VSTRemoveFromSelection(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  self.RefreshIconsStatus();
end;

procedure TFormFileManager.DoResize();
begin
  inherited;
  ///

  combodrive.Width := PanelBodyHeader.Width - ComboDrive.left - ButtonRefreshDrives.Width - 4;
end;

procedure TFormFileManager.downloadselectedfiles1Click(Sender: TObject);
begin
  ButtonDownloadClick(ButtonDownload);
end;

procedure TFormFileManager.EditPathChange(Sender: TObject);
begin
  self.RefreshIconsStatus();
end;

procedure TFormFileManager.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(self.Handle, True);

  self.RefreshIconsStatus();
end;

procedure TFormFileManager.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(self.Handle, False);
end;

procedure TFormFileManager.FormShow(Sender: TObject);
begin
  self.RefreshDrives();
end;

procedure TFormFileManager.ButtonRefreshDrivesClick(Sender: TObject);
begin
  self.RefreshDrives();
end;

procedure TFormFileManager.ButtonRenameFileClick(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();
  ///

  VST.EditNode(VST.FocusedNode, 0);
end;

procedure TFormFileManager.ButtonUploadClick(Sender: TObject);
var I : Integer;
begin
  if not OpenDialog.Execute() then
    Exit();
  ///

  for i := 0 to OpenDialog.Files.Count -1 do begin
    FormQueue.UploadFile(OpenDialog.Files.Strings[i], EditPath.text);
  end;
end;

procedure TFormFileManager.ComboDriveChange(Sender: TObject);
var ALetter : String;
begin
  if length(trim(combodrive.text)) > 0 then begin
    ALetter := Copy(combodrive.Text, 1, 2);

    self.BrowsePath(ALetter);
  end;
end;

end.
