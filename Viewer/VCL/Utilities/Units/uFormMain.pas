unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.Menus;

type
  TTreeData = record
    FileName : String;
  end;
  PTreeData = ^TTreeData;

  TFormMain = class(TForm)
    VST: TVirtualStringTree;
    OpenDialog: TOpenDialog;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    OpenFiles1: TMenuItem;
    N1: TMenuItem;
    Clear1: TMenuItem;
    Generate1: TMenuItem;
    procedure Clear1Click(Sender: TObject);
    procedure OpenFiles1Click(Sender: TObject);
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure Generate1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses VCL.Imaging.pngimage, System.ZLib, XSuperObject, UntRC4;

{$R *.dfm}

procedure TFormMain.Clear1Click(Sender: TObject);
begin
  VST.Clear();
end;

procedure TFormMain.Generate1Click(Sender: TObject);
var AFile            : TFileStream;
    ANode            : PVirtualNode;
    pData            : PTreeData;
    AStream          : TMemoryStream;
    ATotalStream     : TMemoryStream;

    AOutputStream    : TMemoryStream;

    AMap             : ISuperArray;
    AJsonMap         : String;
    AHeaderSize      : Int64;
    AHeaderSignature : Int64;
    ARC4             : TRC4;

const HEADER_SIGNATURE = 2801202100014032014;

begin
  AFile := TFileStream.Create('c:\temp\output.bin', fmCreate or fmShareExclusive);
  try
    AStream      := TMemoryStream.Create();
    ATotalStream := TMemoryStream.Create();
    try
      AMap := TSuperArray.Create();

      for ANode in VST.Nodes do begin
        pData := ANode.GetData;

        AStream.Clear();

        AStream.LoadFromFile(pData^.FileName);

        AMap.Add(ATotalStream.Size);

        ATotalStream.Write(PByte(AStream.Memory)^, AStream.Size);
      end;

      AOutputStream := TMemoryStream.Create();
      try
        AJsonMap := AMap.AsJson();

        AHeaderSize := Length(AJsonMap) * SizeOf(WideChar);
        AHeaderSignature := HEADER_SIGNATURE;

        AOutputStream.Write(AHeaderSignature, SizeOf(Int64));
        AOutputStream.Write(AHeaderSize, SizeOf(Int64));
        AOutputStream.Write(AJsonMap[1], AHeaderSize);
        AOutputStream.Write(PByte(ATotalStream.Memory)^, ATotalStream.Size);

        ARC4 := TRC4.Create('{7949D6B7-0FC4-4A6A-A597-035C80A8FCE3}');
        try
          ARC4.Encrypt(AOutputStream.Memory, AOutputStream.Size);
        finally
          if Assigned(ARC4) then
            FreeAndNil(ARC4);
        end;

        ///
        AFile.Write(PByte(AOutputStream.Memory)^, AOutputStream.Size);
      finally
        if Assigned(AOutputStream) then
          FreeAndNil(AOutputStream);
      end;
    finally
      if Assigned(AStream) then
        FreeAndNil(AStream);

      if Assigned(ATotalStream) then
        FreeAndNil(ATotalStream);
    end;
  finally
    if Assigned(AFile) then
      FreeAndNil(AFile);
  end;

  ///
  Application.MessageBox('Finished', 'Generate', MB_ICONINFORMATION);
end;

procedure TFormMain.OpenFiles1Click(Sender: TObject);
var i     : Integer;
    ANode : PVirtualNode;
    pData : PTreeData;
begin
  if not OpenDialog.Execute() then
    Exit();

  VST.BeginUpdate();
  try
    for I := 0 to OpenDialog.Files.Count -1 do begin
      ANode := VST.AddChild(nil);

      pData := ANode.GetData;

      pData^.FileName := OpenDialog.Files.Strings[i];
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormMain.VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  VST.Refresh();
end;

procedure TFormMain.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormMain.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;

  case column of
    0 : CellText := ExtractFileName(pData^.FileName);
  end;
end;

end.
