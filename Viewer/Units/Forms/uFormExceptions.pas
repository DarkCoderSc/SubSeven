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

unit uFormExceptions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.CaptionBar, Sub7.Viewer.VCL.SubSevenForm, VirtualTrees,
  S7Panel, Sub7.Core.Protocol, Sub7.Core.Exceptions, S7ImageButton, Sub7.Viewer.VCL.CheckBox, S7OptionDialog,
  Vcl.Menus, S7PopupMenu, ___S7BaseForm;

type
  TTreeData = record
    Date      : TDateTime;
    ClassName : String;
    Context   : TS7ExceptionContext;
    ExMessage : String;
  end;
  PTreeData = ^TTreeData;

  TFormExceptions = class(TS7BaseForm)
    SubSevenForms: TS7Form;
    CaptionBar: TS7CaptionBar;
    PanelClient: TS7Panel;
    PanelHeader: TS7Panel;
    ButtonClear: TS7ImageButton;
    ButtonDelete: TS7ImageButton;
    VST: TVirtualStringTree;
    CheckBoxAutoScroll: TS7CheckBox;
    ButtonOptions: TS7ImageButton;
    Options: TS7OptionDialog;
    PopupMenuAction: TS7PopupMenu;
    deleteselectedexception1: TMenuItem;
    N1: TMenuItem;
    clearlist1: TMenuItem;
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure deleteselectedexception1Click(Sender: TObject);
    procedure clearlist1Click(Sender: TObject);
  private
    {@M}
    procedure RefreshIconsStatus();
  protected
  public
    {@M}
    procedure AddItem(const AException : Exception; const AForceSilent : Boolean = False);
  end;

var
  FormExceptions: TFormExceptions;

implementation

uses System.DateUtils, uFormMain, System.Math;

{$R *.dfm}

procedure TFormExceptions.RefreshIconsStatus();
begin
  ButtonDelete.Enabled                  := VST.FocusedNode <> nil;
  self.deleteselectedexception1.Enabled := ButtonDelete.Enabled;
end;

procedure TFormExceptions.ButtonClearClick(Sender: TObject);
begin
  VST.Clear();

  RefreshIconsStatus();
end;

procedure TFormExceptions.ButtonDeleteClick(Sender: TObject);
begin
  if VST.FocusedNode = nil then
    Exit();

  VST.BeginUpdate();
  try
    VST.DeleteNode(VST.FocusedNode);
  finally
    VST.EndUpdate();
  end;

  ///
  RefreshIconsStatus();
end;

procedure TFormExceptions.ButtonOptionsClick(Sender: TObject);
begin
  self.Options.Show(self);
end;

procedure TFormExceptions.clearlist1Click(Sender: TObject);
begin
  ButtonClearClick(ButtonClear);
end;

procedure TFormExceptions.deleteselectedexception1Click(Sender: TObject);
begin
  ButtonDeleteClick(ButtonDelete);
end;

procedure TFormExceptions.FormCreate(Sender: TObject);
begin
  RefreshIconsStatus();
end;

procedure TFormExceptions.AddItem(const AException : Exception; const AForceSilent : Boolean = False);
var ANode    : PVirtualNode;
    AData    : PTreeData;
    AContext : TS7ExceptionContext;
begin
  if AException is ES7ServerException then
    AContext := ecServer
  else
    AContext := ecViewer;
  ///

  VST.BeginUpdate();
  try
    ANode := VST.AddChild(nil);

    AData := VST.GetNodeData(ANode);

    AData^.Date      := Now();
    AData^.ClassName := AException.ClassName;
    AData^.Context   := AContext;
    AData^.ExMessage := AException.Message;
  finally
    VST.EndUpdate();
  end;

  if not AForceSilent then begin
    if (not Options.Get('silentexc', False)) then
      FormMain.MessageBox.MessageBox(Screen.ActiveForm, AException.Message, 'Sub7 Exception', MB_ICONERROR);
  end;

  ///
  if CheckBoxAutoScroll.Checked then
    VST.Perform(WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TFormExceptions.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
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
        Date
      }
      result := CompareDateTime(AData1^.Date, AData2^.Date);
    end;

    1 : begin
      {
        Int
      }
      result := CompareValue(Integer(AData1^.Context), Integer(AData2^.Context));
    end;

    2 : begin
      {
        Text
      }
      result := CompareText(AData1^.ClassName, AData2^.ClassName);
    end;

    3 : begin
      {
        Text
      }
      result := CompareText(AData1^.ExMessage, AData2^.ExMessage);
    end;
  end;
end;

procedure TFormExceptions.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  RefreshIconsStatus();

  VST.Refresh();
end;

procedure TFormExceptions.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormExceptions.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
  var AData : PTreeData;
begin
  AData := VST.GetNodeData(Node);
  ///

  case column of
    0 : CellText := DateTimeToStr(AData^.Date);

    1 : begin
      CellText := '';

      case AData^.Context of
        ecViewer : CellText := 'Viewer';
        ecServer : CellText := 'Server';
      end;
    end;

    2 : CellText := AData^.ClassName;
    3 : CellText := AData^.ExMessage;
  end;
end;

procedure TFormExceptions.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  RefreshIconsStatus();
end;

end.
