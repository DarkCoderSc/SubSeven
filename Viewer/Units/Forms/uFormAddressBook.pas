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

unit uFormAddressBook;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.CaptionBar, S7ImageButton,
  VirtualTrees, S7Panel, Vcl.Menus, S7PopupMenu, ___S7BaseForm;

type
  TTreeData = record
    DisplayName   : String;
    Description   : String;
    Password      : String;
    RemoteAddress : String;
    RemotePort    : Word;
    HasPassword   : Boolean;
  end;
  PTreeData = ^TTreeData;

  TFormAddressBook = class(TS7BaseForm)
    CaptionBar: TS7CaptionBar;
    SubSevenForms: TS7Form;
    PanelClient: TS7Panel;
    VST: TVirtualStringTree;
    PanelHeader: TS7Panel;
    ButtonDelete: TS7ImageButton;
    ButtonAdd: TS7ImageButton;
    PopupMenuAction: TS7PopupMenu;
    deleteselectedcontact1: TMenuItem;
    addnewcontacttolist1: TMenuItem;
    N1: TMenuItem;
    ButtonEdit: TS7ImageButton;
    editselectedcontact1: TMenuItem;
    ButtonConnect: TS7ImageButton;
    connecttoselectedcontact1: TMenuItem;
    procedure VSTChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ButtonAddClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VSTDblClick(Sender: TObject);
    procedure deleteselectedcontact1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure addnewcontacttolist1Click(Sender: TObject);
    procedure editselectedcontact1Click(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure connecttoselectedcontact1Click(Sender: TObject);
  protected
    {@M}
    procedure SaveToSettings(); override;
    procedure LoadFromSettings(); override;
  private
    {@M}
    procedure RefreshIconsStatus();
  protected
  public
    {@M}
    function ContactExists(const ARemoteAddress : String; const ARemotePort : Word) : Boolean;
    function GetContact(const ARemoteAddress : String; const ARemotePort : Word) : PVirtualNode;
    function GetContactPassword(const ARemoteAddress : String; const ARemotePort : Word) : String;
    procedure ConnectToContact(const ANode : PVirtualNode);

    procedure AddItem(ANode : PVirtualNode; const ADisplayName, ADescription, ARemoteAddress : String; const ARemotePort : Word; const APassword : String = '');
  end;

var
  FormAddressBook: TFormAddressBook;

const
  PWD_GUID = '{D8B350B2-54D2-4F4E-B810-81EE79FF03C6}';

implementation

uses System.StrUtils, System.Math, uFormAddContact, XSuperObject, Sub7.Core.Crypto.RC4,
     uFormMain, System.Hash, Sub7.Core.Utils;

{$R *.dfm}

procedure TFormAddressBook.ConnectToContact(const ANode : PVirtualNode);
var pData : PTreeData;
begin
  if not Assigned(ANode) then
    Exit();
  ///

  if Assigned(FormMain.Session) then begin
    FormMain.MessageBox.MessageBox(self, 'A session is currently openned. Please terminate current session ' +
                                         'before connecting to selected contact.', 'connect to server', MB_ICONWARNING);
  end else begin
    pData := ANode.GetData;

    FormMain.ComboRemoteAddress.ItemIndex := -1;
    FormMain.ComboRemoteAddress.Text      := pData^.RemoteAddress;
    FormMain.EditRemotePort.Text          := IntToStr(pData^.RemotePort);

    FormMain.OpenSession(
      pData^.RemoteAddress,
      pData^.RemotePort,
      '',
      HashSubSevenPassword(pData^.Password)
    );

    self.Close();
  end;
end;

procedure TFormAddressBook.connecttoselectedcontact1Click(Sender: TObject);
begin
  ButtonConnectClick(ButtonConnect);
end;

function TFormAddressBook.GetContactPassword(const ARemoteAddress : String; const ARemotePort : Word) : String;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  result := '';

  ANode := self.GetContact(ARemoteAddress, ARemotePort);
  if Assigned(ANode) then begin
    pData := ANode.GetData;

    result := HashSubSevenPassword(pData^.Password);
  end;
end;

function TFormAddressBook.GetContact(const ARemoteAddress : String; const ARemotePort : Word) : PVirtualNode;
var ANode : PVirtualNode;
    pData : PTreeData;
begin
  result := nil;
  ///

  for ANode in VST.Nodes do begin
    pData := ANode.GetData;

    if (String.Compare(pData^.RemoteAddress, ARemoteAddress, True) = 0) and
       (pData^.RemotePort = ARemotePort) then begin
         result := ANode;

         break;
       end;
  end;
end;

function TFormAddressBook.ContactExists(const ARemoteAddress : String; const ARemotePort : Word) : Boolean;
begin
  result := Assigned(self.GetContact(ARemoteAddress, ARemotePort));
end;

procedure TFormAddressBook.RefreshIconsStatus();
begin
  ButtonDelete.Enabled  := (VST.FocusedNode <> nil);
  ButtonEdit.Enabled    := (VST.FocusedNode <> nil);
  ButtonConnect.Enabled := (VST.FocusedNode <> nil);

  deleteselectedcontact1.Enabled := ButtonDelete.Enabled;
  editselectedcontact1.Enabled   := ButtonEdit.Enabled;
end;

procedure TFormAddressBook.SaveToSettings();
var ANode       : PVirtualNode;
    pData       : PTreeData;
    AJsonArray  : ISuperArray;
    AJsonObject : ISuperObject;
    ARC4        : TRC4;
begin
  inherited;
  ///

  AJsonArray := TSuperArray.Create();

  for ANode in VST.Nodes do begin
    pData := ANode.GetData;

    AJsonObject := TSuperObject.Create();

    AJsonObject.S['display_name']   := pData^.DisplayName;
    AJsonObject.S['description']    := pData^.Description;
    AJsonObject.S['remote_address'] := pData^.RemoteAddress;
    AJsonObject.I['remote_port']    := pData^.RemotePort;

    if pData^.HasPassword then begin
      ARC4 := TRC4.Create(PWD_GUID);
      try
        AJsonObject.S['password'] := ARC4.Encrypt(pData^.Password, True);
      finally
        if Assigned(ARC4) then
          FreeAndNil(ARC4);
      end;
    end;

    AJsonArray.Add(AJsonObject);
  end;

  ///
  FormMain.SettingHandler.UpdateOrCreateArray('address_book', AJsonArray);
end;

procedure TFormAddressBook.LoadFromSettings();
var AJsonObject : ISuperObject;
    AJsonArray  : ISuperArray;
    ANode       : PVirtualNode;
    pData       : PTreeData;
    I           : Integer;
    APassword   : String;
    ARC4        : TRC4;
begin
  inherited;
  ///

  VST.Clear();

  AJsonArray := FormMain.SettingHandler.GetArray('address_book');
  if not Assigned(AJsonArray) then
    Exit();

  VST.BeginUpdate();
  try
    for I := 0 to AJsonArray.Length -1 do begin
      AJsonObject := AJsonArray.O[I];
      ///

      if not AJsonObject.Contains('display_name') or
         not AJsonObject.Contains('description') or
         not AJsonObject.Contains('remote_address') or
         not AJsonObject.Contains('remote_port') then
        continue;

      if self.ContactExists(
                              AJsonObject.S['remote_address'],
                              AJsonObject.I['remote_port']
      ) then
        continue;
      ///

      APassword := '';

      try
        if AJsonObject.Contains('password') then begin
          ARC4 := TRC4.Create(PWD_GUID);
          try
            APassword := ARC4.Decrypt(AJsonObject.S['password']);
          finally
            if Assigned(ARC4) then
              FreeAndNil(ARC4);
          end;
        end;

        self.AddItem(
          nil,
          AJsonObject.S['display_name'],
          AJsonObject.S['description'],
          AJsonObject.S['remote_address'],
          AJsonObject.I['remote_port'],
          APassword
        );
      except

      end;
    end;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormAddressBook.AddItem(ANode : PVirtualNode; const ADisplayName, ADescription, ARemoteAddress : String; const ARemotePort : Word; const APassword : String = '');
var pData : PTreeData;
begin
  VST.BeginUpdate();
  try
    if ANode = nil then
      ANode := VST.AddChild(nil);

    pData := ANode.GetData;

    pData^.DisplayName   := ADisplayName;
    pData^.RemoteAddress := ARemoteAddress;
    pData^.RemotePort    := ARemotePort;
    pData^.HasPassword   := Length(APassword) > 0;
    pData^.Password      := APassword;
    pData^.Description   := ADescription;
  finally
    VST.EndUpdate();
  end;
end;

procedure TFormAddressBook.addnewcontacttolist1Click(Sender: TObject);
begin
  ButtonAddClick(ButtonAdd);
end;

procedure TFormAddressBook.ButtonAddClick(Sender: TObject);
var AForm : TFormAddContact;
begin
  AForm := TFormAddContact.Create(self, nil);

  AForm.Show();
end;

procedure TFormAddressBook.ButtonConnectClick(Sender: TObject);
begin
  self.ConnectToContact(VST.FocusedNode);
end;

procedure TFormAddressBook.ButtonDeleteClick(Sender: TObject);
begin
  VST.DeleteSelectedNodes();
end;

procedure TFormAddressBook.ButtonEditClick(Sender: TObject);
var AForm           : TFormAddContact;
    ANode           : PVirtualNode;
    F               : TForm;
    I               : Integer;
    AAlreadyEditing : Boolean;
begin
  ANode := VST.FocusedNode;

  if not Assigned(ANode) then
    Exit();

  AAlreadyEditing := False;
  for I := 0 to Screen.FormCount -1 do begin
    F := Screen.Forms[I];

    if F is TFormAddContact then begin
      if Assigned(TFormAddContact(F).Node) then
        if TFormAddContact(F).Node = ANode then begin
          AAlreadyEditing := True;

          F.SetFocus();
        end;
    end;
  end;

  if not AAlreadyEditing then begin
    AForm := TFormAddContact.Create(self, ANode);

    AForm.Show();
  end;
end;

procedure TFormAddressBook.deleteselectedcontact1Click(Sender: TObject);
begin
  ButtonDeleteClick(ButtonDelete);
end;

procedure TFormAddressBook.editselectedcontact1Click(Sender: TObject);
begin
  ButtonEditClick(ButtonEdit);
end;

procedure TFormAddressBook.FormDestroy(Sender: TObject);
begin
  { It is very important to clear the VST before destruction of form to avoid
  Access Violation. For some reason nodes are freed after form destruction whcih
  can cause issues when we are hooking some nodes even like "OnFreeNode" }
  VST.Clear();
end;

procedure TFormAddressBook.FormShow(Sender: TObject);
begin
  self.RefreshIconsStatus();
end;

procedure TFormAddressBook.VSTChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VST.Refresh();

  self.RefreshIconsStatus();
end;

procedure TFormAddressBook.VSTCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var pData1, pData2 : PTreeData;

  function BoolToInt(const AValue : Boolean) : Byte;
  begin
    if AValue then
      result := 1
    else
      result := 0;
  end;

begin
  pData1 := Node1.GetData;
  pData2 := Node2.GetData;

  case Column of
    0 : result := CompareText(pData1^.DisplayName, pData2^.DisplayName);
    1 : result := CompareValue(BoolToInt(pData1^.HasPassword), BoolToInt(pData2^.HasPassword));
    2 : result := CompareText(pData1^.RemoteAddress, pData2^.RemoteAddress);
    3 : result := CompareValue(pData1^.RemotePort, pData2^.RemotePort);
    4 : result := CompareText(pData1^.Description, pData2^.Description);
  end;
end;

procedure TFormAddressBook.VSTDblClick(Sender: TObject);
begin
  self.ConnectToContact(VST.FocusedNode);
end;

procedure TFormAddressBook.VSTFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  VST.Refresh();
end;

procedure TFormAddressBook.VSTFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  self.RefreshIconsStatus();
end;

procedure TFormAddressBook.VSTGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TFormAddressBook.VSTGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var pData : PTreeData;
begin
  pData := Node.GetData;

  case Column of
    0 : CellText := pData^.DisplayName;

    1 : begin
      if pData^.HasPassword then
        CellText := 'Yes'
      else
        CellText := 'No';
    end;

    2 : CellText := pData^.RemoteAddress;
    3 : CellText := IntToStr(pData^.RemotePort);
    4 : CellText := pData^.Description;
  end;
end;

procedure TFormAddressBook.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  self.RefreshIconsStatus();
end;

end.
