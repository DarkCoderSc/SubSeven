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

unit S7OptionDialog;

interface

uses System.Classes, VCL.Forms, S7SettingHandler;

type
  TS7OptionDialog = class;

  TOptionKind = (
    okCheckBox
  );

  TS7OptionItem = class(TCollectionItem)
  private
    FKind         : TOptionKind;
    FCaption      : String;
    FChecked      : Boolean;
    FHint         : String;
    FEnabled      : Boolean;
    FName         : String;

    FOptionDialog : TS7OptionDialog;
  public
    {@C}
    constructor Create(ACollection : TCollection); override;
    destructor Destroy(); override;
  published
    {@G/S}
    property Kind    : TOptionKind read FKind    write FKind;
    property Caption : String      read FCaption write FCaption;
    property Checked : Boolean     read FChecked write FChecked;
    property Hint    : String      read FHint    write FHint;
    property Enabled : Boolean     read FEnabled write FEnabled;
    property Name    : String      read FName    write FName;
  end;

  TS7OptionItems = class(TOwnedCollection)
  private
  protected
    {@M}
    function GetItem(Index: Integer): TS7OptionItem;
    procedure SetItem(Index: Integer; Value: TS7OptionItem);
    procedure Notify(Item : TCollectionItem; Action : TCollectionNotification); override;
  public
    {@C}
    constructor Create(AOwner: TPersistent);

    {@M}
    function Add: TS7OptionItem;
    function Get(AName : String; ADefault : Boolean) : Boolean;

    {@G/S}
    property Items[Index: Integer]: TS7OptionItem read GetItem write SetItem; default;
  end;

  TS7OptionDialog = class(TComponent)
  private
    FOptions     : TS7OptionItems;
    FCaption     : String;
    FWidth       : Integer;
    FSetting     : TS7SettingHandler;
    FParentChain : String;

    {@M}
    procedure LoadFromSetting();
  public
    {@C}
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    {@M}
    procedure Show(AOwnerForm : TForm);
    function Get(AName : String; ADefault : Boolean) : Boolean;
    procedure Loaded(); override;
    procedure BeforeDestruction(); override;
  published
    {@G}
    property Options  : TS7OptionItems    read FOptions  write FOptions;
    property Caption  : String            read FCaption  write FCaption;
    property Width    : Integer           read FWidth    write FWidth;
    property Setting  : TS7SettingHandler read FSetting  write FSetting;
  end;

implementation

uses System.SysUtils, S7OptionDialogForm, Winapi.Windows, S7Common;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7OptionDialog


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{ TS7OptionDialog.Loaded }

procedure TS7OptionDialog.Loaded();
begin
  inherited Loaded();
  ///

  FParentChain := GetParentChain(self);

  if Assigned(FSetting) then
    self.LoadFromSetting();
end;

{ TS7OptionDialog.BeforeDestruction }

procedure TS7OptionDialog.BeforeDestruction();
var AItem : TS7OptionItem;
    I     : Integer;
begin
  if not Assigned(FSetting) then
    Exit();

  for I := 0 to FOptions.Count -1 do begin
    AItem := FOptions.Items[i];

    FSetting.Write(AItem.Name, AItem.Checked, FParentChain);
  end;

  ///
  inherited BeforeDestruction();
end;

{ TS7OptionDialog.LoadFromSetting }

procedure TS7OptionDialog.LoadFromSetting();
var AItem : TS7OptionItem;
    I     : Integer;
begin
  if not Assigned(FSetting) then
    Exit();

  for I := 0 to FOptions.Count -1 do begin
    AItem := FOptions.Items[i];

    AItem.Checked := FSetting.Read(AItem.Name, AItem.Checked, FParentChain);
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7OptionDialog.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ///

  FOptions     := TS7OptionItems.Create(self);
  FCaption     := 'Options';
  FSetting     := nil;
  FWidth       := 215;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7OptionDialog.Destroy();
begin
  if Assigned(FOptions) then
    FreeAndNil(FOptions);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Show option form (if Refresh is set to true, we re render all options)
-------------------------------------------------------------------------------}
procedure TS7OptionDialog.Show(AOwnerForm : TForm);
var AOptionForm : TSub7FormOptionDialog;
begin
  AOptionForm := TSub7FormOptionDialog.Create(AOwnerForm, self);
  try
    AOptionForm.Render();

    ///
    AOptionForm.ShowModal();
  finally
    if Assigned(AOptionForm) then
      FreeAndNil(AOptionForm);
  end;
end;

{-------------------------------------------------------------------------------
  Retrieve options checked status
-------------------------------------------------------------------------------}
function TS7OptionDialog.Get(AName : String; ADefault : Boolean) : Boolean;
begin
  result := self.Options.Get(AName, ADefault);
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7OptionItem


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

constructor TS7OptionItem.Create(ACollection : TCollection);
begin
  inherited Create(ACollection);
  ///

  FKind         := okCheckBox;
  FCaption      := Format('Option n°%d', [self.Index]);
  FName         := Format('Option%d', [self.Index]);
  FChecked      := False;
  FHint         := '';
  FEnabled      := True;

  FOptionDialog := TS7OptionDialog(TS7OptionItems(self.GetOwner).Owner);
end;

destructor TS7OptionItem.Destroy();
begin
  inherited Destroy();
  ///

end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  TS7OptionItems


+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

function TS7OptionItems.Add: TS7OptionItem;
begin
  Result := TS7OptionItem(inherited Add);
end;

function TS7OptionItems.Get(AName : String; ADefault : Boolean) : Boolean;
var I : integer;
begin
  result := ADefault;
  ///

  for i := 0 to self.Count -1 do begin
    if String.Compare(self.Items[i].Name, AName, True) = 0 then begin
      result := self.Items[i].Checked;

      break;
    end;
  end;
end;

constructor TS7OptionItems.create(AOwner : TPersistent);
begin
  inherited Create(AOwner, TS7OptionItem);
  ///

end;

function TS7OptionItems.GetItem(Index: Integer): TS7OptionItem;
begin
  Result := TS7OptionItem(inherited GetItem(Index));
end;

procedure TS7OptionItems.SetItem(Index: Integer; Value: TS7OptionItem);
begin
  inherited SetItem(Index, Value);
  ///

end;

{ TS7OptionItems.Notify }

procedure TS7OptionItems.Notify(Item : TCollectionItem; Action : TCollectionNotification);
begin
  inherited Notify(Item, Action);
  ///

  case Action of
    cnAdding: ;
    cnAdded: ;
    cnExtracting: ;
    cnExtracted: ;
    cnDeleting: ;
    cnRemoved: ;
  end;
end;

end.
