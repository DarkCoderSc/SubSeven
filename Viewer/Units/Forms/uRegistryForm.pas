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

unit uRegistryForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ___S7ControlWindow, VirtualTrees, Vcl.ExtCtrls,
  Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.CaptionBar, Sub7.Viewer.VCL.CheckBox, S7ImageButton, S7Panel,
  Vcl.StdCtrls, S7Edit;

type
  TKeyTreeData = record
    Hive : HKEY;
  end;
  PKeyTreeData = ^TKeyTreeData;

  TFormRegistry = class(TS7ControlWindow)
    SubSevenForms: TS7Form;
    CaptionBar: TS7CaptionBar;
    PanelHeader: TS7Panel;
    ButtonClear: TS7ImageButton;
    ButtonDeleteException: TS7ImageButton;
    ButtonOptions: TS7ImageButton;
    PanelClient: TS7Panel;
    PanelLocation: TS7Panel;
    EditPath: TS7Edit;
  private
    {@M}
   // procedure InitializeKeyHives();
  public

  end;

var
  FormRegistry: TFormRegistry;

implementation

uses uFormMain;

{$R *.dfm}

//constructor TRegistryForm.Create(AOwner : TComponent; const ASocket : TS7Socket);
//begin
//  inherited;
//  ///
//
//  self.InitializeKeyHives();
//end;

//procedure TFormRegistry.InitializeKeyHives();
//
//    procedure AddHive(const AHive : HKEY);
//    var ANode : PVirtualNode;
//        pData : PKeyTreeData;
//    begin
//      ANode := VSTTree.AddChild(nil);
//      pData := ANode.GetData();
//
//      pData^.Hive := AHive;
//    end;
//
//begin
//  VSTTree.BeginUpdate();
//  try
//    AddHive(HKEY_CLASSES_ROOT);
//    AddHive(HKEY_CURRENT_USER);
//    AddHive(HKEY_LOCAL_MACHINE);
//    AddHive(HKEY_USERS);
//    AddHive(HKEY_PERFORMANCE_DATA);
//    AddHive(HKEY_CURRENT_CONFIG);
//    AddHive(HKEY_DYN_DATA);
//  finally
//    VSTTree.EndUpdate();
//  end;
//end;

//procedure TFormRegistry.VSTTreeGetText(Sender: TBaseVirtualTree;
//  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
//  var CellText: string);
//  var pData : PKeyTreeData;
//begin
//  pData := Node.GetData;
//  ///
//
//  if column <> 0 then
//    Exit();
//
//  case pData^.Hive of
//    HKEY_CLASSES_ROOT     : CellText := 'HKEY_CLASSES_ROOT';
//    HKEY_CURRENT_USER     : CellText := 'HKEY_CURRENT_USER';
//    HKEY_LOCAL_MACHINE    : CellText := 'HKEY_LOCAL_MACHINE';
//    HKEY_USERS            : CellText := 'HKEY_USERS';
//    HKEY_PERFORMANCE_DATA : CellText := 'HKEY_PERFORMANCE_DATA';
//    HKEY_CURRENT_CONFIG   : CellText := 'HKEY_CURRENT_CONFIG';
//    HKEY_DYN_DATA         : CellText := 'HKEY_DYN_DATA';
//  end;
//end;

end.
