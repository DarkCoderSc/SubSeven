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

unit uFormEditConfig;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ExtCtrls,
  SynEditHighlighter, SynHighlighterJSON, SynEdit, Sub7.Core.FileSystem.Utils.Lock,
  Vcl.StdCtrls, Vcl.ComCtrls, Sub7.Core.VCL.MultiPanel;

type
  TFormEditConfiguration = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Save1: TMenuItem;
    ools1: TMenuItem;
    HashPassword1: TMenuItem;
    SynConfig: TSynEdit;
    SynJSONSyn1: TSynJSONSyn;
    TopSeparator: TShape;
    Help1: TMenuItem;
    Openonlinedocumentation1: TMenuItem;
    Shape1: TShape;
    PanelMessages: TPanel;
    LabelMessages: TLabel;
    MultiPanel: TS7CMultiPanel;
    PanelRichEdit: TPanel;
    Shape2: TShape;
    RichEditResult: TRichEdit;
    N1: TMenuItem;
    ResetConfig1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Save1Click(Sender: TObject);
    procedure HashPassword1Click(Sender: TObject);
    procedure SynConfigChange(Sender: TObject);
    procedure SynConfigSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure ResetConfig1Click(Sender: TObject);
    procedure Openonlinedocumentation1Click(Sender: TObject);
  private
    FConfigLock      : TFileLock;
    FOldCaption      : String;
    FConfigErrorLine : Integer;
    FConfigTemplate  : String;
    FHasChanged      : Boolean;

    {@M}
    function CheckAndFormatJson(const AJsonString : String) : String;
    procedure SetConfigErrorLine(const AValue : Integer);
    procedure LogAction(const AText : String);
    procedure SetHasChanged(const AValue : Boolean);
  protected
    {@M}
    procedure CreateParams(var AParams: TCreateParams); override;
  public
    { Public declarations }
  end;

var
  FormEditConfiguration: TFormEditConfiguration;

implementation

uses Sub7.Core.Application.Env, Sub7.Core.Exceptions, XSuperObject, System.JSON,
     Sub7.Service.Config, uFormMain, uFormHashPassword, Sub7.Core.Utils,
     Sub7.Core.FileSystem.Utils;

{$R *.dfm}

procedure TFormEditConfiguration.SetHasChanged(const AValue : Boolean);
begin
  if FHasChanged = AValue then
    Exit();
  ///

  FHasChanged := AValue;

  if FHasChanged then
    self.Caption := FOldCaption + '*'
  else
    self.Caption := FOldCaption;
end;

procedure TFormEditConfiguration.LogAction(const AText : String);
begin
  self.RichEditResult.Lines.Add(Format('%s %s', [DateTimeToStr(Now), AText]));

  SendMessage(RichEditResult.Handle, WM_VSCROLL, SB_LINEDOWN, 0);
end;

procedure TFormEditConfiguration.Openonlinedocumentation1Click(Sender: TObject);
begin
  Open('https://www.sub7crew.org/documentation/configure-subseven-server-service');
end;

procedure TFormEditConfiguration.ResetConfig1Click(Sender: TObject);
begin
  if Application.MessageBox('Current configuration will be definitively lost. Are you sure?', 'Reset Config', MB_ICONQUESTION + MB_YESNO) = ID_NO then
    Exit();
  ///

  self.SynConfig.Text := FConfigTemplate;

  self.SetHasChanged(True);
end;

procedure TFormEditConfiguration.SetConfigErrorLine(const AValue : Integer);
begin
  FConfigErrorLine := AValue;

  self.SynConfig.Invalidate();
end;

function TFormEditConfiguration.CheckAndFormatJson(const AJsonString : String) : String;
var AJsonValue : TJsonValue;
begin
  // Check and format JSON
  SetConfigErrorLine(-1); // Reset
  try
    AJsonValue := TJsonObject.ParseJSONValue(AJsonString, True, True);
    try
      result := AJsonValue.Format();
    finally
      if Assigned(AJsonValue) then
        FreeAndNil(AJsonValue);
    end;
  except
    on E : EJSONParseException do begin
      SetConfigErrorLine(E.Line);

      raise;
    end;

    else
      raise;
  end;
end;

procedure TFormEditConfiguration.CreateParams(var AParams: TCreateParams);
begin
  inherited;
  ///

  AParams.ExStyle := AParams.ExStyle or WS_EX_APPWINDOW;

  AParams.WndParent := FormMain.Handle;
end;

procedure TFormEditConfiguration.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(FConfigLock) then
    FreeAndNil(FConfigLock);
end;

procedure TFormEditConfiguration.FormCreate(Sender: TObject);
begin
  FConfigLock      := nil;
  FOldCaption      := self.Caption;
  FConfigErrorLine := -1;
  FHasChanged      := False;

  try
    FConfigTemplate := ReadStringResource('config_template', MAKEINTRESOURCE(10));
  except
    FConfigTemplate := '{}';
  end;
end;

procedure TFormEditConfiguration.FormDestroy(Sender: TObject);
begin
  if Assigned(FConfigLock) then
    FreeAndNil(FConfigLock);
end;

procedure TFormEditConfiguration.FormShow(Sender: TObject);
var AFileContent : String;
    AJsonBuffer  : TStringBuilder;
begin
  SynConfig.Clear();
  ///

  // Prepare config file usage
  try
    AFileContent := '';

    if not FileExists(APP_ENV_ConfigFile) then
      TouchFile(APP_ENV_ConfigFile);

    // Lock config file in read only
    if Assigned(FConfigLock) then
      FreeAndNil(FConfigLock);

    FConfigLock := TFileLock.Create(APP_ENV_ConfigFile, True);
    FConfigLock.Lock();

    // Display config file content
    ReadTextFile(APP_ENV_ConfigFile, AFileContent);
    if Trim(AFileContent) = '' then begin
      AFileContent := FConfigTemplate;

      SetHasChanged(True);
    end;

    ///
    SynConfig.Text := self.CheckAndFormatJson(AFileContent);
  except
    on E : Exception do
      self.LogAction(Format('Could not initalize env with error: "%s".', [E.Message]));
  end;
end;

procedure TFormEditConfiguration.HashPassword1Click(Sender: TObject);
var AForm : TFormHashPassword;
begin
  AForm := TFormHashPassword.Create(self);
  try
    AForm.ShowModal();
  finally
    if Assigned(AForm) then
      FreeAndNil(AForm);
  end;
end;

procedure TFormEditConfiguration.Save1Click(Sender: TObject);
var AJsonString : String;
    AConfig     : TServiceConfig;
    AJsonConfig : ISuperObject;
begin
  if not FHasChanged then
    Exit();
  try
    AJsonString := self.CheckAndFormatJson(self.SynConfig.Text);

    ///

    AConfig := TServiceConfig.Create();
    try
      AJsonConfig := TSuperObject.Create(AJsonString);

      // Just to check if configuration meets defined criterias
      AConfig.DeSerialize(AJsonConfig);
    finally
      if Assigned(AConfig) then
        FreeAndNil(AConfig);
    end;
  except
    on E : Exception do begin
      self.LogAction(E.Message);

      Exit();
    end;
  end;

  ///

  try
    if Assigned(FConfigLock) then
      FConfigLock.Unlock();
    try
      WriteTextToFile(AJsonString, APP_ENV_ConfigFile);
    finally
      if Assigned(FConfigLock) then
        FConfigLock.Lock();
    end;

    self.LogAction('Configuration file successfully saved.');

    ///
    SetHasChanged(False);
  except
    on E : Exception do
      self.LogAction(Format('Could not save config to disk with error: "%s".', [E.Message]));
  end;
end;

procedure TFormEditConfiguration.SynConfigChange(Sender: TObject);
begin
  if not Assigned(self) then
    Exit();

  SetHasChanged(True);

  if FConfigErrorLine > -1 then
    SetConfigErrorLine(-1);

//  if (self.SynConfig.CaretXY.Line = FConfigErrorLine) and (FConfigErrorLine > -1) then begin
//    self.CheckAndFormatJson(self.SynConfig.Text);
//  end;
end;

procedure TFormEditConfiguration.SynConfigSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  if (FConfigErrorLine > -1) and (Line = FConfigErrorLine) then begin
    Special := True;

    BG := RGB(253, 153, 156);

    FG := RGB(252, 37, 49);
  end;
end;

end.
