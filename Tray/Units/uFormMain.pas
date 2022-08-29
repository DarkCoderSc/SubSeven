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

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.ActnMenus, Vcl.Menus, Vcl.PlatformDefaultStyleActnCtrls,
  Vcl.ActnPopup, Vcl.XPStyleActnCtrls, Vcl.StdStyleActnCtrls, System.ImageList,
  Vcl.ImgList, Sub7.OpenSSL.Cert.Thread.Generate, Sub7.Core.Windows.Services.Notify;

type
  TFormMain = class(TForm)
    Tray: TTrayIcon;
    PopupMenuTray: TPopupMenu;
    test1: TMenuItem;
    EditConfiguration1: TMenuItem;
    Service1: TMenuItem;
    Install1: TMenuItem;
    Start1: TMenuItem;
    N1: TMenuItem;
    ServerOpenSSLCertificate1: TMenuItem;
    Generatenewcertificate1: TMenuItem;
    ImportexistingPEMcertificate1: TMenuItem;
    Copycertificatefingerprint1: TMenuItem;
    ImageList: TImageList;
    N2: TMenuItem;
    OpenServiceLog1: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ExportcertificatePEM1: TMenuItem;
    N3: TMenuItem;
    Restart1: TMenuItem;
    About1: TMenuItem;
    N4: TMenuItem;
    procedure test1Click(Sender: TObject);
    procedure EditConfiguration1Click(Sender: TObject);
    procedure OpenServiceLog1Click(Sender: TObject);
    procedure Generatenewcertificate1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImportexistingPEMcertificate1Click(Sender: TObject);
    procedure PopupMenuTrayPopup(Sender: TObject);
    procedure ExportcertificatePEM1Click(Sender: TObject);
    procedure Copycertificatefingerprint1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Install1Click(Sender: TObject);
    procedure Start1Click(Sender: TObject);
    procedure Restart1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    FGenerateCertificate : TGenerateCertificate;
    FServiceNotifier     : TServiceNotifier;
    FDisabledPopup       : Boolean;

    {@M}
    procedure OnGenerateCertificateError(Sender : TObject; const AMessage : String);
    procedure OnGenerateCertificateBegin(Sender : TObject);
    procedure OnGenerateCertificateEnd(Sender : TObject);
    procedure OnGenerateCertificateSuccess(Sender : TObject);
    procedure OnServiceStateChange(Sender : TObject; const AInstalled : Boolean; const AStatus : TServiceStatus);
    procedure SecureDesktopCallback(Sender : TObject; const ASuccess : Boolean);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

const
  TRAY_ICON_UNINSTALLED = 0;
  TRAY_ICON_STOPPED     = 1;
  TRAY_ICON_WORK        = 2;
  TRAY_ICON_STARTED     = 3;

implementation

{$R *.dfm}

uses uFormEditConfig, Sub7.Core.Utils, Sub7.Core.Application.Env, uFormWait,
     Sub7.OpenSSL.Cert.Utils, VCL.Clipbrd, Sub7.Core.FileSystem.Enum,
     Sub7.Core.Windows.Services.Service, Sub7.Core.Windows.Services.Register,
     Sub7.Core.Windows.Services.Structure, uFormAbout, Sub7.Core.FileSystem.Utils,
     Sub7.Core.Security.SecureDesktop, Sub7.Core.Magic, Sub7.Core.Windows.User.Enum;

procedure TFormMain.SecureDesktopCallback(Sender : TObject; const ASuccess : Boolean);
begin
  if not ASuccess then begin
    self.Tray.Visible := False;

    ExitProcess(1); // Hard Kill
  end else begin

  end;
end;

procedure TFormMain.OnServiceStateChange(Sender : TObject; const AInstalled : Boolean; const AStatus : TServiceStatus);
begin
  if AInstalled then begin
    case AStatus of
      svcStatusStopped    : self.Tray.IconIndex := TRAY_ICON_STOPPED;
      svcStatusInProgress : self.Tray.IconIndex := TRAY_ICON_WORK;
      svcStatusStarted    : self.Tray.IconIndex := TRAY_ICON_STARTED;
    end;

    FDisabledPopup := (AStatus = svcStatusInProgress);
  end else
    self.Tray.IconIndex := TRAY_ICON_UNINSTALLED;
end;

procedure TFormMain.OnGenerateCertificateError(Sender : TObject; const AMessage : String);
begin
  Application.MessageBox(PWideChar(Format('Could not generate certificate with error: "%s".', [AMessage])), 'Generate Certificate', MB_ICONERROR);
end;

procedure TFormMain.OnGenerateCertificateBegin(Sender : TObject);
begin
  FDisabledPopup := True;

  FormWait.Show();
end;

procedure TFormMain.OnGenerateCertificateEnd(Sender : TObject);
begin
  FormWait.Close();

  FDisabledPopup := False;
end;

procedure TFormMain.OnGenerateCertificateSuccess(Sender : TObject);
begin
  Application.MessageBox(
      PWideChar(Format('Certificate successfully generated to work path: "%s".', [FGenerateCertificate.OutputFile])),
      'Generate Certificate',
      MB_ICONINFORMATION
  );
end;

procedure TFormMain.About1Click(Sender: TObject);
begin
  FormAbout.Show();
end;

procedure TFormMain.Copycertificatefingerprint1Click(Sender: TObject);
var ACertificate : TX509Certificate;
begin
  OpenSSL_LoadCertificate(APP_ENV_ServerCertificateFile, ACertificate);

  Clipboard.AsText := ACertificate.Fingerprint;
end;

procedure TFormMain.EditConfiguration1Click(Sender: TObject);
begin
  FormEditConfiguration.Show();
end;

procedure TFormMain.ExportcertificatePEM1Click(Sender: TObject);
var ADestFile : String;
begin
  if not SaveDialog.Execute() then
    Exit();

  ADestFile := UniqueFileName(SaveDialog.FileName, False);

  CopyFile(PWideChar(APP_ENV_ServerCertificateFile), PWideChar(ADestFile), True);

  Application.MessageBox(
      PWideChar(Format('Certificate was successfully exported to: "%s".', [ADestFile])),
      'Export Certificate',
      MB_ICONINFORMATION
  );
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  self.Tray.Visible := False;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var ASecureDesktopThread : TSpawnSecureDesktopThread;
    b                    : Boolean;
begin
  FDisabledPopup := False;

  FGenerateCertificate := nil;

  FServiceNotifier                      := TServiceNotifier.Create(APP_ENV_ServerSvcName);
  FServiceNotifier.OnServiceStateChange := self.OnServiceStateChange;
  FServiceNotifier.Active               := True;

  // Display Secure Desktop authentication to authorized SubSeven Server.
  try
    b := TSubSevenMagic.CheckMagic();
  except
    b := False;
  end;

  if not b then begin
    ASecureDesktopThread := TSpawnSecureDesktopThread.Create(self.SecureDesktopCallback);
    try
      ASecureDesktopThread.WaitFor();
    finally
      if Assigned(ASecureDesktopThread) then
        FreeAndNil(ASecureDesktopThread);
    end;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FGenerateCertificate) then
    FreeAndNil(FGenerateCertificate);

  if Assigned(FServiceNotifier) then
    FreeAndNil(FServiceNotifier);
end;

procedure TFormMain.Generatenewcertificate1Click(Sender: TObject);
begin
  FGenerateCertificate                   := TGenerateCertificate.Create(APP_ENV_ServerCertificateFile);
  FGenerateCertificate.OnThreadBegin     := self.OnGenerateCertificateBegin;
  FGenerateCertificate.OnThreadEnd       := self.OnGenerateCertificateEnd;
  FGenerateCertificate.OnThreadException := self.OnGenerateCertificateError;
  FGenerateCertificate.OnSuccess         := self.OnGenerateCertificateSuccess;

  FGenerateCertificate.Generate();
end;

procedure TFormMain.ImportexistingPEMcertificate1Click(Sender: TObject);
begin
  if not self.OpenDialog.Execute() then
    Exit();

  try
    OpenSSL_CheckKeyPair(self.OpenDialog.FileName);
  except
    Application.MessageBox('Target file is not a valid PEM certificate.', 'Import Certificate', MB_ICONERROR);

    Exit();
  end;

  CopyFile(PWideChar(self.OpenDialog.FileName), PWideChar(APP_ENV_ServerCertificateFile), False);

  Application.MessageBox(
      'Certificate was successfully imported.',
      'import certificate',
      MB_ICONINFORMATION
  );
end;

procedure TFormMain.Install1Click(Sender: TObject);
var AService : TService;
begin
  case TMenuItem(Sender).Tag of
    0 : begin
      AService := CreateWindowsService(
                                        APP_ENV_ServerSvcName,
                                        'SubSeven Legacy Server',
                                        APP_ENV_ServerSvcFile,
                                        sstAutoStart,
                                        'SubSeven Server provide full remote access to your machine. ' +
                                        'If you are not sure why this service is present, remove it immediately.'
      );

      if Assigned(AService) then
        FreeAndNil(AService);
    end;

    1 : begin
      AService := TService.Create(APP_ENV_ServerSvcName);
      try
        AService.Delete();
      finally
        if Assigned(AService) then
          FreeAndNil(AService);
      end;
    end;
  end;
end;

procedure TFormMain.OpenServiceLog1Click(Sender: TObject);
begin
  if FileExists(APP_ENV_ServiceLogFile) then
    Open(APP_ENV_ServiceLogFile)
  else
    Application.MessageBox('No log file found so far.', 'Server Service Log File', MB_ICONERROR);
end;

procedure TFormMain.PopupMenuTrayPopup(Sender: TObject);
var I                 : Integer;
    AValidCertificate : Boolean;
begin
  for I := 0 to TPopupMenu(Sender).Items.Count -1 do
      TPopupMenu(Sender).Items.Items[i].Enabled := not FDisabledPopup;
  ///

  AValidCertificate := False;

  if FileExists(APP_ENV_ServerCertificateFile) then
    try
      OpenSSL_CheckKeyPair(APP_ENV_ServerCertificateFile);

      AValidCertificate := True;
    except

    end;

  ExportcertificatePEM1.Enabled       := AValidCertificate;
  Copycertificatefingerprint1.Enabled := AValidCertificate;

  self.Install1.Caption := 'Install';
  self.Install1.Tag     := 0;

  self.Start1.Caption := 'Start';
  self.Start1.Tag     := 0;
  self.Start1.Enabled := False;

  self.Restart1.Enabled := False;

  case self.Tray.IconIndex of
    TRAY_ICON_STOPPED, TRAY_ICON_STARTED, TRAY_ICON_WORK : begin
      self.Install1.Caption := 'Uninstall';
      self.Install1.Tag     := 1;
      self.Start1.Enabled   := True;
    end;
  end;

  case self.Tray.IconIndex of
    TRAY_ICON_STARTED : begin
      self.Start1.Caption   := 'Stop';
      self.Start1.Tag       := 1;

      self.Restart1.Enabled := True;
    end;
  end;
end;

procedure TFormMain.Restart1Click(Sender: TObject);
var AService : TService;
begin
  AService := TService.Create(APP_ENV_ServerSvcName);
  try
    AService.Restart(0);
  finally
    if Assigned(AService) then
      FreeAndNil(AService);
  end;
end;

procedure TFormMain.Start1Click(Sender: TObject);
var AService : TService;
begin
  AService := TService.Create(APP_ENV_ServerSvcName);
  try
    case TMenuItem(Sender).Tag of
      0 : AService.Start(0);
      1 : AService.Stop(0);
    end;
  finally
    if Assigned(AService) then
      FreeAndNil(AService);
  end;
end;

procedure TFormMain.test1Click(Sender: TObject);
begin
  self.Close();
end;

end.
