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

unit uFormClientCertificate;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.SubSevenForm, Sub7.Viewer.VCL.CaptionBar, S7Panel,
  Vcl.StdCtrls, Sub7.Viewer.VCL.Button, S7Gauge, Sub7.OpenSSL.Cert.Thread.Generate,
  Sub7.Core.FileSystem.Utils.Lock, ___S7BaseForm;

type
  TFormClientCertificate = class(TS7BaseForm)
    CaptionBar: TS7CaptionBar;
    SubSevenForms: TS7Form;
    PanelClient: TS7Panel;
    PanelButtons: TS7Panel;
    ButtonCopyFingerprint: TS7Button;
    ButtonExport: TS7Button;
    ButtonGenerate: TS7Button;
    ButtonImport: TS7Button;
    GaugeMarquee: TS7Gauge;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonGenerateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonImportClick(Sender: TObject);
    procedure ButtonExportClick(Sender: TObject);
    procedure ButtonCopyFingerprintClick(Sender: TObject);
  private
    FGenerateCertificate : TGenerateCertificate;
    FCertificateLock     : TFileLock;


    {@M}
    procedure DoResize();

    procedure OnGenerateCertificateError(Sender : TObject; const AMessage : String);
    procedure OnGenerateCertificateBegin(Sender : TObject);
    procedure OnGenerateCertificateEnd(Sender : TObject);
    procedure OnGenerateCertificateSuccess(Sender : TObject);

    procedure SetButtonBusy(const ABusy : Boolean);

    procedure CheckCertificate();
    function ConfirmCertificateFlush() : Boolean;
  protected

  public
    { Public declarations }
  end;

var
  FormClientCertificate: TFormClientCertificate;

implementation

uses uFormMain, Sub7.Core.Application.Env, Sub7.Core.Exceptions, Sub7.OpenSSL.Cert.Utils,
     Sub7.Core.FileSystem.Utils, VCL.Clipbrd;

{$R *.dfm}

function TFormClientCertificate.ConfirmCertificateFlush() : Boolean;
begin
  if not Assigned(FCertificateLock) then
    Exit();
  ///

  result := FormMain.MessageBox.MessageBox(
                self,
                'This action will replace definitively an existing certificate. You could lose connectivity to servers' +
                ' that accept current certificate for authentication. Overwrite anyway?',
                'certificate replace',
                MB_ICONQUESTION + MB_YESNO
            ) = ID_YES
end;

procedure TFormClientCertificate.ButtonImportClick(Sender: TObject);
begin
  if not self.ConfirmCertificateFlush() then
    Exit();
  ///

  if not self.OpenDialog.Execute() then
    Exit();

  try
    OpenSSL_CheckKeyPair(self.OpenDialog.FileName);
  except
    FormMain.MessageBox.MessageBox(self, 'Target file is not a valid PEM certificate.', 'import certificate', MB_ICONERROR);

    Exit();
  end;

  CopyFile(PWideChar(self.OpenDialog.FileName), PWideChar(APP_ENV_ClientCertificateFile), False);

  self.CheckCertificate();

  FormMain.MessageBox.MessageBox(
      self,
      'Certificate was successfully imported.',
      'import certificate',
      MB_USERICON
  );
end;

procedure TFormClientCertificate.CheckCertificate();
var ACertFile : String;
begin
  if FileExists(APP_ENV_ClientCertificateFile) then begin
    FCertificateLock := TFileLock.Create(APP_ENV_ClientCertificateFile, True);

    FCertificateLock.Lock();
  end;

  ///
  self.ButtonExport.Enabled          := Assigned(FCertificateLock);
  self.ButtonCopyFingerprint.Enabled := Assigned(FCertificateLock);
end;

procedure TFormClientCertificate.SetButtonBusy(const ABusy : Boolean);
begin
  self.ButtonGenerate.Busy        := ABusy;
  self.ButtonImport.Busy          := ABusy;
  self.ButtonExport.Busy          := ABusy;
  self.ButtonCopyFingerprint.Busy := ABusy;
end;

procedure TFormClientCertificate.OnGenerateCertificateError(Sender : TObject; const AMessage : String);
begin
  FormMain.MessageBox.MessageBox(self, Format('Could not generate certificate with error: "%s".', [AMessage]), 'generate certificate', MB_ICONERROR);
end;

procedure TFormClientCertificate.OnGenerateCertificateBegin(Sender : TObject);
begin
  self.SetButtonBusy(True);

  self.GaugeMarquee.Mode    := gmMarquee;
  self.GaugeMarquee.Visible := True;
end;

procedure TFormClientCertificate.OnGenerateCertificateEnd(Sender : TObject);
begin
  self.SetButtonBusy(False);

  self.GaugeMarquee.Mode    := gmProgressBar;
  self.GaugeMarquee.Visible := False;
end;

procedure TFormClientCertificate.OnGenerateCertificateSuccess(Sender : TObject);
begin
  self.CheckCertificate();
  ///

  FormMain.MessageBox.MessageBox(
      self,
      Format('Certificate successfully generated to work path: "%s".', [FGenerateCertificate.OutputFile]),
      'generate certificate',
      MB_USERICON
  );
end;

procedure TFormClientCertificate.ButtonCopyFingerprintClick(Sender: TObject);
var ACertificate : TX509Certificate;
begin
  OpenSSL_LoadCertificate(APP_ENV_ClientCertificateFile, ACertificate);

  Clipboard.AsText := ACertificate.Fingerprint;
end;

procedure TFormClientCertificate.ButtonExportClick(Sender: TObject);
var ADestFile : String;
begin
  if not SaveDialog.Execute() then
    Exit();

  ADestFile := UniqueFileName(SaveDialog.FileName, False);

  CopyFile(PWideChar(APP_ENV_ClientCertificateFile), PWideChar(ADestFile), True);

  FormMain.MessageBox.MessageBox(
      self,
      Format('Certificate was successfully exported to: "%s".', [ADestFile]),
      'export certificate',
      MB_USERICON
  );
end;

procedure TFormClientCertificate.ButtonGenerateClick(Sender: TObject);
begin
  if not self.ConfirmCertificateFlush() then
    Exit();
  ///

  if Assigned(FCertificateLock) then
    FreeAndNil(FCertificateLock);
  ///

  FGenerateCertificate                   := TGenerateCertificate.Create(APP_ENV_ClientCertificateFile);
  FGenerateCertificate.OnThreadBegin     := self.OnGenerateCertificateBegin;
  FGenerateCertificate.OnThreadEnd       := self.OnGenerateCertificateEnd;
  FGenerateCertificate.OnThreadException := self.OnGenerateCertificateError;
  FGenerateCertificate.OnSuccess         := self.OnGenerateCertificateSuccess;

  FGenerateCertificate.Generate();
end;

procedure TFormClientCertificate.DoResize();
begin
  PanelButtons.Top  := ((PanelClient.Height div 2) - (PanelButtons.Height div 2));
  PanelButtons.Left := (PanelClient.Width div 2) - (PanelButtons.Width div 2);
end;

procedure TFormClientCertificate.FormCreate(Sender: TObject);
begin
  FGenerateCertificate := nil;
  FCertificateLock     := nil;

  ///
  self.CheckCertificate();
end;

procedure TFormClientCertificate.FormDestroy(Sender: TObject);
begin
  if Assigned(FGenerateCertificate) then
    FreeAndNil(FGenerateCertificate);
end;

procedure TFormClientCertificate.FormResize(Sender: TObject);
begin
  DoResize();
end;

procedure TFormClientCertificate.FormShow(Sender: TObject);
begin
  DoResize();
end;

end.
