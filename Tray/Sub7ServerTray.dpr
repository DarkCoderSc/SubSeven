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

program Sub7ServerTray;

uses
  Vcl.Forms,
  Winapi.Windows,
  System.SysUtils,
  uFormMain in 'Units\uFormMain.pas' {FormMain},
  uFormEditConfig in 'Units\uFormEditConfig.pas' {FormEditConfiguration},
  uFormHashPassword in 'Units\uFormHashPassword.pas' {FormHashPassword},
  uFormWait in 'Units\uFormWait.pas' {FormWait},
  uFormAbout in 'Units\uFormAbout.pas' {FormAbout},
  Sub7.OpenSSL.Cert.Utils in '..\Shared\Sub7.OpenSSL.Cert.Utils.pas',
  Sub7.OpenSSL.TLS.Utils in '..\Shared\Sub7.OpenSSL.TLS.Utils.pas',
  Sub7.OpenSSL.Headers in '..\Shared\Sub7.OpenSSL.Headers.pas',
  Sub7.OpenSSL.TLS.Exceptions in '..\Shared\Sub7.OpenSSL.TLS.Exceptions.pas',
  Sub7.OpenSSL.TLS.Context in '..\Shared\Sub7.OpenSSL.TLS.Context.pas',
  XSuperObject in '..\Shared\XSuperObject.pas',
  XSuperJson in '..\Shared\XSuperJson.pas',
  Sub7.Core.Windows.Information in '..\Shared\Sub7.Core.Windows.Information.pas',
  Sub7.Core.FileSystem.Utils.Lock in '..\Shared\Sub7.Core.FileSystem.Utils.Lock.pas',
  Sub7.OpenSSL.Cert.Thread.Generate in '..\Shared\Sub7.OpenSSL.Cert.Thread.Generate.pas',
  Sub7.Core.Diagnostic in '..\Shared\Sub7.Core.Diagnostic.pas',
  Sub7.Core.Application.Env in '..\Shared\Sub7.Core.Application.Env.pas',
  Sub7.Core.Exceptions in '..\Shared\Sub7.Core.Exceptions.pas',
  Sub7.Core.Windows.Services.Structure in '..\Shared\Sub7.Core.Windows.Services.Structure.pas',
  Sub7.Core.Windows.Services.Service in '..\Shared\Sub7.Core.Windows.Services.Service.pas',
  Sub7.Core.Windows.Services.Notify in '..\Shared\Sub7.Core.Windows.Services.Notify.pas',
  Sub7.Core.Windows.Services.Register in '..\Shared\Sub7.Core.Windows.Services.Register.pas',
  Sub7.Core.Windows.Services.Utils in '..\Shared\Sub7.Core.Windows.Services.Utils.pas',
  Sub7.Core.IntervalThread in '..\Shared\Sub7.Core.IntervalThread.pas',
  Sub7.Core.Thread in '..\Shared\Sub7.Core.Thread.pas',
  Sub7.Core.Synchronization.CriticalSectionEx in '..\Shared\Sub7.Core.Synchronization.CriticalSectionEx.pas',
  Sub7.Core.Utils in '..\Shared\Sub7.Core.Utils.pas',
  Sub7.Core.Bundle in '..\Shared\Sub7.Core.Bundle.pas',
  Sub7.Core.OOP.Interfaces in '..\Shared\Sub7.Core.OOP.Interfaces.pas',
  Sub7.Core.Input.Validators in '..\Shared\Sub7.Core.Input.Validators.pas',
  Sub7.Service.Config in '..\Service\Units\Sub7.Service.Config.pas',
  Sub7.Core.FileSystem.Enum in '..\Shared\Sub7.Core.FileSystem.Enum.pas',
  Sub7.Core.Windows.PE.Version in '..\Shared\Sub7.Core.Windows.PE.Version.pas',
  Sub7.Core.FileSystem.Utils in '..\Shared\Sub7.Core.FileSystem.Utils.pas',
  Sub7.Core.Security.SecureDesktop in '..\Shared\Sub7.Core.Security.SecureDesktop.pas',
  Sub7.Core.UX.Utils in '..\Shared\Sub7.Core.UX.Utils.pas',
  Sub7.Core.Magic in '..\Shared\Sub7.Core.Magic.pas',
  Sub7.Core.Windows.User.Enum in '..\Shared\Sub7.Core.Windows.User.Enum.pas',
  Sub7.Core.Windows.Userland.Desktop in '..\Shared\Sub7.Core.Windows.Userland.Desktop.pas';

{$R *.res}
{$R version.res}
{$R strings.res}

begin
  IsMultiThread := True;

  { Prepare VCL}

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := False;

  { Avoid multiple tray instance}

  CreateMutex(nil, False, '{9C4FD531-540D-4C56-B903-7B0582146EFA}');
  if (GetLastError = ERROR_ALREADY_EXISTS) then begin
    MessageBox(0, 'SubSeven Legacy Service Controller is already running.', 'Runtime Error', MB_ICONHAND);
    Exit();
  end;
  ///

  { Check OpenSSL Library }
  try
    LoadOpenSSL();
  except
    on E : Exception do begin
      Application.MessageBox(PWideChar(E.Message), 'OpenSSL Library Exception', MB_ICONERROR);

      ExitProcess(1);
    end;
  end;

  { Finalize & Run VCL }

  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormEditConfiguration, FormEditConfiguration);
  Application.CreateForm(TFormWait, FormWait);
  Application.CreateForm(TFormAbout, FormAbout);
  //Application.CreateForm(TFormHashPassword, FormHashPassword);

  Application.Run;
end.
