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

program Sub7Viewer;

uses
  Vcl.Forms,
  System.SysUtils,
  Winapi.Windows,
  uFormMain in 'Units\Forms\uFormMain.pas' {FormMain},
  uFormPassword in 'Units\Forms\uFormPassword.pas' {FormPassword},
  uFormFileManager in 'Units\Forms\uFormFileManager.pas' {FormFileManager},
  uFormSockets in 'Units\Forms\uFormSockets.pas' {FormSockets},
  uFormAbout in 'Units\Forms\uFormAbout.pas' {FormAbout},
  uFormExceptions in 'Units\Forms\uFormExceptions.pas' {FormExceptions},
  uFormAddressBook in 'Units\Forms\uFormAddressBook.pas' {FormAddressBook},
  uFormAddContact in 'Units\Forms\uFormAddContact.pas' {FormAddContact},
  uRegistryForm in 'Units\Forms\uRegistryForm.pas' {FormRegistry},
  uFormQueue in 'Units\Forms\uFormQueue.pas' {FormQueue},
  uFormTransfer in 'Units\Forms\uFormTransfer.pas' {FormTransfer},
  uFormRun in 'Units\Forms\uFormRun.pas' {FormRun},
  uFormRemoteShell in 'Units\Forms\uFormRemoteShell.pas' {FormRemoteShell},
  uFormClientCertificate in 'Units\Forms\uFormClientCertificate.pas' {FormClientCertificate},
  uFormServerHive in 'Units\Forms\uFormServerHive.pas' {FormServerHive},
  ___S7DockFrame in 'Units\Frames\___Frames\___S7DockFrame.pas' {S7FrameDock: TFrame},
  ___S7Frame in 'Units\Frames\___Frames\___S7Frame.pas' {S7Frame: TFrame},
  uFrameProcess in 'Units\Frames\uFrameProcess.pas' {FrameProcess: TFrame},
  uFrameSystemInformationHook in 'Units\Frames\uFrameSystemInformationHook.pas' {FrameSystemInformationHook: TFrame},
  uFrameShell in 'Units\Frames\uFrameShell.pas' {FrameShell: TFrame},
  uFrameRunAsGroup in 'Units\Frames\uFrameRunAsGroup.pas' {FrameRunAsGroup: TFrame},
  uFrameOpen in 'Units\Frames\uFrameOpen.pas' {FrameOpen: TFrame},
  uFrameComboSessions in 'Units\Frames\uFrameComboSessions.pas' {FrameComboSessions: TFrame},
  uFrameComboUser in 'Units\Frames\uFrameComboUser.pas' {FrameComboUser: TFrame},
  ___S7ControlWindow in 'Units\Forms\___Forms\___S7ControlWindow.pas',
  ___S7DockWindow in 'Units\Forms\___Forms\___S7DockWindow.pas',
  ___S7MessagedWindow in 'Units\Forms\___Forms\___S7MessagedWindow.pas',
  ___S7ContextWindow in 'Units\Forms\___Forms\___S7ContextWindow.pas',
  Sub7.Core.Protocol in '..\Shared\Sub7.Core.Protocol.pas',
  XSuperObject in '..\Shared\XSuperObject.pas',
  XSuperJson in '..\Shared\XSuperJson.pas',
  Sub7.Core.Diagnostic in '..\Shared\Sub7.Core.Diagnostic.pas',
  Sub7.Core.Crypto.RC4 in '..\Shared\Sub7.Core.Crypto.RC4.pas',
  Sub7.Core.Crypto.CRC32 in '..\Shared\Sub7.Core.Crypto.CRC32.pas',
  Sub7.Core.FileSystem.Drives.Enum in '..\Shared\Sub7.Core.FileSystem.Drives.Enum.pas',
  Sub7.Core.FileSystem.Enum in '..\Shared\Sub7.Core.FileSystem.Enum.pas',
  Sub7.Core.FileSystem.Utils in '..\Shared\Sub7.Core.FileSystem.Utils.pas',
  Sub7.Core.Windows.Information in '..\Shared\Sub7.Core.Windows.Information.pas',
  Sub7.Core.Bundle in '..\Shared\Sub7.Core.Bundle.pas',
  Sub7.Thread.Net.Client.Transfer.Upload in 'Units\Sub7.Thread.Net.Client.Transfer.Upload.pas',
  Sub7.Thread.Net.Client.Transfer.Download in 'Units\Sub7.Thread.Net.Client.Transfer.Download.pas',
  Sub7.Thread.Net.Client.Transfer in 'Units\Sub7.Thread.Net.Client.Transfer.pas',
  Sub7.Core.Types in '..\Shared\Sub7.Core.Types.pas',
  Sub7.Core.OOP.Interfaces in '..\Shared\Sub7.Core.OOP.Interfaces.pas',
  Sub7.Core.Windows.Process.Enum in '..\Shared\Sub7.Core.Windows.Process.Enum.pas',
  Sub7.Core.Windows.Sessions.Enum in '..\Shared\Sub7.Core.Windows.Sessions.Enum.pas',
  Sub7.Core.Input.Validators in '..\Shared\Sub7.Core.Input.Validators.pas',
  Sub7.Core.Messages.Listener in '..\Shared\Sub7.Core.Messages.Listener.pas',
  Sub7.Viewer.Messages in 'Units\Sub7.Viewer.Messages.pas',
  Sub7.Core.Windows.Process in '..\Shared\Sub7.Core.Windows.Process.pas',
  Sub7.Core.Shell.Emulator in '..\Shared\Sub7.Core.Shell.Emulator.pas',
  Sub7.Core.Shell.Event in '..\Shared\Sub7.Core.Shell.Event.pas',
  Sub7.Core.Shell.Thread.Instance in '..\Shared\Sub7.Core.Shell.Thread.Instance.pas',
  Sub7.Core.Windows.User.Enum in '..\Shared\Sub7.Core.Windows.User.Enum.pas',
  Sub7.Core.Thread in '..\Shared\Sub7.Core.Thread.pas',
  Sub7.OpenSSL.TLS.Context in '..\Shared\Sub7.OpenSSL.TLS.Context.pas',
  Sub7.OpenSSL.TLS.Socket in '..\Shared\Sub7.OpenSSL.TLS.Socket.pas',
  Sub7.OpenSSL.Headers in '..\Shared\Sub7.OpenSSL.Headers.pas',
  Sub7.OpenSSL.TLS.Exceptions in '..\Shared\Sub7.OpenSSL.TLS.Exceptions.pas',
  Sub7.OpenSSL.TLS.IOHandler in '..\Shared\Sub7.OpenSSL.TLS.IOHandler.pas',
  Sub7.TLS.IOHandler in '..\Shared\Sub7.TLS.IOHandler.pas',
  Sub7.Core.Synchronization.CriticalSectionEx in '..\Shared\Sub7.Core.Synchronization.CriticalSectionEx.pas',
  Sub7.Thread.Net.Client.Base in 'Units\Sub7.Thread.Net.Client.Base.pas',
  Sub7.Thread.Net.Client.Session in 'Units\Sub7.Thread.Net.Client.Session.pas',
  Sub7.Thread.Net.Client.NonBlockingCmd in 'Units\Sub7.Thread.Net.Client.NonBlockingCmd.pas',
  Sub7.Thread.Net.Client.Session.Cmd in 'Units\Sub7.Thread.Net.Client.Session.Cmd.pas',
  Sub7.Thread.Net.Client.SystemInformationHook in 'Units\Sub7.Thread.Net.Client.SystemInformationHook.pas',
  Sub7.Net.Client.Context in 'Units\Sub7.Net.Client.Context.pas',
  Sub7.Viewer.Types in 'Units\Sub7.Viewer.Types.pas',
  Sub7.Thread.Net.Client.RemoteShell in 'Units\Sub7.Thread.Net.Client.RemoteShell.pas',
  Sub7.Core.Thread.Watcher in '..\Shared\Sub7.Core.Thread.Watcher.pas',
  Sub7.Core.IntervalThread in '..\Shared\Sub7.Core.IntervalThread.pas',
  Sub7.Core.Windows in '..\Shared\Sub7.Core.Windows.pas',
  Sub7.OpenSSL.TLS.Utils in '..\Shared\Sub7.OpenSSL.TLS.Utils.pas',
  Sub7.OpenSSL.Cert.Utils in '..\Shared\Sub7.OpenSSL.Cert.Utils.pas',
  bass in '..\Assets\Libs\bass\bass.pas',
  Sub7.OpenSSL.Cert.Thread.Generate in '..\Shared\Sub7.OpenSSL.Cert.Thread.Generate.pas',
  Sub7.Core.FileSystem.Utils.Lock in '..\Shared\Sub7.Core.FileSystem.Utils.Lock.pas',
  Sub7.Core.Application.Env in '..\Shared\Sub7.Core.Application.Env.pas',
  Sub7.Core.Exceptions in '..\Shared\Sub7.Core.Exceptions.pas',
  Sub7.Core.Utils in '..\Shared\Sub7.Core.Utils.pas',
  Sub7.Core.Windows.Services.Enum in '..\Shared\Sub7.Core.Windows.Services.Enum.pas',
  Sub7.Core.Windows.Services.Service in '..\Shared\Sub7.Core.Windows.Services.Service.pas',
  Sub7.Core.Windows.Services.Structure in '..\Shared\Sub7.Core.Windows.Services.Structure.pas',
  Sub7.Core.Windows.Services.Register in '..\Shared\Sub7.Core.Windows.Services.Register.pas',
  Sub7.Core.Windows.Services.Utils in '..\Shared\Sub7.Core.Windows.Services.Utils.pas',
  Sub7.Core.Windows.PE.Version in '..\Shared\Sub7.Core.Windows.PE.Version.pas',
  ___S7BaseForm in 'Units\Forms\___Forms\___S7BaseForm.pas',
  Sub7.Viewer.Clipboard in 'Units\Sub7.Viewer.Clipboard.pas',
  Sub7.Viewer.Singletons in 'Units\Sub7.Viewer.Singletons.pas',
  Sub7.Core.Utils.Memory in '..\Shared\Sub7.Core.Utils.Memory.pas',
  Sub7.Viewer.Clipboard.FileSystem in 'Units\Sub7.Viewer.Clipboard.FileSystem.pas',
  Sub7.Core.Serializers.TStringList in '..\Shared\Sub7.Core.Serializers.TStringList.pas',
  Sub7.Core.FileSystem.Types in '..\Shared\Sub7.Core.FileSystem.Types.pas';

{$R *.res}
{$R version.res}

begin
  IsMultiThread := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  try
    LoadOpenSSL();
  except
    on E : Exception do begin
      Application.MessageBox(PWideChar(E.Message), 'OpenSSL Library Exception', MB_ICONERROR);

      ExitProcess(1);
    end;
  end;

  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormPassword, FormPassword);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TFormExceptions, FormExceptions);
  Application.CreateForm(TFormQueue, FormQueue);
  Application.CreateForm(TFormSockets, FormSockets);
  Application.CreateForm(TFormServerHive, FormServerHive);
  Application.CreateForm(TFormClientCertificate, FormClientCertificate);
  Application.CreateForm(TFormAddressBook, FormAddressBook);
  Application.Run;
end.
