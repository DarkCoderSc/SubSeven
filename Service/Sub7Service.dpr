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

program Sub7Service;

uses
  Vcl.SvcMgr,
  System.SysUtils,
  Winapi.Windows,
  uMain in 'Units\uMain.pas' {Sub7Legacy: TService},
  Sub7.Core.Application.Env in '..\Shared\Sub7.Core.Application.Env.pas',
  Sub7.Core.Diagnostic in '..\Shared\Sub7.Core.Diagnostic.pas',
  XSuperObject in '..\Shared\XSuperObject.pas',
  XSuperJson in '..\Shared\XSuperJson.pas',
  Sub7.Core.Protocol in '..\Shared\Sub7.Core.Protocol.pas',
  Sub7.Core.Exceptions in '..\Shared\Sub7.Core.Exceptions.pas',
  Sub7.Core.Crypto.RC4 in '..\Shared\Sub7.Core.Crypto.RC4.pas',
  Sub7.Core.Crypto.CRC32 in '..\Shared\Sub7.Core.Crypto.CRC32.pas',
  Sub7.Core.Windows.Information in '..\Shared\Sub7.Core.Windows.Information.pas',
  Sub7.Core.FileSystem.Drives.Enum in '..\Shared\Sub7.Core.FileSystem.Drives.Enum.pas',
  Sub7.Core.FileSystem.Enum in '..\Shared\Sub7.Core.FileSystem.Enum.pas',
  Sub7.Core.Bundle in '..\Shared\Sub7.Core.Bundle.pas',
  Sub7.Thread.Net.Server.Peer.Transfer.Receive in 'Units\Sub7.Thread.Net.Server.Peer.Transfer.Receive.pas',
  Sub7.Thread.Net.Server.Peer.Transfer.Send in 'Units\Sub7.Thread.Net.Server.Peer.Transfer.Send.pas',
  Sub7.Core.Windows.Process.Enum in '..\Shared\Sub7.Core.Windows.Process.Enum.pas',
  Sub7.Core.OOP.Interfaces in '..\Shared\Sub7.Core.OOP.Interfaces.pas',
  Sub7.Core.Windows.Sessions.Enum in '..\Shared\Sub7.Core.Windows.Sessions.Enum.pas',
  Sub7.Service.External.Helper in 'Units\Sub7.Service.External.Helper.pas',
  Sub7.Core.Windows.Process in '..\Shared\Sub7.Core.Windows.Process.pas',
  Sub7.Core.Shell.Emulator in '..\Shared\Sub7.Core.Shell.Emulator.pas',
  Sub7.Core.Shell.Event in '..\Shared\Sub7.Core.Shell.Event.pas',
  Sub7.Core.Shell.Thread.Instance in '..\Shared\Sub7.Core.Shell.Thread.Instance.pas',
  Sub7.Core.Windows.User.Enum in '..\Shared\Sub7.Core.Windows.User.Enum.pas',
  Sub7.Core.Input.Validators in '..\Shared\Sub7.Core.Input.Validators.pas',
  Sub7.Core.Thread in '..\Shared\Sub7.Core.Thread.pas',
  Sub7.OpenSSL.TLS.Context in '..\Shared\Sub7.OpenSSL.TLS.Context.pas',
  Sub7.OpenSSL.TLS.Socket in '..\Shared\Sub7.OpenSSL.TLS.Socket.pas',
  Sub7.OpenSSL.Headers in '..\Shared\Sub7.OpenSSL.Headers.pas',
  Sub7.OpenSSL.TLS.Exceptions in '..\Shared\Sub7.OpenSSL.TLS.Exceptions.pas',
  Sub7.OpenSSL.TLS.IOHandler in '..\Shared\Sub7.OpenSSL.TLS.IOHandler.pas',
  Sub7.OpenSSL.TLS.Utils in '..\Shared\Sub7.OpenSSL.TLS.Utils.pas',
  Sub7.TLS.IOHandler in '..\Shared\Sub7.TLS.IOHandler.pas',
  Sub7.Core.Synchronization.CriticalSectionEx in '..\Shared\Sub7.Core.Synchronization.CriticalSectionEx.pas',
  Sub7.Thread.Net.Server.SystemInformationHook in 'Units\Sub7.Thread.Net.Server.SystemInformationHook.pas',
  Sub7.Thread.Net.Server.Peer.IntervalThread in 'Units\Sub7.Thread.Net.Server.Peer.IntervalThread.pas',
  Sub7.Thread.Net.Server.Peer.Base in 'Units\Sub7.Thread.Net.Server.Peer.Base.pas',
  Sub7.Thread.Net.Server.Listener in 'Units\Sub7.Thread.Net.Server.Listener.pas',
  Sub7.Thread.Net.Server.Peer.Session in 'Units\Sub7.Thread.Net.Server.Peer.Session.pas',
  Sub7.Thread.Net.Server.Peer.Session.Cmd in 'Units\Sub7.Thread.Net.Server.Peer.Session.Cmd.pas',
  Sub7.Thread.Net.Server.Peer.NonBlockingCmd in 'Units\Sub7.Thread.Net.Server.Peer.NonBlockingCmd.pas',
  Sub7.Core.Types in '..\Shared\Sub7.Core.Types.pas',
  Sub7.Thread.Net.Server.Session.Registrar in 'Units\Sub7.Thread.Net.Server.Session.Registrar.pas',
  Sub7.Service.Types in 'Units\Sub7.Service.Types.pas',
  Sub7.Core.IntervalThread in '..\Shared\Sub7.Core.IntervalThread.pas',
  Sub7.Thread.Net.Server.RemoteShell in 'Units\Sub7.Thread.Net.Server.RemoteShell.pas',
  Sub7.Core.SafeSocketList in '..\Shared\Sub7.Core.SafeSocketList.pas',
  Sub7.Core.Thread.Watcher in '..\Shared\Sub7.Core.Thread.Watcher.pas',
  Sub7.Core.Windows in '..\Shared\Sub7.Core.Windows.pas',
  Sub7.Core.Utils in '..\Shared\Sub7.Core.Utils.pas',
  Sub7.Service.Config in 'Units\Sub7.Service.Config.pas',
  Sub7.Core.Windows.PE.Version in '..\Shared\Sub7.Core.Windows.PE.Version.pas',
  Sub7.Core.FileSystem.Utils in '..\Shared\Sub7.Core.FileSystem.Utils.pas',
  Sub7.Core.Magic in '..\Shared\Sub7.Core.Magic.pas';

{$R *.RES}
{$R version.res}

begin
  IsMultiThread := True;
  ///

  LoadOpenSSL();

  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;

  Application.CreateForm(TSub7Legacy, Sub7Legacy);
  Application.Run;
end.
