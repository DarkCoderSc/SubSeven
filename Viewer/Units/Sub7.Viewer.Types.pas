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

unit Sub7.Viewer.Types;

interface

uses Sub7.Thread.Net.Client.Base, Sub7.Core.Types;

type
  TNavMenu = (
    nmMain,
    nmCategory,
    nmFileManager,
    nmRegistryEditor,
    nmProcessManager,
    nmServiceManager,
    nmRun,
    nmOpen,
    nmQueue,
    nmSearchForFiles,
    nmTerminal,
    nmRemoteDesktop,
    nmMachineActions,
    nmMachinePing
  );

  function WorkerKindToString(const AKind : TWorkerKind) : String;
  function WorkerClassToKind(const AClientClass : TSub7ClientBase) : TWorkerKind;

implementation

uses Sub7.Core.Bundle, System.SysUtils,

     {
        Workers Goes Here
     }
     Sub7.Thread.Net.Client.Session.Cmd,
     Sub7.Thread.Net.Client.SystemInformationHook,
     Sub7.Thread.Net.Client.RemoteShell,
     Sub7.Thread.Net.Client.Transfer.Upload,
     Sub7.Thread.Net.Client.Transfer.Download

     ///
     ;
     ///

{-------------------------------------------------------------------------------
  Translate Worker Type to String
-------------------------------------------------------------------------------}
function WorkerKindToString(const AKind : TWorkerKind) : String;
begin
  result := 'Handler';
  ///

  case AKind of
    wkMainHandler           : result := 'Main Handler';
    wkRemoteDesktopHandler  : result := 'Remote Desktop';
    wkUploadHandler         : result := 'Stream Upload';
    wkDownloadHandler       : result := 'Stream Download';
    wkSessionListener       : result := 'System Information Hook';
    wkTerminal              : result := 'Remote Terminal';
  end;
end;

{-------------------------------------------------------------------------------
  Translate Worker Class Type to Worker
-------------------------------------------------------------------------------}
function WorkerClassToKind(const AClientClass : TSub7ClientBase) : TWorkerKind;
begin
  result := wkUnknown;
  ///

  if AClientClass is TSub7ClientSessionCmd then
    Exit(wkMainHandler);

  if AClientClass is TSub7SystemInformationHook then
    Exit(wkSessionListener);

  if AClientClass is TSub7ClientRemoteShell then
    Exit(wkTerminal);

  if AClientClass is TThreadTransferUpload then
    Exit(wkUploadHandler);

  if AClientClass is TThreadTransferDownload then
    Exit(wkDownloadHandler);
end;

end.
