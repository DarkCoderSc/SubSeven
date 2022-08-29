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

unit Sub7.Core.Protocol;

interface

uses WinAPI.Winsock2, Winapi.Messages;

const S7_PAQUET_SIZE            = 4096;

      S7_MEM_SCREENHELPER_USR   = '{41282EE4-5B87-4393-A228-FACE66981CDD}';
      S7_MEM_SCREENHELPER_MSIZE = 1024 * 1024 * 32;

      {
        Messages
      }
      S7_WM_BASE      = WM_USER + 28012; // 28/01/2021
      S7_WM_ADD_ENTRY = S7_WM_BASE + 1;


{
  Commands
}
type
  TS7UploadDispatcher = (
    udFileManager
  );

  TS7TransferDirection = (
                            tdDownload,
                            tdUpload
  );

  TS7Command = (
    {
      Global Commands
    }
    s7cUnknown,
    s7cServerException,
    s7cCreateSession,
    s7cAttachToSession,
    s7cSessionAttached,
    s7cInvalidSession,
    s7cVerifySessionKey,
    s7cSessionVerified,
    s7cNoConcurrent,
    s7cPasswordAuthentication,
    s7cVersionMismatch,
    s7cPeerKeyVerificationFailed,
    s7cAck,

    {
      Main Commands
    }
    mhcPing,
    mhcPong,
    mhcProcessList,
    mhcTerminateProcess,
    mhcShutdown,
    mhcPoweroff,
    mhcReboot,
    mhcLogoff,
    mhcLock,
    mhcHibernate,
    mhcSleep,
    mhcRun,
    mhcSuccess,
    mhcOpen,

    mhcFileManager,

    {
      File Manager Commands
    }
    fmcRefreshDrives,
    fmcBrowseFolder,
    fmcCreateFolder,
    fmcRenameFile,
    fmcDeleteFile,
    fmcFileAction,

    {
      Transfer Commands
    }
    tcNegociate,
    tcFileInformation,
    tcUpdateInformation,

    {
      Upload Stream
    }
    uscUpload,

    {
      Download Stream
    }
    dscDownload,

    {
      Process Manager
    }
    pmRefreshProcess,

    {
      SystemInformationHook
    }
    tshEnumSessions,
    sihUser,

    {
      Remote Terminal (Shell)
    }
    rtStarted,
    rtStopped,
    rtData,
    rtNew,
    rtCommand,
    rtBreak,
    rtClose
  );

  TS7UploadKind = (
    ukSilent,
    ukFileManager
  );

  TS7ReactKind = (
    rkFileManager
  );

  TS7SessionInformation = record
    SessionKey    : String;
    SessionId     : String;
    RemoteAddress : String;
    RemotePort    : Word;
    Domain        : String;
  end;

  TS7TransferStatus = (
                        tsQueue,
                        tsProgress,
                        tsError
  );

  TS7ExceptionContext = (
                            ecViewer,
                            ecServer
  );

  TS7HelperCommand = (
    hcTestSuccess,
    hcTestException,
    hcLockComputer,
    hcLogoff,
    hcOpen
  );

  TS7RunAsMode = (
    ramNT,
    ramSession,
    ramUser
  );

implementation

end.
