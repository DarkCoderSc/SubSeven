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

unit Sub7.Core.Windows.Services.Structure;

interface

uses Winapi.WinSvc;

const SERVICE_ACCEPT_USERMODEREBOOT = $00000800;
      SERVICE_DELETE                = $00010000;

type
  TServiceDescription = SERVICE_DESCRIPTION;
  PServiceDescription = ^TServiceDescription;

  TServiceNotify = SERVICE_NOTIFY;
  PServiceNotify = ^TServiceNotify;

  TEnumServiceType = (
      estDriver,
      estFileSystemDriver,
      estKernelDriver,
      estWin32,
      estWin32OwnProcess,
      estWin32ShareProcess
  );

  TEnumServiceTypes = set of TEnumServiceType;

  TEnumServiceState = (
    essActive,
    essInactive
  );

  TEnumServiceStates = set of TEnumServiceState;

  TServiceType = (
    stUnknown,
    stFileSystemDriver,
    stKernelDriver,
    stWin32OwnProcess,
    stWin32ShareProcess,
    stUserOwnProcess,
    stUserShareProcess,
    stInteractiveProcess
  );

  TServiceTypes = set of TServiceType;

  TServiceState = (
    ssUnknown,
    ssContinuePending,
    ssPausePending,
    ssPaused,
    ssRunning,
    ssStartPending,
    ssStopPending,
    ssStopped
  );

  TServiceControlAccepted = (
    scaNetBindChange,
    scaParamChange,
    scaPauseContinue,
    scaPreshutdown,
    scaShutdown,
    scaStop,
    scaHardwareProfileChange,
    scaPowerEvent,
    scaSessionChange,
    scaTimeChange,
    scaTriggerEvent,
    scaUserModeReboot
  );

  TServiceControlsAccepted = set of TServiceControlAccepted;

  TServiceControlType = (
    sctStop,
    sctPause,
    sctContinue
  );

  TServiceStartType = (
    sstAutoStart,
    sstBootStart,
    sstDemandStart,
    sstDisabled,
    sstSystemStart
  );

  function ResolveWin32_ServiceType(const AServiceType : Cardinal) : TServiceType;
  function ResolveWin32_ServiceState(const AServiceState : Cardinal) : TServiceState;
  function ResolveWin32_ServiceControlsAccepted(const AServiceControlsAccepted : Cardinal) : TServiceControlsAccepted;

  function GetServiceStartType_Native(const AStartType : TServiceStartType) : Cardinal;

implementation

uses System.SysUtils, Winapi.Windows;

{ _.ResolveServiceType }

function ResolveWin32_ServiceType(const AServiceType : Cardinal) : TServiceType;
begin
  result := stUnknown;
  ///

  if (AServiceType and SERVICE_FILE_SYSTEM_DRIVER) = SERVICE_FILE_SYSTEM_DRIVER then
    result := stFileSystemDriver
  else
  if (AServiceType and SERVICE_KERNEL_DRIVER) = SERVICE_KERNEL_DRIVER then
    result := stKernelDriver
  else
  if (AServiceType and SERVICE_WIN32_OWN_PROCESS) = SERVICE_WIN32_OWN_PROCESS then
    result := stWin32OwnProcess
  else
  if (AServiceType and SERVICE_WIN32_SHARE_PROCESS) = SERVICE_WIN32_SHARE_PROCESS then
    result := stWin32ShareProcess
  else
  if (AServiceType and SERVICE_USER_OWN_PROCESS) = SERVICE_USER_OWN_PROCESS then
    result := stUserOwnProcess
  else
  if (AServiceType and SERVICE_USER_SHARE_PROCESS) = SERVICE_USER_SHARE_PROCESS then
    result := stUserShareProcess
  else
  if (AServiceType and SERVICE_INTERACTIVE_PROCESS) = SERVICE_INTERACTIVE_PROCESS then
    result := stInteractiveProcess
end;

{ _.UnresolveServiceType }

{ _.ResolveWin32_ServiceState }

function ResolveWin32_ServiceState(const AServiceState : Cardinal) : TServiceState;
begin
  case AServiceState of
    SERVICE_CONTINUE_PENDING : result := ssContinuePending;
    SERVICE_PAUSE_PENDING    : result := ssPausePending;
    SERVICE_PAUSED           : result := ssPaused;
    SERVICE_RUNNING          : result := ssRunning;
    SERVICE_START_PENDING    : result := ssStartPending;
    SERVICE_STOP_PENDING     : result := ssStopPending;
    SERVICE_STOPPED          : result := ssStopped;

    else
      result := ssUnknown;
  end;
end;

{ _.GetServiceControlsAccepted }

function ResolveWin32_ServiceControlsAccepted(const AServiceControlsAccepted : Cardinal) : TServiceControlsAccepted;
begin
  result := [];
  ///

  if (AServiceControlsAccepted and SERVICE_ACCEPT_NETBINDCHANGE) = SERVICE_ACCEPT_NETBINDCHANGE then
    result := result + [scaNetBindChange];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_PARAMCHANGE) = SERVICE_ACCEPT_PARAMCHANGE then
    result := result + [scaParamChange];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_PAUSE_CONTINUE) = SERVICE_ACCEPT_PAUSE_CONTINUE then
    result := result + [scaPauseContinue];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_PRESHUTDOWN) = SERVICE_ACCEPT_PRESHUTDOWN then
    result := result + [scaPreshutdown];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_SHUTDOWN) = SERVICE_ACCEPT_SHUTDOWN then
    result := result + [scaShutdown];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_STOP) = SERVICE_ACCEPT_STOP then
    result := result + [scaStop];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_HARDWAREPROFILECHANGE) = SERVICE_ACCEPT_HARDWAREPROFILECHANGE then
    result := result + [scaHardwareProfileChange];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_POWEREVENT) = SERVICE_ACCEPT_POWEREVENT then
    result := result + [scaPowerEvent];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_SESSIONCHANGE) = SERVICE_ACCEPT_SESSIONCHANGE then
    result := result + [scaSessionChange];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_TIMECHANGE) = SERVICE_ACCEPT_TIMECHANGE then
    result := result + [scaTimeChange];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_TRIGGEREVENT) = SERVICE_ACCEPT_TRIGGEREVENT then
    result := result + [scaTriggerEvent];

  if (AServiceControlsAccepted and SERVICE_ACCEPT_USERMODEREBOOT) = SERVICE_ACCEPT_USERMODEREBOOT then
    result := result + [scaUserModeReboot];
end;

{ _.GetServiceStartType_Native }

function GetServiceStartType_Native(const AStartType : TServiceStartType) : Cardinal;
begin
  case AStartType of
    sstAutoStart   : result := SERVICE_AUTO_START;
    sstBootStart   : result := SERVICE_BOOT_START;
    sstDemandStart : result := SERVICE_DEMAND_START;
    sstDisabled    : result := SERVICE_DISABLED;
    sstSystemStart : result := SERVICE_SYSTEM_START;

    else
      result := 0;
  end;
end;

end.
