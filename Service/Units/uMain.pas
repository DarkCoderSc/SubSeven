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

{
  Passer un TServerConfig au Listener thread plutot que toutes ces options moches
}

unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs;

type
  TSub7Legacy = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
    procedure ServiceBeforeInstall(Sender: TService);
    procedure ServiceBeforeUninstall(Sender: TService);
    procedure ServiceContinue(Sender: TService; var Continued: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceDestroy(Sender: TObject);
    procedure ServicePause(Sender: TService; var Paused: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceExecute(Sender: TService);
  private

  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  Sub7Legacy: TSub7Legacy;

implementation

uses Sub7.Core.Diagnostic, Sub7.Core.Utils, Sub7.Core.Application.Env, Sub7.Core.Input.Validators,
     Sub7.Core.Thread.Watcher, Sub7.Thread.Net.Server.Listener, Sub7.Core.SafeSocketList,
     Sub7.Service.Config, Sub7.Core.Magic, Sub7.Core.Bundle;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  Sub7Legacy.Controller(CtrlCode);
end;

function TSub7Legacy.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TSub7Legacy.ServiceAfterInstall(Sender: TService);
begin
  Log('Service was successfully installed.');
  ///

end;

procedure TSub7Legacy.ServiceAfterUninstall(Sender: TService);
begin
   Log('Service was successfully uninstalled.');
  ///
end;

procedure TSub7Legacy.ServiceBeforeInstall(Sender: TService);
begin
  Log('Service is beeing installed...');
  ///

end;

procedure TSub7Legacy.ServiceBeforeUninstall(Sender: TService);
begin
  Log('Service is beeing uninstalled...');
  ///

end;

procedure TSub7Legacy.ServiceContinue(Sender: TService; var Continued: Boolean);
begin
  Log('Service Continue Signal Received.');
  ///

end;

procedure TSub7Legacy.ServiceCreate(Sender: TObject);
begin
  Log('Service Create.');
  ///

end;

procedure TSub7Legacy.ServiceDestroy(Sender: TObject);
begin
  Log('Service Destroy.');
  ///

end;

procedure TSub7Legacy.ServiceExecute(Sender: TService);
var AServer : TSub7ServerListener;
    AConfig : TServiceConfig;
begin
  Log('Service Execution Thread Begins.');
  try
    // *************************************************************************
    // Anti Hacking Code Begin *************************************************
    // *************************************************************************
    try
      var b : Boolean := TSubSevenMagic.CheckMagic();
      if not b then
        raise Exception.Create('');
    except
      on E : Exception do begin
        Log(ERR_MAGIC);

        raise;

        ExitProcess(0); // Just in case
      end;
    end;
    // *************************************************************************
    // Anti Hacking Code End ***************************************************
    // *************************************************************************

    AConfig := TServiceConfig.Create();
    try
      AConfig.LoadFromFile(APP_ENV_ConfigFile);
      ///

      // Server Listener
      AServer := TSub7ServerListener.Create(AConfig.ServerConfig);
      try
        while not Terminated do begin
          ServiceThread.ProcessRequests(false);
          ///

          if not isThreadActive(AServer) then
            break;

          ///
          Sleep(1000);
        end;
      finally
        {
          Bellow instruction ensure that in case a socket is blocked somewhere it
          will get release.

          Then we can shutdown listener normally with a higher chance rate.
        }
        GLOBAL_SafeSocketList.Clear();
        ///

        if Assigned(AServer) then
          FreeAndNil(AServer);

        if Assigned(GLOBAL_THREAD_WATCHER) then
          FreeAndNil(GLOBAL_THREAD_WATCHER);
      end;
    finally
      if Assigned(AConfig) then
        FreeAndNil(AConfig);
    end;
  except
    on E : Exception do
      CrashLog(E, self);
  end;
  Log('Service Execution Thread Ends.');
end;

procedure TSub7Legacy.ServicePause(Sender: TService; var Paused: Boolean);
begin
  Log('Service Pause Signal Received.');
  ///

end;

procedure TSub7Legacy.ServiceShutdown(Sender: TService);
begin
  Log('Service Shutdown Signal Received.');
  ///

end;

procedure TSub7Legacy.ServiceStart(Sender: TService; var Started: Boolean);
begin
  Log('Service Start Signal Received');
  ///

  Sleep(1000);

  // ***************************************************************************
  // Anti Hacking Code Begin ***************************************************
  // ***************************************************************************
  try
    var b : Boolean := TSubSevenMagic.CheckMagic();
    if not b then
      raise Exception.Create('');
  except
    on E : Exception do begin
      Log(ERR_MAGIC);

      raise;

      ExitProcess(0); // Just in case
    end;
  end;
  // ***************************************************************************
  // Anti Hacking Code End *****************************************************
  // ***************************************************************************
end;

procedure TSub7Legacy.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  Log('Service Stop Signal Received.');
  ///


end;

end.
