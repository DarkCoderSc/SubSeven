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

unit Sub7.Core.Windows.Services.Notify;

interface

uses System.Classes, Sub7.Core.Windows.Services.Structure, Sub7.Core.IntervalThread;

type
  TServiceStatus = (
    svcStatusNone,
    svcStatusStopped,
    svcStatusStarted,
    svcStatusInProgress
  );

  TOnServiceStatusChange = procedure(Sender : TObject; const AInstalled : Boolean; const AStatus : TServiceStatus) of object;

  TServiceNotifier = class;

  TServiceNotifierThread = class(TCoreIntervalThread)
  protected
    FServiceManager       : THandle;
    FOnServiceStateChange : TOnServiceStatusChange;
    FServiceName          : String;

    {@M}
    procedure ExecuteInterval(); override;
  public
    {@C}
    constructor Create(const AServiceNotifier : TServiceNotifier);
    destructor Destroy(); override;
  end;

  TServiceNotifier = class
  private
    FOnServiceStateChange : TOnServiceStatusChange;
    FServiceName          : String;
    FWorker               : TServiceNotifierThread;

    {@M}
    function GetActive() : Boolean;
    procedure SetActive(const AValue : Boolean);
  public
    {@C}
    constructor Create(const AServiceName : String);
    destructor Destroy(); override;

    {@G/S}
    property OnServiceStateChange : TOnServiceStatusChange read FOnServiceStateChange write FOnServiceStateChange;
    property ServiceName          : String                 read FServiceName          write FServiceName;
    property Active               : Boolean                read GetActive             write SetActive;
  end;

implementation

uses Winapi.WinSvc, Sub7.Core.Exceptions, Winapi.Windows, Sub7.Core.Bundle,
     System.SysUtils, Sub7.Core.Windows.Services.Utils,
     Sub7.Core.Windows.Services.Service;

(*
  TServiceNotifierThread
*)

{ TServiceNotifierThread.Create }

constructor TServiceNotifierThread.Create(const AServiceNotifier : TServiceNotifier);
begin
  inherited Create(500);
  ///

  FServiceManager := 0;

  if not Assigned(AServiceNotifier) then
    raise Exception.Create(ERR_MISSING_INSTANCE);

  FServiceManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if FServiceManager = 0 then
    raise ES7WindowsException.Create('OpenSCManager');

  FServiceName          := AServiceNotifier.ServiceName;
  FOnServiceStateChange := AServiceNotifier.OnServiceStateChange;
end;

{ TServiceNotifierThread.Destroy }

destructor TServiceNotifierThread.Destroy();
begin
  if FServiceManager <> 0 then
    CloseServiceHandle(FServiceManager);

  ///
  inherited Destroy();
end;

{ TServiceNotifierThread.ExecuteInterval }

procedure TServiceNotifierThread.ExecuteInterval();
var AService : TService;
    ARunning : Boolean;
    AStatus  : TServiceStatus;
begin
  if not TService.Exists(FServiceName) then begin
    // Service Doesn't exists
    AStatus := svcStatusNone;
    if Assigned(FOnServiceStateChange) then
      SafeSynchronize(procedure begin
        FOnServiceStateChange(self, False, AStatus);
      end);
  end else begin
    // Service Exists

    AService := TService.Create(FServiceName);
    try
      try
        case AService.State of
          ssRunning : AStatus := svcStatusStarted;
          ssStopped : AStatus := svcStatusStopped;

          else
            AStatus := svcStatusInProgress;
        end;

        if Assigned(FOnServiceStateChange) then
          SafeSynchronize(procedure begin
            FOnServiceStateChange(self, True, AStatus);
          end);
      except
        on E : ES7ServiceNotFound do begin
          //
        end;
      end;
    finally
      if Assigned(AService) then
        FreeAndNil(AService);
    end;
  end;
end;

(*
  TServiceNotifier
*)

{ TServiceNotifier.Create }

constructor TServiceNotifier.Create(const AServiceName : String);
begin
  inherited Create();
  ///

  FWorker               := nil;
  FOnServiceStateChange := nil;
  FServiceName          := AServiceName;
end;

{ TServiceNotifier.Destroy }

destructor TServiceNotifier.Destroy();
begin
  if Assigned(FWorker) then
    FreeAndNil(FWorker);

  ///
  inherited Destroy();
end;

{ TServiceNotifier.GetActive }

function TServiceNotifier.GetActive() : Boolean;
begin
  result := False;
  ///

  if not Assigned(FWorker) then
    Exit();

  result := FWorker.Active;
end;

{ TServiceNotifier.SetActive }

procedure TServiceNotifier.SetActive(const AValue : Boolean);
begin
  if AValue and not self.Active then
    FWorker := TServiceNotifierThread.Create(self)
  else if Assigned(FWorker) then
    FreeAndNil(FWorker);
end;

end.

