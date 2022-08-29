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

unit Sub7.Core.Windows.Services.Service;

interface

uses System.Classes, Sub7.Core.Windows.Services.Structure, Winapi.WinSvc,
     Winapi.Windows;

type
  TServiceConfig = record
    ImagePath   : array[0..(MAX_PATH * 2) -1] of WideChar;
    SvcType     : TServiceType;
    DisplayName : String[255];
  end;

  TService = class
  private
    FName           : String;
    FServiceManager : THandle;

    {@M}
    function GetDescription() : String;
    procedure SetDescription(const ADescription : String);

    function GetServiceConfig() : TServiceConfig;

    function GetServiceType() : TServiceType;
    procedure SetServiceType(const AValue : TServiceType);

    function GetImagePath() : String;
    procedure SetImagePath(const AValue : String);

    function GetRunning() : Boolean;

    function GetDisplayName() : String;
    procedure SetDisplayName(const AValue : String);

    function GetServiceStatus() : TServiceStatus;

    function GetState() : TServiceState;
    function GetControlsAccepted() : TServiceControlsAccepted;

    function OpenService(const AFlags : Cardinal) : THandle;

    procedure ServiceControl(const AControlType : TServiceControlType; const AWaitTimeout : Cardinal = INFINITE);
  public
    {@C}
    constructor Create(const AName : String);
    destructor Destroy(); override;

    {@M}
    procedure Start(const AWaitTimeout : Cardinal = INFINITE);
    procedure Stop(const AWaitTimeout : Cardinal = INFINITE);
    procedure Pause(const AWaitTimeout : Cardinal = INFINITE);
    procedure Resume(const AWaitTimeout : Cardinal = INFINITE);
    procedure Continue(const AWaitTimeout : Cardinal = INFINITE); {ALIAS: Resume()}
    procedure Restart(const AWaitTimeout : Cardinal = INFINITE);

    class function Exists(const AName : String) : Boolean; static;

    procedure Delete();

    procedure WaitFor(const AWaitState : Cardinal; const ATimeout : Cardinal = INFINITE);

    {@G}
    property Name             : String                   read FName;
    property State            : TServiceState            read GetState;
    property ControlsAccepted : TServiceControlsAccepted read GetControlsAccepted;
    property Running          : Boolean                  read GetRunning;

    {@G/S}
    property ImagePath   : String        read GetImagePath   write SetImagePath;
    property DisplayName : String        read GetDisplayName write SetDisplayName;
    property ServiceType : TServiceType  read GetServiceType write SetServiceType;
    property Description : String        read GetDescription write SetDescription;
  end;

implementation

uses Sub7.Core.Exceptions, System.SysUtils, System.Diagnostics,
     Sub7.Core.Windows.Services.Utils;

{ TService.Exists }

class function TService.Exists(const AName : String) : Boolean;
var AServiceManager : THandle;
    AService        : THandle;
begin
  result := False;
  ///

  AServiceManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if AServiceManager = 0 then
    raise ES7WindowsException.Create('OpenSCManager');
  try
    try
      AService := Sub7.Core.Windows.Services.Utils.OpenService(AServiceManager, AName, SERVICE_QUERY_STATUS);

      CloseServiceHandle(AService);

      ///
      result := True;
    except
      on E : ES7ServiceNotFound do begin
        ///
      end;

      else
        raise;
    end;
  finally
    CloseServiceHandle(AServiceManager);
  end;
end;

{ TService.GetDescription }

function TService.GetDescription() : String;
var ARequiredBufferSize : Cardinal;
    pDescription        : PServiceDescription;
    AService            : THandle;
begin
  result := '';
  ///

  AService := self.OpenService(SERVICE_QUERY_CONFIG);
  try
    if not QueryServiceConfig2(AService, SERVICE_CONFIG_DESCRIPTION, nil, 0, @ARequiredBufferSize) then
      if GetLastError() <> ERROR_INSUFFICIENT_BUFFER then
        Exit();

    if ARequiredBufferSize <= SizeOf(Pointer) then
      Exit();

    GetMem(pDescription, ARequiredBufferSize);
    try
      if not QueryServiceConfig2W(
                                  AService,
                                  SERVICE_CONFIG_DESCRIPTION,
                                  PByte(pDescription),
                                  ARequiredBufferSize,
                                  @ARequiredBufferSize
      ) then
        Exit();

      ///
      result := String(pDescription^.lpDescription);
    finally
      FreeMem(pDescription, ARequiredBufferSize);
    end;
  finally
    CloseServiceHandle(AService);
  end;
end;

{ TService.SetDescription }

procedure TService.SetDescription(const ADescription : String);
var AServiceDescription  : TServiceDescription;
    AService             : THandle;
begin
  AService := self.OpenService(SERVICE_CHANGE_CONFIG);
  try
    ZeroMemory(@AServiceDescription, SizeOf(TServiceDescription));
    ///

    AServiceDescription.lpDescription := PWideChar(ADescription);

    if not ChangeServiceConfig2W(AService, SERVICE_CONFIG_DESCRIPTION, @AServiceDescription) then
      raise ES7WindowsException.Create('ChangeServiceConfig2W');
  finally
    CloseServiceHandle(AService);
  end;
end;

{ TService.GetServiceConfig }

function TService.GetServiceConfig() : TServiceConfig;
var AService            : THandle;
    ARequiredBufferSize : Cardinal;
    ABufferSize         : Cardinal;
    pConfig             : PQueryServiceConfig;
begin
  ZeroMemory(@result, SizeOf(TServiceConfig));
  ///

  AService := self.OpenService(SERVICE_QUERY_CONFIG);
  try
    if not QueryServiceConfig(AService, nil, 0, ARequiredBufferSize) then
    if GetLastError() <> ERROR_INSUFFICIENT_BUFFER then
      raise ES7WindowsException.Create('(1)QueryServiceConfig');
    ///

    ABufferSize := ARequiredBufferSize;

    GetMem(pConfig, ABufferSize);
    try
      if not QueryServiceConfig(AService, pConfig, ARequiredBufferSize, ARequiredBufferSize) then
        raise ES7WindowsException.Create('(2)QueryServiceConfig');
      ///

      // Read Config Informations
      result.DisplayName := pConfig^.lpDisplayName;
      result.SvcType     := ResolveWin32_ServiceType(pConfig.dwServiceType);

      StrCopy(PWideChar(result.ImagePath[0]), pConfig^.lpBinaryPathName);
    finally
      FreeMem(pConfig, ABufferSize);
    end;
  finally
    CloseServiceHandle(AService);
  end;
end;

{ TService.GetServiceType }

function TService.GetServiceType() : TServiceType;
begin
  result := self.GetServiceConfig().SvcType;
end;

{ TService.SetServiceType }

procedure TService.SetServiceType(const AValue : TServiceType);
var AService : THandle;
begin
//  AService := self.OpenService(SERVICE_CHANGE_CONFIG);
//  try
//    Winapi.WinSvc.ChangeServiceConfigW(
//      {Handle}               AService,
//      {Type}                 SERVICE_NO_CHANGE,
//      {Start Type}           SERVICE_NO_CHANGE,
//      {Error Control}        SERVICE_NO_CHANGE,
//      {Image Path}           nil,
//      {Load Order Group}     nil,
//      {Tag Id}               nil,
//      {Dependencies}         nil,
//      {Service Start Name}   nil,
//      {Password}             nil,
//      {Display Name}         nil
//    );
//  finally
//    CloseServiceHandle(AService);
//  end;
  // TODO
end;

{ TService.GetImagePath }

function TService.GetImagePath() : String;
begin
  result := String(self.GetServiceConfig().ImagePath);
end;

{ TService.SetImagePath }

procedure TService.SetImagePath(const AValue : String);
var AService : THandle;
begin
  AService := self.OpenService(SERVICE_CHANGE_CONFIG);
  try
    Winapi.WinSvc.ChangeServiceConfigW(
      {Handle}               AService,
      {Type}                 SERVICE_NO_CHANGE,
      {Start Type}           SERVICE_NO_CHANGE,
      {Error Control}        SERVICE_NO_CHANGE,
      {Image Path}           PWideChar(AValue),
      {Load Order Group}     nil,
      {Tag Id}               nil,
      {Dependencies}         nil,
      {Service Start Name}   nil,
      {Password}             nil,
      {Display Name}         nil
    );
  finally
    CloseServiceHandle(AService);
  end;
end;

{ TService.GetDisplayName }

function TService.GetDisplayName() : String;
begin
  result := self.GetServiceConfig().DisplayName;
end;

{ TService.SetDisplayName }

procedure TService.SetDisplayName(const AValue : String);
var AService : THandle;
begin
  AService := self.OpenService(SERVICE_CHANGE_CONFIG);
  try
    Winapi.WinSvc.ChangeServiceConfigW(
      {Handle}               AService,
      {Type}                 SERVICE_NO_CHANGE,
      {Start Type}           SERVICE_NO_CHANGE,
      {Error Control}        SERVICE_NO_CHANGE,
      {Image Path}           nil,
      {Load Order Group}     nil,
      {Tag Id}               nil,
      {Dependencies}         nil,
      {Service Start Name}   nil,
      {Password}             nil,
      {Display Name}         PWideChar(AValue)
    );
  finally
    CloseServiceHandle(AService);
  end;
end;

{ TService.GetServiceStatus }

function TService.GetServiceStatus() : TServiceStatus;
var AService : THandle;
begin
  AService := self.OpenService(SERVICE_QUERY_STATUS);
  try
    ZeroMemory(@result, SizeOf(TServiceStatus));
    ///

    if not QueryServiceStatus(AService, result) then
      raise ES7WindowsException.Create('QueryServiceStatus');
  finally
    CloseServiceHandle(AService);
  end;
end;

{ TService.GetState }

function TService.GetState() : TServiceState;
begin
  result := ResolveWin32_ServiceState(self.GetServiceStatus.dwCurrentState);
end;

{ TService.GetControlsAccepted }

function TService.GetControlsAccepted() : TServiceControlsAccepted;
begin
  result := ResolveWin32_ServiceControlsAccepted(self.GetServiceStatus.dwControlsAccepted);
end;

{ TService.OpenService }

function TService.OpenService(const AFlags : Cardinal) : THandle;
begin
  result := Sub7.Core.Windows.Services.Utils.OpenService(FServiceManager, FName, AFlags);
end;

{ TService.WaitFor }

procedure TService.WaitFor(const AWaitState : Cardinal; const ATimeout : Cardinal = INFINITE);
var AStatus    : TServiceStatus;
    AService   : THandle;
    AStopwatch : TStopwatch;
begin
  AService := self.OpenService(SERVICE_QUERY_STATUS);
  try
    AStopwatch := TStopwatch.Create();
    ///

    while True do begin
      if not QueryServiceStatus(AService, AStatus) then
        break;

      if AStatus.dwCurrentState = AWaitState then
        break;

      if (ATimeout <> INFINITE) and (AStopwatch.ElapsedMilliseconds >= ATimeout) then
        break;
    end;
  finally
    CloseServiceHandle(AService);
  end;
end;

{ TService.GetRunning }

function TService.GetRunning() : Boolean;
begin
  result := self.State = ssRunning;
end;

{ TService.Start }

procedure TService.Start(const AWaitTimeout : Cardinal = INFINITE);
var AService          : THandle;
    pServiceArgVector : PWideChar;
begin
  if self.Running then
    Exit();
  ///

  AService := self.OpenService(SERVICE_START);
  try
    pServiceArgVector := nil;

    if not StartService(AService, 0, pServiceArgVector) then
      raise ES7WindowsException.Create('StartService');

    if AWaitTimeout > 0 then
      self.WaitFor(SERVICE_RUNNING, AWaitTimeout);
  finally
    CloseServiceHandle(AService);
  end;
end;

{ TService.Restart }

procedure TService.Restart(const AWaitTimeout : Cardinal = INFINITE);
begin
  if not self.Running then
    Exit();
  ///

  self.Stop(); // Wait

  self.Start(AWaitTimeout);
end;

{ TService.ServiceControl }

procedure TService.ServiceControl(const AControlType : TServiceControlType; const AWaitTimeout : Cardinal = INFINITE);
var AService    : THandle;
    AStatus     : TServiceStatus;
    AControl    : DWORD;
    AFlags      : Cardinal;
    AWaitSignal : Cardinal;
begin
  AFlags := SERVICE_QUERY_STATUS;

  case AControlType of
    sctStop     : AFlags := AFlags or SERVICE_STOP;

    sctPause,
    sctContinue : AFlags := AFlags or SERVICE_PAUSE_CONTINUE;
  end;

  AService := self.OpenService(AFlags);
  try
    if not QueryServiceStatus(AService, AStatus) then
      raise ES7WindowsException.Create('QueryServiceStatus');
    //

    case AControlType of
      sctStop : begin
        AControl    := SERVICE_CONTROL_STOP;
        AWaitSignal := SERVICE_STOPPED;

        if not (scaStop in self.ControlsAccepted) then
          raise ES7ServiceException.Create(Format('Stop control is not available in "%s" service.', [FName]));
      end;

      sctPause, sctContinue : begin
        if AControlType = sctPause then begin
          AControl    := SERVICE_CONTROL_PAUSE;
          AWaitSignal := SERVICE_PAUSED;
        end else begin
          AControl    := SERVICE_CONTROL_CONTINUE;
          AWaitSignal := SERVICE_RUNNING;
        end;

        if not (scaPauseContinue in self.ControlsAccepted) then
          raise ES7ServiceException.Create(Format('Pause/Continue control is not available in "%s" service.', [FName]));
      end;
    end;

    if not ControlService(AService, AControl, AStatus) then
      raise ES7WindowsException.Create('ControlService');

    if AWaitTimeout > 0 then
      self.WaitFor(AWaitSignal, AWaitTimeout);
  finally
    CloseServiceHandle(AService);
  end;
end;

{ TService.Stop }

procedure TService.Stop(const AWaitTimeout : Cardinal = INFINITE);
begin
  if self.Running then
    self.ServiceControl(sctStop, AWaitTimeout);
end;

{ TService.Pause }

procedure TService.Pause(const AWaitTimeout : Cardinal = INFINITE);
begin
  if self.Running then
    self.ServiceControl(sctPause, AWaitTimeout);
end;

{ TService.Resume }

procedure TService.Resume(const AWaitTimeout : Cardinal = INFINITE);
begin
  if self.State = ssPaused then
    self.ServiceControl(sctContinue, AWaitTimeout);
end;

{ TService.Continue(Alias) }

procedure TService.Continue(const AWaitTimeout : Cardinal = INFINITE);
begin
  self.Resume(AWaitTimeout);
end;

{ TService.Delete }

procedure TService.Delete();
var AService : THandle;
begin
  AService := self.OpenService(SERVICE_DELETE);
  try
    self.Stop();
    ///

    if not DeleteService(AService) then
      raise ES7WindowsException.Create('DeleteService');
  finally
    CloseServiceHandle(AService);
  end;
end;

{ TService.Create }

constructor TService.Create(const AName : String);
begin
  inherited Create();
  ///

  FName := AName;

  FServiceManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if FServiceManager = 0 then
    raise ES7WindowsException.Create('OpenSCManager');
end;

{ TService.Destroy }

destructor TService.Destroy();
begin
  if FServiceManager <> 0 then
    CloseServiceHandle(FServiceManager);

  ///
  inherited Destroy();
end;

end.
