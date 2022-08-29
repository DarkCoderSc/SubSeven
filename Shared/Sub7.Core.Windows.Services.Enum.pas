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

unit Sub7.Core.Windows.Services.Enum;

interface

uses System.Classes, Winapi.Windows, Winapi.WinSvc, Sub7.Core.OOP.Interfaces,
     XSuperObject, Generics.Collections, Sub7.Core.Windows.Services.Service,
     Sub7.Core.Windows.Services.Structure;

type
  TEnumService = class(TInterfacedObject, IS7Serializable, IS7Enumerator)
  private
    FServiceManager    : Cardinal;
    FItems             : TObjectList<TService>;

    FEnumServiceTypes  : TEnumServiceTypes;
    FEnumServiceStates : TEnumServiceStates;

    {@M}
    procedure SetEnumServiceTypes(const AValue : TEnumServiceTypes);
    procedure SetEnumServiceStates(const AValue : TEnumServiceStates);

    function ResolveEnumServiceTypes(const AServiceTypes : TEnumServiceTypes) : DWORD;
    function ResolveEnumServiceStates(const AServiceTypes : TEnumServiceStates) : DWORD;
  public

    {@C}
    constructor Create(const AOwnsObject : Boolean; const ARefresh : Boolean = True);
    destructor Destroy(); override;

    {@M}
    function Refresh() : Integer;
    procedure Clear();

    function Serialize() : ISuperObject;
    procedure DeSerialize(const ASerializedObject : ISuperObject);

    {@G/S}
    property EnumServiceTypes  : TEnumServiceTypes  read FEnumServiceTypes  write SetEnumServiceTypes;
    property EnumServiceStates : TEnumServiceStates read FEnumServiceStates write SetEnumServiceStates;
  end;

implementation

uses Sub7.Core.Exceptions, System.SysUtils;

{ TEnumService.Create }

constructor TEnumService.Create(const AOwnsObject : Boolean; const ARefresh : Boolean = True);
begin
  inherited Create();
  ///

  FItems := TObjectList<TService>.Create(AOwnsObject);

  FServiceManager := OpenSCManager(nil, nil, SC_MANAGER_ENUMERATE_SERVICE);

  if FServiceManager = 0 then
    raise ES7WindowsException.Create('OpenSCManager');

  SetEnumServiceTypes([]);  // all
  SetEnumServiceStates([]); // all

  if ARefresh then
    self.Refresh();
end;

{ TEnumService.Destroy }

destructor TEnumService.Destroy();
begin
  if FServiceManager <> 0 then
    CloseServiceHandle(FServiceManager);

  if Assigned(FItems) then
    FreeAndNil(FItems);

  ///
  inherited Destroy();
end;

{ TEnumService.Refresh }

function TEnumService.Refresh() : Integer;
var ARequiredBufferSize : Cardinal;
    ABufferSize         : Cardinal;
    AServiceCount       : Cardinal;
    AResumeHandle       : Cardinal;

    pServices           : PEnumServiceStatus;
    pService            : PEnumServiceStatus;
    i                   : Integer;

    AService            : TService;
begin
  if not Assigned(FItems) or (FServiceManager = 0) then
    Exit();

  self.Clear();

  AServiceCount       := 0;
  ARequiredBufferSize := 0;
  AResumeHandle       := 0;

  // First call will retrieve the required buffer size to handle services informations
  if not EnumServicesStatus(
                              FServiceManager,
                              self.ResolveEnumServiceTypes(self.FEnumServiceTypes),
                              self.ResolveEnumServiceStates(self.FEnumServiceStates),
                              nil,
                              0,
                              ARequiredBufferSize,
                              AServiceCount,
                              AResumeHandle
  ) then begin
    if GetLastError() <> ERROR_MORE_DATA then
      raise ES7WindowsException.Create('(1)EnumServicesStatus');
  end;

  ABufferSize := ARequiredBufferSize;

  GetMem(pServices, ABufferSize);
  try
    AServiceCount := 0;
    AResumeHandle := 0;

    // This time we grab service information in our new allocated pointer
    if not EnumServicesStatus(
                                FServiceManager,
                                self.ResolveEnumServiceTypes(self.FEnumServiceTypes),
                                self.ResolveEnumServiceStates(self.FEnumServiceStates),
                                pServices,
                                ARequiredBufferSize,
                                ARequiredBufferSize,
                                AServiceCount,
                                AResumeHandle
    ) then
      raise ES7WindowsException.Create('(2)EnumServicesStatus');
    ///

    pService := pServices;
    for I := 0 to AServiceCount -1 do begin
      try
        FItems.Add(TService.Create(String(pService^.lpServiceName)));
      finally
        Inc(pService);
      end;
    end;

  finally
    FreeMem(pServices, ABufferSize);
  end;
end;

{ TEnumService.Clear }

procedure TEnumService.Clear();
begin
  if Assigned(FItems) then
    FItems.Clear();
end;

{ TEnumService.Serialize }

function TEnumService.Serialize() : ISuperObject;
begin
  ///
end;

{ TEnumService.DeSerialize }

procedure TEnumService.DeSerialize(const ASerializedObject : ISuperObject);
begin
  ///
end;

{ TEnumService.SetEnumServiceTypes }

procedure TEnumService.SetEnumServiceTypes(const AValue : TEnumServiceTypes);
begin
  if AValue = [] then
    FENumServiceTypes := [
                            estDriver,
                            estFileSystemDriver,
                            estKernelDriver,
                            estWin32,
                            estWin32OwnProcess,
                            estWin32ShareProcess
    ]
  else
    FENumServiceTypes := AValue;
end;

{ TEnumService.SetEnumServiceStates }

procedure TEnumService.SetEnumServiceStates(const AValue : TEnumServiceStates);
begin
  if AValue = [] then
    FEnumServiceStates := [
                              essActive,
                              essInactive
    ]
  else
    FEnumServiceStates := AValue;
end;

{ TEnumService.ResolveServiceTypes }

function TEnumService.ResolveEnumServiceTypes(const AServiceTypes : TEnumServiceTypes) : DWORD;
begin
  result := 0;
  ///

  if estDriver in AServiceTypes  then
    result := result or SERVICE_DRIVER;

  if estFileSystemDriver in AServiceTypes  then
    result := result or SERVICE_FILE_SYSTEM_DRIVER;

  if estKernelDriver in AServiceTypes  then
    result := result or SERVICE_KERNEL_DRIVER;

  if estWin32 in AServiceTypes  then
    result := result or SERVICE_WIN32;

  if estWin32OwnProcess in AServiceTypes  then
    result := result or SERVICE_WIN32_OWN_PROCESS;

  if estWin32ShareProcess in AServiceTypes  then
    result := result or SERVICE_WIN32_SHARE_PROCESS;
end;

{ TEnumService.ResolveServiceStates }

function TEnumService.ResolveEnumServiceStates(const AServiceTypes : TEnumServiceStates) : DWORD;
begin
  result := 0;
  ///

  if essActive in AServiceTypes then
    result := result or SERVICE_ACTIVE;

  if essInactive in AServiceTypes then
    result := result or SERVICE_INACTIVE;
end;

end.

