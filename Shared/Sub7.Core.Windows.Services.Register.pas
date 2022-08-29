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

unit Sub7.Core.Windows.Services.Register;

interface

uses Winapi.WinSvc, System.Classes, Sub7.Core.Windows.Services.Structure, 
     Sub7.Core.Windows.Services.Service;

function CreateWindowsService(const AName, ADisplayName, AImagePath : String; const AStartType : TServiceStartType = sstAutoStart; const ADescription : String = '') : TService;

implementation

uses Sub7.Core.Exceptions;

{ _.CreateWindowsService }

function CreateWindowsService(const AName, ADisplayName, AImagePath : String; const AStartType : TServiceStartType = sstAutoStart; const ADescription : String = '') : TService;
var AServiceManager : THandle;
    AServiceHandle  : THandle;
begin
  result := nil;
  ///
  
  AServiceManager := OpenSCManager(nil, nil, SC_MANAGER_CREATE_SERVICE);
  if AServiceManager = 0 then
    raise ES7WindowsException.Create('OpenSCManager');
  try    
    AServiceHandle := CreateService(
                                      AServiceManager,
                                      PWideChar(AName),
                                      PWideChar(ADisplayName),
                                      0,
                                      SERVICE_WIN32_OWN_PROCESS,
                                      GetServiceStartType_Native(AStartType),
                                      SERVICE_ERROR_NORMAL,
                                      PWideChar(AImagePath),
                                      nil,
                                      0,
                                      nil,
                                      nil,
                                      nil
    );

    if AServiceHandle = 0 then
      raise ES7WindowsException.Create('CreateService');

    CloseServiceHandle(AServiceHandle); // Important to unlock service

    result := TService.Create(AName);

    if ADescription <> '' then
      result.Description := ADescription;
  finally
    if AServiceManager <> 0 then
      CloseServiceHandle(AServiceManager);
  end;
end;

end.
