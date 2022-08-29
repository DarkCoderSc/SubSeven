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

unit Sub7.Core.Application.Env;

interface

uses System.SysUtils, System.IOUtils;

var APP_ENV_ReleaseName               : String = 'alpha 2';
    APP_ENV_ReleaseDate               : String = 'July 2021';

    APP_ENV_CurrentDirectory          : String = '';
    APP_ENV_FileName                  : String = '';
    APP_ENV_FileNameNoExt             : String = '';

    APP_ENV_ServerLogFileName         : String = 'Sub7Service.log';
    APP_ENV_ServiceLogFile            : String = '';

    APP_ENV_ServerCertificateFileName : String = 'sub7server.pem';
    APP_ENV_ServerCertificateFile     : String = '';

    APP_ENV_ClientCertificateFileName : String = 'sub7client.pem';
    APP_ENV_ClientCertificateFile     : String = '';

    APP_ENV_ServerSvcName             : String = 'Sub7Legacy';
    APP_ENV_ServerSvcFileName         : String = 'Sub7Service.exe';
    APP_ENV_ServerSvcFile             : String = '';

    APP_ENV_HelperFileName            : String = 'Sub7Helper.exe';
    APP_ENV_HelperFile                : String = '';

    APP_ENV_ConfigFileName            : String = 'config.json';
    APP_ENV_ConfigFile                : String = '';

    APP_ENV_SecureDesktopFileName     : String = 'SecureDesktop.dll';
    APP_ENV_SecureDesktopFile         : String = '';

const PROJECT_NAME = 'SubSeven';
      PROJECT_GUID = '{498A4E76-24E3-4C62-AD85-3384B7B7E0BB}';
      EGG          = 'Y2ljYWRpYTMzMDE=';  // cicadia

      {$IFDEF DEBUG}
        SERVICE_SRC_PATH        = 'I:\SubSeven\SRC\Service\';
        HELPER_SRC_PATH         = 'I:\SubSeven\SRC\Helper\';
        TRAY_SRC_PATH           = 'I:\SubSeven\SRC\Tray\';
        VIEWER_SRC_PATH         = 'I:\SubSeven\SRC\Viewer\';
        SECURE_DESKTOP_SRC_PATH = 'I:\SubSeven\SRC\SecureDesktop\';
      {$ENDIF}

implementation

uses Sub7.Core.Exceptions, Winapi.Windows;

{$IFDEF DEBUG}
  { _.GetCompiledPath }

  function GetCompiledPath(const APath : String) : String;
  const PATH_TEMPLATE = '%sWin%s\Debug\';
  var AProcArch : String;
  begin
    {$IFDEF WIN64}
      AProcArch := '64';
    {$ELSE}
      AProcArch := '32';
    {$ENDIF}

    ///
    result := Format(PATH_TEMPLATE, [IncludeTrailingPathDelimiter(APath), AProcArch]);
  end;
{$ENDIF}

initialization
  APP_ENV_CurrentDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(0)));
  APP_ENV_FileName         := ExtractFileName(GetModuleName(0));
  APP_ENV_FileNameNoExt    := System.IOUtils.TPath.GetFileNameWithoutExtension(GetModuleName(0));

  { Define Static Defined Files }

  {$IFDEF DEBUG}
    APP_ENV_ServiceLogFile := GetCompiledPath(SERVICE_SRC_PATH);
  {$ELSE}
    APP_ENV_ServiceLogFile := APP_ENV_CurrentDirectory;
  {$ENDIF}
  APP_ENV_ServiceLogFile := APP_ENV_ServiceLogFile + APP_ENV_ServerLogFileName;

  ///

  {$IFDEF DEBUG}
    APP_ENV_ServerCertificateFile := GetCompiledPath(SERVICE_SRC_PATH);
  {$ELSE}
    APP_ENV_ServerCertificateFile := APP_ENV_CurrentDirectory;
  {$ENDIF}
  APP_ENV_ServerCertificateFile := APP_ENV_ServerCertificateFile + APP_ENV_ServerCertificateFileName;

  ///

  {$IFDEF DEBUG}
    APP_ENV_ServerSvcFile := GetCompiledPath(SERVICE_SRC_PATH);
  {$ELSE}
    APP_ENV_ServerSvcFile := APP_ENV_CurrentDirectory;
  {$ENDIF}
  APP_ENV_ServerSvcFile := APP_ENV_ServerSvcFile + APP_ENV_ServerSvcFileName;

  ///

  {$IFDEF DEBUG}
    APP_ENV_HelperFile := GetCompiledPath(HELPER_SRC_PATH);
  {$ELSE}
    APP_ENV_HelperFile := APP_ENV_CurrentDirectory;
  {$ENDIF}
  APP_ENV_HelperFile := APP_ENV_HelperFile + APP_ENV_HelperFileName;

  ///

  {$IFDEF DEBUG}
    APP_ENV_ClientCertificateFile := GetCompiledPath(VIEWER_SRC_PATH);
  {$ELSE}
    APP_ENV_ClientCertificateFile := APP_ENV_CurrentDirectory;
  {$ENDIF}
  APP_ENV_ClientCertificateFile := APP_ENV_ClientCertificateFile + APP_ENV_ClientCertificateFileName;

  ///

  {$IFDEF DEBUG}
    APP_ENV_ConfigFile := GetCompiledPath(TRAY_SRC_PATH);
  {$ELSE}
    APP_ENV_ConfigFile := APP_ENV_CurrentDirectory;
  {$ENDIF}
  APP_ENV_ConfigFile := APP_ENV_ConfigFile + APP_ENV_ConfigFileName;

  ///

  {$IFDEF DEBUG}
    APP_ENV_SecureDesktopFile := GetCompiledPath(SECURE_DESKTOP_SRC_PATH);
  {$ELSE}
    APP_ENV_SecureDesktopFile := APP_ENV_CurrentDirectory;
  {$ENDIF}
  APP_ENV_SecureDesktopFile := APP_ENV_SecureDesktopFile + APP_ENV_SecureDesktopFileName;

  ///
  SetCurrentDir(APP_ENV_CurrentDirectory);

end.
