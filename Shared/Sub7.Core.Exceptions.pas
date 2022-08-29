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

unit Sub7.Core.Exceptions;

interface

uses System.SysUtils;

type
  TAuthenticationError = (aeError, aeBadLogin, aeCanceled, aePeerCertificate);

  ES7Exception = class(Exception);

  ES7WindowsException = class(ES7Exception)
  private
    FLastError : Integer;
  public
    {@C}
    constructor Create(const WinAPI : String); overload;

    {@G}
    property LastError : Integer read FLastError;
  end;

  ES7AuthenticationException = class(ES7Exception)
  private
    FReason : TAuthenticationError;
  public
    {@C}
    constructor Create(const AReason : TAuthenticationError; const ADetail : String = ''); overload;

    {@G}
    property Reason : TAuthenticationError read FReason;
  end;

  ES7FileDoesNotExistsException = class(ES7Exception)
  public
    constructor Create(const AFileName : String); overload;
  end;

  ES7ProtocolException           = class(ES7Exception);
  ES7VersionException            = class(ES7Exception);
  ES7ParseException              = class(ES7Exception);
  ES7InvalidDirectory            = class(ES7Exception);
  ES7BrowseDirectoryException    = class(ES7Exception);
  ES7TransferException           = class(ES7Exception);
  ES7InvalidDataException        = class(ES7Exception);
  ES7IncompleteDataException     = class(ES7Exception);
  ES7FileException               = class(ES7Exception);
  ES7DeserializationError        = class(ES7Exception);
  ES7ServerException             = class(ES7Exception);
  ES7SystemException             = class(ES7Exception);
  ES7MissingProcess              = class(ES7Exception);
  ES7TimeoutException            = class(ES7Exception);
  ES7HelperException             = class(ES7Exception);
  ES7FormException               = class(ES7Exception);
  ES7ServiceNotFound             = class(ES7Exception);
  ES7AccessDenied                = class(ES7Exception);
  ES7ServiceException            = class(ES7Exception);
  ES7IOException                 = class(ES7Exception);
  ES7NotEnoughPrivilegeException = class(ES7Exception);
  ES7BadLoginException           = class(ES7Exception);
  ES7CastException               = class(ES7Exception);

implementation

uses WinAPI.Winsock2;

{ ES7FileDoesNotExistsException.Create }

constructor ES7FileDoesNotExistsException.Create(const AFileName : String);
begin
  inherited Create(Format('"%s" does not exists.', [AFileName]));
end;

{ ES7AuthenticationException.Create }

constructor ES7AuthenticationException.Create(const AReason : TAuthenticationError; const ADetail : String = '');
var AFormatedMessage : String;
begin
  FReason := AReason;
  ///

  AFormatedMessage := '';
  case AReason of
    aeError           : AFormatedMessage := 'Authentication Error';
    aeBadLogin        : AFormatedMessage := 'Bad Login';
    aeCanceled        : AFormatedMessage := 'Authentication Canceled';
    aePeerCertificate : AFormatedMessage := 'Peer Certificate';
  end;

  if ADetail <> '' then
    AFormatedMessage := Format('%s: "%s"', [AFormatedMessage, ADetail]);

  ///
  inherited Create(AFormatedMessage);
end;

{ ES7WindowsException.Create }

constructor ES7WindowsException.Create(const WinAPI : String);
var AFormatedMessage : String;
begin
  FLastError := GetLastError();

  AFormatedMessage := Format('___%s: last_err=%d, last_err_msg="%s".', [
      WinAPI,
      FLastError,
      SysErrorMessage(FLastError)
  ]);

  ///
  inherited Create(AFormatedMessage);
end;


end.
