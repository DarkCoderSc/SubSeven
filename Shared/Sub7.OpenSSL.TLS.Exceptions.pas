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

unit Sub7.OpenSSL.TLS.Exceptions;

interface

uses System.SysUtils;

type
  ETLSSocketException = class(Exception)
  private
    FWSALastError : Integer;
  public
    {@C}
    constructor Create(); overload;

    {@G}
    property WSALastError : Integer read FWSALastError;
  end;

  EOpenSSLBaseException = class(Exception)
  private
    FErrorCode   : Integer;
    FErrorString : AnsiString;
  public
    {@C}
    constructor Create(); overload;
    constructor Create(pSSL : Pointer); overload;

    {@G}
    property ErrorCode   : Integer    read FErrorCode;
    property ErrorString : AnsiString read FErrorString;
  end;

  EOpenSSLLibraryException = class(Exception);

implementation

uses Winapi.Winsock2, Sub7.OpenSSL.Headers, Winapi.Windows;

{+++ ETLSSocketException +++}

{ ETLSSocketException.Create }

constructor ETLSSocketException.Create();
var AFormatedMessage : String;
begin
  FWSALastError := WSAGetLastError();

  AFormatedMessage := Format('%s.', [
      SysErrorMessage(FWSALastError)
  ]);

  ///
  inherited Create(AFormatedMessage);
end;

{+++ EOpenSSLBaseException +++}

{ EOpenSSLBaseException.Create }

constructor EOpenSSLBaseException.Create();
var AFormatedMessage : String;
begin
  FErrorCode := ERR_get_error();

  SetLength(FErrorString, 256);

  ERR_error_string_n(FErrorCode, PAnsiChar(FErrorString), 256);

  AFormatedMessage := Format('OpenSSL Error: last_error=[%d], error_str=[%s].', [
      FErrorCode,
      FErrorString
  ]);

  ///
  inherited Create(AFormatedMessage);
end;

constructor EOpenSSLBaseException.Create(pSSL : Pointer);
var AFormatedMessage : String;
begin
  SSL_get_error(pSSL, FErrorCode);

  FErrorString := 'Err';
  //SetLength(FErrorString, 256);

  //ERR_error_string_n(FErrorCode, PAnsiChar(FErrorString), 256);

  AFormatedMessage := Format('OpenSSL SSL Error: last_error=[%d], error_str=[%s].', [
      FErrorCode,
      FErrorString
  ]);

  ///
  inherited Create(AFormatedMessage);
end;

end.
