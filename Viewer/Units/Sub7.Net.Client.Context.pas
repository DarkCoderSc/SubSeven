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

unit Sub7.Net.Client.Context;

interface

uses System.Classes, Sub7.OpenSSL.TLS.Context, System.SyncObjs;

type
  TSub7ClientContext = class(TOpenSSLContext)
  private
    FRemoteAddress : String;
    FRemotePort    : Word;
    FSessionId     : String;
    FPassword      : String;
  public
    {@C}
    constructor Create(const ARemoteAddress : String; const ARemotePort : Word; const ACertFile : AnsiString; const APassword : String = ''); overload;
    destructor Destroy(); override;

    {@G}
    property RemoteAddress : String read FRemoteAddress;
    property RemotePort    : Word   read FRemotePort;

    {@G/S}
    property SessionId : String read FSessionId write FSessionId;
    property Password  : String read FPassword  write FPassword;
  end;

implementation

uses System.SysUtils;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TSub7ClientContext.Create(const ARemoteAddress : String; const ARemotePort : Word; const ACertFile : AnsiString; const APassword : String = '');
begin
  inherited Create(cmClient, ACertFile);
  ///

  FRemoteAddress := ARemoteAddress;
  FRemotePort    := ARemotePort;
  FSessionId     := '';
  FPassword      := APassword;
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TSub7ClientContext.Destroy();
begin

  ///
  inherited Destroy();
end;

end.
