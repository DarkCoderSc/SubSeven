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

unit Sub7.TLS.IOHandler;

interface

uses System.Classes, Sub7.OpenSSL.TLS.IOHandler, Winapi.Windows, Sub7.Core.Protocol, XSuperObject,
     System.SysUtils;

type
  TSub7TLSIOHandler = class(TOpenSSL_TLSIOHandler)
  private

  public
    {@M}
    procedure SendException(const AException : Exception);
    procedure SendCommand(const ACommand : TS7Command; AJsonData : ISuperObject = nil);
    procedure GetCommand(var ACommand : TS7Command; var AJsonData : ISuperObject); overload;
    procedure GetCommand(var ACommand : TS7Command); overload;

    procedure SendAck();
    procedure WaitForAck();
  end;

implementation

uses Sub7.Core.Exceptions, Sub7.Core.Bundle;

{-------------------------------------------------------------------------------
  Send Sub7 Exception to peer
-------------------------------------------------------------------------------}
procedure TSub7TLSIOHandler.SendException(const AException : Exception);
var AJsonData : ISuperObject;
begin
  if not Assigned(AException) then
    Exit();
  ///

  AJsonData := TSuperObject.Create();

  AJsonData.V['message'] := AException.Message;

  ///
  SendCommand(s7cServerException, AJsonData);
end;

{-------------------------------------------------------------------------------
  Send Sub7 Command to peer
-------------------------------------------------------------------------------}
procedure TSub7TLSIOHandler.SendCommand(const ACommand : TS7Command; AJsonData : ISuperObject = nil);
var AJsonCommand : ISuperObject;
begin
  AJsonCommand := TSuperObject.Create();

  AJsonCommand.V['command'] := Integer(ACommand);

  if Assigned(AJsonData) then
    AJsonCommand.O['data'] := AJsonData;

  ///
  self.SendString(AJsonCommand.AsJSON());
end;

{-------------------------------------------------------------------------------
  Receive Sub7 Command from peer
-------------------------------------------------------------------------------}

procedure TSub7TLSIOHandler.GetCommand(var ACommand : TS7Command; var AJsonData : ISuperObject);
var AJsonCommand : ISuperObject;
    AJsonString  : String;
    AMessage     : String;
begin
  AJsonData := nil;
  ACommand  := s7cUnknown;
  ///

  self.ReceiveString(AJsonString);

  AJsonCommand := TSuperObject.Create(AJsonString);

  {
    Read command index
  }
  if not AJsonCommand.Contains('command') then
    raise ES7ParseException.Create(Format(ERR_MISSING_PARAMETER, ['command']));
  ///

  ACommand := TS7Command(AJsonCommand.V['command']);

  {
    Read optionnal data that comes with
  }
  if AJsonCommand.Contains('data') then
    AJsonData := AJsonCommand.O['data'];

  {
    Handle Action Exception (Only client)
  }
  if (ACommand = s7cServerException) then begin
    if AJsonData.Contains('message') then
      AMessage := AJsonData.V['message'];

    raise ES7ServerException.Create(AMessage);
  end;
end;

{ TSub7TLSIOHandler.GetCommand }

procedure TSub7TLSIOHandler.GetCommand(var ACommand : TS7Command);
var AData : ISuperObject;
begin
  AData := nil;

  self.GetCommand(ACommand, AData);
end;

{ TSub7TLSIOHandler.SendAck }

procedure TSub7TLSIOHandler.SendAck();
begin
  self.SendCommand(s7cAck);
end;

{ TSub7TLSIOHandler.ReceiveAck }

procedure TSub7TLSIOHandler.WaitForAck();
var ACommand : TS7Command;
begin
  self.GetCommand(ACommand);

  if ACommand <> s7cAck then
    raise ES7ProtocolException.Create(ERR_UNEXPECTED_CMD);
end;

end.
