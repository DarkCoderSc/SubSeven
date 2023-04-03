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

unit Sub7.Thread.Net.Server.Peer.Transfer.Receive;

interface

uses System.Classes, XSuperObject, Sub7.Core.Protocol, Sub7.Thread.Net.Server.Peer.Base;

type
  TThreadTransferReceive = class(TSub7ServerPeerBase)
  private
    FDestination : String;
    FSize        : Int64;
    FFileStream  : TFileStream;

    {@M}
    procedure NegociateTransfer(const AData : ISuperObject);
    procedure UpdateUploadDestination();
    procedure Receive();
    procedure SendFileAcknowledgement();
  protected
    {@M}
    procedure OnPeerExecute(); override;
  public

  end;

implementation

uses Sub7.Core.Exceptions, Sub7.Core.Bundle, Sub7.Core.FileSystem.Utils, System.SysUtils,
     Sub7.OpenSSL.TLS.Exceptions, Sub7.Core.FileSystem.Enum;

{-------------------------------------------------------------------------------
  Tell client a new destination was choose, because overwrite was set to false
  and destination file was already existing.
-------------------------------------------------------------------------------}
procedure TThreadTransferReceive.UpdateUploadDestination();
var AData : ISuperObject;
begin
  AData := TSuperObject.Create();

  AData.S['destination'] := FDestination;

  FIOHandler.SendCommand(tcUpdateInformation, AData);
end;

{-------------------------------------------------------------------------------
  Receive Client Negociation
-------------------------------------------------------------------------------}
procedure TThreadTransferReceive.NegociateTransfer(const AData : ISuperObject);
var AOverwrite   : Boolean;
    ADestination : String;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('destination') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['destination']));

  ADestination := SecurePath(AData.S['destination']);

  if not AData.Contains('file_size') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['file_size']));

  FSize := AData.I['file_size'];

  AOverwrite := True;
  if AData.Contains('overwrite') then
    AOverwrite := AData.B['overwrite'];

  if FSize <= 0 then
    raise ES7InvalidDataException.Create('Invalid stream size.');

  FDestination := UniqueFileName(ADestination, AOverwrite);

  FFileStream := TFileStream.Create(FDestination, fmShareExclusive or fmCreate);

  FFileStream.Position := 0;

  if String.Compare(FDestination, ADestination, True) <> 0 then
    self.UpdateUploadDestination()
  else
    FIOHandler.SendCommand(uscUpload);
end;

{-------------------------------------------------------------------------------
  Receive Stream
-------------------------------------------------------------------------------}
procedure TThreadTransferReceive.Receive();
var ABuffer    : array of byte;
    ABytesRead : Cardinal;
    AChunkSize : Cardinal;
    ACompleted : Boolean;
begin
  if not Assigned(FFileStream) then
    raise Exception.Create(ERR_MISSING_INSTANCE);
  ///
  try
    if (FSize <= S7_PAQUET_SIZE) then begin
      { Receive in one chunk }
      SetLength(ABuffer, FSize);

      FIOHandler.recv(PByte(ABuffer), Length(ABuffer));

      FFileStream.WriteBuffer(ABuffer[0], Length(ABuffer));
    end else begin
      { Receive multiple chunks }
      ABytesRead := 0;
      repeat
        AChunkSize := (FSize - ABytesRead);

        if AChunkSize >= S7_PAQUET_SIZE then
          AChunkSize := S7_PAQUET_SIZE;

        if AChunkSize <> Length(ABuffer) then
          SetLength(ABuffer, AChunkSize);

        FIOHandler.Recv(PByte(ABuffer), Length(ABuffer));

        FFileStream.WriteBuffer(ABuffer[0], Length(ABuffer));

        Inc(ABytesRead, Length(ABuffer));

        ///
        ACompleted := (ABytesRead >= FSize);
      until ACompleted or self.Terminated;

      if not ACompleted then begin
        DeleteFile(FDestination);

        raise ES7IncompleteDataException.Create(
          Format('Receive stream incomplete, %d/%d transfered.', [
                                                                    FFileStream.Position,
                                                                    FFileStream.Size
          ])
        );
      end;
    end;
  finally
    SetLength(ABuffer, 0);

    if Assigned(FFileStream) then
      FreeAndNil(FFileStream);
  end;
end;

{-------------------------------------------------------------------------------
  Send file information to client to validate it exists
-------------------------------------------------------------------------------}
procedure TThreadTransferReceive.SendFileAcknowledgement();
var AFileInfo : TFileInformation;
begin
  if not GetFileInfo(FDestination, AFileInfo) then
    raise ES7FileException.Create(Format(ERR_FILEINFO, [FDestination]));

  FIOHandler.SendCommand(tcFileInformation, AFileInfo.Serialize());
end;

{-------------------------------------------------------------------------------
  ___execute
-------------------------------------------------------------------------------}
procedure TThreadTransferReceive.OnPeerExecute();
var ACommand : TS7Command;
    AData    : ISuperObject;
begin
  if not Assigned(FIOHandler) then
    Exit();
  ///
  try
    FIOHandler.GetCommand(ACommand, AData);

    if (ACommand <> tcNegociate) then
      raise ES7ProtocolException.Create(ERR_UNEXPECTED_CMD);

    self.NegociateTransfer(AData);

    self.Receive();

    self.SendFileAcknowledgement();
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
