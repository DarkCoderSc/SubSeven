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

unit Sub7.Thread.Net.Client.Transfer.Download;

interface

uses System.Classes, Sub7.Core.Protocol, VirtualTrees, uFormQueue, System.SysUtils,
     XSuperObject, Sub7.Thread.Net.Client.Transfer;

type
  TThreadTransferDownload = class(TThreadTransfer)
  private
    FSize : Int64;

    {@M}
    procedure NegociateTransfer();
    procedure UpdateFileInformation(const AData : ISuperObject);
    procedure Download();
  protected
    {@M}
    procedure Transfer(); override;
  public
  end;

implementation

uses Sub7.Core.Exceptions, Sub7.Core.Bundle, System.Math, WinAPI.Windows, System.Diagnostics;

{-------------------------------------------------------------------------------
  Tell server information about the file we want to download
-------------------------------------------------------------------------------}
procedure TThreadTransferDownload.NegociateTransfer();
var AData : ISuperObject;
begin
  AData := TSuperObject.Create();

  AData.S['origin'] := FOrigin;

  FIOHandler.SendCommand(tcNegociate, AData);
end;

{-------------------------------------------------------------------------------
  Update File Information
-------------------------------------------------------------------------------}
procedure TThreadTransferDownload.UpdateFileInformation(const AData : ISuperObject);
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('file_size') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['file_size']));

  FSize := AData.I['file_size'];

  if FSize <= 0 then
    raise ES7Exception.Create('Can''t download an empty file.');

  SafeSynchronize(procedure begin
    FormQueue.SetFileSize(FNode, FSize);
  end);
end;

{-------------------------------------------------------------------------------
  Start File Transfer
-------------------------------------------------------------------------------}
procedure TThreadTransferDownload.Download();
var ABuffer          : array of byte;
    ABytesRead       : Cardinal;
    AChunkSize       : Cardinal;
    ACompleted       : Boolean;
    AStopwatch       : TStopwatch;
    ABytesPerSeconds : Int64;
begin
  try
    if (FSize <= S7_PAQUET_SIZE) then begin
      { Receive in one chunk }
      SetLength(ABuffer, FSize);

      FIOHandler.recv(PByte(ABuffer), Length(ABuffer));

      FFileStream.WriteBuffer(ABuffer[0], Length(ABuffer));
    end else begin
      { Receive multiple chunks }
      ABytesRead := 0;
      AStopwatch := TStopwatch.StartNew();
      ABytesPerSeconds := 0;
      repeat
        AChunkSize := (FSize - ABytesRead);

        if AChunkSize >= S7_PAQUET_SIZE then
          AChunkSize := S7_PAQUET_SIZE;

        if AChunkSize <> Length(ABuffer) then
          SetLength(ABuffer, AChunkSize);

        FIOHandler.Recv(PByte(ABuffer), Length(ABuffer));

        FFileStream.WriteBuffer(ABuffer[0], Length(ABuffer));

        Inc(ABytesRead, Length(ABuffer));

        { Calculate bytes per seconds }
        Inc(ABytesPerSeconds,Length(ABuffer));
        if AStopwatch.ElapsedMilliseconds >= 500 then begin
          SafeSynchronize(procedure begin
            FormQueue.SetProgress(FNode, ABytesRead, (ABytesPerSeconds * 2));
          end);

          ///
          AStopwatch := TStopwatch.StartNew;
          ABytesPerSeconds := 0;
        end;

        ///
        ACompleted := (ABytesRead >= FSize);
      until ACompleted or self.Terminated;

      if not ACompleted then begin
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
  end;
end;

{-------------------------------------------------------------------------------
  Transfer
-------------------------------------------------------------------------------}
procedure TThreadTransferDownload.Transfer();
var ACommand : TS7Command;
    AData    : ISuperObject;
begin
  self.NegociateTransfer();

  FIOHandler.GetCommand(ACommand, AData);

  if (ACommand <> tcUpdateInformation) then
    raise ES7ProtocolException.Create(ERR_UNEXPECTED_CMD);

  self.UpdateFileInformation(AData);

  FIOHandler.SendCommand(dscDownload);

  self.Download();
end;

end.
