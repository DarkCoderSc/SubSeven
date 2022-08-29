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

unit Sub7.Thread.Net.Client.Transfer.Upload;

interface

uses System.Classes, uFormQueue, Sub7.Core.Protocol, VirtualTrees, XSuperObject,
     System.SysUtils, Sub7.Thread.Net.Client.Transfer, Sub7.Net.Client.Context;

type
  TThreadTransferUpload = class(TThreadTransfer)
  private
    FOverwrite   : Boolean;
    FUploadKind  : TS7UploadKind;

    {@M}
    procedure NegociateTransfer();
    procedure UpdateUploadDestination(const AData : ISuperObject);
    procedure Upload();
    procedure ReceiveFileAcknowledgement(const AData : ISuperObject);
  protected
    {@M}
    procedure Transfer(); override;
  public
    {@C}
    constructor Create(const AContext : TSub7ClientContext; const ANode : PVirtualNode; const AUploadKind : TS7UploadKind); overload;
  end;

implementation

uses Sub7.Core.Exceptions, Sub7.Core.Bundle, System.Math, Winapi.Windows, Sub7.Core.FileSystem.Utils,
     uFormMain, System.Diagnostics, Sub7.Core.FileSystem.Enum;

{-------------------------------------------------------------------------------
  File Ack received
-------------------------------------------------------------------------------}
procedure TThreadTransferUpload.ReceiveFileAcknowledgement(const AData : ISuperObject);
var AFileInfo : TFileInformation;
begin
  if not Assigned(AData) then
    raise Exception.Create(ERR_MISSING_INSTANCE);
  ///

  AFileInfo := TFileInformation.Create(AData);

  case FUploadKind of
    {
      Notify File Manager(s)
    }
    ukFileManager : begin
      SafeSynchronize(procedure begin
        AData.S['Path'] := ExtractFilePath(FDestination);
        ///

        FormMain.ReactControlWindows(uscUpload, AData);
      end);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TThreadTransferUpload.Create(const AContext : TSub7ClientContext; const ANode : PVirtualNode; const AUploadKind : TS7UploadKind);
var pData : PTreeData;
begin
  inherited Create(AContext, ANode);
  ///

  FUploadKind := AUploadKind;

  SafeSynchronize(procedure begin
    pData := FNode.GetData;

    FOverwrite := pData^.Overwrite;
  end);
end;

{-------------------------------------------------------------------------------
  Tell server information about the file we want to upload
-------------------------------------------------------------------------------}
procedure TThreadTransferUpload.NegociateTransfer();
var AData : ISuperObject;
begin
  AData := TSuperObject.Create();

  AData.S['destination'] := FDestination;
  AData.I['file_size']   := FFileStream.Size;
  AData.B['overwrite']   := FOverwrite;

  FIOHandler.SendCommand(tcNegociate, AData);
end;

{-------------------------------------------------------------------------------
  In case server rectify file destination (in the case overwrite is set to false
  and destination file already exists)
-------------------------------------------------------------------------------}
procedure TThreadTransferUpload.UpdateUploadDestination(const AData : ISuperObject);
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('destination') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['destination']));

  SafeSynchronize(procedure begin
    FormQueue.UpdateDestination(FNode, AData.S['destination']);
  end);

  ///
  self.Upload();
end;

{-------------------------------------------------------------------------------
  Start Upload Stream
-------------------------------------------------------------------------------}
procedure TThreadTransferUpload.Upload();
var ABuffer          : array of byte;
    AChunkSize       : Cardinal;
    ACompleted       : Boolean;
    ABytesPerSeconds : Int64;
    AStopwatch       : TStopwatch;
begin
  try
    if (FFileStream.Size <= S7_PAQUET_SIZE) then begin
      SetLength(ABuffer, FFileStream.Size);

      FFileStream.ReadBuffer(ABuffer[0], Length(ABuffer));

      FIOHandler.Send(PByte(ABuffer), Length(ABuffer));
    end else begin
      ABytesPerSeconds := 0;
      AStopwatch       := TStopwatch.StartNew();
      ACompleted       := False;
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

        { Calculate bytes per seconds }
        Inc(ABytesPerSeconds, Length(ABuffer));
        if AStopwatch.ElapsedMilliseconds >= 500 then begin
          SafeSynchronize(procedure begin
            FormQueue.SetProgress(FNode, FFileStream.Position, (ABytesPerSeconds * 2));
          end);

          ///
          AStopwatch := TStopwatch.StartNew;
          ABytesPerSeconds := 0;
        end;

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
  Transfer
-------------------------------------------------------------------------------}
procedure TThreadTransferUpload.Transfer();
var ACommand : TS7Command;
    AData    : ISuperObject;
begin
  self.NegociateTransfer();

  FIOHandler.GetCommand(ACommand, AData);

  case ACommand of
    tcUpdateInformation : begin
      self.UpdateUploadDestination(AData);
    end;

    uscUpload : begin
      self.Upload();
    end

    else
      raise ES7ProtocolException.Create(ERR_UNEXPECTED_CMD);
  end;

  { Receive new file information }
  FIOHandler.GetCommand(ACommand, AData);

  case ACommand of
    tcFileInformation : begin
      self.ReceiveFileAcknowledgement(AData);
    end;

    else
      raise ES7ProtocolException.Create(ERR_UNEXPECTED_CMD);
  end;
end;

end.
