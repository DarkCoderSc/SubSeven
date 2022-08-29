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

unit uFormTransfer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.CaptionBar, Sub7.Viewer.VCL.SubSevenForm, S7Panel, Sub7.Core.Protocol,
  Vcl.StdCtrls, Vcl.Samples.Gauges, Sub7.Viewer.VCL.Button, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  VirtualTrees, S7Gauge, ___S7ContextWindow;

type
  TFormTransfer = class(TS7ContextWindow)
    SubSevenForms: TS7Form;
    CaptionBar: TS7CaptionBar;
    PanelClient: TS7Panel;
    PanelProgress: TS7Panel;
    LabelOrigin: TLabel;
    LabelDestination: TLabel;
    PanelFooter: TS7Panel;
    ButtonRight: TS7Button;
    PanelIcon: TS7Panel;
    ImageIcon: TImage;
    ButtonLeft: TS7Button;
    GaugeProgress: TS7Gauge;
    LabelEstimationTime: TLabel;
    TimerETA: TTimer;
    procedure FormShow(Sender: TObject);
    procedure ButtonLeftClick(Sender: TObject);
    procedure ButtonRightClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    FNode : PVirtualNode;
  protected
    {@M}
    procedure DoResize(); override;
  public
    {@C}
    constructor Create(AOwner : TComponent; const ANode : PVirtualNode); overload;

    {@M}
    procedure UpdateAll();
    procedure UpdateStatus();
    procedure UpdateProgress();
  end;

var
  FormTransfer: TFormTransfer;

implementation

uses uFormMain, Sub7.Core.FileSystem.Enum, uFormQueue, Sub7.Core.Utils,
     Sub7.Core.FileSystem.Utils;

{$R *.dfm}

procedure TFormTransfer.UpdateAll();
begin
  self.UpdateStatus();
  self.UpdateProgress();
end;

procedure TFormTransfer.UpdateProgress();
var pData : PTreeData;
    AText : String;
    AETA  : String;
begin
  if not self.Visible then
    Exit();
  ///

  if not Assigned(FNode) then
    Exit();

  pData := FNode.GetData();

  if not Assigned(pData) then
    Exit();

  if pData^.Status <> tsProgress then
    Exit();

  AText := Format('%s/ %s, %d%%', [
                                    FormatSize(pData^.WorkCount),
                                    FormatSize(pData^.Size),
                                    pData^.Progress
  ]);

  if pData^.bps > 0 then
    AETA := Format('eta: %s/s, %s', [
                                        FormatSize(pData^.bps),
                                        TimeSince((pData^.Size - pData^.WorkCount) div pData^.bps)
    ])
  else
    AETA := 'eta: n/a';

  LabelEstimationTime.Caption := AETA;

  GaugeProgress.Text     := AText;
  GaugeProgress.Progress := pData^.Progress;
end;

procedure TFormTransfer.UpdateStatus();
var pData : PTreeData;
begin
  if not self.Visible then
    Exit();
  ///

  if not Assigned(FNode) then
    Exit();

  pData := FNode.GetData;

  if not Assigned(pData) then
    Exit();

  ButtonLeft.Visible    := False;
  ButtonRight.Value     := -1;
  GaugeProgress.Progress := 0;
  GaugeProgress.Hint     := '';
  ///

  case pData^.Status of
    tsQueue, tsError : begin
      ButtonLeft.Visible := True;
    end;
  end;

  case pData^.Status of
    tsQueue : begin
      ButtonLeft.Caption  := 'Delete';
      ButtonRight.Caption := 'Start';
      ButtonRight.Value   := 0;

      GaugeProgress.State  := gsNormal;
      GaugeProgress.Mode   := gmMarquee;
      GaugeProgress.Text   := 'on queue...';
    end;

    tsProgress : begin
      ButtonRight.Caption := 'Stop';
      ButtonRight.Value   := 1;

      GaugeProgress.State  := gsNormal;
      GaugeProgress.Mode   := gmProgressBar;
      GaugeProgress.Text   := '0%';
    end;

    tsError : begin
      ButtonLeft.Caption  := 'Delete';
      ButtonRight.Caption := 'Retry';
      ButtonRight.Value   := 2;

      GaugeProgress.State  := gsError;
      GaugeProgress.Mode   := gmProgressBar;
      GaugeProgress.Text   := pData^.LastError;
      GaugeProgress.Hint   := GaugeProgress.Text;
    end;
  end;

  ///
  self.Resize();
end;

procedure TFormTransfer.ButtonLeftClick(Sender: TObject);
begin
  FormQueue.DeleteTransfer(FNode);
end;

procedure TFormTransfer.ButtonRightClick(Sender: TObject);
begin
  case TS7Button(Sender).Value of
    0 : FormQueue.StartTransfer(FNode);
    1 : FormQueue.StopTransfer(FNode);
    2 : FormQueue.RetryTransfer(FNode);
  end;
end;

constructor TFormTransfer.Create(AOwner : TComponent; const ANode : PVirtualNode);
var AIcon : TIcon;
    pData : PTreeData;
begin
  inherited Create(AOwner);
  ///

  FNode := ANode;

  pData := FNode.GetData;

  LabelOrigin.Caption      := Format('origin: %s', [pData^.Origin]);
  LabelDestination.Caption := Format('destination: %s', [pData^.Destination]);

  LabelOrigin.Hint      := LabelOrigin.Caption;
  LabelDestination.Hint := LabelDestination.Caption;

  case pData^.Direction of
    tdDownload : CaptionBar.Caption := Format('download: %s', [ExtractFileName(pData^.Origin)]);
    tdUpload   : CaptionBar.Caption := Format('upload: %s', [ExtractFileName(pData^.Origin)]);
  end;

  AIcon := TIcon.Create();
  try
    FormMain.ImageSystemBig.GetIcon(SystemFileIcon('.' + ExtractFileExt(pData^.Origin)), AIcon);

    self.ImageIcon.Picture.Assign(AIcon);
  finally
    if Assigned(AIcon) then
      FreeAndNil(AIcon);
  end;

  DoResize();
end;

procedure TFormTransfer.DoResize();
begin
  inherited;
  ///

  if ButtonLeft.Visible then begin
    ButtonLeft.Left  := (PanelFooter.Width div 2) - 4 - ButtonLeft.Width;
    ButtonRight.Left := ButtonLeft.Left + ButtonLeft.Width + 8;
  end else
    ButtonRight.Left := (PanelFooter.Width div 2) - (ButtonRight.Width div 2);

  ButtonLeft.Top   := (PanelFooter.Height div 2) - (ButtonLeft.Height div 2);
  ButtonRight.Top  := ButtonLeft.Top;
end;

procedure TFormTransfer.FormCreate(Sender: TObject);
begin
  self.Constraints.MinWidth  := Width;
  self.Constraints.MinHeight := Height;
end;

procedure TFormTransfer.FormHide(Sender: TObject);
begin
  self.TimerETA.Enabled := False;
end;

procedure TFormTransfer.FormShow(Sender: TObject);
begin
  self.UpdateAll();
end;

end.
