{******************************************************************************}
{                                                                              }
{ 	    	 ____             _     ____          _           ____               }
{     		|  _ \  __ _ _ __| | __/ ___|___   __| | ___ _ __/ ___|  ___         }
{ 	    	| | | |/ _` | '__| |/ / |   / _ \ / _` |/ _ \ '__\___ \ / __|        }
{ 	    	| |_| | (_| | |  |   <| |__| (_) | (_| |  __/ |   ___) | (__         }
{ 	    	|____/ \__,_|_|  |_|\_\\____\___/ \__,_|\___|_|  |____/ \___|        }
{         								                                									   }
{                           +++++++++++++++++++++                              }
{						                +       +           +                              }
{						                +       +           +                              }
{						                +    +++++++++      +                              }
{						                +            +      +                              }
{						                +            +      +                              }
{						                +++++++      +      +                              }
{						                +            +      +                              }
{						                +            +      +                              }
{						                +++++++++++++++++++++                              }
{								                  SubSeven Legacy                              }
{                                                                              }
{                                                                              }
{				           Author: DarkCoderSc (Jean-Pierre LESUEUR)                   }
{				           https://www.twitter.com/                                    }
{				           https://github.com/darkcodersc                              }
{				           License: Apache License 2.0                                 }
{                                                                              }
{                                                                              }
{	  Disclaimer:                                                                }
{  	-----------                                                                }
{		We are doing our best to prepare the content of this app and/or code.      }
{		However, The author cannot warranty the expressions and suggestions        }
{		of the contents, as well as its accuracy. In addition, to the extent       }
{		permitted by the law, author shall not be responsible for any losses       }
{		and/or damages due to the usage of the information on our app and/or       }
{		code.                                                                      }
{                                                                              }
{		By using our app and/or code, you hereby consent to our disclaimer         }
{		and agree to its terms.                                                    }
{                                                                              }
{		Any links contained in our app may lead to external sites are provided     }
{		for convenience only.                                                      }
{		Any information or statements that appeared in these sites or app or       }
{		files are not sponsored, endorsed, or otherwise approved by the author.    }
{		For these external sites, the author cannot be held liable for the         }
{		availability of, or the content located on or through it.                  }
{		Plus, any losses or damages occurred from using these contents or the      }
{		internet generally.                                                        }
{                                                                              }
{                                                                              }
{                                                                              }
{   I dedicate this work to my daughter.                                       }
{                                                                              }
{******************************************************************************}

unit uFormAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Sub7.Viewer.VCL.CaptionBar, Sub7.Viewer.VCL.SubSevenForm, S7Panel,
  ___S7BaseForm, Vcl.Imaging.pngimage, Vcl.ExtCtrls, BASS, Generics.Collections,
  Vcl.StdCtrls, S7ScrollingCredit, S7Timer, S7PaintScene;

type
  TFormAbout = class(TS7BaseForm)
    TimerScene: TS7Timer;
    PanelScene: TPanel;
    ScrollingCredits: TS7ScrollingCredit;
    Scene: TImage;
    CaptionBar: TS7CaptionBar;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure TimerSceneTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SceneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    {@M}
    procedure CreateParams(var AParams: TCreateParams); override;
    procedure CreateWindowHandle(const AParams: TCreateParams); override;
    procedure WndProc(var AMessage: TMessage); override;
  private
    FBassmod     : Boolean;
    FTune        : TResourceStream;
    FPlaying     : Boolean;
    FFirstShow   : Boolean;
    FMusicHandle : HMUSIC;

    FSceneFrames : TObjectList<TPngImage>;
    FFrameIndex  : Cardinal;

    {@M}
    function LoadTunes() : Boolean;
    procedure InitializeBassMod();
    function PlayTune() : Boolean;
    function LoadTune() : Boolean;
    procedure StopTune();
    procedure NextFrame();

    procedure LoadSceneMap();
    function SetBordelessShadow() : Boolean;

    procedure DoResize();
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

uses uFormMain, XSuperObject, Winapi.DwmApi, Winapi.UxTheme, Sub7.Core.Crypto.RC4,
     Sub7.Core.Application.Env, Sub7.Core.Windows.PE.Version;

{$R *.dfm}

procedure TFormAbout.WndProc(var AMessage: TMessage);
begin
  case AMessage.Msg of
    WM_DWMCOMPOSITIONCHANGED, WM_DWMNCRENDERINGCHANGED:
      if SetBordelessShadow() then
        AMessage.Result := 0;

    else
      inherited;
  end;
end;

procedure TFormAbout.SceneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture();
  SendMessage(self.Handle, WM_SYSCOMMAND, $F012, 0);
end;

function TFormAbout.SetBordelessShadow() : Boolean;
var AMargins : TMargins;
    APolicy  : Integer;
begin
  if TOSVersion.Major < 6 then
    Exit(False);
  ///

  APolicy := DWMNCRP_ENABLED;
  Result := Succeeded(DwmSetWindowAttribute(Handle, DWMWA_NCRENDERING_POLICY, @APolicy, SizeOf(Integer))) and DwmCompositionEnabled;

  if Result then begin
    AMargins.cxLeftWidth    := 1;
    AMargins.cxRightWidth   := 1;
    AMargins.cyTopHeight    := 1;
    AMargins.cyBottomHeight := 1;

    ///
    Result := Succeeded(DwmExtendFrameIntoClientArea(self.Handle, AMargins));
  end;
end;

procedure TFormAbout.CreateWindowHandle(const AParams: TCreateParams);
begin
  inherited;
  ///

  if TOSVersion.Major >= 6 then
    SetBordelessShadow();
end;

procedure TFormAbout.CreateParams(var AParams: TCreateParams);
begin
  inherited CreateParams(AParams);
  ///

  if TOSVersion.Major < 6 then begin
    AParams.Style             := WS_POPUP;
    AParams.WindowClass.Style := AParams.WindowClass.Style or CS_DROPSHADOW;
  end;

  AParams.ExStyle := AParams.ExStyle or WS_EX_APPWINDOW;

  AParams.WndParent := FormMain.Handle;
end;

procedure TFormAbout.DoResize();
begin
  self.ScrollingCredits.Left := (PanelScene.Width div 2) - (self.ScrollingCredits.Width div 2);
  self.ScrollingCredits.Top  := (PanelScene.Height - self.ScrollingCredits.Height);

  self.CaptionBar.Left       := 8;
  self.CaptionBar.Top        := 8;
  self.CaptionBar.Width      := PanelScene.Width - 16;
end;

procedure TFormAbout.NextFrame();
begin
  if not Assigned(FSceneFrames) then
    Exit();

  if FFrameIndex > FSceneFrames.Count -1 then
    FFrameIndex := 0;

  self.Scene.Picture.Assign(FSceneFrames.Items[FFrameIndex]);

  Inc(FFrameIndex);
end;

procedure TFormAbout.LoadSceneMap();
var AMapStream       : TMemoryStream;
    AHeaderSize      : Int64;
    AJson            : String;
    AFrameMap        : ISuperArray;
    I                : Integer;
    APng             : TPngImage;
    AFrameSize       : Int64;
    AFrame           : TMemoryStream;
    ARC4             : TRC4;
    AHeaderSignature : Int64;
    AResourceStream  : TResourceStream;

const
  HEADER_SIGNATURE = 2801202100014032014;

begin
  if not Assigned(FSceneFrames) then
    Exit();
  ///

  FSceneFrames.Clear();

  AMapStream := TMemoryStream.Create();
  try
    try
      AResourceStream := TResourceStream.Create(hInstance, 'about_textures', 'sub7');
      try
        AResourceStream.Position :=  0;

        AMapStream.LoadFromStream(AResourceStream);
      finally
        if Assigned(AResourceStream) then
          FreeAndNil(AResourceStream);
      end;
    except
      Exit();
    end;

    ///

    { Unprotect our about scenes }
    ARC4 := TRC4.Create('{7949D6B7-0FC4-4A6A-A597-035C80A8FCE3}');
    try
      try
        ARC4.Decrypt(AMapStream.Memory, AMapStream.Size);
      except
        Exit();
      end;
    finally
      if Assigned(ARC4) then
        FreeAndNil(ARC4);
    end;

    AMapStream.Position := 0;

    // Read Header Size
    AMapStream.Read(AHeaderSignature, SizeOf(Int64));

    if AHeaderSignature <> HEADER_SIGNATURE then
      Exit();

    AMapStream.Read(AHeaderSize, SizeOf(Int64));

    SetLength(AJson, AHeaderSize div SizeOf(WideChar));

    // Read Header
    AMapStream.Read(AJson[1], AHeaderSize);

    try
      AFrameMap := TSuperArray.Create(AJson);
    except
      Exit();
    end;

    Inc(AHeaderSize, SizeOf(Int64) * 2);

    // Extract Each Frames
    AFrame := TMemoryStream.Create();
    try
      for I := 0 to AFrameMap.Length -1 do begin
        AMapStream.Seek(AHeaderSize + AFrameMap.I[I], soBeginning);

        if I < (AFrameMap.Length -1) then
          AFrameSize := AHeaderSize + AFrameMap.I[I + 1] - AMapStream.Position
        else
          AFrameSize := AMapStream.Size - AMapStream.Position;

        // Load Frame
        AFrame.Clear();

        AFrame.Write(PByte(NativeUint(AMapStream.Memory) + AMapStream.Position)^, AFrameSize);

        AFrame.Position := 0;

        APng := TPngImage.Create();
        try
          APng.CheckCRC := False;

          APng.LoadFromStream(AFrame);
        except
          on E : EPngInvalidCrc do begin

          end;

          on E : Exception do begin
            if Assigned(APng) then
              FreeAndNil(APng);

            continue;
          end;
        end;

        ///
        FSceneFrames.Add(APng);
      end;
    finally
      if Assigned(AFrame) then
        FreeAndNil(AFrame);
    end;
  finally
    if Assigned(AMapStream) then
      FreeAndNil(AMapStream);
  end;

  ///
  TimerScene.Enabled := True;
end;

procedure TFormAbout.StopTune();
begin
  if FPlaying then
    BASS_MusicFree(FMusicHandle);
end;

procedure TFormAbout.TimerSceneTimer(Sender: TObject);
begin
  self.NextFrame();

  self.ScrollingCredits.NextFrame();
end;

function TFormAbout.LoadTune() : Boolean;
var AFlags : Integer;
begin
  result := False;

  if not FBassmod then
    Exit();

  AFlags := BASS_MUSIC_LOOP     or BASS_MUSIC_RAMPS or
            BASS_MUSIC_SURROUND or BASS_MUSIC_POSRESET;

  FMusicHandle := BASS_MusicLoad(
                                  True,
                                  FTune.Memory,
                                  0,
                                  FTune.Size,
                                  AFlags,
                                  1
  );

  result := FMusicHandle > 0;
end;

function TFormAbout.PlayTune() : Boolean;
begin
  result := False;
  ///

  if not Assigned(FTune) then
    Exit();
  ///

  if FPlaying then
    self.StopTune();

  if not LoadTune() then
    Exit();

  BASS_ChannelPlay(FMusicHandle, True);

  ///
  result := True;
end;

procedure TFormAbout.InitializeBassMod();
begin
  FBassmod := False;
  FPlaying := False;
  ///

  if not BASS_Init(-1, 44100, 0, self.Handle, nil) then
    BASS_Free();

  ///
  FBassmod := True;
end;

function TFormAbout.LoadTunes() : Boolean;
begin
  if Assigned(FTune) then
    result := True
  else begin
    result := False;
    try
      FTune := TResourceStream.Create(hInstance, 'mktn', 'sub7');

      FTune.Position := 0;

      result := True;
    except

    end;
  end;
end;

procedure TFormAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  self.StopTune();
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  FTune := nil;
  FFirstShow := True;
  FMusicHandle := 0;

  InitializeBassMod();

  // Patch credit text with dynamic content
  self.ScrollingCredits.Text.Text := StringReplace(
    self.ScrollingCredits.Text.Text,
    '%version%',
    Format('%s %s (REL %s)', [
      TVersion.GetFileVersion(GetModuleName(0)),
      APP_ENV_ReleaseName,
      APP_ENV_ReleaseDate
    ]),
    []
  );

  // Create the object list that will handle PNG Images
  FSceneFrames := TObjectList<TPngImage>.Create(True);
  FFrameIndex  := 0;
end;

procedure TFormAbout.FormDestroy(Sender: TObject);
begin
  if FBassmod then
    BASS_Free();

  if Assigned(FSceneFrames) then
    FreeAndNil(FSceneFrames);
end;

procedure TFormAbout.FormResize(Sender: TObject);
begin
  self.DoResize();
end;

procedure TFormAbout.FormShow(Sender: TObject);
begin
  if FFirstShow then begin
    self.LoadTunes();

    self.LoadSceneMap();

    FFirstShow := False;
  end else begin
    FFrameIndex        := 0;
    TimerScene.Enabled := True;
  end;

  FPlaying := PlayTune();

  self.ScrollingCredits.RefreshScene();

  self.DoResize();
end;

end.
