unit Demo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    Button2: TButton;
    Button3: TButton;
    ScrollBar1: TScrollBar;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private-Deklarationen }
    UserChange: Boolean;
    procedure Error(s: string);
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses BASSMOD;

procedure TForm1.Error(s: string);
var
  t: string;
begin
  t := s + #13#10 + '(error code: ' + IntToStr(BASSMOD_ErrorGetCode) + ')';
  MessageBox(handle, PChar(t), 'Error', MB_OK or MB_ICONERROR);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  (* Check that BASSMOD 2.0 was loaded *)
  if BASSMOD_GetVersion <> MAKELONG(2,0) then begin
    Error('BASSMOD version 2.0 was not loaded');
    self.Destroy;
    Halt;
  end;
  (* setup output - default device, 44100hz, stereo, 16 bits *)
  if  not BASSMOD_Init(-1,44100,0) then begin
    Error('Can''t initialize device');
    BASSMOD_Free;
    self.Destroy;
    Halt;
  end;
  UserChange := FALSE;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  BASSMOD_Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  i: Integer;
begin
  i := BASSMOD_MusicGetPosition;
  if i = -1 then i := 0;
  Panel1.Caption := IntToStr(LoWord(i)) + '.' + IntToStr(HiWord(i));
  Panel2.Caption := IntToStr(BASSMOD_GetCpu) + '%';
  UserChange := FALSE;
  ScrollBar1.Position := LoWord(i);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    BASSMOD_MusicFree;
    if BASSMOD_MusicLoad(FALSE, PChar(OpenDialog1.FileName), 0, 0, BASS_MUSIC_LOOP or BASS_MUSIC_RAMPS or BASS_MUSIC_SURROUND or BASS_MUSIC_POSRESET) <> 0 then begin
      Button1.Caption := OpenDialog1.FileName;
      ScrollBar1.Max := BASSMOD_MusicGetLength(FALSE);
      ScrollBar1.Enabled := TRUE;
      BASSMOD_MusicPlay;
    end
    else Error('Can''t play the file');
  end;
end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin
  if UserChange then BASSMOD_MusicSetPosition(ScrollBar1.Position);
  UserChange := TRUE;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  BASSMOD_MusicPlay;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  BASSMOD_MusicPause;
end;

end.
