unit uFormWait;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TFormWait = class(TForm)
    ProgressBar: TProgressBar;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWait: TFormWait;

implementation

{$R *.dfm}

procedure TFormWait.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  self.ProgressBar.Style := pbstNormal;

  Application.MainForm.Enabled := True;
end;

procedure TFormWait.FormShow(Sender: TObject);
begin
  self.ProgressBar.Style := pbstMarquee;

  Application.MainForm.Enabled := False;
end;

end.
