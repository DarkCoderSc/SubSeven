program Sub7VideoGenerator;

uses
  Vcl.Forms,
  XSuperObject in '..\..\..\Shared\XSuperObject.pas',
  XSuperJson in '..\..\..\Shared\XSuperJson.pas',
  UntRC4 in '..\..\..\Shared\UntRC4.pas',
  UntCRC32 in '..\..\..\Shared\UntCRC32.pas',
  uFormMain in 'Units\uFormMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
