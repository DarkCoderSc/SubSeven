;  ______     __  __     ______     ______     ______     __   __   ______     __   __    
; /\  ___\   /\ \/\ \   /\  == \   /\  ___\   /\  ___\   /\ \ / /  /\  ___\   /\ "-.\ \   
; \ \___  \  \ \ \_\ \  \ \  __<   \ \___  \  \ \  __\   \ \ \'/   \ \  __\   \ \ \-.  \  
;  \/\_____\  \ \_____\  \ \_____\  \/\_____\  \ \_____\  \ \__|    \ \_____\  \ \_\\"\_\ 
;   \/_____/   \/_____/   \/_____/   \/_____/   \/_____/   \/_/      \/_____/   \/_/ \/_/ 

; Jean-Pierre LESUEUR
; @DarkCoderSc

#define MyAppName "SubSeven Legacy Server"
#define MyAppVersion "0.1.0 Alpha 1"
#define MyAppPublisher "Sub7Crew"
#define MyAppURL "https://www.sub7crew.org/"
#define MyAppExeName "Sub7ServerTray.exe"
#define ServiceName "Sub7Legacy"
#define CertName "sub7server.pem"

[Setup]
AppId={{1400794A-7B7A-4627-A8D8-DE3D85E2B2F5}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
WizardSmallImageFile=icon.bmp
LicenseFile=license.txt
UninstallDisplayIcon={app}\{#MyAppExeName}
DefaultDirName={code:GetProgramFiles}\{#MyAppName}
DisableDirPage=yes
DisableProgramGroupPage=yes
OutputBaseFilename=Sub7LegacyServerSetup
Compression=lzma
SolidCompression=yes
WizardStyle=modern
PrivilegesRequired=admin 

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}";

[Files]
; x86-32 Files
Source: "..\common-bin\x86-32\bass.dll";                    DestDir: "{app}"; Flags: ignoreversion; Check: not IsWin64
Source: "..\common-bin\x86-32\libcrypto-1_1.dll";           DestDir: "{app}"; Flags: ignoreversion; Check: not IsWin64
Source: "..\common-bin\x86-32\libssl-1_1.dll";              DestDir: "{app}"; Flags: ignoreversion; Check: not IsWin64

Source: "..\Service\Win32\Release\Sub7Service.exe";         DestDir: "{app}"; Flags: ignoreversion; Check: not IsWin64
Source: "..\Helper\Win32\Release\Sub7Helper.exe";           DestDir: "{app}"; Flags: ignoreversion; Check: not IsWin64
Source: "..\Tray\Win32\Release\Sub7ServerTray.exe";         DestDir: "{app}"; Flags: ignoreversion; Check: not IsWin64
Source: "..\CertGen\Win32\Release\CertGenerator.exe";       DestDir: "{app}"; Flags: ignoreversion; Check: not IsWin64
Source: "..\SecureDesktop\Win32\Release\SecureDesktop.dll"; DestDir: "{app}"; Flags: ignoreversion; Check: not IsWin64

; x86-64 Files
Source: "..\common-bin\x86-64\bass.dll";                    DestDir: "{app}"; Flags: ignoreversion; Check: IsWin64
Source: "..\common-bin\x86-64\libcrypto-1_1-x64.dll";       DestDir: "{app}"; Flags: ignoreversion; Check: IsWin64
Source: "..\common-bin\x86-64\libssl-1_1-x64.dll";          DestDir: "{app}"; Flags: ignoreversion; Check: IsWin64

Source: "..\Service\Win64\Release\Sub7Service.exe";         DestDir: "{app}"; Flags: ignoreversion; Check: IsWin64
Source: "..\Helper\Win64\Release\Sub7Helper.exe";           DestDir: "{app}"; Flags: ignoreversion; Check: IsWin64
Source: "..\Tray\Win64\Release\Sub7ServerTray.exe";         DestDir: "{app}"; Flags: ignoreversion; Check: IsWin64
Source: "..\CertGen\Win64\Release\CertGenerator.exe";       DestDir: "{app}"; Flags: ignoreversion; Check: IsWin64
Source: "..\SecureDesktop\Win64\Release\SecureDesktop.dll"; DestDir: "{app}"; Flags: ignoreversion; Check: IsWin64

[Icons]
Name: "{autoprograms}\SubSeven Service Controller"; Filename: "{app}\{#MyAppExeName}";
Name: "{autodesktop}\SubSeven Service Controller";  Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon;

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram, 'SubSeven Service Controller'}"; Flags: nowait postinstall skipifsilent shellexec

[UninstallRun]
Filename: {sys}\sc.exe; Parameters: "stop {#ServiceName}";   Flags: runhidden
Filename: {sys}\sc.exe; Parameters: "delete {#ServiceName}"; Flags: runhidden

[code]

const
  WAIT_TIMEOUT            = $00000102;
  SEE_MASK_NOCLOSEPROCESS = $00000040;

type
  TShellExecuteInfo = record
    cbSize       : DWORD;
    fMask        : Cardinal;
    Wnd          : HWND;
    lpVerb       : String;
    lpFile       : String;
    lpParameters : String;
    lpDirectory  : String;
    nShow        : Integer;
    hInstApp     : THandle;    
    lpIDList     : DWORD;
    lpClass      : String;
    hkeyClass    : THandle;
    dwHotKey     : DWORD;
    hMonitor     : THandle;
    hProcess     : THandle;
  end;

function ShellExecuteEx(var lpExecInfo: TShellExecuteInfo): BOOL; external 'ShellExecuteExW@shell32.dll stdcall';
function WaitForSingleObject(hHandle: THandle; dwMilliseconds: DWORD): DWORD; external 'WaitForSingleObject@kernel32.dll stdcall';
function TerminateProcess(hProcess: THandle; uExitCode: UINT): BOOL; external 'TerminateProcess@kernel32.dll stdcall';
function GetExitCodeProcess(hProcess: THandle; var lpExitCode : DWORD) : BOOL; external 'GetExitCodeProcess@kernel32.dll stdcall';

var
  GenerateNewCertificate : TNewCheckBox;
  ProgressPage           : TOutputProgressWizardPage;
  ProgressPageShown      : Boolean;

procedure InitializeWizard();
var AParent : TWinControl;
begin
  // Create additional tasks
  AParent := WizardForm.SelectTasksPage;

  GenerateNewCertificate         := TNewCheckBox.Create(WizardForm);
  GenerateNewCertificate.Parent  := AParent;
  GenerateNewCertificate.Caption := 'Generate new SSL/TLS Certificate';
  GenerateNewCertificate.Left    := ScaleX(4);
  GenerateNewCertificate.Top     := ScaleY(80);
  GenerateNewCertificate.Width   := AParent.ClientWidth - GenerateNewCertificate.Left - ScaleX(8);
  GenerateNewCertificate.Checked := True;

  // Create progress page for generating certificate
  ProgressPage      := CreateOutputProgressPage('Generating SSL/TLS Certificate, please wait...','');
  ProgressPageShown := False;
end;

procedure CurPageChanged(CurPageID: Integer);
var I         : Integer;
    AExecInfo : TShellExecuteInfo;
    AExitCode : DWORD;
begin  
  if (CurPageID = wpFinished) and (not ProgressPageShown) and (GenerateNewCertificate.Checked) then begin
    ProgressPageShown := True;

    ProgressPage.SetText('Generating SSL/TLS Certificate, please wait...', '');
    ProgressPage.ProgressBar.Style   := npbstMarquee;
    ProgressPage.ProgressBar.Visible := True;
   
    ProgressPage.Show();
    try
      AExecInfo.cbSize       := SizeOf(AExecInfo);
      AExecInfo.fMask        := SEE_MASK_NOCLOSEPROCESS; // To receive hProcess
      AExecInfo.Wnd          := 0;
      AExecInfo.lpFile       := ExpandConstant('{app}') + '\CertGenerator.exe';
      AExecInfo.lpParameters := '"' + ExpandConstant('{app}') + '\{#CertName}' + '"';
      AExecInfo.nShow        := SW_HIDE;
      ///

      if not ShellExecuteEx(AExecInfo) then
        MsgBox('Could execute SSL/TLS Certificate Generator.', mbError, MB_OK);

      while True do begin
        if WaitForSingleObject(AExecInfo.hProcess, 10) <> WAIT_TIMEOUT then
          break;  

        ProgressPage.ProgressBar.Refresh();
      end;
    
      GetExitCodeProcess(AExecInfo.hProcess, AExitCode);

      if AExitCode <> 0 then
        MsgBox(Format('Could not generate SSL/TLS Certificate with exit code=[%d].', [AExitCode]), mbError, MB_OK);
    finally
      ProgressPage.Hide;
      ProgressPage.Free();
    end;
  end;
end;

function GetProgramFiles(AParam: String): String;
begin
  if IsWin64 then 
    Result := ExpandConstant('{pf64}')
  else
    Result := ExpandConstant('{pf32}')
end;

function PrepareToInstall(var NeedsRestart: Boolean): String;
var AServiceName : String;
    AResult      : Integer;
begin
  result := '';
 
  AServiceName := ExpandConstant('{#ServiceName}');

  Exec('sc.exe', Format('stop %s', [AServiceName]), '', SW_HIDE, ewWaitUntilTerminated, AResult); 
end;
