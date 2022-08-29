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

unit Sub7.Core.Windows.Process;

interface

uses System.Classes, Winapi.Windows, System.SysUtils;

type
  TS7SpawnProcessMode = (
    spmCreateCurrent,
    spmCreateInTermSession,
    spmCreateAs
  );

  TS7SpawnProcessHelper = class
  private
    {@Parameters}
    FCommand                  : String;
    FWorkingDirectory         : String;
    FUsername                 : String;
    FPassword                 : String;
    FSpawnMode                : TS7SpawnProcessMode;
    FTermSessionId            : Cardinal;
    FShowWindow               : Boolean;
    FAttachToCurrentProcess   : Boolean;
    FControlIO                : Boolean;

    {@Session}
    FProcessId                : Cardinal;
    FPipeOutWrite             : THandle;
    FPipeOutRead              : THandle;
    FPipeInWrite              : THandle;
    FPipeInRead               : THandle;
    FJob                      : THandle;
    FStartupInformation       : TStartupInfo;
    FProcessInformation       : TProcessInformation;

    {@M}
    function GetActive() : Boolean;
    function GetBytesInBuffer() : Int64;
    procedure CleanPipes();
  public
    {@C}
    constructor Create(const ACommand : String);
    destructor Destroy(); override;

    {@M}
    procedure Spawn();
    procedure CloseProcess();
    function SpawnCaptureOutput(const ATimeout : Cardinal = INFINITE) : String;

    function ReadStdout(var pBuffer : PVOID; var ABytesRead : Cardinal; AOemToChar : Boolean) : Boolean;
    procedure BreakConsole();
    procedure Write(pData : PVOID; const ADataSize : Cardinal);
    procedure WriteLn(const AStr : AnsiString = '');
    procedure CheckActive();

    {@G/S}
    property AttachToCurrentProcess : Boolean             read FAttachToCurrentProcess write FAttachToCurrentProcess;
    property SpawnMode              : TS7SpawnProcessMode read FSpawnMode              write FSpawnMode;
    property Command                : String              read FCommand                write FCommand;
    property WorkingDirectory       : String              read FWorkingDirectory       write FWorkingDirectory;
    property Username               : String              read FUsername               write FUsername;
    property Password               : String              read FPassword               write FPassword;
    property TermSessionId          : Cardinal            read FTermSessionId          write FTermSessionId;
    property ShowWindow             : Boolean             read FShowWindow             write FShowWindow;
    property ControlIO              : Boolean             read FControlIO              write FControlIO;

    {@G}
    property Active        : Boolean read GetActive;
    property BytesInBuffer : Int64   read GetBytesInBuffer;
  end;

implementation

uses Sub7.Core.Exceptions, Sub7.Core.Bundle, Sub7.Core.Windows.Sessions.Enum,
     Sub7.Core.Diagnostic, System.Diagnostics, Sub7.Core.Windows;

{-------------------------------------------------------------------------------
  ___constructor
-------------------------------------------------------------------------------}
constructor TS7SpawnProcessHelper.Create(const ACommand : String);
begin
  FCommand                := ACommand;
  FWorkingDirectory       := '';
  FUsername               := '';
  FPassword               := '';
  FSpawnMode              := spmCreateCurrent;
  FTermSessionId          := 0;
  FShowWindow             := True;
  FAttachToCurrentProcess := False;
  FControlIO              := True;

  FProcessId := 0;
  FJob       := 0;

  ZeroMemory(@FStartupInformation, SizeOf(TStartupInfo));
  FStartupInformation.cb := SizeOf(TStartupInfo);

  ZeroMemory(@FProcessInformation, SizeOf(TProcessInformation));
end;

{-------------------------------------------------------------------------------
  ___destructor
-------------------------------------------------------------------------------}
destructor TS7SpawnProcessHelper.Destroy();
begin
  self.CloseProcess();

  self.CleanPipes();

  ///
  inherited Destroy();
end;

{ TS7SpawnProcessHelper.CleanPipes

  Close pipes and set their value back to zero }
procedure TS7SpawnProcessHelper.CleanPipes();
begin
  // Close pipes
  CloseValidPositiveHandle(FPipeOutWrite);
  CloseValidPositiveHandle(FPipeOutRead);
  CloseValidPositiveHandle(FPipeInWrite);
  CloseValidPositiveHandle(FPipeInRead);

  // Reset pipes
  FPipeOutWrite := 0;
  FPipeOutRead  := 0;
  FPipeInWrite  := 0;
  FPipeInRead   := 0;
end;

{ TS7SpawnProcessHelper.CloseProcess

  Close attached process }

procedure TS7SpawnProcessHelper.CloseProcess();
var AExitCode : Cardinal;
begin
  // Terminate job objects
  if FJob > 0 then begin
    if FAttachToCurrentProcess then begin
      AExitCode := 0;

      TerminateJobObject(FJob, AExitCode);
    end;

    CloseValidPositiveHandle(FJob);
  end;
  FJob := 0;

  // Close process
  if FProcessInformation.hProcess > 0 then begin
    if FAttachToCurrentProcess then
      TerminateProcess(FProcessInformation.hProcess, 0);

    ///
    CloseHandle(FProcessInformation.hProcess);
    CloseHandle(FProcessInformation.hThread);
  end;
  ZeroMemory(@FProcessInformation, SizeOf(TProcessInformation));
end;

{-------------------------------------------------------------------------------
  ___spawn
-------------------------------------------------------------------------------}
procedure TS7SpawnProcessHelper.Spawn();
var AJobInfo            : TJobObjectExtendedLimitInformation;
    ASecurityAttributes : TSecurityAttributes;
    AFlags              : Cardinal;
    hContextToken       : THandle;
    hToken              : THandle;


const CREATE_BREAKAWAY_FROM_JOB = $01000000;
begin
  ZeroMemory(@AJobInfo, SizeOf(TJobObjectExtendedLimitInformation));
  ZeroMemory(@ASecurityAttributes, SizeOf(TSecurityAttributes));
  ///

  {
    @Job
  }
  if FAttachToCurrentProcess then begin
    FJob := CreateJobObjectW(nil, PWideChar(TGUID.NewGuid.ToString()));
    if (FJob = 0) then
      raise ES7WindowsException.Create('CreateJobObjectW');
    ///

    AJobInfo.BasicLimitInformation.LimitFlags := JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE
    ;
    if not SetInformationJobObject(
                                      FJob,
                                      JobObjectExtendedLimitInformation,
                                      @AJobInfo,
                                      SizeOf(TJobObjectExtendedLimitInformation)
    ) then
      raise ES7WindowsException.Create('SetInformationJobObject');
  end;

  {
    @Pipes
  }
  ASecurityAttributes.nLength              := SizeOf(TSecurityAttributes);
  ASecurityAttributes.lpSecurityDescriptor := nil;
  ASecurityAttributes.bInheritHandle       := True;

  if FControlIO then
    if not CreatePipe(FPipeOutRead, FPipeOutWrite, @ASecurityAttributes, 0) then
      raise ES7WindowsException.Create('(1)CreatePipe');

//  if FAttachToCurrentProcess then
//    if not SetHandleInformation(FPipeOutRead, HANDLE_FLAG_INHERIT, 0) then
//      raise ES7WindowsException.Create('(1)SetHandleInformation');

  if FControlIO then
    if not CreatePipe(FPipeInRead, FPipeInWrite, @ASecurityAttributes, 0) then
      raise ES7WindowsException.Create('(2)CreatePipe');

//  if FAttachToCurrentProcess then
//    if not SetHandleInformation(FPipeInWrite, HANDLE_FLAG_INHERIT, 0) then
//      raise ES7WindowsException.Create('(2)SetHandleInformation');

  {
    @StartInfo
  }
  if FShowWindow then
    FStartupInformation.wShowWindow := SW_SHOWNORMAL
  else
    FStartupInformation.wShowWindow := SW_HIDE;

  FStartupInformation.dwFlags := STARTF_USESTDHANDLES or
                                 STARTF_USESHOWWINDOW;

  if FControlIO then begin
    FStartupInformation.hStdOutput := FPipeInWrite;
    FStartupInformation.hStdError  := FPipeInWrite;
    FStartupInformation.hStdInput  := FPipeOutRead;
  end;

  {
    @CreateProcess
  }

  UniqueString(FCommand);

  AFlags := 0;

//  if self.FAttachToCurrentProcess then
//    AFlags := AFlags or CREATE_NEW_CONSOLE
//  else
//    AFlags := AFlags or DETACHED_PROCESS or
//                        CREATE_NEW_PROCESS_GROUP;


  AFlags := AFlags or CREATE_NEW_CONSOLE;

  if FAttachToCurrentProcess then
    AFlags := AFlags or CREATE_BREAKAWAY_FROM_JOB;

  if FSpawnMode = spmCreateCurrent then begin
    {
      Create process as current user
    }
    if not CreateProcessW(
                           nil,
                           PWideChar(FCommand),
                           nil,
                           nil,
                           FControlIO, // Inherit Handle if we want to control I/O
                           AFlags,
                           nil,
                           nil,
                           FStartupInformation,
                           FProcessInformation
    ) then
      raise ES7WindowsException.Create('CreateProcessW');

    odbs('new process created: ' + inttostr(FProcessInformation.dwProcessId));
  end else begin
    {
      Create process in a different user context
    }
    FStartupInformation.lpDesktop := nil;
    hContextToken := 0;
    try
      case FSpawnMode of
        spmCreateInTermSession : begin
          {
            Create process in specific terminal session
          }
          if not WTSQueryUserToken(FTermSessionId, hContextToken) then
            raise ES7WindowsException.Create('WTSQueryUserToken');
        end;

        spmCreateAs : begin
          {
            Create process as defined user
          }
          if not LogonUserW(
                           PWideChar(FUsername),
                           nil, // TODO
                           PWideChar(FPassword),
                           LOGON32_LOGON_INTERACTIVE,
                           LOGON32_PROVIDER_DEFAULT,
                           hToken
          ) then
            raise ES7WindowsException.Create('LogonUserW');
          try
            if not DuplicateTokenEx(
                                      hToken,
                                      MAXIMUM_ALLOWED,
                                      nil,
                                      SecurityIdentification,
                                      TokenPrimary,
                                      hContextToken
            ) then
              raise ES7WindowsException.Create('DuplicateTokenEx');
          finally
            CloseHandle(hToken);
          end;
        end;
      end;

      if not CreateProcessAsUserW(
                                    hContextToken,
                                    nil,
                                    PWideChar(FCommand),
                                    nil,
                                    nil,
                                    FControlIO, // Inherit Handle if we want to control I/O
                                    AFlags,
                                    nil,
                                    nil,
                                    FStartupInformation,
                                    FProcessInformation
      ) then
        raise ES7WindowsException.Create('CreateProcessAsUserW');
    finally
      if hContextToken <> 0 then
        CloseHandle(hContextToken);
    end;
  end;

  if FAttachToCurrentProcess then begin
    if not AssignProcessToJobObject(FJob, FProcessInformation.hProcess) then
      raise ES7WindowsException.Create('AssignProcessToJobObject');
  end;
end;

{-------------------------------------------------------------------------------
  Spawn process and immediately capture output. Support capture timeout
-------------------------------------------------------------------------------}
function TS7SpawnProcessHelper.SpawnCaptureOutput(const ATimeout : Cardinal = INFINITE) : String;
var AStopWatch    : TStopWatch;
    pBuffer       : Pointer;
    ABufferSize   : Cardinal;
    AChunk        : AnsiString;
    ABuilder      : TStringBuilder;
    AOldControlIO : Boolean;

    procedure ReadBuffer();
    begin
      while self.ReadStdout(pBuffer, ABufferSize, True) do begin
        try
          SetString(AChunk, PAnsiChar(pBuffer), ABufferSize);

          ABuilder.Append(AChunk);

          SetLength(AChunk, 0);
        finally
          FreeMem(pBuffer, ABufferSize);
        end;

        if AStopWatch.ElapsedMilliseconds >= ATimeout then
          break;
      end;
    end;

begin
  AOldControlIO := self.FControlIO;
  try
    self.FControlIO := True; // Force this option

    self.Spawn();
    ///

    result := '';

    ABuilder := TStringBuilder.Create();
    try
      AStopWatch := TStopWatch.StartNew;

      while True do begin
        ReadBuffer();

        if not GetActive() or (AStopWatch.ElapsedMilliseconds >= ATimeout) then
          break;
      end;

      ReadBuffer();

      ///
      result := ABuilder.ToString();
    finally
      if Assigned(ABuilder) then
        FreeAndNil(ABuilder);
    end;
  finally
    self.FControlIO := AOldControlIO;
  end;
end;

{-------------------------------------------------------------------------------
  Send CTRl+C / Break to Shell
-------------------------------------------------------------------------------}
procedure TS7SpawnProcessHelper.BreakConsole();
begin
  self.CheckActive();
  ///

  if not AttachConsole(FProcessInformation.dwProcessId) then
    Exit();
  try
    if not SetConsoleCtrlHandler(nil, True) then
      Exit();
    try
      GenerateConsoleCtrlEvent(CTRL_C_EVENT, FProcessInformation.dwProcessId);
    finally
      SetConsoleCtrlHandler(nil, False);
    end;
  finally
    FreeConsole();
  end;
end;

{-------------------------------------------------------------------------------
  Read available bytes using pipes
-------------------------------------------------------------------------------}
function TS7SpawnProcessHelper.ReadStdout(var pBuffer : PVOID; var ABytesRead : Cardinal; AOemToChar : Boolean) : Boolean;
var ABytesAvailable : Int64;
begin
  ABytesRead := 0;
  pBuffer    := nil;
  result     := False;
  ///

  if not FControlIO then
    raise ES7IOException.Create(ERR_READ_STDOUT_DISABLED);

  ABytesAvailable := GetBytesInBuffer();
  if ABytesAvailable = 0 then
    Exit();
  ///

  GetMem(pBuffer, ABytesAvailable);
  try
    if not ReadFile(FPipeInRead, PByte(pBuffer)^, ABytesAvailable, ABytesRead, nil) then
      raise ES7WindowsException.Create('ReadFile');
    ///

    if AOemToChar then
      OemToCharBuffA(PAnsiChar(pBuffer), PAnsiChar(pBuffer), ABytesRead);

    ///
    result := (ABytesRead > 0);
  except
    on E : Exception do begin
      FreeMem(pBuffer, ABytesAvailable);

      raise;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Write Data to Current Terminal Session (stdin)
-------------------------------------------------------------------------------}

procedure TS7SpawnProcessHelper.Write(pData : PVOID; const ADataSize : Cardinal);
var ABytesWritten : DWORD;
begin
  if not FControlIO then
    raise ES7IOException.Create('Control I/O is disabled. Can''t write to process stdin.');

  self.CheckActive();
  ///

  if (ADataSize = 0) or (not Assigned(pData)) then
    raise ES7IOException.Create('Can''t write empty data in stdin.');

  if (NOT WriteFile(FPipeOutWrite, PByte(pData)^, ADataSize, ABytesWritten, nil)) then
    raise ES7WindowsException.Create('WriteFile');
end;

procedure TS7SpawnProcessHelper.WriteLn(const AStr : AnsiString = '');
var AValue : AnsiString;
begin
  AValue := AStr + #13#10;

  self.Write(@AStr[1], Length(AStr));
end;

{-------------------------------------------------------------------------------
  Check if process is active, if not it will raise an exception
-------------------------------------------------------------------------------}

procedure TS7SpawnProcessHelper.CheckActive();
begin
  if not self.Active then
    raise Exception.Create('No running process attached to current object.');
end;

{-------------------------------------------------------------------------------
  Getters / Setters
-------------------------------------------------------------------------------}

function TS7SpawnProcessHelper.GetActive() : Boolean;
begin
  result := False;
  ///

  if FProcessInformation.hProcess = 0 then
    Exit();
  ///

  case WaitForSingleObject(FProcessInformation.hProcess, 10) of
    WAIT_OBJECT_0 : ;

    else
      result := True;
  end;
end;

function TS7SpawnProcessHelper.GetBytesInBuffer() : Int64;
begin
  result := 0;
  ///

  if NOT PeekNamedPipe(FPipeInRead, nil, 0, nil, @result, nil) then
    raise ES7WindowsException.Create('PeekNamedPipe');
end;

end.
