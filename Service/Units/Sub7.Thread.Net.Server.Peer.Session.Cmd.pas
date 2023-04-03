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

unit Sub7.Thread.Net.Server.Peer.Session.Cmd;

interface

uses System.Classes, Sub7.Core.Protocol, Sub7.Thread.Net.Server.Peer.Session, XSuperObject;

type
  TSub7ServerPeerSessionCmd = class(TSub7ServerPeerSession)
  private
    {@M}
    procedure SendCommand(const ACommand : TS7Command; const AData : ISuperObject = nil); overload;
    procedure SendCommand(const ACommand : TS7Command; const AGUID : String; const AData : ISuperObject); overload;
    procedure SendCommand(const ACommand : TS7Command; const AGUID : String); overload;
    procedure SendSuccessMessage(const AMessage : String);

    {--> MAIN}
    procedure RefreshProcess();
    procedure TerminateProcess(const AData : ISuperObject);
    procedure ExitWindows(const AFlag : Integer);
    procedure PowerSleep(const AHibernate : Boolean);
    procedure LockSession(const AData : ISuperObject);
    procedure LogoffSession(const AData : ISuperObject);
    procedure Open(const AData : ISuperObject);
    procedure Run(const AData : ISuperObject);

    {--> FILE MANAGER}
    procedure RefreshDrives(const AGUID : String);
    procedure BrowseFolder(const AGUID : String; AData : ISuperObject);
    procedure CreateFolder(AData : ISuperObject);
    procedure RenameFile(AData : ISuperObject);
    procedure DeleteFile(AData : ISuperObject);
  protected
    {@M}
    procedure OnCommand(const ACommand : TS7Command; const AData : ISuperObject = nil); override;
  end;

implementation

uses WinAPI.Windows, System.SysUtils, Sub7.Core.Bundle, Sub7.Core.Windows.Process.Enum, Sub7.Core.Exceptions,
     Sub7.Core.Windows.Sessions.Enum, Sub7.Core.Diagnostic, Sub7.Service.External.Helper, Sub7.Core.Utils,
     Sub7.Core.FileSystem.Drives.Enum, Sub7.Core.FileSystem.Utils, IOUtils, Sub7.Core.Windows.Process,
     Sub7.Core.FileSystem.Enum;

{-------------------------------------------------------------------------------
  Send command to main handler
-------------------------------------------------------------------------------}

procedure TSub7ServerPeerSessionCmd.SendCommand(const ACommand : TS7Command; const AData : ISuperObject = nil);
begin
  FIOHandler.SendCommand(ACommand, AData);
end;

procedure TSub7ServerPeerSessionCmd.SendCommand(const ACommand : TS7Command; const AGUID : String; const AData : ISuperObject);
begin
  if not Assigned(AData) then
    self.SendCommand(ACommand, AGUID)
  else begin
    AData.S['form_guid'] := AGUID;

    self.SendCommand(ACommand, AData);
  end;
end;

procedure TSub7ServerPeerSessionCmd.SendCommand(const ACommand : TS7Command; const AGUID : String);
var AData : ISuperObject;
begin
  AData := TSuperObject.Create();

  self.SendCommand(ACommand, AGUID, AData);
end;

procedure TSub7ServerPeerSessionCmd.SendSuccessMessage(const AMessage : String);
var AData : ISuperObject;
begin
  AData := TSuperObject.Create();

  AData.S['message'] := AMessage;

  FIOHandler.SendCommand(mhcSuccess, AData);
end;

{-------------------------------------------------------------------------------
  Refresh Hard Drives Information
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.RefreshDrives(const AGUID : String);
var ADrives : TS7EnumHardDrives;
begin
  ADrives := TS7EnumHardDrives.Create(True);
  try
    self.SendCommand(fmcRefreshDrives, AGUID, ADrives.Serialize());
  finally
    if Assigned(ADrives) then
      FreeAndNil(ADrives);
  end;
end;

{-------------------------------------------------------------------------------
  Browse Folder Path
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.BrowseFolder(const AGUID : String; AData : ISuperObject);
var AFolder : TS7EnumFolder;
    APath   : String;
begin
  AFolder := TS7EnumFolder.Create(True);
  try
    if AData.Contains('path') then
      APath := AData.V['path'];

    AFolder.Browse(APath);

    {
      Success
    }
    self.SendCommand(fmcBrowseFolder, AGUID, AFolder.Serialize());
  finally
    if Assigned(AFolder) then
      FreeAndNil(AFolder);
  end;
end;

{-------------------------------------------------------------------------------
  Create Folder
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.CreateFolder(AData : ISuperObject);
var AFolderName : String;
    APath       : String;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('name') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['name']));

  if not AData.Contains('path') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['path']));
  ///

  AFolderName := AData.V['name'];
  APath       := AData.V['path'];

  APath := IncludeTrailingPathDelimiter(APath) + AFolderName;

  if not CreateDir(APath) then
    raise ES7Exception.Create(Format('Could not create "%s "folder, %s', [AFolderName, SysErrorMessage(GetLastError)]));

  ///
  FIOHandler.SendCommand(fmcCreateFolder, AData);
end;

{-------------------------------------------------------------------------------
  Rename File / Folder
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.RenameFile(AData : ISuperObject);
var APath : String;
    ANew  : String;
    AOld  : String;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('path') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['path']));

  if not AData.Contains('new_name') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['new_name']));

  if not AData.Contains('old_name') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['old_name']));
  ///

  APath := IncludeTrailingPathDelimiter(AData.S['path']);

  ANew  := AData.S['new_name'];
  AOld  := AData.S['old_name'];

  if String.Compare(ANew, AOld, True) = 0 then
    raise ES7Exception.Create(ERR_NOCHANGES);
  ///

  if WinAPI.Windows.MoveFile(PWideChar(APath + AOld), PWideChar(APath + ANew)) then
    FIOHandler.SendCommand(fmcRenameFile, AData)
  else
    raise ES7Exception.Create(Format('Could not rename "%s" to "%s", %s', [AOld, ANew, SysErrorMessage(GetLastError)]));
end;

{-------------------------------------------------------------------------------
  Delete File(s) / Folder(s)
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.DeleteFile(AData : ISuperObject);
var APath         : String;
    AFiles        : ISuperArray;
    AFile         : String;
    ASuccessFiles : ISuperArray;
    AFailedFiles  : ISuperArray;
    AFileName     : String;

    I             : Integer;

    procedure RaiseFailedFile(AReason : String);
    var AFailedFile : ISuperObject;
    begin
      AFailedFile := TSuperObject.Create();
      ///

      AFailedFile.V['name'] := AFile;
      AFailedFile.V['reason'] := AReason;

      ///
      AFailedFiles.Add(AFailedFile);
    end;

begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('path') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['path']));

  APath := AData.V['path'];

  if not AData.Contains('files') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['files']));
  ///

  AFiles := AData.A['files'];

  ASuccessFiles := TSuperArray.Create();
  AFailedFiles  := TSuperArray.Create();
  ///

  for I := 0 to AFiles.Length -1 do begin
    AFileName := AFiles.S[I];

    AFile := IncludeTrailingPathDelimiter(APath) + AFileName;
    ///

    if IsDir(AFile) then begin
      {
        Delete Folder (Recursively)
      }
      try
        IOUtils.TDirectory.Delete(AFile, True);

        ///
        ASuccessFiles.Add(AFileName);
      except
        on E : Exception do
          RaiseFailedFile(Format('Could not delete folder "%s", %s', [AFileName, E.Message]));
      end;
    end else if FileExists(AFile) then begin
      {
        Delete Regular File
      }
      if WinAPI.Windows.DeleteFile(PWideChar(AFile)) then
        ASuccessFiles.Add(AFileName)
      else
        RaiseFailedFile(Format('Could not delete file "%s", %s', [AFileName, SysErrorMessage(GetLastError)]));
    end else begin
      RaiseFailedFile('File does not exists or is invalid.');
    end;
  end;

  if (AFailedFiles.Length > 0) or (ASuccessFiles.Length > 0) then begin
    AData := TSuperObject.Create();

    AData.V['path'] := APath;

    if ASuccessFiles.Length > 0 then
      AData.A['success'] := ASuccessFiles;

    if AFailedFiles.Length > 0 then
      AData.A['failed'] := AFailedFiles;

    FIOHandler.SendCommand(fmcDeleteFile, AData);
  end else
    raise ES7Exception.Create(ERR_UNEXPECTED);
end;

{-------------------------------------------------------------------------------
  Lock session, if no session defined, then current active session is logged out
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.LockSession(const AData : ISuperObject);
var ASessionId : Integer;
begin
  if Assigned(AData) then
    if AData.Contains('session_id') then
      ASessionId := AData.I['session_id']
  else
    ASessionId := GetActiveTerminalSessionId();
  ///

  RunHelperCommand(hcLockComputer, nil, ASessionId);
end;

{-------------------------------------------------------------------------------
  Logoff target session, if no session defined, then current active session is
  logged out
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.LogoffSession(const AData : ISuperObject);
var ASessionId : Integer;
begin
  if Assigned(AData) then
    if AData.Contains('session_id') then
      ASessionId := AData.I['session_id']
  else
    ASessionId := GetActiveTerminalSessionId();
  ///

  RunHelperCommand(hcLogoff, nil, ASessionId);
end;

{-------------------------------------------------------------------------------
  ShellExecute(open)
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.Open(const AData : ISuperObject);
var ASessionId : Integer;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('session_id') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['session_id']));

  ASessionId := AData.I['session_id'];

  if not AData.Contains('command') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['command']));
  ///

  RunHelperCommand(hcOpen, AData, ASessionId);
end;


{-------------------------------------------------------------------------------
  Refresh Process List
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.RefreshProcess();
var AProcessList : TS7EnumProcess;
begin
  AProcessList := TS7EnumProcess.Create(True);
  try
    AProcessList.Refresh();
    ///

    FIOHandler.SendCommand(mhcProcessList, AProcessList.Serialize);
  finally
    if Assigned(AProcessList) then
      FreeAndNil(AProcessList);
  end;
end;

{-------------------------------------------------------------------------------
  Terminate Process by Id
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.TerminateProcess(const AData : ISuperObject);
var AProcessList : ISuperArray;
    I            : Integer;
    ASuccessList : ISuperArray;
    AFailedList  : ISuperArray;
    AFailedNode  : ISuperObject;
    AProcessId   : Cardinal;
    ASummaryData : ISuperObject;
    AProcessName : String;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('process') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['process']));

  AProcessList := AData.A['process'];

  ASuccessList := nil;
  AFailedList  := nil;

  for I := 0 to AProcessList.Length -1 do begin
    try
      AProcessId := AProcessList.I[I];

      try
        Sub7.Core.Windows.Process.Enum.TerminateProcess(AProcessId, AProcessName);
      except
        on E : ES7MissingProcess do begin
          // Ignore missing PID
        end;
      end;

      if not Assigned(ASuccessList) then
        ASuccessList := TSuperArray.Create();

      ASuccessList.Add(AProcessId);
    except
      on E : Exception do begin
        if not Assigned(AFailedList) then
          AFailedList := TSuperArray.Create();

        AFailedNode := TSuperObject.Create();

        AFailedNode.I['pid']    := AProcessId;
        AFailedNode.S['reason'] := E.Message;

        if AProcessName <> '' then
          AFailedNode.S['name'] := AProcessName;

        ///
        AFailedList.Add(AFailedNode);
      end;
    end;
  end;

  ASummaryData := TSuperObject.Create();

  if Assigned(ASuccessList) then
    ASummaryData.A['success'] := ASuccessList;

  if Assigned(AFailedList) then
    ASummaryData.A['failed'] := AFailedList;

  ///
  FIOHandler.SendCommand(mhcTerminateProcess, ASummaryData);
end;

{-------------------------------------------------------------------------------
  Exit Windows
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.ExitWindows(const AFlag : Integer);
var AFlags : Integer;
begin
  AFlags := AFlag or EWX_FORCE or EWX_FORCEIFHUNG;
  ///

  case AFlag of
    EWX_REBOOT, EWX_SHUTDOWN, EWX_POWEROFF : begin
      NTSetPrivilege('SeShutdownPrivilege', True);
    end;
  end;

  if not Winapi.Windows.ExitWindowsEx(AFlag, 0) then
    raise ES7WindowsException.Create('ExitWindowsEx');
end;

{-------------------------------------------------------------------------------
  Put machine on sleep state
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.PowerSleep(const AHibernate : Boolean);
var SetSuspendState : function(Hibernate, ForceCritical, DisableWakeEvent: BOOL) : BOOL; stdcall;

    hPowrProf : THandle;
begin
  hPowrProf := LoadLibraryW('PowrProf.dll');
  if hPowrProf = 0 then
    raise ES7WindowsException.Create('LoadLibraryW');

  @SetSuspendState := GetProcAddress(hPowrProf, 'SetSuspendState');
  if not Assigned(SetSuspendState) then
    raise ES7WindowsException.Create('GetProcAddress');
  try
    NTSetPrivilege('SeShutdownPrivilege', True);

    SetSuspendState(AHibernate, True, False);
  finally
    FreeLibrary(hPowrProf);
  end;
end;

{-------------------------------------------------------------------------------
  Run Program
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.Run(const AData : ISuperObject);
var AMode      : TS7RunAsMode;
    AUser      : String;
    APassword  : String;
    AProgram   : String;
    AArgv      : String;
    ARunAs     : ISuperObject;
    AProcess   : TS7SpawnProcessHelper;
    ACommand   : String;
begin
  if not Assigned(AData) then
    Exit();
  ///

  if not AData.Contains('runas') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['runas']));

  ARunAs := AData.O['runas'];

  if not ARunAs.Contains('mode') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['mode']));

  if not AData.Contains('program') then
    raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['program']));

  AProgram := AData.S['program'];

  IsValidPEFile(AProgram);

  if AData.Contains('argv') then
    AArgv := AData.S['argv'];

  AMode := TS7RunAsMode(ARunAs.I['mode']);

  if AArgv <> '' then
    ACommand := Format('%s "%s"', [AProgram, AArgv])
  else
    ACommand := AProgram;

  AProcess := TS7SpawnProcessHelper.Create(ACommand);
  try
    case AMode of
      ramNT : AProcess.SpawnMode := spmCreateCurrent;

      ramSession : begin
        if not ARunAs.Contains('session_id') then
          raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['session_id']));

        AProcess.TermSessionId := ARunAs.I['session_id'];
        AProcess.SpawnMode     := spmCreateInTermSession;
      end;

      ramUser : begin
        if not ARunAs.Contains('user') then
          raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['user']));

        if not ARunAs.Contains('password') then
          raise ES7ProtocolException.Create(Format(ERR_MISSING_PARAMETER, ['password']));

        AProcess.Username  := ARunAs.S['user'];
        AProcess.Password  := ARunAs.S['password'];
        AProcess.SpawnMode := spmCreateAs;
      end;
    end;

    {
      Run
    }
    AProcess.AttachToCurrentProcess := False;
    AProcess.ControlIO              := False;

    ///
    AProcess.Spawn();
  finally
    if Assigned(AProcess) then
      FreeAndNil(AProcess);
  end;

  ///
  self.SendSuccessMessage(Format('Program "%s" was successfully executed!', [ExtractFileName(AProgram)]));
end;

{-------------------------------------------------------------------------------
  On Command Received
-------------------------------------------------------------------------------}
procedure TSub7ServerPeerSessionCmd.OnCommand(const ACommand : TS7Command; const AData : ISuperObject = nil);
var AGUID : String;
begin
  {
    Unwrap GUID Window
  }
  if Assigned(AData) then begin
    if AData.Contains('form_guid') then begin
      AGUID := AData.S['form_guid'];

      AData.Remove('form_guid');
    end;
  end;

  {
    Dispatch Command
  }
  case ACommand of
    {***************************************************************************
      Main Commands
    ***************************************************************************}

    {
      Client request Ping
    }
    mhcPing : begin
      FIOHandler.SendCommand(mhcPong, AData);
    end;

    {
      Process List
    }
    mhcProcessList : begin
      self.RefreshProcess();
    end;

    {
      Kill / Terminate Process
    }
    mhcTerminateProcess : begin
      self.TerminateProcess(AData);
    end;

    {
      Machine Actions
    }
    mhcShutdown : begin
      self.ExitWindows(EWX_SHUTDOWN);
    end;

    mhcPoweroff : begin
      self.ExitWindows(EWX_POWEROFF);
    end;

    mhcReboot : begin
      self.ExitWindows(EWX_REBOOT);
    end;

    mhcSleep : begin
      self.PowerSleep(False);
    end;

    mhcHibernate : begin
      self.PowerSleep(True);
    end;

    mhcLock : begin
      self.LockSession(AData);
    end;

    mhcLogoff : begin
      self.LogoffSession(AData);
    end;

    {
      Run Program
    }
    mhcRun : begin
      self.Run(AData);
    end;

    {
      Open Program
    }
    mhcOpen : begin
      self.Open(AData);
    end;

    {***************************************************************************
      File Manager
    ***************************************************************************}

    {
      Refresh Hard Drives
    }
    fmcRefreshDrives : begin
      self.RefreshDrives(AGUID);
    end;

    {
      Browse for Files
    }
    fmcBrowseFolder : begin
      self.BrowseFolder(AGUID, AData);
    end;

    {
      Create Folder
    }
    fmcCreateFolder : begin
      self.CreateFolder(AData);
    end;

    {
      Rename Folder / File
    }
    fmcRenameFile : begin
      self.RenameFile(AData);
    end;

    {
      Delete Folder(s) / File(s)
    }
    fmcDeleteFile : begin
      self.DeleteFile(AData);
    end;
  end;
end;

end.
