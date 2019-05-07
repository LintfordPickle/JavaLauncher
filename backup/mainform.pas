unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  // System
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, ShellApi,

  // Luancher
  LauncherConfig, VersionForm;

type

  { TForm1 }

  TForm1 = class(TForm)
    InfoButton: TButton;
    WebButton: TButton;
    LaunchButton: TButton;
    SettingsButton: TButton;
    ExitButton: TButton;

    procedure Initialize(AConfiguration: TLauncherConfig);

    procedure ExitButtonClick(Sender: TObject);
    procedure InfoButtonClick(Sender: TObject);
    procedure LaunchButtonClick(Sender: TObject);
    procedure WebButtonClick(Sender: TObject);

  private
    FLauncherConfig: TLauncherConfig;

    FVersionWindow: TVersion;

  public
    property Configuration: TLauncherConfig  read FLauncherConfig;

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
// returns a human-readable string form ShellExec error codes.
function GetShellExecErrorString(const AnErrorCode: Integer): String;
begin
  case AnErrorCode of
    0: Result := 'Out of Memory (0)';
    2: Result := 'File not found (2)';
    3: Result := 'Path not found (3)';
    8: Result := 'Insufficient memory to start the application (8)';
    10: Result := 'Incorrect windows version (10)';
    11: Result := 'Executable file is invalid (11)';
    12: Result := 'Executable file is invalid (12)';
    13: Result := 'Executable file designed for MS-DOS 4.0 (13)';
    14: Result := 'Unknown eecutable file type (14)';
    15: Result := 'Incorrect version of windows (15)';
    16: Result := 'Cannot start multiple instances of this application (16)';
    19: Result := 'Cannot execute a compressed executable (19)';
    20: Result := 'A Dynamic-Link library file is invalid or corrupt (20)';
    21: Result := 'Application requires Microsoft Windows 32-bit extensions (21)';
  else Result := 'Unknown error (' + IntToStr(AnErrorCode) + ')';
  end;

end;

{ TForm1 }

// Launches the application
procedure TForm1.LaunchButtonClick(Sender: TObject);
var
  LPath:              String;

  LJRELocation:       String;
  LApplicationName:   String;
  LApplicationParams: String;

  ExecuteResult:      Integer;

begin
  LJRELocation        := Configuration.JRELocation;
  LApplicationName    := Configuration.ApplcationFileName;
  LApplicationParams  := Configuration.ApplicationParams;

  if LJRELocation = ''  then
  begin
    MessageDlg('An Error has Occurred!', 'You need to specify the JRE location in the ini file.', mtError, [mbOK], '');
    exit;
  end;

  if LApplicationName = ''  then
  begin
    MessageDlg('An Error has Occurred!', 'You need to specify the application name in the ini file.', mtError, [mbOK], '');
    exit;
  end;

  // Execute the application
  ExecuteResult       := ShellExecute(0, nil, PChar(LJRELocation), PChar('-jar ' + LApplicationName + '.jar ' + LApplicationParams), nil, 0);

  // If the application was launched successfully, then close the launcher
  // return codes below 32 indicate an error occured. Results >32 are the new window
  // instance handle
  if ExecuteResult > 32 then
    Application.Terminate
  else
  begin
    // Output a readable error message
    // ShowMessage(GetShellExecErrorString(ExecuteResult));
    MessageDlg('An Error has Occurred!', GetShellExecErrorString(ExecuteResult), mtError, [mbOK], '');
  end;

end;

// Opens the application webpage in the default browser, if applicable.
procedure TForm1.WebButtonClick(Sender: TObject);
begin
  if Configuration.WebPageURL <> '' then
    ShellExecute(0, nil, PChar(Configuration.WebPageURL), nil, nil, 0) ;

end;

procedure TForm1.ExitButtonClick(Sender: TObject);
begin
  // Terminte the application
  Application.Terminate;
end;

procedure TForm1.InfoButtonClick(Sender: TObject);
begin
  if not Assigned(FVersionWindow) then
    Application.CreateForm(TVersion, FVersionWindow);

  // If the form is not visible, then show it, else set it in focus
  if not FVersionWindow.IsVisible then
    FVersionWindow.Show
  else FVersionWindow.SetFocus;
end;

procedure TForm1.Initialize(AConfiguration: TLauncherConfig);
begin
  if AConfiguration = nil then
     AConfiguration := TLauncherConfig.Create;

  FLauncherConfig := AConfiguration;

  Caption := AConfiguration.ApplcationName;

end;

end.

