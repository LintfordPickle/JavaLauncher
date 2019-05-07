unit LauncherConfig;

{$mode objfpc}{$H+}

interface

uses
  // System
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles, ShellApi;

const
  cDefaultAppName = 'JavaLauncher';
  cDefaultJRELocation = 'JRE';
  cDefaultFileName = 'core';

type

  { TLauncherConfig }

  TLauncherConfig = class(TForm)
     protected
       procedure ReadConfig(AINIPathname : String);

     private
       FJRELocation:         String;
       FApplicationName:     String;
       FApplicationFilename: String;
       FApplicationParams:   String;

       FJREAvailable:        Boolean;
       // Controls whether the launch GUI should be displayed, or the
       // underlying jar simply run (i.e. start by exe)
       FStrightLaunch:       Boolean;
	   
       FWebpageURL:          String;

     public
       constructor Create; overload;

       // creates a configuration file from command line options
       procedure CreateConfigFile;
       procedure LoadConfigFile;
       procedure PerformStartupChecks;

       property IsJREAvailable:       Boolean read FJREAvailable;
       property IsStraightLunch:      Boolean read FStrightLaunch;

       property JRELocation:          String read FJRELocation;
       property ApplcationName:       String read FApplicationName;
       property ApplcationFileName:   String read FApplicationFilename;
       property ApplicationParams:    String read FApplicationParams;
       property WebPageURL:           String read FWebpageURL;

  end;

implementation

{ TLauncherConfig }

constructor TLauncherConfig.Create;
begin
  inherited;

  FJREAvailable := False;

end;

procedure TLauncherConfig.LoadConfigFile;
var
  LINIPathname: String;

begin
  LINIPathname := ParamStr(0);
  LINIPathname := ChangeFileExt(LINIPathname, '.ini');

  ReadConfig(LINIPathname);

end;

procedure TLauncherConfig.CreateConfigFile;
var
  LIniFile:            TIniFile;
  LConfigPath:         String;
  LTempInt:            Integer;

  LJRELocation:        String;
  LApplicationName:    String;
  LApplicationJARName: String;
  LApplicationParams:  String;
  LWebAddress:         String;
  LDirectLaunch:       String;

begin
  // Set the name of the configuration file to that of the application (with extension .ini)
  LConfigPath := ParamStr(0);
  LConfigPath := ChangeFileExt(LConfigPath, '.ini');
  LIniFile := TIniFile.Create(LConfigPath);

  // Figure out what was handed to us on the command line
  LJRELocation := Application.GetOptionValue('jre');
  LApplicationName := Application.GetOptionValue('n','name');
  LApplicationJARName := Application.GetOptionValue('j', 'jar');
  LApplicationParams := Application.GetOptionValue('p', 'params');
  LWebAddress := Application.GetOptionValue('w', 'web');

  LTempInt :=  StrToIntDef(Application.GetOptionValue('d', 'directlaunch'), 0);
  if (LTempInt < 0) or (LTempInt > 1) then
    LTempInt := 0;

  try
    LIniFile.WriteString('Settings', 'JREPath',    LJRELocation);
    LIniFile.WriteString('Settings', 'AppName',    LApplicationName);
    LIniFile.WriteString('Settings', 'AppFile',    LApplicationJARName);
    LIniFile.WriteString('Settings', 'Params',     LApplicationParams);
    LIniFile.WriteString('Settings', 'Web',        LWebAddress);

    LIniFile.WriteString('Settings', 'DirectLaunch',  IntToStr(LTempInt));

    LIniFile.UpdateFile;

  finally
    LIniFile.free;
  end;

end;

procedure TLauncherConfig.ReadConfig(AINIPathname : String);
var
  LINIFile: TIniFile;

begin
  LINIFile := nil;

  // First check to see if a configuration file with the same name as the application exists,
  // if not, then check for a configuration file called 'configuration.ini'
  if not FileExists(AINIPathname) then
  begin
    AINIPathname := ExtractFilePath(AINIPathname) + 'configuration.ini';
    if not FileExists(AINIPathname) then
    begin
      MessageDlg('An Error has Occurred!', 'Cound not find a valid configuration file.', mtError, [mbOK], '');
      exit;
    end;
  end;

  try
    LINIFile := TINIFile.Create(AINIPathname);

    FJRELocation         := LINIFile.ReadString('Settings',  'jre', cDefaultJRELocation);
    FApplicationName     := LINIFile.ReadString('Settings',  'appname', cDefaultAppName);
    FApplicationFilename := LINIFile.ReadString('Settings',  'appfile', cDefaultFileName);
    FStrightLaunch       := LINIFile.ReadInteger('Settings', 'directrun', 0) = 1;
    FApplicationParams   := LINIFile.ReadString('Settings',  'params', '');
	
    FWebpageURL          := LINIFile.ReadString('Settings',  'web', '');

  finally
    if Assigned(LINIFile) and not (LINIFile = nil) then
      LINIFile.Free;

  end;

  PerformStartupChecks;

end;

procedure TLauncherConfig.PerformStartupChecks;
var
  LExecuteResult:   Integer;
  LPath:            String;
  LJRELocation:     String;

begin

  // First check the JRE Location from the config file (bundled JRE)
  LJRELocation        := FJRELocation;

  LPath               := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  LJRELocation        := LPath + IncludeTrailingPathDelimiter(LJRELocation);

  LJRELocation        := LJRELocation + 'bin\java.exe';

  LExecuteResult := ShellExecute(0, nil, PChar(LJRELocation), PChar(''), nil, 0);

  if LExecuteResult < 32 then
  begin
    // Apparantely the location of the bundle JRE is not valid
    // Check if java is available on the class-path
    LExecuteResult := ShellExecute(0, nil, PChar('java.exe'), PChar(''), nil, 0);

    if LExecuteResult > 32 then
    begin
      // Just calling java.exe should work
      FJREAvailable := True;
      FJRELocation  := 'java.exe';
    end;

    // If by this stage we couldn't find a JRE, then notify the user
    // TODO: Direct the user to the openJDK download page
    if FJRELocation = ''  then
    begin
      MessageDlg('An Error has Occurred!', 'Couldn''t find a Java Runtime Enviroment (x64)', mtError, [mbOK], '');
      exit;
    end;

  end else
  begin
    FJRELocation := LJRELocation;

  end;

end;

end.

