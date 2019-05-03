unit LauncherConfig;

{$mode objfpc}{$H+}

interface

uses
  // System
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles;

type

  { TLauncherConfig }

  TLauncherConfig = class(TForm)
     protected
       procedure ReadConfig(AINIPathname : String);

     private
       FJRELocation:         String;
       FApplicationName:     String;
       FApplicationParams:   String;
	   
	   FWebpageURL:          String;

     public
       constructor Create; overload;

       // creates a configuration file from command line options
       procedure CreateConfigFile;

       property JRELocation:          String read FJRELocation;
       property ApplcationName:       String read FApplicationName;
       property ApplicationParams:    String read FApplicationParams;
       property WebPageURL:           String read FWebpageURL;

  end;

implementation

{ TLauncherConfig }

constructor TLauncherConfig.Create;
var
  LINIPathname: String;

begin
  inherited;

  LINIPathname := ParamStr(0);
  LINIPathname := ChangeFileExt(LINIPathname, '.ini');

  ReadConfig(LINIPathname);

end;

procedure TLauncherConfig.CreateConfigFile;
var
  LIniFile:            TIniFile;
  LConfigPath:         String;

  LJRELocation:        String;
  LApplicationName:    String;
  LApplicationJARName: String;
  LApplicationParams:  String;
  LWebAddress:         String;

begin
  // Set the name of the configuration file to that of the application (with extension .ini)
  LConfigPath := ParamStr(0);
  LConfigPath := ChangeFileExt(LConfigPath, '.ini');
  LIniFile := TIniFile.Create(LConfigPath);

  // Figure out what was handed to us on the command line
  LJRELocation := Application.GetOptionValue('jre');
  LApplicationName := Application.GetOptionValue('n','N');
  LApplicationJARName := Application.GetOptionValue('j', 'J');
  LApplicationParams := Application.GetOptionValue('p', 'P');
  LWebAddress := Application.GetOptionValue('w', 'W');

  try
    LIniFile.WriteString('Settings', 'jre',     LJRELocation);
    LIniFile.WriteString('Settings', 'appname', LApplicationName);
    LIniFile.WriteString('Settings', 'app',     LApplicationJARName);
    LIniFile.WriteString('Settings', 'params',  LApplicationParams);
    LIniFile.WriteString('Settings', 'web',     LWebAddress);

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

    FJRELocation := LINIFile.ReadString('Settings', 'jre', '');
    FApplicationName := LINIFile.ReadString('Settings', 'app', '');
    FApplicationParams := LINIFile.ReadString('Settings', 'params', '');
	
    FWebpageURL := LINIFile.ReadString('Settings', 'web', '');

  finally
    if Assigned(LINIFile) and not (LINIFile = nil) then
      LINIFile.Free;

  end;

end;

end.

