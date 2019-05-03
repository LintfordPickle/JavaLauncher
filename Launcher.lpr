program Launcher;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainForm,
  LauncherConfig,
  SysUtils,
  Windows,
  VersionForm;

{$R *.res}
var
  GAppIconFilename:  String;
  GConfiguration:    TLauncherConfig;

begin
  RequireDerivedFormResource:=True;

  GConfiguration := TLauncherConfig.Create;

  // Check to see if the param -c was used, and create a new config file if so
  if Application.HasOption('c', 'config') then
  begin
    GConfiguration.CreateConfigFile;
    exit;
  end else // otherwise load config from file
    GConfiguration.LoadConfigFile;

  // Change the application icon
  GAppIconFilename := ParamStr(0);
  GAppIconFilename := ChangeFileExt(GAppIconFilename, '.ico');

  if FileExists(GAppIconFilename) then
    Application.Icon.LoadFromFile(GAppIconFilename);

  Application.Title:='JavaLauncher';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);

  // Initalize the launcher
  Form1.Initialize(GConfiguration);

  Application.Run;
end.

