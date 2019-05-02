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

       property JRELocation:          String read FJRELocation;
       property ApplcationName:       String read FApplicationName;
       property ApplicationParams:    String read FApplicationParams;
	   property WebPageURL:   		  String read FWebpageURL;

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

