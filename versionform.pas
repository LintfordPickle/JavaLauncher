unit versionform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TVersion }

  TVersion = class(TForm)
    CloseButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure CloseButtonClick(Sender: TObject);
  private

  public

  end;

var
  Version: TVersion;

implementation

{$R *.lfm}

{ TVersion }

// Closes the form
procedure TVersion.CloseButtonClick(Sender: TObject);
begin
  Close;

end;

end.

