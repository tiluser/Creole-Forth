program Crdemo;

uses
  Forms,
  tabinterface in 'tabinterface.pas' {PagesDlg},
  cr2about in 'cr2about.pas' {AboutBox};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPagesDlg, PagesDlg);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
