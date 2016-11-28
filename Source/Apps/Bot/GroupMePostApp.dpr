program GroupMePostApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  GroupMeConstU in '..\..\Common\GroupMeConstU.pas',
  AppFuncU in 'AppFuncU.pas',
  MainFormU in 'MainFormU.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
