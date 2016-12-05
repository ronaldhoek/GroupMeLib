program GroupMeMessenger;

(*
  INFORMATION

    This unit containts a login form for GroupMe
    send or requested using the GRoupMe API.

  VERSION HISTORY

    2016-11-28 -> initialversion bij Ronald Hoek
*)

uses
  System.StartUpCopy,
  FMX.Forms,
  GroupMeConstU in '..\..\Common\GroupMeConstU.pas',
  GroupMeObjectsU in '..\..\Common\GroupMeObjectsU.pas',
  GroupMeRequestManagerU in '..\..\Common\GroupMeRequestManagerU.pas',
  AppFuncU in 'AppFuncU.pas',
  MainFormU in 'MainFormU.pas' {frmMain},
  LoginFormU in 'LoginFormU.pas' {frmLogin};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
