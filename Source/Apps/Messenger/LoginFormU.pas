unit LoginFormU;

(*
  INFORMATION

    This unit containts a login form for GroupMe
    send or requested using the GRoupMe API.

  VERSION HISTORY

    2016-11-28 -> initialversion bij Ronald Hoek
*)

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser,
  System.Actions, FMX.ActnList, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, IdContext, IdCustomHTTPServer, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdHTTPServer;

type
  TfrmLogin = class(TForm)
    WebBrowser1: TWebBrowser;
    ActionList1: TActionList;
    actnPaste: TAction;
    oAuthCallbackServer: TIdHTTPServer;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actnPasteExecute(Sender: TObject);
    procedure oAuthCallbackServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure oAuthCallbackServerCommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  private
    FClientID: string;
    FToken: string;
    procedure ProcessRequest(ARequestInfo: TIdHTTPRequestInfo);
  public
    property Token: string read FToken;
  end;

implementation

uses
  System.IniFiles, AppFuncU;

{$R *.fmx}

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  with TMemIniFile.Create(AppMainConfigFile) do
  try
    FClientID := ReadString('Settings', 'ClientID', '');
  finally
    Free;
  end;
end;

procedure TfrmLogin.ProcessRequest(ARequestInfo: TIdHTTPRequestInfo);
begin
  if (ARequestInfo.CommandType = THTTPCommandType.hcGET) then
  begin
    FToken := ARequestInfo.Params.Values['access_token'];
    if FToken > '' then Self.ModalResult := mrOK;
  end;
end;

procedure TfrmLogin.actnPasteExecute(Sender: TObject);
begin
  WebBrowser1.EvaluateJavaScript('document.execCommand(''Paste'');');
end;

procedure TfrmLogin.FormShow(Sender: TObject);
begin
  WebBrowser1.URL := 'https://oauth.groupme.com/oauth/authorize?client_id=' + FClientID;
end;

procedure TfrmLogin.oAuthCallbackServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  ProcessRequest(ARequestInfo);
end;

procedure TfrmLogin.oAuthCallbackServerCommandOther(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  ProcessRequest(ARequestInfo);
end;

end.
