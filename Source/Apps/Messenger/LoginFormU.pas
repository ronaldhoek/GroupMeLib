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
  System.Actions, FMX.ActnList, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TfrmLogin = class(TForm)
    WebBrowser1: TWebBrowser;
    ActionList1: TActionList;
    actnPaste: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actnPasteExecute(Sender: TObject);
    procedure WebBrowser1DidFinishLoad(ASender: TObject);
  private
    FClientID: string;
    FToken: string;
  public
    property Token: string read FToken;
  end;

implementation

uses
  System.IniFiles, AppFuncU, IdURI;

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

procedure TfrmLogin.actnPasteExecute(Sender: TObject);
begin
  WebBrowser1.EvaluateJavaScript('document.execCommand(''Paste'');');
end;

procedure TfrmLogin.FormShow(Sender: TObject);
begin
  WebBrowser1.URL := 'https://oauth.groupme.com/oauth/authorize?client_id=' + FClientID;
end;

procedure TfrmLogin.WebBrowser1DidFinishLoad(ASender: TObject);
var
  p: TStrings;
  uri: TIdURI;
  I: Integer;
begin
  uri := TIdURI.Create((ASender as TWebBrowser).URL);
  try
    // Is URL specified in GroupMe application profile
    if SameText(uri.Host, 'localhost') and (uri.Params > '') then
    begin
      p := TStringList.Create;
      try
        p.Delimiter := '&';
        p.StrictDelimiter := True;
        // See 'TIdHTTPRequestInfo.DecodeAndSetParams' regarding '+' replacement
        p.DelimitedText := StringReplace(uri.Params, '+', ' ', [rfReplaceAll]);
        // Decode parameters (not required in this situation ????)
        for I := 0 to p.Count - 1 do
          p[I] := TIdURI.URLDecode(p[I]);

        FToken := p.Values['access_token'];
        if FToken > '' then Self.ModalResult := mrOK;
      finally
        p.Free;
      end;
    end;
  finally
    uri.Free;
  end;
end;

end.
