unit MainFormU;

(*
  INFORMATION

    This unit containts a basic test form for GroupMe to
    - request available groups
    - request group details
    - request group messages

  VERSION HISTORY

    2016-11-28 -> initialversion bij Ronald Hoek
*)

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.EditBox,
  FMX.NumberBox, GroupMeRequestManagerU, GroupMeObjectsU, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView;

type
  TfrmMain = class(TForm)
    btnGetBGroups: TButton;
    btnGetGroup: TButton;
    btnLogin: TButton;
    edtGroupListPageSize: TNumberBox;
    IdHTTP1: TIdHTTP;
    Label1: TLabel;
    Memo1: TMemo;
    btnGetMessages: TButton;
    lvGroups: TListView;
    procedure btnGetBGroupsClick(Sender: TObject);
    procedure btnGetGroupClick(Sender: TObject);
    procedure btnGetMessagesClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure edtGroupListPageSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvGroupsChange(Sender: TObject);
  private
    FRequestManager: TGroupMeRequestManager;
    function GetGroupID(out aGroupID: TGroupMeGroupID): Boolean;
    procedure LoadMessages(aGRoupID: TGroupMeGroupID);
    procedure TokenChanged;
    procedure OnRequest(const aRequestID: TGUID; aType:
        TGroupMeMessengerRequestType; const URL, RequestData: string; out
        ResponseData: string);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  LoginFormU, System.IniFiles, AppFuncU;

{$R *.fmx}

procedure TfrmMain.btnGetBGroupsClick(Sender: TObject);
begin
  lvGroups.Items.Clear;
  TThread.CreateAnonymousThread(
    procedure
    var
      aList: TGroupMeResponseGroupList;
      iPage: Integer;
    begin
      iPage := 1;
      try
        repeat
          aList := FRequestManager.GetGroupList(iPage);
          try
            if Length(aList.response) > 0 then
            begin
              TThread.Synchronize(nil,
                procedure
                var
                  I: Integer;
                  item: TListViewItem;
                begin
                  for I := 0 to Length(aList.response) - 1 do
                  begin
                    item := lvGroups.Items.Add;
                    item.Text := aList.response[I].name;
                    item.Tag := aList.response[I].id;
                  end;
                end);
            end else
              Break;
          finally
            aList.Free;
          end;
          Inc(iPage);
        until False;
      except
        on E: Exception do
          TThread.Synchronize(nil,
            procedure
            begin
              Memo1.Lines.Add(E.ClassName + ' - ' + E.Message);
            end);
      end;
    end).Start;
end;

procedure TfrmMain.btnGetGroupClick(Sender: TObject);
var
  aGroupID: TGroupMeGroupID;
  _GroupInfo: TGroupMeResponseGroupInfo;
begin
  if not GetGroupID(aGroupID) then Exit;

  _GroupInfo := FRequestManager.GetGroupInfo(aGroupID);
  try
    Memo1.Lines.Add(
        _GroupInfo.response.id.ToString + ' - ' +
        _GroupInfo.response.name + ' - ' +
        _GroupInfo.response.messages.last_message_id.ToString()
      );
  finally
    _GroupInfo.Free;
  end;
end;

procedure TfrmMain.btnLoginClick(Sender: TObject);
var
  ini: TCustomIniFile;
begin
  with TfrmLogin.Create(Self) do
  try
    if ShowModal = mrOK then
    begin
      FRequestManager.Token := Token;
      ForceDirectories(AppUserDataPath);
      ini := TMemIniFile.Create(AppConfigFile);
      try
        ini.WriteString('UserAccess', 'Token', Token);
        TMemIniFile(ini).UpdateFile;
      finally
        ini.Free;
      end;
      TokenChanged;
    end;
  finally
    Free;
  end;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  FRequestManager := TGroupMeRequestManager.Create(Self);
  FRequestManager.OnRequest := OnRequest;
  inherited;
end;

procedure TfrmMain.btnGetMessagesClick(Sender: TObject);
var
  aGroupID: TGroupMeGroupID;
begin
  if GetGroupID(aGroupID) then
  try
    Memo1.Lines.Clear;
    LoadMessages(aGRoupID);
  except
    on E: Exception do
      Memo1.Lines.Add(E.ClassName + ' - ' + E.Message);
  end;
end;

procedure TfrmMain.edtGroupListPageSizeChange(Sender: TObject);
begin
  FRequestManager.PageSizeGroupList := Round(edtGroupListPageSize.Value);
//  FRequestManager.PageSizeGroupList := Round(edtGroupListPageSize.Value);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ini: TCustomIniFile;
begin
  ini := TMemIniFile.Create(AppConfigFile);
  try
    FRequestManager.Token := ini.ReadString('UserAccess', 'Token', '');
    TokenChanged;
  finally
    ini.Free;
  end;
end;

function TfrmMain.GetGroupID(out aGroupID: TGroupMeGroupID): Boolean;
var
  item: TListViewItem;
begin
  item := (lvGroups.Selected as TListViewItem);
  if Assigned(item) then
  begin
    aGroupID := item.Tag;
    Result := True;
  end else
    Result := False;
end;

procedure TfrmMain.LoadMessages(aGRoupID: TGroupMeGroupID);
var
  aList: TGroupMeResponseMessages;
  aMsgID: TGroupMeMessageID;
  I, iCount: Integer;
begin
  aMsgID := 0;
  iCount := 0;
  repeat
    aList := FRequestManager.GetMessagesBefore(aGroupID, aMsgID);
    try
      if (Length(aList.response.messages) > 0) and (iCount < 100) then
      begin
        for I := 0 to Length(aList.response.messages) - 1 do
          Memo1.Lines.Add(
              aList.response.messages[I].id.ToString + ' - ' +
              aList.response.messages[I].source_guid + ' - ' +
              aList.response.messages[I].created_at.ToString() + ' - ' +
              aList.response.messages[I].text
            );

        // Last message ID
        aMsgID := aList.response.messages[Length(aList.response.messages) - 1].id;

        Inc(iCount, Length(aList.response.messages));
      end else
        Break;
    finally
      aList.Free;
    end;
  until False;
end;

procedure TfrmMain.lvGroupsChange(Sender: TObject);
var
  item: TListViewItem;
begin
  item := (Sender as TListView).Selected as TListViewItem;
  if Assigned(item) then
  begin
    Memo1.Lines.Clear;
    LoadMessages(item.Tag);
  end;
end;

procedure TfrmMain.OnRequest(const aRequestID: TGUID; aType:
    TGroupMeMessengerRequestType; const URL, RequestData: string; out
    ResponseData: string);
var
  str: TStream;
begin
  case aType of
    grtGet:
      ResponseData := IdHTTP1.Get(URL);
    grtPost:
      begin
        str := TStringStream.Create(RequestData);
        try
          ResponseData := IdHTTP1.Post(URL, str);
        finally
          str.Free;
        end;
      end
  else
    ResponseData := '';
  end;
end;

procedure TfrmMain.TokenChanged;
begin
  Label1.Text := 'Token: ' + FRequestManager.Token;
end;

end.
