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
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  GroupMeMessengerU, System.SyncObjs, FMX.Layouts, FMX.TreeView;

type
  TfrmMain = class(TForm)
    btnGetGroup: TButton;
    btnLogin: TButton;
    edtGroupListPageSize: TNumberBox;
    IdHTTP1: TIdHTTP;
    Label1: TLabel;
    lvGroups: TListView;
    TreeView1: TTreeView;
    btnSend: TButton;
    mmText: TMemo;
    procedure btnGetGroupClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure edtGroupListPageSizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FHttpUseLock: TSynchroObject;
    FMessenger: TGroupMeMessenger;
    FRequestManager: TGroupMeRequestManager;
    procedure AddTreeViewGroup(aGroup: TGroupMeGroup);
    procedure AddTreeViewMessage(aParent: TTreeViewItem; aMessage: TGroupMeMessage);
    function GetGroupID(out aGroupID: TGroupMeGroupID): Boolean;
    function GetTreeViewGroup(aGroupID: TGroupMeGroupID): TTreeViewItem;
    procedure InitMessengerGroups;
    procedure TokenChanged;
    procedure OnRequest(const aRequestID: TGUID; aType:
        TGroupMeMessengerRequestType; const URL, RequestData: string; out
        ResponseData: string);
  protected
    procedure OnNewMessages(aGroupID: TGroupMeGroupID; aMessages:
        ArrayOf_TGroupMeMessage; aMessageOrder: TGroupMeMessageOrder; var
        aEventHandlerOwnsMessages: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  LoginFormU, System.IniFiles, AppFuncU;

{$R *.fmx}

procedure TfrmMain.btnGetGroupClick(Sender: TObject);
var
  aGroupID: TGroupMeGroupID;
  _GroupInfo: TGroupMeResponseGroupInfo;
begin
  if not GetGroupID(aGroupID) then Exit;

  _GroupInfo := FRequestManager.GetGroupInfo(aGroupID);
  try
    ShowMessage(
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
      FMessenger.Token := Token;
      ForceDirectories(AppUserDataPath);
      ini := TMemIniFile.Create(AppUserConfigFile);
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
  FHttpUseLock := TCriticalSection.Create;
  FRequestManager := TGroupMeRequestManager.Create(Self);
  FRequestManager.OnRequest := OnRequest;
  FMessenger := TGroupMeMessenger.Create(Self);
  FMessenger.OnRequest := OnRequest;
  FMessenger.OnNewMessages := OnNewMessages;
  FMessenger.Active := True;
  inherited;
end;

destructor TfrmMain.Destroy;
begin
  FHttpUseLock.Acquire;
  FMessenger.Active := False;
  FMessenger.WaitFor;
  inherited;
  FreeAndNil(FHttpUseLock);
end;

procedure TfrmMain.AddTreeViewGroup(aGroup: TGroupMeGroup);
var
  tvi: TTreeViewItem;
begin
  tvi := TTreeViewItem.Create(TreeView1);
  tvi.Text := aGroup.name;
  tvi.Tag := aGroup.group_id;
  TreeView1.AddObject(tvi);
end;

procedure TfrmMain.AddTreeViewMessage(aParent: TTreeViewItem; aMessage:
    TGroupMeMessage);

  function AttData: string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to Length(aMessage.attachments) - 1 do
      Result := Result + ' ' + aMessage.attachments[I].type_ + ':' + aMessage.attachments[I].url;
  end;

var
  tvi: TTreeViewItem;
begin
  tvi := TTreeViewItem.Create(aParent);
  tvi.Text := aMessage.created_at.ToString() + ' - ' + aMessage.text + AttData();
  tvi.Tag := aMessage.id;
  aParent.AddObject(tvi);
end;

procedure TfrmMain.btnSendClick(Sender: TObject);
var
  aGroupID: TGroupMeGroupID;
  msg: TGroupMeSendMessage;
begin
  if not GetGroupID(aGroupID) then Exit;

  msg := TGroupMeSendMessage.Create;
  msg.message.text := mmText.Lines.Text;

  TThread.CreateAnonymousThread(
    procedure
    begin
      FRequestManager.SendMessage(aGroupID, msg);
    end).Start;

  mmText.Lines.Clear;
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
  ini := TMemIniFile.Create(AppUserConfigFile);
  try
    FRequestManager.Token := ini.ReadString('UserAccess', 'Token', '');
    FMessenger.Token := FRequestManager.Token;
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

function TfrmMain.GetTreeViewGroup(aGroupID: TGroupMeGroupID): TTreeViewItem;
var
  I: Integer;
begin
  for I := 0 to TreeView1.Count - 1 do
  begin
    Result := TreeView1.Items[I];
    if Result.Tag = aGroupID then
      Exit;
  end;
  Result := nil;
end;

procedure TfrmMain.InitMessengerGroups;
begin
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
                  _GroupIDs: ArrayOf_TGroupMeGroupID;
                begin
                  SetLength(_GroupIDs, Length(aList.response));
                  for I := 0 to Length(aList.response) - 1 do
                  begin
                    _GroupIDs[I] := aList.response[I].id;
                    // ListView
                    item := lvGroups.Items.Add;
                    item.Text := aList.response[I].name;
                    item.Tag := aList.response[I].id;
                    // TreeView
                    AddTreeViewGroup(aList.response[I]);
                  end;
                  FMessenger.GroupIDs := _GroupIDs;
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
              ShowMessage(E.ClassName + ' - ' + E.Message);
            end);
      end;
    end).Start;
end;

procedure TfrmMain.OnNewMessages(aGroupID: TGroupMeGroupID; aMessages:
    ArrayOf_TGroupMeMessage; aMessageOrder: TGroupMeMessageOrder; var
    aEventHandlerOwnsMessages: boolean);
begin
  TThread.Synchronize(nil,
    procedure
    var
      tvi: TTreeViewItem;
      I: Integer;
    begin
      tvi := GetTreeViewGroup(aGroupID);

      // what's the order of the message list?
      case aMessageOrder of
        moCreatedDescending:
          for I := Length(aMessages) - 1 downto 0 do
            AddTreeViewMessage(tvi, aMessages[I]);
        moCreatedAscending:
          for I := 0 to Length(aMessages) - 1 do
            AddTreeViewMessage(tvi, aMessages[I]);
      end;
    end);
end;

procedure TfrmMain.OnRequest(const aRequestID: TGUID; aType:
    TGroupMeMessengerRequestType; const URL, RequestData: string; out
    ResponseData: string);
var
  str: TStream;
begin
  if FHttpUseLock = nil then Exit;

  FHttpUseLock.Acquire;
  try
    if FHttpUseLock = nil then Exit;
    case aType of
      grtGet:
        ResponseData := IdHTTP1.Get(URL);
      grtPost:
        begin
          str := TStringStream.Create(RequestData);
          try
            IdHTTP1.Request.ContentType := 'application/json; charset=UTF-8';
            ResponseData := IdHTTP1.Post(URL, str);
          finally
            str.Free;
          end;
        end
    else
      ResponseData := '';
    end;
  finally
    FHttpUseLock.Release;
  end;
end;

procedure TfrmMain.TokenChanged;
begin
  Caption := 'Token: ' + FRequestManager.Token;
  if FRequestManager.Token > '' then
    InitMessengerGroups;
end;

end.
