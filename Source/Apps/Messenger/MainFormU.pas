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
  FMX.NumberBox, GroupMeMessengerU, GroupMeObjectsU;

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
    procedure btnGetBGroupsClick(Sender: TObject);
    procedure btnGetGroupClick(Sender: TObject);
    procedure btnGetMessagesClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMessenger: TGroupMeMessenger;
    procedure TokenChanged;
    procedure OnRequest(aType: TGroupMeMessengerRequestType; const URL, Data:
        string; out ResponseData: string);
  public
    constructor Create(AOwner: TComponent); override;
    function GetGroupID: TGroupMeGroupID;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  LoginFormU, System.IniFiles, AppFuncU;

{$R *.fmx}

procedure TfrmMain.btnGetBGroupsClick(Sender: TObject);
var
  aList: TGroupMeResponseGroupList;
  I, iPage: Integer;
begin
  FMessenger.PageSizeGroupList := Round(edtGroupListPageSize.Value);
  iPage := 1;
  try
    repeat
      aList := FMessenger.GetGroupList(iPage);
      try
        if Length(aList.response) > 0 then
          for I := 0 to Length(aList.response) - 1 do
            Memo1.Lines.Add(
                aList.response[I].id.ToString + ' - ' +
                aList.response[I].name + ' - ' +
                aList.response[I].max_members.ToString
              )
        else
          Break;
      finally
        aList.Free;
      end;
      Inc(iPage);
    until False;
  except
    on E: Exception do
      Memo1.Lines.Add(E.ClassName + ' - ' + E.Message);
  end;
end;

procedure TfrmMain.btnGetGroupClick(Sender: TObject);
var
  _GroupInfo: TGroupMeResponseGroupInfo;
begin
  _GroupInfo := FMessenger.GetGroupInfo(GetGroupID);
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
      FMessenger.Token := Token;
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
  FMessenger := TGroupMeMessenger.Create(Self);
  FMessenger.OnRequest := OnRequest;
  inherited;
end;

procedure TfrmMain.btnGetMessagesClick(Sender: TObject);
var
  aList: TGroupMeResponseMessages;
  aMsgID: TGroupMeMessageID;
  I, iCount: Integer;
begin
  FMessenger.PageSizeGroupList := Round(edtGroupListPageSize.Value);
  try
    aMsgID := 0;
    iCount := 0;
    repeat
      aList := FMessenger.GetMessagesBefore(GetGroupID, aMsgID);
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
  except
    on E: Exception do
      Memo1.Lines.Add(E.ClassName + ' - ' + E.Message);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ini: TCustomIniFile;
begin
  ini := TMemIniFile.Create(AppConfigFile);
  try
    FMessenger.Token := ini.ReadString('UserAccess', 'Token', '');
    TokenChanged;
  finally
    ini.Free;
  end;
end;

function TfrmMain.GetGroupID: TGroupMeGroupID;
var
  sGroupID: string;
begin
  sGroupID := Memo1.Lines[Memo1.CaretPosition.Line];
  Delete(sGroupID, Pos(' ', sGroupID), Length(sGroupID));
  Result := StrToInt(sGroupID);
end;

procedure TfrmMain.OnRequest(aType: TGroupMeMessengerRequestType; const URL,
    Data: string; out ResponseData: string);
var
  str: TStream;
begin
  case aType of
    grtGet:
      ResponseData := IdHTTP1.Get(URL);
    grtPost:
      begin
        str := TStringStream.Create(Data);
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
  Label1.Text := 'Token: ' + FMessenger.Token;
end;

end.
