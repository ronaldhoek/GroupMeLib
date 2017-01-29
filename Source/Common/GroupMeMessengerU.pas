unit GroupMeMessengerU;

(*
  INFORMATION

    This unit containts a messenger object which can
    - periodicly request messages form a list of groups
    - does this ASync

  VERSION HISTORY

    2016-12-05 -> initialversion bij Ronald Hoek
*)

interface

uses
  System.Classes, System.SyncObjs, GroupMeObjectsU, GroupMeRequestManagerU;

const
  DefaultUpdateInterval = 5; // 5 seconds

type
  TGroupMeMessageOrder = (moCreatedDescending, moCreatedAscending);

  TGroupMeOnNewMessages = procedure(aGroupID: TGroupMeGroupID; aMessages:
      ArrayOf_TGroupMeMessage; aMessageOrder: TGroupMeMessageOrder) of object;

  ArrayOf_TGroupMeGroupID = array of TGroupMeGroupID;

  ArrayOf_TGroupMeMessageID = array of TGroupMeMessageID;

  TGroupMeMessenger = class(TComponent)
  private
    FActive: Boolean;
    FGroupIDs: ArrayOf_TGroupMeGroupID;
    FGroupLastMessageIDs: array of record
      GroupID: TGroupMeGroupID;
      MessageID: TGroupMeMessageID;
    end;
    FLock: TSynchroObject;
    FOnNewMessages: TGroupMeOnNewMessages;
    FOnRequest: TGroupMeMessengerRequestEvent;
    FPollingThread: TThread;
    FToken: string;
    FUpdateInterval: Integer;
    procedure InternalDeactivated(Sender: TObject);
    procedure SetActive(const Value: Boolean);
    procedure SetGroupIDs(const Value: ArrayOf_TGroupMeGroupID);
    procedure SetToken(const Value: string);
    procedure UpdatePollingThread;
  protected
    procedure Activate;
    procedure Deactivate;
    function LoadLastMessageIDs(const aGroupIDs: ArrayOf_TGroupMeGroupID):
        ArrayOf_TGroupMeMessageID;
    procedure SaveLastMessageIDs(const aGroupIDs: ArrayOf_TGroupMeGroupID; const
        aLastMessageIDs: ArrayOf_TGroupMeMessageID);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    procedure WaitFor;
  published
    property Active: Boolean read FActive write SetActive;
    property GroupIDs: ArrayOf_TGroupMeGroupID read FGroupIDs write SetGroupIDs;
    property Token: string read FToken write SetToken;
    property UpdateInterval: Integer read FUpdateInterval write FUpdateInterval
        default DefaultUpdateInterval;
    property OnNewMessages: TGroupMeOnNewMessages read FOnNewMessages write FOnNewMessages;
    property OnRequest: TGroupMeMessengerRequestEvent read FOnRequest write
        FOnRequest;
  end;

implementation

uses
  System.SysUtils;

type
  TGroupMePollingThread = class(TThread)
  private
    FEvent: TEvent;
    FGroupIDs: ArrayOf_TGroupMeGroupID;
    FGroupLastMessageID: ArrayOf_TGroupMeMessageID;
    FReqMan: TGroupMeRequestManager;
    FOnNewMessages: TGroupMeOnNewMessages;
    FUpdateInterval: Integer;
  protected
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create(AOwner: TGroupMeMessenger);
    destructor Destroy; override;
  end;

{ TGroupMePollingThread }

constructor TGroupMePollingThread.Create(AOwner: TGroupMeMessenger);
begin
  inherited Create(False);
  OnTerminate := AOwner.InternalDeactivated;
  FUpdateInterval := AOwner.FUpdateInterval;
  FGroupIDs := AOwner.FGroupIDs;
  FGroupLastMessageID := AOwner.LoadLastMessageIDs(FGroupIDs);
  FOnNewMessages := AOwner.OnNewMessages;
  FReqMan := TGroupMeRequestManager.Create(nil);
  FReqMan.Token := AOwner.Token;
  FReqMan.OnRequest := AOwner.OnRequest;
  FEvent := TEvent.Create();
end;

destructor TGroupMePollingThread.Destroy;
begin
  inherited;
  FreeAndNil(FEvent);
  FreeAndNil(FReqMan);
end;

procedure TGroupMePollingThread.Execute;
const
  _Order: array[boolean] of TGroupMeMessageOrder = (moCreatedAscending, moCreatedDescending);
var
  bDescending: Boolean;
  I: Integer;
  resp: TGroupMeResponseMessages;
begin
  while not Terminated do
  begin
    for I := 0 to Length(FGroupIDs) - 1 do
    begin
      if Terminated then Exit;
      try
        bDescending := (FGroupLastMessageID[I] = 0);
        resp := FReqMan.GetMessagesAfter(FGroupIDs[I], FGroupLastMessageID[I]);
        try
          if Length(resp.response.messages) > 0 then
          begin
            if bDescending then
              FGroupLastMessageID[I] := resp.response.messages[0].id
            else
              FGroupLastMessageID[I] := resp.response.messages[Length(resp.response.messages)-1].id;

            FOnNewMessages(FGroupIDs[I], resp.response.messages, _Order[bDescending]);
            resp.response.messages := nil;
          end;
        finally
          resp.Free;
        end;
      except
        on E: Exception do;
      end;
    end;
    FEvent.WaitFor(FUpdateInterval * MSecsPerSec);
  end;
end;

procedure TGroupMePollingThread.TerminatedSet;
begin
  FEvent.SetEvent;
end;

{ TGroupMeMessenger }

procedure TGroupMeMessenger.Activate;
begin
  FLock.Acquire;
  try
    if not Assigned(FPollingThread) then
    begin
      if Length(FGroupIDs) > 0 then
        FPollingThread := TGroupMePollingThread.Create(Self);
      FActive := True;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TGroupMeMessenger.BeforeDestruction;
var
  _Thread: TGroupMePollingThread;
begin
  inherited;
  FLock.Acquire;
  try
    if Assigned(FPollingThread) then
    begin
      _Thread := FPollingThread as TGroupMePollingThread;
      FPollingThread := nil;
    end else
      _Thread := nil;
  finally
    FLock.Release;
  end;

  if Assigned(_Thread) then
  begin
    _Thread.Terminate;
    _Thread.WaitFor;
    SaveLastMessageIDs(_Thread.FGroupIDs, _Thread.FGroupLastMessageID);
    _Thread.Free;
  end;
end;

constructor TGroupMeMessenger.Create(AOwner: TComponent);
begin
  FLock := TCriticalSection.Create;
  FUpdateInterval := DefaultUpdateInterval;
  inherited;
end;

procedure TGroupMeMessenger.Deactivate;
begin
  FLock.Acquire;
  try
    if Assigned(FPollingThread) then
      FPollingThread.Terminate;
  finally
    FLock.Release;
  end;
end;

destructor TGroupMeMessenger.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

procedure TGroupMeMessenger.InternalDeactivated(Sender: TObject);
var
  _Thread: TGroupMePollingThread;
begin
  FLock.Acquire;
  try
    if FPollingThread = Sender then
    begin
      FPollingThread := nil;
      _Thread := Sender as TGroupMePollingThread;
      SaveLastMessageIDs(_Thread.FGroupIDs, _Thread.FGroupLastMessageID);
      FActive := False;
    end;
  finally
    FLock.Release;
  end;
end;

function TGroupMeMessenger.LoadLastMessageIDs(const aGroupIDs:
    ArrayOf_TGroupMeGroupID): ArrayOf_TGroupMeMessageID;

  function GetGroupIndex(aGroupID: TGroupMeGroupID; out idxGroup: Integer):
      Boolean;
  var
    I: Integer;
  begin
    for I := 0 to Length(aGroupIDs) - 1 do
      if aGroupIDs[I] = aGroupID then
    begin
      idxGroup := I;
      Exit(True);
    end;
    Result := False;
  end;

var
  I, idxGroup: Integer;
begin
  SetLength(Result, Length(aGroupIDs));
  for I := 0 to Length(FGroupLastMessageIDs) - 1 do
    if GetGroupIndex(FGroupLastMessageIDs[I].GroupID, idxGroup) then
      Result[idxGroup] := FGroupLastMessageIDs[I].MessageID;
end;

procedure TGroupMeMessenger.SaveLastMessageIDs(const aGroupIDs:
    ArrayOf_TGroupMeGroupID; const aLastMessageIDs: ArrayOf_TGroupMeMessageID);

  procedure SaveLastMessageID(aGroupID: TGroupMeGroupID; aLastMessageID:
      TGroupMeMessageID);
  var
    I: Integer;
  begin
    // Find entry
    for I := 0 to Length(FGroupLastMessageIDs) - 1 do
      if FGroupLastMessageIDs[I].GroupID = aGroupID then
    begin
      FGroupLastMessageIDs[I].MessageID := aLastMessageID;
      Exit;
    end;
    // Add entry
    I := Length(FGroupLastMessageIDs);
    SetLength(FGroupLastMessageIDs, I + 1);
    FGroupLastMessageIDs[I].GroupID := aGroupID;
    FGroupLastMessageIDs[I].MessageID := aLastMessageID;
  end;

var
  I: Integer;
begin
  for I := 0 to Length(aGroupIDs) - 1 do
    SaveLastMessageID(aGroupIDs[I], aLastMessageIDs[I]);
end;

procedure TGroupMeMessenger.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
      Exit;

    // Activate of deactivate backgroundthread
    if Value then
      Activate
    else
      Deactivate;
  end;
end;

procedure TGroupMeMessenger.SetGroupIDs(const Value: ArrayOf_TGroupMeGroupID);
begin
  if FGroupIDs <> Value then
  begin
    FGroupIDs := Value;
    if FActive then
      UpdatePollingThread;
  end;
end;

procedure TGroupMeMessenger.SetToken(const Value: string);
begin
  if FToken <> Value then
  begin
    FToken := Value;
    if FActive then
      UpdatePollingThread;
  end;
end;

procedure TGroupMeMessenger.UpdatePollingThread;
var
  _Thread: TGroupMePollingThread;
begin
  FLock.Acquire;
  try
    if Assigned(FPollingThread) then
    begin
      _Thread := FPollingThread as TGroupMePollingThread;
      FPollingThread := nil; // Remove free
      _Thread.Terminate;
      _Thread.WaitFor;
      SaveLastMessageIDs(_Thread.FGroupIDs, _Thread.FGroupLastMessageID);
      _Thread.Free;
    end;
    if Length(FGroupIDs) > 0 then
      FPollingThread := TGroupMePollingThread.Create(Self);
  finally
    FLock.Release;
  end;
end;

procedure TGroupMeMessenger.WaitFor;
var
  _Thread: TThread;
begin
  FLock.Acquire;
  try
    _Thread := FPollingThread;
  finally
    FLock.Release;
  end;
  if Assigned(_Thread) then
    _Thread.WaitFor;
end;

end.
