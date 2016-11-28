unit GroupMeMessengerU;

(*
  INFORMATION

    This unit containt the basic code for creating GroupMe API request.
    NOTE this code uses generics, so you must a least have Delpi XE/XE2 or up!

    The basic idea for the unit, is to only create the request and decode the
    response to the known GroupMe API object structures.

    The actuel HTTP trasnfer needs to be implemented in another unit or by
    assigning the correct events!

    Currently I've hard coded the 'REST.Json' to marshal the request and
    responses. But I'd like to 'virtualize' this as well (like the HTTP part).

  VERSION HISTORY

    2016-11-28 -> initialversion bij Ronald Hoek
*)

interface

uses
  System.Classes, GroupMeObjectsU;

type
  TGroupMeMessengerRequestType = (grtGet, grtPost);

  TGroupMeMessengerRequestEvent = procedure(aType: TGroupMeMessengerRequestType;
    const URL, Data: string; out ResponseData: string) of object;

  ///  <summary>
  ///  This object containt the base requests for interacting with the
  ///  GroupMe api. It does NOT contain HTTP transport code, to make the
  ///  request API as customisable as possible.
  ///
  ///  Override 'DoRequest' or set the 'OnRequest' event to actualy send
  ///  the request using your own favorite HTTP methods.
  ///  </summary>
  TGroupMeMessenger = class(TComponent)
  private
    FBaseURL: string;
    FOnRequest: TGroupMeMessengerRequestEvent;
    FPageSizeGroupList: Integer;
    FPageSizeMessages: Integer;
    FToken: string;
    function GetBaseURIMessages(aGroupID: TGroupMeGroupID): string;
    function GetTokenParam: string;
  protected
    procedure DoRequest(aType: TGroupMeMessengerRequestType; const URL, Data:
        string; out ResponseData: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function GetGroupInfo(aGroupID: TGroupMeGroupID): TGroupMeResponseGroupInfo;
    function GetGroupList(aPage: Integer): TGroupMeResponseGroupList;
    function GetMessages(aGroupID: TGroupMeGroupID): TGroupMeResponseMessages;
    function GetMessagesAfter(aGroupID: TGroupMeGroupID; aAfterID:
        TGroupMeMessageID): TGroupMeResponseMessages;
    function GetMessagesBefore(aGroupID: TGroupMeGroupID; aBeforeID:
        TGroupMeMessageID): TGroupMeResponseMessages;
    property BaseURL: string read FBaseURL write FBaseURL;
    property PageSizeGroupList: Integer read FPageSizeGroupList write
        FPageSizeGroupList default 10;
    property PageSizeMessages: Integer read FPageSizeMessages write
        FPageSizeMessages default 20;
    property Token: string read FToken write FToken;
    property OnRequest: TGroupMeMessengerRequestEvent read FOnRequest write
        FOnRequest;
  end;

implementation

uses
  System.SysUtils, REST.Json, GroupMeConstU;

const
  sTokenParam = 'token=%s';

constructor TGroupMeMessenger.Create(AOwner: TComponent);
begin
  FPageSizeGroupList := 10;
  FPageSizeMessages := 20;
  FBaseURL := sUrlGroupMeApi;
  inherited;
end;

procedure TGroupMeMessenger.DoRequest(aType: TGroupMeMessengerRequestType;
    const URL, Data: string; out ResponseData: string);
begin
  if Assigned(FOnRequest) then
    FOnRequest(aType, URL, Data, ResponseData)
  else
    ResponseData := '';
end;

function TGroupMeMessenger.GetBaseURIMessages(aGroupID: TGroupMeGroupID):
    string;
begin
  Result := FBaseURL + '/groups/' + aGroupID.ToString() + '/messages' +
    '?' + GetTokenParam +
    '&limit=' + PageSizeMessages.ToString();
end;

function TGroupMeMessenger.GetGroupInfo(aGroupID: TGroupMeGroupID):
    TGroupMeResponseGroupInfo;
var
  sURI, sResponse: string;
begin
  sURI := FBaseURL + '/groups/' + aGroupID.ToString() +
    '?' + GetTokenParam;
  DoRequest(grtGet, sURI, '', sResponse);
  Result := TJson.JsonToObject<TGroupMeResponseGroupInfo>(sResponse);
end;

function TGroupMeMessenger.GetGroupList(aPage: Integer):
    TGroupMeResponseGroupList;
var
  sURI, sResponse: string;
begin
  sURI := FBaseURL + '/groups' +
    '?' + GetTokenParam +
    '&per_page=' + PageSizeGroupList.ToString() +
    '&page=' + aPage.ToString();
  DoRequest(grtGet, sURI, '', sResponse);
  Result := TJson.JsonToObject<TGroupMeResponseGroupList>(sResponse);
end;

function TGroupMeMessenger.GetMessages(aGroupID: TGroupMeGroupID):
    TGroupMeResponseMessages;
var
  sURI, sResponse: string;
begin
  sURI := GetBaseURIMessages(aGroupID);
  DoRequest(grtGet, sURI, '', sResponse);
  Result := TJson.JsonToObject<TGroupMeResponseMessages>(sResponse);
end;

function TGroupMeMessenger.GetMessagesAfter(aGroupID: TGroupMeGroupID;
    aAfterID: TGroupMeMessageID): TGroupMeResponseMessages;
var
  sURI, sResponse: string;
begin
  if aAfterID = 0 then
    Result := GetMessages(aGroupID)
  else begin
    sURI := GetBaseURIMessages(aGroupID) + '&after_id=' + aAfterID.ToString();
    DoRequest(grtGet, sURI, '', sResponse);
    Result := TJson.JsonToObject<TGroupMeResponseMessages>(sResponse);
  end;
end;

function TGroupMeMessenger.GetMessagesBefore(aGroupID: TGroupMeGroupID;
    aBeforeID: TGroupMeMessageID): TGroupMeResponseMessages;
var
  sURI, sResponse: string;
begin
  if aBeforeID = 0 then
    Result := GetMessages(aGroupID)
  else begin
    sURI := GetBaseURIMessages(aGroupID) + '&before_id=' + aBeforeID.ToString();
    DoRequest(grtGet, sURI, '', sResponse);
    Result := TJson.JsonToObject<TGroupMeResponseMessages>(sResponse);
  end;
end;

function TGroupMeMessenger.GetTokenParam: string;
begin
  Result := Format(sTokenParam, [FToken]);
end;

end.
