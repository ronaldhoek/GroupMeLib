unit GroupMeRequestManagerU;

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

const
  iDefaultPageSizeGroupList = 10;
  iDefaultPageSizeMessages = 5;

type
  TGroupMeMessengerRequestType = (grtGet, grtPost);

  TGroupMeMessengerRequestEvent = procedure(const aRequestID: TGUID; aType:
      TGroupMeMessengerRequestType; const URL, RequestData: string; out
      ResponseData: string) of object;

  ///  <summary>
  ///  This object containt the base requests for interacting with the
  ///  GroupMe api. It does NOT contain HTTP transport code, to make the
  ///  request API as customisable as possible.
  ///
  ///  Override 'InternalDoRequest' or set the 'OnRequest' event to actualy send
  ///  the request using your own favorite HTTP methods.
  ///
  ///  The 'aRequestID' parameter is something like a correlation ID, when
  ///  using the messenger in ASync processed/threads.
  ///  </summary>
  TGroupMeRequestManager = class(TComponent)
  private
    FBaseURL: string;
    FOnRequest: TGroupMeMessengerRequestEvent;
    FPageSizeGroupList: Integer;
    FPageSizeMessages: Integer;
    FToken: string;
    function GetBaseURIMessages(aGroupID: TGroupMeGroupID): string;
    function GetTokenParam: string;
    procedure InternalDoRequest(aType: TGroupMeMessengerRequestType; const URL,
        Data: string; out ResponseData: string); virtual;
  protected
    procedure DoRequest(const aRequestID: TGUID; aType:
        TGroupMeMessengerRequestType; const URL, Data: string; out ResponseData:
        string); virtual;
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
        FPageSizeGroupList default iDefaultPageSizeGroupList;
    property PageSizeMessages: Integer read FPageSizeMessages write
        FPageSizeMessages default iDefaultPageSizeMessages;
    property Token: string read FToken write FToken;
  published
    property OnRequest: TGroupMeMessengerRequestEvent read FOnRequest write
        FOnRequest;
  end;

implementation

uses
  System.SysUtils, REST.Json, GroupMeConstU;

const
  sTokenParam = 'token=%s';

constructor TGroupMeRequestManager.Create(AOwner: TComponent);
begin
  FPageSizeGroupList := iDefaultPageSizeGroupList;
  FPageSizeMessages := iDefaultPageSizeMessages;
  FBaseURL := sUrlGroupMeApi;
  inherited;
end;

procedure TGroupMeRequestManager.DoRequest(const aRequestID: TGUID; aType:
    TGroupMeMessengerRequestType; const URL, Data: string; out ResponseData:
    string);
begin
  if Assigned(FOnRequest) then
    FOnRequest(aRequestID, aType, URL, Data, ResponseData)
  else
    ResponseData := '';
end;

procedure TGroupMeRequestManager.InternalDoRequest(aType:
    TGroupMeMessengerRequestType; const URL, Data: string; out ResponseData:
    string);
begin
  DoRequest(TGUID.NewGuid, aType, URL, Data, ResponseData);
end;

function TGroupMeRequestManager.GetBaseURIMessages(aGroupID: TGroupMeGroupID):
    string;
begin
  Result := FBaseURL + '/groups/' + aGroupID.ToString() + '/messages' +
    '?' + GetTokenParam +
    '&limit=' + PageSizeMessages.ToString();
end;

function TGroupMeRequestManager.GetGroupInfo(aGroupID: TGroupMeGroupID):
    TGroupMeResponseGroupInfo;
var
  sURI, sResponse: string;
begin
  sURI := FBaseURL + '/groups/' + aGroupID.ToString() +
    '?' + GetTokenParam;
  InternalDoRequest(grtGet, sURI, '', sResponse);
  Result := TJson.JsonToObject<TGroupMeResponseGroupInfo>(sResponse, [joDateFormatUnix]);
end;

function TGroupMeRequestManager.GetGroupList(aPage: Integer):
    TGroupMeResponseGroupList;
var
  sURI, sResponse: string;
begin
  sURI := FBaseURL + '/groups' +
    '?' + GetTokenParam +
    '&per_page=' + PageSizeGroupList.ToString() +
    '&page=' + aPage.ToString();
  InternalDoRequest(grtGet, sURI, '', sResponse);
  Result := TJson.JsonToObject<TGroupMeResponseGroupList>(sResponse, [joDateFormatUnix]);
end;

function TGroupMeRequestManager.GetMessages(aGroupID: TGroupMeGroupID):
    TGroupMeResponseMessages;
var
  sURI, sResponse: string;
begin
  sURI := GetBaseURIMessages(aGroupID);
  InternalDoRequest(grtGet, sURI, '', sResponse);
  Result := TJson.JsonToObject<TGroupMeResponseMessages>(sResponse, [joDateFormatUnix]);
end;

function TGroupMeRequestManager.GetMessagesAfter(aGroupID: TGroupMeGroupID;
    aAfterID: TGroupMeMessageID): TGroupMeResponseMessages;
var
  sURI, sResponse: string;
begin
  if aAfterID = 0 then
    Result := GetMessages(aGroupID)
  else begin
    sURI := GetBaseURIMessages(aGroupID) + '&after_id=' + aAfterID.ToString();
    InternalDoRequest(grtGet, sURI, '', sResponse);
    Result := TJson.JsonToObject<TGroupMeResponseMessages>(sResponse, [joDateFormatUnix]);
  end;
end;

function TGroupMeRequestManager.GetMessagesBefore(aGroupID: TGroupMeGroupID;
    aBeforeID: TGroupMeMessageID): TGroupMeResponseMessages;
var
  sURI, sResponse: string;
begin
  if aBeforeID = 0 then
    Result := GetMessages(aGroupID)
  else begin
    sURI := GetBaseURIMessages(aGroupID) + '&before_id=' + aBeforeID.ToString();
    InternalDoRequest(grtGet, sURI, '', sResponse);
    Result := TJson.JsonToObject<TGroupMeResponseMessages>(sResponse, [joDateFormatUnix]);
  end;
end;

function TGroupMeRequestManager.GetTokenParam: string;
begin
  Result := Format(sTokenParam, [FToken]);
end;

end.
