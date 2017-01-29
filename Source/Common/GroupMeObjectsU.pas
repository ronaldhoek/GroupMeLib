unit GroupMeObjectsU;

(*
  INFORMATION

    This unit containts the objects, which represent the data which can be
    send or requested using the GroupMe API.

    Note:
    When receiving nodes with the name 'type', you need to create a property
    with the name 'type_' but with a fieldname of 'Ftype' (without '_'), when
    using the default Delphi JSON REST parser.

  VERSION HISTORY

    2016-11-28 -> Ronald Hoek
      Initial version
    2016-12-21 -> Ronald Hoek
      TGroupMeAttachment object filled with properties
      'type' members can be used now
      TGroupMeMessage 'attachments' available
*)

interface

// Uncomment define to make al timestamps 'integer' which will require the
// actual time to be calculated from UNIX timestamp to Delphi datetime.
{.$DEFINE TIMESTAMP_AS_INTEGER}

type
  (*
    Simples types
  *)

  TGroupMeMessageID = Int64;
  TGroupMeMemberID = Integer;
  TGroupMeGroupID = Integer;

  // Timestamp is always in UTC
{$IFDEF TIMESTAMP_AS_INTEGER}
  TGroupMeTimeStamp = type Integer;
{$ELSE}
  TGroupMeTimeStamp = TDateTime;
{$ENDIF}

  // Conversion functionality for the UNIX/POSIX timestamp
  TGroupMeTimeStampHelper = record helper for TGroupMeTimeStamp
    function ToDateTime(AReturnUTC: Boolean = False): TDateTime;
    function ToString(AReturnUTC: Boolean = False): string;
  end;

  (*
    Complex types
  *)

  TGroupMeMeta = class
  private
    Fcode: Integer;
  public
    property code: Integer read Fcode write Fcode;
  end;

  TGroupMeMember = class
  private
    Fautokicked: Boolean;
    Fid: TGroupMeMemberID;
    Fimage_url: string;
    Fmuted: Boolean;
    Fnickname: string;
    Fuser_id: TGroupMeMemberID;
  public
    property autokicked: Boolean read Fautokicked write Fautokicked;
    property id: TGroupMeMemberID read Fid write Fid;
    property image_url: string read Fimage_url write Fimage_url;
    property muted: Boolean read Fmuted write Fmuted;
    property nickname: string read Fnickname write Fnickname;
    property user_id: TGroupMeMemberID read Fuser_id write Fuser_id;
  end;

  ArrayOf_TGroupMeMember = array of TGroupMeMember;

  TGroupMeAttachment = class
  private
    Ftype: string;
    Furl: string;
  public
    property type_: string read Ftype write Ftype;
    property url: string read Furl write Furl;
  end;

  ArrayOf_TGroupMeAttachment = array of TGroupMeAttachment;

  TGroupMeMessagePreview = class
  private
    Fattachments: ArrayOf_TGroupMeAttachment;
    Fimage_url: string;
    Fnickname: string;
    Ftext: string;
  public
    destructor Destroy; override;
    property attachments: ArrayOf_TGroupMeAttachment read Fattachments write
        Fattachments;
    property image_url: string read Fimage_url write Fimage_url;
    property nickname: string read Fnickname write Fnickname;
    property text: string read Ftext write Ftext;
  end;

  TGroupMeGroupMessageInfo = class
  private
    Fcount: Integer;
    Flast_message_created_at: TGroupMeTimeStamp;
    Flast_message_id: TGroupMeMessageID;
    Fpreview: TGroupMeMessagePreview;
  public
    destructor Destroy; override;
    property count: Integer read Fcount write Fcount;
    property last_message_created_at: TGroupMeTimeStamp read
        Flast_message_created_at write Flast_message_created_at;
    property last_message_id: TGroupMeMessageID read Flast_message_id write
        Flast_message_id;
    property preview: TGroupMeMessagePreview read Fpreview write Fpreview;
  end;

  TGroupMeGroup = class
  private
    Fcreated_at: TGroupMeTimeStamp;
    Fcreator_user_id: TGroupMeMemberID;
    Fdescription: string;
    Fgroup_id: TGroupMeGroupID;
    Fid: TGroupMeGroupID;
    Fimage_url: string;
    Fmax_members: Integer;
    Fmembers: ArrayOf_TGroupMeMember;
    Fmessages: TGroupMeGroupMessageInfo;
    Fname: string;
    Foffice_mode: Boolean;
    Fphone_number: string;
    Fshare_url: string;
    Ftype: string;
    Fupdated_at: TGroupMeTimeStamp;
  public
    destructor Destroy; override;
    property created_at: TGroupMeTimeStamp read Fcreated_at write Fcreated_at;
    property creator_user_id: TGroupMeMemberID read Fcreator_user_id write
        Fcreator_user_id;
    property description: string read Fdescription write Fdescription;
    property group_id: TGroupMeGroupID read Fgroup_id write Fgroup_id;
    property id: TGroupMeGroupID read Fid write Fid;
    property image_url: string read Fimage_url write Fimage_url;
    property max_members: Integer read Fmax_members write Fmax_members;
    property members: ArrayOf_TGroupMeMember read Fmembers write Fmembers;
    property messages: TGroupMeGroupMessageInfo read Fmessages write Fmessages;
    property name: string read Fname write Fname;
    property office_mode: Boolean read Foffice_mode write Foffice_mode;
    property phone_number: string read Fphone_number write Fphone_number;
    property share_url: string read Fshare_url write Fshare_url;
    property type_: string read Ftype write Ftype;
    property updated_at: TGroupMeTimeStamp read Fupdated_at write Fupdated_at;
  end;

  ArrayOf_TGroupMeGroup = array of TGroupMeGroup;

  TGroupMeMessage = class
  private
    Fattachments: ArrayOf_TGroupMeAttachment;
    Favatar_url: string;
    Fcreated_at: TGroupMeTimeStamp;
    Fgroup_id: TGroupMeGroupID;
    Fid: TGroupMeMessageID;
    Fname: string;
    Fsource_guid: string;
    Fsystem: Boolean;
    Ftext: string;
    Fuser_id: TGroupMeMemberID;
  public
    destructor Destroy; override;
    property attachments: ArrayOf_TGroupMeAttachment read Fattachments write
        Fattachments;
    property avatar_url: string read Favatar_url write Favatar_url;
    property created_at: TGroupMeTimeStamp read Fcreated_at write Fcreated_at;
    property group_id: TGroupMeGroupID read Fgroup_id write Fgroup_id;
    property id: TGroupMeMessageID read Fid write Fid;
    property name: string read Fname write Fname;
    property source_guid: string read Fsource_guid write Fsource_guid;
    property system: Boolean read Fsystem write Fsystem;
    property text: string read Ftext write Ftext;
    property user_id: TGroupMeMemberID read Fuser_id write Fuser_id;
  end;

  ArrayOf_TGroupMeMessage = array of TGroupMeMessage;

  (*
    Response objects
  *)

  TGroupMeResponseGroupList = class
  private
    Fmeta: TGroupMeMeta;
    Fresponse: ArrayOf_TGroupMeGroup;
  public
    destructor Destroy; override;
    property meta: TGroupMeMeta read Fmeta write Fmeta;
    property response: ArrayOf_TGroupMeGroup read Fresponse write Fresponse;
  end;

  TGroupMeResponseGroupInfo = class
  private
    Fresponse: TGroupMeGroup;
  public
    destructor Destroy; override;
    property response: TGroupMeGroup read Fresponse write Fresponse;
  end;

  TGroupMeResponseMessagesResponse = class
  private
    Fcount: Integer;
    Fmessages: ArrayOf_TGroupMeMessage;
  public
    destructor Destroy; override;
    property count: Integer read Fcount write Fcount;
    property messages: ArrayOf_TGroupMeMessage read Fmessages write Fmessages;
  end;

  TGroupMeResponseMessages = class
  private
    Fresponse: TGroupMeResponseMessagesResponse;
  public
    destructor Destroy; override;
    property response: TGroupMeResponseMessagesResponse read Fresponse write
        Fresponse;
  end;

  (*
    Helper routine to convert message timestamps to date/time

    http://www.delphifaq.com/faq/f91.shtml
  *)

implementation

uses
  System.SysUtils, System.DateUtils;

{ TGroupMeTimeStampHelper }

function TGroupMeTimeStampHelper.ToDateTime(AReturnUTC: Boolean = False):
    TDateTime;
begin
{$IFDEF TIMESTAMP_AS_INTEGER}
  Result := UnixToDateTime(Self, AReturnUTC);
{$ELSE}
  if AReturnUTC then
    Result := Self
  else
    Result := TTimeZone.Local.ToLocalTime(Self);
{$ENDIF}
end;

function TGroupMeTimeStampHelper.ToString(AReturnUTC: Boolean = False): string;
begin
  Result := DateTimeToStr(ToDateTime(AReturnUTC));
end;

{ TGroupMeMessagePreview }

destructor TGroupMeMessagePreview.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Fattachments) - 1 do
    Fattachments[I].Free;
  inherited;
end;

{ TGroupMeGroupMessageInfo }

destructor TGroupMeGroupMessageInfo.Destroy;
begin
  Fpreview.Free;
  inherited;
end;

{ TGroupMeGroup }

destructor TGroupMeGroup.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Fmembers) - 1 do
    Fmembers[I].Free;
  Fmessages.Free;
  inherited;
end;

{ TGroupMeMessage }

destructor TGroupMeMessage.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Fattachments) - 1 do
    Fattachments[I].Free;
  inherited;
end;


{ TGroupMeResponseGroupList }

destructor TGroupMeResponseGroupList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Fresponse) - 1 do
    Fresponse[I].Free;
  Fmeta.Free;
  inherited;
end;

{ TGroupMeResponseGroupInfo }

destructor TGroupMeResponseGroupInfo.Destroy;
begin
  Fresponse.Free;
  inherited;
end;

{ TGroupMeResponseMessagesResponse }

destructor TGroupMeResponseMessagesResponse.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(Fmessages) - 1 do
    Fmessages[I].Free;
  inherited;
end;

{ TGroupMeResponseMessages }

destructor TGroupMeResponseMessages.Destroy;
begin
  Fresponse.Free;
  inherited;
end;

end.
