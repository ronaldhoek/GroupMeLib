unit MainFormU;

(*
  Version history

  2016-11-24 -> initialversion bij Ronald Hoek
*)

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP, FMX.Controls.Presentation, FMX.Edit;

type
  TForm1 = class(TForm)
    btnSaveSettings: TButton;
    Button1: TButton;
    edtBotID: TEdit;
    edtGroupID: TEdit;
    edtPostURL: TEdit;
    IdHTTP1: TIdHTTP;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mmText: TMemo;
    procedure btnSaveSettingsClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IniFiles, AppFuncU, GroupMeConstU;

const
  sSectionSettings = 'Settings';
  sIdentBotID = 'BotID';
  sIdentGroupID = 'GroupID';

procedure TForm1.btnSaveSettingsClick(Sender: TObject);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(AppConfigFile);
  try
    ForceDirectories(AppUserDataPath);
    // NB: use some encryption to store the Bot ID in open platforms with
    // an open filesystem like Windows/Android/MacOS/etc.
    ini.WriteString(sSectionSettings, sIdentBotID, edtBotID.Text);
    ini.WriteString(sSectionSettings, sIdentGroupID, edtGroupID.Text);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  sData: TStrings;
begin
  sData := TStringList.Create;
  try
    sData.Add('{');
    sData.Add('"bot_id"  : "' + edtBotID.Text + '",'); // do not localize
    sData.Add('"text"    : "' + mmText.Text + '"'); // do not localize
    sData.Add('}');

    IdHTTP1.Post(edtPostURL.Text, sData);
  finally
    sData.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ini: TCustomIniFile;
begin
  edtPostURL.Text := sUrlGroupMeApiBot + '/post';
  ini := TMemIniFile.Create(AppConfigFile);
  try
    edtBotID.Text := ini.ReadString(sSectionSettings, sIdentBotID, '');
    edtGroupID.Text := ini.ReadString(sSectionSettings, sIdentGroupID, '');
  finally
    ini.Free;
  end;
end;

end.

