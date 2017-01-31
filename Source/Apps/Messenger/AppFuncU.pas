unit AppFuncU;

(*
  INFORMATION

    This unit containts the objects, which represent the data which can be
    send or requested using the GRoupMe API.

  VERSION HISTORY

    2016-11-28 -> initialversion bij Ronald Hoek
*)

interface

  function AppMainConfigFile: string;
  function AppUserDataPath: string;
  function AppUserConfigFile: string;

implementation

uses
  System.SysUtils, System.IOUtils;

function AppMainConfigFile: string;
begin
  Result := TPath.ChangeExtension(GetModuleName(MainInstance), '.ini');
end;

function AppUserDataPath: string;
begin
  if TOSVersion.Platform in [pfiOS, pfAndroid] then
    Result := TPath.GetHomePath
  else
    Result := TPath.GetHomePath + TPath.DirectorySeparatorChar + 'GroupMeMessenger';
end;

function AppUserConfigFile: string;
begin
  Result := AppUserDataPath + TPath.DirectorySeparatorChar + 'settings.ini';
end;

end.
