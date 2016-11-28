unit AppFuncU;

(*
  Version history

  2016-11-24 -> initialversion bij Ronald Hoek
*)

interface

  function AppUserDataPath: string;
  function AppConfigFile: string;

implementation

uses
  System.SysUtils, System.IOUtils;

function AppUserDataPath: string;
begin
  if TOSVersion.Platform in [pfiOS, pfAndroid] then
    Result := TPath.GetHomePath
  else
    Result := TPath.GetHomePath + TPath.DirectorySeparatorChar + 'GroupMePostApp';
end;

function AppConfigFile: string;
begin
  Result := AppUserDataPath + TPath.DirectorySeparatorChar + 'settings.ini';
end;

end.
