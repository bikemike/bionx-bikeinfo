unit BikeInfoIni;

{$mode objfpc}{$H+}

interface

uses
  IniFiles;
//  Classes,
//  SysUtils;

type

  { TBikeInfoIni }

  TBikeInfoIni = class ( TIniFile )
  private
    function GetAdapter: string;
    procedure SetAdapter ( AValue: string ) ;
  public
    property Adapter : string read GetAdapter write SetAdapter;
  end;

implementation

const
  section_Config       = 'Config';
    entry_Adapter      = 'Adapter';
    default_Adapter    = 'TinyCAN';

{ TBikeInfoIni }

function TBikeInfoIni.GetAdapter: string;
begin
  Result := ReadString ( section_Config, entry_Adapter, default_Adapter );
end;

procedure TBikeInfoIni.SetAdapter ( AValue: string ) ;
begin
  WriteString ( section_Config, entry_Adapter, AValue );
end;

end.

