{ Copyright (C) 2013 Thorsten Schmidt (tschmidt@ts-soft.de)              }
{     www.ts-soft.de                                                     }
{                                                                        }
{ This program is free software; you can redistribute it and/or modify   }
{ it under the terms of the GNU General Public License as published by   }
{ the Free Software Foundation; either version 2 of the License, or      }
{ (at your option) any later version.                                    }
{                                                                        }
{ This program is distributed in the hope that it will be useful,        }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of         }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          }
{ GNU General Public License for more details.                           }
{                                                                        }
{ You should have received a copy of the GNU General Public License      }
{ along with this program; if not, write to the Free Software            }
{ Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.              }

unit BionXProfiles;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, IniFiles;

type

  { TBionXProfileFile }

  TBionXProfileFile = class ( TIniFile )
  private
    function GetAssistMinSpeed: double;
    procedure SetAssistMinSpeed ( AValue: double ) ;
    function GetAssistMaxSpeed: double;
    procedure SetAssistMaxSpeed ( AValue: double ) ;
    function GetThrottleMaxSpeed: double;
    procedure SetThrottleMaxSpeed ( AValue: double ) ;

    function GetInitialAssisLevel: integer;
    procedure SetInitialAssistLevel ( AValue: integer ) ;
    function GetAssistLevel1: integer;
    procedure SetAsistLevel1 ( AValue: integer ) ;
    function GetAssistLevel2: integer;
    procedure SetAsistLevel2 ( AValue: integer ) ;
    function GetAssistLevel3: integer;
    procedure SetAsistLevel3 ( AValue: integer ) ;
    function GetAssistLevel4: integer;
    procedure SetAsistLevel4 ( AValue: integer ) ;
    function GetAssistLevelMountain: integer;
    procedure SetAsistLevelMountain ( AValue: integer ) ;

    function GetTorqueSensorGain: double;
    procedure SetTorquesensorGain ( AValue: double ) ;
    function GetTorqueSensorSpeed: integer;
    procedure SetTorquesensorSpeed ( AValue: integer ) ;
    function GetTorqueSensorExtraGain: double;
    procedure SetTorquesensorExtraGain ( AValue: double ) ;
    function GetTorqueSensorExtraGainMaxSpeed: double;
    procedure SetTorquesensorExtraGainMaxSpeed ( AValue: double ) ;

    function GetBoostLevel: double;
    procedure SetBoostLevel ( AValue: double ) ;
    function GetBoostDisplay: integer;
    procedure SetBoostDisplay ( AValue: integer ) ;
  protected
  public
    property AssistMinSpeed : double read GetAssistMinSpeed write SetAssistMinSpeed;
    property AssistMaxSpeed : double read GetAssistMaxSpeed write SetAssistMaxSpeed;
    property ThrottleMaxSpeed : double read GetThrottleMaxSpeed write SetThrottleMaxSpeed;

    property InitialAssistLevel : integer read GetInitialAssisLevel write SetInitialAssistLevel;
    property AssistLevel1 : integer read GetAssistLevel1 write SetAsistLevel1;
    property AssistLevel2 : integer read GetAssistLevel2 write SetAsistLevel2;
    property AssistLevel3 : integer read GetAssistLevel3 write SetAsistLevel3;
    property AssistLevel4 : integer read GetAssistLevel4 write SetAsistLevel4;
    property AssistLevelMountain : integer read GetAssistLevelMountain write SetAsistLevelMountain;

    property TorquesensorGain : double read GetTorqueSensorGain write SetTorquesensorGain;
    property TorquesensorSpeed : integer read GetTorqueSensorSpeed write SetTorquesensorSpeed;
    property TorquesensorExtraGain : double read GetTorqueSensorExtraGain write SetTorquesensorExtraGain;
    property TorquesensorExtraGainMaxSpeed : double read GetTorqueSensorExtraGainMaxSpeed write SetTorquesensorExtraGainMaxSpeed;

    property BoostLevel : double read GetBoostLevel write SetBoostLevel;
    property BoostDisplay : integer read GetBoostDisplay write SetBoostDisplay;
  end;

implementation

const
  SECTION_SPEED_LIMITS                 = 'SPEED_LIMITS';
    ENTRY_ASSIST_MIN_SPEED             = 'ASSIST_MIN_SPEED';
    ENTRY_ASSIST_MAX_SPEED             = 'ASSIST_MAX_SPEED';
    ENTRY_THROTTLE_MAX_SPEED           = 'THROTLE_MAX_SPEED';

  SECTION_ASSIST_LEVELS                = 'ASSIST_LEVELS';
    ENTRY_INITIAL_LEVEL                = 'INITIAL_LEVEL';
    ENTRY_LEVEL_1                      = 'LEVEL_1';
    ENTRY_LEVEL_2                      = 'LEVEL_2';
    ENTRY_LEVEL_3                      = 'LEVEL_3';
    ENTRY_LEVEL_4                      = 'LEVEL_4';
    ENTRY_LEVEL_MOUNTAIN               = 'LEVEL_MOUNTAIN';

  SECTION_TORQUE_SENSOR                = 'TORQUE_SENSOR';
    ENTRY_SENSOR_GAIN                  = 'SENSOR_GAIN';
    ENTRY_SENSOR_SPEED                 = 'SENSOR_SPEED';
    ENTRY_SENSOR_EXTRAGAIN             = 'SENSOR_EXTRAGAIN';
    ENTRY_SENSOR_EXTRAGAIN_MAXSPEED    = 'SENSOR_EXTRAGAIN_MAXSPEED';

  SECTION_BOOST                        = 'BOOST';
    ENTRY_BOOST_LEVEL                  = 'LEVEL';
    ENTRY_BOOST_DISPLAY                = 'DISPLAY';

(******************************************************************************)
{ TBionXProfileFile }


function TBionXProfileFile.GetAssistMinSpeed: double;
begin
  Result := ReadFloat( SECTION_SPEED_LIMITS, ENTRY_ASSIST_MIN_SPEED, -1 );
end;

procedure TBionXProfileFile.SetAssistMinSpeed ( AValue: double ) ;
begin
  WriteFloat( SECTION_SPEED_LIMITS, ENTRY_ASSIST_MIN_SPEED, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetAssistMaxSpeed: double;
begin
  Result := ReadFloat( SECTION_SPEED_LIMITS, ENTRY_ASSIST_MAX_SPEED, -1 );
end;

procedure TBionXProfileFile.SetAssistMaxSpeed ( AValue: double ) ;
begin
  WriteFloat( SECTION_SPEED_LIMITS, ENTRY_ASSIST_MAX_SPEED, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetThrottleMaxSpeed: double;
begin
  Result := ReadFloat( SECTION_SPEED_LIMITS, ENTRY_THROTTLE_MAX_SPEED, -1 );
end;

procedure TBionXProfileFile.SetThrottleMaxSpeed ( AValue: double ) ;
begin
  WriteFloat( SECTION_SPEED_LIMITS, ENTRY_THROTTLE_MAX_SPEED, AValue );
end;

(******************************************************************************)

function TBionXProfileFile.GetInitialAssisLevel: integer;
begin
  Result := ReadInteger ( SECTION_ASSIST_LEVELS, ENTRY_INITIAL_LEVEL, -1 );
end;

procedure TBionXProfileFile.SetInitialAssistLevel ( AValue: integer ) ;
begin
  WriteInteger ( SECTION_ASSIST_LEVELS, ENTRY_INITIAL_LEVEL, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetAssistLevel1: integer;
begin
  Result := ReadInteger ( SECTION_ASSIST_LEVELS, ENTRY_LEVEL_1, -1 );
end;

procedure TBionXProfileFile.SetAsistLevel1 ( AValue: integer ) ;
begin
  WriteInteger ( SECTION_ASSIST_LEVELS, ENTRY_LEVEL_1, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetAssistLevel2: integer;
begin
  Result := ReadInteger ( SECTION_ASSIST_LEVELS, ENTRY_LEVEL_2, -1 );
end;

procedure TBionXProfileFile.SetAsistLevel2 ( AValue: integer ) ;
begin
  WriteInteger ( SECTION_ASSIST_LEVELS, ENTRY_LEVEL_2, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetAssistLevel3: integer;
begin
  Result := ReadInteger ( SECTION_ASSIST_LEVELS, ENTRY_LEVEL_3, -1 );
end;

procedure TBionXProfileFile.SetAsistLevel3 ( AValue: integer ) ;
begin
  WriteInteger ( SECTION_ASSIST_LEVELS, ENTRY_LEVEL_3, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetAssistLevel4: integer;
begin
  Result := ReadInteger ( SECTION_ASSIST_LEVELS, ENTRY_LEVEL_4, -1 );
end;

procedure TBionXProfileFile.SetAsistLevel4 ( AValue: integer ) ;
begin
  WriteInteger ( SECTION_ASSIST_LEVELS, ENTRY_LEVEL_4, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetAssistLevelMountain: integer;
begin
  Result := ReadInteger ( SECTION_ASSIST_LEVELS, ENTRY_LEVEL_MOUNTAIN, -1 );
end;

procedure TBionXProfileFile.SetAsistLevelMountain ( AValue: integer ) ;
begin
  WriteInteger ( SECTION_ASSIST_LEVELS, ENTRY_LEVEL_MOUNTAIN, AValue );
end;

(******************************************************************************)

function TBionXProfileFile.GetTorqueSensorGain: double;
begin
  Result := ReadFloat( SECTION_TORQUE_SENSOR, ENTRY_SENSOR_GAIN, -1 );
end;

procedure TBionXProfileFile.SetTorquesensorGain ( AValue: double ) ;
begin
  WriteFloat( SECTION_TORQUE_SENSOR, ENTRY_SENSOR_GAIN, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetTorqueSensorSpeed: integer;
begin
  Result := ReadInteger( SECTION_TORQUE_SENSOR, ENTRY_SENSOR_SPEED, -1 );
end;

procedure TBionXProfileFile.SetTorquesensorSpeed ( AValue: integer ) ;
begin
  WriteInteger( SECTION_TORQUE_SENSOR, ENTRY_SENSOR_SPEED, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetTorqueSensorExtraGain: double;
begin
  Result := ReadFloat( SECTION_TORQUE_SENSOR, ENTRY_SENSOR_EXTRAGAIN, -1 );
end;

procedure TBionXProfileFile.SetTorquesensorExtraGain ( AValue: double ) ;
begin
  WriteFloat( SECTION_TORQUE_SENSOR, ENTRY_SENSOR_EXTRAGAIN, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetTorqueSensorExtraGainMaxSpeed: double;
begin
  Result := ReadFloat( SECTION_TORQUE_SENSOR, ENTRY_SENSOR_EXTRAGAIN_MAXSPEED, -1 );
end;

procedure TBionXProfileFile.SetTorquesensorExtraGainMaxSpeed ( AValue: double ) ;
begin
  WriteFloat( SECTION_TORQUE_SENSOR, ENTRY_SENSOR_EXTRAGAIN_MAXSPEED, AValue );
end;

(******************************************************************************)

function TBionXProfileFile.GetBoostLevel: double;
begin
  Result := ReadFloat ( SECTION_BOOST, ENTRY_BOOST_LEVEL, -1 );
end;

procedure TBionXProfileFile.SetBoostLevel ( AValue: double ) ;
begin
  WriteFloat ( SECTION_BOOST, ENTRY_BOOST_LEVEL, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXProfileFile.GetBoostDisplay: integer;
begin
  Result := ReadInteger ( SECTION_BOOST, ENTRY_BOOST_DISPLAY, -1 );
end;

procedure TBionXProfileFile.SetBoostDisplay ( AValue: integer ) ;
begin
  WriteInteger ( SECTION_BOOST, ENTRY_BOOST_DISPLAY, AValue );
end;


end.

