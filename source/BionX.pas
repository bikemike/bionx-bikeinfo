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
{                                                                        }
{ Portions of this software are taken and adapted from BigXionFlasher    }
{ published by Thomas König <info@bigxionflasher.org>                    }
{                                                                        }
{ special thanks to elsi from open-ebike.com who provided a list with    }
{ the identified registers                                               }

unit BionX;

{$mode objfpc}{$H+}

{$define adapter}

interface
uses
  CANAdapter;

type
  { TBionXComponent }
  TBionXComponent = class ( TObject )
  private
    FCANId   : byte;
    FCANAdapter : PCANAdapter;

    // data access helpers
    function GetByteValue ( Reg : byte ) : byte;
    procedure SetByteValue ( Reg : byte; Value : byte );

    function GetBoolValue ( Reg : byte ) : boolean;
    procedure SetBoolValue ( Reg : byte; Value : boolean );

    function  GetWordValue ( RegHIGH : byte ) : word;
    procedure SetWordValue ( RegHIGH : byte; Value : word );

    function GetDWordValue ( RegHIGH : byte ) : dword;
    procedure SetDWordValue ( RegHIGH : byte; Value : dword );


    // common property gets
    function GetHardwareVersion: byte; virtual; abstract;
    function GetSoftwareVersion: byte; virtual; abstract;
    function GetPartNumber : word; virtual; abstract;
    function GetItemNumber : word; virtual; abstract;
    function GetProductionDate : TDate; virtual; abstract;
  protected


  public
    constructor Create ( CANAdapter : PCANAdapter; CANId : byte );

    // common properties
    property CANId : byte read FCANId;
    property HardwareVersion : byte read GetHardwareVersion;
    property SoftwareVersion : byte read GetSoftwareVersion;
    property PartNumber : word read GetPartNumber;
    property ItemNumber : word read GetItemNumber;
    property ProductionDate : TDate read GetProductionDate;

    // for evaluation purpose only
    property ByteValue [ Reg : byte ] : byte read GetByteValue write SetByteValue;
  end;


  { TBionXConsole }

  TBionXConsole = class ( TBionXComponent )
  private
    function GetHardwareVersion: byte; override;
    function GetSoftwareVersion: byte; override;
    function GetPartNumber : word; override;
    function GetItemNumber : word; override;
    function GetProductionDate : TDate; override;

    function GetOdometer: double;
    procedure SetOdometer ( AValue : double ) ;
    function GetDistance: double;
    procedure SetDistance ( AValue : double ) ;
    function GetAverageSpeed: double;
    procedure SetAverageSpeed ( AValue : double ) ;
    function GetChrono: TTime;
    procedure SetChrono ( AValue : TTime ) ;

    function GetSlaveMode: boolean;
    procedure SetSlaveMode ( AValue: boolean ) ;

    function GetWheelCircumference: word;
    procedure SetWheelCircumference ( AValue: word ) ;
    function GetFlippedSwitches: boolean;
    procedure SetFlippedSwitches ( AValue: boolean ) ;
    function GetMetricUnits: boolean;
    procedure SetMetricUnits ( AValue: boolean ) ;
    function GetShowRemainigDistanceAndTime: boolean;
    procedure SetShowRemainigDistanceAndTime ( AValue : boolean ) ;

    function GetAssistMaxSpeedFlag: boolean;
    procedure SetAssistMaxSpeedFlag(AValue: boolean);
    function GetAssistMaxSpeed: double;
    procedure SetAssistMaxSpeed(AValue: double);
    function GetAssistMinSpeedFlag: boolean;
    procedure SetAssistMinSpeedFlag(AValue: boolean);
    function GetAssistMinSpeed: double;
    procedure SetAssistMinSpeed(AValue: double);
    function GetThrottleMaxSpeedFlag: boolean;
    procedure SetThrottleMaxSpeedFlag(AValue: boolean);
    function GetThrottleMaxSpeed: double;
    procedure SetThrottleMaxSpeed(AValue: double);

    function GetTorqueSensorGain: double;
    procedure SetTorqueSensorGain ( AValue: double ) ;
    function GetTorqueSensorSpeed: byte;
    procedure SetTorqueSensorSpeed ( AValue: byte ) ;
    function GetTorqueSensorExtraGain: double;
    procedure SetTorqueSensorExtraGain ( AValue: double ) ;
    function GetTorqueSensorExtraGainMaxSpeed: double;
    procedure SetTorqueSensorExtraGainMaxSpeed ( AValue: double ) ;

    function GetBoostLevel : byte;
    procedure SetBoostLevel ( AValue : byte ) ;

    function GetInitialAssistLevel: byte;
    procedure SetInitialAssistLevel ( AValue: byte ) ;
    function GetAssistLevel1: double;
    procedure SetAssistLevel1 ( AValue: double ) ;
    function GetAssistLevel2: double;
    procedure SetAssistLevel2 ( AValue: double ) ;
    function GetAssistLevel3: double;
    procedure SetAssistLevel3 ( AValue: double ) ;
    function GetAssistLevel4: double;
    procedure SetAssistLevel4 ( AValue: double ) ;
    function GetMountainAssistLevel: double;
    procedure SetMountainAssistLevel ( AValue: double ) ;

    function GetBrakeSensorFlag: boolean;
    procedure SetBrakeSensorFlag ( AValue: boolean ) ;
    function GetBrakeSensorType: byte;
    procedure SetBrakeSensorType ( AValue: byte ) ;
    function GetBrakeRekuperationLevel: byte;
    procedure SetBrakeRekuperationLevel ( AValue: byte ) ;

  protected
  public
    function SetToSlaveMode : boolean;
    // RO properties
    property Distance : double read GetDistance write SetDistance; // value cannot be written
    property AverageSpeed : double read GetAverageSpeed write SetAverageSpeed; // value cannot be written
    property Chrono : TTime read GetChrono write SetChrono; // value cannot be written

    // R/W properties
    property SlaveMode : boolean read GetSlaveMode write SetSlaveMode;

    property WheelCircumference : word read GetWheelCircumference write SetWheelCircumference;
    property FlippedSwitches : boolean read GetFlippedSwitches write SetFlippedSwitches;
    property MetricUnits : boolean read GetMetricUnits write SetMetricUnits;
    property ShowRemainigDistanceAndTime : boolean read GetShowRemainigDistanceAndTime write SetShowRemainigDistanceAndTime;
    property Odometer : double read GetOdometer write SetOdometer;

    property AssistMaxSpeedFlag : boolean read GetAssistMaxSpeedFlag write SetAssistMaxSpeedFlag;
    property AssistMaxSpeed : double read GetAssistMaxSpeed write SetAssistMaxSpeed;
    property AssistMinSpeedFlag : boolean read GetAssistMinSpeedFlag write SetAssistMinSpeedFlag;
    property AssistMinSpeed : double read GetAssistMinSpeed write SetAssistMinSpeed;
    property ThrottleMaxSpeedFlag : boolean read GetThrottleMaxSpeedFlag write SetThrottleMaxSpeedFlag;
    property ThrottleMaxSpeed : double read GetThrottleMaxSpeed write SetThrottleMaxSpeed;

    property TorqueSensorGain : double read GetTorqueSensorGain write SetTorqueSensorGain;
    property TorqueSensorSpeed : byte read GetTorqueSensorSpeed write SetTorqueSensorSpeed;
    property TorqueSensorExtraGain : double read GetTorqueSensorExtraGain write SetTorqueSensorExtraGain;
    property TorqueSensorExtraGainMaxSpeed : double read GetTorqueSensorExtraGainMaxSpeed write SetTorqueSensorExtraGainMaxSpeed;

    property BoostLevel : byte read GetBoostLevel write SetBoostLevel;

    property InitialAssistLevel : byte read GetInitialAssistLevel write SetInitialAssistLevel;
    property AssistLevel1 : double read GetAssistLevel1 write SetAssistLevel1;
    property AssistLevel2 : double read GetAssistLevel2 write SetAssistLevel2;
    property AssistLevel3 : double read GetAssistLevel3 write SetAssistLevel3;
    property AssistLevel4 : double read GetAssistLevel4 write SetAssistLevel4;
    property MountainAssistLevel : double read GetMountainAssistLevel write SetMountainAssistLevel;

    property BrakeSensorFlag : boolean read GetBrakeSensorFlag write SetBrakeSensorFlag;
    property BrakeSensorType : byte read GetBrakeSensorType write SetBrakeSensorType;
    property BrakeRekuperationLevel : byte read GetBrakeRekuperationLevel write SetBrakeRekuperationLevel;
  end;

  { TBionXMotor }

  TBionXMotor = class ( TBionXComponent )
  private
    function GetHardwareVersion: byte; override;
    function GetSoftwareVersion: byte; override;
    function GetPartNumber : word; override;
    function GetItemNumber : word; override;
    function GetProductionDate : TDate; override;

    procedure UnlockProtection;

    function GetMaxSpeed: double;
    procedure SetMaxSpeed(AValue: double);
    function GetWheelCircumference: word;
    procedure SetWheelCircumference(AValue: word);
    function GetTemperature: byte;
  protected
  public
    // RO properties
    property Temperature : byte read GetTemperature;

    // R/W properties
    property MaxSpeed : double read GetMaxSpeed write SetMaxSpeed;
    property WheelCircumference : word read GetWheelCircumference write SetWheelCircumference;
  public
  end;


  { TBionXBattery }

  TBionXBattery = class ( TBionXComponent )
  private
    function GetHardwareVersion: byte; override;
    function GetSoftwareVersion: byte; override;
    function GetPartNumber : word; override;
    function GetItemNumber : word; override;
    function GetProductionDate : TDate; override;

    function GetAutoShutdownDelay: word;
    procedure SetAutoShutdownDelay ( AValue: word ) ;

    function GetChargeTimeMean: word;
    function GetChargeTimeWorst: word;
    function GetChargeLevel: double;
    function GetVoltage: double;
    function GetInputVoltage : double;
    function GetBalancerEnabled: boolean;
    function GetPackSerial: byte;
    function GetPackParallel: byte;
    function GetChargeTimes(Level : byte): word;
    function GetCellVoltage(CellNo : byte): double;
    function GetPackTemperature(PackNo : byte): byte;
    function GetChargeCycles: word;
    function GetFullChargeCycles: word;
    function GetPowerCycles: word;
    function GetCellCapacity: double;
    function GetResets: word;
    function GetLMD: double;
    function GetVCtrlShorts: byte;
    function GetVoltageMax: double;
    function GetVoltageMin: double;
    function GetVoltageMean: double;
    function GetTemperatureMax: byte;
    function GetTemperatureMin: byte;
  protected
  public
    procedure Shutdown;

    // RO properties
    property ChargeLevel : double read GetChargeLevel;
    property Voltage : double read GetVoltage;
    property InputVoltage : double read GetInputVoltage;
    property BalancerEnabled : boolean read GetBalancerEnabled;
    property PackSerial : byte read GetPackSerial;
    property PackParallel : byte read GetPackParallel;
    property ChargeTimes[Level : byte] : word read GetChargeTimes;
    property CellVoltage[CellNo : byte] : double read GetCellVoltage;
    property PackTemperature[PackNo : byte] : byte read GetPackTemperature;
    property ChargeCycles : word read GetChargeCycles;
    property FullChargeCycles : word read GetFullChargeCycles;
    property PowerCycles : word read GetPowerCycles;
    property CellCapacity : double read GetCellCapacity;
    property ChargeTimeMean : word read GetChargeTimeMean;
    property ChargeTimeWorst : word read GetChargeTimeWorst;
    property Resets : word read GetResets;
    property LMD : double read GetLMD;
    property VCtrlShorts : byte read GetVCtrlShorts;
    property VoltageMax : double read GetVoltageMax;
    property VoltageMin : double read GetVoltageMin;
    property VoltageMean : double read GetVoltageMean;
    property TemperatureMax : byte read GetTemperatureMax;
    property TemperatureMin : byte read GetTemperatureMin;

    // R/W properties
    property AutoShutdownDelay : word read GetAutoShutdownDelay write SetAutoShutdownDelay;

  public
  end;


  { TBionXBike }

  TBionXBike = class ( TObject )
  private
    FCANAdapter: TCANAdapter;
    FConsole : TBionXConsole;
    FMotor   : TBionXMotor;
    FBattery : TBionxBattery;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function Connect ( CANAdapter : TCANAdapter ) : boolean;
    procedure Disconnect;

    // as some of the settings require changes in console and motor, I decided
    // to route all Set calls through the Bike class.
    // It's a little more work against accessing the console/motor members direct,
    // but this way we have all register manipulations at one place rather than
    // spread over the code
    // value validation is also done here
    procedure SetWheelCircumference ( Circumference : word ) ;
    procedure SetConsoleSwitchFlipped ( Flipped : boolean );
    procedure SetMetricUnits ( MetricUnits : boolean );

    procedure SetAssistMaxSpeed ( Speed : double );
    procedure SetAssistMinSpeed ( Speed : double );
    procedure SetThrottleMaxSpeed ( Speed : double );

    procedure SetTorqueSensorGain ( Gain : double );
    procedure SetTorqueSensorSpeed ( Speed : byte );

    procedure SetTorqueSensorExtraGain ( Gain : double );
    procedure SetTorqueSensorExtraGainMaxSpeed ( Speed : double );

    procedure SetBoostLevel ( BoostLevel : byte );

    procedure SetBrakeSensor ( SensorEnabled : boolean; SensorType : byte );
    procedure SetBrakeRekuperationLevel ( Level : byte );

    procedure SetAssistInitialLevel ( Level : byte );

    procedure SetAssistLevel1 ( Level : double );
    procedure SetAssistLevel2 ( Level : double );
    procedure SetAssistLevel3 ( Level : double );
    procedure SetAssistLevel4 ( Level : double );
    procedure SetAssistLevelMountain ( Level : double );

    procedure SetAutoShutdownDelay ( delay : word );
    procedure SetShowRemainingDistanceAndTime ( Show : boolean );


    procedure SetOdometer ( Odometer : double );
    procedure SetDistance ( Distance : double );
    procedure SetAverageSpeed ( AvgSpeed : double );
    procedure SetChrono ( Chrono : TTime );

    property Console : TBionXConsole read FConsole;
    property Motor : TBionXMotor read FMotor;
    property Battery : TBionxBattery read FBattery;

  end;

function BoolToStr ( b : boolean ) : string;
function BrakeSensorTypeToStr ( SensorType : byte ) : string;

implementation
uses
  SysUtils;

const

  UNLIMITED_SPEED_VALUE                 = 70; // Km/h
//  UNLIMITED_MIN_SPEED_VALUE             = 30; // Km/h
  UNLIMITED_THROTTLE_SPEED_VALUE        = 70; // Km/h

  SPEED_FACTOR                          = 0.1;
  DISTANCE_FACTOR                       = 0.1;
  SENESORGAIN_FACTOR                    = 0.1;
  ASSIST_LEVEL_FAKTOR                   = 1.5625;
  BATTERY_VOLTAGE_OFFSET                = 20.8333;
  BATTERY_VOLTAGE_FAKTOR                = 0.416667;

  ID_CONSOLE                                     = $48; // (CAN ID in slave mode)

    // Reg 0..79 unused

    REG_CONSOLE_STATUS_DIST_HI                   = $50; //  80
    REG_CONSOLE_STATUS_DIST_LO                   = $51; //  81
    REG_CONSOLE_STATUS_AVGSPEED_HI               = $52; //  82
    REG_CONSOLE_STATUS_AVGSPEED_LO               = $53; //  83

    // Reg 84..99 unused

    REG_CONSOLE_STATUS_ODO_HI                    = $64; // 100 total distance MSB
    REG_CONSOLE_STATUS_ODO_2                     = $65; // 101 total distance
    REG_CONSOLE_STATUS_ODO_3                     = $66; // 102 total distance
    REG_CONSOLE_STATUS_ODO_4                     = $67; // 103 total distance LSB

    REG_CONSOLE_104                              = $68;
    REG_CONSOLE_105                              = $69;
    REG_CONSOLE_106                              = $6A;
    REG_CONSOLE_107                              = $6B;
    REG_CONSOLE_108                              = $6C; // ??? detected (throttle?) ???
    REG_CONSOLE_CHRONO_SECOND                    = $6D; // 109
    REG_CONSOLE_CHRONO_MINUTE                    = $6E; // 110
    REG_CONSOLE_CHRONO_HOUR                      = $6F; // 111
    REG_CONSOLE_112                              = $70;
    REG_CONSOLE_113                              = $71; // ??? location ???
    REG_CONSOLE_PRODUCTIONDATE_YEAR              = $72; // 114 mfd year
    REG_CONSOLE_PRODUCTIONDATE_MONTH             = $73; // 115 mfd month
    REG_CONSOLE_PRODUCTIONDATE_DAY               = $74; // 116 mfd day

    REG_CONSOLE_SN_PN_HI                         = $75; // 117 product number MSB
    REG_CONSOLE_SN_PN_LO                         = $76; // 118 product number LSB
    REG_CONSOLE_SN_ITEM_HI                       = $77; // 119 serial number MSB
    REG_CONSOLE_SN_ITEM_LO                       = $78; // 120 serial number LSB

    // Reg 121 unused

    REG_CONSOLE_122                              = $7A; // ??? gauge joint ???
    REG_CONSOLE_123                              = $7B; // ??? min (throttle?) MSB
    REG_CONSOLE_124                              = $7C; // ??? min (throttle?) LSB
    REG_CONSOLE_125                              = $7D; // ??? max (throttle?) MSB
    REG_CONSOLE_126                              = $7E; // ??? max (throttle?) LSB

    // Reg 127 unused

    REG_CONSOLE_REG_128                          = $80;

    REG_CONSOLE_GEOMETRY_CIRC_HI                 = $81; // 129 circumference MSB
    REG_CONSOLE_GEOMETRY_CIRC_LO                 = $82; // 130 circumference LSB

    REG_CONSOLE_ASSIST_MAXSPEEDFLAG              = $83; // 131 max speed flag
    REG_CONSOLE_ASSIST_MAXSPEED_HI               = $84; // 132 max speed MSB PC3773
    REG_CONSOLE_ASSIST_MAXSPEED_LO               = $85; // 133 max speed LSB

    REG_CONSOLE_THROTTLE_MAXSPEEDFLAG            = $86; // 134 throttle max speed flag
    REG_CONSOLE_THROTTLE_MAXSPEED_HI             = $87; // 135 throttle max speed MSB PC3775
    REG_CONSOLE_THROTTLE_MAXSPEED_LO             = $88; // 136 throttle max speed LSB
    REG_CONSOLE_ASSIST_MINSPEEDFLAG              = $89; // 137 min speed flag
    REG_CONSOLE_ASSIST_MINSPEED                  = $8A; // 138 min speed PC3776

    REG_CONSOLE_BRAKE_REKU_LEVEL                 = $8B; // 139 brake rekuperation level PC2002
    REG_CONSOLE_SHOW_REMAINING_FLAG              = $8C; // 140 0:hide, 1:show remaining time/dist PC2003 (no effect anymore?)
    REG_CONSOLE_METRICUNITS                      = $8D; // 141 units 0: miles/mph, 1:kilometers/km/h
    REG_CONSOLE_142                              = $8E; // ??? Enable On Strain ???

    // Reg 143..159 unused

    REG_CONSOLE_BRAKESENSOR_FLAG                 = $A0; // 160 0 : brake sensor off, 1 : sensor on  PC2006
    REG_CONSOLE_BRAKESENSOR_TYPE                 = $A1; // 161 0: NO, 1: NC, 2:0-5, 3:5-0           PC2006
    REG_CONSOLE_TORQUESENSOR_SPEED               = $A2; // 162 0..8 (1..4 recommended) PC1234

    REG_CONSOLE_REF_SW                           = $A3; // 163 software version

    REG_CONSOLE_TORQUESENSOR_GAIN                = $A4; // 164  sensor gain 0.1..4.0 PC0007
    REG_CONSOLE_TORQUESENSOR_EXTRAGAIN           = $A5; // 165  extra gain 0.1..4.0 PC0008A
    REG_CONSOLE_TORQUESENSOR_EXTRAGAIN_MAXSPEED  = $A6; // 166  extra gain max speed 0.0..25.0 PC0008B
    REG_CONSOLE_167                              = $A7; // ??? Type 0: EPS; 1: RIDE+; 2: BOOST ???
    REG_CONSOLE_168                              = $A8;
    REG_CONSOLE_169                              = $A9;
    REG_CONSOLE_170                              = $AA; // ??? Enable Boost Display ???
    REG_CONSOLE_171                              = $AB; // ??? Autoregen Flag ???

    // Reg 172 unused

    REG_CONSOLE_173                              = $AD;
    REG_CONSOLE_174                              = $AE;
    REG_CONSOLE_175                              = $AF;

    REG_CONSOLE_ASSIST_LEVEL_1                   = $B0; // 176 assist level 1
    REG_CONSOLE_ASSIST_LEVEL_2                   = $B1; // 177 assist level 2
    REG_CONSOLE_ASSIST_LEVEL_3                   = $B2; // 178 assist level 3
    REG_CONSOLE_ASSIST_LEVEL_4                   = $B3; // 179 assist level 4
    REG_CONSOLE_ASSIST_INITLEVEL                 = $B4; // 180 initial assist level

    REG_CONSOLE_181                              = $B5;
    REG_CONSOLE_182                              = $B6;
    REG_CONSOLE_183                              = $B7;
    REG_CONSOLE_184                              = $B8;
    REG_CONSOLE_185                              = $B9;
    REG_CONSOLE_186                              = $BA;
    REG_CONSOLE_187                              = $BB;
    REG_CONSOLE_188                              = $BC;
    REG_CONSOLE_189                              = $BD; // ??? OEM MSB ???
    REG_CONSOLE_190                              = $BE; // ??? OEM LSB ???
    REG_CONSOLE_191                              = $BF;
    REG_CONSOLE_192                              = $C0;
    REG_CONSOLE_193                              = $C1; // ??? PRODUCT MSB ???
    REG_CONSOLE_194                              = $C2; // ??? PRODUCT LSB ???
    REG_CONSOLE_BOOST_LEVEL                      = $C3; // 195 Boost Trigger Level  PC3779
    REG_CONSOLE_KEYFLIP_FLAG                     = $C4; // 196 0:left on/off, 1:right on/off  PC2009
    REG_CONSOLE_197                              = $C5;

    REG_CONSOLE_ASSIST_LEVEL_MOUNTAIN            = $C6; // 198 assist level mountain mode
    REG_CONSOLE_199                              = $C7;

    // Reg 200..207 unused

    REG_CONSOLE_REF_HW                           = $D0; // 208 hardware version
    REG_CONSOLE_STATUS_SLAVE                     = $D1; // 209 1:Slavemode, cannot be set to 0

    REG_CONSOLE_210                              = $D2;
    REG_CONSOLE_211                              = $D3;
    REG_CONSOLE_212                              = $D4;

    // Reg 213 unused
    // Reg 214 unused

    REG_CONSOLE_215                              = $D7;
    REG_CONSOLE_216                              = $D8;
    REG_CONSOLE_217                              = $D9;
    REG_CONSOLE_218                              = $DA;
    REG_CONSOLE_219                              = $DB;
    REG_CONSOLE_220                              = $DC;

    // Reg 211 unused

    REG_CONSOLE_222                              = $DE;
    REG_CONSOLE_223                              = $DF;

    // Reg 224..255 unused

  ID_BATTERY                                     = $10;
    REG_BATTERY_CONFIG_SHUTDOWN                  = $25;

    REG_BATTERY_STATUS_VOLTAGE_MEAN              = $33;

    REG_BATTERY_REF_HW                           = $3b;
    REG_BATTERY_REF_SW                           = $3c;

    REG_BATTERY_STATUS_RESETS_HI                 = $48;
    REG_BATTERY_STATUS_RESETS_LO                 = $49;

    REG_BATTERY_CONFIG_AUTOSHUTDOWNDELAY_HI      = $56;
    REG_BATTERY_CONFIG_AUTOSHUTDOWNDELAY_LO      = $57;

    REG_BATTERY_STATUS_CHARGELEVEL               = $61;
    REG_BATTERY_CELLMON_BALANCERENABLED          = $65;

    REG_BATTERY_STATUS_TEMPERATURE_PACK_1        = $66;
    REG_BATTERY_STATUS_TEMPERATURE_PACK_2        = $67;
    REG_BATTERY_STATUS_TEMPERATURE_PACK_3        = $68;
    REG_BATTERY_STATUS_TEMPERATURE_PACK_4        = $69;
    REG_BATTERY_STATUS_TEMPERATURE_PACK_5        = $6A;
    REG_BATTERY_STATUS_TEMPERATURE_PACK_6        = $6B;

    REG_BATTERY_CELLVOLTAGE_CHANNEL              = $6c;
    REG_BATTERY_CELLVOLTAGE_DATA_HI              = $6d;
    REG_BATTERY_CELLVOLTAGE_DATA_LO              = $6e;

    REG_BATTERY_PRODUCTIONDATE_YEAR              = $72; // 114
    REG_BATTERY_PRODUCTIONDATE_MONTH             = $73; // 115
    REG_BATTERY_PRODUCTIONDATE_DAY               = $74; // 116

    REG_BATTERY_SN_PN_HI                         = $75;
    REG_BATTERY_SN_PN_LO                         = $76;
    REG_BATTERY_SN_ITEM_HI                       = $77;
    REG_BATTERY_SN_ITEM_LO                       = $78;

    REG_BATTERY_STATS_CHARGETIMEMEAN_HI          = $8A;
    REG_BATTERY_STATS_CHARGETIMEMEAN_LO          = $8B;
    REG_BATTERY_STATS_CHARGETIMEWORST_HI         = $8C;
    REG_BATTERY_STATS_CHARGETIMEWORST_LO         = $8D;

    REG_BATTERY_STATUS_CHARGECYCLES_HI           = $8e;
    REG_BATTERY_STATUS_CHARGECYCLES_LO           = $8f;

    REG_BATTERY_STATUS_FULLCHARGECYCLES_HI       = $92;
    REG_BATTERY_STATUS_FULLCHARGECYCLES_LO       = $93;

    REG_BATTERY_STATUS_POWERCYCLES_HI            = $96;
    REG_BATTERY_STATUS_POWERCYCLES_LO            = $97;

    REG_BATTERY_STATUS_VOLTAGE_MAX               = $98;
    REG_BATTERY_STATUS_VOLTAGE_MIN               = $99;

    REG_BATTERY_STATUS_INPUTVOLTAGE_HI           = $9A;
    REG_BATTERY_STATUS_INPUTVOLTAGE_LO           = $9B;

    REG_BATTERY_STATUS_TEMPERATURE_MAX           = $9C;
    REG_BATTERY_STATUS_TEMPERATURE_MIN           = $9D;

    REG_BATTERY_STATUS_VCTRLSHORTS               = $9E;

    REG_BATTERY_STATUS_VOLTAGE_HI                = $a6;
    REG_BATTERY_STATUS_VOLTAGE_LO                = $a7;

    REG_BATTERY_CONFIG_PACKSERIAL                = $ae;
    REG_BATTERY_CONFIG_PACKPARALLEL              = $af;

    REG_BATTERY_STATUS_LMD_HI                    = $D5;
    REG_BATTERY_STATUS_LMD_LO                    = $D6;

    REG_BATTERY_CHARGE_TIMES_CHANNEL             = $F6;
    REG_BATTERY_CHARGE_TIMES_DATA_HI             = $F7;
    REG_BATTERY_CHARGE_TIMES_DATA_LO             = $F8;

    REG_BATTERY_CONFIG_CELLCAPACITY_HI           = $fd;
    REG_BATTERY_CONFIG_CELLCAPACITY_LO           = $fe;


  ID_MOTOR                                       = $20;
    REG_MOTOR_REF_HW                             = $19;
    REG_MOTOR_REF_SW                             = $20;

    REG_MOTOR_CIRC_HI                            = $44;
    REG_MOTOR_CIRC_LO                            = $45;

    REG_MOTOR_SN_ITEM_HI                         = $60;
    REG_MOTOR_SN_ITEM_LO                         = $61;
    REG_MOTOR_SN_PN_HI                           = $62;
    REG_MOTOR_SN_PN_LO                           = $63;
    REG_MOTOR_PRODUCTIONDATE_YEAR                = $64;
    REG_MOTOR_PRODUCTIONDATE_MONTH               = $65;
    REG_MOTOR_PRODUCTIONDATE_DAY                 = $66;

    REG_MOTOR_ASSIST_MAXSPEED                    = $8b;

    REG_MOTOR_PROTECT_UNLOCK                     = $a5;
      MOTOR_PROTECT_UNLOCK_KEY                   = $aa;

(******************************************************************************)

function BoolToStr ( b : boolean ) : string;
begin
  if b then
    Result := 'yes'
  else
    Result := 'no'
end;

function BrakeSensorTypeToStr ( SensorType : byte ) : string;
begin
  case SensorType of
    0 : Result := 'NO (normal open)';
    1 : Result := 'NC (normal closed)';
    2 : Result := 'analog 0..5V';
    3 : Result := 'analog 5..0V';
    else
        Result := 'unknown';
  end;
end;

(******************************************************************************)

function GetNodeName(id : byte) : string;
begin
  case id of
    ID_CONSOLE :
      Result := 'console';
    ID_BATTERY :
      Result := 'battery';
    ID_MOTOR :
      Result := 'motor';
//    BIB :
//      Result := 'bib';
    else
      Result := 'UNKNOWN';
  end;
end;

(******************************************************************************)

constructor TBionXComponent.Create ( CANAdapter : PCANAdapter; CANId : byte );
begin
  inherited Create;
  FCANId := CANId;
  FCANAdapter := CANAdapter;
end;

function TBionXComponent.GetByteValue ( Reg : byte ) : byte;
begin
  Result := FCANAdapter^.ReadByte ( FCANId, Reg );
end;

procedure TBionXComponent.SetByteValue ( Reg : byte; Value : byte );
begin
  FCANAdapter^.WriteByte ( FCANId, Reg, Value );
end;

function TBionXComponent.GetBoolValue ( Reg : byte ) : boolean;
begin
  Result := GetByteValue ( Reg ) <> 0;
end;

procedure TBionXComponent.SetBoolValue ( Reg : byte; Value : boolean );
begin
  if Value then
    SetByteValue ( Reg, 1 )
  else
    SetByteValue ( Reg, 0 );
end;

function TBionXComponent.GetWordValue ( RegHIGH : byte ) : word;
begin
  Result :=  ( GetByteValue( RegHIGH ) shl 8 )
            +  GetByteValue( RegHIGH+1 );
end;

procedure TBionXComponent.SetWordValue ( RegHIGH : byte; Value : word );
begin
  SetByteValue( RegHIGH, Value shr 8 );
  SetByteValue( RegHIGH+1, Value and $ff );
end;

function TBionXComponent.GetDWordValue ( RegHIGH : byte ) : dword;
begin
  Result :=   ( GetByteValue( RegHIGH ) shl 24 )
            + ( GetByteValue( RegHIGH+1 ) shl 16 )
            + ( GetByteValue( RegHIGH+2 ) shl  8 )
            +   GetByteValue( RegHIGH+3 );
end;

procedure TBionXComponent.SetDWordValue ( RegHIGH : byte; Value : dword );
begin
  // !!! very strange !!!
  // the console reverses the bytes somehow
  SetByteValue( RegHIGH+3, Value shr 24 );
  SetByteValue( RegHIGH+2, Value shr 16 );
  SetByteValue( RegHIGH+1, Value shr  8 );
  SetByteValue( RegHIGH+0, Value and $ff );
end;

(******************************************************************************)
(*++++++++++
function TBionXConsole.SetToSlaveMode : boolean;
var
  Retries : integer;
begin
  Retries := 5;
  SlaveMode := true;
  repeat
    dec ( Retries );
    sleep ( 200 );
    try
      Result := SlaveMode;
    except
      if Retries = 0 then
        raise;
    end;
  until Result or ( Retries = 0 )
end;
++++++++++*)
function TBionXConsole.SetToSlaveMode : boolean;
var
  Retries : integer;
begin
  Retries := 5;
  repeat
    try
      SlaveMode := true;
      dec ( Retries );
      sleep ( 200 );
      Result := SlaveMode;
    except
      if Retries = 0 then
        raise;
    end;
  until Result or ( Retries = 0 )
end;

function TBionXConsole.GetHardwareVersion: byte;
begin
  Result := GetByteValue( REG_CONSOLE_REF_HW );
end;

function TBionXConsole.GetSoftwareVersion: byte;
begin
  Result := GetByteValue( REG_CONSOLE_REF_SW );
end;

function TBionXConsole.GetPartNumber : word;
begin
  Result := GetWordValue( REG_CONSOLE_SN_PN_HI );
end;

function TBionXConsole.GetItemNumber : word;
begin
  Result := GetWordValue( REG_CONSOLE_SN_ITEM_HI );
end;

function TBionXConsole.GetProductionDate : TDate;
begin
  Result := EncodeDate ( GetByteValue ( REG_CONSOLE_PRODUCTIONDATE_YEAR ) + 2000,
                         GetByteValue ( REG_CONSOLE_PRODUCTIONDATE_MONTH ),
                         GetByteValue ( REG_CONSOLE_PRODUCTIONDATE_DAY )
                       );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetOdometer: double;
begin
  Result := GetDWordValue ( REG_CONSOLE_STATUS_ODO_HI ) * DISTANCE_FACTOR;
end;

procedure TBionXConsole.SetOdometer ( AValue : double ) ;
begin
  SetDWordValue ( REG_CONSOLE_STATUS_ODO_HI, round ( AValue / DISTANCE_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetDistance: double;
begin
  Result := GetWordValue ( REG_CONSOLE_STATUS_DIST_HI ) * DISTANCE_FACTOR;
end;

procedure TBionXConsole.SetDistance ( AValue : double ) ;
begin
  SetWordValue ( REG_CONSOLE_STATUS_DIST_HI, round ( AValue / DISTANCE_FACTOR ) );;
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAverageSpeed: double;
begin
  Result := GetWordValue ( REG_CONSOLE_STATUS_AVGSPEED_HI ) * SPEED_FACTOR;
end;

procedure TBionXConsole.SetAverageSpeed ( AValue : double ) ;
begin
  SetWordValue ( REG_CONSOLE_STATUS_AVGSPEED_HI, round ( AValue / SPEED_FACTOR ) );;
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetChrono: TTime;
begin
  Result := EncodeTime ( GetByteValue ( REG_CONSOLE_CHRONO_HOUR ),
                         GetByteValue ( REG_CONSOLE_CHRONO_MINUTE ),
                         GetByteValue ( REG_CONSOLE_CHRONO_SECOND ),
                         0
                       );
end;

procedure TBionXConsole.SetChrono ( AValue : TTime ) ;
var
  Hour        : word;
  Minute      : word;
  Second      : word;
  MilliSecond : word;
begin
  DecodeTime( AValue, Hour, Minute, Second, MilliSecond );
  SetByteValue ( REG_CONSOLE_CHRONO_HOUR, Hour );
  SetByteValue ( REG_CONSOLE_CHRONO_MINUTE, Minute );
  SetByteValue ( REG_CONSOLE_CHRONO_SECOND, Second );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetSlaveMode: boolean;
begin
  Result := GetBoolValue( REG_CONSOLE_STATUS_SLAVE );
end;

procedure TBionXConsole.SetSlaveMode ( AValue: boolean ) ;
begin
  SetBoolValue( REG_CONSOLE_STATUS_SLAVE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetWheelCircumference: word;
begin
  Result := GetWordValue( REG_CONSOLE_GEOMETRY_CIRC_HI );
end;

procedure TBionXConsole.SetWheelCircumference ( AValue: word ) ;
begin
  SetWordValue( REG_CONSOLE_GEOMETRY_CIRC_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetFlippedSwitches: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_KEYFLIP_FLAG );
end;

procedure TBionXConsole.SetFlippedSwitches ( AValue: boolean ) ;
begin
  SetBoolValue ( REG_CONSOLE_KEYFLIP_FLAG, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetMetricUnits: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_METRICUNITS );
end;

procedure TBionXConsole.SetMetricUnits ( AValue: boolean ) ;
begin
  SetBoolValue ( REG_CONSOLE_METRICUNITS, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetShowRemainigDistanceAndTime: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_SHOW_REMAINING_FLAG );
end;

procedure TBionXConsole.SetShowRemainigDistanceAndTime ( AValue : boolean ) ;
begin
  SetBoolValue ( REG_CONSOLE_SHOW_REMAINING_FLAG, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistMaxSpeedFlag: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_ASSIST_MAXSPEEDFLAG );
end;

procedure TBionXConsole.SetAssistMaxSpeedFlag(AValue: boolean);
begin
  SetBoolValue ( REG_CONSOLE_ASSIST_MAXSPEEDFLAG, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistMaxSpeed: double;
begin
  Result := GetWordValue( REG_CONSOLE_ASSIST_MAXSPEED_HI ) * SPEED_FACTOR;
end;

procedure TBionXConsole.SetAssistMaxSpeed(AValue: double);
begin
  SetWordValue( REG_CONSOLE_ASSIST_MAXSPEED_HI, round ( AValue / SPEED_FACTOR ));
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistMinSpeedFlag: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_ASSIST_MINSPEEDFLAG );
end;

procedure TBionXConsole.SetAssistMinSpeedFlag(AValue: boolean);
begin
  SetBoolValue ( REG_CONSOLE_ASSIST_MINSPEEDFLAG, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistMinSpeed: double;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_MINSPEED ) * SPEED_FACTOR;
end;

procedure TBionXConsole.SetAssistMinSpeed(AValue: double);
begin
  SetByteValue( REG_CONSOLE_ASSIST_MINSPEED, round ( AValue / SPEED_FACTOR ));
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetThrottleMaxSpeedFlag: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_THROTTLE_MAXSPEEDFLAG );
end;

procedure TBionXConsole.SetThrottleMaxSpeedFlag(AValue: boolean);
begin
  SetBoolValue ( REG_CONSOLE_THROTTLE_MAXSPEEDFLAG, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetThrottleMaxSpeed: double;
begin
  Result := GetWordValue( REG_CONSOLE_THROTTLE_MAXSPEED_HI ) * SPEED_FACTOR;
end;

procedure TBionXConsole.SetThrottleMaxSpeed(AValue: double);
begin
  SetWordValue( REG_CONSOLE_THROTTLE_MAXSPEED_HI, round ( AValue / SPEED_FACTOR ));
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetInitialAssistLevel: byte;
begin
  Result := GetByteValue ( REG_CONSOLE_ASSIST_INITLEVEL );
end;

procedure TBionXConsole.SetInitialAssistLevel ( AValue: byte ) ;
begin
  SetByteValue ( REG_CONSOLE_ASSIST_INITLEVEL, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevel1: double;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_LEVEL_1 ) * ASSIST_LEVEL_FAKTOR;
end;

procedure TBionXConsole.SetAssistLevel1 ( AValue: double ) ;
begin
  SetByteValue( REG_CONSOLE_ASSIST_LEVEL_1, round(AValue / ASSIST_LEVEL_FAKTOR) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevel2: double;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_LEVEL_2 ) * ASSIST_LEVEL_FAKTOR;
end;

procedure TBionXConsole.SetAssistLevel2 ( AValue: double ) ;
begin
  SetByteValue( REG_CONSOLE_ASSIST_LEVEL_2, round(AValue / ASSIST_LEVEL_FAKTOR) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevel3: double;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_LEVEL_3 ) * ASSIST_LEVEL_FAKTOR;
end;

procedure TBionXConsole.SetAssistLevel3 ( AValue: double ) ;
begin
  SetByteValue( REG_CONSOLE_ASSIST_LEVEL_3, round(AValue / ASSIST_LEVEL_FAKTOR) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevel4: double;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_LEVEL_4 ) * ASSIST_LEVEL_FAKTOR;
end;

procedure TBionXConsole.SetAssistLevel4 ( AValue: double ) ;
begin
  SetByteValue( REG_CONSOLE_ASSIST_LEVEL_4, round(AValue / ASSIST_LEVEL_FAKTOR) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetMountainAssistLevel: double;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_LEVEL_MOUNTAIN ) * ASSIST_LEVEL_FAKTOR;
end;

procedure TBionXConsole.SetMountainAssistLevel ( AValue: double ) ;
begin
  SetByteValue( REG_CONSOLE_ASSIST_LEVEL_MOUNTAIN, round(AValue / ASSIST_LEVEL_FAKTOR) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetTorqueSensorGain: double;
begin
  Result := GetByteValue( REG_CONSOLE_TORQUESENSOR_GAIN ) * SENESORGAIN_FACTOR;
end;

procedure TBionXConsole.SetTorqueSensorGain ( AValue: double ) ;
begin
  SetByteValue( REG_CONSOLE_TORQUESENSOR_GAIN, trunc ( AValue / SENESORGAIN_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetTorqueSensorSpeed: byte;
begin
  Result := GetByteValue( REG_CONSOLE_TORQUESENSOR_SPEED );
end;

procedure TBionXConsole.SetTorqueSensorSpeed ( AValue: byte ) ;
begin
  SetByteValue( REG_CONSOLE_TORQUESENSOR_SPEED, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetTorqueSensorExtraGain: double;
begin
  Result := GetByteValue( REG_CONSOLE_TORQUESENSOR_EXTRAGAIN ) * SENESORGAIN_FACTOR;
end;

procedure TBionXConsole.SetTorqueSensorExtraGain ( AValue: double ) ;
begin
  SetByteValue( REG_CONSOLE_TORQUESENSOR_EXTRAGAIN, round( AValue / SENESORGAIN_FACTOR ));
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetTorqueSensorExtraGainMaxSpeed: double;
begin
  Result := GetByteValue( REG_CONSOLE_TORQUESENSOR_EXTRAGAIN_MAXSPEED ) * SPEED_FACTOR;
end;

procedure TBionXConsole.SetTorqueSensorExtraGainMaxSpeed ( AValue: double ) ;
begin
  SetByteValue( REG_CONSOLE_TORQUESENSOR_EXTRAGAIN_MAXSPEED, round ( AValue / SPEED_FACTOR ));
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetBoostLevel : byte;
begin
  Result := GetByteValue( REG_CONSOLE_BOOST_LEVEL );
end;

procedure TBionXConsole.SetBoostLevel ( AValue : byte ) ;
begin
  SetByteValue( REG_CONSOLE_BOOST_LEVEL, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetBrakeSensorFlag: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_BRAKESENSOR_FLAG );
end;

procedure TBionXConsole.SetBrakeSensorFlag ( AValue: boolean ) ;
begin
  SetBoolValue ( REG_CONSOLE_BRAKESENSOR_FLAG, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetBrakeSensorType: byte;
begin
  Result := GetByteValue ( REG_CONSOLE_BRAKESENSOR_TYPE );
end;

procedure TBionXConsole.SetBrakeSensorType ( AValue: byte ) ;
begin
  SetByteValue ( REG_CONSOLE_BRAKESENSOR_TYPE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetBrakeRekuperationLevel: byte;
begin
  Result := GetByteValue( REG_CONSOLE_BRAKE_REKU_LEVEL );
end;

procedure TBionXConsole.SetBrakeRekuperationLevel ( AValue: byte ) ;
begin
  SetByteValue( REG_CONSOLE_BRAKE_REKU_LEVEL, AValue );
end;


(******************************************************************************)

{ TBionXMotor }

procedure TBionXMotor.UnlockProtection;
begin
  SetByteValue( REG_MOTOR_PROTECT_UNLOCK, MOTOR_PROTECT_UNLOCK_KEY );
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetHardwareVersion: byte;
begin
  Result := GetByteValue( REG_MOTOR_REF_HW );
end;

function TBionXMotor.GetSoftwareVersion: byte;
begin
  Result := GetByteValue( REG_MOTOR_REF_SW );
end;

function TBionXMotor.GetPartNumber: word;
begin
  Result := GetWordValue( REG_MOTOR_SN_PN_HI );
end;

function TBionXMotor.GetItemNumber: word;
begin
  Result := GetWordValue ( REG_MOTOR_SN_ITEM_HI );
end;

function TBionXMotor.GetProductionDate : TDate;
begin
  Result := EncodeDate ( GetByteValue ( REG_MOTOR_PRODUCTIONDATE_YEAR ) + 2000,
                         GetByteValue ( REG_MOTOR_PRODUCTIONDATE_MONTH ),
                         GetByteValue ( REG_MOTOR_PRODUCTIONDATE_DAY )
                       );
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetMaxSpeed: double;
begin
  Result := GetByteValue ( REG_MOTOR_ASSIST_MAXSPEED );
end;

procedure TBionXMotor.SetMaxSpeed(AValue: double);
begin
  UnlockProtection;
  SetByteValue ( REG_MOTOR_ASSIST_MAXSPEED, round(AValue) );
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetWheelCircumference: word;
begin
  Result := GetWordValue ( REG_MOTOR_CIRC_HI );
end;

procedure TBionXMotor.SetWheelCircumference(AValue: word);
begin
  UnlockProtection;
  SetWordValue ( REG_MOTOR_CIRC_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTemperature: byte;
begin
  Result := 0; //GetByteValue ( REG_MOTOR_TEMERATURE );
end;

(******************************************************************************)

{ TBionXBattery }

function TBionXBattery.GetHardwareVersion: byte;
begin
  Result := GetByteValue( REG_BATTERY_REF_HW );
end;

function TBionXBattery.GetSoftwareVersion: byte;
begin
  Result := GetByteValue( REG_BATTERY_REF_SW );
end;

function TBionXBattery.GetPartNumber: word;
begin
  Result := GetWordValue( REG_BATTERY_SN_PN_HI );
end;

function TBionXBattery.GetItemNumber: word;
begin
  Result := GetWordValue( REG_BATTERY_SN_ITEM_HI );
end;

function TBionXBattery.GetProductionDate : TDate;
begin
  Result := EncodeDate ( GetByteValue ( REG_BATTERY_PRODUCTIONDATE_YEAR ) + 2000,
                         GetByteValue ( REG_BATTERY_PRODUCTIONDATE_MONTH ),
                         GetByteValue ( REG_BATTERY_PRODUCTIONDATE_DAY )
                       );
end;

(*----------------------------------------------------------------------------*)

procedure TBionXBattery.Shutdown;
begin
  SetBoolValue ( REG_BATTERY_CONFIG_SHUTDOWN, true );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetChargeLevel: double;
begin
  Result := GetByteValue ( REG_BATTERY_STATUS_CHARGELEVEL ) * 6.6667;
end;

function TBionXBattery.GetVoltage: double;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_VOLTAGE_HI ) / 1000.0;
end;

function TBionXBattery.GetInputVoltage : double;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_INPUTVOLTAGE_HI ) / 1000.0;
end;

function TBionXBattery.GetBalancerEnabled: boolean;
begin
  Result := GetBoolValue ( REG_BATTERY_CELLMON_BALANCERENABLED );
end;

function TBionXBattery.GetPackSerial: byte;
begin
  Result := GetByteValue ( REG_BATTERY_CONFIG_PACKSERIAL );
  if Result > 20 then
    Result :=0 ;
end;

function TBionXBattery.GetPackParallel: byte;
begin
  Result := GetByteValue ( REG_BATTERY_CONFIG_PACKPARALLEL );
  if Result > 20 then
    Result :=0 ;
end;

function TBionXBattery.GetChargeTimes(Level : byte): word;
begin
  // Level: 1..10 -> 10..100%
  if ( Level >= 1 ) and ( Level <= 10 ) then
  begin
    SetByteValue ( REG_BATTERY_CHARGE_TIMES_CHANNEL, Level );
    Result := GetWordValue ( REG_BATTERY_CHARGE_TIMES_DATA_HI );
  end
  else
    Result := 0;
end;

function TBionXBattery.GetCellVoltage(CellNo : byte): double;
begin
  // CellNo: 1..PackSerial
  SetByteValue ( REG_BATTERY_CELLVOLTAGE_CHANNEL, $80 + CellNo );
  Result := GetWordValue ( REG_BATTERY_CELLVOLTAGE_DATA_HI ) / 1000.0;
end;

function TBionXBattery.GetPackTemperature(PackNo : byte): byte;
begin
  // PackNo: 1..PackSerial
  Result := GetByteValue ( REG_BATTERY_STATUS_TEMPERATURE_PACK_1 + PackNo - 1 );
end;

function TBionXBattery.GetChargeCycles: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_CHARGECYCLES_HI );
end;

function TBionXBattery.GetFullChargeCycles: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_FULLCHARGECYCLES_HI );
end;

function TBionXBattery.GetPowerCycles: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_POWERCYCLES_HI );
end;

function TBionXBattery.GetCellCapacity: double;
begin
  Result := GetWordValue ( REG_BATTERY_CONFIG_CELLCAPACITY_HI ) / 1000;
end;

function TBionXBattery.GetChargeTimeMean: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATS_CHARGETIMEMEAN_HI );
end;

function TBionXBattery.GetChargeTimeWorst: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATS_CHARGETIMEWORST_HI );
end;

function TBionXBattery.GetResets: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_RESETS_HI );
end;

function TBionXBattery.GetLMD: double;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_LMD_HI ) * 0.002142;
end;

function TBionXBattery.GetVCtrlShorts: byte;
begin
  Result := GetByteValue ( REG_BATTERY_STATUS_VCTRLSHORTS );
end;

function TBionXBattery.GetVoltageMax: double;
begin
  Result := ( GetByteValue ( REG_BATTERY_STATUS_VOLTAGE_MAX ) + BATTERY_VOLTAGE_OFFSET ) * BATTERY_VOLTAGE_FAKTOR;
end;

function TBionXBattery.GetVoltageMin: double;
begin
  Result := ( GetByteValue ( REG_BATTERY_STATUS_VOLTAGE_MIN ) + BATTERY_VOLTAGE_OFFSET ) * BATTERY_VOLTAGE_FAKTOR;
end;

function TBionXBattery.GetVoltageMean: double;
begin
  Result := ( GetByteValue ( REG_BATTERY_STATUS_VOLTAGE_MEAN ) + BATTERY_VOLTAGE_OFFSET ) * BATTERY_VOLTAGE_FAKTOR;
end;

function TBionXBattery.GetTemperatureMax: byte;
begin
  Result := GetByteValue ( REG_BATTERY_STATUS_TEMPERATURE_MAX );
end;

function TBionXBattery.GetTemperatureMin: byte;
begin
  Result := GetByteValue ( REG_BATTERY_STATUS_TEMPERATURE_MIN );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetAutoShutdownDelay: word;
begin
  Result := GetWordValue ( REG_BATTERY_CONFIG_AUTOSHUTDOWNDELAY_HI );
end;

procedure TBionXBattery.SetAutoShutdownDelay ( AValue: word ) ;
begin
  SetWordValue ( REG_BATTERY_CONFIG_AUTOSHUTDOWNDELAY_HI, AValue );
end;

(******************************************************************************)

constructor TBionXBike.Create;
begin
  inherited Create;

  // create the BionXComponents with a pointer to bike's FCANAdapter,
  // which will be set and valid after successfull connect
  FConsole := TBionXConsole.Create ( @FCANAdapter, ID_CONSOLE );
  FMotor   := TBionXMotor.Create ( @FCANAdapter, ID_MOTOR );
  FBattery := TBionxBattery.Create ( @FCANAdapter, ID_BATTERY );
end;

destructor TBionXBike.Destroy;
begin
  Disconnect;
  FConsole.Free;
  FMotor.Free;
  FBattery.Free;
  inherited;
end;

function TBionXBike.Connect ( CANAdapter : TCANAdapter ) : boolean;
begin
  Result := false;
  FCANAdapter := CANAdapter;
  try
    if FCANAdapter.Connect then
    try
      Result := Console.SetToSlaveMode;
    except
      on E:Exception do
        raise Exception.Create ( 'Error setting console to slave mode'#13+E.Message );
    end;
  except
    on E:Exception do
    begin
      Disconnect; // free the FCANAdapter
      raise Exception.Create ( 'Error connecting to CAN'#13+E.Message );
    end;
  end;
end;

procedure TBionXBike.Disconnect;
begin
  if assigned ( FCANAdapter ) then
  begin
    FCANAdapter.Disconnect;
    FCANAdapter.Free;
    FCANAdapter := nil;
  end;
end;

procedure TBionXBike.SetWheelCircumference( Circumference : word ) ;
begin
  Console.WheelCircumference := Circumference;
  Motor.WheelCircumference := Circumference;
end;

procedure TBionXBike.SetConsoleSwitchFlipped ( Flipped: boolean );
begin
  Console.FlippedSwitches := Flipped;
end;

procedure TBionXBike.SetMetricUnits ( MetricUnits : boolean );
begin
  Console.MetricUnits  := MetricUnits;
end;

procedure TBionXBike.SetAutoShutdownDelay ( delay : word );
begin
  // be aware to set the delay too short, as the sytem may
  // shutdown, before you are able to reincrease the value
  if delay >= 300 then
    Battery.AutoShutdownDelay := delay;
end;

procedure TBionXBike.SetShowRemainingDistanceAndTime ( Show : boolean );
begin
  Console.ShowRemainigDistanceAndTime := Show;
end;


procedure TBionXBike.SetAssistMaxSpeed ( Speed : double );
var
  limit : boolean;
begin
  limit := speed<>0;
  if not limit then
    speed := UNLIMITED_SPEED_VALUE;
  Console.AssistMaxSpeedFlag := limit;
  Console.AssistMaxSpeed := Speed;
  Motor.MaxSpeed := Speed;
end;

procedure TBionXBike.SetAssistMinSpeed( Speed : double );
begin
  Console.AssistMinSpeedFlag := ( Speed <> 0 );
  Console.AssistMinSpeed := Speed;
end;

procedure TBionXBike.SetThrottleMaxSpeed ( Speed : double );
var
  limit : boolean;
begin
  limit := speed<>0;
  if not limit then
    speed := UNLIMITED_THROTTLE_SPEED_VALUE;
  Console.ThrottleMaxSpeedFlag := limit;
  Console.ThrottleMaxSpeed := Speed;
end;

procedure TBionXBike.SetTorqueSensorGain ( Gain: double );
begin
  if ( Gain > 0 ) and ( Gain <= 4 ) then
    Console.TorqueSensorGain := Gain;
end;

procedure TBionXBike.SetTorqueSensorSpeed(Speed: byte);
begin
  if ( Speed > 0 ) and ( Speed < 5 ) then
    Console.TorqueSensorSpeed := Speed;
end;

procedure TBionXBike.SetTorqueSensorExtraGain(Gain: double);
begin
  Console.TorqueSensorExtraGain := Gain;
end;

procedure TBionXBike.SetTorqueSensorExtraGainMaxSpeed(Speed: double);
begin
  Console.TorqueSensorExtraGainMaxSpeed := Speed;
end;

procedure TBionXBike.SetBoostLevel ( BoostLevel : byte );
begin
  if ( BoostLevel >= 7 ) and ( BoostLevel <= 16 ) then
    Console.BoostLevel := BoostLevel;
end;

procedure TBionXBike.SetBrakeSensor(SensorEnabled: boolean; SensorType: byte);
begin
  Console.BrakeSensorFlag := SensorEnabled;
  if ( SensorType <= 4 ) then
    Console.BrakeSensorType := SensorType;
end;

procedure TBionXBike.SetBrakeRekuperationLevel(Level: byte);
begin
  if ( Level >= 5 ) and ( Level <= 40 ) then
    Console.BrakeRekuperationLevel := Level;
end;

procedure TBionXBike.SetAssistInitialLevel(Level: byte);
begin
  Console.InitialAssistLevel := Level;
end;

procedure TBionXBike.SetAssistLevel1(Level: double);
begin
  Console.AssistLevel1 := Level;
end;

procedure TBionXBike.SetAssistLevel2(Level: double);
begin
  Console.AssistLevel2 := Level;
end;

procedure TBionXBike.SetAssistLevel3(Level: double);
begin
  Console.AssistLevel3 := Level;
end;

procedure TBionXBike.SetAssistLevel4(Level: double);
begin
  Console.AssistLevel4 := Level;
end;

procedure TBionXBike.SetAssistLevelMountain(Level: double);
begin
  Console.MountainAssistLevel := Level;
end;

procedure TBionXBike.SetOdometer ( Odometer : double );
begin
  if Odometer >= 0 then
    Console.Odometer := Odometer;
end;

procedure TBionXBike.SetDistance ( Distance : double );
begin
  // distance value is limited by max word value = 65535
  if ( Distance >= 0 ) and ( Distance < 6553.5 ) then
    Console.Distance := Distance;
end;

procedure TBionXBike.SetAverageSpeed ( AvgSpeed : double );
begin
  // avg. speed value is limited by max word value = 65535
  if ( AvgSpeed >= 0 ) and ( AvgSpeed < 6553.5 ) then
    Console.AverageSpeed := AvgSpeed;
end;

procedure TBionXBike.SetChrono ( Chrono : TTime );
begin
  Console.Chrono := Chrono;
end;

end.

