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
{                                                                        }
{                                                                        }
{ !!!!! IMPORTANT NOTE !!!!!                                             }
{                                                                        }
{ all known registers do have a get method and I have also added most    }
{ of the corresponding set methods, at least with an empty body.         }
{ This makes it easy to add more needed register writings, just add some }
{ lines of code in the body.                                             }
{ The set methods are partly untested, marked with {*} at the property   }
{ declaration. Some of them do have already a complete body, some have   }
{ an empty body. They will throw an exception when called, so you know   }
{ they are untested and must be validated.                               }
{ For easy coresponding to the registernames in the bionx.xml, I've      }
{ named the properties mostly like the registers. As that names are      }
{ sometimes not very meaningfull, there do partly exist additional       }
{ properties with "nice" names, marked with an {N}.                      }
{                                                                        }
{ As it might be dangerous to write to some registers, be carefull when  }
{ implementig the set methods. I.e. the battery has a property           }
{ "PermanentFailureFlag". Do not set this prop, unless you know whether  }
{ and how to reset this flag.                                            }
{ Other props are probably useless to set although the registers are not }
{ marked as read only in the bionx.xml. Decide yourself, what you need.  }

unit BionX;
{$HINTS OFF}

{$mode objfpc}{$H+}

{$define adapter}

interface
uses
  SysUtils,
dialogs, controls, // used by untested only
  Classes,
//  CANAdapter,
  CANInterface;

type
  { TBionXComponent }
  TBionXComponent = class ( TObject )
  private
    FCANId           : byte;
    FCANIntf         : PCANInterface;

    // cache soft- and hardware versions
    FHardwareVersion : byte;
    FSoftwareVersion : byte;
    FSubVersion      : byte;

    // data access helpers
    function GetByteValue ( Reg : byte ) : byte;
    procedure SetByteValue ( Reg : byte; Value : byte );

    function GetBoolValue ( Reg : byte ) : boolean;
    procedure SetBoolValue ( Reg : byte; Value : boolean );

    function  GetWordValue ( RegHIGH : byte ) : word;
    procedure SetWordValue ( RegHIGH : byte; Value : word );

    function GetDWordValue ( RegHIGH : byte ) : dword;
    procedure SetDWordValue ( RegHIGH : byte; Value : dword );
    function GetDWordValueR ( RegHIGH : byte ) : dword;
    procedure SetDWordValueR ( RegHIGH : byte; Value : dword );

    // common property gets
    function GetHardwareVersion: byte; virtual; abstract;
    function GetSoftwareVersion: byte; virtual; abstract;
    function GetSubVersion: byte; virtual; abstract;
  protected
    function CheckVersion ( sw_since, sw_until, sub_since, sub_until, hw_since, hw_until : byte ) : boolean;
  public
//    constructor Create ( CANAdapter : PCANAdapter; ACANId : byte );
    constructor Create ( CANIntf : PCANInterface; ACANId : byte );

    procedure CheckDeviceReady;

    // common properties
    property CANId : byte read FCANId write FCANId;
    property HardwareVersion : byte read GetHardwareVersion;
    property SoftwareVersion : byte read GetSoftwareVersion;
    property SubVersion : byte read GetSubVersion;

    // for evaluation purpose only
    property ByteValue [ Reg : byte ] : byte read GetByteValue write SetByteValue;
    property WordValue [ Reg : byte ] : word read GetWordValue write SetWordValue;
  end;

  { TBionXConsole }

  TBionXConsole = class ( TBionXComponent )
  private
    // console.rev
    function GetSoftwareVersion: byte; override;
    function GetSubVersion: byte; override;
    function GetHardwareVersion: byte; override;

    // console.sn
    function GetPartNumber : word;
    function GetLocation : byte;
    function GetManufacturingDate : TDate;
    function GetItemNumber : word;
    function GetOEM : word;
    function GetProduct : word;
    function GetConsoleType : byte;

    // console.geometry
    function GetGeometryCirc: word;
    procedure SetGeometryCirc ( AValue: word ) ;

    // console.config
    function GetConfigTestMode: boolean;
    procedure SetConfigTestMode ( AValue : boolean ) ;
    function GetConfigServiceTimeStamp: word;
    procedure SetConfigServiceTimeStamp ( AValue : word ) ;
    function GetConfigServiceDistance: word;
    procedure SetConfigServiceDistance ( AValue : word ) ;
    function GetConfigLastMode: boolean;
    procedure SetConfigLastMode ( AValue: boolean ) ;

    // console.status
    function GetStatusSlave: boolean;
    procedure SetStatusSlave ( AValue: boolean ) ;

    // console.assist
    function GetAssistMaxSpeedFlag: boolean;
    procedure SetAssistMaxSpeedFlag(AValue: boolean);
    function GetAssistMaxSpeed: double;
    procedure SetAssistMaxSpeed(AValue: double);
    function GetAssistMinSpeedFlag: boolean;
    procedure SetAssistMinSpeedFlag(AValue: boolean);
    function GetAssistMinSpeed: double;
    procedure SetAssistMinSpeed(AValue: double);
    function GetAssistBrakeLevel: double;
    procedure SetAssistBrakeLevel ( AValue: double ) ;
    function GetAssistBrakeFlag: boolean;
    procedure SetAssistBrakeFlag ( AValue: boolean ) ;
    function GetAssistAutoRegen: boolean;
    procedure SetAssistAutoRegen ( AValue : boolean ) ;
    function GetAssistBrakePolarity: byte;
    procedure SetAssistBrakePolarity ( AValue: byte ) ;
    function GetAssistGaugeFilter: byte;
    procedure SetAssistGaugeFilter ( AValue: byte ) ;
    function GetAssistGaugeGain: double;
    procedure SetAssistGaugeGain ( AValue: double ) ;
    function GetAssistGaugeGainA: double;
    procedure SetAssistGaugeGainA ( AValue: double ) ;
    function GetAssistGaugeGainB: double;
    procedure SetAssistGaugeGainB ( AValue: double ) ;
    function GetAssistSpeedGain: double;
    procedure SetAssistSpeedGain ( AValue: double ) ;
    function GetAssistGaugeJoint: byte;
    procedure SetAssistGaugeJoint ( AValue: byte ) ;
    function GetAssistLevel1: double;
    procedure SetAssistLevel1 ( AValue: double ) ;
    function GetAssistLevel2: double;
    procedure SetAssistLevel2 ( AValue: double ) ;
    function GetAssistLevel3: double;
    procedure SetAssistLevel3 ( AValue: double ) ;
    function GetAssistLevel4: double;
    procedure SetAssistLevel4 ( AValue: double ) ;
    function GetAssistLevelR1: double;
    procedure SetAssistLevelR1 ( AValue: double ) ;
    function GetAssistLevelR2: double;
    procedure SetAssistLevelR2 ( AValue: double ) ;
    function GetAssistLevelR3: double;
    procedure SetAssistLevelR3 ( AValue: double ) ;
    function GetAssistLevelR4: double;
    procedure SetAssistLevelR4 ( AValue: double ) ;
    function GetAssistInitLevel: byte;
    procedure SetAssistInitLevel ( AValue: byte ) ;
    function GetAssistMountainCap: double;
    procedure SetAssistMountainCap ( AValue: double ) ;

    // console.throttle
    function GetThrottleMaxSpeedFlag: boolean;
    procedure SetThrottleMaxSpeedFlag(AValue: boolean);
    function GetThrottleMaxSpeed: double;
    procedure SetThrottleMaxSpeed(AValue: double);
    function GetThrottleEnabledOnStrain: boolean;
    procedure SetThrottleEnabledOnStrain ( AValue: boolean ) ;
    function GetThrottleEnableBoostDisplay: boolean;
    procedure SetThrottleEnableBoostDisplay ( AValue: boolean ) ;
    function GetThrottleBoostTrigerLevel: double;
    procedure SetThrottleBoostTrigerLevel ( AValue: double ) ;
    function GetThrottleCalibrated: boolean;
    procedure SetThrottleCalibrated ( AValue : boolean ) ;
    function GetThrottlePosition: double;
    procedure SetThrottlePosition ( AValue : double ) ;
    function GetThrottleRaw: word;
    procedure SetThrottleRaw ( AValue : word ) ;
    function GetThrottleMin: word;
    procedure SetThrottleMin ( AValue : word ) ;
    function GetThrottleMax: word;
    procedure SetThrottleMax ( AValue : word ) ;

    // console.preference
    function GetPreferenceTripToEmptyFlag: boolean;
    procedure SetPreferenceTripToEmptyFlag ( AValue : boolean ) ;
    function GetPreferenceDisplayUnits: byte;
    procedure SetPreferenceDisplayUnits ( AValue: byte ) ;
    function GetPreferenceNIP: string;
    procedure SetPreferenceNIP ( const AValue : string ) ;
    function GetPreferenceLCDContrast: byte;
    procedure SetPreferenceLCDContrast ( AValue: byte ) ;
    function GetPreferenceCodes: dword;
    procedure SetPreferenceCodes ( AValue : dword ) ;
    function GetPreferenceCodesRW: dword;
    procedure SetPreferenceCodesRW ( AValue : dword ) ;
    function GetPreferenceRegion: byte;
    procedure SetPreferenceRegion ( AValue : byte ) ;
    function GetPreferenceConfigBit0: byte;
    procedure SetPreferenceConfigBit0 ( AValue : byte ) ;
    function GetPreferenceFlipSide: boolean;
    procedure SetPreferenceFlipSide ( AValue: boolean ) ;
    function GetPreferenceLightButtonMode: byte;
    procedure SetPreferenceLightButtonMode ( AValue: byte ) ;
    function GetPreferenceLightsOnAtStart: boolean;
    procedure SetPreferenceLightsOnAtStart ( AValue: boolean ) ;
    function GetPreferenceExpertMode: boolean;
    procedure SetPreferenceExpertMode ( AValue : boolean );
    function GetPreferenceThrottleMode: byte;
    procedure SetPreferenceThrottleMode ( AValue : byte ) ;

    // console.stats
    function GetStatsOdo: double;
    procedure SetStatsOdo ( AValue : double ) ;
    function GetStatsChrono: LongWord;
    procedure SetStatsChrono ( AValue : LongWord ) ;
    function GetStatsTrip: double;
    function GetStatsAvgSpeed: double;

  protected

  public

    // console.rev
    property SoftwareVersion;
    property SubVersion;
    property HardwareVersion;

    // console.sn
    property PartNumber : word read GetPartNumber; // write SetPartNumber
    property Location : byte read GetLocation; // write SetLocation
    property ManufacturingDate : TDate read GetManufacturingDate; // write SetManufacturingDate
    property ItemNumber : word read GetItemNumber; // write SetItemNumber
    property OEM : word read GetOEM; // write /SetOEM
    property Product : word read GetProduct; // write SetProduct;
    property ConsoleType : byte read GetConsoleType; // write SetConsoleType

    // console.geometry
    property GeometryCirc : word read GetGeometryCirc write SetGeometryCirc;
    {N} property WheelCircumference : word read GetGeometryCirc write SetGeometryCirc;

    // console.config
    property ConfigTestMode : boolean read GetConfigTestMode write SetConfigTestMode;
    property ConfigServiceTimeStamp : word read GetConfigServiceTimeStamp write SetConfigServiceTimeStamp;
    {N} property NextServiceDay : word read GetConfigServiceTimeStamp write SetConfigServiceTimeStamp;
    property ConfigServiceDistance : word read GetConfigServiceDistance write SetConfigServiceDistance;
    {N} property NextServiceOdo : word read GetConfigServiceDistance write SetConfigServiceDistance;
    property ConfigLastMode : boolean read GetConfigLastMode write SetConfigLastMode;
    {N} property RememberDisplayMode : boolean read GetConfigLastMode write SetConfigLastMode;

    // console.status
    property StatusSlave : boolean read GetStatusSlave write SetStatusSlave;

    // console.assist
    property AssistMaxSpeedFlag : boolean read GetAssistMaxSpeedFlag write SetAssistMaxSpeedFlag;
    property AssistMaxSpeed : double read GetAssistMaxSpeed write SetAssistMaxSpeed;
    property AssistMinSpeedFlag : boolean read GetAssistMinSpeedFlag write SetAssistMinSpeedFlag;
    property AssistMinSpeed : double read GetAssistMinSpeed write SetAssistMinSpeed;
    property AssistBrakeLevel : double read GetAssistBrakeLevel write SetAssistBrakeLevel;
    {N} property BrakeRekuperationLevel : double read GetAssistBrakeLevel write SetAssistBrakeLevel;
    property AssistBrakeFlag : boolean read GetAssistBrakeFlag write SetAssistBrakeFlag;
    {N} property BrakeSensorFlag : boolean read GetAssistBrakeFlag write SetAssistBrakeFlag;
    property AssistAutoRegen : boolean read GetAssistAutoRegen write SetAssistAutoRegen;
    property AssistBrakePolarity : byte read GetAssistBrakePolarity write SetAssistBrakePolarity;
    {N} property BrakeSensorType : byte read GetAssistBrakePolarity write SetAssistBrakePolarity;
    property AssistGaugeFilter : byte read GetAssistGaugeFilter write SetAssistGaugeFilter;
    {N} property TorqueSensorSpeed : byte read GetAssistGaugeFilter write SetAssistGaugeFilter;
    property AssistGaugeGain : double read GetAssistGaugeGain write SetAssistGaugeGain;
    {N} property TorqueSensorGain : double read GetAssistGaugeGain write SetAssistGaugeGain;
    property AssistGainA : double read GetAssistGaugeGainA write SetAssistGaugeGainA;
    {N} property TorqueSensorExtraGain : double read GetAssistGaugeGainA write SetAssistGaugeGainA;
    property AssistGainB : double read GetAssistGaugeGainB write SetAssistGaugeGainB;
    {N} property TorqueSensorExtraGainMaxSpeed : double read GetAssistGaugeGainB write SetAssistGaugeGainB;
    property AssistSpeedGain : double read GetAssistSpeedGain write SetAssistSpeedGain;
    property AssistGaugeJoint : byte read GetAssistGaugeJoint write SetAssistGaugeJoint;
    property AssistLevel1 : double read GetAssistLevel1 write SetAssistLevel1;
    property AssistLevel2 : double read GetAssistLevel2 write SetAssistLevel2;
    property AssistLevel3 : double read GetAssistLevel3 write SetAssistLevel3;
    property AssistLevel4 : double read GetAssistLevel4 write SetAssistLevel4;
    property RekuperationLevel1 : double read GetAssistLevelR1 write SetAssistLevelR1;
    property RekuperationLevel2 : double read GetAssistLevelR2 write SetAssistLevelR2;
    property RekuperationLevel3 : double read GetAssistLevelR3 write SetAssistLevelR3;
    property RekuperationLevel4 : double read GetAssistLevelR4 write SetAssistLevelR4;
    property AssistInitLevel : byte read GetAssistInitLevel write SetAssistInitLevel;
    // property RegenEnabled : boolean not yet implemented by BionX
    property AssistMountainCap : double read GetAssistMountainCap write SetAssistMountainCap;
    {N} property MountainAssistLevel : double read GetAssistMountainCap write SetAssistMountainCap;

    // console.throttle
    property ThrottleMaxSpeedFlag : boolean read GetThrottleMaxSpeedFlag write SetThrottleMaxSpeedFlag;
    property ThrottleMaxSpeed : double read GetThrottleMaxSpeed write SetThrottleMaxSpeed;
    property ThrottleEnabledOnStrain : boolean read GetThrottleEnabledOnStrain write SetThrottleEnabledOnStrain;
    property ThrottleEnableBoostDisplay : boolean read GetThrottleEnableBoostDisplay write SetThrottleEnableBoostDisplay;
    property ThrottleBoostTriggerLevel : double read GetThrottleBoostTrigerLevel write SetThrottleBoostTrigerLevel;
    property ThrottleCalibrated : boolean read GetThrottleCalibrated write SetThrottleCalibrated;
    {-} property ThrottlePosition : double read GetThrottlePosition write SetThrottlePosition; // Set does nothing
    {-} property ThrottleRawPosition : word read GetThrottleRaw write SetThrottleRaw; // Set does nothing
    property ThrottleMinActorValue : word read GetThrottleMin write SetThrottleMin;
    property ThrottleMaxActorValue : word read GetThrottleMax write SetThrottleMax;

    // console.preference
    property PreferenceTripToEmptyFlag : boolean read GetPreferenceTripToEmptyFlag write SetPreferenceTripToEmptyFlag;
    property PreferenceDisplayUnits : byte read GetPreferenceDisplayUnits write SetPreferenceDisplayUnits;
    {-} property PreferenceNIP : string read GetPreferenceNIP write SetPreferenceNIP; // Set does nothing
    property PreferenceLCDContrast : byte read GetPreferenceLCDContrast write SetPreferenceLCDContrast;
    property PreferenceCodes : dword read GetPreferenceCodes write SetPreferenceCodes;
    property PreferenceCodesRW : dword read GetPreferenceCodesRW write SetPreferenceCodesRW;
    property PreferenceRegion : byte read GetPreferenceRegion write SetPreferenceRegion;
    property PreferenceConfigBit0 : byte read GetPreferenceConfigBit0 write SetPreferenceConfigBit0;
    property PreferenceFlipSide : boolean read GetPreferenceFlipSide write SetPreferenceFlipSide;
    property PreferenceLightButtonMode : byte read GetPreferenceLightButtonMode write SetPreferenceLightButtonMode;
    property PreferenceLightsOnAtStart : boolean read GetPreferenceLightsOnAtStart write SetPreferenceLightsOnAtStart;
    // Language outdated
    property PreferenceExpertMode : boolean read GetPreferenceExpertMode write SetPreferenceExpertMode;
    property PreferenceThrottleMode : byte read GetPreferenceThrottleMode write SetPreferenceThrottleMode;

    // console.stats
    property StatsOdometer : double read GetStatsOdo write SetStatsOdo;
    {-} property StatsChrono : LongWord read GetStatsChrono write SetStatsChrono; // value cannot be written
    property StatsTrip : double read GetStatsTrip; // RO
    property StatsAverageSpeed : double read GetStatsAvgSpeed; // RO
  end;

  { TBionXBattery }

  TBionXBattery = class ( TBionXComponent )
  private
    FUnlockLevel : integer;
    FManufacturingDate : TDateTime;
  private
    // battery.rev
    function GetSoftwareVersion: byte; override;
    function GetHardwareVersion: byte; override;
    function GetSubVersion: byte; override;
    function GetChargerVersion: byte;
    function GetSupervisorVersion : byte;
    function GetBOM: byte;

    // battery.sn
    function GetCellpPackItemNumber: word;
    function GetPartNumber : word;
    function GetLocation : byte;
    function GetManufacturingDate : TDate;
    function GetItemNumber : word;

    // battery.pcbsn
    function GetPCBSNPartNumber : word;
    function GetPCBSNLocation : byte;
    function GetPCBSNManufacturingDate : TDate;
    function GetPCBSNItemNumber : word;

    // battery.timer
    function GetTimerPower : word;
    procedure SetTimerPower ( AValue : word ) ;
    function GetTimerAccessory : word;
    procedure SetTimerAccessory ( AValue : word ) ;
    function GetTimerPrecharge : byte;
    procedure SetTimerPrecharge ( AValue : byte ) ;
    function GetTimerMasterShutdown: word;
    procedure SetTimerMasterShutdown ( AValue: word ) ;

    // battery.status
    function GetStatusV5V: double;
    function GetStatusV3V3: double;
    function GetStatusVPackId: double;
    function GetStatusVBOMId: double;
    function GetStatusFlags : word;
    function GetStatusTestFlags : word;
    procedure SetStatusTestFlags ( AValue : word ) ;
    function GetStatusPermanentFailureFlags : boolean;
    procedure SetStatusPermanentFailureFlags ( AValue : boolean ) ;
    function GetStatusChargerManagerStatus : byte;
    function GetStatusVBattNorm: double;
    function GetStatusVBattInternal : double;
    function GetStatusVBatt: double;
    function GetStatusVPower : double;
    function GetStatusVControl : double;
    function GetStatusVConsole : double;
    function GetStatusV12V : double;
    function GetStautsVAccessory : double;
    function GetStatusVDCIn : double;
    function GetStatusICellpack : double;
    function GetStatusLevel: double;
    function GetStatusLeds: byte;
    procedure SetStatusLeds(AValue: byte);
    function GetStatusVChannel ( CellNo : byte ) : double;
    function GetStatusVCell ( CellNo : byte ) : double;
    function GetStatusPackTemperature ( SensorNo : byte ) : shortint;
    function GetStatusCapSense : dword;
    procedure SetStatusCapSense ( AValue : dword ) ;
    function GetStatusCapSenseReference : dword;
    procedure SetStatusCapSenseReference ( AValue : dword ) ;
    function GetStatusEstimatedSOC : byte;
    function GetStatusPowerOnResetCount : byte;
    function GetStatusWatchdogResetCount : byte;

    // battery.cellmon
    function GetCellMonChannelAddress: byte;
    procedure SetCellMonChannelAddress(AValue: byte);
    function GetCellMonChannelData: word;
    procedure SetCellMonChannelData(AValue: word);
    function GetCellMonCalibrationData: word;
    procedure SetCellMonCalibrationData(AValue: word);
    function GetCellMonBalancerEnabled: boolean;
    procedure SetCellMonBalancerEnabled(AValue: boolean);

    // battery.charger
    function GetChargerCurrent: double;
    procedure SetChargerCurrent(AValue: double);
    function GetChargerCurrentCalibration: double;
    function GetChargerFinalVoltage: double;
    procedure SetChargerFinalVoltage(AValue: double);
    function GetChargerMode: byte;
    procedure SetChargerMode(AValue: byte);
    function GetChargerStatusFlags: word;
    function GetChargerVoltageCalibration: double;

    // battery.bridges
    function GetBridgeI2CAddress: word;
    procedure SetBridgeI2CAddress(AValue: word);
    function GetBridgeI2CData: byte;
    procedure SetBridgeI2CData(AValue: byte);
    function GetBridgeChargerAddress: byte;
    procedure SetBridgeChargerAddress(AValue: byte);
    function GetBridgeChargerData: byte;
    procedure SetBridgeChargerData(AValue: byte);

    // battery.calib
    function GetCalibCapsense : double;
    procedure SetCalibCapsense(AValue: double);
    function GetCalibCalibration ( CellNo : byte ) : double;
    procedure SetCellCalibration(CellNo : byte; AValue: double);
    function GetCalibCalibration3V3 : double;
    procedure SetCalibrationValue3V3(AValue: double);

    // battery.stats
    function GetStats5VShorts : word;
    procedure SetStats5VShorts ( AValue : word ) ;
    function GetStatsVControlShorts : word;
    procedure SetStatsVControlShorts ( AValue : word ) ;
    function GetStatsLowBattBuzzCount : word;
    procedure SetStatsLowBattBuzzCount ( AValue : word ) ;
    function GetStatsCellVoltageCollapseCount : word;
    procedure SetStatsCellVoltageCollapseCount ( AValue : word ) ;
    function GetStatsCellPartialShortCount : word;
    procedure SetStatsCellPartialShortCount ( AValue : word ) ;
    function GetStatsCellDeadShortCount : word;
    procedure SetStatsCellDeadShortCount ( AValue : word ) ;
    function GetStatsDeepSleepAfterLongInactivityPeriodCount : word;
    procedure SetStatsDeepSleepAfterLongInactivityPeriodCount ( AValue : word ) ;
    function GetStatsDeepSleepAfterLowSOCCount : word;
    procedure SetStatsDeepSleepAfterLowSOCCount ( AValue : word ) ;
    function GetStatsDeepSleepExtremeLowBatteryVoltageCount : word;
    procedure SetStatsDeepSleepExtremeLowBatteryVoltageCount ( AValue : word ) ;
    function GetStatsDischargeEnergy : DWord;
    procedure SetStatsDischargeEnergy ( AValue : DWord ) ;
    function GetStatsChargeEnergy : DWord;
    procedure SetStatsChargeEnergy ( AValue : DWord ) ;
    function GetStatsLMD: double;
    function GetStatsReset: word;
    procedure SetStatsReset ( AValue : word ) ;
    function GetStatsGGJrCalib : byte;
    procedure SetStatsGGJrCalib ( AValue : byte ) ;
    function GetStatsResetWdt: byte;
    procedure SetStatsResetWdt(AValue: byte);
    function GetStatsRTCResync : byte;
    procedure SetStatsRTCResync ( AValue : byte ) ;
    function GetStatsChargeTimeWorst: word;
    procedure SetStatsChargeTimeWorst ( AValue : word ) ;
    function GetStatsChargeTimeMean: word;
    procedure SetStatsChargeTimeMean ( AValue : word ) ;
    function GetStatsLMDAdapt : byte;
    procedure SetStatsLMDAdapt ( AValue : byte ) ;
    function GetStatsBattCycles: word;
    procedure SetStatsBattCycles ( AValue : word ) ;
    function GetStatsBattFullCycles: word;
    procedure SetStatsBattFullCycles ( AValue : word ) ;
    function GetStatsPowerCycles: word;
    procedure SetStatsPowerCycles ( AValue : word ) ;
    function GetStatsVBattMax: double;
    procedure SetVBattMax ( AValue : double ) ;
    function GetVBattMin: double;
    procedure SetVBattMin ( AValue : double ) ;
    function GetVBattMean: double;
    function GetStatsTBattMax: shortint;
    procedure SetStatsTBattMax ( AValue : shortint ) ;
    function GetStatsTBattMin: shortint;
    procedure SetStatsTBattMin ( AValue : shortint ) ;
    function GetStatsCharge ( Level : byte ) : word;
    procedure SetStatsCharge ( Level : byte; AValue : word ) ;

    // battery.rtc
    function GetRTCTime : dword;
    procedure SetRTCTime ( AValue : dword ) ;
    function GetRTCTimeDT: TDateTime;
    procedure SetRTCTimeDT(AValue: TDateTime);
    function GetRTCLastChargeTimestamp : dword;
    procedure SetRTCLastChargeTimestamp ( AValue : dword ) ;
    function GetRTCLastChargeTimestampDT: TDateTime;
    procedure SetRTCLastChargeTimestampDT(AValue: TDateTime);
    function GetRTCLastValidTimestamp : dword;
    function GetRTCLastValidTimestampDT: TDateTime;
    function GetRTCCtrl : byte;
    procedure SetRTCCtrl ( AValue : byte ) ;
    function GetRTCStatus : byte;

    // battery.gg
    function GetGasGageAI : double;
    function GetGasGageVoltage : double;
    function GetGasGageSOC : byte;
    function GetGasGageLMD : double;
    function GetGasGageStatusFlags : byte;
    function GetGasGageTemperature : double;
    function GetGasgageDMFSD : byte;
    procedure SetGasgageDMFSD ( AValue : byte ) ;
    function GetGasGageVoltageDivider : double;
    procedure SetGasGageVoltageDivider ( AValue : double ) ;

    // battery.config
    function GetConfigMaxPackTemperature : shortint;
    procedure SetConfigMaxPackTemperature ( AValue : shortint ) ;
    function GetConfigMinPackTemperature : shortint;
    procedure SetConfigMinPackTemperature ( AValue : shortint ) ;
    function GetConfigMaxGGTemperature : shortint;
    procedure SetConfigMaxGGTemperature ( AValue : shortint ) ;
    function GetConfigMinGGTemperature : shortint;
    procedure SetConfigMinGGTemperature ( AValue : shortint ) ;
    function GetConfigMaxCellDeltaVoltage : double;
    procedure SetConfigMaxCellDeltaVoltage ( AValue : double ) ;
    function GetConfigTaillampIntensity : byte;
    procedure SetConfigTaillampIntensity ( AValue : byte ) ;
    function GetConfigShipmode : boolean;
    procedure SetConfigShipmode ( AValue : boolean ) ;
    function GetConfigDeepSleepAfterLongInactivityPeriodDelay : byte;
    procedure SetConfigDeepSleepAfterLongInactivityPeriodDelay ( AValue : byte ) ;
    function GetConfigDeepSleepLowSOCDelay : byte;
    procedure SetConfigDeepSleepLowSOCDelay ( AValue : byte ) ;
    function GetConfigCommMode: byte;
    procedure SetConfigCommMode ( AValue : byte ) ;
    function GetConfigAutoSwitchComm: boolean;
    procedure SetConfigAutoSwitchComm(AValue: boolean);
    function GetConfigWakeOnPowerVoltage : boolean;
    procedure SetConfigWakeOnPowerVoltage ( AValue : boolean ) ;
    function GetConfigAllowBuckChargingOnBike : boolean;
    procedure SetConfigAllowBuckChargingOnBike ( AValue: boolean ) ;
    function GetConfigDiag : byte;
    procedure SetConfigDiag ( AValue : byte ) ;
    function GetConfigType : byte;
    procedure SetConfigType ( AValue : byte ) ;
    function GetConfigVBattNominal: byte;
    procedure SetConfigVBattNominal ( AValue : byte ) ;
    function GetConfigAccessoryMounted : boolean;
    procedure SetConfigAccessoryMounted ( AValue : boolean ) ;
    function GetConfigAccessoryEnabled : boolean;
    procedure SetConfigAccessoryEnabled ( AValue : boolean ) ;
    function GetConfigAccessoryVoltage : double;
    procedure SetConfigAccessoryVoltage ( AValue : double ) ;
    function GetConfigILMD : double;
    procedure SetConfigILMD ( AValue : double ) ;
    function GetConfigMaxCharge : double;
    procedure SetConfigMaxCharge ( AValue: double ) ;
    function GetConfigMaxDischarge : double;
    procedure SetConfigMaxDischarge ( AValue: double ) ;
    function GetConfigCellCapacity: double;
    procedure SetConfigCellCapacity ( AValue: double ) ;
    function GetConfigPackSerial: byte;
    procedure SetConfigPackSerial ( AValue : byte ) ;
    function GetConfigPackParallel: byte;
    procedure SetConfigPackParallel ( AValue: byte ) ;
    function GetConfigNAC : word;
    procedure SetConfigNAC ( AValue : word ) ;
    function GetConfigForceDone: boolean;
    procedure SetConfigForceDone ( AValue: boolean ) ;
    function GetConfigShutdown: boolean;
    procedure SetConfigShutdown ( AValue: boolean ) ;
    function GetConfigVPower : boolean;
    procedure SetConfigVPower ( AValue : boolean ) ;
    function GetConfigVControl : boolean;
    procedure SetConfigVControl ( AValue : boolean ) ;
    function GetConfigVBattInt : boolean;
    procedure SetConfigVBattInt ( AValue : boolean ) ;
    function GetConfigCapSenseMode : byte;
    procedure SetConfigCapSenseMode ( AValue : byte ) ;
    function GetConfigChargerCalibrationSourceFlags : byte;
    function GetConfigChargerVoltageCalibrationValue : double;
    function GetConfigChargerCurrentCalibrationValue : double;

    // battery.protect
    function GetProtectUnlock: byte;
    procedure SetProtectUnlock ( AValue: byte ) ;
    function GetProtectMode : byte;
    procedure SetProtectMode(AValue: byte);
    function GetProtectControl : byte;
    procedure SetProtectControl(AValue: byte);

  protected
    function ReadI2CByteValue ( DevReg : word ) : byte;
    procedure WriteI2CByteValue ( DevReg : word; AValue : byte );
    function ReadI2CWordValue ( DevReg : word ) : word;
    procedure WriteI2CWordValue ( DevReg : word; AValue : word );
    function ReadI2CDWordValue ( DevReg : word ) : dword;
    procedure WriteI2CDWordValue ( DevReg : word; AValue : dword );

    function ReadCellMonVoltageValue ( Reg : byte ) : word;
//    procedure WriteCellMonVoltageValue ( Reg : byte; AValue : word );
    function ReadCellMonCalibrationValue ( Reg : byte ) : smallint;
    procedure WriteCellMonCalibrationValue ( Reg : byte; AValue : smallint );

    function ReadChargerByteValue ( Addr : byte ) : byte;
    procedure WriteChargerByteValue ( Addr : byte; AValue : byte );
    function ReadChargerWordValue ( Addr : byte ) : word;
    procedure WriteChargerWordValue ( Addr : byte; AValue : word );

  public

    procedure UnlockProtection;
    procedure LockProtection;

    // battery.rev
    property SoftwareVersion;
    property HardwareVersion;
    property SubVersion;
    property ChargerVersion : byte read GetChargerVersion;
    property SupervisorVersion : byte read GetSupervisorVersion;
    property BOM : byte read GetBOM;

    // battery.sn
    property CellPackItemNumber : word read GetCellpPackItemNumber;
    property PartNumber : word read GetPartNumber; // write SetPartNumber
    property Location : byte read GetLocation; // write SetLocation
    property ManufacturingDate : TDate read GetManufacturingDate; // write SetManufacturingDate
    property ItemNumber : word read GetItemNumber; // write SetItemNumber

    // battery.pcbSn
    property PCBSNPartNumber : word read GetPCBSNPartNumber;
    property PCBSNLocation : byte read GetPCBSNLocation;
    property PCBSNManufacturingDate : TDate read GetPCBSNManufacturingDate;
    property PCBSNItemNumber : word read GetPCBSNItemNumber;

    // battery.timer
    property TimerPower : word read GetTimerPower write SetTimerPower;
    {N} property PowerOutputShutdownDelay : word read GetTimerPower write SetTimerPower;
    property TimerAccessory : word read GetTimerAccessory write SetTimerAccessory;
    {N} property AccessoryShutdownDelay : word read GetTimerAccessory write SetTimerAccessory;
    property TimerPreCharge : byte read GetTimerPrecharge write SetTimerPrecharge;
    {N} property MotorPrechargeTime : byte read GetTimerPrecharge write SetTimerPrecharge;
    property TimerMasterShutdown : word read GetTimerMasterShutdown write SetTimerMasterShutdown;
    {N} property AutoShutdownDelay : word read GetTimerMasterShutdown write SetTimerMasterShutdown;

    // battery.status
    property CellMonitor5VVoltage : double read GetStatusV5V;
    property CellMonitor3V3Voltage : double read GetStatusV3V3;
    property CellMonitorPackIDVoltage : double read GetStatusVPackId;
    property CellMonitorBOMIDVoltage : double read GetStatusVBOMId;
    property StatusFlags : word read GetStatusFlags;
    {*}    property StatusTestFlags : word read GetStatusTestFlags write SetStatusTestFlags;
    {*}    property StatusPermanentFailureFlags : boolean read GetStatusPermanentFailureFlags write SetStatusPermanentFailureFlags;
    property ChargerManagerStatus : byte read GetStatusChargerManagerStatus;
    property NormalizedVoltage : double read GetStatusVBattNorm;
    property InternalBatteryVoltage : double read GetStatusVBattInternal;
    // vBattAvgNorm = stats.VoltageMean
    property Voltage : double read GetStatusVBatt;
    property CurrentPowerVoltage : double read GetStatusVPower;
    property CurrentControlVoltage : double read GetStatusVControl;
    property ConsoleVoltage : double read GetStatusVConsole;
    property Voltage12V : double read GetStatusV12V;
    property CurrentAccessoryVoltage : double read GetStautsVAccessory;
    property InputVoltage : double read GetStatusVDCIn;
    property CellpackCurrent : double read GetStatusICellpack;
    property ChargeLevel : double read GetStatusLevel;
    {*}    property StatusLeds : byte read GetStatusLeds write SetStatusLeds;
    property CellVoltage[CellNo : byte] : double read GetStatusVChannel;
    property SumCellVoltage[CellNo : byte] : double read GetStatusVCell;
    property PackTemperature[SensorNo : byte] : shortint read GetStatusPackTemperature;
    {*}    property StatusCapSense : dword read GetStatusCapSense write SetStatusCapSense;
    {*}    property StatusCapSenseReference : dword read GetStatusCapSenseReference write SetStatusCapSenseReference;
    property EstimatedSOC : byte read GetStatusEstimatedSOC;
    property PowerOnResets : byte read GetStatusPowerOnResetCount;
    property WatchdogResets : byte read GetStatusWatchdogResetCount;

    // battery.cellmon
    property CellMonChannelAddress : byte read GetCellMonChannelAddress write SetCellMonChannelAddress;
    property CellMonChannelData : word read GetCellMonChannelData; // write SetCellMonChannelData;
    property CellMonCalibrationData : word read GetCellMonCalibrationData write SetCellMonCalibrationData;
    property CellMonBalancerEnabled : boolean read GetCellMonBalancerEnabled write SetCellMonBalancerEnabled;

    // battery.charger
    property ChargerCurrent : double read GetChargerCurrent write SetChargerCurrent;
    property ChargerCurrentCalibration : double read GetChargerCurrentCalibration;
    {*}    property ChargerFinalVoltage : double read GetChargerFinalVoltage write SetChargerFinalVoltage;
    {*}    property ChargerMode : byte read GetChargerMode write SetChargerMode;
    property ChargerStatusFlags : word read GetChargerStatusFlags;
    property ChargerVoltageCalibration : double read GetChargerVoltageCalibration;

    // battery.bridges
    property BridgeI2CAddress : word read GetBridgeI2CAddress write SetBridgeI2CAddress;
    property BridgeI2CData : byte read GetBridgeI2CData write SetBridgeI2CData;
    property BridgeChargerAddress : byte read GetBridgeChargerAddress write SetBridgeChargerAddress;
    property BridgeChargerData : byte read GetBridgeChargerData write SetBridgeChargerData;

    // battery.calib
    property CalibCapsense : double read GetCalibCapsense write SetCalibCapsense;
    property CalibCalibration[CellNo : byte] : double read GetCalibCalibration write SetCellCalibration;
    property CalibCalibration3V3 : double read GetCalibCalibration3V3 write SetCalibrationValue3V3;

    // battery.stats
    property StatsV5VShorts : word read GetStats5VShorts write SetStats5VShorts;
    property StatsVControlShorts : word read GetStatsVControlShorts write SetStatsVControlShorts;
    property StatsLowVoltageBuzzerCount : word read GetStatsLowBattBuzzCount write SetStatsLowBattBuzzCount;
    property StatsCellVoltageCollapseCount : word read GetStatsCellVoltageCollapseCount write SetStatsCellVoltageCollapseCount;
    property StatsCellPartialShortCount : word read GetStatsCellPartialShortCount write SetStatsCellPartialShortCount;
    property StatsCellDeadShortCount : word read GetStatsCellDeadShortCount write SetStatsCellDeadShortCount;
    property StatsDeepSleepInactivityCount : word read GetStatsDeepSleepAfterLongInactivityPeriodCount write SetStatsDeepSleepAfterLongInactivityPeriodCount;
    property StatsDeepSleepSOCLowCount : word read GetStatsDeepSleepAfterLowSOCCount write SetStatsDeepSleepAfterLowSOCCount;
    property StatsDeepSleepLowVoltageCount : word read GetStatsDeepSleepExtremeLowBatteryVoltageCount write SetStatsDeepSleepExtremeLowBatteryVoltageCount;
    property TotalDischargedEnergy : DWord read GetStatsDischargeEnergy write SetStatsDischargeEnergy;
    property TotalChargedEnergy : DWord read GetStatsChargeEnergy write SetStatsChargeEnergy;
    property LMD : double read GetStatsLMD;
    property StatsReset : word read GetStatsReset write SetStatsReset;
    property StatsGasGageJitterCalibration : byte read GetStatsGGJrCalib write SetStatsGGJrCalib;
    property StatsResetWdt : byte read GetStatsResetWdt write SetStatsResetWdt; // = status.WatchdogResets, but writeable?
    property StatsRTCResync : byte read GetStatsRTCResync write SetStatsRTCResync;
    property StatsChargeTimeWorst : word read GetStatsChargeTimeWorst write SetStatsChargeTimeWorst;
    property StatsChargeTimeMean : word read GetStatsChargeTimeMean write SetStatsChargeTimeMean;
    {*}    property StatsLMDAdapt : byte read GetStatsLMDAdapt write SetStatsLMDAdapt;
    property StatsBattCycles : word read GetStatsBattCycles write SetStatsBattCycles;
    {-} property StatsBattFullCycles : word read GetStatsBattFullCycles write SetStatsBattFullCycles; // Set does nothing
    property StatsPowerCycles : word read GetStatsPowerCycles write SetStatsPowerCycles;
    {*}    property StatsVoltageMax : double read GetStatsVBattMax write SetVBattMax;
    {*}    property StatsVoltageMin : double read GetVBattMin write SetVBattMin;
    property StatsVoltageMean : double read GetVBattMean;
    property StatsTemperatureMax : shortint read GetStatsTBattMax write SetStatsTBattMax;
    property StatsTemperatureMin : shortint read GetStatsTBattMin write SetStatsTBattMin;
    property StatsCharge[Level : byte] : word read GetStatsCharge write SetStatsCharge;

    // battery.rtc
    property RTCTime : dword read GetRTCTime write SetRTCTime;
    property RTCTimeDT : TDateTime read GetRTCTimeDT write SetRTCTimeDT;
    property RTCLastChargeTimestamp : dword read GetRTCLastChargeTimestamp write SetRTCLastChargeTimestamp;
    property RTCLastChargeTimestampDT : TDateTime read GetRTCLastChargeTimestampDT write SetRTCLastChargeTimestampDT;
    property RTCLastValidTimestamp : dword read GetRTCLastValidTimestamp;
    property RTCLastValidTimestampDT : TDateTime read GetRTCLastValidTimestampDT;
    {*}    property RTCControl : byte read GetRTCCtrl write SetRTCCtrl;
    property RTCStatus : byte read GetRTCStatus;


    // battery.gg
    property GasGageAI : double read GetGasGageAI;
    property GasGageVoltage : double read GetGasGageVoltage;
    property GasGageSOC : byte read GetGasGageSOC;
    property GasGageLMD : double read GetGasGageLMD;
    property GasGageStatusFlags : byte read GetGasGageStatusFlags;
    property GasGageTemperature : double read GetGasGageTemperature;
    {*}    property GasGageDMFSD : byte read GetGasgageDMFSD write SetGasgageDMFSD;
    {*}    property GasGageVoltageDivider : double read GetGasGageVoltageDivider write SetGasGageVoltageDivider;

    // battery.config
    property ConfigMaxPackTemperature : shortint read GetConfigMaxPackTemperature write SetConfigMaxPackTemperature;
    property ConfigMinPackTemperature : shortint read GetConfigMinPackTemperature write SetConfigMinPackTemperature;
    property ConfigMaxGasgageTemperature : shortint read GetConfigMaxGGTemperature write SetConfigMaxGGTemperature;
    property ConfigMinGasgageTemperature : shortint read GetConfigMinGGTemperature write SetConfigMinGGTemperature;
    property MaxCellDeltaVoltage : double read GetConfigMaxCellDeltaVoltage write SetConfigMaxCellDeltaVoltage;
    property TaillampIntensity : byte read GetConfigTaillampIntensity write SetConfigTaillampIntensity;
    {*}    property Shipmode : boolean read GetConfigShipmode write SetConfigShipmode;
    property DeepSleepInactivityDelay : byte read GetConfigDeepSleepAfterLongInactivityPeriodDelay write SetConfigDeepSleepAfterLongInactivityPeriodDelay;
    property DeepSleepSOCLowDelay : byte read GetConfigDeepSleepLowSOCDelay write SetConfigDeepSleepLowSOCDelay;
    {*}    property CommunicationMode : byte read GetConfigCommMode write SetConfigCommMode;
    {*}    property ConfigAutoSwitchComm : boolean read GetConfigAutoSwitchComm write SetConfigAutoSwitchComm;
    {*}    property WakeOnPowerVoltage : boolean read GetConfigWakeOnPowerVoltage write SetConfigWakeOnPowerVoltage;
    property AllowChargingOnBike : boolean read GetConfigAllowBuckChargingOnBike write SetConfigAllowBuckChargingOnBike;
    {*}    property ConfigDiag : byte read GetConfigDiag write SetConfigDiag;
    {*}    property ConfigType : byte read GetConfigType write SetConfigType;
    {*}    property NominalVoltage : byte read GetConfigVBattNominal write SetConfigVBattNominal;
    property AccessoryMounted : boolean read GetConfigAccessoryMounted write SetConfigAccessoryMounted;
    property AccessoryEnabled : boolean read GetConfigAccessoryEnabled write SetConfigAccessoryEnabled;
    property AccessoryVoltage : double read GetConfigAccessoryVoltage write SetConfigAccessoryVoltage;
    {*}    property ConfigILMD : double read GetConfigILMD write SetConfigILMD;
    property MaxPowervoltageRegenCurrent : double read GetConfigMaxCharge write SetConfigMaxCharge;
    property MaxPowervoltageCurrent : double read GetConfigMaxDischarge write SetConfigMaxDischarge;
    property CellCapacity : double read GetConfigCellCapacity write SetConfigCellCapacity;
    {*}    property PackSerial : byte read GetConfigPackSerial write SetConfigPackSerial;
    property PackParallel : byte read GetConfigPackParallel write SetConfigPackParallel;
    {*}    property ConfigNAC : word read GetConfigNAC write SetConfigNAC;
    {*}    property ConfigForceDone : boolean read GetConfigForceDone write SetConfigForceDone;
    property ConfigShutdown : boolean read GetConfigShutdown write SetConfigShutdown;
    {*}    property EnablePowerVoltage : boolean read GetConfigVPower write SetConfigVPower;
    {*}    property EnableControlVoltage : boolean read GetConfigVControl write SetConfigVControl;
    {*}    property EnableInternalBatteryVoltage : boolean read GetConfigVBattInt write SetConfigVBattInt;
    property CapSenseSOCMode : byte read GetConfigCapSenseMode write SetConfigCapSenseMode;
    property ChargerCalibrationValuesSource : byte read GetConfigChargerCalibrationSourceFlags; // write SetChargerCalibrationValuesSource;
    property ChargerVoltageCalibrationValue : double read GetConfigChargerVoltageCalibrationValue; // write SetChargerVoltageCalibrationValue;
    property ChargerCurrentCalibrationValue : double read GetConfigChargerCurrentCalibrationValue; // write SetChargerCurrentCalibrationValue;

    // battery.protect
    property ProtectUnlock : byte read GetProtectUnlock write SetProtectUnlock;
    {*}    property ProtectMode : byte read GetProtectMode write SetProtectMode;
    {*}    property ProtectControl : byte read GetProtectControl write SetProtectControl;

  end;

  { TBionXMotor }

  TBionXMotor = class ( TBionXComponent )
  private
    FUnlockLevel : integer;

  private
    // motor.rev
    function GetSoftwareVersion: byte; override;
    function GetHardwareVersion: byte; override;
    function GetSubVersion: byte; override;

    // motor.sn
    function GetPartNumber : word;
    function GetLocation : byte;
    function GetManufacturingDate : TDate;
    function GetItemNumber : word;
    function GetOEM : word;
    function GetProduct : word;
    function GetStatorType : byte;

    // motor.protect
    function GetProtectUnlock: byte;
    procedure SetProtectUnlock ( AValue: byte ) ;

    // motor.geometry
    function GetGeometryCirc: word;
    procedure SetGeometryCirc(AValue: word);

    // motor.assist
    function GetAssistLevel : double;
    procedure SetAssistLevel ( AValue : double ) ;
    function GetAssistLevelOffSlope : double;
    procedure SetAssistLevelOffSlope ( AValue : double ) ;
    function GetAssistDirection : byte;
    procedure SetAssistDirection ( AValue : byte ) ;
    function GetAssistMaxSpeed: double;
    procedure SetAssistMaxSpeed(AValue: double);
    function GetAssistMaxSpeedDerateDelta : double;
    procedure SetAssistMaxSpeedDerateDelta ( AValue : double ) ;
    function GetAssistVQDynamicFlag : boolean;
    procedure SetAssistVQDynamicFlag ( AValue : boolean ) ;
    function GetAssistStatorPartNumber : word;
    procedure SetAssistStatorPartNumber ( AValue : word ) ;
    function GetAssistRegenInflex : double;
    procedure SetAssistRegenInflex ( AValue : double ) ;
    function GetAssistWalkSpeedDecreaseStart : double;
    procedure SetAssistWalkSpeedDecreaseStart ( AValue : double ) ;
    function GetAssistWalkSpeedDecreaseEnd : double;
    procedure SetAssistWalkSpeedDecreaseEnd ( AValue : double ) ;
    function GetAssistWalkMaxLevel : double;
    procedure SetAssistWalkMaxLevel ( AValue : double ) ;
    function GetAssistWalkLevel : double;
    procedure SetAssistWalkLevel ( AValue : double ) ;
    function GetAssistLowSpeedRampFlag : boolean;
    procedure SetAssistLowSpeedRampFlag ( AValue : boolean ) ;

    // motor.torque
    function GetTorqueGaugePolarity : byte;
    procedure SetTorqueGaugePolarity ( AValue : byte ) ;
    function GetTorqueGaugeType : byte;
    procedure SetTorqueGaugeType ( AValue : byte ) ;
    function GetTorqueGaugeValue : double;
    function GetTorqueGaugeVoltage : double;
    function GetTorqueGaugeNoise : double;
    procedure SetTorqueGaugeNoise ( AValue : double ) ;
    function GetTorqueGaugeDelay : double;
    procedure SetTorqueGaugeDelay ( AValue : double ) ;
    function GetTorqueGaugeSpeed : double;
    procedure SetTorqueGaugeSpeed ( AValue : double ) ;
    function GetTorqueGaugeReference : double;
    procedure SetTorqueGaugeReference ( AValue : double ) ;
    function GetTorqueGaugeGain : double;
    procedure SetTorqueGaugeGain ( AValue : double ) ;
    function GetTorqueGaugeMaxVoltage : double;
    procedure SetTorqueGaugeMaxVoltage ( AValue : double ) ;
    function GetTorqueGaugeMaxVoltageDelay : double;
    procedure SetTorqueGaugeMaxVoltageDelay ( AValue : double ) ;

    // motor.preference
    function GetPreferenceRegion : byte;
    procedure SetPreferenceRegion ( AValue : byte ) ;

    // motor.config
    function GetConfigCommMode : word;
    procedure SetConfigCommMode ( AValue : word ) ;
    function GetConfigEnablePWMLimit : boolean;
    procedure SetConfigEnablePWMLimit ( AValue : boolean ) ;

    // motor.status
    function GetStatusMain : byte;
    procedure SetStatusMain ( AValue : byte ) ;
    function GetStatusCodes : byte;
    function GetStatusCodesLatch : byte;
    function GetStatusSpeed : double;
    function GetStatusPowerMeter : double;
    function GetStatusTemp: byte;
    function GetStatusVPower : double;
    function GetStatusV12V : double;
    function GetStatusV5V : double;

    // motor.stats
    function GetStatsMaxVPower : double;
    procedure SetStatsMaxVPower ( AValue : double ) ;
    function GetStatsMaxVTemp : double;
    procedure SetStatsMaxVTemp ( AValue : double ) ;
    function GetStatsOdo : word;
    procedure SetStatsOdo ( AValue : word ) ;
    function GetStatsChronoHours : word;
    procedure SetStatsChronoHours ( AValue : word ) ;
    function GetStatsChronoSeconds : word;
    procedure SetStatsChronoSeconds ( AValue : word ) ;
    function GetStatsHallDCHS : word;
    procedure SetStatsHallDCHS ( AValue : word ) ;
    function GetStatsHallTrans : word;
    procedure SetStatsHallTrans ( AValue : word ) ;
    function GetStatsHallRing : word;
    procedure SetStatsHallRing ( AValue : word ) ;
    function GetStatsHallLost : word;
    procedure SetStatsHallLost ( AValue : word ) ;

  public
    procedure UnlockProtection;
    procedure LockProtection;

    // motor.rev
    property SoftwareVersion;
    property HardwareVersion;
    property SubVersion;

    // motor.sn
    property PartNumber : word read GetPartNumber; // write SetPartNumber
    property Location : byte read GetLocation; // write SetLocation
    property ManufacturingDate : TDate read GetManufacturingDate; // write SetManufacturingDate
    property ItemNumber : word read GetItemNumber; // write SetItemNumber
    property OEM : word read GetOEM; // write SetOEM
    property Product : word read GetProduct; // write SetProduct;
    property StatorType : byte read GetStatorType; // write SetStatorType;

    // motor.protect
    property ProtectUnlock : byte read GetProtectUnlock write SetProtectUnlock;

    // motor.geometry
    property GeometryCirc : word read GetGeometryCirc write SetGeometryCirc;
    {N} property WheelCircumference : word read GetGeometryCirc write SetGeometryCirc;

    // motor.assist
    {*}    property AssistLevel : double read GetAssistLevel write SetAssistLevel;
    {*}    property AssistLevelOffSlope : double read GetAssistLevelOffSlope write SetAssistLevelOffSlope;
    {*}    property AssistDirection : byte read GetAssistDirection write SetAssistDirection;
    property AssistMaxSpeed : double read GetAssistMaxSpeed write SetAssistMaxSpeed;
    {*}    property AssistMaxSpeedDerateDelta : double read GetAssistMaxSpeedDerateDelta write SetAssistMaxSpeedDerateDelta;
    {*}    property AssistVQDynamicFlag : boolean read GetAssistVQDynamicFlag write SetAssistVQDynamicFlag;
    {*}    property StatorPartNumber : word read GetAssistStatorPartNumber write SetAssistStatorPartNumber;
    {*}    property AssistRegenInflex : double read GetAssistRegenInflex write SetAssistRegenInflex;
    property WalkSpeedDecreaseStartSpeed : double read GetAssistWalkSpeedDecreaseStart write SetAssistWalkSpeedDecreaseStart;
    property WalkSpeedDecreaseEndSpeed : double read GetAssistWalkSpeedDecreaseEnd write SetAssistWalkSpeedDecreaseEnd;
    property WalkLevelMax : double read GetAssistWalkMaxLevel write SetAssistWalkMaxLevel;
    property WalkLevel : double read GetAssistWalkLevel write SetAssistWalkLevel;
    {*}    property LowSpeedRamp : boolean read GetAssistLowSpeedRampFlag write SetAssistLowSpeedRampFlag;

    // motor.torque
    {*}    property TorqueGaugePolarity : byte read GetTorqueGaugePolarity write SetTorqueGaugePolarity;
    {*}    property TorqueGaugeType : byte read GetTorqueGaugeType write SetTorqueGaugeType;
    property TorqueGaugeValue : double read GetTorqueGaugeValue;
    property TorqueGaugeVoltage : double read GetTorqueGaugeVoltage;
    {*}    property TorqueGaugeNoise : double read GetTorqueGaugeNoise write SetTorqueGaugeNoise;
    {*}    property TorqueGaugeDelay : double read GetTorqueGaugeDelay write SetTorqueGaugeDelay;
    {*}    property TorqueGaugeSpeed : double read GetTorqueGaugeSpeed write SetTorqueGaugeSpeed;
    {*}    property TorqueGaugeReference : double read GetTorqueGaugeReference write SetTorqueGaugeReference;
    {*}    property TorqueGaugeGain : double read GetTorqueGaugeGain write SetTorqueGaugeGain;
    {*}    property TorqueGaugeMaxVoltage : double read GetTorqueGaugeMaxVoltage write SetTorqueGaugeMaxVoltage;
    {*}    property TorqueGaugeMaxVoltageDelay : double read GetTorqueGaugeMaxVoltageDelay write SetTorqueGaugeMaxVoltageDelay;

    // motor.preference
    {*}    property PreferenceRegion : byte read GetPreferenceRegion write SetPreferenceRegion;

    // motor.config
    {*}    property ConfigCommMode : word read GetConfigCommMode write SetConfigCommMode;
    {*}    property ConfigEnablePWMLimit : boolean read GetConfigEnablePWMLimit write SetConfigEnablePWMLimit;

    // motor.status
    {*}    property StatusMain : byte read GetStatusMain write SetStatusMain;
    property StatusCodes : byte read GetStatusCodes;
    property StatusCodesLatch : byte read GetStatusCodesLatch;
    property MotorSpeed : double read GetStatusSpeed;
    property MotorPower : double read GetStatusPowerMeter;
    property Temperature : byte read GetStatusTemp;
    property PowerVoltage : double read GetStatusVPower;
    property _12VVoltage : double read GetStatusV12V;
    property _5VVoltage : double read GetStatusV5V;

    // motor.stats
    {*}    property StatsMaxVPower : double read GetStatsMaxVPower write SetStatsMaxVPower;
    {*}    property StatsMaxVTemp : double read GetStatsMaxVTemp write SetStatsMaxVTemp;
    {*}    property StatsOdo : word read GetStatsOdo write SetStatsOdo;
    {*}    property StatsChronoHours : word read GetStatsChronoHours write SetStatsChronoHours;
    {*}    property StatsChronoSeconds : word read GetStatsChronoSeconds write SetStatsChronoSeconds;
    {*}    property StatsHallDCHS : word read GetStatsHallDCHS write SetStatsHallDCHS;
    {*}    property StatsHallTrans : word read GetStatsHallTrans write SetStatsHallTrans;
    {*}    property StatsHallRing : word read GetStatsHallRing write SetStatsHallRing;
    {*}    property StatsHallLost : word read GetStatsHallLost write SetStatsHallLost;

  public
  end;

  { TBionXSensor }

  TBionXSensor = class ( TBionXComponent )
  private
    FMode : byte;
  private
    // sensor.rev
    function GetSoftwareVersion: byte; override;
    function GetHardwareVersion: byte; override;
    function GetSubVersion: byte; override;

    // sensor.sn
    function GetPartNumber : word;
    function GetLocation : byte;
    function GetManufacturingDate : TDate;
    function GetItemNumber : word;

    // sensor.config
    function GetConfigMode: byte;
    procedure SetConfigMode ( AValue: byte ) ;
    function GetConfigGaugeGain: double;
    procedure SetConfigGaugeGain ( AValue: double ) ;
    function GetConfigRampUpSteps: word;
    procedure SetConfigRampUpSteps ( AValue: word ) ;
    function GetConfigDecayDelay: word;
    procedure SetConfigDecayDelay ( AValue: word ) ;
    function GetConfigDecaySteps: word;
    procedure SetConfigDecaySteps ( AValue: word ) ;
    function GetConfigSpeedThreshold: double;
    procedure SetConfigSpeedThreshold ( AValue: double ) ;
    function GetConfigRampActiveOverThreshold: boolean;
    procedure SetConfigRampActiveOverThreshold ( AValue: boolean ) ;
    function GetConfigInputOffset: double;
    procedure SetConfigInputOffset ( AValue: double ) ;

    // sensor.status
    function GetStatusVTorque: double;
    function GetStatusCadence: double;
    function GetStatusVOutput: double;
    function GetStatusPulseCounter: Byte;

  public

    // sensor.rev
    property SoftwareVersion;
    property HardwareVersion;
    property SubVersion;

    // sensor.sn
    property PartNumber : word read GetPartNumber; // write SetPartNumber
    property Location : byte read GetLocation; // write SetLocation
    property ManufacturingDate : TDate read GetManufacturingDate; // write SetManufacturingDate
    property ItemNumber : word read GetItemNumber; // write SetItemNumber

    // sensor.config
    property ConfigMode : byte read GetConfigMode write SetConfigMode;
    property ConfigGaugeGain : double read GetConfigGaugeGain write SetConfigGaugeGain;
    property ConfigRampUpSteps : word read GetConfigRampUpSteps write SetConfigRampUpSteps;
    property ConfigDecayDelay : word read GetConfigDecayDelay write SetConfigDecayDelay;
    property ConfigDecaySteps : word read GetConfigDecaySteps write SetConfigDecaySteps;
    property ConfigSpeedThreshold : double read GetConfigSpeedThreshold write SetConfigSpeedThreshold;
    property ConfigRampActiveOverThreshold : boolean read GetConfigRampActiveOverThreshold write SetConfigRampActiveOverThreshold;
    property ConfigInputOffset : double read GetConfigInputOffset write SetConfigInputOffset;

    // sensor.status
    property TorqueVoltage : double read GetStatusVTorque; // write SetTorqueVoltage;
    property Cadence : double read GetStatusCadence; // write SetCadence;
    property OutputVoltage : double read GetStatusVOutput; // write SetOutputVoltage;
    property PulseCounter : Byte read GetStatusPulseCounter; // write SetPulseCounter;

  end;

type
  TKeepAliveThread = class ( TThread )
  private
    FBattery : TBionXBattery;
  protected
    procedure Execute; override;
  public
    constructor Create ( aBattery : TBionXBattery );
  end;

  { TBionXBike }

type
  TBionXBike = class ( TObject )
  private
    FCANIntf   : TCANInterface;
    FConsole : TBionXConsole;
    FBattery : TBionxBattery;
    FMotor   : TBionXMotor;
    FSensor  : TBionXSensor;
    FForceKeepAlive  : boolean;
    FKeepAliveThread : TKeepAliveThread;
    function GetKeepAlive: boolean;
    procedure SetKeepAlive ( AValue: boolean ) ;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    function Connect ( CANIntf : TCANInterface ) : boolean; overload;
    procedure Disconnect;

    function SetToSlaveMode : boolean;

    procedure Shutdown;

    // here we provide those Set-procedures as a member of TBionXBike,
    // which need settings in more than one register or device

    procedure SetWheelCircumference ( Circumference : word ) ;

    procedure SetAssistMaxSpeed ( Speed : double );
    procedure SetAssistMinSpeed ( Speed : double );
    procedure SetThrottleMaxSpeed ( Speed : double );

    procedure SetBrakeSensor ( SensorEnabled : boolean; SensorType : byte );

    property ForceKeepAlive : boolean read FForceKeepAlive write FForceKeepAlive;
    property KeepAlive : boolean read GetKeepAlive write SetKeepAlive;

    property Console : TBionXConsole read FConsole;
    property Battery : TBionxBattery read FBattery;
    property Motor : TBionXMotor read FMotor;
    property Sensor : TBionXSensor read FSensor;

    // the CANAdapter is public for tests only. You should not use it from the
    // main program. Implement a function call in this class or in Console..Sensor
    // instead.
    property CANIntf : TCANInterface read FCANIntf;
  end;

type
  VersionException = class ( Exception );

function BoolToStr ( b : boolean ) : string;
function DisplayUnitsToStr ( Units : byte ) : string;
function LightButtonModeToStr ( Mode : byte ) : string;
function BrakeSensorTypeToStr ( SensorType : byte ) : string;
function ConsoleTypeToStr ( ConsoleType : byte ) : string;
function BatteryCommunicationModeToStr ( Mode : byte ) : string;
function BOMToStr ( BOM : byte ) : string;
function ChargerStatusToStr ( Status : byte ) : string;
function ChargerModeToStr ( Mode : byte  ) : string;
function CapSenseSOCModeToStr ( Mode : byte ) : string;
function RTCStatusToStr ( Status : byte ) : string;
function BatteryFlagsToStr ( Flags : word ) : string;
function MotorCommunicationModeToStr ( Mode : word ) : string;
function MotorStatusToStr ( Status : byte ) : string;
function SensorModeToStr ( Mode : byte ) : string;
function ConsoleCodeBitString ( bit : word ) : string;
function ConsoleCodesToStr ( Code : longword ) : string;
function _GetCANIdName( CANId : byte ) : string;

implementation

type
  TVersionInfo = record
    sw_since  : byte;
    sw_until  : byte;
    sub_since : byte;
    sub_until : byte;
    hw_since  : byte;
    hw_until  : byte;
  end;

//const
//  allVersions : TVersionInfo = ( sw_since : 0; sw_until : 255; sub_since : 0; sub_until : 255; hw_since : 0; hw_until : 255 );

const

  UNLIMITED_SPEED_VALUE                                    = 70; // Km/h
//  UNLIMITED_MIN_SPEED_VALUE                                = 30; // Km/h
  UNLIMITED_THROTTLE_SPEED_VALUE                           = 70; // Km/h

  SPEED_FACTOR                                             = 0.1;
  DISTANCE_FACTOR                                          = 0.1;
  SENESORGAIN_FACTOR                                       = 0.1;
  ASSIST_FACTOR                                            = 1.5625;
  VOLTAGE_FACTOR                                           = 0.001;
  CURRENT_FACTOR                                           = 0.001;
  NORMALIZED_VOLTAGE_OFFSET                                = 20.8333;
  NORMALIZED_VOLTAGE_FAKTOR                                = 0.416667;

// abbreviations:
//   SOC = State Of Charge
//   LMD = Last Measured Discharge
//   NIP = ?

  {%REGION Console}
  ID_CONSOLE_MASTER                                        = $08; // (CAN ID in master mode)
  ID_CONSOLE_SLAVE                                         = $48; // (CAN ID in slave mode)
//  ID_CONSOLE_RESPONSE                                      = $58;
  ID_BIB                                                   = $58;

    // Reg 0..79 unused

    REG_CONSOLE_STATISTIC_DIST_HI                          = $50; // [factor:0.1]
    REG_CONSOLE_STATISTIC_DIST_LO                          = $51;
    REG_CONSOLE_STATISTIC_AVGSPEED_HI                      = $52; // [factor:0.1]
    REG_CONSOLE_STATISTIC_AVGSPEED_LO                      = $53;

    // Reg 84..99 unused

    REG_CONSOLE_STATISTIC_ODOMETER_HIHI                    = $64; // Odometer [unit:km, faktor:0.1]
    REG_CONSOLE_STATISTIC_ODOMETER_HILO                    = $65; // !!! reverse byte order on writing !!!
    REG_CONSOLE_STATISTIC_ODOMOTER_LOHI                    = $66;
    REG_CONSOLE_STATISTIC_ODOMETER_LOLO                    = $67;

    REG_CONSOLE_PREFERENCE_NIP_HIHI                        = $68; // -
    REG_CONSOLE_PREFERENCE_NIP_HILO                        = $69;
    REG_CONSOLE_PREFERENCE_NIP_LOHI                        = $6A;
    REG_CONSOLE_PREFERENCE_NIP_LOLO                        = $6B;
    REG_CONSOLE_THROTTLE_CALIBRATED                        = $6C; // throttle calibration performed
    REG_CONSOLE_STATISTIC_CHRONO_SECOND                    = $6D; // trip time seconds
    REG_CONSOLE_STATISTIC_CHRONO_MINUTE                    = $6E; // trip time minutes
    REG_CONSOLE_STATISTIC_CHRONO_HOUR                      = $6F; // trip time hours
    REG_CONSOLE_PREFERENCE_LCD_CONTRAST                    = $70; // LCD contrast
    REG_CONSOLE_SN_LOCATION                                = $71; // location
    REG_CONSOLE_SN_YEAR                                    = $72; // mfd year
    REG_CONSOLE_SN_MONTH                                   = $73; // mfd month
    REG_CONSOLE_SN_DAY                                     = $74; // mfd day

    REG_CONSOLE_SN_PN_HI                                   = $75; // product number
    REG_CONSOLE_SN_PN_LO                                   = $76;
    REG_CONSOLE_SN_ITEM_HI                                 = $77; // serial number
    REG_CONSOLE_SN_ITEM_LO                                 = $78;

    // Reg 121 unused

    REG_CONSOLE_ASSIST_GAUGE_JOINT                         = $7A; // gauge joint [range:0..11]
    REG_CONSOLE_THROTTLE_MIN_HI                            = $7B; // throttle min actor value
    REG_CONSOLE_THROTTLE_MIN_LO                            = $7C;
    REG_CONSOLE_THROTTLE_MAX_HI                            = $7D; // throttle max actor value
    REG_CONSOLE_THROTTLE_MAX_LO                            = $7E;

    // Reg 127 unused

    REG_CONSOLE_PREFERENCE_LIGHT_ON_AT_START               = $80; // LightOnAtStart Indicate if the accessory shall be enabled when turning the bike on. 0-No, 1-Yes

    REG_CONSOLE_GEOMETRY_CIRC_HI                           = $81; // Circumference of the rear wheel [mm]
    REG_CONSOLE_GEOMETRY_CIRC_LO                           = $82;

    REG_CONSOLE_ASSIST_MAXSPEED_FLAG                       = $83; // Indicates that there is an upper speed limit where the motor is allowed to assist
    REG_CONSOLE_ASSIST_MAXSPEED_HI                         = $84; // Maximum speed for which the motor can assist. This is irrelevant if maxSpeedFlag is not set [unit:km/h, range:1..??, factor:0.1] (Code 3773)
    REG_CONSOLE_ASSIST_MAXSPEED_LO                         = $85;

    REG_CONSOLE_THROTTLE_MAXSPEED_FLAG                     = $86; // throttle max speed flag
    REG_CONSOLE_THROTTLE_MAXSPEED_HI                       = $87; // throttle max speed MSB [unit:km/h, factor:0.1] Code 3775
    REG_CONSOLE_THROTTLE_MAXSPEED_LO                       = $88;
    REG_CONSOLE_ASSIST_MINSPEED_FLAG                       = $89; // min speed flag
    REG_CONSOLE_ASSIST_MINSPEED                            = $8A; // min speed Code 3776

    REG_CONSOLE_ASSIST_BRAKE_LEVEL                         = $8B; // brake rekuperation level [unit:%, range:0..64, factor:1.5625] Code 2002
    REG_CONSOLE_PREFERENCE_TRIP_TO_EMPTY_FLAG              = $8C; // 0:hide, 1:show remaining time/dist PC2003 (no effect anymore?)
    REG_CONSOLE_PREFERENCE_DISPLAY_UNITS                   = $8D; // Indicates unit system to use when displaying units. 0-English, 1-Metric
    REG_CONSOLE_THROTTLE_ENABLE_ONSTRAIN                   = $8E; // Allow throttle over maxSpeed if strain gauge signal detected

    // Reg 143..159 unused

    REG_CONSOLE_ASSIST_BRAKE_FLAG                          = $A0; // brage sensor enable; 0: brake sensor off, 1: sensor on  Code 2006
    REG_CONSOLE_ASSIST_BRAKE_POLARITY                      = $A1; // brake sensor type:   0: NO (Normal Open), 1: NC (Normal Closed), 2:0-5 (analog 0..5V), 3:5-0 (analog 5..0V) Code 2006
    REG_CONSOLE_ASSIST_GAUGE_FILTER                        = $A2; // Torque sensor speed  [range:0..8 (1..4 recommended)] Code 1234

    REG_CONSOLE_REV_SW                                     = $A3; // software version

    REG_CONSOLE_ASSIST_GAUGE_GAIN                          = $A4; // sensor gain [range:0.1..4.0, factor:0.1] Code 0007
    REG_CONSOLE_ASSIST_GAIN_A                              = $A5; // torque sensor extra gain [range:0.1..4.0, factor:0.1] Code 0008a
    REG_CONSOLE_ASSIST_GAIN_B                              = $A6; // torque sensor extra gain max speed [unit:km/h, range:0.0..25.0, factor:0.1] Code 0008b
    REG_CONSOLE_SN_TYPE                                    = $A7; // Type of console: 0-EPS, 1-RIDE+, 2-Boost.
    REG_CONSOLE_PREFERENCE_REGION                          = $A8; // region
    REG_CONSOLE_PREFERENCE_CONFIGBIT_0                     = $A9; // ConfigBit0
    REG_CONSOLE_THROTTLE_ENABLE_BOOST_DISPLAY              = $AA; // Display the "boost" message or pictogram when it activates
    REG_CONSOLE_ASSIST_AUTOREGEN_FLAG                      = $AB; // Autoregen Flag

    // Reg 172 unused                                             // RegenEnabled, yet unused

    REG_CONSOLE_REV_SUB                                    = $AD; // software subversion
    REG_CONSOLE_PREFERENCE_LIGHT_BUTTON_MODE               = $AE; // Indicate the operation of console button "light/power". 0-Click turns bike off, hold toggles accessory; 1-Click toggle accessory, hold turns bike off
    REG_CONSOLE_PREFERENCE_EXPERTMODE                      = $AF; // ExpertMode Add new display mode for expert mode (unitl hw 15)

    REG_CONSOLE_ASSIST_LEVEL_1                             = $B0; // assist level 1 [unit:%, factor:1,5625]
    REG_CONSOLE_ASSIST_LEVEL_2                             = $B1; // assist level 2 [unit:%, factor:1,5625]
    REG_CONSOLE_ASSIST_LEVEL_3                             = $B2; // assist level 3 [unit:%, factor:1,5625]
    REG_CONSOLE_ASSIST_LEVEL_4                             = $B3; // assist level 4 [unit:%, factor:1,5625]
    REG_CONSOLE_ASSIST_INITLEVEL                           = $B4; // initial assist level [range:0..4]

    REG_CONSOLE_PREFERENCE_CODES_HIHI                      = $B5; // Codes LSB 0-(3771, 2005)Wheel circumference, 1-(3772)Diagnostic mode, 2-(3773)Max speed, 3-(3774)Overvoltage protection, 4-(3775)Max throttle speed, 5-(3776)Minimum assist speed, 6-(1976)Motor direction, 7-(5000)Deprecated, 8-(2001)metric vs imperial, 9-(2002)Regen value, 10-(2003)Remaining distance, 11-(2004)Clock time, 12-(2006)Brake switch config, 13-(2007)Throttle polarity, 14-(2008)Accessory voltage, 15-(0041)Slave console, 16-(1234)Filter, 17-(1970)Light sensor, 18-(0007)Gauge gain, 19-(0008)Assistance gain, 20-(0009)Gauge joint, 21-(0911)Deprecated, 22-(0001)Console info, 23-(0002)Battery info, 24-(0003)Motor info, 25-(6000)Battery statistics, 26-(0006)Speed gain, 27-Alarm, 28-Time
    REG_CONSOLE_PREFERENCE_CODES_HILO                      = $B6;
    REG_CONSOLE_PREFERENCE_CODES_LOHI                      = $B7;
    REG_CONSOLE_PREFERENCE_CODES_LOLO                      = $B8;
    REG_CONSOLE_PREFERENCE_CODESRW_HIHI                    = $B9; // CodesRW LSB Same bit as console.preference.codes except for: 28-Mountain mode
    REG_CONSOLE_PREFERENCE_CODESRW_HILO                    = $BA;
    REG_CONSOLE_PREFERENCE_CODESRW_LOHI                    = $BB;
    REG_CONSOLE_PREFERENCE_CODESRW_LOLO                    = $BC;
    REG_CONSOLE_SN_OEM_HI                                  = $BD; // OEM
    REG_CONSOLE_SN_OEM_LO                                  = $BE;
    REG_CONSOLE_PREFERENCE_THROTTLE_MODE                   = $BF; // ThrottleMode Configure the throttle for the menu change mode (until hw 15)
    REG_CONSOLE_ASSIST_SPEEDGAIN                           = $C0; // [ factor:0,1 ]
    REG_CONSOLE_SN_PRODUCT_HI                              = $C1; // product
    REG_CONSOLE_SN_PRODUCT_LO                              = $C2;
    REG_CONSOLE_THROTTLE_BOOST_TRIGGERLEVEL                = $C3; // Boost Trigger Level [unit:%, range:1.5..50, factor:1.5625] Code 3779
    REG_CONSOLE_PREFERENCE_FLIP_SIDE                       = $C4; // Indicates on what side of the handlebar is located the console. 0-Right, 1-Left Code 2009
    REG_CONSOLE_CONFIG_TESTMODE                            = $C5; // Writing a 1 to this register allows setting the console in test mode to test the LCD and buttons
    REG_CONSOLE_CONFIG_TESTMODE_HW14                       = $C3; // Reg C3 with HW14

    REG_CONSOLE_ASSIST_MOUNTAIN_CAP                        = $C6; // Maximum set point to send to motor when in mountain mode. 0-Disabled [unit:%, range:0..100, faktor:1.5625]
    REG_CONSOLE_CONFIG_LAST_MODE                           = $C7; // Writing 255 will disable the last mode on display power on, 0 will enable the last mode on display power on
      CONSOLE_CONFIG_LAST_MODE_ON                          = $00;
      CONSOLE_CONFIG_LAST_MODE_OFF                         = $FF;

    // Reg 200..207 unused

    REG_CONSOLE_REV_HW                                     = $D0; // hardware version
    REG_CONSOLE_STATUS_SLAVE                               = $D1; // Writing a 1 to this register allows going imemdiatly in slave mode, cannot be set to 0

    REG_CONSOLE_THROTTLE_RAW_HI                            = $D2; // throttle raw position
    REG_CONSOLE_THROTTLE_RAW_LO                            = $D3;
    REG_CONSOLE_THROTTLE_POSITION                          = $D4; // throttle position [factor:1.5625]

    // Reg 213 unused                                             // outdated, formerly used by pref. language
    // Reg 214 unused                                             // and boat related. Now no data from regs.

    REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_3                = $D7; // 215 Reku Level 3 [unit:%, factor:1,5625]
    REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_4                = $D8; // 216 Reku Level 4 [unit:%, factor:1,5625]
    REG_CONSOLE_CONFIG_SERVICE_TIMESTAMP_HI                = $D9; // Day, in battery-relative value, from which console displays "service". 0 to disable the feature
    REG_CONSOLE_CONFIG_SERVICE_ZIMESTAMP_LO                = $DA;
    REG_CONSOLE_CONFIG_SERVICE_DISTANCE_HI                 = $DB; // Odometer value, from which console displays "service". 0 to disable the feature
    REG_CONSOLE_CONFIG_SERVICE_DISTANCE_LO                 = $DC;

    // Reg 211 unused

    REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_1                = $DE; // Reku Level 1 [unit:%, factor:1,5625]
    REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_2                = $DF; // Reku level 2 [unit:%, factor:1,5625]

    // Reg 224..255 unused
  {%ENDREGION Console}

  {%REGION Battery}
  ID_BATTERY                                               = $10;

//  ID_BATTERY_RESPONSE                                      = $08;
    REG_BATTERY_CONFIG_ALLOW_BUCKCHARGING_ON_BIKE          = $12; // Specifies if the battery can recharge in buck mode even on a bike. Make sure it is IMPOSSIBLE to have an accessory output before setting this to 1. 0: Disallow, 1: Allow
    REG_BATTERY_STATUS_CHARGER_MANAGER_STATUS              = $13; // Gives state of charging Mef: 0-Off, 1-Stand-by, 2-Charger, 3-Accessory, 4-Vdcin sense, 5-Overtemp, 6-Charge done, 7-Buck failed
    REG_BATTERY_CONFIG_WAKE_ON_POWERVOLTAGE                = $14; // Specifies if the battery should wake up automatically when a voltage is present on the vPower. A value of 0 disables the feature

    REG_BATTERY_CONFIG_SLA_CONSTANT_A                      = $15; // ++++ Boat related (SOC estimator)
    REG_BATTERY_CONFIG_SLA_CONSTANT_B                      = $16; // ++++ Boat related (SOC estimator)
    REG_BATTERY_CONFIG_SLA_CONSTANT_C                      = $15; // ++++ Boat related (SOC estimator)

    REG_BATTERY_REV_SUB                                    = $18; // software subversion

    REG_BATTERY_CONFIG_SLA_CONSTANT_D                      = $15; // ++++ Boat related (SOC estimator)

    REG_BATTERY_RTC_LAST_VALID_TIMESTAMP_HIHI              = $19; // Indicates last valid battery time. This read-only register is set to rtc.time when written and then refresh each 34 minutes
    REG_BATTERY_RTC_LAST_VALID_TIMESTAMP_HILO              = $1A;
    REG_BATTERY_RTC_LAST_VALID_TIMESTAMP_LOHI              = $1B;
    REG_BATTERY_RTC_LAST_VALID_TIMESTAMP_LOLO              = $1C;

    REG_BATTERY_STATUS_FLAGS_HI                            = $1D; // Alert status bits: 0-Vctrl (code 20), 1-Precharge (code 21 and 67), 2-Relay (code 22), 3-BMS (code 23), 4-DCDC (code 28), 6-GG out of range temperature, 7-battery pack out of range temperature, 8-balancer overvolt (code 62), 9-Balancer undervolt (code 61), 10-Pack problem (code 63), 11-Accessory overcurrent (code 60), 12-Electronic fuse (code 66), 13-Balancer plug not connected, 14- +5v short(lached)

    REG_BATTERY_STATUS_CELLPACK_CURRENT_HI                 = $1E; // Reading battery current by a shunt resistor. No delay, no calibration compared to battery.gg.ai [unit:A, factor:0.001]
    REG_BATTERY_STATUS_CELLPACK_CURRENT_LO                 = $1F; // !!! signed !!!

    REG_BATTERY_CONFIG_POWER_VOLTAGE_ENABLE                = $21; // - ??? Enable/Disable vPower ???

    REG_BATTERY_CONFIG_ACCESSORY_ENABLED                   = $22; // -

    REG_BATTERY_CONFIG_SHUTDOWN                            = $25; // write 1 to shutdwon system

    REG_BATTERY_CONFIG_CONTROL_VOLTAGE_ENABLE              = $26; // Enable/Disable vControl

    REG_BATTERY_CONFIG_ACCESSORY_VOLTAGE                   = $28; // - until hw 52 [unit:V, factor:6]
                                                                  //   since hw 60 [unit:V, factor:0.1]

    REG_BATTERY_CONFIG_CAP_SENSE_MODE                      = $29; // Controls the mode of operation of the SoC level indicator. 0: Inactive, 1: Touch detect when the battery is OFF, 2: Touch detect when OFF and SoC indication when battery is ON. 3: red and blue colors . 4: 5levels SoC

    REG_BATTERY_CONFIG_COMMUNICATION_MODE                  = $2A; // Determines how the battery communicates. To change the value, we must first write 0xaa to this register and then write 1 to switch to I2C or 2 to switch to CAN
      BATTERY_CONFIG_COMMUNICATION_MODE_KEY                = $AA;

    REG_BATTERY_STATUS_ESTIMATED_SOC                       = $30; // Return an estimated value of SOC based on battery voltage. Only works with LiIon battery [unit:%]

    REG_BATTERY_STATUS_BATTERY_VOLTAGE_NORMALIZED          = $32; // Battery voltage normalized with 3.7V/cell. status.vBattInternal it used in Rev 104 and less otherwise status.vBatt [unit:V, factor:0.416667, offset:20.8333]
    REG_BATTERY_STATISTIC_BATTERY_AVGVOLTAGE_NORMALIZED    = $33; // Average battery voltage read during 50s based on battery.status.vBatt, in percentage of its nominal voltage [unit:V, factor:0.416667, offset:20.8333]

    REG_BATTERY_CONFIG_SHIPMODE                            = $37; // Determines if the battery is to go in ship mode (only external power can wake it) upon its next shutdown. We need to first write 0xaa and then the desired value to set the value. 0-Normal mode, 1-Ship mode
      BATTERY_CONFIG_SHIPMODE_KEY                          = $AA;

    REG_BATTERY_REV_HW                                     = $3B; // hardware version
    REG_BATTERY_REV_SW                                     = $3C; // software versin

    REG_BATTERY_CONFIG_TYPE                                = $3D; // -

    REG_BATTERY_REV_BOM                                    = $41; // Identification of printed circuit board bill of material version. 1: All smc6.2 and SMC#6.3r1. 8: SMC#6.3r4

    REG_BATTERY_CONFIG_TAILLAMP_INTENSITY                  = $43; // Controls the intensity of the tail Lamp. 0: OFF. 1-100%: On

    REG_BATTERY_CONFIG_ACCESSORY_MOUNTED                   = $44; // -

    REG_BATTERY_CONFIG_BATTINT_VOLTAGE_ENABLE              = $45; // Enable/Disable vBattInt

    REG_BATTERY_CONFIG_DIAG                                = $46; // -

    REG_BATTERY_CONFIG_FORCE_DONE                          = $47; // ++++ Setting this register to 0x45 will force SOC to 100%
      BATTERY_CONFIG_FORCE_DONE                            = $45;

    REG_BATTERY_STATISTIC_RESETS_HI                        = $48; // reset counter
    REG_BATTERY_STATISTIC_RESETS_LO                        = $49;

    REG_BATTERY_STATUS_INTERNAL_BATTERY_VOLTAGE_HI         = $4A; // Reading of vBattInternal [unit:V, factor:0.001]
    REG_BATTERY_STATUS_INTERNAL_BATTERY_VOLTAGE_LO         = $4B;

    REG_BATTERY_STATUS_CONSOLE_VOLTAGE_HI                  = $4C; // Reading of vConsole (voltage applied to console) [unit:V, factor:0.001]
    REG_BATTERY_STATUS_CONSOLE_VOLTAGE_LO                  = $4D;

    REG_BATTERY_STATUS_12V_VOLTAGE_HI                      = $4E; // Reading of internal 12V [unit:V, factor:0.001]
    REG_BATTERY_STATUS_12V_VOLTAGE_LO                      = $4F;

    REG_BATTERY_CONFIG_NOMINAL_BATTERY_VOLTAGE             = $50; // Battery system nominal voltage

    REG_BATTERY_TIMER_POWER_HI                             = $51; // Time before the power output shuts down [unit:s]
    REG_BATTERY_TIMER_POWER_LO                             = $52;
    REG_BATTERY_TIMER_ACCESSORY_HI                         = $53; // Time before the accessory voltage shuts down [unit:s]
    REG_BATTERY_TIMER_ACCESSORY_LO                         = $54;
    REG_BATTERY_TIMER_PRECHARGE                            = $55; // Time allowed to precharge the motor, before enabling full power [unit:s]
    REG_BATTERY_TIMER_SHUTDOWN_HI                          = $56; // Time of inactivity before the system shuts down [unit:s]
    REG_BATTERY_TIMER_SHUTDOWN_LO                          = $57;

    REG_BATTERY_SN_LOCATION                                = $5B; // location

    REG_BATTERY_STATUS_ACCESSORY_VOLTAGE_HI                = $5E; // Reading of vAccessory [unit:V, factor:0.001]
    REG_BATTERY_STATUS_ACCESSORY_VOLTAGE_LO                = $5F;

    REG_BATTERY_STATUS_CHARGE_LEVEL                        = $61; // Batterylevel [unit:%, factor:6.6667]
    REG_BATTERY_CELLMON_BALANCER_ENABLED                   = $65; //

    REG_BATTERY_STATUS_TEMPERATURE_SENSOR_1                = $66; // [unit:C]
    REG_BATTERY_STATUS_TEMPERATURE_SENSOR_2                = $67; // !!! signed !!!
    REG_BATTERY_STATUS_TEMPERATURE_SENSOR_3                = $68;
    REG_BATTERY_STATUS_TEMPERATURE_SENSOR_4                = $69;

    REG_BATTERY_SN_CELLPACK_HI                             = $6A; // serial number cellpack
    REG_BATTERY_SN_CELLPACK_LO                             = $6B;

    REG_BATTERY_CELLMON_CHANNEL_ADDRESS                    = $6C; // gateway to cell monitor; write address here
    REG_BATTERY_CELLMON_CHANNELDATA_HI                     = $6D; // and read 16 bit voltages here
    REG_BATTERY_CELLMON_CHANNELDATA_LO                     = $6E; // and here

    REG_BATTERY_CELLMON_CALIBRATION_DATA_LO                = $6F; // cell calibration data, select cell via REG_BATTERY_CELLMON_CHANNEL register
                                                                  // since hw 60, sw 103 16 bit values are provided, see REG_BATTERY_CALIBRATION_DATA_HI below
    REG_BATTERY_PROTECT_UNLOCK                             = $71;
      BATTERY_PROTECT_LOCK_KEY                             = $00;
      BATTERY_PROTECT_UNLOCK_KEY                           = $AA;

    REG_BATTERY_SN_YEAR                                    = $72; // mfd. year
    REG_BATTERY_SN_MONTH                                   = $73; // mfd. month
    REG_BATTERY_SN_DAY                                     = $74; // mfd day

    REG_BATTERY_SN_PN_HI                                   = $75; // part number
    REG_BATTERY_SN_PN_LO                                   = $76;
    REG_BATTERY_SN_ITEM_HI                                 = $77; // serial number
    REG_BATTERY_SN_ITEM_LO                                 = $78;

    REG_BATTERY_CELLMON_CALIBRATION_DATA_HI                = $7C; // refer REG_BATTERY_CALIBRATION_DATA_LO above

    REG_BATTERY_STATUS_POWERON_RESET_COUNT                 = $7D; // Return how many time main microcontroller hardly reset

    REG_BATTERY_CONFIG_AUTOSWITCH_COMMUNICATION            = $7E; // ++++ Allow to switch communication mode without shutdown. Write 0xAA, then 0x01. Comm. mode switches. Communicate with desired comm. mode before 5s (100ms min) to validate. Write 0 to desactivate
      BATTERY_CONFIG_AUTOSWITCH_COMMUNICATION_KEY          = $AA;

    REG_BATTERY_BRIGDE_CHARGER_ADDR                        = $85; // gateway to charger; write address here (needs unlocking)
    REG_BATTERY_BRIGDE_CHARGER_DATA                        = $86; // and read data here

    REG_BATTERY_STATUS_LEDS                                = $87; // -

    REG_BATTERY_STATISTIC_CHARGETIME_MEAN_HI               = $8A; // -
    REG_BATTERY_STATISTIC_CHARGETIME_MEAN_LO               = $8B; // -
    REG_BATTERY_STATISTIC_CHARGETIME_WORST_HI              = $8C; // -
    REG_BATTERY_STATISTIC_CHARGETIME_WORST_LO              = $8D; // -

    REG_BATTERY_STATISTIC_BATTERY_CYCLES_HI                = $8E; // - battery charge cycles
    REG_BATTERY_STATISTIC_BATTERY_CYCLES_LO                = $8F; // -

    REG_BATTERY_STATISTIC_RTC_RESYNC                       = $90; // -
    REG_BATTERY_STATISTIC_LMD_ADAPT                        = $91; // -

    REG_BATTERY_STATISTIC_BATTERY_FULL_CYCLES_HI           = $92; // - battery full charge cycles
    REG_BATTERY_STATISTIC_BATTERY_FULL_CYCLES_LO           = $93; // -

    REG_BATTERY_STATISTIC_POWER_CYCLES_HI                  = $96; // Power on cycles
    REG_BATTERY_STATISTIC_POWER_CYCLES_LO                  = $97; // -

    REG_BATTERY_STATISTIC_BATTERY_MAX_VOLTAGE              = $98; // Maximum voltage ever seen by the battery, in percentage of its nominal voltage [unit:%, factor:0.416667, offset:20.8333]
    REG_BATTERY_STATISTIC_BATTERY_MIN_VOLTAGE              = $99; // Minimum voltage ever seen by the battery, in percentage of its nominal voltage [unit:%, factor:0.416667, offset:20.8333]

    REG_BATTERY_STATUS_DCIN_VOLTAGE_HI                     = $9A; // Reading of external power supply voltage [unit:V, factor:0.001]
    REG_BATTERY_STATUS_DCIN_VOLTAGE_LO                     = $9B;

    REG_BATTERY_STATISTIC_TEMPERATURE_MAX                  = $9C; // - !!! signed !!!
    REG_BATTERY_STATISTIC_TEMPERATURE_MIN                  = $9D; // - !!! signed !!!

    REG_BATTERY_STATISTIC_CONTROL_VOLTAGE_SHORTS           = $9E; // until hw 52, now via I2C bridge

    REG_BATTERY_STATISTIC_WATCHDOG_RESET_COUNT             = $9F; // Return how many time watchdog reset trigged
    REG_BATTERY_STATUS_RESET_WDT                           = $9F; // - same as above?

    REG_BATTERY_RTC_CTRL                                   = $A0; // -

    REG_BATTERY_RTC_TIME_HIHI                              = $A1; // Indicates current battery time in seconds. Its value is normally relative to assembly time
    REG_BATTERY_RTC_TIME_HILO                              = $A2;
    REG_BATTERY_RTC_TIME_LOHI                              = $A3;
    REG_BATTERY_RTC_TIME_LOLO                              = $A4;

    REG_BATTERY_RTC_STATUS                                 = $A5; // Describes the status of the RTC: 0-in sync, 1-Write ok, 2-Read ok, 3-Update time, 4-Update ctrl, 5-Osc ok, 7-RTC detected

    REG_BATTERY_STATUS_BATTERY_VOLTAGE_HI                  = $A6; // Reading of vBatt. Return same value as vCell13 [unit:V, factor:0.001]
    REG_BATTERY_STATUS_BATTERY_VOLTAGE_LO                  = $A7;

    REG_BATTERY_STATUS_POWER_VOLTAGE_HI                    = $AA; // Reading of vPower ("High" voltage applied to motor) [unit:V, factor:0.001]
    REG_BATTERY_STATUS_POWER_VOLTAGE_LO                    = $AB;

    REG_BATTERY_STATUS_CONTROL_VOLTAGE_HI                  = $AC; // Reading of vControl (Control voltage applied to motor) [unit:V, factor:0.001]
    REG_BATTERY_STATUS_CONTROL_VOLTAGE_LO                  = $AD;

    REG_BATTERY_CONFIG_PACK_SERIAL                         = $AE; // no of cells in serial
    REG_BATTERY_CONFIG_PACK_PARALLEL                       = $AF; // no of cells in parallel

    REG_BATTERY_GASGAGE_DMFSD                              = $B5; // Digital Magnitude Filter and Self Discharge rate

    REG_BATTERY_CONFIG_ILMD                                = $B9; // - [unit:Ah, factor:0.54835]

    REG_BATTERY_GASGAGE_SOC                                = $BC; // - [unit:%]

    REG_BATTERY_GASGAGE_AI_HI                              = $D3; // - [unit:A, factor:0.002141]
    REG_BATTERY_GASGAGE_AI_LO                              = $D4;

    REG_BATTERY_STATISTIC_LMD_HI                           = $D5; // - [unit:Ah, factor:0.002142]
    REG_BATTERY_STATISTIC_LMD_LO                           = $D6;
    REG_BATTERY_GASGAGE_LMD_HI                             = $D5; // - [unit:Ah, factor:0.002142]
    REG_BATTERY_GASGAGE_LMD_LO                             = $D6;

    REG_BATTERY_CONFIG_NAC_RADDR_HI                        = $DB; // -
    REG_BATTERY_CONFIG_NAC_RADDR_LO                        = $DC;

    REG_BATTERY_GASGAGE_STATUS_FLAGS                       = $DE; // -

    REG_BATTERY_GASGAGE_VOLTAGE_HI                         = $DF; // - [unit:V, factor:0.008]
    REG_BATTERY_GASGAGE_VOLTAGE_LO                         = $E0;

    REG_BATTERY_GASGAGE_TEMPERATUR_HI                      = $E1; // - [unit:C, factor:0.25, offset:-273]
    REG_BATTERY_GASGAGE_TEMPERATUR_LO                      = $E2;

    REG_BATTERY_STATISTIC_GGJR_CALIB                       = $EC; // -

    REG_BATTERY_GASGAGE_VOLTAGE_DIVIDER                    = $ED; // Gas gage external divider value. Indicates how the voltage is divided before reaching the GG. For example, if the battery voltage is 30V and this register is 10, the GG will have 3V at its input [factor:0.1]

    REG_BATTERY_CONFIG_NAC_HI                              = $E5; // -
    REG_BATTERY_CONFIG_NAC_LO                              = $E6;

    REG_BATTERY_PROTECT_MODE                               = $E7; // -
    REG_BATTERY_PROTECT_CONTROL                            = $E8; // -

    REG_BATTERY_STATUS_FLAGS_LO                            = $F0; // Alert status bits: 0-Vctrl (code 20), 1-Precharge (code 21 and 67), 2-Relay (code 22), 3-BMS (code 23), 4-DCDC (code 28), 6-GG out of range temperature, 7-battery pack out of range temperature, 8-balancer overvolt (code 62), 9-Balancer undervolt (code 61), 10-Pack problem (code 63), 11-Accessory overcurrent (code 60), 12-Electronic fuse (code 66), 13-Balancer plug not connected, 14- +5v short(lached)

    REG_BATTERY_RTC_LAST_CHARGE_TIMESTAMP_HIHI             = $F2; // Written on boot-up and shutdown, this register indicates when last minimum 10% charge was completed
    REG_BATTERY_RTC_LAST_CHARGE_TIMESTAMP_HILO             = $F3;
    REG_BATTERY_RTC_LAST_CHARGE_TIMESTAMP_LOHI             = $F4;
    REG_BATTERY_RTC_LAST_CHARGE_TIMESTAMP_LOLO             = $F5;

    REG_BATTERY_STATISTIC_CHARGE_TIMES_CHANNEL             = $F6; // Indicates which charge statistic should be accessed when accessing chargeData. Value can be any value between 1 and 9, 1 being for 10% stat and 9 for 90% stat
    REG_BATTERY_STATISTIC_CHARGE_TIMES_DATA_HI             = $F7;
    REG_BATTERY_STATISTIC_CHARGE_TIMES_DATA_LO             = $F8;

    REG_BATTERY_CONFIG_MAX_CHARGE_HI                       = $F9; // Maximum regen. current on vPower [unit:A, factor:0.001]
    REG_BATTERY_CONFIG_MAX_CHARGE_LO                       = $FA;

    REG_BATTERY_CONFIG_MAX_DISCHARGE_HI                    = $FB; // Maximum drawn current on vPower [unit:A, factor:0.001]
    REG_BATTERY_CONFIG_MAX_DISCHARGE_LO                    = $FC;

    REG_BATTERY_CONFIG_CELLCAPACITY_HI                     = $FD; // - [unit:Ah, factor:0.001]
    REG_BATTERY_CONFIG_CELLCAPACITY_LO                     = $FE;


    REG_BATTERY_BRIDGE_I2C_REGADDR_DEVICE                  = $58; // gateway to I2C registers; write device (highbyte) here,
    REG_BATTERY_BRIDGE_I2C_REGADDR_REGISTER                = $70; // register (lowbyte) here
    REG_BATTERY_BRIDGE_I2C_REGISTER_DATA                   = $60; // and read data here

    // battery i2c bus divices and registers; highbyte:device, lowbyte register

    REG_I2C_CONFIG_CHARGER_VOLTAGE_CALIBRATION_VALUE_HI    = $A007; // Contains voltage calibration value of the charger. This value is written at each charge if bit 0 of _chargerCalibrationSourceFlags is set
    REG_I2C_CONFIG_CHARGER_VOLTAGE_CALIBRATION_VALUE_LO    = $A008;

    REG_I2C_CONFIG_CHARGER_CURRENT_CALIBRATION_VALUE_HI    = $A009; // Contains current calibration value of the charger. This value is written at each charge if bit 1 of _chargerCalibrationSourceFlags is set
    REG_I2C_CONFIG_CHARGER_CURRENT_CALIBRATION_VALUE_LO    = $A00A;

    REG_I2C_CONFIG_CHARGER_CALIBRATION_SOURCE_FLAGS        = $A00B; // Controls charger calibration values source. 0xFF:Autocalibration value, Bit0 set:Voltage calibration value from EEPROM, Bit1 set:Current calibration from EEPROM

    REG_I2C_CALIBRATION_3V3_VOLTAGE_HI                     = $A01C; // SMC6 ADC reference calibration (nominal value = 3V3) [factor:0.001]
    REG_I2C_CALIBRATION_3V3_VOLTAGE_LO                     = $A01D;

    REG_I2C_CALIBRATION_CAPSENSE                           = $A030; // Sensitivity of the capsensing detection. 1 is less sensitive, 10 is most sensitive [range:1..10,factor:-0.2, offset:12]

    REG_I2C_CONFIG_MAX_CELL_DELTA_VOLTAGE                  = $A038; // Delta voltage, between the lowest and the highest cell in the pack, over which the balancer is activated. 0.255 means 0.125 [unit:V, range:25..250, factor:0.001]

    REG_I2C_STATISTIC_LOWVOLTAGE_BUZZER_COUNT_HI           = $A03A; // Number of times the buzzer did emit a sound because of a low battery
    REG_I2C_STATISTIC_LOWVOLTAGE_BUZZER_COUNT_LO           = $A03B;

    REG_I2C_STATISTIC_CELL_DEAD_SHORT_COUNT_HI             = $A04A; // Number of times the charging process detected a cell that is dead short
    REG_I2C_STATISTIC_CELL_DEAD_SHORT_COUNT_LO             = $A04B;

    REG_I2C_STATISTIC_CELL_PARTIAL_SHORT_COUNT_HI          = $A04C; // Number of times the charging process detected a cell that is partially short
    REG_I2C_STATISTIC_CELL_PARTIAL_SHORT_COUNT_LO          = $A04D;

    REG_I2C_STATISTIC_CELLVOLTAGE_COLLAPSE_COUNT_HI        = $A04E; // Number of times the charging process detected a cell for which the voltage collapsed
    REG_I2C_STATISTIC_CELLVOLTAGE_COLLAPSE_COUNT_LO        = $A04F;

    REG_I2C_CONFIG_GASGAGE_TEMPERATURE_MIN                 = $A052; // Temperature of the gaz gauge (board) under which the power gets interrupted. 255 means -30 [unit:C, range:-40..0] !!! signed !!!
    REG_I2C_CONFIG_GASGAGE_TEMPERATURE_MAX                 = $A053; // Temperature of the gaz gauge (board) over which the power gets interrupted. 255 means 85 [unit:C, range:35..100] !!! signed !!!

    REG_I2C_CONFIG_PACK_TEMPERATURE_MIN                    = $A054; // Temperature of the cell pack under which the power gets interrupted. 255 means -30 [unit:C, range:-40..0] !!! signed !!!
    REG_I2C_CONFIG_PACK_TEMPERATURE_MAX                    = $A055; // Temperature of the cell pack over which the power gets interrupted. 255 means 60 [unit:C, range:30..90] !!! signed !!!

    REG_I2C_STATISTIC_CONTROL_VOLTAGE_SHORTS_HI            = $A057; // Number of times the vControl has been shorted
    REG_I2C_STATISTIC_CONTROL_VOLTAGE_SHORTS_LO            = $A058;

    REG_I2C_STATISTIC_5V_VOLTAGE_SHORTS_HI                 = $A059; // Number of times the internal 5.5V has been shorted
    REG_I2C_STATISTIC_5V_VOLTAGE_SHORTS_LO                 = $A05A;

    REG_I2C_CONFIG_DEEPSLEEP_LONG_INACTIVITY_DELAY         = $A05B; // Number of days the battery entered deep sleep after inactivity (Default = 60)
    REG_I2C_CONFIG_DEEPSLEEP_LOW_SOC_DELAY                 = $A05C; // Number of days after which the battery automatically goes in deep sleep when its Soc is low (Default = 7)

    REG_I2C_STATISTIC_DEEPSLEEP_INACTIVITY_COUNT_HI        = $A05D; // Number of times the battery entered deep sleep after X days of inactivity (specified in battery.config.deepSleepAfterLongInactivityPeriodDelay)
    REG_I2C_STATISTIC_DEEPSLEEP_INACTIVITY_COUNT_LO        = $A05E;

    REG_I2C_STATISTIC_DEEPSLEEP_LOW_SOC_COUNT_HI           = $A05F; // Number of times the battery entered deep sleep due to SOC less than 10% during X days (specified in battery.config.deepSleepLowSocDelay)
    REG_I2C_STATISTIC_DEEPSLEEP_LOW_SOC_COUNT_LO           = $A060;

    REG_I2C_STATISTIC_CHARGE_ENERGY_HIHI                   = $A067; // Total energy stored in the battery over its life [ unit:Wh ]
    REG_I2C_STATISTIC_CHARGE_ENERGY_HILO                   = $A068;
    REG_I2C_STATISTIC_CHARGE_ENERGY_LOHI                   = $A069;
    REG_I2C_STATISTIC_CHARGE_ENERGY_LOLO                   = $A06A;

    REG_I2C_STATISTIC_DISCHARGE_ENERGY_HIHI                = $A06B; // Total energy delivered by the battery over its life [ unit:Wh ]
    REG_I2C_STATISTIC_DISCHARGE_ENERGY_HILO                = $A06C;
    REG_I2C_STATISTIC_DISCHARGE_ENERGY_LOHI                = $A06D;
    REG_I2C_STATISTIC_DISCHARGE_ENERGY_LOLO                = $A06E;

    REG_I2C_STATISTIC_DEEPSLEEP_LOW_VOLTAGE_COUNT_HI       = $A08F; // Number of times the battery entered deep sleep due to an average voltage under 2.54V/cell
    REG_I2C_STATISTIC_DEEPSLEEP_LOW_VOLTAGE_COUNT_LO       = $A090;

    REG_I2C_PCBSN_PN_HI                                    = $A09C; // battery pcb part number
    REG_I2C_PCBSN_PN_LO                                    = $A09D;

    REG_I2C_PCBSN_LOCATION                                 = $A0A0; // battery pcb location
    REG_I2C_PCBSN_YEAR                                     = $A099; // battery pcb mfd. year
    REG_I2C_PCBSN_MONTH                                    = $A09A; // battery pcb mfd. month
    REG_I2C_PCBSN_DAY                                      = $A09B; // battery pcb mfd. day

    REG_I2C_PCBSN_ITEM_HI                                  = $A09E; // battery pcb serial number
    REG_I2C_PCBSN_ITEM_LO                                  = $A09F;

    REG_I2C_STATUS_PERMANENT_FAILURE_FLAGS                 = $A0A1; // Having this register set to an non-0xFF or non-zero value indicates this battery shouldn't be used (see /wiki/EepromForTester)
    REG_I2C_STATUS_TEST_FLAGS_HI                           = $A0FD; // Gives battery test information (see /wiki/EepromForTester)
    REG_I2C_STATUS_TEST_FLAGS_LO                           = $A0FE;

    REG_I2C_REV_SUPERVISOR                                 = $A200; // -

    REG_I2C_STATUS_CAPSENSE_HI                             = $A203; // battery test "button" sensitivity
    REG_I2C_STATUS_CAPSENSE_LO                             = $A204;
    REG_I2C_STATUS_CAPSENSE_REFERENCE_HI                   = $A205; // battery test "button" sensitivity reference
    REG_I2C_STATUS_CAPSENSE_REFERENCE_LO                   = $A206;

    // battery cell monitor registers
    REG_CELLMON_CHANNEL_VOLTAGE_1                          = $01; // [unit:V, factor:0.001]
    // ...
    REG_CELLMON_CHANNEL_VOLTAGE_13                         = $0D; // [unit:V, factor:0.001]

    REG_CELLMON_CELL_VOLTAGE_1                             = $81; // [unit:V, factor:0.001]
    // ...                                                        // !!! signed !!!
    REG_CELLMON_CELL_VOLTAGE_13                            = $8D; // [unit:V, factor:0.001]

    REG_CELLMON_STATUS_BOMID_VOLTAGE                       = $C8; // Raw voltage of the resistor divider used to identify the revision of the BOM
    REG_CELLMON_STATUS_PACKID_VOLTAGE                      = $C9; // Raw voltage of the resistor divider used to identify the cell pack
    REG_CELLMON_STATUS_3V3_VOLTAGE                         = $CA; // Internal voltage of the 3.3V derived from the internal 5V [unit:V, factor:0.001]
    REG_CELLMON_STATUS_5V_VOLTAGE                          = $CB; // Internal voltage of the 5(5.5)V derived from the internal 12V [unit:V, factor:0.001]

    REG_CELLMON_CALIBRATION_1                              = $01; // [unit:%, factor:0.03] !!! signed !!!
    // ...                                                        // !!! signed !!!
    REG_CELLMON_CALIBRATION_13                             = $0D; // [unit:%, factor:0.03]

    REG_CHARGER_STATUS_FLAGS_HI                            = $02; // -
    REG_CHARGER_STATUS_FLAGS_LO                            = $03; // -

    REG_CHARGER_MODE                                       = $10; // Indicates the mode of the on-board charger. 0-Low power, 1-Idle, 2-Charging, 3-Accessory output, 5-Calibration, 99-Fault

    REG_CHARGER_FINAL_VOLTAGE_HI                           = $12; // Set final charge voltage [unit:V, factor:0.01]
    REG_CHARGER_FINAL_VOLTAGE_LO                           = $13;

    REG_CHARGER_CURRENT_HI                                 = $14; // Set charge current [unit:A, factor:0.001]
    REG_CHARGER_CURRENT_LO                                 = $15;

    REG_CHARGER_VOLTAGE_CALIBRATION_HI                     = $42; // Multiplier applied on charger voltage reading for its calibration
    REG_CHARGER_VOLTAGE_CALIBRATION_LO                     = $43;

    REG_CHARGER_CURRENT_CALIBRATION_HI                     = $46; // Multiplier applied on charger current reading for its calibration
    REG_CHARGER_CURRENT_CALIBRATION_LO                     = $47;

    REG_CHARGER_REV_CHARGER                                = $56; // -
  {%ENDREGION}

  {%REGION Motor}
  ID_MOTOR                                                 = $20;
//  ID_MOTOR_RESPONSE                                        = $08;
    REG_MOTOR_ASSIST_LEVEL                                 = $09; // [unit:%, range:-100..100, factor:1.5625] !!! signed !!!

    REG_MOTOR_ASSIST_WALK_LEVEL                            = $0A; // Top level when assisting in walk mode [unit:%, factor:1.5625]
    REG_MOTOR_ASSIST_WALK_SPEED_DECREASE_START             = $0B; // Speed from which the motor starts diminishing its assistance when using the "walk mode" [unit:km/h, factor:0.1]
    REG_MOTOR_ASSIST_WALK_SPEED_DECREASE_END               = $0C; // Speed at which the motor gives no more assistance when using the "walk mode" [unit:km/h, factor:0.1]
    REG_MOTOR_ASSIST_WALK_LEVEL_MAX                        = $0D; // Top level when assisting in walk mode [unit:%, factor:1.5625]

    REG_MOTOR_STATUS_SPEED                                 = $11; // - [unit:rpm, factor:9.091]
    REG_MOTOR_STATUS_POWER_METER                           = $14; // - [unit:%, factor:1.5625]

    REG_MOTOR_STATUS_TEMPERATURE                           = $16; // - [unit:C]

    REG_MOTOR_REV_HW                                       = $19; // hardware version
    REG_MOTOR_REV_SW                                       = $20; // software version

    REG_MOTOR_TORQUE_GAUGE_VALUE                           = $21; // - [unit:%, range:0?..100, factor:1.5625]

    REG_MOTOR_REV_SUB                                      = $22; // software subversion

    REG_MOTOR_CONFIG_COMMUNICATION_MODE_LO                 = $36; // - 8 bit until sw 83

    REG_MOTOR_ASSIST_LOWSPEED_RAMP_FLAG                    = $40; // Enables a lower speed ramp. 0: Ramp disabled; 1: Ramp enabled

    REG_MOTOR_ASSIST_DIRECTION                             = $42; // -
    REG_MOTOR_SN_STATOR_TYPE                               = $43; // -
    REG_MOTOR_GEOMETRY_CIRC_HI                             = $44;
    REG_MOTOR_GEOMETRY_CIRC_LO                             = $45;

    REG_MOTOR_TORQUE_GAUGE_POLARITY                        = $46; // -

    REG_MOTOR_STATUS_MAIN                                  = $47; // Indicates the current main status of the motor. 0-Running, 1-NoCommand, 2-Startup, 3-I2CShutOff, 4-AntiBackwardShort, 5-AlarmRegen, 6-AlarmShort, 7-OverSpeedI, 8-OverSpeedV, 9-V12UVP, 10-V12OVP, 11-VPwrUVP, 12-VPwrOVP, 13-OCProtect, 14-BadStatorPN, 15-HallError

    REG_MOTOR_SN_ITEM_HI                                   = $60; // serial number
    REG_MOTOR_SN_ITEM_LO                                   = $61;
    REG_MOTOR_SN_PN_HI                                     = $62; // partnumber
    REG_MOTOR_SN_PN_LO                                     = $63;
    REG_MOTOR_SN_YEAR                                      = $64; // mfd. year
    REG_MOTOR_SN_MONTH                                     = $65; // mfd. month
    REG_MOTOR_SN_DAY                                       = $66; // mfd. day

    REG_MOTOR_SN_OEM_HI                                    = $67; // OEM
    REG_MOTOR_SN_OEM_LO                                    = $68;
    REG_MOTOR_SN_PRODUCT_HI                                = $69; // product
    REG_MOTOR_SN_PRODUCT_LO                                = $6A;
    REG_MOTOR_SN_LOCATION                                  = $6B; // Location

    REG_MOTOR_TORQUE_GAUGE_TYPE                            = $6C; // -

    REG_MOTOR_ASSIST_STATOR_PN_HI                          = $6D; // -
    REG_MOTOR_ASSIST_STATOR_PN_LO                          = $6E; //

    REG_MOTOR_STATUS_POWER_VOLTAGE_HI                      = $70; // - [unit:V, factor:0.001]
    REG_MOTOR_STATUS_POWER_VOLTAGE_LO                      = $71;

    REG_MOTOR_STATUS_12V_VOLTAGE_HI                        = $72; // - [unit:V, factor:0.001]
    REG_MOTOR_STATUS_12V_VOLTAGE_LO                        = $73;

    REG_MOTOR_STATUS_5V_VOLTAGE_HI                         = $74; // - [unit:V, factor:0.001]
    REG_MOTOR_STATUS_5V_VOLTAGE_LO                         = $75;

    REG_MOTOR_STATISTIC_MAX_POWER_VOLTAGE_HI               = $80; // - [unit:V, factor:0.001]
    REG_MOTOR_STATISTIC_MAX_POWER_VOLTAGE_LO               = $81;

    REG_MOTOR_STATISTIC_MAX_TEMPERATURE_HI                 = $82; // -
    REG_MOTOR_STATISTIC_MAX_TEMPERATURE_LO                 = $83;

    REG_MOTOR_STATISTIC_ODOMETER_HI                        = $84; // - [unit:km]
    REG_MOTOR_STATISTIC_ODOMETER_LO                        = $85;

    REG_MOTOR_STATISTIC_CHRONO_HOURS_HI                    = $86; // - [unit:h]
    REG_MOTOR_STATISTIC_CHRONO_HOURS_LO                    = $87;

    REG_MOTOR_STATISTIC_CHRONO_SECONDS_HI                  = $88; // - [unit:s]
    REG_MOTOR_STATISTIC_CHRONO_SECONDS_LO                  = $89;

    REG_MOTOR_PREFERENCE_REGION                            = $8A; // -

    REG_MOTOR_ASSIST_MAXSPEED                              = $8B; // - [unit:km/h]
    REG_MOTOR_ASSIST_DYNAMIC_FLAG                          = $8C; // - [range:0..1]

    REG_MOTOR_CONFIG_PWM_LIMIT_ENABLE                      = $8D; // -

    REG_MOTOR_STATUS_CODES                                 = $92; // Indicates conditions currently detected by motor. Bit 0-Sensor saturation
    REG_MOTOR_STATUS_CODES_LATCH                           = $93; // Indicates conditions detected by motor since its last power up. See bit description of status.codes


    REG_MOTOR_PROTECT_UNLOCK                               = $A5; // unlock register; write UNLOCK_KEY here before setting protected registers
      MOTOR_PROTECT_UNLOCK_KEY                             = $AA;
      MOTOR_PROTECT_LOCK_KEY                               = $00;

    REG_MOTOR_STATISTIC_HALL_DCHS_HI                       = $B0; // -
    REG_MOTOR_STATISTIC_HALL_DCHS_LO                       = $B1;

    REG_MOTOR_STATISTIC_HALL_TRANS_HI                      = $B2; // -
    REG_MOTOR_STATISTIC_HALL_TRANS_LO                      = $B3;

    REG_MOTOR_STATISTIC_HALL_RING_HI                       = $B4; // -
    REG_MOTOR_STATISTIC_HALL_RING_LO                       = $B5;

    REG_MOTOR_STATISTIC_HALL_LOST_HI                       = $B6; // -
    REG_MOTOR_STATISTIC_HALL_LOST_LO                       = $B7;

    REG_MOTOR_TORQUE_GAUGE_NOISE_HI                        = $C4; // - [unit:%, range:0..100, factor:0.0015259]
    REG_MOTOR_TORQUE_GAUGE_NOISE_LO                        = $C5;

    REG_MOTOR_TORQUE_GAUGE_DELAY_HI                        = $C6; // - [unit:s, range:0..?, factor:0.001]
    REG_MOTOR_TORQUE_GAUGE_DELAY_LO                        = $C7;

    REG_MOTOR_TORQUE_GAUGE_SPEED                           = $C8; // - [unit:rpm, range:0..?, factor:9.091]

    REG_MOTOR_TORQUE_GAUGE_VOLTAGE_HI                      = $C9; // - [unit:V, range:0..5, factor:0.000076295, offset:5]
    REG_MOTOR_TORQUE_GAUGE_VOLTAGE_LO                      = $CA;

    REG_MOTOR_TORQUE_GAUGE_REFERENCE_HI                    = $CB; // - [unit:V, range:0..5, factor:0.000076295, offset:5]
    REG_MOTOR_TORQUE_GAUGE_REFERENCE_LO                    = $CC;

    REG_MOTOR_CONFIG_COMMUNICATION_MODE_HI                 = $CD; // Sets the communication mode. 0 for CAN and 0xca01 for I2C

    REG_MOTOR_TORQUE_GAUGE_GAIN                            = $CE; // - [unit:%, range:0..398, factor:1.5625]

    REG_MOTOR_TORQUE_GAUGE_MAX_VOLTAGE                     = $E0; // Maximum voltage allowed for the sensor. When the sensor detect a voltage over this value for motor.torque.gaugeMaxVoltageDelay, it assumes an electrical failure and cuts assistance [unit:V, range:0..5, factor:0.019608]
    REG_MOTOR_TORQUE_GAUGE_MAX_VOLTAGE_DELAY               = $E1; // Time after which a voltage over motor.torque.gaugeMaxVoltage is assumed to be an electrical failure, cutting assistance [unit:s, range:0..25.5, factor:0.1]

    REG_MOTOR_ASSIST_LEVEL_OFFSLOPE_HI                     = $D0; // Speed at which the assist level set in the motor decreases when the console stops sending requests (when it is removed for example) [unit:%/s, factor:3.05]
    REG_MOTOR_ASSIST_LEVEL_OFFSLOPE_LO                     = $D1;

    REG_MOTOR_ASSIST_REGEN_INFLEX                          = $D2; // Speed from which regen is not attenuated [unit:rpm, range:5..?, factor:9.091]

    REG_MOTOR_ASSIST_MAXSPEED_DERATE_DELTA                 = $D3; // Speed before maxSpeed to start derating [unit:rpm, factor:9.091]
  {%ENDREGION}


  {%REGION Sensor}
  ID_SENSOR                                                = $68;
    REG_SENSOR_CONFIG_GAUGE_GAIN_HI                        = $10;
    REG_SENSOR_CONFIG_GAUGE_GAIN_LO                        = $11;

    REG_SENSOR_CONFIG_RAMP_UP_STEPS_HI                     = $12;
    REG_SENSOR_CONFIG_RAMP_UP_STEPS_LO                     = $13;

    REG_SENSOR_CONFIG_DECAY_DELAY_HI                       = $14;
    REG_SENSOR_CONFIG_DECAY_DELAY_LO                       = $15;

    REG_SENSOR_CONFIG_DECAY_STEPS_HI                       = $16;
    REG_SENSOR_CONFIG_DECAY_STEPS_LO                       = $17;

    REG_SENSOR_CONFIG_SPEED_THRESHOLD_HI                   = $18;
    REG_SENSOR_CONFIG_SPEED_THRESHOLD_LO                   = $19;

    REG_SENSOR_CONFIG_RAMP_ACTIVE_OVER_THRESHOLD           = $1A;

    REG_SENSOR_STATUS_TORQUE_VOLTAGE                       = $1B; // Torque sensor voltage

    REG_SENSOR_STATUS_CADENCE                              = $1C; // Number of turns per minutes made with the pedals

    REG_SENSOR_STATUS_OUTPUT_VOLTAGE                       = $1D; // Voltage output to the motor's gauge sensor

    REG_SENSOR_STATUS_PULSE_COUNTER                        = $1E; // Pulse counter. Increases when back pedaling and decreases when forward pedaling

    REG_SENSOR_CONFIG_INPUT_OFFSET                         = $40; // Permit to offset the input value of the torque sensor from +1.64 to -1.65V

    REG_SENSOR_SN_LOCATION                                 = $71; // Location

    REG_SENSOR_SN_YEAR                                     = $72; // mfd. year
    REG_SENSOR_SN_MONTH                                    = $73; // mfd. month
    REG_SENSOR_SN_DAY                                      = $74; // mfd. day

    REG_SENSOR_SN_PN_HI                                    = $75; // partnumber
    REG_SENSOR_SN_PN_LO                                    = $76;

    REG_SENSOR_SN_ITEM_HI                                  = $77; // serial number
    REG_SENSOR_SN_ITEM_LO                                  = $78;

    REG_SENSOR_REV_HW                                      = $80; // hardware version
    REG_SENSOR_REV_SW                                      = $81; // software version

    REG_SENSOR_CONFIG_MODE                                 = $82; // 0-Thune, 1-Fag

    REG_SENSOR_REV_SUB                                     = $83; // software subversion
  {%ENDREGION}

const
  SECONDS_PER_DAY                                          = SecsPerDay; //60 * 60 * 24;

(*
type
  TValueType = ( vt_byte, vt_word );
type
  TBionXRegisterRec = record
    Device : byte;
    Reg         : byte;
    Len         : byte;
    VType       : TValueType;
    Factor      : double;
    Version     : TVersionInfo;
    units       : string[10];
    description : ShortString;
    protect     : byte;
  end;


//const
//  TestRegRec : TBionXRegisterRec =
//    ( Device : $48; Reg : $44; Len : 1; VType : vt_byte; Factor : 1.0; Version : ( sw_since : 0; sw_until : 255 ) );

//  TestRegRec2 : TBionXRegisterRec =
//    ( Device : $48; Reg : $44; Len : 1; VType : vt_byte; Factor : 1.0; Version : allVersions );
*)
(******************************************************************************)

function BoolToStr ( b : boolean ) : string;
begin
  if b then
    Result := 'yes'
  else
    Result := 'no'
end;

function DisplayUnitsToStr ( Units : byte ) : string;
begin
  case Units of
    0 : Result := 'mph';
    1 : Result := 'kmh';
    else
        Result := 'unknown';
  end;
end;


function LightButtonModeToStr ( Mode : byte ) : string;
begin
  case Mode of
    0 : Result := 'Click turns bike off, hold toggles accessory';
    1 : Result := 'Click toggles accessory, hold turns bike off';
    else
        Result := 'unknown';
  end;
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

function ConsoleTypeToStr ( ConsoleType : byte ) : string;
begin
  case ConsoleType of
    0 : Result := 'EPS';
    1 : Result := 'RIDE+';
    2 : Result := 'Boost';
   else
        Result := 'unknown';
  end;
end;

function BatteryCommunicationModeToStr ( Mode : byte ) : string;
begin
  case Mode of
    1 : Result := 'I2C';
    2 : Result := 'CAN';
   else
        Result := 'unknown';
  end;
end;

function BOMToStr ( BOM : byte ) : string;
begin
  case BOM of
    1 : Result := 'SMC 6.2 or SMC 6.3r1';
    8 : Result := 'SMC 6.3r4';
   else
        Result := 'unknown';
  end;
end;

function ChargerModeToStr ( Mode : byte  ) : string;
begin
  case Mode of
    0 : Result := 'Low power';
    1 : Result := 'Idle';
    2 : Result := 'Charging';
    3 : Result := 'Accessory output';
    5 : Result := 'Calibration';
   99 : Result := 'Fault';
   else
     Result := 'unknown';
  end;
end;

function ChargerStatusToStr ( Status : byte ) : string;
begin
  case Status of
    0 : Result := 'Off';
    1 : Result := 'Stand-by';
    2 : Result := 'Charger';
    3 : Result := 'Accessory';
    4 : Result := 'Vdcin sense';
    5 : Result := 'Overtemp';
    6 : Result := 'Charge done';
    7 : Result := 'Buck failed';
   else
        Result := Format ( 'unknown (0x%0.2x)', [Status] );
  end;
end;

function CapSenseSOCModeToStr ( Mode : byte ) : string;
begin
  case Mode of
    0 : Result := 'Inactive';
    1 : Result := 'Touch detect when the battery is OFF';
    2 : Result := 'Touch detect when OFF and SoC indication when battery is ON';
    3 : Result := 'red and blue colors';
    4 : Result := '5 levels SoC';
   else
        Result := 'unknown';
  end;
end;

function RTCStatusToStr ( Status : byte ) : string;
var
  i    : integer;
  mask : byte;

  function BitString ( bit : byte ) : string;
  begin
    case Bit of
      0 : Result := 'in sync';
      1 : Result := 'Write ok';
      2 : Result := 'Read ok';
      3 : Result := 'Update time';
      4 : Result := 'Update ctrl';
      5 : Result := 'Osc ok';
      7 : Result := 'RTC detected';
    end;
  end;

begin
  Result := '';
  mask := 1;
  for i := 0 to 7 do
  begin
    if ( Status and mask ) = mask then
      if Result = '' then
        Result := BitString(i)
      else
        Result := Result + ', ' + BitString(i);
    mask := mask shl 1;
  end;
  Result := Format ( '0x%0.2x (%s)', [ Status, Result ] );
end;

function MotorCommunicationModeToStr ( Mode : word ) : string;
begin
  case Mode of
    $0000 : Result := 'CAN';
    $CA01 : Result := 'I2C';
   else
            Result := 'unknown';
  end;
end;

function MotorStatusToStr ( Status : byte ) : string;
begin
  case Status of
    0 : Result := 'Running';
    1 : Result := 'NoCommand';
    2 : Result := 'Startup';
    3 : Result := 'I2CShutOff';
    4 : Result := 'AntiBackwardShort';
    5 : Result := 'AlarmRegen';
    6 : Result := 'AlarmShort';
    7 : Result := 'OverSpeedI';
    8 : Result := 'OverSpeedV';
    9 : Result := 'V12UVP';
   10 : Result := 'V12OVP';
   11 : Result := 'VPwrUVP';
   12 : Result := 'VPwrOVP';
   13 : Result := 'OCProtect';
   14 : Result := 'BadStatorPN';
   15 : Result := 'HallError';
   else
        Result := Format ( 'unknown (0x%0.2x)', [Status] );
  end;
end;

function BatteryFlagsToStr ( Flags : word ) : string;
var
  i    : integer;
  mask : word;

  function BitString ( bit : word ) : string;
  begin
    case Bit of
       0 : Result := 'Vctrl (code 20)';
       1 : Result := 'Precharge (code 21 and 67)';
       2 : Result := 'Relay (code 22)';
       3 : Result := 'BMS (code 23)';
       4 : Result := 'DCDC (code 28)';
       6 : Result := 'GG out of range temperature';
       7 : Result := 'Battery pack out of range temperature';
       8 : Result := 'Balancer overvolt (code 62)';
       9 : Result := 'Balancer undervolt (code 61)';
      10 : Result := 'Pack problem (code 63)';
      11 : Result := 'Accessory overcurrent (code 60)';
      12 : Result := 'Electronic fuse (code 66)';
      13 : Result := 'Balancer plug not connected';
      14 : Result := '+5v short(lached)';
    end;
  end;

begin
  Result := '';
  mask := 1;
  for i := 0 to 15 do
  begin
    if ( Flags and mask ) = mask then
      if Result = '' then
        Result := BitString(i)
      else
        Result := Result + ', ' + BitString(i);
    mask := mask shl 1;
  end;
  Result := Format ( '0x%0.4x (%s)', [ Flags, Result ] );
end;

function SensorModeToStr ( Mode : byte ) : string;
begin
  case Mode of
    0 : Result := 'Thune';
    1 : Result := 'Fag';
   else
        Result := Format ( 'unknown (0x%0.2x)', [Mode] );
  end;
end;

function ConsoleCodeBitString ( bit : word ) : string;
begin
  case Bit of
     0 : Result := 'Wheel circumference (3771+2005)';
     1 : Result := 'Diagnostic mode (3772)';
     2 : Result := 'Max speed (3773)';
     3 : Result := 'Overvoltage protection (3774)';
     4 : Result := 'Max throttle speed (3775)';
     5 : Result := 'Minimum assist speed (3776)';
     6 : Result := 'Motor direction (1976)';
     7 : Result := 'Deprecated (5000)';
     8 : Result := 'metric vs imperial (2001)';
     9 : Result := 'Regen value (2002)';
    10 : Result := 'Remaining distance (2003)';

    11 : Result := 'Clock time (2004)';
    12 : Result := 'Brake switch config (2006)';
    13 : Result := 'Throttle polarity (2007)';
    14 : Result := 'Accessory voltage (2008)';
    15 : Result := 'Slave console (0041)';
    16 : Result := 'Filter (1234)';
    17 : Result := 'Light sensor (1970)';
    18 : Result := 'Gauge gain (0007)';
    19 : Result := 'Assistance gain (0008)';
    20 : Result := 'Gauge joint (0009)';

    21 : Result := 'Deprecated (0911)';
    22 : Result := 'Console info (0001)';
    23 : Result := 'Battery info (0002)';
    24 : Result := 'Motor info (0003)';
    25 : Result := 'Battery statistics (6000)';
    26 : Result := 'Speed gain (0006)';
    27 : Result := 'Alarm';
    28 : Result := 'Time / Mountain mode';
    else Result := 'unused';
  end;
end;

function ConsoleCodesToStr ( Code : longword ) : string;
var
  i    : integer;
  mask : longword;

begin
  Result := '';
  mask := 1;
  for i := 0 to 31 do
  begin
    if ( Code and mask ) = mask then
      if Result = '' then
        Result := ConsoleCodeBitString(i)
      else
        Result := Result + ', ' + ConsoleCodeBitString(i);
    mask := mask shl 1;
  end;
  Result := Format ( '0x%0.8x (%s)', [ Code, Result ] );
end;

(******************************************************************************)

function GetDeviceName( CANId : byte ) : string;
begin
  case CANId of
    ID_CONSOLE_MASTER,
    ID_CONSOLE_SLAVE :
      Result := 'console';
    ID_BATTERY :
      Result := 'battery';
    ID_MOTOR :
      Result := 'motor';
    ID_SENSOR :
      Result := 'sensor';
    ID_BIB :
      Result := 'bib';
    else
      Result := 'Id 0x'+IntToHex(CanId,2);
  end;
end;

function _GetCANIdName( CANId : byte ) : string;
begin
  case CANId of
    ID_CONSOLE_MASTER :
      Result := 'ConM';
    ID_CONSOLE_SLAVE :
      Result := 'ConS';
    ID_BATTERY :
      Result := 'Batt';
    ID_MOTOR :
      Result := 'Mot';
    ID_SENSOR :
      Result := 'Sens';
    ID_BIB :
      Result := 'BIB';
    else
      Result := '0x'+IntToHex(CanId,2);
  end;
end;

procedure VersionError;
begin
  Raise VersionException.Create('N/A in this version');
end;

procedure Untested;
begin
  if MessageDlg ( 'ooops', 'untested set method'#13'Perform write operation?', mtConfirmation, [mbYes,mbNo], 0, mbNo ) = mrNo then
    abort;
end;

(******************************************************************************)
(******************************************************************************)
(*                                                                            *)
(*                                                                            *)
(* TBionXComponent                                                            *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
(******************************************************************************)

constructor TBionXComponent.Create ( CANIntf : PCANInterface; ACANId : byte );
begin
  inherited Create;
  FCANId := ACANId;
  FCANIntf := CANIntf;
  FHardwareVersion := 0;
  FSoftwareVersion := 0;
  FSubVersion      := 0;
end;

function TBionXComponent.GetByteValue ( Reg : byte ) : byte;
begin
  if not FCANIntf^.ReadByte ( FCANId, Reg, Result ) then
    raise  ECANError.Create ( FCANIntf^.LastError );
end;

procedure TBionXComponent.SetByteValue ( Reg : byte; Value : byte );
begin
  if not FCANIntf^.WriteByte ( FCANId, Reg, Value ) then
    raise  ECANError.Create ( FCANIntf^.LastError );
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
  SetByteValue( RegHIGH+0, Value shr 24 );
  SetByteValue( RegHIGH+1, Value shr 16 );
  SetByteValue( RegHIGH+2, Value shr  8 );
  SetByteValue( RegHIGH+3, Value and $ff );
end;

function TBionXComponent.GetDWordValueR ( RegHIGH : byte ) : dword;
begin
  // used by console codes, codesrw
  Result :=   ( GetByteValue( RegHIGH+3 ) shl 24 )
            + ( GetByteValue( RegHIGH+2 ) shl 16 )
            + ( GetByteValue( RegHIGH+1 ) shl  8 )
            +   GetByteValue( RegHIGH+0 );
end;

procedure TBionXComponent.SetDWordValueR ( RegHIGH : byte; Value : dword );
begin
  // used by console odometer set proc
  SetByteValue( RegHIGH+3, Value shr 24 );
  SetByteValue( RegHIGH+2, Value shr 16 );
  SetByteValue( RegHIGH+1, Value shr  8 );
  SetByteValue( RegHIGH+0, Value and $ff );
end;

function TBionXComponent.CheckVersion ( sw_since, sw_until, sub_since, sub_until, hw_since, hw_until : byte ) : boolean;
begin
  Result := ( SoftwareVersion >= sw_since )  and ( SoftwareVersion <= sw_until ) and
            ( SubVersion      >= sub_since ) and ( SubVersion      <= sub_until ) and
            ( HardwareVersion >= hw_since )  and ( HardwareVersion <= hw_until );
end;

procedure TBionXComponent.CheckDeviceReady;
begin
  try
    if SoftwareVersion = 0 then
      abort; // jump into the exception handling below
  except
    raise Exception.CreateFmt ( 'device "%s" not responding', [ GetDeviceName( FCANId ) ] );
  end;
end;

(******************************************************************************)
(******************************************************************************)
(*                                                                            *)
(*                                                                            *)
(* TBionXConsole                                                              *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
(******************************************************************************)

function TBionXConsole.GetSoftwareVersion: byte;
begin
  if FSoftwareVersion = 0 then
    FSoftwareVersion := GetByteValue( REG_CONSOLE_REV_SW );
  Result := FSoftwareVersion;
end;

function TBionXConsole.GetSubVersion: byte;
begin
  if FSubVersion = 0 then
    // !! do NOT use CheckVersion here as that will cause recursion !!
    if SoftwareVersion >= 59 then
      FSubVersion := GetByteValue( REG_CONSOLE_REV_SUB )
    else
      FSubVersion := 0;
  Result := FSubVersion;
end;

function TBionXConsole.GetHardwareVersion: byte;
begin
  if FHardwareVersion = 0 then
    // !! do NOT use CheckVersion here as that will cause recursion !!
    if SoftwareVersion >= 46 then
      FHardwareVersion := GetByteValue( REG_CONSOLE_REV_HW )
    else
      FHardwareVersion := 0;
  Result := FHardwareVersion;
end;

function TBionXConsole.GetPartNumber : word;
begin
  Result := GetWordValue( REG_CONSOLE_SN_PN_HI );
end;

function TBionXConsole.GetLocation : byte;
begin
  Result := GetByteValue ( REG_CONSOLE_SN_LOCATION );
end;

function TBionXConsole.GetManufacturingDate : TDate;
begin
  Result := EncodeDate ( GetByteValue ( REG_CONSOLE_SN_YEAR ) + 2000,
                         GetByteValue ( REG_CONSOLE_SN_MONTH ),
                         GetByteValue ( REG_CONSOLE_SN_DAY )
                       );
end;

function TBionXConsole.GetItemNumber : word;
begin
  Result := GetWordValue( REG_CONSOLE_SN_ITEM_HI );
end;

function TBionXConsole.GetOEM : word;
begin
  if CheckVersion ( 43, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_CONSOLE_SN_OEM_HI )
  else
    Result := 0;
end;

function TBionXConsole.GetProduct : word;
begin
  if CheckVersion ( 43, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_CONSOLE_SN_PRODUCT_HI )
  else
    Result := 0;
end;

function TBionXConsole.GetConsoleType : byte;
begin
  Result := GetByteValue ( REG_CONSOLE_SN_TYPE );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetGeometryCirc: word;
begin
  Result := GetWordValue( REG_CONSOLE_GEOMETRY_CIRC_HI );
end;

procedure TBionXConsole.SetGeometryCirc ( AValue: word ) ;
begin
  SetWordValue( REG_CONSOLE_GEOMETRY_CIRC_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetConfigTestMode: boolean;
begin
  if CheckVersion ( 56, 255, 0, 255, 12, 12 ) or
     CheckVersion ( 56, 255, 0, 255, 15, 255 ) then
    Result := GetBoolValue ( REG_CONSOLE_CONFIG_TESTMODE )
  else
    if CheckVersion ( 56, 255, 0, 255, 14, 14 ) then
      Result := GetBoolValue ( REG_CONSOLE_CONFIG_TESTMODE_HW14 )
    else
      Result := false;
end;

procedure TBionXConsole.SetConfigTestMode ( AValue : boolean ) ;
begin
  if CheckVersion ( 56, 255, 0, 255, 12, 12 ) or
     CheckVersion ( 56, 255, 0, 255, 15, 255 ) then
    SetBoolValue ( REG_CONSOLE_CONFIG_TESTMODE, AValue )
  else
    if CheckVersion ( 56, 255, 0, 255, 14, 14 ) then
      SetBoolValue ( REG_CONSOLE_CONFIG_TESTMODE_HW14, AValue )
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetConfigServiceTimeStamp: word;
begin
  if CheckVersion ( 60, 255, 0, 255, 15, 255 ) then
    Result := GetWordValue ( REG_CONSOLE_CONFIG_SERVICE_TIMESTAMP_HI )
  else
    Result := 0;
end;

procedure TBionXConsole.SetConfigServiceTimeStamp ( AValue : word ) ;
begin
  if CheckVersion ( 60, 255, 0, 255, 15, 255 ) then
    SetWordValue ( REG_CONSOLE_CONFIG_SERVICE_TIMESTAMP_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetConfigServiceDistance: word;
begin
  if CheckVersion ( 60, 255, 0, 255, 15, 255 ) then
    Result := GetWordValue ( REG_CONSOLE_CONFIG_SERVICE_DISTANCE_HI )
  else
    Result := 0;
end;

procedure TBionXConsole.SetConfigServiceDistance ( AValue : word ) ;
begin
  if CheckVersion ( 60, 255, 0, 255, 15, 255 ) then
    SetWordValue ( REG_CONSOLE_CONFIG_SERVICE_DISTANCE_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

// LastMode:
// Writing 255 will disable the last mode on display power on
//           0 will enable the last mode on display power on
// ---> this is not a usual boolean register
function TBionXConsole.GetConfigLastMode: boolean;
begin
  if CheckVersion ( 62, 255, 0, 255, 15, 15 ) then
    Result := GetByteValue ( REG_CONSOLE_CONFIG_LAST_MODE )=CONSOLE_CONFIG_LAST_MODE_ON
  else
    Result := false;
end;

procedure TBionXConsole.SetConfigLastMode ( AValue: boolean ) ;
begin
  if CheckVersion ( 62, 255, 0, 255, 15, 15 ) then
    if AValue then
      SetByteValue ( REG_CONSOLE_CONFIG_LAST_MODE, CONSOLE_CONFIG_LAST_MODE_ON )
    else
      SetByteValue ( REG_CONSOLE_CONFIG_LAST_MODE, CONSOLE_CONFIG_LAST_MODE_OFF );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetStatusSlave: boolean;
begin
// bugfix connect error "could not read register A3 from node 48"
// we should not try to read registers before the bus isn't in slave mode
//  if CheckVersion ( 48, 255, 0, 255, 0, 255 ) then
//    Result := GetBoolValue( REG_CONSOLE_STATUS_SLAVE )
//  else
//    Result := false;
  Result := GetBoolValue( REG_CONSOLE_STATUS_SLAVE )
end;

procedure TBionXConsole.SetStatusSlave ( AValue: boolean ) ;
begin
// bugfix connect error "could not read register A3 from node 48"
// we should not try to read registers before the bus isn't in slave mode
//  if CheckVersion ( 48, 255, 0, 255, 0, 255 ) then
    SetBoolValue( REG_CONSOLE_STATUS_SLAVE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistMaxSpeedFlag: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_ASSIST_MAXSPEED_FLAG );
end;

procedure TBionXConsole.SetAssistMaxSpeedFlag(AValue: boolean);
begin
  SetBoolValue ( REG_CONSOLE_ASSIST_MAXSPEED_FLAG, AValue );
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
  Result := GetBoolValue ( REG_CONSOLE_ASSIST_MINSPEED_FLAG );
end;

procedure TBionXConsole.SetAssistMinSpeedFlag(AValue: boolean);
begin
  SetBoolValue ( REG_CONSOLE_ASSIST_MINSPEED_FLAG, AValue );
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

function TBionXConsole.GetAssistBrakeLevel: double;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_BRAKE_LEVEL ) * ASSIST_FACTOR;
end;

procedure TBionXConsole.SetAssistBrakeLevel ( AValue: double ) ;
begin
  if ( AValue >= 0 ) and ( AValue <= 62.5 ) then
    SetByteValue( REG_CONSOLE_ASSIST_BRAKE_LEVEL, round ( AValue / ASSIST_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistBrakeFlag: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_ASSIST_BRAKE_FLAG );
end;

procedure TBionXConsole.SetAssistBrakeFlag ( AValue: boolean ) ;
begin
  SetBoolValue ( REG_CONSOLE_ASSIST_BRAKE_FLAG, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistAutoRegen: boolean;
begin
  if CheckVersion ( 50, 255, 0, 255, 0, 255 ) then
    Result := GetBoolValue ( REG_CONSOLE_ASSIST_AUTOREGEN_FLAG )
  else
    Result := false;
end;

procedure TBionXConsole.SetAssistAutoRegen ( AValue : boolean ) ;
begin
  if CheckVersion ( 50, 255, 0, 255, 0, 255 ) then
    SetBoolValue ( REG_CONSOLE_ASSIST_AUTOREGEN_FLAG, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistBrakePolarity: byte;
begin
  Result := GetByteValue ( REG_CONSOLE_ASSIST_BRAKE_POLARITY );
end;

procedure TBionXConsole.SetAssistBrakePolarity ( AValue: byte ) ;
begin
  SetByteValue ( REG_CONSOLE_ASSIST_BRAKE_POLARITY, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistGaugeFilter: byte;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_GAUGE_FILTER );
end;

procedure TBionXConsole.SetAssistGaugeFilter ( AValue: byte ) ;
begin
  // the BionX.xml shows a range (1..4)
  // the console allows (1..8)
  if ( AValue > 0 ) and ( AValue <= 8 ) then
    SetByteValue( REG_CONSOLE_ASSIST_GAUGE_FILTER, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistGaugeGain: double;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_GAUGE_GAIN ) * SENESORGAIN_FACTOR;
end;

procedure TBionXConsole.SetAssistGaugeGain ( AValue: double ) ;
begin
  if ( AValue > 0 ) and ( AValue <= 4 ) then
    SetByteValue( REG_CONSOLE_ASSIST_GAUGE_GAIN, trunc ( AValue / SENESORGAIN_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistGaugeGainA: double;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_GAIN_A ) * SENESORGAIN_FACTOR;
end;

procedure TBionXConsole.SetAssistGaugeGainA ( AValue: double ) ;
begin
  SetByteValue( REG_CONSOLE_ASSIST_GAIN_A, round( AValue / SENESORGAIN_FACTOR ));
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistGaugeGainB: double;
begin
  Result := GetByteValue( REG_CONSOLE_ASSIST_GAIN_B ) * SPEED_FACTOR;
end;

procedure TBionXConsole.SetAssistGaugeGainB ( AValue: double ) ;
begin
  SetByteValue( REG_CONSOLE_ASSIST_GAIN_B, round ( AValue / SPEED_FACTOR ));
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistSpeedGain: double;
begin
  if CheckVersion ( 43, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue( REG_CONSOLE_ASSIST_SPEEDGAIN ) * SPEED_FACTOR;
end;

procedure TBionXConsole.SetAssistSpeedGain ( AValue: double ) ;
begin
  if CheckVersion ( 43, 255, 0, 255, 0, 255 ) then
    SetByteValue( REG_CONSOLE_ASSIST_SPEEDGAIN, round ( AValue / SPEED_FACTOR ));
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistGaugeJoint: byte;
begin
  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_CONSOLE_ASSIST_GAUGE_JOINT )
  else
    Result := 0;
end;

procedure TBionXConsole.SetAssistGaugeJoint ( AValue: byte ) ;
begin
  if ( AValue >= 0 ) and ( AValue <= 11 ) and
     CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_CONSOLE_ASSIST_GAUGE_JOINT, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevel1: double;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue( REG_CONSOLE_ASSIST_LEVEL_1 ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXConsole.SetAssistLevel1 ( AValue: double ) ;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    SetByteValue( REG_CONSOLE_ASSIST_LEVEL_1, round(AValue / ASSIST_FACTOR) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevel2: double;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue( REG_CONSOLE_ASSIST_LEVEL_2 ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXConsole.SetAssistLevel2 ( AValue: double ) ;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    SetByteValue( REG_CONSOLE_ASSIST_LEVEL_2, round(AValue / ASSIST_FACTOR) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevel3: double;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue( REG_CONSOLE_ASSIST_LEVEL_3 ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXConsole.SetAssistLevel3 ( AValue: double ) ;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    SetByteValue( REG_CONSOLE_ASSIST_LEVEL_3, round(AValue / ASSIST_FACTOR) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevel4: double;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue( REG_CONSOLE_ASSIST_LEVEL_4 ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXConsole.SetAssistLevel4 ( AValue: double ) ;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    SetByteValue( REG_CONSOLE_ASSIST_LEVEL_4, round(AValue / ASSIST_FACTOR) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevelR1: double;
begin
  if CheckVersion ( 60, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_1 ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXConsole.SetAssistLevelR1 ( AValue: double ) ;
begin
  if ( AValue >= 0 ) and ( AValue <= 62.5 ) and
     CheckVersion ( 60, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_1, round ( AValue / ASSIST_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevelR2: double;
begin
  if CheckVersion ( 60, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_2 ) * ASSIST_FACTOR
  else
    Result := 0;
//  VersionError;
end;

procedure TBionXConsole.SetAssistLevelR2 ( AValue: double ) ;
begin
  if ( AValue >= 0 ) and ( AValue <= 62.5 ) and
     CheckVersion ( 60, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_2, round ( AValue / ASSIST_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevelR3: double;
begin
  if CheckVersion ( 60, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_3 ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXConsole.SetAssistLevelR3 ( AValue: double ) ;
begin
  if ( AValue >= 0 ) and ( AValue <= 62.5 ) and
     CheckVersion ( 60, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_3, round ( AValue / ASSIST_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistLevelR4: double;
begin
  if CheckVersion ( 60, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_4 ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXConsole.SetAssistLevelR4 ( AValue: double ) ;
begin
  if ( AValue >= 0 ) and ( AValue <= 62.5 ) and
     CheckVersion ( 60, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_CONSOLE_ASSIST_LEVEL_REKUPERATION_4, round ( AValue / ASSIST_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistInitLevel: byte;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_CONSOLE_ASSIST_INITLEVEL )
  else
    Result := 0;
end;

procedure TBionXConsole.SetAssistInitLevel ( AValue: byte ) ;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) and
     ( AValue >= 0 ) and ( AValue <= 4 ) then
    SetByteValue ( REG_CONSOLE_ASSIST_INITLEVEL, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetAssistMountainCap: double;
begin
// error in bionx.xml???
// user reports read-error with G1 console, HW 12
//  if CheckVersion ( 59, 255, 0, 255, 0, 255 ) then

// 0.74pre
//  if CheckVersion ( 59, 255, 0, 255, 13, 255 ) then

// Korrektur nach neuem bionx.xml
  if CheckVersion ( 60, 255, 0, 255, 12, 15 ) or
     CheckVersion ( 59, 255, 0, 255, 15, 16 ) or
     CheckVersion ( 60, 255, 0, 255, 16, 22 ) or
     CheckVersion ( 60, 255, 0, 255, 23, 27 ) or
     CheckVersion ( 59, 255, 0, 255, 27, 255 ) then
    Result := GetByteValue( REG_CONSOLE_ASSIST_MOUNTAIN_CAP ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXConsole.SetAssistMountainCap ( AValue: double ) ;
begin
  if ( AValue > 0 ) and ( AValue <= 100 ) and
// error in bionx.xml???
// user reports read-error with G1 console, HW 12
//     CheckVersion ( 59, 255, 0, 255, 0, 255 ) then

// 0.74pre
//  if CheckVersion ( 59, 255, 0, 255, 13, 255 ) then

// Korrektur nach neuem bionx.xml
     CheckVersion ( 60, 255, 0, 255, 12, 15 ) or
     CheckVersion ( 59, 255, 0, 255, 15, 16 ) or
     CheckVersion ( 60, 255, 0, 255, 16, 22 ) or
     CheckVersion ( 60, 255, 0, 255, 23, 27 ) or
     CheckVersion ( 59, 255, 0, 255, 27, 255 ) then
    SetByteValue( REG_CONSOLE_ASSIST_MOUNTAIN_CAP, round(AValue / ASSIST_FACTOR) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetThrottleMaxSpeedFlag: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_THROTTLE_MAXSPEED_FLAG );
end;

procedure TBionXConsole.SetThrottleMaxSpeedFlag(AValue: boolean);
begin
  SetBoolValue ( REG_CONSOLE_THROTTLE_MAXSPEED_FLAG, AValue );
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

function TBionXConsole.GetThrottleEnabledOnStrain: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_THROTTLE_ENABLE_ONSTRAIN )
end;

procedure TBionXConsole.SetThrottleEnabledOnStrain ( AValue: boolean ) ;
begin
  SetBoolValue ( REG_CONSOLE_THROTTLE_ENABLE_ONSTRAIN, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetThrottleEnableBoostDisplay: boolean;
begin
  if CheckVersion ( 54, 255, 0, 255, 15, 255 ) then
    Result := GetBoolValue ( REG_CONSOLE_THROTTLE_BOOST_TRIGGERLEVEL )
  else
    Result := false;
end;

procedure TBionXConsole.SetThrottleEnableBoostDisplay ( AValue: boolean ) ;
begin
  if CheckVersion ( 54, 255, 0, 255, 15, 255 ) then
    SetBoolValue ( REG_CONSOLE_THROTTLE_BOOST_TRIGGERLEVEL, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetThrottleBoostTrigerLevel: double;
begin
  if CheckVersion ( 46, 255, 0, 255, 15, 16 ) then
    Result := GetByteValue ( REG_CONSOLE_THROTTLE_BOOST_TRIGGERLEVEL ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXConsole.SetThrottleBoostTrigerLevel ( AValue: double ) ;
begin
  if ( AValue >= 1.5 ) and ( AValue <= 50 ) and
     CheckVersion ( 46, 255, 0, 255, 15, 16 ) then
    SetByteValue ( REG_CONSOLE_THROTTLE_BOOST_TRIGGERLEVEL, round ( AValue / ASSIST_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetThrottleCalibrated: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_THROTTLE_CALIBRATED );
end;

procedure TBionXConsole.SetThrottleCalibrated ( AValue : boolean ) ;
begin
  SetBoolValue ( REG_CONSOLE_THROTTLE_CALIBRATED, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetThrottlePosition: double;
begin
//  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
  // Korrektur in neuer bionx.xml
  if CheckVersion ( 47, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_CONSOLE_THROTTLE_POSITION ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXConsole.SetThrottlePosition ( AValue : double ) ;
begin
//  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
  // Korrektur in neuer bionx.xml
  if CheckVersion ( 47, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_CONSOLE_THROTTLE_POSITION, round ( AValue / ASSIST_FACTOR ));
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetThrottleRaw: word;
begin
//  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
  // Korrektur in neuer bionx.xml
  if CheckVersion ( 47, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_CONSOLE_THROTTLE_RAW_HI )
  else
    Result := 0;
end;

procedure TBionXConsole.SetThrottleRaw ( AValue : word ) ;
begin
//  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
  // Korrektur in neuer bionx.xml
  if CheckVersion ( 47, 255, 0, 255, 0, 255 ) then
    SetWordValue ( REG_CONSOLE_THROTTLE_RAW_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetThrottleMin: word;
begin
  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_CONSOLE_THROTTLE_MIN_HI )
  else
    Result := 0;
end;

procedure TBionXConsole.SetThrottleMin ( AValue : word ) ;
begin
  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
    SetWordValue ( REG_CONSOLE_THROTTLE_MIN_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetThrottleMax: word;
begin
  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_CONSOLE_THROTTLE_MAX_HI )
  else
    Result := 0;
end;

procedure TBionXConsole.SetThrottleMax ( AValue : word ) ;
begin
  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
    SetWordValue ( REG_CONSOLE_THROTTLE_MAX_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceTripToEmptyFlag: boolean;
begin
  Result := GetBoolValue ( REG_CONSOLE_PREFERENCE_TRIP_TO_EMPTY_FLAG );
end;

procedure TBionXConsole.SetPreferenceTripToEmptyFlag ( AValue : boolean ) ;
begin
  SetBoolValue ( REG_CONSOLE_PREFERENCE_TRIP_TO_EMPTY_FLAG, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceDisplayUnits: byte;
begin
  Result := GetByteValue ( REG_CONSOLE_PREFERENCE_DISPLAY_UNITS );
end;

procedure TBionXConsole.SetPreferenceDisplayUnits ( AValue: byte ) ;
begin
  SetByteValue ( REG_CONSOLE_PREFERENCE_DISPLAY_UNITS, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceNIP: string;

begin
  Result :=   Char ( GetByteValue ( REG_CONSOLE_PREFERENCE_NIP_HIHI ) )
            + Char ( GetByteValue ( REG_CONSOLE_PREFERENCE_NIP_HILO ) )
            + Char ( GetByteValue ( REG_CONSOLE_PREFERENCE_NIP_LOHI ) )
            + Char ( GetByteValue ( REG_CONSOLE_PREFERENCE_NIP_LOLO ) );
  Result := Trim(Result);
end;

procedure TBionXConsole.SetPreferenceNIP ( const AValue : string ) ;
begin
  if length ( AValue ) > 0 then
    SetByteValue ( REG_CONSOLE_PREFERENCE_NIP_HIHI, ord(AValue[1]) )
  else
    SetByteValue ( REG_CONSOLE_PREFERENCE_NIP_HIHI, 0 );
  if length ( AValue ) > 1 then
    SetByteValue ( REG_CONSOLE_PREFERENCE_NIP_HILO, ord(AValue[2]) )
  else
    SetByteValue ( REG_CONSOLE_PREFERENCE_NIP_HIHI, 0 );
  if length ( AValue ) > 2 then
    SetByteValue ( REG_CONSOLE_PREFERENCE_NIP_LOHI, ord(AValue[3]) )
  else
    SetByteValue ( REG_CONSOLE_PREFERENCE_NIP_HIHI, 0 );
  if length ( AValue ) > 3 then
    SetByteValue ( REG_CONSOLE_PREFERENCE_NIP_LOLO, ord(AValue[4]) )
  else
    SetByteValue ( REG_CONSOLE_PREFERENCE_NIP_HIHI, 0 );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceLCDContrast: byte;
begin
  Result := GetByteValue ( REG_CONSOLE_PREFERENCE_LCD_CONTRAST );
end;

procedure TBionXConsole.SetPreferenceLCDContrast ( AValue: byte ) ;
begin
  SetByteValue ( REG_CONSOLE_PREFERENCE_LCD_CONTRAST, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceCodes: dword;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    Result := GetDWordValueR ( REG_CONSOLE_PREFERENCE_CODES_HIHI )
  else
    Result := 0;
end;

procedure TBionXConsole.SetPreferenceCodes ( AValue : dword ) ;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    SetDWordValueR ( REG_CONSOLE_PREFERENCE_CODES_HIHI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceCodesRW: dword;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    Result := GetDWordValueR ( REG_CONSOLE_PREFERENCE_CODESRW_HIHI )
  else
    Result := 0;
end;

procedure TBionXConsole.SetPreferenceCodesRW ( AValue : dword ) ;
begin
  if CheckVersion ( 42, 255, 0, 255, 0, 255 ) then
    SetDWordValueR ( REG_CONSOLE_PREFERENCE_CODESRW_HIHI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceRegion: byte;
begin
  if CheckVersion ( 40, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_CONSOLE_PREFERENCE_REGION )
  else
    Result := 0;
end;

procedure TBionXConsole.SetPreferenceRegion ( AValue : byte ) ;
begin
  if CheckVersion ( 40, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_CONSOLE_PREFERENCE_REGION, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceConfigBit0: byte;
begin
  if CheckVersion ( 40, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_CONSOLE_PREFERENCE_CONFIGBIT_0 )
  else
    Result := 0;
end;

procedure TBionXConsole.SetPreferenceConfigBit0 ( AValue : byte ) ;
begin
  if CheckVersion ( 40, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_CONSOLE_PREFERENCE_CONFIGBIT_0, Avalue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceFlipSide: boolean;
begin
//  if ( CheckVersion ( 45, 45, 0, 255, 0, 255 ) and ( PartNumber = 3205 ) ) or
//     CheckVersion ( 46, 255, 0, 255, 15, 15 ) then
  // Korrektur in neuer bionx.xml
  if CheckVersion ( 46, 255, 0, 255, 15, 15 ) or
     CheckVersion ( 57, 255, 0, 255, 27, 27 ) then
    Result := GetBoolValue ( REG_CONSOLE_PREFERENCE_FLIP_SIDE )
  else
    Result := false;
end;

procedure TBionXConsole.SetPreferenceFlipSide ( AValue: boolean ) ;
begin
//  if ( CheckVersion ( 45, 45, 0, 255, 0, 255 ) and ( PartNumber = 3205 ) ) or
//     CheckVersion ( 46, 255, 0, 255, 15, 15 ) then
  // Korrektur in neuer bionx.xml
  if CheckVersion ( 46, 255, 0, 255, 15, 15 ) or
     CheckVersion ( 57, 255, 0, 255, 27, 27 ) then
    SetBoolValue ( REG_CONSOLE_PREFERENCE_FLIP_SIDE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceLightButtonMode: byte;
begin
  if CheckVersion ( 59, 255, 0, 255, 15, 15 ) then
    Result := GetByteValue ( REG_CONSOLE_PREFERENCE_LIGHT_BUTTON_MODE )
  else
    Result := 0;
end;

procedure TBionXConsole.SetPreferenceLightButtonMode ( AValue: byte ) ;
begin
  if CheckVersion ( 59, 255, 0, 255, 15, 15 ) then
    SetByteValue ( REG_CONSOLE_PREFERENCE_LIGHT_BUTTON_MODE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceLightsOnAtStart: boolean;
begin
  if CheckVersion ( 59, 255, 0, 255, 15, 15 ) then
    Result := GetBoolValue ( REG_CONSOLE_PREFERENCE_LIGHT_ON_AT_START )
  else
    Result := false;
end;

procedure TBionXConsole.SetPreferenceLightsOnAtStart ( AValue: boolean ) ;
begin
  if CheckVersion ( 59, 255, 0, 255, 15, 15 ) then
    SetBoolValue ( REG_CONSOLE_PREFERENCE_LIGHT_ON_AT_START, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceExpertMode: boolean;
begin
  if CheckVersion ( 62, 255, 0, 255, 15, 15 ) then
    Result := GetBoolValue ( REG_CONSOLE_PREFERENCE_EXPERTMODE )
  else
    Result := false;
end;

procedure TBionXConsole.SetPreferenceExpertMode ( AValue : boolean );
begin
  if CheckVersion ( 62, 255, 0, 255, 15, 15 ) then
    SetBoolValue ( REG_CONSOLE_PREFERENCE_EXPERTMODE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetPreferenceThrottleMode: byte;
begin
//  if CheckVersion ( 62, 255, 3, 255, 15, 15 ) then
  // nach Korrektur in bionx.xml
  if CheckVersion ( 62, 62, 3, 255, 15, 15 ) or
     CheckVersion ( 63, 255, 0, 255, 15, 15 ) then
    Result := GetByteValue ( REG_CONSOLE_PREFERENCE_THROTTLE_MODE )
  else
    Result := 0;
end;

procedure TBionXConsole.SetPreferenceThrottleMode ( AValue : byte ) ;
begin
//  if CheckVersion ( 62, 255, 3, 255, 15, 15 ) then
  // nach Korrektur in bionx.xml
  if CheckVersion ( 62, 62, 3, 255, 15, 15 ) or
     CheckVersion ( 63, 255, 0, 255, 15, 15 ) then
    SetByteValue ( REG_CONSOLE_PREFERENCE_THROTTLE_MODE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetStatsOdo: double;
begin
  Result := GetDWordValue ( REG_CONSOLE_STATISTIC_ODOMETER_HIHI ) * DISTANCE_FACTOR;
end;

procedure TBionXConsole.SetStatsOdo ( AValue : double ) ;
begin
  // !!! very strange !!!
  // the console reverses the bytes somehow
  // therefore use SetDWordValueR to write registers
  SetDWordValueR ( REG_CONSOLE_STATISTIC_ODOMETER_HIHI, round ( AValue / DISTANCE_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetStatsChrono: LongWord;
var
  h, m, s : byte;
begin
  // return the Chronotime in seconds
  h := GetByteValue ( REG_CONSOLE_STATISTIC_CHRONO_HOUR );
  m := GetByteValue ( REG_CONSOLE_STATISTIC_CHRONO_MINUTE );
  s := GetByteValue ( REG_CONSOLE_STATISTIC_CHRONO_SECOND );
  Result := ( h * 60 + m ) * 60 + s;
end;

procedure TBionXConsole.SetStatsChrono ( AValue : LongWord ) ;
var
  Hours        : word;
  Minutes      : word;
  Seconds      : word;
begin
  Seconds := AValue mod 60;
  Minutes := AValue div 60;
  Hours := Minutes div 60;
  Minutes := Minutes mod 60;
  SetByteValue ( REG_CONSOLE_STATISTIC_CHRONO_HOUR, Hours );
  SetByteValue ( REG_CONSOLE_STATISTIC_CHRONO_MINUTE, Minutes );
  SetByteValue ( REG_CONSOLE_STATISTIC_CHRONO_SECOND, Seconds );
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetStatsTrip: double;
begin
  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_CONSOLE_STATISTIC_DIST_HI ) * DISTANCE_FACTOR
  else
    Result := 0;
end;

(*----------------------------------------------------------------------------*)

function TBionXConsole.GetStatsAvgSpeed: double;
begin
  if CheckVersion ( 39, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_CONSOLE_STATISTIC_AVGSPEED_HI ) * SPEED_FACTOR
  else
    Result := 0;
end;


(******************************************************************************)
(******************************************************************************)
(*                                                                            *)
(*                                                                            *)
(* TBionXBattery                                                              *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
(******************************************************************************)

function TBionXBattery.ReadI2CByteValue ( DevReg : word ) : byte;
begin
  BridgeI2CAddress := DevReg;
  sleep(25); // give it some time to fill the register
  Result := BridgeI2CData;
end;

procedure TBionXBattery.WriteI2CByteValue ( DevReg : word; AValue : byte );
begin
  BridgeI2CAddress := DevReg;
  sleep(25); // give it some time to fill the register
  BridgeI2CData := AValue;
  sleep(25); // give it some time to handle written data
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.ReadI2CWordValue ( DevReg : word ) : word;
begin
  Result :=   ReadI2CByteValue ( DevReg ) shl 8
            + ReadI2CByteValue ( DevReg + 1 );
end;

procedure TBionXBattery.WriteI2CWordValue ( DevReg : word; AValue : word );
begin
  // unlock once for both I2C bridgedata write operations
  UnlockProtection;
  try
    WriteI2CByteValue ( DevReg,     AValue shr 8 );
    WriteI2CByteValue ( DevReg + 1, AValue and $FF );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.ReadI2CDWordValue ( DevReg : word ) : dword;
begin
  Result :=   ReadI2CByteValue ( DevReg     ) shl 24
            + ReadI2CByteValue ( DevReg + 1 ) shl 16
            + ReadI2CByteValue ( DevReg + 2 ) shl 8
            + ReadI2CByteValue ( DevReg + 3 );
end;

procedure TBionXBattery.WriteI2CDWordValue ( DevReg : word; AValue : dword );
begin
  // unlock once for all I2C bridgedata write operations
  UnlockProtection;
  try
    WriteI2CByteValue ( DevReg,     AValue shr 24 );
    WriteI2CByteValue ( DevReg + 1, AValue shr 16 );
    WriteI2CByteValue ( DevReg + 2, AValue shr  8 );
    WriteI2CByteValue ( DevReg + 3, AValue and $FF );
  finally
    LockProtection;
  end;
end;

function TBionXBattery.ReadCellMonVoltageValue ( Reg : byte ) : word;
begin
  CellMonChannelAddress := Reg;
  sleep(25); // give it some time to fill the register
  Result := CellMonChannelData;
end;
(*+++
procedure TBionXBattery.WriteCellMonVoltageValue ( Reg : byte; AValue : word );
begin
  raise Exception.Create ( 'untested set method' );
  CellMonChannelAddress := Reg;
  sleep(25); // give it some time to fill the register
  CellMonChannelData := AValue;
end;
+++*)
function TBionXBattery.ReadCellMonCalibrationValue ( Reg : byte ) : smallint;
begin
//  SetByteValue ( REG_BATTERY_CELLMON_CHANNEL_ADDRESS, Reg );
//  sleep(25); // give it some time to fill the register
//  Result :=   GetByteValue ( REG_BATTERY_CELLMON_CALIBRATION_DATA_HI ) shl 8
//            + GetByteValue ( REG_BATTERY_CELLMON_CALIBRATION_DATA_LO );

  CellMonChannelAddress := Reg;
  sleep(10); // give it some time to fill the register // 25ms
  Result := CellMonCalibrationData;
end;

procedure TBionXBattery.WriteCellMonCalibrationValue ( Reg : byte; AValue : smallint );
begin
  CellMonChannelAddress := Reg;
  sleep(25); // give it some time to fill the register
  CellMonCalibrationData := AValue;
end;

function TBionXBattery.ReadChargerByteValue ( Addr : byte ) : byte;
begin
  BridgeChargerAddress := Addr;
  sleep(10); // give it some time to fill the register //25ms
  Result := BridgeChargerData;
end;

procedure TBionXBattery.WriteChargerByteValue ( Addr : byte; AValue : byte );
begin
  BridgeChargerAddress := Addr;
  sleep(100); // give it some time to fill the register before
  BridgeChargerData := AValue;
  sleep(100); // give it some time to handle written data
end;

function TBionXBattery.ReadChargerWordValue ( Addr : byte ) : word;
begin
  Result :=   ReadChargerByteValue ( Addr ) shl 8
            + ReadChargerByteValue ( Addr + 1 );
end;

procedure TBionXBattery.WriteChargerWordValue ( Addr : byte; AValue : word );
begin
  WriteChargerByteValue ( Addr,     AValue shr 8 );
  WriteChargerByteValue ( Addr + 1, AValue and $FF );
end;

(*----------------------------------------------------------------------------*)

procedure TBionXBattery.UnlockProtection;
begin
  if FUnlockLevel = 0 then
  begin
    ProtectUnlock := BATTERY_PROTECT_UNLOCK_KEY;
    sleep(1000);
  end;
  inc ( FUnlockLevel );
end;

procedure TBionXBattery.LockProtection;
begin
  if FUnlockLevel > 0 then
    dec ( FUnlockLevel );
  if FUnlockLevel = 0 then
  begin
    ProtectUnlock := BATTERY_PROTECT_LOCK_KEY;
    sleep(0);
  end;
end;

(******************************************************************************)

function TBionXBattery.GetSoftwareVersion: byte;
begin
  if FSoftwareVersion = 0 then
    FSoftwareVersion := GetByteValue( REG_BATTERY_REV_SW );
  Result := FSoftwareVersion;
end;

function TBionXBattery.GetHardwareVersion: byte;
begin
  if FHardwareVersion = 0 then
    FHardwareVersion := GetByteValue( REG_BATTERY_REV_HW );
  Result := FHardwareVersion;
end;

function TBionXBattery.GetSubVersion: byte;
begin
  if FSubVersion = 0 then
    FSubVersion := GetByteValue( REG_BATTERY_REV_SUB );
  Result := FSubVersion;
end;

function TBionXBattery.GetChargerVersion: byte;
begin
  if CheckVersion ( 91, 255, 0, 255, 60, 255 ) then
    Result := ReadChargerByteValue ( REG_CHARGER_REV_CHARGER )
  else
    Result := 0;
end;

function TBionXBattery.GetSupervisorVersion : byte;
begin
  if CheckVersion ( 91, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_REV_SUPERVISOR )
  else
    Result := 0;
end;

function TBionXBattery.GetBOM: byte;
begin
  if CheckVersion ( 107, 255, 0, 255, 60, 255 ) then
    Result := GetByteValue ( REG_BATTERY_REV_BOM )
  else
    Result := 0;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetCellpPackItemNumber: word;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := GetWordValue ( REG_BATTERY_SN_CELLPACK_HI )
  else
    Result := 0;
end;


function TBionXBattery.GetPartNumber: word;
begin
  if CheckVersion ( 0, 255, 0, 255,  0, 52 ) or
     CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := GetWordValue( REG_BATTERY_SN_PN_HI )
  else
    Result := 0;
end;

function TBionXBattery.GetLocation : byte;
begin
  if CheckVersion ( 0, 255, 0, 255,  0, 52 ) or
     CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := GetByteValue ( REG_BATTERY_SN_LOCATION )
  else
    Result := 0;
end;

function TBionXBattery.GetManufacturingDate : TDate;
begin
  if FManufacturingDate = 0 then
  begin
    if CheckVersion ( 0, 255, 0, 255,  0, 52 ) or
       CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
      FManufacturingDate := EncodeDate ( GetByteValue ( REG_BATTERY_SN_YEAR ) + 2000,
                                         GetByteValue ( REG_BATTERY_SN_MONTH ),
                                         GetByteValue ( REG_BATTERY_SN_DAY )
                                       )
    else
      FManufacturingDate := 0;
  end;
  Result := FManufacturingDate;
end;

function TBionXBattery.GetItemNumber: word;
begin
  if CheckVersion ( 0, 255, 0, 255,  0, 52 ) or
     CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := GetWordValue( REG_BATTERY_SN_ITEM_HI )
  else
    Result := 0;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetPCBSNPartNumber: word;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_PCBSN_PN_HI )
  else
    Result := 0;
end;

function TBionXBattery.GetPCBSNLocation : byte;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_PCBSN_LOCATION )
  else
    Result := 0;
end;

function TBionXBattery.GetPCBSNManufacturingDate : TDate;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
  begin
    try
      Result := EncodeDate ( ReadI2CByteValue ( REG_I2C_PCBSN_YEAR ) + 2000,
                             ReadI2CByteValue ( REG_I2C_PCBSN_MONTH ),
                             ReadI2CByteValue ( REG_I2C_PCBSN_DAY )
                           );
    except
      Result := 0;
    end;
  end
  else
    Result := 0;
end;

function TBionXBattery.GetPCBSNItemNumber : word;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_PCBSN_ITEM_HI );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetTimerPower : word;
begin
  if CheckVersion ( 75, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_BATTERY_TIMER_POWER_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetTimerPower ( AValue : word ) ;
begin
  if CheckVersion ( 75, 255, 0, 255, 0, 255 ) then
    SetWordValue ( REG_BATTERY_TIMER_POWER_HI, AValue )
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetTimerAccessory : word;
begin
  if CheckVersion ( 75, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_BATTERY_TIMER_ACCESSORY_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetTimerAccessory ( AValue : word ) ;
begin
  if CheckVersion ( 75, 255, 0, 255, 0, 255 ) then
    SetWordValue ( REG_BATTERY_TIMER_ACCESSORY_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetTimerPrecharge : byte;
begin
  if CheckVersion ( 75, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_BATTERY_TIMER_PRECHARGE )
  else
    Result := 0;
end;

procedure TBionXBattery.SetTimerPrecharge ( AValue : byte ) ;
begin
  if CheckVersion ( 75, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_BATTERY_TIMER_PRECHARGE, AValue )
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetTimerMasterShutdown: word;
begin
  if CheckVersion ( 75, 75, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_BATTERY_TIMER_SHUTDOWN_HI ) div 2
  else
    if CheckVersion ( 76, 255, 0, 255, 0, 255 ) then
      Result := GetWordValue ( REG_BATTERY_TIMER_SHUTDOWN_HI )
    else
      Result := 0;
end;

procedure TBionXBattery.SetTimerMasterShutdown ( AValue: word ) ;
begin
  // be aware to set the delay too short, as the sytem may
  // shutdown, before you are able to increase the value again.
  // I don't know how the BionX system handles this, but for
  // security this program allows a minimum of 5 min ( 300 sec ) only
  if AValue >= 300 then
  begin
    if CheckVersion ( 75, 75, 0, 255, 0, 255 ) then
      SetWordValue ( REG_BATTERY_TIMER_SHUTDOWN_HI, AValue * 2 )
    else
      if CheckVersion ( 76, 255, 0, 255, 0, 255 ) then
        SetWordValue ( REG_BATTERY_TIMER_SHUTDOWN_HI, AValue );
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatusV5V: double;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadCellMonVoltageValue ( REG_CELLMON_STATUS_5V_VOLTAGE ) * VOLTAGE_FACTOR
  else
    Result := 0;
end;


function TBionXBattery.GetStatusV3V3: double;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadCellMonVoltageValue ( REG_CELLMON_STATUS_3V3_VOLTAGE ) * VOLTAGE_FACTOR
  else
    Result := 0;
end;

function TBionXBattery.GetStatusVPackId: double;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadCellMonVoltageValue ( REG_CELLMON_STATUS_PACKID_VOLTAGE ) * VOLTAGE_FACTOR
  else
    Result := 0;
end;

function TBionXBattery.GetStatusVBOMId: double;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadCellMonVoltageValue ( REG_CELLMON_STATUS_BOMID_VOLTAGE ) * VOLTAGE_FACTOR
  else
    Result := 0;
end;

function TBionXBattery.GetStatusFlags : word;
begin
  if CheckVersion ( 0, 255, 0, 255, 0, 52 ) then
    Result := GetByteValue ( REG_BATTERY_STATUS_FLAGS_LO )
  else
    if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
      Result :=   GetByteValue ( REG_BATTERY_STATUS_FLAGS_HI ) shl 8
                + GetByteValue ( REG_BATTERY_STATUS_FLAGS_LO )
    else
      Result := 0;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatusTestFlags : word;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATUS_TEST_FLAGS_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatusTestFlags ( AValue : word ) ;
begin
  Untested;
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATUS_TEST_FLAGS_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatusPermanentFailureFlags : boolean;
var
  Flag : byte;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
  begin
    Flag := ReadI2CByteValue ( REG_I2C_STATUS_PERMANENT_FAILURE_FLAGS );
    Result := ( Flag <> 0 ) and
              ( Flag <> $FF );
  end
  else
    Result := false;
end;

procedure TBionXBattery.SetStatusPermanentFailureFlags ( AValue : boolean ) ;
begin
  // "Having this register set to an non-0xFF or non-zero value indicates this
  // battery shouldn't be used (see /wiki/EepromForTester)"

  // ooops, we better never write to this register except to reanimate
  // a dead battery
  Untested;
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
  begin
    // only allow setting to false at this time
    if AValue then
    begin
      // uncomment this, if you really want to try
      // WriteI2CByteValue ( REG_I2C_STATUS_PERMANENT_FAILURE_FLAGS, $FF )
    end
    else
      // my healthy battery reads a $FF, so we write $FF
      WriteI2CByteValue ( REG_I2C_STATUS_PERMANENT_FAILURE_FLAGS, $FF );
  end
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatusChargerManagerStatus : byte;
begin
  if CheckVersion ( 102, 255, 0, 255, 60, 255 ) then
    Result := GetByteValue ( REG_BATTERY_STATUS_CHARGER_MANAGER_STATUS )
  else
    Result := 0;
end;

function TBionXBattery.GetStatusVBattNorm: double;
begin
  Result := GetByteValue ( REG_BATTERY_STATUS_BATTERY_VOLTAGE_NORMALIZED ) * NORMALIZED_VOLTAGE_FAKTOR + NORMALIZED_VOLTAGE_OFFSET;
end;

function TBionXBattery.GetStatusVBattInternal : double;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_INTERNAL_BATTERY_VOLTAGE_HI ) * VOLTAGE_FACTOR;
end;

function TBionXBattery.GetStatusVBatt: double;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_BATTERY_VOLTAGE_HI ) * VOLTAGE_FACTOR;
end;

function TBionXBattery.GetStatusVPower : double;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_POWER_VOLTAGE_HI ) * VOLTAGE_FACTOR;
end;

function TBionXBattery.GetStatusVControl : double;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_CONTROL_VOLTAGE_HI ) * VOLTAGE_FACTOR;
end;

function TBionXBattery.GetStatusVConsole : double;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_CONSOLE_VOLTAGE_HI ) * VOLTAGE_FACTOR;
end;

function TBionXBattery.GetStatusV12V : double;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_12V_VOLTAGE_HI ) * VOLTAGE_FACTOR;
end;

function TBionXBattery.GetStautsVAccessory : double;
begin
  Result := GetWordValue ( REG_BATTERY_STATUS_ACCESSORY_VOLTAGE_HI ) * VOLTAGE_FACTOR;
end;

function TBionXBattery.GetStatusVDCIn : double;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := GetWordValue ( REG_BATTERY_STATUS_DCIN_VOLTAGE_HI ) * VOLTAGE_FACTOR
  else
    Result := 0;
end;

function TBionXBattery.GetStatusICellpack : double;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := smallint(GetWordValue ( REG_BATTERY_STATUS_CELLPACK_CURRENT_HI )) * CURRENT_FACTOR
  else
    Result := 0;
end;

function TBionXBattery.GetStatusLevel: double;
begin
  Result := GetByteValue ( REG_BATTERY_STATUS_CHARGE_LEVEL ) * 6.6667;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatusLeds: byte;
begin
  Result := GetByteValue ( REG_BATTERY_STATUS_LEDS );
end;

procedure TBionXBattery.SetStatusLeds(AValue: byte);
begin
  Untested;
  SetByteValue ( REG_BATTERY_STATUS_LEDS, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatusVChannel(CellNo : byte): double;
begin
  // CellNo: 1..PackSerial
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := smallint ( ReadCellMonVoltageValue ( $80 + CellNo )) * VOLTAGE_FACTOR
  else
    Result := 0;
end;

function TBionXBattery.GetStatusVCell ( CellNo : byte ) : double;
begin
  // CellNo: 1..PackSerial
  if CheckVersion ( 96, 255, 0, 255, 60, 255 ) then
    Result := ReadCellMonVoltageValue ( CellNo ) * VOLTAGE_FACTOR
  else
    Result := 0;
end;

function TBionXBattery.GetStatusPackTemperature(SensorNo : byte): shortint;
begin
  // SensorNo: 1..4
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := GetByteValue ( REG_BATTERY_STATUS_TEMPERATURE_SENSOR_1 + SensorNo - 1 )
  else
    Result := 0;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatusCapSense : dword;
begin
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATUS_CAPSENSE_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatusCapSense ( AValue : dword ) ;
begin
  Untested;
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATUS_CAPSENSE_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatusCapSenseReference : dword;
begin
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATUS_CAPSENSE_REFERENCE_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatusCapSenseReference ( AValue : dword ) ;
begin
  Untested;
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATUS_CAPSENSE_REFERENCE_HI, AValue )
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatusEstimatedSOC : byte;
begin
  Result := GetByteValue ( REG_BATTERY_STATUS_ESTIMATED_SOC );
end;

function TBionXBattery.GetStatusPowerOnResetCount : byte;
begin
  if CheckVersion ( 95, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_BATTERY_STATUS_POWERON_RESET_COUNT )
  else
    Result := 0;
end;

function TBionXBattery.GetStatusWatchdogResetCount : byte;
begin
  if CheckVersion ( 95, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_BATTERY_STATISTIC_WATCHDOG_RESET_COUNT )
  else
    Result := 0;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetCellMonChannelAddress: byte;
begin
  Result := GetByteValue ( REG_BATTERY_CELLMON_CHANNEL_ADDRESS );
end;

procedure TBionXBattery.SetCellMonChannelAddress(AValue: byte);
begin
  SetByteValue ( REG_BATTERY_CELLMON_CHANNEL_ADDRESS, AValue );
end;


function TBionXBattery.GetCellMonChannelData: word;
begin
  Result := GetWordValue ( REG_BATTERY_CELLMON_CHANNELDATA_HI );
end;

procedure TBionXBattery.SetCellMonChannelData(AValue: word);
begin
  SetWordValue ( REG_BATTERY_CELLMON_CHANNELDATA_HI, AValue );
end;

function TBionXBattery.GetCellMonCalibrationData: word;
begin
  if CheckVersion ( 0, 102, 0, 255, 60, 255 ) then
    Result := GetByteValue ( REG_BATTERY_CELLMON_CALIBRATION_DATA_LO )
  else
    if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
      Result :=   GetByteValue ( REG_BATTERY_CELLMON_CALIBRATION_DATA_HI ) shl 8
                + GetByteValue ( REG_BATTERY_CELLMON_CALIBRATION_DATA_LO )
    else
      Result := 0;
end;

procedure TBionXBattery.SetCellMonCalibrationData(AValue: word);
begin
  if CheckVersion ( 0, 102, 0, 255, 60, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_BATTERY_CELLMON_CALIBRATION_DATA_LO, AValue and $FF )
    finally
      LockProtection;
    end;
  end
  else
    if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    begin
      UnlockProtection;
      try
        SetByteValue ( REG_BATTERY_CELLMON_CALIBRATION_DATA_HI, AValue shr 8 );
        SetByteValue ( REG_BATTERY_CELLMON_CALIBRATION_DATA_LO, AValue and $FF );
        sleep(100);
      finally
        LockProtection;
      end;
    end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetCellMonBalancerEnabled: boolean;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := GetBoolValue ( REG_BATTERY_CELLMON_BALANCER_ENABLED )
  else
    Result := false;
end;

procedure TBionXBattery.SetCellMonBalancerEnabled(AValue: boolean);
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
  begin
    UnlockProtection;
    try
      SetBoolValue ( REG_BATTERY_CELLMON_BALANCER_ENABLED, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetChargerCurrent: double;
begin
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    Result := ReadChargerWordValue ( REG_CHARGER_CURRENT_HI ) * 0.001
  else
    Result := 0;
end;

procedure TBionXBattery.SetChargerCurrent(AValue: double);
begin
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    WriteChargerWordValue ( REG_CHARGER_CURRENT_HI, round ( AValue / 0.001 ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetChargerCurrentCalibration: double;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := ReadChargerWordValue ( REG_CHARGER_CURRENT_CALIBRATION_HI ) * 0.001
  else
    Result := 0;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetChargerFinalVoltage: double;
begin
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    Result := ReadChargerWordValue ( REG_CHARGER_FINAL_VOLTAGE_HI ) * 0.01 // !!! not VOLTAGE_FACTOR here !!!
  else
    Result := 0;
end;

procedure TBionXBattery.SetChargerFinalVoltage(AValue: double);
begin
  Untested;
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    WriteChargerWordValue ( REG_CHARGER_FINAL_VOLTAGE_HI, round ( AValue / 0.01 ) ); // !!! not VOLTAGE_FACTOR here !!!
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetChargerMode: byte;
begin
  if CheckVersion ( 91, 255, 0, 255, 60, 255 ) then
    Result := ReadChargerByteValue ( REG_CHARGER_MODE )
  else
    Result := 0;
end;

procedure TBionXBattery.SetChargerMode(AValue: byte);
begin
  Untested;
  if CheckVersion ( 91, 255, 0, 255, 60, 255 ) then
    WriteChargerByteValue ( REG_CHARGER_MODE, AValue )
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetChargerStatusFlags: word;
begin
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    Result := ReadChargerWordValue ( REG_CHARGER_STATUS_FLAGS_HI )
  else
    Result := 0;
end;

function TBionXBattery.GetChargerVoltageCalibration: double;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := ReadChargerWordValue ( REG_CHARGER_VOLTAGE_CALIBRATION_HI ) * 0.001
  else
    Result := 0;
end;

(******************************************************************************)

function TBionXBattery.GetBridgeI2CAddress: word;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result :=   GetByteValue ( REG_BATTERY_BRIDGE_I2C_REGADDR_DEVICE ) shl 8
              + GetByteValue ( REG_BATTERY_BRIDGE_I2C_REGADDR_REGISTER )
  else
    Result := 0;
end;

procedure TBionXBattery.SetBridgeI2CAddress(AValue: word);
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
  begin
    SetByteValue ( REG_BATTERY_BRIDGE_I2C_REGADDR_DEVICE, ( AValue shr 8 ) );
    sleep(100);
    SetByteValue ( REG_BATTERY_BRIDGE_I2C_REGADDR_REGISTER, ( AValue and $FF ) );
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetBridgeI2CData: byte;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := GetByteValue ( REG_BATTERY_BRIDGE_I2C_REGISTER_DATA )
  else
    Result := 0;
end;

procedure TBionXBattery.SetBridgeI2CData(AValue: byte);
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_BATTERY_BRIDGE_I2C_REGISTER_DATA, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetBridgeChargerAddress: byte;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := GetByteValue ( REG_BATTERY_BRIGDE_CHARGER_ADDR )
  else
    Result := 0;
end;

procedure TBionXBattery.SetBridgeChargerAddress(AValue: byte);
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_BATTERY_BRIGDE_CHARGER_ADDR, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetBridgeChargerData: byte;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := GetByteValue ( REG_BATTERY_BRIGDE_CHARGER_DATA )
  else
    Result := 0;
end;

procedure TBionXBattery.SetBridgeChargerData(AValue: byte);
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_BATTERY_BRIGDE_CHARGER_DATA, AValue )
    finally
      LockProtection;
    end;
  end;
end;

(******************************************************************************)

function TBionXBattery.GetCalibCapsense : double;
begin
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_CALIBRATION_CAPSENSE ) * -0.2 + 12
  else
    Result := 0;
end;

procedure TBionXBattery.SetCalibCapsense(AValue: double);
begin
  if CheckVersion ( 92, 255, 0, 255, 60, 255 ) then
    WriteI2CByteValue ( REG_I2C_CALIBRATION_CAPSENSE, round ( ( AValue - 12 ) / -0.2 ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetCalibCalibration ( CellNo : byte ) : double;
begin
//  if CheckVersion (   0, 102, 0, 255, 60, 255 ) or
//     CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
  if CheckVersion (   0, 255, 0, 255, 60, 255 ) then
    Result := ReadCellMonCalibrationValue ( CellNo ) * 0.03
  else
    Result := 0;
end;

procedure TBionXBattery.SetCellCalibration(CellNo : byte; AValue: double);
begin
  //  if CheckVersion (   0, 102, 0, 255, 60, 255 ) or
  //     CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
  if CheckVersion (   0, 255, 0, 255, 60, 255 ) then
    WriteCellMonCalibrationValue ( CellNo, round ( AValue / 0.03 ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetCalibCalibration3V3 : double;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_CALIBRATION_3V3_VOLTAGE_HI ) * VOLTAGE_FACTOR
  else
    Result := 0;
end;

procedure TBionXBattery.SetCalibrationValue3V3(AValue: double);
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_CALIBRATION_3V3_VOLTAGE_HI, round ( AValue / VOLTAGE_FACTOR ) );
end;

(******************************************************************************)

function TBionXBattery.GetStats5VShorts : word;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATISTIC_5V_VOLTAGE_SHORTS_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStats5VShorts ( AValue : word ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATISTIC_5V_VOLTAGE_SHORTS_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsVControlShorts : word;
begin
  if CheckVersion ( 0, 255, 0, 255, 0, 58 ) then
    Result := GetByteValue ( REG_BATTERY_STATISTIC_CONTROL_VOLTAGE_SHORTS )
  else
    if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
      Result := ReadI2CWordValue ( REG_I2C_STATISTIC_CONTROL_VOLTAGE_SHORTS_HI )
    else
      Result := 0;
end;

procedure TBionXBattery.SetStatsVControlShorts ( AValue : word ) ;
begin
  if CheckVersion ( 0, 255, 0, 255, 0, 58 ) then
    SetByteValue ( REG_BATTERY_STATISTIC_CONTROL_VOLTAGE_SHORTS, AValue and $FF )
  else
    if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
      WriteI2CWordValue ( REG_I2C_STATISTIC_CONTROL_VOLTAGE_SHORTS_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsLowBattBuzzCount : word;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATISTIC_LOWVOLTAGE_BUZZER_COUNT_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatsLowBattBuzzCount ( AValue : word ) ;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATISTIC_LOWVOLTAGE_BUZZER_COUNT_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsCellVoltageCollapseCount : word;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATISTIC_CELLVOLTAGE_COLLAPSE_COUNT_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatsCellVoltageCollapseCount ( AValue : word ) ;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATISTIC_CELLVOLTAGE_COLLAPSE_COUNT_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsCellPartialShortCount : word;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATISTIC_CELL_PARTIAL_SHORT_COUNT_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatsCellPartialShortCount ( AValue : word ) ;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATISTIC_CELL_PARTIAL_SHORT_COUNT_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsCellDeadShortCount : word;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATISTIC_CELL_DEAD_SHORT_COUNT_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatsCellDeadShortCount ( AValue : word ) ;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATISTIC_CELL_DEAD_SHORT_COUNT_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsDeepSleepAfterLongInactivityPeriodCount : word;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATISTIC_DEEPSLEEP_INACTIVITY_COUNT_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatsDeepSleepAfterLongInactivityPeriodCount ( AValue : word ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATISTIC_DEEPSLEEP_INACTIVITY_COUNT_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsDeepSleepAfterLowSOCCount : word;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATISTIC_DEEPSLEEP_LOW_SOC_COUNT_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatsDeepSleepAfterLowSOCCount ( AValue : word ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATISTIC_DEEPSLEEP_LOW_SOC_COUNT_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsDeepSleepExtremeLowBatteryVoltageCount : word;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_STATISTIC_DEEPSLEEP_LOW_VOLTAGE_COUNT_HI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatsDeepSleepExtremeLowBatteryVoltageCount ( AValue : word ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CWordValue ( REG_I2C_STATISTIC_DEEPSLEEP_LOW_VOLTAGE_COUNT_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsDischargeEnergy : DWord;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CDWordValue ( REG_I2C_STATISTIC_DISCHARGE_ENERGY_HIHI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatsDischargeEnergy ( AValue : DWord ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CDWordValue ( REG_I2C_STATISTIC_DISCHARGE_ENERGY_HIHI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsChargeEnergy : DWord;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CDWordValue ( REG_I2C_STATISTIC_CHARGE_ENERGY_HIHI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatsChargeEnergy ( AValue : DWord ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CDWordValue ( REG_I2C_STATISTIC_CHARGE_ENERGY_HIHI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsLMD: double;
begin
  Result := GetWordValue ( REG_BATTERY_STATISTIC_LMD_HI ) * 0.002142;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsReset: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATISTIC_RESETS_HI );
end;

procedure TBionXBattery.SetStatsReset ( AValue : word ) ;
begin
  // against bionx.xml unlocking is needed to write
  UnlockProtection;
  try
    SetWordValue ( REG_BATTERY_STATISTIC_RESETS_HI, Avalue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsGGJrCalib : byte;
begin
  Result := GetByteValue ( REG_BATTERY_STATISTIC_GGJR_CALIB );
end;

procedure TBionXBattery.SetStatsGGJrCalib ( AValue : byte ) ;
begin
  // against bionx.xml unlocking is needed to write
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_STATISTIC_GGJR_CALIB, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsResetWdt: byte;
begin
  Result := GetByteValue ( REG_BATTERY_STATUS_RESET_WDT )
end;

procedure TBionXBattery.SetStatsResetWdt(AValue: byte);
begin
  // against bionx.xml unlocking is needed to write
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_STATUS_RESET_WDT, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsRTCResync : byte;
begin
  Result := GetByteValue ( REG_BATTERY_STATISTIC_RTC_RESYNC );
end;

procedure TBionXBattery.SetStatsRTCResync ( AValue : byte ) ;
begin
  // against bionx.xml unlocking is needed to write
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_STATISTIC_RTC_RESYNC, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsChargeTimeWorst: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATISTIC_CHARGETIME_WORST_HI );
end;

procedure TBionXBattery.SetStatsChargeTimeWorst ( AValue : word ) ;
begin
  // against bionx.xml unlocking is needed to write
  // corrected in new bionx.xml
  UnlockProtection;
  try
    SetWordValue ( REG_BATTERY_STATISTIC_CHARGETIME_WORST_HI, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsChargeTimeMean: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATISTIC_CHARGETIME_MEAN_HI );
end;

procedure TBionXBattery.SetStatsChargeTimeMean ( AValue : word ) ;
begin
  // against bionx.xml unlocking is needed to write
  // corrected in new bionx.xml
  UnlockProtection;
  try
    SetWordValue ( REG_BATTERY_STATISTIC_CHARGETIME_MEAN_HI, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsLMDAdapt : byte;
begin
  Result := GetByteValue ( REG_BATTERY_STATISTIC_LMD_ADAPT );
end;

procedure TBionXBattery.SetStatsLMDAdapt ( AValue : byte ) ;
begin
  Untested;
  SetByteValue ( REG_BATTERY_STATISTIC_LMD_ADAPT, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsBattCycles: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATISTIC_BATTERY_CYCLES_HI );
end;

procedure TBionXBattery.SetStatsBattCycles ( AValue : word ) ;
begin
  UnlockProtection;
  try
    SetWordValue ( REG_BATTERY_STATISTIC_BATTERY_CYCLES_HI, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsBattFullCycles: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATISTIC_BATTERY_FULL_CYCLES_HI );
end;

procedure TBionXBattery.SetStatsBattFullCycles ( AValue : word ) ;
begin
  Untested;
  SetWordValue ( REG_BATTERY_STATISTIC_BATTERY_FULL_CYCLES_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsPowerCycles: word;
begin
  Result := GetWordValue ( REG_BATTERY_STATISTIC_POWER_CYCLES_HI );
end;

procedure TBionXBattery.SetStatsPowerCycles ( AValue : word ) ;
begin
  UnlockProtection;
  try
    SetWordValue ( REG_BATTERY_STATISTIC_POWER_CYCLES_HI, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsVBattMax: double;
begin
  Result := GetByteValue ( REG_BATTERY_STATISTIC_BATTERY_MAX_VOLTAGE )  * NORMALIZED_VOLTAGE_FAKTOR + NORMALIZED_VOLTAGE_OFFSET;
end;

procedure TBionXBattery.SetVBattMax ( AValue : double ) ;
begin
  Untested;
  SetByteValue ( REG_BATTERY_STATISTIC_BATTERY_MAX_VOLTAGE, round ( ( AValue - NORMALIZED_VOLTAGE_OFFSET ) / NORMALIZED_VOLTAGE_FAKTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetVBattMin: double;
begin
  Result := GetByteValue ( REG_BATTERY_STATISTIC_BATTERY_MIN_VOLTAGE )  * NORMALIZED_VOLTAGE_FAKTOR + NORMALIZED_VOLTAGE_OFFSET;
end;

procedure TBionXBattery.SetVBattMin ( AValue : double ) ;
begin
  Untested;
  SetByteValue ( REG_BATTERY_STATISTIC_BATTERY_MIN_VOLTAGE, round ( ( AValue - NORMALIZED_VOLTAGE_OFFSET ) / NORMALIZED_VOLTAGE_FAKTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetVBattMean: double;
begin
  Result := GetByteValue ( REG_BATTERY_STATISTIC_BATTERY_AVGVOLTAGE_NORMALIZED )  * NORMALIZED_VOLTAGE_FAKTOR + NORMALIZED_VOLTAGE_OFFSET;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsTBattMax: shortint;
begin
  Result := GetByteValue ( REG_BATTERY_STATISTIC_TEMPERATURE_MAX );
end;

procedure TBionXBattery.SetStatsTBattMax ( AValue : shortint ) ;
begin
  // against bionx.xml unlocking is needed to write
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_STATISTIC_TEMPERATURE_MAX, byte ( AValue ) );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsTBattMin: shortint;
begin
  Result := GetByteValue ( REG_BATTERY_STATISTIC_TEMPERATURE_MIN );
end;

procedure TBionXBattery.SetStatsTBattMin ( AValue : shortint ) ;
begin
  // against bionx.xml unlocking is needed to write
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_STATISTIC_TEMPERATURE_MIN, byte ( AValue ) );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetStatsCharge(Level : byte): word;
begin
  if CheckVersion ( 78, 255, 0, 255, 0, 255 ) and
  // Level: 1..9 -> 10..90%
     ( Level >= 1 ) and ( Level <= 9 ) then
  begin
    SetByteValue ( REG_BATTERY_STATISTIC_CHARGE_TIMES_CHANNEL, Level );
    sleep(25);
    Result := GetWordValue ( REG_BATTERY_STATISTIC_CHARGE_TIMES_DATA_HI );
  end
  else
    Result := 0;
end;

procedure TBionXBattery.SetStatsCharge ( Level : byte; AValue : word ) ;
begin
  if CheckVersion ( 78, 255, 0, 255, 0, 255 ) and
  // Level: 1..9 -> 10..90%
     ( Level >= 1 ) and ( Level <= 9 ) then
  begin
    SetByteValue ( REG_BATTERY_STATISTIC_CHARGE_TIMES_CHANNEL, Level );
    UnlockProtection;
    try
      SetWordValue ( REG_BATTERY_STATISTIC_CHARGE_TIMES_DATA_HI, AValue );
    finally
      LockProtection;
    end;
  end
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetRTCTime : dword;
begin
  Result := GetDWordValue ( REG_BATTERY_RTC_TIME_HIHI );
end;

procedure TBionXBattery.SetRTCTime ( AValue : dword ) ;
begin
  UnlockProtection;
  try
    SetDWordValue ( REG_BATTERY_RTC_TIME_HIHI, AValue );
  finally
    LockProtection;
  end;
end;

function TBionXBattery.GetRTCTimeDT: TDateTime;
begin
  Result := ManufacturingDate + RTCTime / SECONDS_PER_DAY;
end;

procedure TBionXBattery.SetRTCTimeDT(AValue: TDateTime);
begin
  RTCTime := round ( ( AValue - ManufacturingDate ) * SECONDS_PER_DAY );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetRTCLastChargeTimestamp : dword;
begin
  if CheckVersion ( 77, 255, 0, 255, 0, 255 ) then
    Result := GetDWordValue ( REG_BATTERY_RTC_LAST_CHARGE_TIMESTAMP_HIHI )
  else
    Result := 0;
end;

procedure TBionXBattery.SetRTCLastChargeTimestamp ( AValue : dword ) ;
begin
  if CheckVersion ( 77, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetDWordValue ( REG_BATTERY_RTC_LAST_CHARGE_TIMESTAMP_HIHI, AValue );
    finally
      LockProtection;
    end;
  end;
end;

function TBionXBattery.GetRTCLastChargeTimestampDT: TDateTime;
begin
  Result := ManufacturingDate + RTCLastChargeTimestamp  / SECONDS_PER_DAY;
end;

procedure TBionXBattery.SetRTCLastChargeTimestampDT(AValue: TDateTime);
begin
  RTCLastChargeTimestamp := round ( ( AValue - ManufacturingDate ) * SECONDS_PER_DAY );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetRTCLastValidTimestamp : dword;
begin
  if CheckVersion ( 77, 255, 0, 255, 0, 255 ) then
    Result := GetDWordValue ( REG_BATTERY_RTC_LAST_VALID_TIMESTAMP_HIHI )
  else
    Result := 0;
end;

function TBionXBattery.GetRTCLastValidTimestampDT: TDateTime;
begin
  Result := ManufacturingDate + RTCLastValidTimestamp / SECONDS_PER_DAY;
end;


(*----------------------------------------------------------------------------*)

function TBionXBattery.GetRTCCtrl : byte;
begin
  Result := GetByteValue ( REG_BATTERY_RTC_CTRL );
end;

procedure TBionXBattery.SetRTCCtrl ( AValue : byte ) ;
begin
  Untested;
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_RTC_CTRL, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetRTCStatus : byte;
begin
  Result := GetByteValue ( REG_BATTERY_RTC_STATUS );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetGasGageAI : double;
begin
  Result := GetWordValue ( REG_BATTERY_GASGAGE_AI_HI ) * 0.002142;
end;

function TBionXBattery.GetGasGageVoltage : double;
begin
  Result := GetWordValue ( REG_BATTERY_GASGAGE_VOLTAGE_HI ) * 0.008;
end;

function TBionXBattery.GetGasGageSOC : byte;
begin
  Result := GetByteValue ( REG_BATTERY_GASGAGE_SOC );
end;

function TBionXBattery.GetGasGageLMD : double;
begin
  Result := GetWordValue ( REG_BATTERY_GASGAGE_LMD_HI ) * 0.002142;
end;

function TBionXBattery.GetGasGageStatusFlags : byte;
begin
  Result := GetByteValue ( REG_BATTERY_GASGAGE_STATUS_FLAGS );
end;

function TBionXBattery.GetGasGageTemperature : double;
begin
  Result := GetWordValue ( REG_BATTERY_GASGAGE_TEMPERATUR_HI ) * 0.25 - 273;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetGasgageDMFSD : byte;
begin
  Result := GetByteValue ( REG_BATTERY_GASGAGE_DMFSD );
end;

procedure TBionXBattery.SetGasgageDMFSD ( AValue : byte ) ;
begin
  Untested;
  SetByteValue ( REG_BATTERY_GASGAGE_DMFSD, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetGasGageVoltageDivider : double;
begin
  Result := GetByteValue ( REG_BATTERY_GASGAGE_VOLTAGE_DIVIDER ) * 0.1;
end;

procedure TBionXBattery.SetGasGageVoltageDivider ( AValue : double ) ;
begin
  Untested;
  SetByteValue ( REG_BATTERY_GASGAGE_VOLTAGE_DIVIDER, round ( AValue / 0.1 ) );
end;

(******************************************************************************)

function TBionXBattery.GetConfigMaxPackTemperature : shortint;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_CONFIG_PACK_TEMPERATURE_MAX )
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigMaxPackTemperature ( AValue : shortint ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CByteValue ( REG_I2C_CONFIG_PACK_TEMPERATURE_MAX, byte(AValue) )
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigMinPackTemperature : shortint;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_CONFIG_PACK_TEMPERATURE_MIN )
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigMinPackTemperature ( AValue : shortint ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CByteValue ( REG_I2C_CONFIG_PACK_TEMPERATURE_MIN, byte ( AValue ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigMaxGGTemperature : shortint;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_CONFIG_GASGAGE_TEMPERATURE_MAX )
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigMaxGGTemperature ( AValue : shortint ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CByteValue ( REG_I2C_CONFIG_GASGAGE_TEMPERATURE_MAX, byte ( AValue ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigMinGGTemperature : shortint;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_CONFIG_GASGAGE_TEMPERATURE_MIN )
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigMinGGTemperature ( AValue : shortint ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CByteValue ( REG_I2C_CONFIG_GASGAGE_TEMPERATURE_MIN, byte ( AValue ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigMaxCellDeltaVoltage : double;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_CONFIG_MAX_CELL_DELTA_VOLTAGE ) * VOLTAGE_FACTOR
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigMaxCellDeltaVoltage ( AValue : double ) ;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    WriteI2CByteValue ( REG_I2C_CONFIG_MAX_CELL_DELTA_VOLTAGE, round ( AValue / VOLTAGE_FACTOR ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigTaillampIntensity : byte;
begin
  if CheckVersion ( 100, 255, 0, 255, 60, 255 ) then
    Result := GetByteValue ( REG_BATTERY_CONFIG_TAILLAMP_INTENSITY )
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigTaillampIntensity ( AValue : byte ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    SetByteValue ( REG_BATTERY_CONFIG_TAILLAMP_INTENSITY, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigShipmode : boolean;
begin
  if CheckVersion ( 106, 255, 0, 255, 60, 255 ) then
    Result := GetBoolValue ( REG_BATTERY_CONFIG_SHIPMODE )
  else
    Result := false;
end;

procedure TBionXBattery.SetConfigShipmode ( AValue : boolean ) ;
begin
  Untested;
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_CONFIG_SHIPMODE, BATTERY_CONFIG_SHIPMODE_KEY );
    SetBoolValue ( REG_BATTERY_CONFIG_SHIPMODE, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigDeepSleepAfterLongInactivityPeriodDelay : byte;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_CONFIG_DEEPSLEEP_LONG_INACTIVITY_DELAY )
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigDeepSleepAfterLongInactivityPeriodDelay ( AValue : byte ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CByteValue ( REG_I2C_CONFIG_DEEPSLEEP_LONG_INACTIVITY_DELAY, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigDeepSleepLowSOCDelay : byte;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_CONFIG_DEEPSLEEP_LOW_SOC_DELAY )
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigDeepSleepLowSOCDelay ( AValue : byte ) ;
begin
  if CheckVersion ( 108, 255, 0, 255, 60, 255 ) then
    WriteI2CByteValue ( REG_I2C_CONFIG_DEEPSLEEP_LOW_SOC_DELAY, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigCommMode: byte;
begin
  Result := GetByteValue ( REG_BATTERY_CONFIG_COMMUNICATION_MODE );
end;

procedure TBionXBattery.SetConfigCommMode ( AValue : byte ) ;
begin
  Untested;
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_CONFIG_COMMUNICATION_MODE, BATTERY_CONFIG_COMMUNICATION_MODE_KEY );
    SetByteValue ( REG_BATTERY_CONFIG_COMMUNICATION_MODE, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigAutoSwitchComm: boolean;
begin
  Result := GetBoolValue ( REG_BATTERY_CONFIG_AUTOSWITCH_COMMUNICATION );
end;

procedure TBionXBattery.SetConfigAutoSwitchComm(AValue: boolean);
begin
  Untested;
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_CONFIG_AUTOSWITCH_COMMUNICATION, BATTERY_CONFIG_AUTOSWITCH_COMMUNICATION_KEY );
    SetBoolValue ( REG_BATTERY_CONFIG_AUTOSWITCH_COMMUNICATION, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigWakeOnPowerVoltage : boolean;
begin
  if CheckVersion ( 96, 255, 0, 255, 0, 255 ) then
    Result := GetBoolValue ( REG_BATTERY_CONFIG_WAKE_ON_POWERVOLTAGE )
  else
    Result := false;
end;

procedure TBionXBattery.SetConfigWakeOnPowerVoltage ( AValue : boolean ) ;
begin
  Untested;
  if CheckVersion ( 96, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetBoolValue ( REG_BATTERY_CONFIG_WAKE_ON_POWERVOLTAGE, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigAllowBuckChargingOnBike : boolean;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
    Result := GetBoolValue ( REG_BATTERY_CONFIG_ALLOW_BUCKCHARGING_ON_BIKE )
  else
    Result := false;
end;

procedure TBionXBattery.SetConfigAllowBuckChargingOnBike ( AValue: boolean ) ;
begin
  if CheckVersion ( 103, 255, 0, 255, 60, 255 ) then
  begin
    UnlockProtection;
    try
      SetBoolValue ( REG_BATTERY_CONFIG_ALLOW_BUCKCHARGING_ON_BIKE, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigDiag : byte;
begin
  Result := GetByteValue ( REG_BATTERY_CONFIG_DIAG );
end;

procedure TBionXBattery.SetConfigDiag ( AValue : byte ) ;
begin
  Untested;
  SetByteValue ( REG_BATTERY_CONFIG_DIAG, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigType : byte;
begin
  Result := GetByteValue ( REG_BATTERY_CONFIG_TYPE );
end;

procedure TBionXBattery.SetConfigType ( AValue : byte ) ;
begin
  Untested;
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_CONFIG_TYPE, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigVBattNominal: byte;
begin
  Result := GetByteValue ( REG_BATTERY_CONFIG_NOMINAL_BATTERY_VOLTAGE );
end;

procedure TBionXBattery.SetConfigVBattNominal ( AValue : byte ) ;
begin
  Untested;
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_CONFIG_NOMINAL_BATTERY_VOLTAGE, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigAccessoryMounted : boolean;
begin
  Result := GetBoolValue ( REG_BATTERY_CONFIG_ACCESSORY_MOUNTED );
end;

procedure TBionXBattery.SetConfigAccessoryMounted ( AValue : boolean ) ;
begin
  SetBoolValue ( REG_BATTERY_CONFIG_ACCESSORY_MOUNTED, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigAccessoryEnabled : boolean;
begin
  Result := GetBoolValue ( REG_BATTERY_CONFIG_ACCESSORY_ENABLED );
end;

procedure TBionXBattery.SetConfigAccessoryEnabled ( AValue : boolean ) ;
begin
  SetBoolValue ( REG_BATTERY_CONFIG_ACCESSORY_ENABLED, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigAccessoryVoltage : double;
begin
  if CheckVersion ( 0, 255, 0, 255, 0, 52 ) then
    Result := GetByteValue ( REG_BATTERY_CONFIG_ACCESSORY_VOLTAGE ) * 6 + 6
  else
    if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
      Result := GetByteValue ( REG_BATTERY_CONFIG_ACCESSORY_VOLTAGE ) * 0.1
    else
      Result := 0;
end;

procedure TBionXBattery.SetConfigAccessoryVoltage ( AValue : double ) ;
begin
  if CheckVersion ( 0, 255, 0, 255, 0, 52 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_BATTERY_CONFIG_ACCESSORY_VOLTAGE, round ( ( AValue - 6 ) / 6 ) )
    finally
      LockProtection;
    end;
  end
  else
    if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    begin
      UnlockProtection;
      try
        SetByteValue ( REG_BATTERY_CONFIG_ACCESSORY_VOLTAGE, round ( AValue / 0.1 ) );
      finally
        LockProtection;
      end;
    end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigILMD : double;
begin
  Result := GetByteValue ( REG_BATTERY_CONFIG_ILMD ) * 0.54835;
end;

procedure TBionXBattery.SetConfigILMD ( AValue : double ) ;
begin
  Untested;
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_CONFIG_ILMD, round ( AValue / 0.54835 ) );;
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigMaxCharge : double;
begin
  if CheckVersion ( 80, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_BATTERY_CONFIG_MAX_CHARGE_HI ) * CURRENT_FACTOR
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigMaxCharge ( AValue: double ) ;
begin
  if CheckVersion ( 80, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_BATTERY_CONFIG_MAX_CHARGE_HI, round ( AValue / CURRENT_FACTOR ));
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigMaxDischarge : double;
begin
  if CheckVersion ( 80, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_BATTERY_CONFIG_MAX_DISCHARGE_HI ) * CURRENT_FACTOR
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigMaxDischarge ( AValue: double ) ;
begin
  if CheckVersion ( 80, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_BATTERY_CONFIG_MAX_DISCHARGE_HI, round ( AValue / CURRENT_FACTOR ));;
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigCellCapacity: double;
begin
  if CheckVersion ( 80, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_BATTERY_CONFIG_CELLCAPACITY_HI ) / 1000
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigCellCapacity ( AValue: double ) ;
begin
  if CheckVersion ( 80, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_BATTERY_CONFIG_CELLCAPACITY_HI, round( AValue * 1000 ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigPackSerial: byte;
begin
  if CheckVersion ( 80, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_BATTERY_CONFIG_PACK_SERIAL )
  else
    Result := 0;
end;


procedure TBionXBattery.SetConfigPackSerial ( AValue : byte ) ;
begin
  Untested;
  if CheckVersion ( 80, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_BATTERY_CONFIG_PACK_SERIAL, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigPackParallel: byte;
begin
  if CheckVersion ( 80, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_BATTERY_CONFIG_PACK_PARALLEL )
  else
    Result := 0;
end;

procedure TBionXBattery.SetConfigPackParallel ( AValue: byte ) ;
begin
  if CheckVersion ( 80, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_BATTERY_CONFIG_PACK_PARALLEL, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigNAC : word;
begin
  Result := GetWordValue ( REG_BATTERY_CONFIG_NAC_RADDR_HI );
end;

procedure TBionXBattery.SetConfigNAC ( AValue : word ) ;
begin
  Untested;
  UnlockProtection;
  try
    SetWordValue ( REG_BATTERY_CONFIG_NAC_HI, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigForceDone: boolean;
begin
  Result := GetByteValue ( REG_BATTERY_CONFIG_FORCE_DONE ) = BATTERY_CONFIG_FORCE_DONE;
end;

procedure TBionXBattery.SetConfigForceDone ( AValue: boolean ) ;
begin
  Untested;
  if AValue then
    SetByteValue ( REG_BATTERY_CONFIG_FORCE_DONE, BATTERY_CONFIG_FORCE_DONE );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigShutdown: boolean;
begin
  Result := GetBoolValue ( REG_BATTERY_CONFIG_SHUTDOWN );
end;

procedure TBionXBattery.SetConfigShutdown ( AValue: boolean ) ;
begin
  SetBoolValue ( REG_BATTERY_CONFIG_SHUTDOWN, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigVPower : boolean;
begin
  Result := GetBoolValue ( REG_BATTERY_CONFIG_POWER_VOLTAGE_ENABLE );
end;

procedure TBionXBattery.SetConfigVPower ( AValue : boolean ) ;
begin
  Untested;
  SetBoolValue ( REG_BATTERY_CONFIG_POWER_VOLTAGE_ENABLE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigVControl : boolean;
begin
  Result := GetBoolValue ( REG_BATTERY_CONFIG_CONTROL_VOLTAGE_ENABLE );
end;

procedure TBionXBattery.SetConfigVControl ( AValue : boolean ) ;
begin
  Untested;
  SetBoolValue ( REG_BATTERY_CONFIG_CONTROL_VOLTAGE_ENABLE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigVBattInt : boolean;
begin
  Result := GetBoolValue ( REG_BATTERY_CONFIG_BATTINT_VOLTAGE_ENABLE );
end;

procedure TBionXBattery.SetConfigVBattInt ( AValue : boolean ) ;
begin
  Untested;
  SetBoolValue ( REG_BATTERY_CONFIG_BATTINT_VOLTAGE_ENABLE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetConfigCapSenseMode : byte;
begin
  Result := GetByteValue ( REG_BATTERY_CONFIG_CAP_SENSE_MODE );
end;

procedure TBionXBattery.SetConfigCapSenseMode ( AValue : byte ) ;
begin
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_CONFIG_CAP_SENSE_MODE, AValue );
  finally
    LockProtection;
  end;
end;

function TBionXBattery.GetConfigChargerCalibrationSourceFlags : byte;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CByteValue ( REG_I2C_CONFIG_CHARGER_CALIBRATION_SOURCE_FLAGS )
  else
    Result := 0;
end;

function TBionXBattery.GetConfigChargerVoltageCalibrationValue : double;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_CONFIG_CHARGER_VOLTAGE_CALIBRATION_VALUE_HI )
  else
    Result := 0;
end;

function TBionXBattery.GetConfigChargerCurrentCalibrationValue : double;
begin
  if CheckVersion ( 0, 255, 0, 255, 60, 255 ) then
    Result := ReadI2CWordValue ( REG_I2C_CONFIG_CHARGER_CURRENT_CALIBRATION_VALUE_HI )
  else
    Result := 0;
end;

(******************************************************************************)

function TBionXBattery.GetProtectUnlock: byte;
begin
  Result := GetByteValue ( REG_BATTERY_PROTECT_UNLOCK );
end;

procedure TBionXBattery.SetProtectUnlock ( AValue: byte ) ;
begin
  SetByteValue ( REG_BATTERY_PROTECT_UNLOCK, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetProtectMode : byte;
begin
  Result := GetByteValue ( REG_BATTERY_PROTECT_MODE );
end;

procedure TBionXBattery.SetProtectMode(AValue: byte);
begin
  Untested;
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_PROTECT_MODE, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXBattery.GetProtectControl : byte;
begin
  Result := GetByteValue ( REG_BATTERY_PROTECT_CONTROL );
end;

procedure TBionXBattery.SetProtectControl(AValue: byte);
begin
  Untested;
  UnlockProtection;
  try
    SetByteValue ( REG_BATTERY_PROTECT_CONTROL, AValue );
  finally
    LockProtection;
  end;
end;


(******************************************************************************)
(******************************************************************************)
(*                                                                            *)
(*                                                                            *)
(* TBionXMotor                                                                *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
(******************************************************************************)

procedure TBionXMotor.UnlockProtection;
begin
  if FUnlockLevel = 0 then
    ProtectUnlock := MOTOR_PROTECT_UNLOCK_KEY;
  inc ( FUnlockLevel );
end;

procedure TBionXMotor.LockProtection;
begin
  if FUnlockLevel > 0 then
    dec ( FUnlockLevel );
  if FUnlockLevel = 0 then
    ProtectUnlock := MOTOR_PROTECT_LOCK_KEY;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetSoftwareVersion: byte;
begin
  if FSoftwareVersion = 0 then
    FSoftwareVersion := GetByteValue( REG_MOTOR_REV_SW );
  Result := FSoftwareVersion;
end;

function TBionXMotor.GetHardwareVersion: byte;
begin
  if FHardwareVersion = 0 then
    FHardwareVersion := GetByteValue( REG_MOTOR_REV_HW );
  Result := FHardwareVersion;
end;

function TBionXMotor.GetSubVersion: byte;
begin
  if FSubVersion = 0 then
    FSubVersion := GetByteValue( REG_MOTOR_REV_SUB );
  Result := FSubVersion;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetPartNumber: word;
begin
  Result := GetWordValue( REG_MOTOR_SN_PN_HI );
end;

function TBionXMotor.GetLocation : byte;
begin
  Result := GetByteValue ( REG_MOTOR_SN_LOCATION );
end;

function TBionXMotor.GetManufacturingDate : TDate;
begin
  Result := EncodeDate ( GetByteValue ( REG_MOTOR_SN_YEAR ) + 2000,
                         GetByteValue ( REG_MOTOR_SN_MONTH ),
                         GetByteValue ( REG_MOTOR_SN_DAY )
                       );
end;

function TBionXMotor.GetItemNumber: word;
begin
  Result := GetWordValue ( REG_MOTOR_SN_ITEM_HI );
end;

function TBionXMotor.GetOEM : word;
begin
  Result := GetWordValue ( REG_MOTOR_SN_OEM_HI );
end;

function TBionXMotor.GetProduct : word;
begin
  Result := GetWordValue ( REG_MOTOR_SN_PRODUCT_HI );
end;

function TBionXMotor.GetStatorType : byte;
begin
  Result := GetByteValue ( REG_MOTOR_SN_STATOR_TYPE );
end;

(******************************************************************************)

function TBionXMotor.GetProtectUnlock: byte;
begin
  Result := GetByteValue( REG_MOTOR_PROTECT_UNLOCK );
end;

procedure TBionXMotor.SetProtectUnlock ( AValue: byte ) ;
begin
  SetByteValue( REG_MOTOR_PROTECT_UNLOCK, AValue );
end;

(******************************************************************************)

function TBionXMotor.GetGeometryCirc: word;
begin
  Result := GetWordValue ( REG_MOTOR_GEOMETRY_CIRC_HI );
end;

procedure TBionXMotor.SetGeometryCirc(AValue: word);
begin
  UnlockProtection;
  try
    SetWordValue ( REG_MOTOR_GEOMETRY_CIRC_HI, AValue );
  finally
    LockProtection;
  end;
end;

(******************************************************************************)

function TBionXMotor.GetAssistLevel : double;
begin
  Result := shortint(GetByteValue ( REG_MOTOR_ASSIST_LEVEL )) * ASSIST_FACTOR;
end;

procedure TBionXMotor.SetAssistLevel ( AValue : double ) ;
begin
  Untested;
  // type conversion?
  SetByteValue ( REG_MOTOR_ASSIST_LEVEL, byte ( round ( AValue / ASSIST_FACTOR ) ));
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistLevelOffSlope : double;
begin
  if CheckVersion ( 97, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_ASSIST_LEVEL_OFFSLOPE_HI ) * 3.05
  else
    Result := 0;
end;

procedure TBionXMotor.SetAssistLevelOffSlope ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 97, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_ASSIST_LEVEL_OFFSLOPE_HI, round ( AValue / 3.05 ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistDirection : byte;
begin
  Result := GetByteValue ( REG_MOTOR_ASSIST_DIRECTION );
end;

procedure TBionXMotor.SetAssistDirection ( AValue : byte ) ;
begin
  Untested;
  SetByteValue ( REG_MOTOR_ASSIST_DIRECTION, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistMaxSpeed: double;
begin
  if CheckVersion ( 76, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_ASSIST_MAXSPEED )
  else
    Result := 0;
end;

procedure TBionXMotor.SetAssistMaxSpeed(AValue: double);
begin
  if CheckVersion ( 76, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_ASSIST_MAXSPEED, round(AValue) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistMaxSpeedDerateDelta : double;
begin
  if CheckVersion ( 97, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_ASSIST_MAXSPEED_DERATE_DELTA ) * 9.091
  else
    Result := 0;
end;

procedure TBionXMotor.SetAssistMaxSpeedDerateDelta ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 97, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_ASSIST_MAXSPEED_DERATE_DELTA, round ( AValue / 9.091 ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistVQDynamicFlag : boolean;
begin
  if CheckVersion ( 77, 255, 0, 255, 0, 255 ) then
    Result := GetBoolValue ( REG_MOTOR_ASSIST_DYNAMIC_FLAG )
  else
    Result := false;
end;

procedure TBionXMotor.SetAssistVQDynamicFlag ( AValue : boolean ) ;
begin
  // Protet = "2" !?
  Untested;
  if CheckVersion ( 77, 255, 0, 255, 0, 255 ) then
  begin
    try
      SetBoolValue ( REG_MOTOR_ASSIST_DYNAMIC_FLAG, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistStatorPartNumber : word;
begin
  if CheckVersion ( 90, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_ASSIST_STATOR_PN_HI )
  else
    Result := 0;
end;

procedure TBionXMotor.SetAssistStatorPartNumber ( AValue : word ) ;
begin
  Untested;
  if CheckVersion ( 90, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_ASSIST_STATOR_PN_HI, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistRegenInflex : double;
begin
  if CheckVersion ( 97, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_ASSIST_REGEN_INFLEX ) * 9.091
  else
    Result := 0;
end;

procedure TBionXMotor.SetAssistRegenInflex ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 97, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_ASSIST_REGEN_INFLEX, round ( AValue / 9.091 ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistWalkSpeedDecreaseStart : double;
begin
  if CheckVersion ( 99, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_ASSIST_WALK_SPEED_DECREASE_START ) * SPEED_FACTOR
  else
    Result := 0;
end;

procedure TBionXMotor.SetAssistWalkSpeedDecreaseStart ( AValue : double ) ;
begin
  if CheckVersion ( 99, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_ASSIST_WALK_SPEED_DECREASE_START, round ( AValue / SPEED_FACTOR ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistWalkSpeedDecreaseEnd : double;
begin
  if CheckVersion ( 99, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_ASSIST_WALK_SPEED_DECREASE_END ) * SPEED_FACTOR
  else
    Result := 0;
end;

procedure TBionXMotor.SetAssistWalkSpeedDecreaseEnd ( AValue : double ) ;
begin
  if CheckVersion ( 99, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_ASSIST_WALK_SPEED_DECREASE_END, round ( AValue / SPEED_FACTOR ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistWalkMaxLevel : double;
begin
  if CheckVersion ( 99, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_ASSIST_WALK_LEVEL_MAX ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXMotor.SetAssistWalkMaxLevel ( AValue : double ) ;
begin
  if CheckVersion ( 99, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_ASSIST_WALK_LEVEL_MAX, round ( AValue / ASSIST_FACTOR ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistWalkLevel : double;
begin
  if CheckVersion ( 99, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_ASSIST_WALK_LEVEL ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXMotor.SetAssistWalkLevel ( AValue : double ) ;
begin
  if CheckVersion ( 99, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_MOTOR_ASSIST_WALK_LEVEL, round ( AValue / ASSIST_FACTOR ));
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetAssistLowSpeedRampFlag : boolean;
begin
  if CheckVersion ( 98, 255, 0, 255, 0, 255 ) then
    Result := GetBoolValue ( REG_MOTOR_ASSIST_LOWSPEED_RAMP_FLAG )
  else
    Result := false;
end;

procedure TBionXMotor.SetAssistLowSpeedRampFlag ( AValue : boolean ) ;
begin
  Untested;
  if CheckVersion ( 98, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetBoolValue ( REG_MOTOR_ASSIST_LOWSPEED_RAMP_FLAG, AValue )
    finally
      LockProtection;
    end;
  end;
end;

(******************************************************************************)

function TBionXMotor.GetTorqueGaugePolarity : byte;
begin
  Result := GetByteValue ( REG_MOTOR_TORQUE_GAUGE_POLARITY );
end;

procedure TBionXMotor.SetTorqueGaugePolarity ( AValue : byte ) ;
begin
  Untested;
  UnlockProtection;
  try
    SetByteValue ( REG_MOTOR_TORQUE_GAUGE_POLARITY, AValue );
  finally
    LockProtection;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTorqueGaugeType : byte;
begin
  if CheckVersion ( 77, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_TORQUE_GAUGE_TYPE )
  else
    Result := 0;
end;

procedure TBionXMotor.SetTorqueGaugeType ( AValue : byte ) ;
begin
  Untested;
  if CheckVersion ( 77, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_TORQUE_GAUGE_TYPE, AValue );
     finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTorqueGaugeValue : double;
begin
  if CheckVersion ( 77, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_TORQUE_GAUGE_VALUE ) * ASSIST_FACTOR
  else
    Result := 0;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTorqueGaugeVoltage : double;
begin
  if CheckVersion ( 77, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_TORQUE_GAUGE_VOLTAGE_HI ) * 0.000076295 + 5
  else
    Result := 0;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTorqueGaugeNoise : double;
begin
  if CheckVersion ( 81, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_TORQUE_GAUGE_NOISE_HI ) * 0.0015259
  else
    Result := 0;
end;

procedure TBionXMotor.SetTorqueGaugeNoise ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 81, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_TORQUE_GAUGE_NOISE_HI, round ( AValue / 0.0015259 ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTorqueGaugeDelay : double;
begin
  if CheckVersion ( 81, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_TORQUE_GAUGE_DELAY_HI ) * 0.001
  else
    Result := 0;
end;

procedure TBionXMotor.SetTorqueGaugeDelay ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 81, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_TORQUE_GAUGE_DELAY_HI, round ( AValue / 0.001 ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTorqueGaugeSpeed : double;
begin
  if CheckVersion ( 81, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_TORQUE_GAUGE_SPEED ) * 9.091
  else
    Result := 0;
end;

procedure TBionXMotor.SetTorqueGaugeSpeed ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 81, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_TORQUE_GAUGE_SPEED, round ( AValue / 9.091 ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTorqueGaugeReference : double;
begin
  if CheckVersion ( 81, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_TORQUE_GAUGE_REFERENCE_HI ) * -0.000076295 + 5
  else
    Result := 0;
end;

procedure TBionXMotor.SetTorqueGaugeReference ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 81, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_TORQUE_GAUGE_REFERENCE_HI, ( round ( ( AValue - 5 ) / -0.000076295 ) ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTorqueGaugeGain : double;
begin
  if CheckVersion ( 90, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_TORQUE_GAUGE_GAIN ) * ASSIST_FACTOR
  else
    Result := 0;
end;

procedure TBionXMotor.SetTorqueGaugeGain ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 90, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_TORQUE_GAUGE_GAIN, round ( AValue / ASSIST_FACTOR ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTorqueGaugeMaxVoltage : double;
begin
  if CheckVersion ( 98, 255, 2, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_TORQUE_GAUGE_MAX_VOLTAGE ) * 0.019608
  else
    Result := 0;
end;

procedure TBionXMotor.SetTorqueGaugeMaxVoltage ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 98, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_TORQUE_GAUGE_MAX_VOLTAGE, round ( AValue / 0.019608 ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetTorqueGaugeMaxVoltageDelay : double;
begin
  if CheckVersion ( 98, 255, 2, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_TORQUE_GAUGE_MAX_VOLTAGE_DELAY ) * 0.1
  else
    Result := 0;
end;

procedure TBionXMotor.SetTorqueGaugeMaxVoltageDelay ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 98, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_TORQUE_GAUGE_MAX_VOLTAGE_DELAY, round ( AValue / 0.1 ) );
    finally
      LockProtection;
    end;
  end;
end;

(******************************************************************************)

function TBionXMotor.GetPreferenceRegion : byte;
begin
  if CheckVersion ( 76, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_PREFERENCE_REGION )
  else
    Result := 0;
end;

procedure TBionXMotor.SetPreferenceRegion ( AValue : byte ) ;
begin
  Untested;
  if CheckVersion ( 76, 255, 0, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_PREFERENCE_REGION, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(******************************************************************************)

function TBionXMotor.GetConfigCommMode : word;
begin
  if CheckVersion ( 0, 82, 2, 255, 0, 255 ) then
    Result :=   GetByteValue ( REG_MOTOR_CONFIG_COMMUNICATION_MODE_LO )
  else
    if CheckVersion (83, 255, 2, 255, 0, 255 ) then
      Result :=   GetByteValue ( REG_MOTOR_CONFIG_COMMUNICATION_MODE_HI ) shl 8
                + GetByteValue ( REG_MOTOR_CONFIG_COMMUNICATION_MODE_LO )
    else
      Result := 0;
end;

procedure TBionXMotor.SetConfigCommMode ( AValue : word ) ;
begin
  Untested;
  if CheckVersion ( 0, 82, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetByteValue ( REG_MOTOR_CONFIG_COMMUNICATION_MODE_LO, AValue and $FF )
    finally
      LockProtection;
    end;
  end
  else
    if CheckVersion (83, 255, 2, 255, 0, 255 ) then
    begin
      UnlockProtection;
      try
        SetByteValue ( REG_MOTOR_CONFIG_COMMUNICATION_MODE_HI, AValue shr 8 );
        SetByteValue ( REG_MOTOR_CONFIG_COMMUNICATION_MODE_LO, AValue and $FF );
      finally
        LockProtection;
      end;
   end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetConfigEnablePWMLimit : boolean;
begin
  if CheckVersion ( 96, 255, 2, 255, 0, 255 ) then
    Result := GetBoolValue ( REG_MOTOR_CONFIG_PWM_LIMIT_ENABLE )
  else
    Result := false;
end;

procedure TBionXMotor.SetConfigEnablePWMLimit ( AValue : boolean ) ;
begin
  Untested;
  if CheckVersion ( 96, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetBoolValue ( REG_MOTOR_CONFIG_PWM_LIMIT_ENABLE, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(******************************************************************************)

function TBionXMotor.GetStatusMain : byte;
begin
  if CheckVersion ( 93, 255, 2, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_STATUS_MAIN )
  else
    result := 0;
end;

procedure TBionXMotor.SetStatusMain ( AValue : byte ) ;
begin
  Untested;
  if CheckVersion ( 93, 255, 2, 255, 0, 255 ) then
    SetByteValue ( REG_MOTOR_STATUS_MAIN, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetStatusCodes : byte;
begin
  if CheckVersion ( 96, 255, 2, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_STATUS_CODES )
  else
    result := 0;
end;

function TBionXMotor.GetStatusCodesLatch : byte;
begin
  if CheckVersion ( 96, 255, 2, 255, 0, 255 ) then
    Result := GetByteValue ( REG_MOTOR_STATUS_CODES_LATCH )
  else
    result := 0;
end;

function TBionXMotor.GetStatusSpeed : double;
begin
  Result := GetByteValue ( REG_MOTOR_STATUS_SPEED ) * 9.091;
end;

function TBionXMotor.GetStatusPowerMeter : double;
begin
  Result := GetByteValue ( REG_MOTOR_STATUS_POWER_METER ) * ASSIST_FACTOR;
end;

function TBionXMotor.GetStatusTemp: byte;
begin
  Result := GetByteValue ( REG_MOTOR_STATUS_TEMPERATURE );
end;

function TBionXMotor.GetStatusVPower : double;
begin
  Result := GetWordValue ( REG_MOTOR_STATUS_POWER_VOLTAGE_HI ) * VOLTAGE_FACTOR
end;

function TBionXMotor.GetStatusV12V : double;
begin
  Result := GetWordValue ( REG_MOTOR_STATUS_12V_VOLTAGE_HI ) * VOLTAGE_FACTOR
end;

function TBionXMotor.GetStatusV5V : double;
begin
  Result := GetWordValue ( REG_MOTOR_STATUS_5V_VOLTAGE_HI ) * VOLTAGE_FACTOR
end;

(******************************************************************************)

function TBionXMotor.GetStatsMaxVPower : double;
begin
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_STATISTIC_MAX_POWER_VOLTAGE_HI ) * VOLTAGE_FACTOR
  else
    Result := 0;
end;

procedure TBionXMotor.SetStatsMaxVPower ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_STATISTIC_MAX_POWER_VOLTAGE_HI, round ( AValue / VOLTAGE_FACTOR ) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetStatsMaxVTemp : double;
begin
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_STATISTIC_MAX_TEMPERATURE_HI )
  else
    Result := 0;
end;

procedure TBionXMotor.SetStatsMaxVTemp ( AValue : double ) ;
begin
  Untested;
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_STATISTIC_MAX_TEMPERATURE_HI, round(AValue) );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetStatsOdo : word;
begin
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
  begin
    // ??? UnlockProtection;
    Result := GetWordValue ( REG_MOTOR_STATISTIC_ODOMETER_HI );
  end
  else
    Result := 0;
end;

procedure TBionXMotor.SetStatsOdo ( AValue : word ) ;
begin
  Untested;
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_STATISTIC_ODOMETER_HI, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetStatsChronoHours : word;
begin
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_STATISTIC_CHRONO_HOURS_HI )
  else
    Result := 0;
end;

procedure TBionXMotor.SetStatsChronoHours ( AValue : word ) ;
begin
  Untested;
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_STATISTIC_CHRONO_HOURS_HI, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetStatsChronoSeconds : word;
begin
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_STATISTIC_CHRONO_SECONDS_HI )
  else
    Result := 0;
end;

procedure TBionXMotor.SetStatsChronoSeconds ( AValue : word ) ;
begin
  Untested;
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_STATISTIC_CHRONO_SECONDS_HI, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetStatsHallDCHS : word;
begin
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_STATISTIC_HALL_DCHS_HI )
  else
    Result := 0;
end;

procedure TBionXMotor.SetStatsHallDCHS ( AValue : word ) ;
begin
  Untested;
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_STATISTIC_HALL_DCHS_HI, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetStatsHallTrans : word;
begin
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_STATISTIC_HALL_TRANS_HI )
  else
    Result := 0;
end;

procedure TBionXMotor.SetStatsHallTrans ( AValue : word ) ;
begin
  Untested;
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_STATISTIC_HALL_TRANS_HI, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetStatsHallRing : word;
begin
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_STATISTIC_HALL_RING_HI )
  else
    Result := 0;
end;

procedure TBionXMotor.SetStatsHallRing ( AValue : word ) ;
begin
  Untested;
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_STATISTIC_HALL_RING_HI, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(*----------------------------------------------------------------------------*)

function TBionXMotor.GetStatsHallLost : word;
begin
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
    Result := GetWordValue ( REG_MOTOR_STATISTIC_HALL_LOST_HI )
  else
    Result := 0;
end;

procedure TBionXMotor.SetStatsHallLost ( AValue : word ) ;
begin
  Untested;
  if CheckVersion ( 83, 255, 2, 255, 0, 255 ) then
  begin
    UnlockProtection;
    try
      SetWordValue ( REG_MOTOR_STATISTIC_HALL_LOST_HI, AValue );
    finally
      LockProtection;
    end;
  end;
end;

(******************************************************************************)
(******************************************************************************)
(*                                                                            *)
(*                                                                            *)
(* TBionXSensor                                                               *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
(******************************************************************************)

function TBionXSensor.GetSoftwareVersion: byte;
begin
  if FSoftwareVersion = 0 then
    FSoftwareVersion := GetByteValue( REG_SENSOR_REV_SW );
  Result := FSoftwareVersion;
end;

function TBionXSensor.GetHardwareVersion: byte;
begin
  if FHardwareVersion = 0 then
    FHardwareVersion := GetByteValue( REG_SENSOR_REV_HW );
  Result := FHardwareVersion;
end;

function TBionXSensor.GetSubVersion: byte;
begin
  if FSubVersion = 0 then
    // !! do NOT use CheckVersion here as that will cause recursion call
    if SoftwareVersion >= 15 then
      FSubVersion := GetByteValue( REG_SENSOR_REV_SUB )
    else
      FSubVersion := 0;
  Result := FSubVersion;
end;

(*----------------------------------------------------------------------------*)

function TBionXSensor.GetPartNumber: word;
begin
  Result := GetWordValue( REG_SENSOR_SN_PN_HI );
end;

function TBionXSensor.GetLocation: byte;
begin
  Result := GetByteValue ( REG_SENSOR_SN_LOCATION );
end;

function TBionXSensor.GetManufacturingDate: TDate;
begin
  Result := EncodeDate ( GetByteValue ( REG_SENSOR_SN_YEAR ) + 2000,
                         GetByteValue ( REG_SENSOR_SN_MONTH ),
                         GetByteValue ( REG_SENSOR_SN_DAY )
                       );
end;

function TBionXSensor.GetItemNumber: word;
begin
  Result := GetWordValue( REG_SENSOR_SN_ITEM_HI );
end;

(*----------------------------------------------------------------------------*)

function TBionXSensor.GetConfigMode: byte;
begin
  if FMode = 0 then
    FMode := GetByteValue ( REG_SENSOR_CONFIG_MODE );
  Result := FMode;
end;

procedure TBionXSensor.SetConfigMode ( AValue: byte ) ;
begin
  Untested;
  SetByteValue ( REG_SENSOR_CONFIG_MODE, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXSensor.GetConfigGaugeGain: double;
begin
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_SENSOR_CONFIG_GAUGE_GAIN_HI ) * 0.00390625
  else
    Result := 0;
end;

procedure TBionXSensor.SetConfigGaugeGain ( AValue: double ) ;
begin
  Untested;
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    SetWordValue ( REG_SENSOR_CONFIG_GAUGE_GAIN_HI, round ( AValue / 0.00390625 ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXSensor.GetConfigRampUpSteps: word;
begin
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_SENSOR_CONFIG_RAMP_UP_STEPS_HI )
  else
    Result := 0;
end;

procedure TBionXSensor.SetConfigRampUpSteps ( AValue: word ) ;
begin
  Untested;
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    SetWordValue ( REG_SENSOR_CONFIG_RAMP_UP_STEPS_HI, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXSensor.GetConfigDecayDelay: word;
begin
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_SENSOR_CONFIG_DECAY_DELAY_HI )
  else
    Result := 0;
end;

procedure TBionXSensor.SetConfigDecayDelay ( AValue: word ) ;
begin
  Untested;
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    SetWordValue ( REG_SENSOR_CONFIG_DECAY_DELAY_HI, AValue )
end;

(*----------------------------------------------------------------------------*)

function TBionXSensor.GetConfigDecaySteps: word;
begin
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_SENSOR_CONFIG_DECAY_STEPS_HI )
  else
    Result := 0;
end;

procedure TBionXSensor.SetConfigDecaySteps ( AValue: word ) ;
begin
  Untested;
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    SetWordValue ( REG_SENSOR_CONFIG_DECAY_STEPS_HI, AValue )
end;

(*----------------------------------------------------------------------------*)

function TBionXSensor.GetConfigSpeedThreshold: double;
begin
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    Result := GetWordValue ( REG_SENSOR_CONFIG_SPEED_THRESHOLD_HI ) * 9.091
  else
    Result := 0;
end;

procedure TBionXSensor.SetConfigSpeedThreshold ( AValue: double ) ;
begin
  Untested;
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    SetWordValue ( REG_SENSOR_CONFIG_SPEED_THRESHOLD_HI, round ( AValue / 9.091 ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXSensor.GetConfigRampActiveOverThreshold: boolean;
begin
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    Result := GetBoolValue ( REG_SENSOR_CONFIG_RAMP_ACTIVE_OVER_THRESHOLD )
 else
    Result := false;
end;

procedure TBionXSensor.SetConfigRampActiveOverThreshold ( AValue: boolean ) ;
begin
  Untested;
  if CheckVersion ( 12, 255, 0, 255, 0, 255 ) then
    SetBoolValue ( REG_SENSOR_CONFIG_RAMP_ACTIVE_OVER_THRESHOLD, AValue );
end;

(*----------------------------------------------------------------------------*)

function TBionXSensor.GetConfigInputOffset: double;
begin
  if CheckVersion ( 16, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_SENSOR_CONFIG_INPUT_OFFSET ) * 0.012890625
  else
    Result := 0;
end;

procedure TBionXSensor.SetConfigInputOffset ( AValue: double ) ;
begin
  Untested;
  if CheckVersion ( 16, 255, 0, 255, 0, 255 ) then
    SetByteValue ( REG_SENSOR_CONFIG_INPUT_OFFSET, round ( AValue / 0.012890625 ) );
end;

(*----------------------------------------------------------------------------*)

function TBionXSensor.GetStatusVTorque: double;
begin
  if CheckVersion ( 15, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_SENSOR_STATUS_TORQUE_VOLTAGE ) * 0.012941
  else
    Result := 0;
end;


function TBionXSensor.GetStatusCadence: double;
begin
//??  Untested;
  if CheckVersion ( 15, 255, 0, 255, 0, 255 ) then
  begin
    if ConfigMode = 0 then
      Result := GetByteValue ( REG_SENSOR_STATUS_CADENCE ) * 18.75
    else
      if ConfigMode = 1 then
        Result := GetByteValue ( REG_SENSOR_STATUS_CADENCE ) * 8.3333
      else
        Result := 0;
  end
  else
    Result := 0;
end;

function TBionXSensor.GetStatusVOutput: double;
begin
  if CheckVersion ( 15, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_SENSOR_STATUS_OUTPUT_VOLTAGE ) * 0.012941
  else
    Result := 0;
end;

function TBionXSensor.GetStatusPulseCounter: Byte;
begin
  if CheckVersion ( 15, 255, 0, 255, 0, 255 ) then
    Result := GetByteValue ( REG_SENSOR_STATUS_PULSE_COUNTER )
  else
    Result := 0;
end;

(******************************************************************************)
(******************************************************************************)
(*                                                                            *)
(*                                                                            *)
(* TKeepAliveThread                                                           *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
(******************************************************************************)

constructor TKeepAliveThread.Create ( aBattery : TBionXBattery );
begin
  inherited Create ( false );
  FBattery := aBattery;
end;

procedure TKeepAliveThread.Execute;
const
  cycles = 100;
  delay  = 10;
var
  cnt : integer;
begin
  cnt := cycles;
  while not Terminated do
  begin
    if cnt = 0 then
    begin
      cnt := cycles;
      FBattery.ChargeLevel;
    end
    else
    begin
      sleep(delay);
      dec(cnt);
    end;
  end;
end;

(******************************************************************************)
(******************************************************************************)
(*                                                                            *)
(*                                                                            *)
(* TBionXBike                                                                 *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)
(******************************************************************************)

constructor TBionXBike.Create;
begin
  inherited Create;

  // create the BionXComponents with a pointer to bike's FCANIntf,
  // which will be set and valid after successfull connect
  FConsole := TBionXConsole.Create ( @FCANIntf, ID_CONSOLE_SLAVE );
  FMotor   := TBionXMotor.Create ( @FCANIntf, ID_MOTOR );
  FBattery := TBionxBattery.Create ( @FCANIntf, ID_BATTERY );
  FSensor  := TBionxSensor.Create ( @FCANIntf, ID_SENSOR );
end;

destructor TBionXBike.Destroy;
begin
  Disconnect;
  FConsole.Free;
  FMotor.Free;
  FBattery.Free;
  FSensor.Free;
  inherited;
end;

function TBionXBike.Connect ( CANIntf : TCANInterface ) : boolean;
begin
  FCANIntf := CANIntf;

  Result := FCANIntf.Connect;
  if Result then
    KeepAlive := true
  else
  begin
    MessageDlg ( 'Connect error', 'Error connecting to CAN'#13+FCANIntf.LastError, mtError, [mbOK], 0, mbOK );
    Disconnect;
  end;
end;


procedure TBionXBike.Disconnect;
begin
  if assigned ( FCANIntf ) then
  begin
    KeepAlive := false;
    FCANIntf.Disconnect;
    FCANIntf.Free;
    FCANIntf := nil;
  end;
end;

function TBionXBike.SetToSlaveMode : boolean;
var
  Retries : integer;
begin
  Result := false;
  Retries := 3;
  repeat
    try
      dec ( Retries );
      Console.StatusSlave := true;
      sleep ( 100 );
      Result := Console.StatusSlave;
    except
      // the Console.StatusSlave get and set method may raise an exception,
      // but do not report any error here. An error will be shown at the
      // end of the retry loop, when setting slavemode failed
    end;
  until Result or ( Retries = 0 );
  if not Result then
    MessageDlg ( 'Set to slave error', 'Error setting console to slave mode'#13+FCANIntf.LastError, mtError, [mbOK], 0, mbOK )
end;

procedure TBionXBike.Shutdown;
begin
  Battery.ConfigShutdown := true;
end;


function TBionXBike.GetKeepAlive: boolean;
begin
  Result := assigned ( FKeepAliveThread );
end;

procedure TBionXBike.SetKeepAlive ( AValue: boolean ) ;
begin
//  exit;
//  if Console.SoftwareVersion >= 63 then
//  if Battery.SoftwareVersion >= 112 then
  if FForceKeepAlive then
  begin
    if KeepAlive <> AValue then
    begin
      if AValue then
      begin
        FKeepAliveThread := TKeepAliveThread.Create ( Battery );
      end
      else
      begin
        FKeepAliveThread.Terminate;
        FKeepAliveThread.WaitFor;
        FKeepAliveThread := nil;
      end;
    end;
  end;
end;


procedure TBionXBike.SetWheelCircumference( Circumference : word ) ;
begin
  Console.GeometryCirc := Circumference;
  Motor.WheelCircumference := Circumference;
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
  Motor.AssistMaxSpeed := Speed;
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

procedure TBionXBike.SetBrakeSensor(SensorEnabled: boolean; SensorType: byte);
begin
  Console.AssistBrakeFlag := SensorEnabled;
  if ( SensorType <= 4 ) then
    Console.AssistBrakePolarity := SensorType;
end;


end.

