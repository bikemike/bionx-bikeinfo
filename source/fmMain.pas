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

unit fmMain;

{$mode objfpc}{$H+}

interface

uses  tinycandrv,
//  TypInfo,
  Graphics, Classes, SysUtils, FileUtil, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Controls, Spin, Buttons, Menus, EditBtn,
  BionX, CANAdapter, TinyCANAdapter, FileCANAdapter;


{ TfrmBionXMain }
type
  TfrmBionXMain = class(TForm)
    Bevel6: TBevel;
    BitBtn1: TBitBtn;
    btnAbout: TBitBtn;
    btnApplySensor: TBitBtn;
    btnMotorLock: TBitBtn;
    btnMotorUnlock: TBitBtn;
    btnReadSensor: TBitBtn;
    Button2: TButton;
    btnBatteryUnlock: TBitBtn;
    btnBatteryLock: TBitBtn;
    cbBatteryCellMonBalancerEnabled: TComboBox;
    cbBatteryChargerMode: TComboBox;
    cbConsoleAssistBrakeFlag: TComboBox;
    cbConsoleAssistBrakePolarity: TComboBox;
    cbBatteryStatusPermanentFailureFlags: TComboBox;
    cbBatteryConfigCapsenseMode: TComboBox;
    cbBatteryConfigShipMode: TComboBox;
    cbBatteryConfigVPower: TComboBox;
    cbBatteryConfigVControl: TComboBox;
    cbBatteryConfigVBattInt: TComboBox;
    cbConsolePreferenceLightButtonMode: TComboBox;
    cbMotorStatusMain: TComboBox;
    cbSensorConfigMode: TComboBox;
    cbMotorAssistDirection: TComboBox;
    cbMotorAssistVQDynamicFlag: TComboBox;
    cbMotorAssistLowSpeedRampFlag: TComboBox;
    cbMotorConfigEnablePwm: TComboBox;
    cbSensorConfigRampActiveOverThreshold: TComboBox;
    cbBatteryConfigCommMode: TComboBox;
    cbBatteryConfigAutoSwitchComm: TComboBox;
    cbBatteryConfigWakeOnPowerVoltage: TComboBox;
    cbBatteryConfigAllowBuckChargingOnBike: TComboBox;
    cbBatteryConfigAccessoryMounted: TComboBox;
    cbBatteryConfigAccessoryEnabled: TComboBox;
    cbBatteryConfigForceDone: TComboBox;
    edBatteryCalibCalibration01: TFloatSpinEdit;
    edBatteryCalibCalibration02: TFloatSpinEdit;
    edBatteryCalibCalibration03: TFloatSpinEdit;
    edBatteryCalibCalibration04: TFloatSpinEdit;
    edBatteryCalibCalibration05: TFloatSpinEdit;
    edBatteryCalibCalibration06: TFloatSpinEdit;
    edBatteryCalibCalibration07: TFloatSpinEdit;
    edBatteryCalibCalibration08: TFloatSpinEdit;
    edBatteryCalibCalibration09: TFloatSpinEdit;
    edBatteryCalibCalibration10: TFloatSpinEdit;
    edBatteryCalibCalibration11: TFloatSpinEdit;
    edBatteryCalibCalibration12: TFloatSpinEdit;
    edBatteryCalibCalibration13: TFloatSpinEdit;
    edBatteryCalibCalibration3V3: TFloatSpinEdit;
    edBatteryChargerCurrent: TFloatSpinEdit;
    edBatteryChargerFinalVoltage: TFloatSpinEdit;
    edBatteryCalibCapSense: TFloatSpinEdit;
    edBatteryGgDMFSD: TSpinEdit;
    edBatteryGgVoltageDivider: TFloatSpinEdit;
    edBatteryStatsCharge10: TSpinEdit;
    edBatteryStatsCharge20: TSpinEdit;
    edBatteryStatsCharge30: TSpinEdit;
    edBatteryStatsCharge40: TSpinEdit;
    edBatteryStatsCharge50: TSpinEdit;
    edBatteryStatsCharge60: TSpinEdit;
    edBatteryStatsCharge70: TSpinEdit;
    edBatteryStatsCharge80: TSpinEdit;
    edBatteryStatsCharge90: TSpinEdit;
    edBatteryStatusLeds: TSpinEdit;
    edBatteryStatsGgjrCalib: TSpinEdit;
    edBatteryStatsResetWdt: TSpinEdit;
    edBatteryStatsRTCResync: TSpinEdit;
    edBatteryStatsChargeTimeWorst: TSpinEdit;
    edBatteryStatsChargeTimeMean: TSpinEdit;
    edBatteryStatsLMDAdapt: TSpinEdit;
    edBatteryStatsBattCycles: TSpinEdit;
    edBatteryStatsBattFullCycles: TSpinEdit;
    edBatteryStatsPowerCycles: TSpinEdit;
    edBatteryStatsVBattMax: TFloatSpinEdit;
    edBatteryStatsVBattMin: TFloatSpinEdit;
    edBatteryStatsTBattMax: TSpinEdit;
    edBatteryStatsTBattMin: TSpinEdit;
    edBatteryRTCTime: TSpinEdit;
    edBatteryRTCLastChargeTimeStamp: TSpinEdit;
    edBatteryRTCCtrl: TSpinEdit;
    edBatteryConfigMaxPackTemperature: TSpinEdit;
    edBatteryConfigMinPackTemperature: TSpinEdit;
    edBatteryConfigMaxGgTemperature: TSpinEdit;
    edBatteryConfigMinGgTemperature: TSpinEdit;
    edBatteryConfigMaxCellDeltaVoltage: TFloatSpinEdit;
    edBatteryConfigTailLampIntensity: TSpinEdit;
    edBatteryConfigDeepSleepAfterLongInactivityPeriodDelay: TSpinEdit;
    edBatteryConfigDeepSleepLowSocDelay: TSpinEdit;
    edBatteryConfigDiag: TSpinEdit;
    edBatteryConfigType: TSpinEdit;
    edBatteryConfigVBattNominal: TSpinEdit;
    edBatteryConfigAccessoryVoltage: TFloatSpinEdit;
    edBatteryConfigILMD: TFloatSpinEdit;
    edBatteryConfigMaxCharge: TFloatSpinEdit;
    edBatteryConfigMaxDischarge: TFloatSpinEdit;
    edBatteryConfigCellCapacity: TFloatSpinEdit;
    edBatteryConfigPackSerial: TSpinEdit;
    edBatteryConfigPackParallel: TSpinEdit;
    edBatteryConfigNAC: TSpinEdit;
    edBatteryProtectMode: TSpinEdit;
    edBatteryProtectCtrl: TSpinEdit;
    edBatteryTimerAccessory: TSpinEdit;
    edBatteryTimerPower: TSpinEdit;
    edBatteryTimerPreCharge: TSpinEdit;
    edBatteryTimerMasterShutdown: TSpinEdit;
    edConsoleAssistMaxSpeed: TFloatSpinEdit;
    edConsoleAssistMinSpeed: TFloatSpinEdit;
    edConsoleConfigServiceDistance: TSpinEdit;
    edMotorGeometryCirc: TSpinEdit;
    edMotorAssitStatorPn: TSpinEdit;
    edConsolePreferenceNIP: TEdit;
    edBatteryStatusTestFlags: TEdit;
    edRekuperationLevel1: TFloatSpinEdit;
    edRekuperationLevel2: TFloatSpinEdit;
    edRekuperationLevel3: TFloatSpinEdit;
    edAssistMaxSpeed: TFloatSpinEdit;
    edAssistMinSpeed: TFloatSpinEdit;
    edInitialAssistLevel: TSpinEdit;
    edOdometer: TFloatSpinEdit;
    edRekuperationLevel4: TFloatSpinEdit;
    edThrottleMaxSpeed: TFloatSpinEdit;
    edTorqueSensorExtraGain: TFloatSpinEdit;
    edTorqueSensorExtraGainMaxSpeed: TFloatSpinEdit;
    edTorqueSensorGain: TFloatSpinEdit;
    edTorqueSensorSpeed: TSpinEdit;
    edMotorAssistWalkSpeedDecreaseEnd: TFloatSpinEdit;
    edMotorAssistWalkLevel: TFloatSpinEdit;
    edMotorAssistWalkMaxLevel: TFloatSpinEdit;
    edMotorTorqueGaugePolarity: TSpinEdit;
    edMotorStatsChronoSec: TSpinEdit;
    edMotorStatsHallDchs: TSpinEdit;
    edMotorStatsHallTrans: TSpinEdit;
    edMotorStatsHallRing: TSpinEdit;
    edMotorStatsHallLost: TSpinEdit;
    edSensorConfigRampUpSteps: TSpinEdit;
    edSensorConfigGaugeGain: TFloatSpinEdit;
    edSensorConfigDecayDelay: TSpinEdit;
    edSensorConfigDecaySteps: TSpinEdit;
    edMotorTorqueGaugeType: TSpinEdit;
    edSensorConfigSpeedThreshold: TFloatSpinEdit;
    edSensorConfigInputOffset: TFloatSpinEdit;
    edMotorPreferenceRegion: TSpinEdit;
    edMotorConfigCommMode: TSpinEdit;
    edMotorStatsMaxVPower: TFloatSpinEdit;
    edMotorStatsOdo: TSpinEdit;
    edMotorStatsMaxVTemp: TFloatSpinEdit;
    edMotorStatsChronoHr: TSpinEdit;
    edMotorAssistWalkSpeedDecreaseStart: TFloatSpinEdit;
    edMotorAssistLevel: TFloatSpinEdit;
    edMotorTorqueGaugeGain: TFloatSpinEdit;
    edMotorTorqueGaugeMaxVoltage: TFloatSpinEdit;
    edMotorTorqueGaugeMaxVoltageDelay: TFloatSpinEdit;
    edMotorAssistLevelOffSlope: TFloatSpinEdit;
    edMotorAssistMaxSpeed: TFloatSpinEdit;
    edMotorAssistMaxSpeedDerateDelta: TFloatSpinEdit;
    edMotorAssistRegenInflex: TFloatSpinEdit;
    edMotorTorqueGaugeNoise: TFloatSpinEdit;
    edMotorTorqueGaugeDelay: TFloatSpinEdit;
    edMotorTorqueGaugeSpeed: TFloatSpinEdit;
    edMotorTorqueGaugeReference: TFloatSpinEdit;
    gbAssistanceLevel: TGroupBox;
    gbBatteryCalib: TGroupBox;
    gbMotorAssist: TGroupBox;
    gbMotorConfig: TGroupBox;
    gbSensorConfig: TGroupBox;
    gbMotorStatus: TGroupBox;
    gbMotorGeometry: TGroupBox;
    gbMotorStats: TGroupBox;
    gbRekuperationLevel: TGroupBox;
    gbSpeedLimits: TGroupBox;
    gbTorqueSensor: TGroupBox;
    gbBoost: TGroupBox;
    gbBatteryTimer: TGroupBox;
    gbBatteryStatus: TGroupBox;
    gbBatteryRTC: TGroupBox;
    gbBatteryGG: TGroupBox;
    gbBatteryConfig: TGroupBox;
    gbBatteryProtect: TGroupBox;
    gbMotorTorque: TGroupBox;
    gbMotorPreference: TGroupBox;
    gbBatteryCellMon: TGroupBox;
    gbBatteryCharger: TGroupBox;
    Label1: TLabel;
    Label11: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Label115: TLabel;
    Label116: TLabel;
    Label117: TLabel;
    Label118: TLabel;
    Label134: TLabel;
    Label135: TLabel;
    Label136: TLabel;
    Label137: TLabel;
    Label138: TLabel;
    Label139: TLabel;
    Label14: TLabel;
    Label140: TLabel;
    Label141: TLabel;
    Label142: TLabel;
    Label143: TLabel;
    Label144: TLabel;
    Label145: TLabel;
    Label146: TLabel;
    Label147: TLabel;
    Label148: TLabel;
    Label149: TLabel;
    Label15: TLabel;
    Label150: TLabel;
    Label151: TLabel;
    Label152: TLabel;
    Label153: TLabel;
    Label154: TLabel;
    Label155: TLabel;
    Label156: TLabel;
    Label157: TLabel;
    Label158: TLabel;
    Label159: TLabel;
    Label16: TLabel;
    Label160: TLabel;
    Label161: TLabel;
    Label162: TLabel;
    Label163: TLabel;
    Label164: TLabel;
    Label165: TLabel;
    Label166: TLabel;
    Label167: TLabel;
    Label168: TLabel;
    Label169: TLabel;
    Label17: TLabel;
    Label170: TLabel;
    Label171: TLabel;
    Label172: TLabel;
    Label173: TLabel;
    Label174: TLabel;
    Label175: TLabel;
    Label176: TLabel;
    Label177: TLabel;
    Label178: TLabel;
    Label180: TLabel;
    Label181: TLabel;
    Label182: TLabel;
    Label183: TLabel;
    Label185: TLabel;
    Label186: TLabel;
    Label187: TLabel;
    Label189: TLabel;
    Label190: TLabel;
    Label191: TLabel;
    Label192: TLabel;
    Label193: TLabel;
    Label194: TLabel;
    Label195: TLabel;
    Label196: TLabel;
    Label197: TLabel;
    Label198: TLabel;
    Label199: TLabel;
    Label2: TLabel;
    Label200: TLabel;
    Label201: TLabel;
    Label202: TLabel;
    Label203: TLabel;
    Label204: TLabel;
    Label205: TLabel;
    Label206: TLabel;
    Label207: TLabel;
    Label208: TLabel;
    Label209: TLabel;
    Label210: TLabel;
    Label211: TLabel;
    Label212: TLabel;
    Label213: TLabel;
    Label214: TLabel;
    Label215: TLabel;
    Label216: TLabel;
    Label217: TLabel;
    Label218: TLabel;
    Label219: TLabel;
    Label22: TLabel;
    Label220: TLabel;
    Label221: TLabel;
    Label222: TLabel;
    Label223: TLabel;
    Label224: TLabel;
    Label225: TLabel;
    Label226: TLabel;
    Label227: TLabel;
    Label228: TLabel;
    Label229: TLabel;
    Label23: TLabel;
    Label230: TLabel;
    Label231: TLabel;
    Label232: TLabel;
    Label233: TLabel;
    Label234: TLabel;
    Label235: TLabel;
    Label236: TLabel;
    Label237: TLabel;
    Label238: TLabel;
    Label239: TLabel;
    Label24: TLabel;
    Label240: TLabel;
    Label241: TLabel;
    Label242: TLabel;
    Label243: TLabel;
    Label244: TLabel;
    Label245: TLabel;
    Label246: TLabel;
    Label25: TLabel;
    Label250: TLabel;
    Label251: TLabel;
    Label252: TLabel;
    Label253: TLabel;
    Label254: TLabel;
    Label257: TLabel;
    Label258: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label279: TLabel;
    Label28: TLabel;
    Label281: TLabel;
    Label282: TLabel;
    Label283: TLabel;
    Label286: TLabel;
    Label287: TLabel;
    Label288: TLabel;
    Label289: TLabel;
    Label290: TLabel;
    Label291: TLabel;
    Label292: TLabel;
    Label293: TLabel;
    Label294: TLabel;
    Label295: TLabel;
    Label3: TLabel;
    Label300: TLabel;
    Label301: TLabel;
    Label302: TLabel;
    Label308: TLabel;
    Label311: TLabel;
    Label312: TLabel;
    Label313: TLabel;
    Label314: TLabel;
    Label315: TLabel;
    Label316: TLabel;
    Label318: TLabel;
    Label319: TLabel;
    Label320: TLabel;
    Label321: TLabel;
    Label322: TLabel;
    Label323: TLabel;
    Label324: TLabel;
    Label325: TLabel;
    Label326: TLabel;
    Label327: TLabel;
    Label328: TLabel;
    Label329: TLabel;
    Label33: TLabel;
    Label330: TLabel;
    Label331: TLabel;
    Label332: TLabel;
    Label333: TLabel;
    Label334: TLabel;
    Label335: TLabel;
    Label336: TLabel;
    Label337: TLabel;
    Label338: TLabel;
    Label339: TLabel;
    Label34: TLabel;
    Label340: TLabel;
    Label341: TLabel;
    Label342: TLabel;
    Label343: TLabel;
    Label344: TLabel;
    Label345: TLabel;
    Label346: TLabel;
    Label347: TLabel;
    Label348: TLabel;
    Label349: TLabel;
    Label350: TLabel;
    Label351: TLabel;
    Label352: TLabel;
    Label353: TLabel;
    Label354: TLabel;
    Label355: TLabel;
    Label356: TLabel;
    Label357: TLabel;
    Label358: TLabel;
    Label359: TLabel;
    Label360: TLabel;
    Label361: TLabel;
    Label362: TLabel;
    Label363: TLabel;
    Label364: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    mmLog: TMemo;
    pnlSensorButtons: TPanel;
    scbBattery: TScrollBox;
    scbMotor: TScrollBox;
    scbSensor: TScrollBox;
    tsSensor: TTabSheet;
    tsTuning: TTabSheet;
    tsTests: TTabSheet;
    pnlMain: TPanel;
    pcMain: TPageControl;
    tsInfo: TTabSheet;
    tsSettings: TTabSheet;
    sbMain: TStatusBar;
    pnlInfoTop: TPanel;
    mmInfo: TMemo;
    gbBrakes: TGroupBox;
    Label13: TLabel;
    Label30: TLabel;
    cbBrakeSensor: TComboBox;
    Label21: TLabel;
    Label31: TLabel;
    gbMiscSettings: TGroupBox;
    Label9: TLabel;
    edWheelCircumference: TSpinEdit;
    Label18: TLabel;
    btnExit: TBitBtn;
    btnConnect: TBitBtn;
    btnDisconnect: TBitBtn;
    btnShutdown: TBitBtn;
    pnlFileAdapter: TPanel;
    btnSaveToFile: TBitBtn;
    cbAdapter: TComboBox;
    Label29: TLabel;
    Label32 : TLabel;
    edAutoShutdownDelay : TSpinEdit;
    sdInfo : TSaveDialog;
    btnInfoSave : TSpeedButton;
    btnInfoClear : TSpeedButton;
    pnlTest : TPanel;
    btnLogClear : TButton;
    Button1 : TButton;
    gbAccessory : TGroupBox;
    Label37 : TLabel;
    cbAccessoryEnabled : TComboBox;
    edAccessoryVoltage : TFloatSpinEdit;
    Label43 : TLabel;
    Label44 : TLabel;
    edTaillampIntensity : TSpinEdit;
    gbConsole : TGroupBox;
    Label20 : TLabel;
    cbConsoleSwitches : TComboBox;
    Label12 : TLabel;
    Label45 : TLabel;
    cbLightButtonMode : TComboBox;
    Label35 : TLabel;
    cbShowRemainingDistanceAndTime : TComboBox;
    Label36 : TLabel;
    Label10 : TLabel;
    cbDisplayUnits : TComboBox;
    Label19 : TLabel;
    Label46 : TLabel;
    cbLightsOnAtStart : TComboBox;
    Label47 : TLabel;
    cbRememberDisplayMode : TComboBox;
    Label48 : TLabel;
    edLCDContrast : TSpinEdit;
    Label49: TLabel;
    cbBoostDisplay: TComboBox;
    edBoostLevel: TFloatSpinEdit;
    cbShowMotorTemperature: TComboBox;
    Label51: TLabel;
    Label52: TLabel;
    cbEnabledOnStrain: TComboBox;
    pnlTuningButtons: TPanel;
    Bevel2: TBevel;
    btnReadTuning: TBitBtn;
    btnApplyTuning: TBitBtn;
    pnlSettingsButtons: TPanel;
    Bevel1: TBevel;
    btnReadSettings: TBitBtn;
    btnApplySettings: TBitBtn;
    Edit1: TEdit;
    tsBattery: TTabSheet;
    pnlBatteryButtons: TPanel;
    Bevel3: TBevel;
    btnReadBattery: TBitBtn;
    btnApplyBattery: TBitBtn;
    Label60 : TLabel;
    edAccessoryShutdownDelay : TSpinEdit;
    tsMotor : TTabSheet;
    pnlMotorButtons : TPanel;
    Bevel4 : TBevel;
    btnReadMotor : TBitBtn;
    btnApplyMotor : TBitBtn;
    edBrakeRekuLevel : TFloatSpinEdit;
    tsConsole : TTabSheet;
    edAssistLevel1: TFloatSpinEdit;
    edAssistLevel2: TFloatSpinEdit;
    edAssistLevel3: TFloatSpinEdit;
    edAssistLevel4: TFloatSpinEdit;
    edAssistLevelMountain: TFloatSpinEdit;
    pnlConsoleButtons: TPanel;
    Bevel5: TBevel;
    btnReadConsole: TBitBtn;
    btnApplyConsole: TBitBtn;
    scbConsole: TScrollBox;
    gbConsoleGeometry: TGroupBox;
    Label53: TLabel;
    edConsoleGeometryCirc: TSpinEdit;
    gbConsoleConfig: TGroupBox;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    cbConsoleConfigTestmode: TComboBox;
    Label66: TLabel;
    edConsoleConfigServiceTimestamp: TSpinEdit;
    cbConsoleConfigLastmode: TComboBox;
    gbConsoleAssist: TGroupBox;
    Label70: TLabel;
    Label71: TLabel;
    cbConsoleAssistMaxSpeedFlag: TComboBox;
    Label72: TLabel;
    cbConsoleAssistMinSpeedFlag: TComboBox;
    Label73: TLabel;
    edConsoleAssistBrakeLevel: TFloatSpinEdit;
    Label67: TLabel;
    Label74: TLabel;
    cbConsoleAssistAutoregenFlag: TComboBox;
    Label75: TLabel;
    Label76: TLabel;
    edConsoleAssistGaugeFilter: TSpinEdit;
    Label77: TLabel;
    Label78: TLabel;
    edConsoleAssistGaugeGain: TFloatSpinEdit;
    edConsoleAssistGainA: TFloatSpinEdit;
    Label79: TLabel;
    Label80: TLabel;
    edConsoleAssistGainB: TFloatSpinEdit;
    edConsoleAssistGaugeJoint: TSpinEdit;
    Label81: TLabel;
    Label82: TLabel;
    edConsoleAssistLevel1: TFloatSpinEdit;
    Label83: TLabel;
    edConsoleAssistLevel2: TFloatSpinEdit;
    Label84: TLabel;
    edConsoleAssistLevel3: TFloatSpinEdit;
    Label85: TLabel;
    edConsoleAssistLevel4: TFloatSpinEdit;
    Label86: TLabel;
    edConsoleAssistLevelR1: TFloatSpinEdit;
    Label87: TLabel;
    edConsoleAssistLevelR2: TFloatSpinEdit;
    Label88: TLabel;
    edConsoleAssistLevelR3: TFloatSpinEdit;
    Label89: TLabel;
    edConsoleAssistLevelR4: TFloatSpinEdit;
    Label90: TLabel;
    edConsoleAssistInitLevel: TSpinEdit;
    Label91: TLabel;
    edConsoleAssistMountainCap: TFloatSpinEdit;
    gbConsoleThrottle: TGroupBox;
    Label94: TLabel;
    edConsoleThrottleMaxSpeed: TFloatSpinEdit;
    Label98: TLabel;
    cbConsoleThrottleEnabledOnStrain: TComboBox;
    Label92: TLabel;
    cbConsoleThrottleMaxSpeedFlag: TComboBox;
    Label93: TLabel;
    edConsoleThrottleBoostTriggerLevel: TFloatSpinEdit;
    Label95: TLabel;
    cbConsoleThrottleEnableBoostDisplay: TComboBox;
    Label96: TLabel;
    cbConsoleThrottleCalibrated: TComboBox;
    Label97: TLabel;
    Label99: TLabel;
    gbConsolePreference: TGroupBox;
    Label100: TLabel;
    cbConsolePreferenceTripToEmptyFlag: TComboBox;
    Label101: TLabel;
    cbConsolePreferenceDisplayUnits: TComboBox;
    Label102: TLabel;
    edConsolePreference: TEdit;
    Label103: TLabel;
    edConsolePreferenceLCDContrast: TSpinEdit;
    Label104: TLabel;
    Label105: TLabel;
    edConsolePreferenceRegion: TSpinEdit;
    Label106: TLabel;
    edConsolePreferenceConfigBit0: TSpinEdit;
    Label107: TLabel;
    cbConsolePreferenceFlipSide: TComboBox;
    Label108: TLabel;
    Label109: TLabel;
    Label110: TLabel;
    cbConsolePreferenceLightsOnAtStart: TComboBox;
    Label111: TLabel;
    cbConsolePreferenceExpertMode: TComboBox;
    Label112: TLabel;
    edConsoleThrottleMin: TSpinEdit;
    edConsoleThrottleMax: TSpinEdit;
    edConsolePreferenceThrottleMode: TSpinEdit;
    gbConsoleStats: TGroupBox;
    edConsoleStatsOdo: TFloatSpinEdit;
    Label119: TLabel;
    Label120: TLabel;
    edBatteryStatusCapSense: TSpinEdit;
    Label121: TLabel;
    edBatteryStatusCapsenseReference: TSpinEdit;
    gbBatteryStats: TGroupBox;
    Label122: TLabel;
    edBatteryStatsv5VShorts: TSpinEdit;
    Label123: TLabel;
    edBatteryStatsVControlShorts: TSpinEdit;
    edBatteryStatsLowBattBuzzCount: TSpinEdit;
    Label124: TLabel;
    edBatteryStatsCellVoltageCollapseCount: TSpinEdit;
    Label125: TLabel;
    edBatteryStatsCellPartialShortCount: TSpinEdit;
    Label126: TLabel;
    edBatteryStatsCellDeadShortCount: TSpinEdit;
    Label127: TLabel;
    edBatteryStatsDeepSleepAfterLongInactivityPeriodCount: TSpinEdit;
    Label128: TLabel;
    Label129: TLabel;
    edBatteryStatsDeepSleepAfterLowSocCount: TSpinEdit;
    edBatteryStatsDeepSleepExtremeLowBatteryVoltageCount: TSpinEdit;
    Label130: TLabel;
    Label131: TLabel;
    edBatteryStatsDischargeEnergy: TSpinEdit;
    Label132: TLabel;
    edBatteryStatsChargeEnergy: TSpinEdit;
    Label133: TLabel;
    edBatteryStatsReset: TSpinEdit;
    edConsolePreferenceCodes: TEditButton;
    edConsolePreferenceCodesRW: TEditButton;
    pnlWarningConsole: TPanel;
    lblWarning: TLabel;
    lblNote: TLabel;
    pnlWarningBattery: TPanel;
    lblWarning1: TLabel;
    lblNote1: TLabel;
    pnlWarningMotor: TPanel;
    lblWarning2: TLabel;
    lblNote2: TLabel;
    pnlWarningSensor: TPanel;
    lblWarning3: TLabel;
    lblNote3: TLabel;
    pnlInfoButtons: TPanel;
    btnInfoSensor: TBitBtn;
    btnInfoMotor: TBitBtn;
    btnInfoBattery: TBitBtn;
    btnInfoConsole: TBitBtn;
    edConsoleAssistSpeedGain: TFloatSpinEdit;
    Label179: TLabel;
    edTorqueSensorSpeedGain: TFloatSpinEdit;
    Label50: TLabel;
    Label54: TLabel;
    DateEdit1: TDateEdit;
    edBatteryRTCTime2: TEdit;
    edBatteryRTCLastChargeTimeStamp2: TEdit;
    procedure BitBtn1Click ( Sender: TObject ) ;
    procedure btnAboutClick ( Sender: TObject ) ;
    procedure btnApplySettingsClick ( Sender: TObject ) ;
    procedure btnApplyTuningClick ( Sender: TObject ) ;
    procedure btnMotorLockClick(Sender: TObject);
    procedure btnMotorUnlockClick(Sender: TObject);
    procedure btnReadTuningClick ( Sender: TObject ) ;
    procedure btnShutdownClick ( Sender: TObject ) ;
    procedure Button1Click(Sender: TObject);
    procedure btnLogClearClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure EditfieldChange(Sender: TObject);
    procedure FormCreate ( Sender: TObject ) ;
    procedure btnReadSettingsClick(Sender: TObject);
    procedure btnInfoConsoleClick(Sender: TObject);
    procedure btnInfoBatteryClick(Sender: TObject);
    procedure btnInfoClearClick(Sender: TObject);
    procedure btnInfoMotorClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnInfoSaveClick ( Sender : TObject ) ;
    procedure Button2Click ( Sender: TObject ) ;
    procedure btnReadBatteryClick ( Sender: TObject ) ;
    procedure btnApplyBatteryClick ( Sender: TObject ) ;
    procedure btnReadMotorClick ( Sender : TObject ) ;
    procedure btnApplyMotorClick ( Sender : TObject ) ;
    procedure btnInfoSensorClick ( Sender: TObject ) ;
    procedure FormDestroy ( Sender : TObject ) ;
    procedure btnReadConsoleClick ( Sender: TObject ) ;
    procedure btnApplyConsoleClick ( Sender: TObject ) ;
    procedure btnReadSensorClick ( Sender: TObject ) ;
    procedure btnApplySensorClick ( Sender: TObject ) ;
    procedure CodesEditClick(Sender: TObject);
    procedure btnBatteryUnlockClick ( Sender: TObject ) ;
    procedure btnBatteryLockClick ( Sender: TObject ) ;
  private
    { private declarations }
    FBike : TBionXBike;
    FSaveValues : TStringlist;
    procedure HandleCANMessage ( Msg : PCANMsg );

    procedure HandleException ( Sender : TObject; E : Exception);

    procedure EnableControls ( Connected : boolean );
    procedure LogMsg ( const s : string );

    procedure ShowValue ( const Value : string ); overload;
    procedure ShowValue ( const ValueName : string; const Fmt : string; Args : array of const ); overload;

    procedure ShowConsoleSettings;
    procedure ShowBatterySettings;
    procedure ShowMotorSettings;
    procedure ShowSensorSettings;

    procedure ReadSettings;
    procedure WriteSettings;

    procedure ReadTuning;
    procedure WriteTuning;

    procedure ReadConsole;
    procedure WriteConsole;
    procedure EnableConsoleControls ( Enable : boolean );

    procedure ReadBattery;
    procedure WriteBattery;
    procedure EnableBatteryControls ( Enable : boolean );

    procedure ReadMotor;
    procedure WriteMotor;
    procedure EnableMotorControls ( Enable : boolean );

    procedure ReadSensor;
    procedure WriteSensor;
    procedure EnableSensorControls ( Enable : boolean );

  protected
  public
    { public declarations }
  end;

var
  frmBionXMain: TfrmBionXMain;

implementation
uses
  fmCodes;
{$R *.lfm}

const
  Version = 76;

{ TfrmBionXMain }

// puts the value into a control to show it and remember that value for a
// later compare, wether it was changed
procedure SetControlValue ( se : TSpinEdit; Value : integer; SaveValues : TStrings ); overload;
begin
  se.Value := Value;
  SaveValues.Values[se.Name] := se.Text;
  se.Font.Color := clDefault;
//  Application.ProcessMessages;
  se.Refresh;
end;

procedure SetControlValue ( fse : TFloatSpinEdit; Value : double; SaveValues : TStrings ); overload;
begin
  fse.Value := Value;
  SaveValues.Values[fse.Name] := fse.Text;
  fse.Font.Color := clDefault;
  fse.Refresh;
end;

procedure SetControlValue ( cb : TCustomCombobox; Value : integer; SaveValues : TStrings ); overload;
begin
  cb.ItemIndex := Value;
  SaveValues.Values[cb.Name] := IntToStr(Value);
  cb.Font.Color := clDefault;
  cb.Refresh;
end;

procedure SetControlValue ( ed : TCustomEdit; Value : string; SaveValues : TStrings ); overload;
begin
  ed.Text := Value;
  SaveValues.Values[ed.Name] := Value;
  ed.Font.Color := clDefault;
  ed.Refresh;
end;
(*
procedure SetControlValue ( dt : TZVDateTimePicker; Mfd : TDateTime; Value : TDateTime; SaveValues : TStrings ); overload;
begin
  dt.MinDate := Mfd;
  dt.DateTime := Value;
  SaveValues.Values[dt.Name] := DateTimeToStr ( Value );
  dt.Font.Color := clDefault;
  dt.Refresh;
end;
*)
(*
procedure SetControlValue ( Control : TWinControl; Instance : TObject; PropName : string; SaveValues : TStrings ); overload;
var
  TypeKind : TTypeKind;
begin
  try
    TypeKind := PropType ( Instance, PropName );
    try
      case TypeKind of
        tkInteger :
          SetControlValue ( TSpinEdit(Control), GetInt64Prop ( Instance, PropName ), SaveValues );
        tkFloat :
          SetControlValue ( TFloatSpinEdit(Control), GetFloatProp ( Instance, PropName ), SaveValues );
        else
          Control.Visible := false;
      end;
      Control.Enabled := true;
    except
      Control.Enabled := false;
    end;
  except
    raise;
  end;
end;
*)
{------------------------------------------------------------}

// compares the current control value with the one saved in SaveValues
// returnes
// true,  when unchanged
// false, when modified
function CompareControlValue ( se : TSpinEdit; SaveValues : TStrings ) : boolean; overload;
begin
  if se.Enabled then
    Result := se.Text = SaveValues.Values[se.Name]
  else
    Result := true;
end;

function CompareControlValue ( fse : TFloatSpinEdit; SaveValues : TStrings ) : boolean; overload;
begin
  if fse.Enabled then
    Result := fse.Text = SaveValues.Values[fse.Name]
  else
    Result := true;
end;

function CompareControlValue ( cb : TCustomCombobox; SaveValues : TStrings ) : boolean; overload;
begin
  Result := IntToStr(cb.ItemIndex) = SaveValues.Values[cb.Name];
end;

function CompareControlValue ( ed : TCustomEdit; SaveValues : TStrings ) : boolean; overload;
begin
  Result := ed.Text = SaveValues.Values[ed.Name];
end;
(*
function CompareControlValue ( dt : TZVDateTimePicker; SaveValues : TStrings ) : boolean; overload;
begin
  Result := DateTimeToStr ( dt.DateTime ) = SaveValues.Values[dt.Name];
end;
*)
{------------------------------------------------------------}

// get the current value from control and update it in SaveValues
function GetControlValue ( se : TSpinEdit; SaveValues : TStrings ) : integer; overload;
begin
  Result := se.Value;
  SaveValues.Values[se.Name] := se.Text;
  se.Font.Color := clDefault;
end;

function GetControlValue ( fse : TFloatSpinEdit; SaveValues : TStrings ) : double; overload;
begin
  Result := fse.Value;
  SaveValues.Values[fse.Name] := fse.Text;
  fse.Font.Color := clDefault;
end;

function GetControlValue ( cb : TCustomCombobox; SaveValues : TStrings ) : integer; overload;
begin
  Result := cb.ItemIndex;
  SaveValues.Values[cb.Name] := IntToStr(Result);
  cb.Font.Color := clDefault;
end;

function GetControlValue ( ed : TCustomEdit; SaveValues : TStrings ) : string; overload;
begin
  Result :=ed.Text;
  SaveValues.Values[ed.Name] := Result;
  ed.Font.Color := clDefault;
end;

(*
function GetControlValue ( dt : TZVDateTimePicker; SaveValues : TStrings ) : TDateTime; overload;
begin
  Result := dt.DateTime;
  SaveValues.Values[dt.Name] := DateTimeToStr ( Result );
  dt.Font.Color := clDefault;
end;
*)

procedure EnableEditControls ( ctl : TWinControl; Enabled : boolean );
var
  i : integer;
begin
  for i := 0 to ctl.ControlCount-1 do
    if ( ctl.Controls[i] is TButtonControl ) or
       ( ctl.Controls[i] is TCustomEdit ) or
       ( ctl.Controls[i] is TCustomComboBox ) or
       ( ctl.Controls[i] is TSpeedButton ) then
      ctl.Controls[i].Enabled := Enabled
    else
      if ctl.Controls[i] is TWinControl then
        EnableEditControls ( TWinControl(ctl.Controls[i]), Enabled );
end;

{------------------------------------------------------------}

procedure TfrmBionXMain.FormCreate ( Sender: TObject ) ;
begin
  btnAbout.Anchors := [akLeft, akTop];
  btnExit.Anchors := [akLeft, akTop];
  btnInfoSave.Anchors := [akLeft, akTop];
  btnInfoClear.Anchors := [akLeft, akTop];
  btnLogClear.Anchors := [akLeft, akTop];

  //  http://wiki.lazarus.freepascal.org/High_DPI/de
  ScaleBy ( Screen.PixelsPerInch, 96 );

  btnAbout.Anchors := [akRight, akTop];
  btnExit.Anchors := [akRight, akTop];
  btnInfoSave.Anchors := [akRight, akTop];
  btnInfoClear.Anchors := [akRight, akTop];
  btnLogClear.Anchors := [akRight, akTop];

  Caption := Format ( Caption, [Version div 100, Version mod 100] );

  FSaveValues := TStringlist.Create;

  pcMain.ActivePageIndex := 0;

  EnableControls ( false );

  // show the test tab on debug mode
  if not FindCmdLineSwitch ( 'd', true ) then
  begin
    pnlFileAdapter.Visible := false;
    tsTests.TabVisible := false;
  end;

  // show the expert tabs with cmdlineswitch only
  if not FindCmdLineSwitch ( 'x', true ) then
  begin
    tsBattery.TabVisible := false;
    tsMotor.TabVisible := false;
    tsConsole.TabVisible := false;
    tsSensor.TabVisible := false;
  end;

  Application.OnException := @HandleException;

  {$ifdef UNIX}
  Font.Name := 'default';
  Font.Size := 0;
  {$endif}
end;

procedure TfrmBionXMain.FormDestroy ( Sender : TObject ) ;
begin
  FSaveValues.Free;
end;

procedure TfrmBionXMain.HandleException ( Sender : TObject; E : Exception);
begin
  MessageDlg ( 'Error', E.Message, mtError, [mbOK], 0, mbOK );
end;


procedure TfrmBionXMain.EnableControls ( Connected : boolean );
begin
  btnConnect.Enabled := not Connected;
  btnDisconnect.Enabled := Connected;
  btnShutdown.Enabled := Connected;
  btnSaveToFile.Enabled := Connected;

  pnlInfoButtons.Enabled := Connected;
  pnlSettingsButtons.Enabled := Connected;
  pnlTuningButtons.Enabled := Connected;
  pnlConsoleButtons.Enabled := Connected;
  pnlBatteryButtons.Enabled := Connected;
  pnlMotorButtons.Enabled := Connected;
  pnlSensorButtons.Enabled := Connected;


  EnableEditControls ( tsSettings, Connected );
  EnableEditControls ( tsTuning, Connected );

  if not Connected then
  begin
    EnableConsoleControls ( false );
    EnableBatteryControls ( false );
    EnableMotorControls ( false );
    EnableSensorControls ( false );
  end;
end;

procedure TfrmBionXMain.LogMsg ( const s : string );
begin
//  if mmLog.Lines.Count < 50 then
  mmLog.Lines.Add ( s );
//  Application.ProcessMessages;

end;

// write a line to the info page
procedure TfrmBionXMain.ShowValue ( const Value : string );
begin
  mmInfo.Lines.Add ( Value );
end;

// write a line to the info window
procedure TfrmBionXMain.ShowValue ( const ValueName : string; const Fmt : string; Args : array of const );
var
  Line : string;
begin
  Line := '  ' + ValueName + ' ';
  while length ( line ) < 40 do
    Line := Line + '.';
  Line := Line + ': ' + Format ( Fmt, Args );
  ShowValue ( Line );
end;

// write the console settings to the info window
procedure TfrmBionXMain.ShowConsoleSettings;

  procedure ShowConsoleCodes;
  var
    i       : integer;
    Mask    : longword;
    codes   : longword;
    codesrw : longword;
  begin
    codes := FBike.Console.PreferenceCodes;
    codesrw := FBike.Console.PreferenceCodesRW;
    Mask    := 1;
    ShowValue ( 'Console codes', '0x%0.8x 0x%0.8x', [codes, codesrw] );
    ShowValue ( '  Value (Code)', '%5s %5s', [ 'read', 'write' ] );
    for i := 0 to 28 do
    begin
      ShowValue ( '  '+ConsoleCodeBitString(i), '%5s %5s', [ BoolToStr((codes and Mask)<>0), BoolToStr((codesrw and Mask)<>0) ] );
      Mask := Mask shl 1;
    end;
    ShowValue ( '' );
  end;

begin
  FBike.KeepAlive := false;
  try
    FBike.Console.CheckDeviceReady;

    // Partinfo
    ShowValue ( 'Console information:' );
    ShowValue ( 'Hardware version', '%0.2d', [ FBike.Console.HardwareVersion ] );
    ShowValue ( 'Software version', '%0.2d', [ FBike.Console.SoftwareVersion ] );
    ShowValue ( 'Sub version', '%0.2d', [ FBike.Console.SubVersion ] );
    ShowValue ( 'Part number', '%0.5d', [ FBike.Console.PartNumber ] );
    ShowValue ( 'Item number', '%0.5d', [ FBike.Console.ItemNumber ] );
    ShowValue ( 'Manufacturing date', '%s', [ DateToStr ( FBike.Console.ManufacturingDate ) ] );
    ShowValue ( 'Location', '%d', [ FBike.Console.Location ] );
    ShowValue ( 'OEM', '%d', [ FBike.Console.OEM ] );
    ShowValue ( 'Procuct Id', '%d', [ FBike.Console.Product ] );
    ShowValue ( 'Type', '%s', [ ConsoleTypeToStr(FBike.Console.ConsoleType) ] );
    ShowValue ( 'Region', '%d', [ FBike.Console.PreferenceRegion ] );


    ShowValue ( '' );

    // Misc settings
    ShowValue ( '  Configuration' );
    ShowValue ( '  -------------' );
    ShowValue ( 'Wheel circumference', '%d mm', [ FBike.Console.GeometryCirc] );
    ShowValue ( 'Flipped switches', '%s', [ BoolToStr ( FBike.Console.PreferenceFlipSide )] );
    ShowValue ( 'Display units', '%s', [ DisplayUnitsToStr ( FBike.Console.PreferenceDisplayUnits ) ] );
    ShowValue ( 'Show remaining distance and time', '%s', [ BoolToStr ( FBike.Console.PreferenceTripToEmptyFlag ) ] );
    ShowValue ( 'Lights on at start', '%s', [ BoolToStr ( FBike.Console.PreferenceLightsOnAtStart ) ] );
    ShowValue ( 'Light button mode', '%s', [ LightButtonModeToStr ( FBike.Console.PreferenceLightButtonMode ) ] );
    if FBike.Console.HardwareVersion = 15 then
      ShowValue ( 'Remember display mode', '%s', [ BoolToStr ( FBike.Console.ConfigLastMode ) ] );
    ShowValue ( 'LCD Contrast', '%d', [ FBike.Console.PreferenceLCDContrast ] );
    ShowValue ( '' );
    ShowValue ( 'NIP', '"%s"', [ FBike.Console.PreferenceNIP ] );
    ShowValue ( 'ConfigBit0', '%d', [ FBike.Console.PreferenceConfigBit0 ] );
    ShowValue ( 'Test mode', '%s', [ BoolToStr ( FBike.Console.ConfigTestMode ) ] );
    if FBike.Console.HardwareVersion = 15 then
      ShowValue ( 'Expert mode', '%s', [ BoolToStr ( FBike.Console.PreferenceExpertMode ) ] );
    ShowValue ( 'Auto regen', '%s', [ BoolToStr ( FBike.Console.AssistAutoRegen ) ] );
    ShowValue ( 'Throttle mode', '%d', [ FBike.Console.PreferenceThrottlemode ] );
    ShowValue ( '' );

    ShowConsoleCodes;

    // Assistlevels
    ShowValue ( '  Assist & Reku' );
    ShowValue ( '  -------------' );
    ShowValue ( 'Initial assist level', '%d', [ FBike.Console.AssistInitLevel] );
    ShowValue ( 'Assist level 1', '%0.2f%%', [ FBike.Console.AssistLevel1] );
    ShowValue ( 'Assist level 2', '%0.2f%%', [ FBike.Console.AssistLevel2] );
    ShowValue ( 'Assist level 3', '%0.2f%%', [ FBike.Console.AssistLevel3] );
    ShowValue ( 'Assist level 4', '%0.2f%%', [ FBike.Console.AssistLevel4] );
    ShowValue ( 'Assist level mountain mode', '%0.2f%%', [ FBike.Console.MountainAssistLevel ] );
    ShowValue ( '' );

    // Rekuperation Levels
    ShowValue ( 'Rekuperation level 1', '%0.2f%%', [ FBike.Console.RekuperationLevel1] );
    ShowValue ( 'Rekuperation level 2', '%0.2f%%', [ FBike.Console.RekuperationLevel2] );
    ShowValue ( 'Rekuperation level 3', '%0.2f%%', [ FBike.Console.RekuperationLevel3] );
    ShowValue ( 'Rekuperation level 4', '%0.2f%%', [ FBike.Console.RekuperationLevel4] );
    ShowValue ( '' );

    // ASSIST speed limit
    ShowValue ( '  Speed limits' );
    ShowValue ( '  ------------' );
    ShowValue ( 'Assist max speed limited', '%s', [ BoolToStr ( FBike.Console.AssistMaxSpeedFlag )] );
    ShowValue ( 'Assist max speed', '%0.2f Km/h', [ FBike.Console.AssistMaxSpeed ]);

    // MIN speed limit
    ShowValue ( 'Assist min speed limited', '%s', [ BoolToStr ( FBike.Console.AssistMinSpeedFlag ) ] );
    ShowValue ( 'Assist min speed', '%0.2f Km/h', [ FBike.Console.AssistMinSpeed ] );

    // THROTTLE speed limit
    ShowValue ( 'Throttle max speed limited', '%s', [ BoolToStr ( FBike.Console.ThrottleMaxSpeedFlag ) ] );
    ShowValue ( 'Throttle max speed', '%0.2f Km/h', [ FBike.Console.ThrottleMaxSpeed ]);

    ShowValue ( '' );

    // Throttle related
    ShowValue ( '  Throttle' );
    ShowValue ( '  --------' );
    ShowValue ( 'Throttle calibrated', '%s', [ BoolToStr ( FBike.Console.ThrottleCalibrated ) ] );
    ShowValue ( 'Throttle min actor reading', '%d', [ FBike.Console.ThrottleMinActorValue ] );
    ShowValue ( 'Throttle max actor reading', '%d', [ FBike.Console.ThrottleMaxActorValue ] );
    ShowValue ( 'Throttle actor reading', '%d', [ FBike.Console.ThrottleRawPosition ] );
    ShowValue ( 'Throttle current level', '%0.2f%%', [ FBike.Console.ThrottlePosition ] );
    ShowValue ( 'Throttle boost trigger level', '%0.2f%%', [ FBike.Console.ThrottleBoostTriggerLevel ] );
    ShowValue ( 'Throttle boost display', '%s', [ BoolToStr ( FBike.Console.ThrottleEnableBoostDisplay ) ] );
    ShowValue ( 'Throttle enable on strain', '%s', [ BoolToStr ( FBike.Console.ThrottleEnabledOnStrain ) ] );
    ShowValue ( '' );


    // Brake info
    ShowValue ( '  Brake' );
    ShowValue ( '  -----' );
    ShowValue ( 'Brake sensor aktiv', '%s', [ BoolToStr ( FBike.Console.AssistBrakeFlag )] );
    ShowValue ( 'Brake sensor type', '%s', [ BrakeSensorTypeToStr(FBike.Console.AssistBrakePolarity) ] );
    ShowValue ( 'Brake reku level', '%0.2f%%', [ FBike.Console.AssistBrakeLevel ] );
    ShowValue ( '' );

    // Sensor gain
    ShowValue ( '  Torque sensor' );
    ShowValue ( '  -------------' );
    ShowValue ( 'Torque sensor gain', '%0.2f', [ FBike.Console.TorqueSensorGain ] );
    ShowValue ( 'Torque sensor speed', '%d', [ FBike.Console.TorqueSensorSpeed ] );
    ShowValue ( 'Torque sensor extra gain', '%0.2f', [ FBike.Console.TorqueSensorExtraGain ] );
    ShowValue ( 'Torque sensor extra max speed', '%0.2f', [ FBike.Console.TorqueSensorExtraGainMaxSpeed ] );
    ShowValue ( 'Gauge knee', '%d', [ FBike.Console.AssistGaugeJoint ] );
    ShowValue ( '' );

    // Readings
    ShowValue ( '  Readings' );
    ShowValue ( '  --------' );
    ShowValue ( 'Odometer', '%0.1f Km', [ FBike.Console.StatsOdometer] );
    ShowValue ( 'Distance', '%0.1f Km', [ FBike.Console.StatsTrip] );
    ShowValue ( 'Average speed', '%0.1f Km/h', [ FBike.Console.StatsAverageSpeed] );
    ShowValue ( 'Chrono', '%0s', [ TimeToStr(FBike.Console.StatsChrono)] );
    ShowValue ( 'Next service day', '%d', [ FBike.Console.ConfigServiceTimeStamp ] );
    ShowValue ( 'Next service odo', '%d', [ FBike.Console.ConfigServiceDistance ] );
    ShowValue ( '' );

  finally
    FBike.KeepAlive := true;
  end;
end;

// write the battery settings to the info window
procedure TfrmBionXMain.ShowBatterySettings;
var
  MfdDate   : TDate;

  procedure PrintChargeStatistics;
  var
    i   : integer;
    Sum : integer;
    Cnt : integer;
  begin
    ShowValue ( '  Charging statistic' );
    ShowValue ( '  ------------------' );
    ShowValue ( 'Charge time worst', '%d', [ FBike.Battery.StatsChargeTimeWorst ] );
    ShowValue ( 'Charge time mean', '%d', [ FBike.Battery.StatsChargeTimeMean ] );

    ShowValue ( 'Charge cycles', '%d', [ FBike.Battery.StatsBattCycles ] );
    ShowValue ( 'Full charge cycles', '%d', [ FBike.Battery.StatsBattFullCycles ] );

    ShowValue ( 'Total charged energy', '%dWh', [ FBike.Battery.TotalChargedEnergy ] );
    ShowValue ( 'Total discharged energy', '%dWh', [ FBike.Battery.TotalDischargedEnergy ] );

    Sum := 0;
    for i := 1 to 9 do
    begin
      Cnt := FBike.Battery.StatsCharge[i];
      inc ( Sum, Cnt );
      ShowValue ( Format ( 'Charges from level %d%%', [i*10]), '%d', [ Cnt ] );
    end;
    ShowValue ( 'Total # of charges', '%d', [ Sum ]);
    ShowValue ( '' );
  end;

  procedure PrintChargerData;
  begin
    FBike.Battery.UnlockProtection;
    try
      ShowValue ( '  Charger data' );
      ShowValue ( '  ------------' );
      ShowValue ( 'Version', '%d', [ FBike.Battery.ChargerVersion ] );
      ShowValue ( 'Mode', '%s', [ ChargerModeToStr ( FBike.Battery.ChargerMode ) ] );
      ShowValue ( 'Charger manager status', '%s', [ ChargerStatusToStr ( FBike.Battery.ChargerManagerStatus ) ] );
      ShowValue ( 'State flags', '0x%0.8x', [ FBike.Battery.ChargerStatusFlags ] );
      ShowValue ( 'Final voltage', '%0.3fV', [ FBike.Battery.ChargerFinalVoltage ] );
      ShowValue ( 'Charge current', '%0.3fA', [ FBike.Battery.ChargerCurrent ] );
      ShowValue ( 'Voltage calibration', '%0.3fV', [ FBike.Battery.ChargerVoltageCalibration ] );
      ShowValue ( 'Current calibration', '%0.3fA', [ FBike.Battery.ChargerCurrentCalibration ] );
      ShowValue ( '' );
    finally
      FBike.Battery.LockProtection;
    end;
  end;

  procedure PrintCellInfo;
  var
    i            : integer;
    packSerial   : integer;
    packParallel : integer;

  begin
    packSerial := FBike.Battery.PackSerial;
    packParallel := FBike.Battery.PackParallel;

    ShowValue ( '  Cellpack data' );
    ShowValue ( '  -------------' );
    ShowValue ( 'Cell organisation', '%ds%dp', [ packSerial, packParallel ] );
    ShowValue ( 'Cell capacity', '%0.2fAh', [ FBike.Battery.CellCapacity ] );
    ShowValue ( 'Max. cell drift w/o balancing', '%0.2fV', [ FBike.Battery.MaxCellDeltaVoltage ] );
    ShowValue ( 'Balancer active', '%s', [ BoolToStr(FBike.Battery.CellMonBalancerEnabled)] );

    ShowValue ( 'Cell data', '%10s %10s %10s', ['Cell [V]', 'to GND [V]', 'Calib [%]'] );
    for i := 1 to packSerial do
      ShowValue ( Format ( '  Cell #%2d', [i]), '%10.3f %10.3f %10.3f', [ FBike.Battery.CellVoltage[i], FBike.Battery.SumCellVoltage[i], FBike.Battery.CalibCalibration[i] ] );

    for i := 1 to packParallel do
      ShowValue ( Format ( 'Temperature pack #%d', [i] ), '%dC', [ FBike.Battery.PackTemperature [ i ] ]);
    ShowValue ( '' );
  end;

  procedure PrintPCB;
  var
    pn : word;
  begin
    pn := FBike.Battery.PCBSNPartNumber;
    if pn = $FFFF then
      ShowValue ( 'PCB', 'no data', [] )
    else
    begin
      ShowValue ( 'PCB part number', '%d', [ FBike.Battery.PCBSNPartNumber ] );
      ShowValue ( 'PCB location', '%d', [ FBike.Battery.PCBSNLocation ] );
      ShowValue ( 'PCB manufacturing date', '%s', [ DateToStr(FBike.Battery.PCBSNManufacturingDate) ] );
      ShowValue ( 'PCB item number', '%d', [ FBike.Battery.PCBSNItemNumber ] );
    end;
  end;

begin
  FBike.KeepAlive := false;
  try
    FBike.Battery.CheckDeviceReady;

    // Partinfo
    ShowValue ( 'Battery information:' );
    ShowValue ( 'Hardware version', '%0.2d', [ FBike.Battery.HardwareVersion ] );
    ShowValue ( 'Software version', '%0.2d', [ FBike.Battery.SoftwareVersion]);
    ShowValue ( 'Sub version', '%0.2d', [ FBike.Battery.SubVersion ] );
    ShowValue ( 'Part number', '%0.5d', [ FBike.Battery.PartNumber ] );
    ShowValue ( 'Item number', '%0.5d', [ FBike.Battery.ItemNumber ] );
    MfdDate := FBike.Battery.ManufacturingDate;
    ShowValue ( 'Manufacturing date', '%s', [ DateToStr ( MfdDate ) ] );
    ShowValue ( 'Location', '%d', [ FBike.Battery.Location ] );
    ShowValue ( 'Cellpack item number', '%0.5d', [ FBike.Battery.CellPackItemNumber ] );
    ShowValue ( 'Supervisor version', '%d', [ FBike.Battery.SupervisorVersion ] );
    ShowValue ( 'Communication mode', '%s', [ BatteryCommunicationModeToStr ( FBike.Battery.CommunicationMode ) ] );
    ShowValue ( 'BOM', '%s', [ BOMToStr ( FBike.Battery.BOM ) ] );
    ShowValue ( 'Nominal voltage', '%dV', [ FBike.Battery.NominalVoltage ]);
    PrintPCB;
    ShowValue ( '' );

    // Misc settings
    ShowValue ( '' );

    PrintChargerData;

    // Charge info

    PrintChargeStatistics;

    // Voltages readings
    ShowValue ( '  Readings' );
    ShowValue ( '  --------' );
    ShowValue ( 'Battery level', '%0.2f%%', [ FBike.Battery.ChargeLevel ] );
    ShowValue ( 'Voltage', '%0.2fV', [ FBike.Battery.Voltage ]);
    ShowValue ( 'Relative voltage', '%0.2f%%', [ FBike.Battery.NormalizedVoltage ] );
    ShowValue ( 'Maximum relative voltage', '%0.2f%%', [ FBike.Battery.StatsVoltageMax ] );
    ShowValue ( 'Minimum relative voltage', '%0.2f%%', [ FBike.Battery.StatsVoltageMin ] );
    ShowValue ( 'Avg. relative voltage', '%0.2f%%', [ FBike.Battery.StatsVoltageMean ] );
    ShowValue ( '' );
    ShowValue ( 'Input voltage', '%0.2fV', [ FBike.Battery.InputVoltage ]);
    ShowValue ( 'Internal battery voltage', '%0.2fV', [ FBike.Battery.InternalBatteryVoltage ] );
    ShowValue ( 'Console voltage', '%0.2fV', [ FBike.Battery.ConsoleVoltage ] );
    ShowValue ( '12V voltage', '%0.2fV', [ FBike.Battery.Voltage12V ] );
    ShowValue ( 'Accessory voltage reading', '%0.2fV', [ FBike.Battery.CurrentAccessoryVoltage ] );
    ShowValue ( '' );
    ShowValue ( 'Cell monitor BOMID voltage', '%0.0fmV', [ FBike.Battery.CellMonitorBOMIDVoltage*1000 ] );
    ShowValue ( 'Cell monitor PackID voltage', '%0.0fmV', [ FBike.Battery.CellMonitorPackIDVoltage*1000 ] );
    ShowValue ( 'Cell monitor 3.3V voltage', '%0.2fV', [ FBike.Battery.CellMonitor3V3Voltage ] );
    ShowValue ( 'Cell monitor 5V voltage', '%0.2fV', [ FBike.Battery.CellMonitor5VVoltage ] );
    ShowValue ( '' );
    ShowValue ( 'Cellpack current', '%0.2fA', [ FBike.Battery.CellpackCurrent ] );
    ShowValue ( '' );
    ShowValue ( 'Battery temp max', '%dC', [ FBike.Battery.StatsTemperatureMax ] );
    ShowValue ( 'Battery temp min', '%dC', [ FBike.Battery.StatsTemperatureMin ] );
    ShowValue ( '' );
    ShowValue ( 'Power cycles', '%d', [ FBike.Battery.StatsPowerCycles ] );
    ShowValue ( 'Resets', '%d', [ FBike.Battery.StatsReset ] );
    ShowValue ( 'Watchdog resets', '%d', [ FBike.Battery.WatchdogResets ] );
    ShowValue ( 'PowerOn resets', '%d', [ FBike.Battery.PowerOnResets ] );
    ShowValue ( 'RTC resync', '%d', [ FBike.Battery.StatsRTCResync ] );
    ShowValue ( '' );
    ShowValue ( 'Deep sleep on inactivity count', '%d days', [ FBike.Battery.StatsDeepSleepInactivityCount ] );
    ShowValue ( 'Deep sleep on low SOC count', '%d days', [ FBike.Battery.StatsDeepSleepSOCLowCount ] );
    ShowValue ( 'Deep sleep on low voltage count', '%d days', [ FBike.Battery.StatsDeepSleepLowVoltageCount ] );
    ShowValue ( '' );

    PrintCellInfo;

    // Health
    ShowValue ( '  Health' );
    ShowValue ( '  ------' );
    // ShowValue ( 'vctrlShorts', '%d', [ FBike.Battery.VCtrlShorts ] );
    ShowValue ( 'Low voltage alarm count', '%d', [ FBike.Battery.StatsLowVoltageBuzzerCount ] );
    ShowValue ( 'Cell dead short count', '%d', [ FBike.Battery.StatsCellDeadShortCount ] );
    ShowValue ( 'Cell partial short count', '%d', [ FBike.Battery.StatsCellPartialShortCount ] );
    ShowValue ( 'Cell voltage collapse count', '%d', [ FBike.Battery.StatsCellVoltageCollapseCount ] );
    ShowValue ( 'Control voltage shorts count', '%d', [ FBike.Battery.StatsVControlShorts ] );
    ShowValue ( '5V voltage shorts count', '%d', [ FBike.Battery.StatsV5VShorts ] );
    ShowValue ( 'RTC status', '%s', [ RTCStatusToStr ( FBike.Battery.RTCStatus ) ] );
    ShowValue ( 'Status flags', '%s', [ BatteryFlagsToStr ( FBike.Battery.StatusFlags ) ] );
    ShowValue ( 'Permanent failure flag', '%s', [ BoolToStr ( FBike.Battery.StatusPermanentFailureFlags ) ] );
    ShowValue ( '' );

    // Config
    ShowValue ( '  Configuration' );
    ShowValue ( '  -------------' );
    ShowValue ( 'Allow charging on bike', '%s', [ BoolToStr ( FBike.Battery.AllowChargingOnBike ) ] );
    ShowValue ( 'Wake on power voltage', '%s', [ BoolToStr ( FBike.Battery.WakeOnPowerVoltage ) ] );
    ShowValue ( 'Enable power voltage', '%s', [ BoolToStr ( FBike.Battery.EnablePowerVoltage ) ] );
    ShowValue ( 'Enable control voltage', '%s', [ BoolToStr ( FBike.Battery.EnableControlVoltage ) ] );
    ShowValue ( 'Accessory mounted', '%s', [ BoolToStr ( FBike.Battery.AccessoryMounted ) ] );
    ShowValue ( 'Accessory enabled', '%s', [ BoolToStr ( FBike.Battery.AccessoryEnabled ) ] );
    ShowValue ( 'Accessory voltage', '%0.1fV', [ FBike.Battery.AccessoryVoltage ] );
    ShowValue ( 'Accessory shutdown delay', '%ds', [ FBike.Battery.AccessoryShutdownDelay ] );
    ShowValue ( 'Taillamp intensity', '%d%%', [ FBike.Battery.TaillampIntensity ] );
    ShowValue ( 'Motor precharge time', '%ds', [ FBike.Battery.MotorPrechargeTime ] );
    ShowValue ( 'Power voltage shutdown delay', '%ds', [ FBike.Battery.PowerOutputShutdownDelay ] );
    ShowValue ( 'System shutdown delay', '%ds', [ FBike.Battery.AutoShutdownDelay ] );
    ShowValue ( 'Enable internal battery voltage', '%s', [ BoolToStr ( FBike.Battery.EnableInternalBatteryVoltage ) ] );
    ShowValue ( 'Max. power voltage regen curent', '%0.2fA', [ FBike.Battery.MaxPowervoltageRegenCurrent ] );
    ShowValue ( 'Max. power voltage curent', '%0.2fA', [ FBike.Battery.MaxPowervoltageCurrent ] );
    ShowValue ( 'Cap sense SOC mode', '%s', [ CapSenseSOCModeToStr ( FBike.Battery.CapSenseSOCMode ) ] );
    ShowValue ( 'Deep sleep on inactivity delay', '%d days', [ FBike.Battery.DeepSleepInactivityDelay ] );
    ShowValue ( 'Deep sleep on low SOC delay', '%d days', [ FBike.Battery.DeepSleepSOCLowDelay ] );
    ShowValue ( 'Min. allowed gas gage temp.', '%d', [ FBike.Battery.ConfigMinGasgageTemperature ] );
    ShowValue ( 'Max. allowed gas gage temp.', '%d', [ FBike.Battery.ConfigMaxGasgageTemperature ] );
    ShowValue ( 'Min. allowed pack temp.', '%d', [ FBike.Battery.ConfigMinPackTemperature ] );
    ShowValue ( 'Max. allowed pack temp.', '%d', [ FBike.Battery.ConfigMaxPackTemperature ] );
    ShowValue ( 'Charger voltage calibration value', '%0.2f', [ FBike.Battery.ChargerVoltageCalibrationValue ] );
    ShowValue ( 'Charger current calibration value', '%0.2f', [ FBike.Battery.ChargerCurrentCalibrationValue ] );
    ShowValue ( 'Charger calibration value source', '%0.2x', [ FBike.Battery.ChargerCalibrationValuesSource ] );
    ShowValue ( '3V3 calibration value', '%0.3f', [ FBike.Battery.CalibCalibration3V3 ] );
    ShowValue ( 'Cap sense sensitivity', '%0.2f', [ FBike.Battery.CalibCapsense ] );
    ShowValue ( '' );

    // Timestamps
    ShowValue ( '  Timestamps' );
    ShowValue ( '  ----------' );
    ShowValue ( 'RTC time', '%d', [ FBike.Battery.RTCTime ] );
    ShowValue ( 'RTC time', '%s', [ DateTimeToStr ( FBike.Battery.RTCTimeDT ) ] );
    ShowValue ( 'Last valid battery timestamp', '%d', [ FBike.Battery.RTCLastValidTimestamp ] );
    ShowValue ( 'Last valid battery timestamp', '%s', [ DateTimeToStr ( FBike.Battery.RTCLastValidTimestampDT ) ] );
    ShowValue ( 'Last charge timestamp', '%d', [ FBike.Battery.RTCLastChargeTimestamp ] );
    ShowValue ( 'Last charge timestamp', '%s', [ DateTimeToStr ( FBike.Battery.RTCLastChargeTimestampDT ) ] );
    ShowValue ( '' );

    // ????
    ShowValue ( '  yet unassigned' );
    ShowValue ( '  --------------' );
    ShowValue ( 'LMD', '%0.2fAh', [ FBike.Battery.LMD ] );
    ShowValue ( 'Estimated SOC', '%d%%', [ FBike.Battery.EstimatedSOC ] );
    ShowValue ( 'Shipmode', '%s', [ BoolToStr ( FBike.Battery.Shipmode ) ] );
    ShowValue ( 'Config type', '%d', [ FBike.Battery.ConfigType ] );
    ShowValue ( 'Diag', '%d', [ FBike.Battery.ConfigDiag ] );
    ShowValue ( 'LMD adapt', '%d', [ FBike.Battery.StatsLMDAdapt ] );
    ShowValue ( 'RTC control', '%d', [ FBike.Battery.RTCControl ] );
    ShowValue ( 'ILMD', '%0.2fAh', [ FBike.Battery.ConfigILMD ] );
    ShowValue ( 'Gasgage DMFSD', '%d', [ FBike.Battery.GasGageDMFSD ] );
    ShowValue ( 'Gasgage SOC', '%d%%', [ FBike.Battery.GasGageSOC ] );
    ShowValue ( 'Gasgage AI', '%0.2fA', [ FBike.Battery.GasGageAI ] );
    ShowValue ( 'Gasgage LMD', '%0.2fAh', [ FBike.Battery.GasGageLMD ] );
    ShowValue ( 'Gasgage status flags', '%0.2x', [ FBike.Battery.GasGageStatusFlags ] );
    ShowValue ( 'Gasgage voltage', '%0.2fV', [ FBike.Battery.GasGageVoltage ] );
    ShowValue ( 'Gasgage temperature', '%0.2fC', [ FBike.Battery.GasGageTemperature ] );
    ShowValue ( 'Gasgage voltage divider', '%0.2f', [ FBike.Battery.GasGageVoltageDivider ] );
    ShowValue ( 'Gasgage jitter calibration', '%d', [ FBike.Battery.StatsGasGageJitterCalibration ] );
    ShowValue ( 'NAC', '%d', [ FBike.Battery.ConfigNAC ] );
    ShowValue ( 'ProtectMode', '%d', [ FBike.Battery.ProtectMode ] );
    ShowValue ( 'ProtectControl', '%d', [ FBike.Battery.ProtectControl ] );



    ShowValue ( 'Test flags', '%0.4x', [ FBike.Battery.StatusTestFlags ] );

    ShowValue ( 'Battery test button sensitivity', '%d', [ FBike.Battery.StatusCapSense ] );
    ShowValue ( 'Battery test button sensitivity reference', '%d', [ FBike.Battery.StatusCapSenseReference ] );
    ShowValue ( '' );

  finally
    FBike.KeepAlive := true;
  end;
end;

// write the motor settings to the info window
procedure TfrmBionXMain.ShowMotorSettings;
begin
  FBike.KeepAlive := false;
  try
    FBike.Motor.CheckDeviceReady;

    // Partinfo
    ShowValue ( 'Motor information:' );
    ShowValue ( 'Hardware version', '%0.2d', [ FBike.Motor.HardwareVersion ] );
    ShowValue ( 'Software version', '%0.2d', [ FBike.Motor.SoftwareVersion ] );
    ShowValue ( 'Sub version', '%0.2d', [ FBike.Motor.SubVersion ] );
    ShowValue ( 'Part number', '%0.5d', [ FBike.Motor.PartNumber ] );
    ShowValue ( 'Item number', '%0.5d', [ FBike.Motor.ItemNumber ] );
    ShowValue ( 'Manufacturing date', '%s', [ DateToStr ( FBike.Motor.ManufacturingDate ) ] );
    ShowValue ( 'Stator type', '%d', [ FBike.Motor.StatorType ] );
    ShowValue ( 'Stator part number', '%d', [ FBike.Motor.StatorPartNumber ] );
    ShowValue ( 'Location', '%d', [ FBike.Motor.Location ] );
    ShowValue ( 'OEM', '%d', [ FBike.Motor.OEM ] );
    ShowValue ( 'Procuct Id', '%d', [ FBike.Motor.Product ] );
    ShowValue ( 'Region', '%d', [ FBike.Motor.PreferenceRegion ] );
    ShowValue ( '' );


    // Config
    ShowValue ( '  Configuration' );
    ShowValue ( '  -------------' );
    ShowValue ( 'Communication mode', '%s', [ MotorCommunicationModeToStr ( FBike.Motor.ConfigCommMode ) ] );
    ShowValue ( 'Wheel circumference', '%dmm', [ FBike.Motor.WheelCircumference ]);
    ShowValue ( 'Speed limit', '%0.2fkm/h', [ FBike.Motor.AssistMaxSpeed ] );
    ShowValue ( 'Direction', '%d', [ FBike.Motor.AssistDirection ] );
    ShowValue ( 'Low speed ramp', '%s', [ BoolToStr ( FBike.Motor.LowSpeedRamp ) ] );
    ShowValue ( 'Dynamic flag', '%s', [ BoolToStr(FBike.Motor.AssistVQDynamicFlag ) ] );
    ShowValue ( 'PWM limit', '%s', [ BoolToStr(FBike.Motor.ConfigEnablePWMLimit ) ] );
    ShowValue ( '' );
    ShowValue ( 'Walk level', '%0.2f%%', [ FBike.Motor.WalkLevel ] );
    ShowValue ( 'Max. walk level', '%0.2f%%', [ FBike.Motor.WalkLevelMax ] );
    ShowValue ( 'Walk decrease start', '%0.1fkm/h', [ FBike.Motor.WalkSpeedDecreaseStartSpeed ] );
    ShowValue ( 'Walk decrease end', '%0.1fkm/h', [ FBike.Motor.WalkSpeedDecreaseEndSpeed ] );
    ShowValue ( '' );
    ShowValue ( 'Assist level offslope', '%0.2f%%/s', [ FBike.Motor.AssistLevelOffSlope ] );
    ShowValue ( 'Assist regen inflex', '%0.2frpm', [ FBike.Motor.AssistRegenInflex ] );
    ShowValue ( 'Assist max speed derate delta', '%0.2frpm', [ FBike.Motor.AssistMaxSpeedDerateDelta ] );
    ShowValue ( '' );

    // Statistic & Status
    ShowValue ( '  Readings' );
    ShowValue ( '  --------' );
    ShowValue ( 'Odometer', '%dkm', [ FBike.Motor.StatsOdo ] );
    ShowValue ( 'Operating time', '%0.2fh', [ FBike.Motor.StatsChronoHours + FBike.Motor.StatsChronoSeconds/3600 ] );
  //    ShowValue ( 'Chrono hours', '%dh', [ FBike.Motor.ChronoHours ] );
  //    ShowValue ( 'Chrono seconds', '%ds', [ FBike.Motor.ChronoSeconds ] );
    ShowValue ( 'Max measured power voltage', '%0.2fV', [ FBike.Motor.StatsMaxVPower ] );
    ShowValue ( 'Max measured temperature', '%0.2f', [ FBike.Motor.StatsMaxVTemp ] );
    ShowValue ( '' );
    ShowValue ( 'Hall DCHS', '%d', [ FBike.Motor.StatsHallDCHS ] );
    ShowValue ( 'Hall trans', '%d', [ FBike.Motor.StatsHallTrans ] );
    ShowValue ( 'Hall ring', '%d', [ FBike.Motor.StatsHallRing ] );
    ShowValue ( 'Hall lost', '%d', [ FBike.Motor.StatsHallLost ] );
    ShowValue ( '' );
    ShowValue ( 'Status', '%s', [ MotorStatusToStr(FBike.Motor.StatusMain) ] );
    ShowValue ( 'Status codes', '0x%0.2x', [ FBike.Motor.StatusCodes ] );
    ShowValue ( 'Status codes latch', '0x%0.2x', [ FBike.Motor.StatusCodesLatch ] );
    ShowValue ( '' );
    ShowValue ( 'Temperature', '%dC', [ FBike.Motor.Temperature ] );
    ShowValue ( 'Power voltage', '%0.2fV', [ FBike.Motor.PowerVoltage ] );
    ShowValue ( '12V voltage', '%0.2fV', [ FBike.Motor._12VVoltage ] );
    ShowValue ( '5V voltage', '%0.2fV', [ FBike.Motor._5VVoltage ] );
    ShowValue ( '' );
    ShowValue ( 'Assist level', '%0.2f%%', [ FBike.Motor.AssistLevel ] );
    ShowValue ( 'Motor speed', '%0.2frpm', [ FBike.Motor.MotorSpeed ] );
    ShowValue ( 'Motor power', '%0.2f%%', [ FBike.Motor.MotorPower ] );
    ShowValue ( '' );
    ShowValue ( 'Torque gauge polarity', '%d', [ FBike.Motor.TorqueGaugePolarity ] );
    ShowValue ( 'Torque gauge type', '%d', [ FBike.Motor.TorqueGaugeType ] );
    ShowValue ( 'Torque gauge noise', '%0.2f%%', [ FBike.Motor.TorqueGaugeNoise ] );
    ShowValue ( 'Torque gauge delay', '%0.2fs', [ FBike.Motor.TorqueGaugeDelay ] );
    ShowValue ( 'Torque gauge speed', '%0.2frpm', [ FBike.Motor.TorqueGaugeSpeed ] );
    ShowValue ( 'Torque gauge voltage', '%0.2fV', [ FBike.Motor.TorqueGaugeVoltage ] );
    ShowValue ( 'Torque gauge reference', '%0.2fV', [ FBike.Motor.TorqueGaugeReference ] );
    ShowValue ( 'Torque gauge gain', '%0.2f%%', [ FBike.Motor.TorqueGaugeGain ] );
    ShowValue ( 'Torque gauge max voltage', '%0.2fV', [ FBike.Motor.TorqueGaugeMaxVoltage ] );
    ShowValue ( 'Torque gauge max voltage delay', '%0.2fs', [ FBike.Motor.TorqueGaugeMaxVoltageDelay ] );
    ShowValue ( '' );

  finally
    FBike.KeepAlive := true;
  end;
end;

// write the sensor settings to the info window
procedure TfrmBionXMain.ShowSensorSettings;
begin
  FBike.KeepAlive := false;
  try
    FBike.Sensor.CheckDeviceReady;

    // Partinfo
    ShowValue ( 'Sensor information:' );
    ShowValue ( 'Hardware version', '%0.2d', [ FBike.Sensor.HardwareVersion ] );
    ShowValue ( 'Software version', '%0.2d', [ FBike.Sensor.SoftwareVersion ] );
    ShowValue ( 'Sub version', '%0.2d', [ FBike.Sensor.SubVersion ] );
    ShowValue ( 'Part number', '%0.5d', [ FBike.Sensor.PartNumber ] );
    ShowValue ( 'Item number', '%0.5d', [ FBike.Sensor.ItemNumber ] );
    ShowValue ( 'Manufacturing date', '%s', [ DateToStr ( FBike.Sensor.ManufacturingDate ) ] );
    ShowValue ( '' );

    // Config
    ShowValue ( '  Configuration' );
    ShowValue ( '  -------------' );
    ShowValue ( 'Mode', '%s', [ SensorModeToStr ( FBike.Sensor.ConfigMode ) ] );
    ShowValue ( 'Gauge gain', '%0.2f', [ FBike.Sensor.ConfigGaugeGain ]);
    ShowValue ( 'Ramp up steps', '%d', [ FBike.Sensor.ConfigRampUpSteps ] );
    ShowValue ( 'Decay delay', '%d', [ FBike.Sensor.ConfigDecayDelay ] );
    ShowValue ( 'Decay steps', '%d', [ FBike.Sensor.ConfigDecaySteps ] );
    ShowValue ( 'Speedthreshold', '%0.2frpm', [ FBike.Sensor.ConfigSpeedThreshold ] );
    ShowValue ( 'Ramp active over threshold', '%s', [ BoolToStr(FBike.Sensor.ConfigRampActiveOverThreshold ) ] );
    ShowValue ( 'Input offset', '%0.2fV', [ FBike.Sensor.ConfigInputOffset ] );
    ShowValue ( '' );

    // Status
    ShowValue ( '  Status' );
    ShowValue ( '  --------' );
    ShowValue ( 'Torque voltage', '%0.2fV', [ FBike.Sensor.TorqueVoltage ] );
    ShowValue ( 'Cadence', '%0.2frpm', [ FBike.Sensor.Cadence ] );
    ShowValue ( 'Output voltage', '%0.2fV', [ FBike.Sensor.OutputVoltage ] );
    ShowValue ( 'Pulse counter', '%d', [ FBike.Sensor.PulseCounter ] );
    ShowValue ( '' );

  finally
    FBike.KeepAlive := true;
  end;
end;

(******************************************************************************)

// read and remember the values on the settings tab page
procedure TfrmBionXMain.ReadSettings;
begin
  FBike.KeepAlive := false;
  try
    // Console settings
    SetControlValue ( cbConsoleSwitches, ord(FBike.Console.PreferenceFlipSide), FSaveValues );
    SetControlValue ( cbLightButtonMode, FBike.Console.PreferenceLightButtonMode, FSaveValues );
    SetControlValue ( cbDisplayUnits, FBike.Console.PreferenceDisplayUnits, FSaveValues );
    SetControlValue ( cbShowRemainingDistanceAndTime, ord(FBike.Console.PreferenceTripToEmptyFlag), FSaveValues );
    SetControlValue ( cbLightsOnAtStart, ord(FBike.Console.PreferenceLightsOnAtStart), FSaveValues );

    if FBike.Console.HardwareVersion = 15 then
    begin
      cbRememberDisplayMode.Enabled := true;
      SetControlValue ( cbRememberDisplayMode, ord(FBike.Console.ConfigLastMode), FSaveValues );
    end
    else
      cbRememberDisplayMode.Enabled := false;

    SetControlValue ( edLCDContrast, FBike.Console.PreferenceLCDContrast, FSaveValues );
    if FBike.Console.HardwareVersion = 15 then
    begin
      cbShowMotorTemperature.Enabled := true;
      SetControlValue ( cbShowMotorTemperature, ord(FBike.Console.PreferenceExpertMode), FSaveValues );
    end
    else
      cbShowMotorTemperature.Enabled := false;

    // misc settings
    SetControlValue ( edWheelCircumference, FBike.Console.GeometryCirc, FSaveValues );
    SetControlValue ( edAutoShutdownDelay, FBike.Battery.AutoShutdownDelay, FSaveValues );
    SetControlValue ( edOdometer, FBike.Console.StatsOdometer, FSaveValues );

    // brakes
    if FBike.Console.AssistBrakeFlag then
      SetControlValue ( cbBrakeSensor, FBike.Console.AssistBrakePolarity+1, FSaveValues )
    else
      SetControlValue ( cbBrakeSensor, 0, FSaveValues );
    SetControlValue ( edBrakeRekuLevel, FBike.Console.AssistBrakeLevel, FSaveValues );

    // rekuperation levels
    SetControlValue ( edRekuperationLevel1, FBike.Console.RekuperationLevel1, FSaveValues );
    SetControlValue ( edRekuperationLevel2, FBike.Console.RekuperationLevel2, FSaveValues );
    SetControlValue ( edRekuperationLevel3, FBike.Console.RekuperationLevel3, FSaveValues );
    SetControlValue ( edRekuperationLevel4, FBike.Console.RekuperationLevel4, FSaveValues );

    // accessory
    SetControlValue ( cbAccessoryEnabled, ord(FBike.Battery.AccessoryEnabled), FSaveValues );
    SetControlValue ( edAccessoryVoltage, FBike.Battery.AccessoryVoltage, FSaveValues );
    SetControlValue ( edAccessoryShutdownDelay, FBike.Battery.AccessoryShutdownDelay, FSaveValues );
    SetControlValue ( edTaillampIntensity, FBike.Battery.TaillampIntensity, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

// write the modified values from the settings tab page back to bike
procedure TfrmBionXMain.WriteSettings;
begin
  FBike.KeepAlive := false;
  try
    // compare the curent values with remembered values and
    // write only differences to bike

    // Console settings
    if not CompareControlValue ( cbConsoleSwitches, FSaveValues ) then
      FBike.Console.PreferenceFlipSide := GetControlValue ( cbConsoleSwitches, FSaveValues )=1;
    if not CompareControlValue ( cbLightButtonMode, FSaveValues ) then
      FBike.Console.PreferenceLightButtonMode := GetControlValue ( cbLightButtonMode, FSaveValues );
    if not CompareControlValue ( cbDisplayUnits, FSaveValues ) then
      FBike.Console.PreferenceDisplayUnits := GetControlValue ( cbDisplayUnits, FSaveValues );
    if not CompareControlValue ( cbShowRemainingDistanceAndTime, FSaveValues ) then
      FBike.Console.PreferenceTripToEmptyFlag := GetControlValue ( cbShowRemainingDistanceAndTime, FSaveValues )=1;
    if not CompareControlValue ( cbLightsOnAtStart, FSaveValues ) then
      FBike.Console.PreferenceLightsOnAtStart := GetControlValue ( cbLightsOnAtStart, FSaveValues )=1;
    if not CompareControlValue ( cbRememberDisplayMode, FSaveValues ) then
      FBike.Console.ConfigLastMode := GetControlValue ( cbRememberDisplayMode, FSaveValues )=1;
    if not CompareControlValue ( edLCDContrast, FSaveValues ) then
      FBike.Console.PreferenceLCDContrast := GetControlValue ( edLCDContrast, FSaveValues );
    if not CompareControlValue ( cbShowMotorTemperature, FSaveValues ) then
      FBike.Console.PreferenceExpertMode := GetControlValue ( cbShowMotorTemperature, FSaveValues )=1;

    // misc settings
    if not CompareControlValue ( edWheelCircumference, FSaveValues ) then
      FBike.Console.GeometryCirc := GetControlValue ( edWheelCircumference, FSaveValues );
    if not CompareControlValue ( edAutoShutdownDelay, FSaveValues ) then
      FBike.Battery.AutoShutdownDelay := GetControlValue ( edAutoShutdownDelay, FSaveValues );
    if not CompareControlValue ( edOdometer, FSaveValues ) then
      FBike.Console.StatsOdometer := GetControlValue ( edOdometer, FSaveValues );

    // brake
    if not CompareControlValue ( cbBrakeSensor, FSaveValues ) then
      FBike.SetBrakeSensor( GetControlValue ( cbBrakeSensor, FSaveValues )>0, GetControlValue ( cbBrakeSensor, FSaveValues )-1 );
    if not CompareControlValue ( edBrakeRekuLevel, FSaveValues ) then
      FBike.Console.AssistBrakeLevel := GetControlValue ( edBrakeRekuLevel, FSaveValues );

    // rekuperation levels
    if not CompareControlValue ( edRekuperationLevel1, FSaveValues ) then
      FBike.Console.RekuperationLevel1 := GetControlValue ( edRekuperationLevel1, FSaveValues );
    if not CompareControlValue ( edRekuperationLevel2, FSaveValues ) then
      FBike.Console.RekuperationLevel2 := GetControlValue ( edRekuperationLevel2, FSaveValues );
    if not CompareControlValue ( edRekuperationLevel3, FSaveValues ) then
      FBike.Console.RekuperationLevel3 := GetControlValue ( edRekuperationLevel3, FSaveValues );
    if not CompareControlValue ( edRekuperationLevel4, FSaveValues ) then
      FBike.Console.RekuperationLevel4 := GetControlValue ( edRekuperationLevel4, FSaveValues );

    // Accessory
    if not CompareControlValue ( cbAccessoryEnabled, FSaveValues ) then
      FBike.Battery.AccessoryEnabled := GetControlValue ( cbAccessoryEnabled, FSaveValues )=1;
    if not CompareControlValue ( edAccessoryVoltage, FSaveValues ) then
      FBike.Battery.AccessoryVoltage := GetControlValue ( edAccessoryVoltage, FSaveValues );
    if not CompareControlValue ( edAccessoryShutdownDelay, FSaveValues ) then
      FBike.Battery.AccessoryShutdownDelay := GetControlValue ( edAccessoryShutdownDelay, FSaveValues );
    if not CompareControlValue ( edTaillampIntensity, FSaveValues ) then
      FBike.Battery.TaillampIntensity := GetControlValue ( edTaillampIntensity, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

(******************************************************************************)

procedure TfrmBionXMain.ReadTuning;
begin
  FBike.KeepAlive := false;
  try
    // speed limits
    SetControlValue ( edAssistMaxSpeed, FBike.Console.AssistMaxSpeed, FSaveValues );
    SetControlValue ( edAssistMinSpeed, FBike.Console.AssistMinSpeed, FSaveValues );
    SetControlValue ( edThrottleMaxSpeed, FBike.Console.ThrottleMaxSpeed, FSaveValues );
    SetControlValue ( cbEnabledOnStrain, ord(FBike.Console.ThrottleEnabledOnStrain), FSaveValues );

    // assist levels
    SetControlValue ( edInitialAssistLevel, FBike.Console.AssistInitLevel, FSaveValues );
    SetControlValue ( edAssistLevel1, FBike.Console.AssistLevel1, FSaveValues );
    SetControlValue ( edAssistLevel2, FBike.Console.AssistLevel2, FSaveValues );
    SetControlValue ( edAssistLevel3, FBike.Console.AssistLevel3, FSaveValues );
    SetControlValue ( edAssistLevel4, FBike.Console.AssistLevel4, FSaveValues );
    SetControlValue ( edAssistLevelMountain, FBike.Console.MountainAssistLevel, FSaveValues );

    // torque sensor
    SetControlValue ( edTorqueSensorGain, FBike.Console.TorqueSensorGain, FSaveValues );
    SetControlValue ( edTorqueSensorSpeed, FBike.Console.TorqueSensorSpeed, FSaveValues );
    SetControlValue ( edTorqueSensorExtraGain, FBike.Console.TorqueSensorExtraGain, FSaveValues );
    SetControlValue ( edTorqueSensorExtraGainMaxSpeed, FBike.Console.TorqueSensorExtraGainMaxSpeed, FSaveValues );
    SetControlValue ( edTorqueSensorSpeedGain, FBike.Console.AssistSpeedGain, FSaveValues );

    // Boost
    SetControlValue ( edBoostLevel, FBike.Console.ThrottleBoostTriggerLevel, FSaveValues );
    SetControlValue ( cbBoostDisplay, ord(FBike.Console.ThrottleEnableBoostDisplay), FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

procedure TfrmBionXMain.WriteTuning;
begin
  FBike.KeepAlive := false;
  try
    // compare the curent values with remembered values and
    // write only differences to bike

    // speed limits
    if not CompareControlValue ( edAssistMaxSpeed, FSaveValues ) then
      FBike.SetAssistMaxSpeed ( GetControlValue ( edAssistMaxSpeed, FSaveValues ) );
    if not CompareControlValue ( edAssistMinSpeed, FSaveValues ) then
      FBike.SetAssistMinSpeed ( GetControlValue ( edAssistMinSpeed, FSaveValues ) );
    if not CompareControlValue ( edThrottleMaxSpeed, FSaveValues ) then
      FBike.SetThrottleMaxSpeed ( GetControlValue ( edThrottleMaxSpeed, FSaveValues ) );
    if not CompareControlValue ( cbEnabledOnStrain, FSaveValues ) then
      FBike.Console.ThrottleEnabledOnStrain := GetControlValue ( cbEnabledOnStrain, FSaveValues )=1;

    // assist levels
    if not CompareControlValue ( edInitialAssistLevel, FSaveValues ) then
      FBike.Console.AssistInitLevel := GetControlValue ( edInitialAssistLevel, FSaveValues );
    if not CompareControlValue ( edAssistLevel1, FSaveValues ) then
      FBike.Console.AssistLevel1 := GetControlValue ( edAssistLevel1, FSaveValues );
    if not CompareControlValue ( edAssistLevel2, FSaveValues ) then
      FBike.Console.AssistLevel2 := GetControlValue ( edAssistLevel2, FSaveValues );
    if not CompareControlValue ( edAssistLevel3, FSaveValues ) then
      FBike.Console.AssistLevel3 := GetControlValue ( edAssistLevel3, FSaveValues );
    if not CompareControlValue ( edAssistLevel4, FSaveValues ) then
      FBike.Console.AssistLevel4 := GetControlValue ( edAssistLevel4, FSaveValues );
    if not CompareControlValue ( edAssistLevelMountain, FSaveValues ) then
      FBike.Console.MountainAssistLevel := GetControlValue ( edAssistLevelMountain, FSaveValues );

    // torque sensor
    if not CompareControlValue ( edTorqueSensorGain, FSaveValues ) then
      FBike.Console.TorqueSensorGain := GetControlValue ( edTorqueSensorGain, FSaveValues );
    if not CompareControlValue ( edTorqueSensorSpeed, FSaveValues ) then
      FBike.Console.TorqueSensorSpeed := GetControlValue ( edTorqueSensorSpeed, FSaveValues );
    if not CompareControlValue ( edTorqueSensorExtraGain, FSaveValues ) then
      FBike.Console.TorqueSensorExtraGain := GetControlValue ( edTorqueSensorExtraGain, FSaveValues );
    if not CompareControlValue ( edTorqueSensorExtraGainMaxSpeed, FSaveValues ) then
      FBike.Console.TorqueSensorExtraGainMaxSpeed := GetControlValue ( edTorqueSensorExtraGainMaxSpeed, FSaveValues );
    if not CompareControlValue ( edTorqueSensorSpeedGain, FSaveValues ) then
      FBike.Console.AssistSpeedGain := GetControlValue ( edTorqueSensorSpeedGain, FSaveValues );

    // Boost
    if not CompareControlValue ( cbBoostDisplay, FSaveValues ) then
      FBike.Console.ThrottleEnableBoostDisplay := GetControlValue ( cbBoostDisplay, FSaveValues )=1;
    if not CompareControlValue ( edBoostLevel, FSaveValues ) then
      FBike.Console.ThrottleBoostTriggerLevel := GetControlValue ( edBoostLevel, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

(******************************************************************************)

procedure TfrmBionXMain.ReadConsole;
begin
  FBike.KeepAlive := false;
  try
    // Geometry
    SetControlValue ( edConsoleGeometryCirc, FBike.Console.GeometryCirc, FSaveValues );

    //Config
    SetControlValue ( cbConsoleConfigTestmode, ord(FBike.Console.ConfigTestMode), FSaveValues );
    SetControlValue ( edConsoleConfigServiceTimestamp, FBike.Console.ConfigServiceTimeStamp, FSaveValues );
    SetControlValue ( edConsoleConfigServiceDistance, FBike.Console.ConfigServiceDistance, FSaveValues );
    SetControlValue ( cbConsoleConfigLastmode, ord(FBike.Console.ConfigLastMode), FSaveValues );

    // Assist
    SetControlValue ( cbConsoleAssistMaxSpeedFlag, ord(FBike.Console.AssistMaxSpeedFlag), FSaveValues );
    SetControlValue ( edConsoleAssistMaxSpeed, FBike.Console.AssistMaxSpeed, FSaveValues );
    SetControlValue ( cbConsoleAssistMinSpeedFlag, ord(FBike.Console.AssistMinSpeedFlag), FSaveValues );
    SetControlValue ( edConsoleAssistMinSpeed, FBike.Console.AssistMinSpeed, FSaveValues );
    SetControlValue ( edConsoleAssistBrakeLevel, FBike.Console.AssistBrakeLevel, FSaveValues );
    SetControlValue ( cbConsoleAssistBrakeFlag, ord(FBike.Console.AssistBrakeFlag), FSaveValues );
    SetControlValue ( cbConsoleAssistAutoregenFlag, ord(FBike.Console.AssistAutoRegen), FSaveValues );
    SetControlValue ( cbConsoleAssistBrakePolarity, FBike.Console.AssistBrakePolarity, FSaveValues );
    SetControlValue ( edConsoleAssistGaugeFilter, FBike.Console.AssistGaugeFilter, FSaveValues );
    SetControlValue ( edConsoleAssistGaugeGain, FBike.Console.AssistGaugeGain, FSaveValues );
    SetControlValue ( edConsoleAssistGainA, FBike.Console.AssistGainA, FSaveValues );
    SetControlValue ( edConsoleAssistGainB, FBike.Console.AssistGainB, FSaveValues );
    SetControlValue ( edConsoleAssistSpeedGain, FBike.Console.AssistSpeedGain, FSaveValues );
    SetControlValue ( edConsoleAssistGaugeJoint, FBike.Console.AssistGaugeJoint, FSaveValues );
    SetControlValue ( edConsoleAssistLevel1, FBike.Console.AssistLevel1, FSaveValues );
    SetControlValue ( edConsoleAssistLevel2, FBike.Console.AssistLevel2, FSaveValues );
    SetControlValue ( edConsoleAssistLevel3, FBike.Console.AssistLevel3, FSaveValues );
    SetControlValue ( edConsoleAssistLevel4, FBike.Console.AssistLevel4, FSaveValues );
    SetControlValue ( edConsoleAssistLevelR1, FBike.Console.RekuperationLevel1, FSaveValues );
    SetControlValue ( edConsoleAssistLevelR2, FBike.Console.RekuperationLevel2, FSaveValues );
    SetControlValue ( edConsoleAssistLevelR3, FBike.Console.RekuperationLevel3, FSaveValues );
    SetControlValue ( edConsoleAssistLevelR4, FBike.Console.RekuperationLevel4, FSaveValues );
    SetControlValue ( edConsoleAssistInitLevel, FBike.Console.AssistInitLevel, FSaveValues );
    SetControlValue ( edConsoleAssistMountainCap, FBike.Console.AssistMountainCap, FSaveValues );

    //Throttle
    SetControlValue ( cbConsoleThrottleMaxSpeedFlag, ord(FBike.Console.ThrottleMaxSpeedFlag ), FSaveValues );
    SetControlValue ( edConsoleThrottleMaxSpeed, FBike.Console.ThrottleMaxSpeed, FSaveValues );
    SetControlValue ( cbConsoleThrottleEnabledOnStrain, ord(FBike.Console.ThrottleEnabledOnStrain), FSaveValues );
    SetControlValue ( cbConsoleThrottleEnableBoostDisplay, ord(FBike.Console.ThrottleEnableBoostDisplay), FSaveValues );
    SetControlValue ( edConsoleThrottleBoostTriggerLevel, FBike.Console.ThrottleBoostTriggerLevel, FSaveValues );
    SetControlValue ( cbConsoleThrottleCalibrated, ord(FBike.Console.ThrottleCalibrated), FSaveValues );
    SetControlValue ( edConsoleThrottleMin, FBike.Console.ThrottleMinActorValue, FSaveValues );
    SetControlValue ( edConsoleThrottleMax, FBike.Console.ThrottleMaxActorValue, FSaveValues );

    // Preference
    SetControlValue ( cbConsolePreferenceTripToEmptyFlag, ord(FBike.Console.PreferenceTripToEmptyFlag), FSaveValues );
    SetControlValue ( cbConsolePreferenceDisplayUnits, FBike.Console.PreferenceDisplayUnits, FSaveValues );
    SetControlValue ( edConsolePreferenceNIP, FBike.Console.PreferenceNIP, FSaveValues );
    SetControlValue ( edConsolePreferenceLCDContrast, FBike.Console.PreferenceLCDContrast, FSaveValues );
    SetControlValue ( edConsolePreferenceCodes, IntToHex(FBike.Console.PreferenceCodes,8), FSaveValues );
    SetControlValue ( edConsolePreferenceCodesRW, IntToHex(FBike.Console.PreferenceCodesRW,8), FSaveValues );
    SetControlValue ( edConsolePreferenceRegion, FBike.Console.PreferenceRegion, FSaveValues );
    SetControlValue ( edConsolePreferenceConfigBit0, FBike.Console.PreferenceConfigBit0, FSaveValues );
    SetControlValue ( cbConsolePreferenceFlipSide, ord(FBike.Console.PreferenceFlipSide), FSaveValues );
    SetControlValue ( cbConsolePreferenceLightButtonMode, FBike.Console.PreferenceLightButtonMode, FSaveValues );
    SetControlValue ( cbConsolePreferenceLightsOnAtStart, ord(FBike.Console.PreferenceLightsOnAtStart), FSaveValues );
    SetControlValue ( cbConsolePreferenceExpertMode, ord(FBike.Console.PreferenceExpertMode), FSaveValues );
    SetControlValue ( edConsolePreferenceThrottleMode, FBike.Console.PreferenceThrottleMode, FSaveValues );

    // Stats
    SetControlValue ( edConsoleStatsOdo, FBike.Console.StatsOdometer, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

procedure TfrmBionXMain.WriteConsole;
var
  Codes : longint;
begin
  FBike.KeepAlive := false;
  try
    // Geometry
    if not CompareControlValue ( edConsoleGeometryCirc, FSaveValues ) then
      FBike.Console.GeometryCirc := GetControlValue ( edConsoleGeometryCirc, FSaveValues );

    //Config
    if not CompareControlValue ( cbConsoleConfigTestmode, FSaveValues ) then
      FBike.Console.ConfigTestMode := GetControlValue ( cbConsoleConfigTestmode, FSaveValues )=1;
    if not CompareControlValue ( edConsoleConfigServiceTimestamp, FSaveValues ) then
      FBike.Console.ConfigServiceTimeStamp := GetControlValue ( edConsoleConfigServiceTimestamp, FSaveValues );
    if not CompareControlValue ( edConsoleConfigServiceDistance, FSaveValues ) then
      FBike.Console.ConfigServiceDistance := GetControlValue ( edConsoleConfigServiceDistance, FSaveValues );
    if not CompareControlValue ( cbConsoleConfigLastmode, FSaveValues ) then
      FBike.Console.ConfigLastMode  := GetControlValue ( cbConsoleConfigLastmode, FSaveValues )=1;

    // Assist
    if not CompareControlValue ( cbConsoleAssistMaxSpeedFlag, FSaveValues ) then
      FBike.Console.AssistMaxSpeedFlag := GetControlValue ( cbConsoleAssistMaxSpeedFlag, FSaveValues )=1;
    if not CompareControlValue ( edConsoleAssistMaxSpeed, FSaveValues ) then
      FBike.Console.AssistMaxSpeed := GetControlValue ( edConsoleAssistMaxSpeed, FSaveValues );
    if not CompareControlValue ( cbConsoleAssistMinSpeedFlag, FSaveValues ) then
      FBike.Console.AssistMinSpeedFlag := GetControlValue ( cbConsoleAssistMinSpeedFlag, FSaveValues )=1;
    if not CompareControlValue ( edConsoleAssistMinSpeed, FSaveValues ) then
      FBike.Console.AssistMinSpeed := GetControlValue ( edConsoleAssistMinSpeed, FSaveValues );
    if not CompareControlValue ( edConsoleAssistBrakeLevel, FSaveValues ) then
      FBike.Console.AssistBrakeLevel := GetControlValue ( edConsoleAssistBrakeLevel, FSaveValues );
    if not CompareControlValue ( cbConsoleAssistBrakeFlag, FSaveValues ) then
      FBike.Console.AssistBrakeFlag := GetControlValue ( cbConsoleAssistBrakeFlag, FSaveValues )=1;
    if not CompareControlValue ( cbConsoleAssistAutoregenFlag, FSaveValues ) then
      FBike.Console.AssistAutoRegen := GetControlValue ( cbConsoleAssistAutoregenFlag, FSaveValues )=1;
    if not CompareControlValue ( cbConsoleAssistBrakePolarity, FSaveValues ) then
      FBike.Console.AssistBrakePolarity := GetControlValue ( cbConsoleAssistBrakePolarity, FSaveValues );
    if not CompareControlValue ( edConsoleAssistGaugeFilter, FSaveValues ) then
      FBike.Console.AssistGaugeFilter := GetControlValue ( edConsoleAssistGaugeFilter, FSaveValues );
    if not CompareControlValue ( edConsoleAssistGaugeGain, FSaveValues ) then
      FBike.Console.AssistGaugeGain := GetControlValue ( edConsoleAssistGaugeGain, FSaveValues );
    if not CompareControlValue ( edConsoleAssistGainA, FSaveValues ) then
      FBike.Console.AssistGainA := GetControlValue ( edConsoleAssistGainA, FSaveValues );
    if not CompareControlValue ( edConsoleAssistGainB, FSaveValues ) then
      FBike.Console.AssistGainB := GetControlValue ( edConsoleAssistGainB, FSaveValues );
    if not CompareControlValue ( edConsoleAssistSpeedGain, FSaveValues ) then
      FBike.Console.AssistSpeedGain := GetControlValue ( edConsoleAssistSpeedGain, FSaveValues );
    if not CompareControlValue ( edConsoleAssistGaugeJoint, FSaveValues ) then
      FBike.Console.AssistGaugeJoint := GetControlValue ( edConsoleAssistGaugeJoint, FSaveValues );
    if not CompareControlValue ( edConsoleAssistLevel1, FSaveValues ) then
      FBike.Console.AssistLevel1 := GetControlValue ( edConsoleAssistLevel1, FSaveValues );
    if not CompareControlValue ( edConsoleAssistLevel2, FSaveValues ) then
      FBike.Console.AssistLevel2 := GetControlValue ( edConsoleAssistLevel2, FSaveValues );
    if not CompareControlValue ( edConsoleAssistLevel3, FSaveValues ) then
      FBike.Console.AssistLevel3 := GetControlValue ( edConsoleAssistLevel3, FSaveValues );
    if not CompareControlValue ( edConsoleAssistLevel4, FSaveValues ) then
      FBike.Console.AssistLevel4 := GetControlValue ( edConsoleAssistLevel4, FSaveValues );
    if not CompareControlValue ( edConsoleAssistLevelR1, FSaveValues ) then
      FBike.Console.RekuperationLevel1 := GetControlValue ( edConsoleAssistLevelR1, FSaveValues );
    if not CompareControlValue ( edConsoleAssistLevelR2, FSaveValues ) then
      FBike.Console.RekuperationLevel2 := GetControlValue ( edConsoleAssistLevelR2, FSaveValues );
    if not CompareControlValue ( edConsoleAssistLevelR3, FSaveValues ) then
      FBike.Console.RekuperationLevel3 := GetControlValue ( edConsoleAssistLevelR3, FSaveValues );
    if not CompareControlValue ( edConsoleAssistLevelR4, FSaveValues ) then
      FBike.Console.RekuperationLevel4 := GetControlValue ( edConsoleAssistLevelR4, FSaveValues );
    if not CompareControlValue ( edConsoleAssistInitLevel, FSaveValues ) then
      FBike.Console.AssistInitLevel := GetControlValue ( edConsoleAssistInitLevel, FSaveValues );
    if not CompareControlValue ( edConsoleAssistMountainCap, FSaveValues ) then
      FBike.Console.AssistMountainCap := GetControlValue ( edConsoleAssistMountainCap, FSaveValues );

    //Throttle
    if not CompareControlValue ( cbConsoleThrottleMaxSpeedFlag, FSaveValues ) then
      FBike.Console.ThrottleMaxSpeedFlag := GetControlValue ( cbConsoleThrottleMaxSpeedFlag, FSaveValues )=1;
    if not CompareControlValue ( edConsoleThrottleMaxSpeed, FSaveValues ) then
      FBike.Console.ThrottleMaxSpeed := GetControlValue ( edConsoleThrottleMaxSpeed, FSaveValues );
    if not CompareControlValue ( cbConsoleThrottleEnabledOnStrain, FSaveValues ) then
      FBike.Console.ThrottleEnabledOnStrain := GetControlValue ( cbConsoleThrottleEnabledOnStrain, FSaveValues )=1;
    if not CompareControlValue ( cbConsoleThrottleEnableBoostDisplay, FSaveValues ) then
      FBike.Console.ThrottleEnableBoostDisplay := GetControlValue ( cbConsoleThrottleEnableBoostDisplay, FSaveValues )=1;
    if not CompareControlValue ( edConsoleThrottleBoostTriggerLevel, FSaveValues ) then
      FBike.Console.ThrottleBoostTriggerLevel := GetControlValue ( edConsoleThrottleBoostTriggerLevel, FSaveValues );
    if not CompareControlValue ( cbConsoleThrottleCalibrated, FSaveValues ) then
      FBike.Console.ThrottleCalibrated := GetControlValue ( cbConsoleThrottleCalibrated, FSaveValues )=1;
    if not CompareControlValue ( edConsoleThrottleMin, FSaveValues ) then
      FBike.Console.ThrottleMinActorValue := GetControlValue ( edConsoleThrottleMin, FSaveValues );
    if not CompareControlValue ( edConsoleThrottleMax, FSaveValues ) then
      FBike.Console.ThrottleMaxActorValue := GetControlValue ( edConsoleThrottleMax, FSaveValues );

    // Preference
    if not CompareControlValue ( cbConsolePreferenceTripToEmptyFlag, FSaveValues ) then
      FBike.Console.PreferenceTripToEmptyFlag := GetControlValue ( cbConsolePreferenceTripToEmptyFlag, FSaveValues )=1;
    if not CompareControlValue ( cbConsolePreferenceDisplayUnits, FSaveValues ) then
      FBike.Console.PreferenceDisplayUnits := GetControlValue ( cbConsolePreferenceDisplayUnits, FSaveValues );
    if not CompareControlValue ( edConsolePreferenceNIP, FSaveValues ) then
      FBike.Console.PreferenceNIP := GetControlValue ( edConsolePreferenceNIP, FSaveValues );
    if not CompareControlValue ( edConsolePreferenceLCDContrast, FSaveValues ) then
      FBike.Console.PreferenceLCDContrast := GetControlValue ( edConsolePreferenceLCDContrast, FSaveValues );
    if not CompareControlValue ( edConsolePreferenceCodes, FSaveValues ) then
      if TryStrToInt ( '$'+GetControlValue ( edConsolePreferenceCodes, FSaveValues ), Codes ) then
        FBike.Console.ThrottleMaxActorValue := Codes;
    if not CompareControlValue ( edConsolePreferenceCodesRW, FSaveValues ) then
      if TryStrToInt ( '$'+GetControlValue ( edConsolePreferenceCodesRW, FSaveValues ), Codes ) then
        FBike.Console.ThrottleMaxActorValue := Codes;
    if not CompareControlValue ( edConsolePreferenceRegion, FSaveValues ) then
      FBike.Console.PreferenceRegion := GetControlValue ( edConsolePreferenceRegion, FSaveValues );
    if not CompareControlValue ( edConsolePreferenceConfigBit0, FSaveValues ) then
      FBike.Console.PreferenceConfigBit0 := GetControlValue ( edConsolePreferenceConfigBit0, FSaveValues );
    if not CompareControlValue ( cbConsolePreferenceFlipSide, FSaveValues ) then
      FBike.Console.PreferenceFlipSide := GetControlValue ( cbConsolePreferenceFlipSide, FSaveValues )=1;
    if not CompareControlValue ( cbConsolePreferenceLightButtonMode, FSaveValues ) then
      FBike.Console.PreferenceLightButtonMode := GetControlValue ( cbConsolePreferenceLightButtonMode, FSaveValues );
    if not CompareControlValue ( cbConsolePreferenceLightsOnAtStart, FSaveValues ) then
      FBike.Console.PreferenceLightsOnAtStart := GetControlValue ( cbConsolePreferenceLightsOnAtStart, FSaveValues )=1;
    if not CompareControlValue ( cbConsolePreferenceExpertMode, FSaveValues ) then
      FBike.Console.PreferenceExpertMode := GetControlValue ( cbConsolePreferenceExpertMode, FSaveValues )=1;
    if not CompareControlValue ( edConsolePreferenceThrottleMode, FSaveValues ) then
      FBike.Console.PreferenceThrottleMode := GetControlValue ( edConsolePreferenceThrottleMode, FSaveValues );

    // Stats
    if not CompareControlValue ( edConsoleStatsOdo, FSaveValues ) then
      FBike.Console.StatsOdometer := GetControlValue ( edConsoleStatsOdo, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

procedure TfrmBionXMain.ReadBattery;
begin
  FBike.KeepAlive := false;
  try

    // Timer
    SetControlValue ( edBatteryTimerPower, FBike.Battery.TimerPower, FSaveValues );
    SetControlValue ( edBatteryTimerAccessory, FBike.Battery.TimerAccessory, FSaveValues );
    SetControlValue ( edBatteryTimerPreCharge, FBike.Battery.TimerPreCharge, FSaveValues );
    SetControlValue ( edBatteryTimerMasterShutdown, FBike.Battery.TimerMasterShutdown, FSaveValues );

    // Status
    SetControlValue ( edBatteryStatusTestFlags, IntToHex(FBike.Battery.StatusTestFlags,4), FSaveValues );
    SetControlValue ( cbBatteryStatusPermanentFailureFlags, ord(FBike.Battery.StatusPermanentFailureFlags), FSaveValues );
    SetControlValue ( edBatteryStatusLeds, FBike.Battery.StatusLeds, FSaveValues );
    SetControlValue ( edBatteryStatusCapSense, FBike.Battery.StatusCapSense, FSaveValues );
    SetControlValue ( edBatteryStatusCapSenseReference, FBike.Battery.StatusCapSenseReference, FSaveValues );

    // CellMon
    SetControlValue ( cbBatteryCellMonBalancerEnabled, ord(FBike.Battery.CellMonBalancerEnabled), FSaveValues );

    // Charger
    FBike.Battery.UnlockProtection;
    try
      SetControlValue ( edBatteryChargerCurrent, FBike.Battery.ChargerCurrent, FSaveValues );
      SetControlValue ( edBatteryChargerFinalVoltage, FBike.Battery.ChargerFinalVoltage, FSaveValues );
      SetControlValue ( cbBatteryChargerMode, FBike.Battery.ChargerMode, FSaveValues );
    finally
      FBike.Battery.LockProtection;
    end;

    // Calib
    SetControlValue ( edBatteryCalibCapSense, FBike.Battery.CalibCapsense, FSaveValues );
    SetControlValue ( edBatteryCalibCalibration01, FBike.Battery.CalibCalibration[1], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration02, FBike.Battery.CalibCalibration[2], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration03, FBike.Battery.CalibCalibration[3], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration04, FBike.Battery.CalibCalibration[4], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration05, FBike.Battery.CalibCalibration[5], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration06, FBike.Battery.CalibCalibration[6], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration07, FBike.Battery.CalibCalibration[7], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration08, FBike.Battery.CalibCalibration[8], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration09, FBike.Battery.CalibCalibration[9], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration10, FBike.Battery.CalibCalibration[10], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration11, FBike.Battery.CalibCalibration[11], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration12, FBike.Battery.CalibCalibration[12], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration13, FBike.Battery.CalibCalibration[13], FSaveValues );
    SetControlValue ( edBatteryCalibCalibration3V3, FBike.Battery.CalibCalibration3V3, FSaveValues );

    // Stats
    SetControlValue ( edBatteryStatsv5VShorts, FBike.Battery.StatsV5VShorts, FSaveValues );
    SetControlValue ( edBatteryStatsVControlShorts, FBike.Battery.StatsVControlShorts, FSaveValues );
    if FBike.Battery.HardwareVersion < 60 then
      edBatteryStatsVControlShorts.MinValue := 255;
    SetControlValue ( edBatteryStatsLowBattBuzzCount, FBike.Battery.StatsLowVoltageBuzzerCount, FSaveValues );
    SetControlValue ( edBatteryStatsCellVoltageCollapseCount, FBike.Battery.StatsCellVoltageCollapseCount, FSaveValues );
    SetControlValue ( edBatteryStatsCellPartialShortCount, FBike.Battery.StatsCellPartialShortCount, FSaveValues );
    SetControlValue ( edBatteryStatsCellDeadShortCount, FBike.Battery.StatsCellDeadShortCount, FSaveValues );
    SetControlValue ( edBatteryStatsDeepSleepAfterLongInactivityPeriodCount, FBike.Battery.StatsDeepSleepInactivityCount, FSaveValues );
    SetControlValue ( edBatteryStatsDeepSleepAfterLowSocCount, FBike.Battery.StatsDeepSleepSOCLowCount, FSaveValues );
    SetControlValue ( edBatteryStatsDeepSleepExtremeLowBatteryVoltageCount, FBike.Battery.StatsDeepSleepLowVoltageCount, FSaveValues );
    SetControlValue ( edBatteryStatsDischargeEnergy, FBike.Battery.TotalDischargedEnergy, FSaveValues );
    SetControlValue ( edBatteryStatsChargeEnergy, FBike.Battery.TotalChargedEnergy, FSaveValues );
    SetControlValue ( edBatteryStatsReset, FBike.Battery.StatsReset, FSaveValues );
    SetControlValue ( edBatteryStatsGgjrCalib, FBike.Battery.StatsGasGageJitterCalibration, FSaveValues );
    SetControlValue ( edBatteryStatsResetWdt, FBike.Battery.StatsResetWdt, FSaveValues );
    SetControlValue ( edBatteryStatsRTCResync, FBike.Battery.StatsRTCResync, FSaveValues );
    SetControlValue ( edBatteryStatsChargeTimeWorst, FBike.Battery.StatsChargeTimeWorst, FSaveValues );
    SetControlValue ( edBatteryStatsChargeTimeMean, FBike.Battery.StatsChargeTimeMean, FSaveValues );
    SetControlValue ( edBatteryStatsLMDAdapt, FBike.Battery.StatsLMDAdapt, FSaveValues );
    SetControlValue ( edBatteryStatsBattCycles, FBike.Battery.StatsBattCycles, FSaveValues );
    SetControlValue ( edBatteryStatsBattFullCycles, FBike.Battery.StatsBattFullCycles, FSaveValues );
    SetControlValue ( edBatteryStatsPowerCycles, FBike.Battery.StatsPowerCycles, FSaveValues );
    SetControlValue ( edBatteryStatsVBattMax, FBike.Battery.StatsVoltageMax, FSaveValues );
    SetControlValue ( edBatteryStatsVBattMin, FBike.Battery.StatsVoltageMin, FSaveValues );
    SetControlValue ( edBatteryStatsTBattMax, FBike.Battery.StatsTemperatureMax, FSaveValues );
    SetControlValue ( edBatteryStatsTBattMin, FBike.Battery.StatsTemperatureMin, FSaveValues );
    SetControlValue ( edBatteryStatsCharge10, FBike.Battery.StatsCharge[1], FSaveValues );
    SetControlValue ( edBatteryStatsCharge20, FBike.Battery.StatsCharge[2], FSaveValues );
    SetControlValue ( edBatteryStatsCharge30, FBike.Battery.StatsCharge[3], FSaveValues );
    SetControlValue ( edBatteryStatsCharge40, FBike.Battery.StatsCharge[4], FSaveValues );
    SetControlValue ( edBatteryStatsCharge50, FBike.Battery.StatsCharge[5], FSaveValues );
    SetControlValue ( edBatteryStatsCharge60, FBike.Battery.StatsCharge[6], FSaveValues );
    SetControlValue ( edBatteryStatsCharge70, FBike.Battery.StatsCharge[7], FSaveValues );
    SetControlValue ( edBatteryStatsCharge80, FBike.Battery.StatsCharge[8], FSaveValues );
    SetControlValue ( edBatteryStatsCharge90, FBike.Battery.StatsCharge[9], FSaveValues );

    //RTC
    SetControlValue ( edBatteryRTCTime, FBike.Battery.RTCTime, FSaveValues );
      SetControlValue ( edBatteryRTCTime2, DateTimeToStr (FBike.Battery.RTCTimeDT), FSaveValues );
//      SetControlValue ( dtBatteryRTCTime, FBike.Battery.ManufacturingDate, FBike.Battery.RTCTimeDT, FSaveValues );
    SetControlValue ( edBatteryRTCLastChargeTimeStamp, FBike.Battery.RTCLastChargeTimestamp, FSaveValues );
      SetControlValue ( edBatteryRTCLastChargeTimeStamp2, DateTimeToStr(FBike.Battery.RTCLastChargeTimestampDT), FSaveValues );
//      SetControlValue ( dtBatteryRTCLastChargeTimeStamp, FBike.Battery.ManufacturingDate, FBike.Battery.RTCLastChargeTimestampDT, FSaveValues );
    SetControlValue ( edBatteryRTCCtrl, FBike.Battery.RTCControl, FSaveValues );

    // Gg
    SetControlValue ( edBatteryGgDMFSD, FBike.Battery.GasGageDMFSD, FSaveValues );
    SetControlValue ( edBatteryGgVoltageDivider, FBike.Battery.GasGageVoltageDivider, FSaveValues );

    // Config
    SetControlValue ( edBatteryConfigMaxPackTemperature, FBike.Battery.ConfigMaxPackTemperature, FSaveValues );
    SetControlValue ( edBatteryConfigMinPackTemperature, FBike.Battery.ConfigMinPackTemperature, FSaveValues );
    SetControlValue ( edBatteryConfigMaxGgTemperature, FBike.Battery.ConfigMaxGasgageTemperature, FSaveValues );
    SetControlValue ( edBatteryConfigMinGgTemperature, FBike.Battery.ConfigMinGasgageTemperature, FSaveValues );
    SetControlValue ( edBatteryConfigMaxCellDeltaVoltage, FBike.Battery.MaxCellDeltaVoltage, FSaveValues );
    SetControlValue ( edBatteryConfigTailLampIntensity, FBike.Battery.TaillampIntensity, FSaveValues );
    SetControlValue ( cbBatteryConfigShipMode, ord(FBike.Battery.Shipmode), FSaveValues );
    SetControlValue ( edBatteryConfigDeepSleepAfterLongInactivityPeriodDelay, FBike.Battery.DeepSleepInactivityDelay, FSaveValues );
    SetControlValue ( edBatteryConfigDeepSleepLowSocDelay, FBike.Battery.DeepSleepSOCLowDelay, FSaveValues );
    SetControlValue ( cbBatteryConfigCommMode, FBike.Battery.CommunicationMode, FSaveValues );
    SetControlValue ( cbBatteryConfigAutoSwitchComm, ord(FBike.Battery.ConfigAutoSwitchComm), FSaveValues );
    SetControlValue ( cbBatteryConfigWakeOnPowerVoltage, ord(FBike.Battery.WakeOnPowerVoltage), FSaveValues );
    SetControlValue ( cbBatteryConfigAllowBuckChargingOnBike, ord(FBike.Battery.AllowChargingOnBike), FSaveValues );
    SetControlValue ( edBatteryConfigDiag, FBike.Battery.ConfigDiag, FSaveValues );
    SetControlValue ( edBatteryConfigType, FBike.Battery.ConfigType, FSaveValues );
    SetControlValue ( edBatteryConfigVBattNominal, FBike.Battery.NominalVoltage, FSaveValues );
    SetControlValue ( cbBatteryConfigAccessoryMounted, ord(FBike.Battery.AccessoryMounted), FSaveValues );
    SetControlValue ( cbBatteryConfigAccessoryEnabled, ord(FBike.Battery.AccessoryEnabled), FSaveValues );
    SetControlValue ( edBatteryConfigAccessoryVoltage, FBike.Battery.AccessoryVoltage, FSaveValues );
    SetControlValue ( edBatteryConfigILMD, FBike.Battery.ConfigILMD, FSaveValues );
    SetControlValue ( edBatteryConfigMaxCharge, FBike.Battery.MaxPowervoltageRegenCurrent, FSaveValues );
    SetControlValue ( edBatteryConfigMaxDischarge, FBike.Battery.MaxPowervoltageCurrent, FSaveValues );
    SetControlValue ( edBatteryConfigCellCapacity, FBike.Battery.CellCapacity, FSaveValues );
    SetControlValue ( edBatteryConfigPackSerial, FBike.Battery.PackSerial, FSaveValues );
    SetControlValue ( edBatteryConfigPackParallel, FBike.Battery.PackParallel, FSaveValues );
    SetControlValue ( edBatteryConfigNAC, FBike.Battery.ConfigNAC, FSaveValues );
    SetControlValue ( cbBatteryConfigForceDone, ord(FBike.Battery.ConfigForceDone), FSaveValues );
    SetControlValue ( cbBatteryConfigVPower, ord(FBike.Battery.EnablePowerVoltage), FSaveValues );
    SetControlValue ( cbBatteryConfigVControl, ord(FBike.Battery.EnableControlVoltage), FSaveValues );
    SetControlValue ( cbBatteryConfigVBattInt, ord(FBike.Battery.EnableInternalBatteryVoltage), FSaveValues );
    SetControlValue ( cbBatteryConfigCapsenseMode, FBike.Battery.CapSenseSOCMode, FSaveValues );

    // Protect
    SetControlValue ( edBatteryProtectMode, FBike.Battery.ProtectMode, FSaveValues );
    SetControlValue ( edBatteryProtectCtrl, FBike.Battery.ProtectControl, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;

end;

procedure TfrmBionXMain.WriteBattery;
var
  Flags : integer;
begin
  FBike.KeepAlive := false;
  try
    // Timer
    if not CompareControlValue ( edBatteryTimerPower, FSaveValues ) then
      FBike.Battery.TimerPower := GetControlValue ( edBatteryTimerPower, FSaveValues );
    if not CompareControlValue ( edBatteryTimerAccessory, FSaveValues ) then
      FBike.Battery.TimerAccessory := GetControlValue ( edBatteryTimerAccessory, FSaveValues );
    if not CompareControlValue ( edBatteryTimerPreCharge, FSaveValues ) then
      FBike.Battery.TimerPreCharge := GetControlValue ( edBatteryTimerPreCharge, FSaveValues );
    if not CompareControlValue ( edBatteryTimerMasterShutdown, FSaveValues ) then
      FBike.Battery.TimerMasterShutdown := GetControlValue ( edBatteryTimerMasterShutdown, FSaveValues );

    // Status
    if not CompareControlValue ( edBatteryStatusTestFlags, FSaveValues ) then
      if TryStrToInt ( '$'+GetControlValue ( edBatteryStatusTestFlags, FSaveValues ), Flags ) then
        FBike.Battery.StatusTestFlags := Flags;
    if not CompareControlValue ( cbBatteryStatusPermanentFailureFlags, FSaveValues ) then
      FBike.Battery.StatusPermanentFailureFlags := GetControlValue ( cbBatteryStatusPermanentFailureFlags, FSaveValues )=1;
    if not CompareControlValue ( edBatteryStatusLeds, FSaveValues ) then
      FBike.Battery.StatusLeds := GetControlValue ( edBatteryStatusLeds, FSaveValues );
    if not CompareControlValue ( edBatteryStatusCapSense, FSaveValues ) then
      FBike.Battery.StatusCapSense := GetControlValue ( edBatteryStatusCapSense, FSaveValues );
    if not CompareControlValue ( edBatteryStatusCapSenseReference, FSaveValues ) then
      FBike.Battery.StatusCapSenseReference := GetControlValue ( edBatteryStatusCapSenseReference, FSaveValues );

    // CellMon
    if not CompareControlValue ( cbBatteryCellMonBalancerEnabled, FSaveValues ) then
      FBike.Battery.CellMonBalancerEnabled := GetControlValue ( cbBatteryCellMonBalancerEnabled, FSaveValues )=1;

    // Charger
    if not CompareControlValue ( edBatteryChargerCurrent, FSaveValues ) then
      FBike.Battery.ChargerCurrent := GetControlValue ( edBatteryChargerCurrent, FSaveValues );
    if not CompareControlValue ( edBatteryChargerFinalVoltage, FSaveValues ) then
      FBike.Battery.ChargerFinalVoltage := GetControlValue ( edBatteryChargerFinalVoltage, FSaveValues );
    if not CompareControlValue ( cbBatteryChargerMode, FSaveValues ) then
      FBike.Battery.ChargerMode := GetControlValue ( cbBatteryChargerMode, FSaveValues );

    // Calib
    if not CompareControlValue ( edBatteryCalibCapSense, FSaveValues ) then
      FBike.Battery.CalibCapsense := GetControlValue ( edBatteryCalibCapSense, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration01, FSaveValues ) then
      FBike.Battery.CalibCalibration[1] := GetControlValue ( edBatteryCalibCalibration01, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration02, FSaveValues ) then
      FBike.Battery.CalibCalibration[2] := GetControlValue ( edBatteryCalibCalibration02, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration03, FSaveValues ) then
      FBike.Battery.CalibCalibration[3] := GetControlValue ( edBatteryCalibCalibration03, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration04, FSaveValues ) then
      FBike.Battery.CalibCalibration[4] := GetControlValue ( edBatteryCalibCalibration04, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration05, FSaveValues ) then
      FBike.Battery.CalibCalibration[5] := GetControlValue ( edBatteryCalibCalibration05, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration06, FSaveValues ) then
      FBike.Battery.CalibCalibration[6] := GetControlValue ( edBatteryCalibCalibration06, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration07, FSaveValues ) then
      FBike.Battery.CalibCalibration[7] := GetControlValue ( edBatteryCalibCalibration07, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration08, FSaveValues ) then
      FBike.Battery.CalibCalibration[8] := GetControlValue ( edBatteryCalibCalibration08, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration09, FSaveValues ) then
      FBike.Battery.CalibCalibration[9] := GetControlValue ( edBatteryCalibCalibration09, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration10, FSaveValues ) then
      FBike.Battery.CalibCalibration[10] := GetControlValue ( edBatteryCalibCalibration10, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration11, FSaveValues ) then
      FBike.Battery.CalibCalibration[11] := GetControlValue ( edBatteryCalibCalibration11, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration12, FSaveValues ) then
      FBike.Battery.CalibCalibration[12] := GetControlValue ( edBatteryCalibCalibration12, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration13, FSaveValues ) then
      FBike.Battery.CalibCalibration[13] := GetControlValue ( edBatteryCalibCalibration13, FSaveValues );
    if not CompareControlValue ( edBatteryCalibCalibration3V3, FSaveValues ) then
      FBike.Battery.CalibCalibration3V3 := GetControlValue ( edBatteryCalibCalibration3V3, FSaveValues );


    // Stats
    if not CompareControlValue ( edBatteryStatsv5VShorts, FSaveValues ) then
      FBike.Battery.StatsV5VShorts := GetControlValue ( edBatteryStatsv5VShorts, FSaveValues );
    if not CompareControlValue ( edBatteryStatsVControlShorts, FSaveValues ) then
      FBike.Battery.StatsVControlShorts := GetControlValue ( edBatteryStatsVControlShorts, FSaveValues );
    if not CompareControlValue ( edBatteryStatsLowBattBuzzCount, FSaveValues ) then
      FBike.Battery.StatsLowVoltageBuzzerCount := GetControlValue ( edBatteryStatsLowBattBuzzCount, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCellVoltageCollapseCount, FSaveValues ) then
      FBike.Battery.StatsCellVoltageCollapseCount := GetControlValue ( edBatteryStatsCellVoltageCollapseCount, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCellPartialShortCount, FSaveValues ) then
      FBike.Battery.StatsCellPartialShortCount := GetControlValue ( edBatteryStatsCellPartialShortCount, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCellDeadShortCount, FSaveValues ) then
      FBike.Battery.StatsCellDeadShortCount := GetControlValue ( edBatteryStatsCellDeadShortCount, FSaveValues );
    if not CompareControlValue ( edBatteryStatsDeepSleepAfterLongInactivityPeriodCount, FSaveValues ) then
      FBike.Battery.StatsDeepSleepInactivityCount := GetControlValue ( edBatteryStatsDeepSleepAfterLongInactivityPeriodCount, FSaveValues );
    if not CompareControlValue ( edBatteryStatsDeepSleepAfterLowSocCount, FSaveValues ) then
      FBike.Battery.StatsDeepSleepSOCLowCount := GetControlValue ( edBatteryStatsDeepSleepAfterLowSocCount, FSaveValues );
    if not CompareControlValue ( edBatteryStatsDeepSleepExtremeLowBatteryVoltageCount, FSaveValues ) then
      FBike.Battery.StatsDeepSleepLowVoltageCount := GetControlValue ( edBatteryStatsDeepSleepExtremeLowBatteryVoltageCount, FSaveValues );
    if not CompareControlValue ( edBatteryStatsDischargeEnergy, FSaveValues ) then
      FBike.Battery.TotalDischargedEnergy := GetControlValue ( edBatteryStatsDischargeEnergy, FSaveValues );
    if not CompareControlValue ( edBatteryStatsChargeEnergy, FSaveValues ) then
      FBike.Battery.TotalChargedEnergy := GetControlValue ( edBatteryStatsChargeEnergy, FSaveValues );
    if not CompareControlValue ( edBatteryStatsReset, FSaveValues ) then
      FBike.Battery.StatsReset := GetControlValue ( edBatteryStatsReset, FSaveValues );
    if not CompareControlValue ( edBatteryStatsGgjrCalib, FSaveValues ) then
      FBike.Battery.StatsGasGageJitterCalibration := GetControlValue ( edBatteryStatsGgjrCalib, FSaveValues );
    if not CompareControlValue ( edBatteryStatsResetWdt, FSaveValues ) then
      FBike.Battery.StatsResetWdt := GetControlValue ( edBatteryStatsResetWdt, FSaveValues );
    if not CompareControlValue ( edBatteryStatsRTCResync, FSaveValues ) then
      FBike.Battery.StatsRTCResync := GetControlValue ( edBatteryStatsRTCResync, FSaveValues );
    if not CompareControlValue ( edBatteryStatsChargeTimeWorst, FSaveValues ) then
      FBike.Battery.StatsChargeTimeWorst := GetControlValue ( edBatteryStatsChargeTimeWorst, FSaveValues );
    if not CompareControlValue ( edBatteryStatsChargeTimeMean, FSaveValues ) then
      FBike.Battery.StatsChargeTimeMean := GetControlValue ( edBatteryStatsChargeTimeMean, FSaveValues );
    if not CompareControlValue ( edBatteryStatsLMDAdapt, FSaveValues ) then
      FBike.Battery.StatsLMDAdapt := GetControlValue ( edBatteryStatsLMDAdapt, FSaveValues );
    if not CompareControlValue ( edBatteryStatsBattCycles, FSaveValues ) then
      FBike.Battery.StatsBattCycles := GetControlValue ( edBatteryStatsBattCycles, FSaveValues );
    if not CompareControlValue ( edBatteryStatsBattFullCycles, FSaveValues ) then
      FBike.Battery.StatsBattFullCycles := GetControlValue ( edBatteryStatsBattFullCycles, FSaveValues );
    if not CompareControlValue ( edBatteryStatsPowerCycles, FSaveValues ) then
      FBike.Battery.StatsPowerCycles := GetControlValue ( edBatteryStatsPowerCycles, FSaveValues );
    if not CompareControlValue ( edBatteryStatsVBattMax, FSaveValues ) then
      FBike.Battery.StatsVoltageMax := GetControlValue ( edBatteryStatsVBattMax, FSaveValues );
    if not CompareControlValue ( edBatteryStatsVBattMin, FSaveValues ) then
      FBike.Battery.StatsVoltageMin := GetControlValue ( edBatteryStatsVBattMin, FSaveValues );
    if not CompareControlValue ( edBatteryStatsTBattMax, FSaveValues ) then
      FBike.Battery.StatsTemperatureMax := GetControlValue ( edBatteryStatsTBattMax, FSaveValues );
    if not CompareControlValue ( edBatteryStatsTBattMin, FSaveValues ) then
      FBike.Battery.StatsTemperatureMin := GetControlValue ( edBatteryStatsTBattMin, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCharge10, FSaveValues ) then
      FBike.Battery.StatsCharge[1] := GetControlValue ( edBatteryStatsCharge10, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCharge20, FSaveValues ) then
      FBike.Battery.StatsCharge[2] := GetControlValue ( edBatteryStatsCharge20, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCharge30, FSaveValues ) then
      FBike.Battery.StatsCharge[3] := GetControlValue ( edBatteryStatsCharge30, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCharge40, FSaveValues ) then
      FBike.Battery.StatsCharge[4] := GetControlValue ( edBatteryStatsCharge40, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCharge50, FSaveValues ) then
      FBike.Battery.StatsCharge[5] := GetControlValue ( edBatteryStatsCharge50, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCharge60, FSaveValues ) then
      FBike.Battery.StatsCharge[6] := GetControlValue ( edBatteryStatsCharge60, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCharge70, FSaveValues ) then
      FBike.Battery.StatsCharge[7] := GetControlValue ( edBatteryStatsCharge70, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCharge80, FSaveValues ) then
      FBike.Battery.StatsCharge[8] := GetControlValue ( edBatteryStatsCharge80, FSaveValues );
    if not CompareControlValue ( edBatteryStatsCharge90, FSaveValues ) then
      FBike.Battery.StatsCharge[9] := GetControlValue ( edBatteryStatsCharge90, FSaveValues );

    //RTC
    if not CompareControlValue ( edBatteryRTCTime, FSaveValues ) then
      FBike.Battery.RTCTime := GetControlValue ( edBatteryRTCTime, FSaveValues );
//    if not CompareControlValue ( dtBatteryRTCTime, FSaveValues ) then
//      FBike.Battery.RTCTimeDT := GetControlValue ( dtBatteryRTCTime, FSaveValues );
    if not CompareControlValue ( edBatteryRTCTime2, FSaveValues ) then
      FBike.Battery.RTCTimeDT := StrToDateTime ( GetControlValue ( edBatteryRTCTime2, FSaveValues ) );

    if not CompareControlValue ( edBatteryRTCLastChargeTimeStamp, FSaveValues ) then
      FBike.Battery.RTCLastChargeTimestamp := GetControlValue ( edBatteryRTCLastChargeTimeStamp, FSaveValues );
//    if not CompareControlValue ( dtBatteryRTCLastChargeTimeStamp, FSaveValues ) then
//      FBike.Battery.RTCLastChargeTimestampDT := GetControlValue ( dtBatteryRTCLastChargeTimeStamp, FSaveValues );
    if not CompareControlValue ( edBatteryRTCLastChargeTimeStamp2, FSaveValues ) then
      FBike.Battery.RTCLastChargeTimestampDT := StrToDateTime ( GetControlValue ( edBatteryRTCLastChargeTimeStamp2, FSaveValues ) );
    if not CompareControlValue ( edBatteryRTCCtrl, FSaveValues ) then
      FBike.Battery.RTCControl := GetControlValue ( edBatteryRTCCtrl, FSaveValues );

    // Gg
    if not CompareControlValue ( edBatteryGgDMFSD, FSaveValues ) then
      FBike.Battery.GasGageDMFSD := GetControlValue ( edBatteryGgDMFSD, FSaveValues );
    if not CompareControlValue ( edBatteryGgVoltageDivider, FSaveValues ) then
      FBike.Battery.GasGageVoltageDivider := GetControlValue ( edBatteryGgVoltageDivider, FSaveValues );


    // Config
    if not CompareControlValue ( edBatteryConfigMaxPackTemperature, FSaveValues ) then
      FBike.Battery.ConfigMaxPackTemperature := GetControlValue ( edBatteryConfigMaxPackTemperature, FSaveValues );
    if not CompareControlValue ( edBatteryConfigMinPackTemperature, FSaveValues ) then
      FBike.Battery.ConfigMinPackTemperature := GetControlValue ( edBatteryConfigMinPackTemperature, FSaveValues );
    if not CompareControlValue ( edBatteryConfigMaxGgTemperature, FSaveValues ) then
      FBike.Battery.ConfigMaxGasgageTemperature := GetControlValue ( edBatteryConfigMaxGgTemperature, FSaveValues );
    if not CompareControlValue ( edBatteryConfigMinGgTemperature, FSaveValues ) then
      FBike.Battery.ConfigMinGasgageTemperature := GetControlValue ( edBatteryConfigMinGgTemperature, FSaveValues );
    if not CompareControlValue ( edBatteryConfigMaxCellDeltaVoltage, FSaveValues ) then
      FBike.Battery.MaxCellDeltaVoltage := GetControlValue ( edBatteryConfigMaxCellDeltaVoltage, FSaveValues );
    if not CompareControlValue ( edBatteryConfigTailLampIntensity, FSaveValues ) then
      FBike.Battery.TaillampIntensity := GetControlValue ( edBatteryConfigTailLampIntensity, FSaveValues );
    if not CompareControlValue ( cbBatteryConfigShipMode, FSaveValues ) then
      FBike.Battery.Shipmode := GetControlValue ( cbBatteryConfigShipMode, FSaveValues )=1;
    if not CompareControlValue ( edBatteryConfigDeepSleepAfterLongInactivityPeriodDelay, FSaveValues ) then
      FBike.Battery.DeepSleepInactivityDelay := GetControlValue ( edBatteryConfigDeepSleepAfterLongInactivityPeriodDelay, FSaveValues );
    if not CompareControlValue ( edBatteryConfigDeepSleepLowSocDelay, FSaveValues ) then
      FBike.Battery.DeepSleepSOCLowDelay := GetControlValue ( edBatteryConfigDeepSleepLowSocDelay, FSaveValues );
    if not CompareControlValue ( cbBatteryConfigCommMode, FSaveValues ) then
      FBike.Battery.CommunicationMode := GetControlValue ( cbBatteryConfigCommMode, FSaveValues );
    if not CompareControlValue ( cbBatteryConfigAutoSwitchComm, FSaveValues ) then
      FBike.Battery.ConfigAutoSwitchComm := GetControlValue ( cbBatteryConfigAutoSwitchComm, FSaveValues )=1;
    if not CompareControlValue ( cbBatteryConfigWakeOnPowerVoltage, FSaveValues ) then
      FBike.Battery.WakeOnPowerVoltage := GetControlValue ( cbBatteryConfigWakeOnPowerVoltage, FSaveValues )=1;
    if not CompareControlValue ( cbBatteryConfigAllowBuckChargingOnBike, FSaveValues ) then
      FBike.Battery.AllowChargingOnBike := GetControlValue ( cbBatteryConfigAllowBuckChargingOnBike, FSaveValues )=1;
    if not CompareControlValue ( edBatteryConfigDiag, FSaveValues ) then
      FBike.Battery.ConfigDiag := GetControlValue ( edBatteryConfigDiag, FSaveValues );
    if not CompareControlValue ( edBatteryConfigType, FSaveValues ) then
      FBike.Battery.ConfigType := GetControlValue ( edBatteryConfigType, FSaveValues );
    if not CompareControlValue ( edBatteryConfigVBattNominal, FSaveValues ) then
      FBike.Battery.NominalVoltage := GetControlValue ( edBatteryConfigVBattNominal, FSaveValues );
    if not CompareControlValue ( cbBatteryConfigAccessoryMounted, FSaveValues ) then
      FBike.Battery.AccessoryMounted := GetControlValue ( cbBatteryConfigAccessoryMounted, FSaveValues )=1;
    if not CompareControlValue ( cbBatteryConfigAccessoryEnabled, FSaveValues ) then
      FBike.Battery.AccessoryEnabled := GetControlValue ( cbBatteryConfigAccessoryEnabled, FSaveValues )=1;
    if not CompareControlValue ( edBatteryConfigAccessoryVoltage, FSaveValues ) then
      FBike.Battery.AccessoryVoltage := GetControlValue ( edBatteryConfigAccessoryVoltage, FSaveValues );
    if not CompareControlValue ( edBatteryConfigILMD, FSaveValues ) then
      FBike.Battery.ConfigILMD := GetControlValue ( edBatteryConfigILMD, FSaveValues );

    // when one of these values are changed, they must be written BOTH
    // within an unlock session
    if not CompareControlValue ( edBatteryConfigMaxCharge, FSaveValues ) or
       not CompareControlValue ( edBatteryConfigMaxDischarge, FSaveValues ) then
    begin
      FBike.Battery.UnlockProtection;
      try
        FBike.Battery.MaxPowervoltageRegenCurrent := GetControlValue ( edBatteryConfigMaxCharge, FSaveValues );
        FBike.Battery.MaxPowervoltageCurrent := GetControlValue ( edBatteryConfigMaxDischarge, FSaveValues );
      finally
        FBike.Battery.LockProtection;
      end;
    end;

    if not CompareControlValue ( edBatteryConfigCellCapacity, FSaveValues ) then
      FBike.Battery.CellCapacity := GetControlValue ( edBatteryConfigCellCapacity, FSaveValues );
    if not CompareControlValue ( edBatteryConfigPackSerial, FSaveValues ) then
      FBike.Battery.PackSerial := GetControlValue ( edBatteryConfigPackSerial, FSaveValues );
    if not CompareControlValue ( edBatteryConfigPackParallel, FSaveValues ) then
      FBike.Battery.PackParallel := GetControlValue ( edBatteryConfigPackParallel, FSaveValues );
    if not CompareControlValue ( edBatteryConfigNAC, FSaveValues ) then
      FBike.Battery.ConfigNAC := GetControlValue ( edBatteryConfigNAC, FSaveValues );
    if not CompareControlValue ( cbBatteryConfigForceDone, FSaveValues ) then
      FBike.Battery.ConfigForceDone := GetControlValue ( cbBatteryConfigForceDone, FSaveValues )=1;
    if not CompareControlValue ( cbBatteryConfigVPower, FSaveValues ) then
      FBike.Battery.EnablePowerVoltage := GetControlValue ( cbBatteryConfigVPower, FSaveValues )=1;
    if not CompareControlValue ( cbBatteryConfigVControl, FSaveValues ) then
      FBike.Battery.EnableControlVoltage := GetControlValue ( cbBatteryConfigVControl, FSaveValues )=1;
    if not CompareControlValue ( cbBatteryConfigVBattInt, FSaveValues ) then
      FBike.Battery.EnableInternalBatteryVoltage := GetControlValue ( cbBatteryConfigVBattInt, FSaveValues )=1;
    if not CompareControlValue ( cbBatteryConfigCapsenseMode, FSaveValues ) then
      FBike.Battery.CapSenseSOCMode := GetControlValue ( cbBatteryConfigCapsenseMode, FSaveValues );

    // Protect
    if not CompareControlValue ( edBatteryProtectMode, FSaveValues ) then
      FBike.Battery.ProtectMode := GetControlValue ( edBatteryProtectMode, FSaveValues );
    if not CompareControlValue ( edBatteryProtectCtrl, FSaveValues ) then
      FBike.Battery.ProtectControl := GetControlValue ( edBatteryProtectCtrl, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

(******************************************************************************)

procedure TfrmBionXMain.ReadMotor;
begin
  FBike.KeepAlive := false;
  try
    // Geometry
    SetControlValue ( edMotorGeometryCirc, FBike.Motor.GeometryCirc, FSaveValues );

    // Assist
    SetControlValue ( edMotorAssistLevel, FBike.Motor.AssistLevel, FSaveValues );
    SetControlValue ( edMotorAssistLevelOffSlope, FBike.Motor.AssistLevelOffSlope, FSaveValues );
    SetControlValue ( cbMotorAssistDirection, FBike.Motor.AssistDirection, FSaveValues );
    SetControlValue ( edMotorAssistMaxSpeed, FBike.Motor.AssistMaxSpeed, FSaveValues );
    SetControlValue ( edMotorAssistMaxSpeedDerateDelta, FBike.Motor.AssistMaxSpeedDerateDelta, FSaveValues );
    SetControlValue ( cbMotorAssistVQDynamicFlag, ord(FBike.Motor.AssistVQDynamicFlag), FSaveValues );
    SetControlValue ( edMotorAssitStatorPn, FBike.Motor.StatorPartNumber, FSaveValues );
    SetControlValue ( edMotorAssistRegenInflex, FBike.Motor.AssistRegenInflex, FSaveValues );
    SetControlValue ( edMotorAssistWalkSpeedDecreaseStart, FBike.Motor.WalkSpeedDecreaseStartSpeed, FSaveValues );
    SetControlValue ( edMotorAssistWalkSpeedDecreaseEnd, FBike.Motor.WalkSpeedDecreaseEndSpeed, FSaveValues );
    SetControlValue ( edMotorAssistWalkLevel, FBike.Motor.WalkLevel, FSaveValues );
    SetControlValue ( edMotorAssistWalkMaxLevel, FBike.Motor.WalkLevelMax, FSaveValues );
    SetControlValue ( cbMotorAssistLowSpeedRampFlag, ord(FBike.Motor.LowSpeedRamp), FSaveValues );

    // Torque
    SetControlValue ( edMotorTorqueGaugePolarity, FBike.Motor.TorqueGaugePolarity, FSaveValues );
    SetControlValue ( edMotorTorqueGaugeType, FBike.Motor.TorqueGaugeType, FSaveValues );
    SetControlValue ( edMotorTorqueGaugeNoise, FBike.Motor.TorqueGaugeNoise, FSaveValues );
    SetControlValue ( edMotorTorqueGaugeDelay, FBike.Motor.TorqueGaugeDelay, FSaveValues );
    SetControlValue ( edMotorTorqueGaugeSpeed, FBike.Motor.TorqueGaugeSpeed, FSaveValues );
    SetControlValue ( edMotorTorqueGaugeReference, FBike.Motor.TorqueGaugeReference, FSaveValues );
    SetControlValue ( edMotorTorqueGaugeGain, FBike.Motor.TorqueGaugeGain, FSaveValues );
    SetControlValue ( edMotorTorqueGaugeMaxVoltage, FBike.Motor.TorqueGaugeMaxVoltage, FSaveValues );
    SetControlValue ( edMotorTorqueGaugeMaxVoltageDelay, FBike.Motor.TorqueGaugeMaxVoltageDelay, FSaveValues );

    // Preference
    SetControlValue ( edMotorPreferenceRegion, FBike.Motor.PreferenceRegion, FSaveValues );

    // Config
    SetControlValue ( edMotorConfigCommMode, FBike.Motor.ConfigCommMode, FSaveValues );
    SetControlValue ( cbMotorConfigEnablePwm, ord(FBike.Motor.ConfigEnablePWMLimit), FSaveValues );

    // Status
    SetControlValue ( cbMotorStatusMain, FBike.Motor.StatusMain, FSaveValues );

    // Stats
    SetControlValue ( edMotorStatsMaxVPower, FBike.Motor.StatsMaxVPower, FSaveValues );
    SetControlValue ( edMotorStatsMaxVTemp, FBike.Motor.StatsMaxVTemp, FSaveValues );
    SetControlValue ( edMotorStatsOdo, FBike.Motor.StatsOdo, FSaveValues );
    SetControlValue ( edMotorStatsChronoHr, FBike.Motor.StatsChronoHours, FSaveValues );
    SetControlValue ( edMotorStatsChronoSec, FBike.Motor.StatsChronoSeconds, FSaveValues );
    SetControlValue ( edMotorStatsHallDchs, FBike.Motor.StatsHallDCHS, FSaveValues );
    SetControlValue ( edMotorStatsHallTrans, FBike.Motor.StatsHallTrans, FSaveValues );
    SetControlValue ( edMotorStatsHallRing, FBike.Motor.StatsHallRing, FSaveValues );
    SetControlValue ( edMotorStatsHallLost, FBike.Motor.StatsHallLost, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

procedure TfrmBionXMain.WriteMotor;
begin
  FBike.KeepAlive := false;
  try
    // Geometry
    if not CompareControlValue ( edMotorGeometryCirc, FSaveValues ) then
      FBike.Motor.GeometryCirc := GetControlValue ( edMotorGeometryCirc, FSaveValues );

    // Assist
    if not CompareControlValue ( edMotorAssistLevel, FSaveValues ) then
      FBike.Motor.AssistLevel := GetControlValue ( edMotorAssistLevel, FSaveValues );
    if not CompareControlValue ( edMotorAssistLevelOffSlope, FSaveValues ) then
      FBike.Motor.AssistLevelOffSlope := GetControlValue ( edMotorAssistLevelOffSlope, FSaveValues );
    if not CompareControlValue ( cbMotorAssistDirection, FSaveValues ) then
      FBike.Motor.AssistDirection := GetControlValue ( cbMotorAssistDirection, FSaveValues );
    if not CompareControlValue ( edMotorAssistMaxSpeed, FSaveValues ) then
      FBike.Motor.AssistMaxSpeed := GetControlValue ( edMotorAssistMaxSpeed, FSaveValues );
    if not CompareControlValue ( edMotorAssistMaxSpeedDerateDelta, FSaveValues ) then
      FBike.Motor.AssistMaxSpeedDerateDelta := GetControlValue ( edMotorAssistMaxSpeedDerateDelta, FSaveValues );
    if not CompareControlValue ( cbMotorAssistVQDynamicFlag, FSaveValues ) then
      FBike.Motor.AssistVQDynamicFlag := GetControlValue ( cbMotorAssistVQDynamicFlag, FSaveValues )=1;
    if not CompareControlValue ( edMotorAssitStatorPn, FSaveValues ) then
      FBike.Motor.StatorPartNumber := GetControlValue ( edMotorAssitStatorPn, FSaveValues );
    if not CompareControlValue ( edMotorAssistRegenInflex, FSaveValues ) then
      FBike.Motor.AssistRegenInflex := GetControlValue ( edMotorAssistRegenInflex, FSaveValues );
    if not CompareControlValue ( edMotorAssistWalkSpeedDecreaseStart, FSaveValues ) then
      FBike.Motor.WalkSpeedDecreaseStartSpeed := GetControlValue ( edMotorAssistWalkSpeedDecreaseStart, FSaveValues );
    if not CompareControlValue ( edMotorAssistWalkSpeedDecreaseEnd, FSaveValues ) then
      FBike.Motor.WalkSpeedDecreaseEndSpeed := GetControlValue ( edMotorAssistWalkSpeedDecreaseEnd, FSaveValues );
    if not CompareControlValue ( edMotorAssistWalkLevel, FSaveValues ) then
      FBike.Motor.WalkLevel := GetControlValue ( edMotorAssistWalkLevel, FSaveValues );
    if not CompareControlValue ( edMotorAssistWalkMaxLevel, FSaveValues ) then
      FBike.Motor.WalkLevelMax := GetControlValue ( edMotorAssistWalkMaxLevel, FSaveValues );
    if not CompareControlValue ( cbMotorAssistLowSpeedRampFlag, FSaveValues ) then
      FBike.Motor.LowSpeedRamp := GetControlValue ( cbMotorAssistLowSpeedRampFlag, FSaveValues )=1;

    // Torque
    if not CompareControlValue ( edMotorTorqueGaugePolarity, FSaveValues ) then
      FBike.Motor.TorqueGaugePolarity := GetControlValue ( edMotorTorqueGaugePolarity, FSaveValues );
    if not CompareControlValue ( edMotorTorqueGaugeType, FSaveValues ) then
      FBike.Motor.TorqueGaugeType := GetControlValue ( edMotorTorqueGaugeType, FSaveValues );
    if not CompareControlValue ( edMotorTorqueGaugeNoise, FSaveValues ) then
      FBike.Motor.TorqueGaugeNoise := GetControlValue ( edMotorTorqueGaugeNoise, FSaveValues );
    if not CompareControlValue ( edMotorTorqueGaugeDelay, FSaveValues ) then
      FBike.Motor.TorqueGaugeDelay := GetControlValue ( edMotorTorqueGaugeDelay, FSaveValues );
    if not CompareControlValue ( edMotorTorqueGaugeSpeed, FSaveValues ) then
      FBike.Motor.TorqueGaugeSpeed := GetControlValue ( edMotorTorqueGaugeSpeed, FSaveValues );
    if not CompareControlValue ( edMotorTorqueGaugeReference, FSaveValues ) then
      FBike.Motor.TorqueGaugeReference := GetControlValue ( edMotorTorqueGaugeReference, FSaveValues );
    if not CompareControlValue ( edMotorTorqueGaugeGain, FSaveValues ) then
      FBike.Motor.TorqueGaugeGain := GetControlValue ( edMotorTorqueGaugeGain, FSaveValues );
    if not CompareControlValue ( edMotorTorqueGaugeMaxVoltage, FSaveValues ) then
      FBike.Motor.TorqueGaugeMaxVoltage := GetControlValue ( edMotorTorqueGaugeMaxVoltage, FSaveValues );
    if not CompareControlValue ( edMotorTorqueGaugeMaxVoltageDelay, FSaveValues ) then
      FBike.Motor.TorqueGaugeMaxVoltageDelay := GetControlValue ( edMotorTorqueGaugeMaxVoltageDelay, FSaveValues );

    // Preference
    if not CompareControlValue ( edMotorPreferenceRegion, FSaveValues ) then
      FBike.Motor.PreferenceRegion := GetControlValue ( edMotorPreferenceRegion, FSaveValues );

    // Config
    if not CompareControlValue ( edMotorConfigCommMode, FSaveValues ) then
      FBike.Motor.ConfigCommMode := GetControlValue ( edMotorConfigCommMode, FSaveValues );
    if not CompareControlValue ( cbMotorConfigEnablePwm, FSaveValues ) then
      FBike.Motor.ConfigEnablePWMLimit := GetControlValue ( cbMotorConfigEnablePwm, FSaveValues )=1;

    // Status
    if not CompareControlValue ( cbMotorStatusMain, FSaveValues ) then
      FBike.Motor.StatusMain := GetControlValue ( cbMotorStatusMain, FSaveValues );

    // Stats
    if not CompareControlValue ( edMotorStatsMaxVPower, FSaveValues ) then
      FBike.Motor.StatsMaxVPower := GetControlValue ( edMotorStatsMaxVPower, FSaveValues );
    if not CompareControlValue ( edMotorStatsMaxVTemp, FSaveValues ) then
      FBike.Motor.StatsMaxVTemp := GetControlValue ( edMotorStatsMaxVTemp, FSaveValues );
    if not CompareControlValue ( edMotorStatsOdo, FSaveValues ) then
      FBike.Motor.StatsOdo := GetControlValue ( edMotorStatsOdo, FSaveValues );
    if not CompareControlValue ( edMotorStatsChronoHr, FSaveValues ) then
      FBike.Motor.StatsChronoHours := GetControlValue ( edMotorStatsChronoHr, FSaveValues );
    if not CompareControlValue ( edMotorStatsChronoSec, FSaveValues ) then
      FBike.Motor.StatsChronoSeconds := GetControlValue ( edMotorStatsChronoSec, FSaveValues );
    if not CompareControlValue ( edMotorStatsHallDchs, FSaveValues ) then
      FBike.Motor.StatsHallDCHS := GetControlValue ( edMotorStatsHallDchs, FSaveValues );
    if not CompareControlValue ( edMotorStatsHallTrans, FSaveValues ) then
      FBike.Motor.StatsHallTrans := GetControlValue ( edMotorStatsHallTrans, FSaveValues );
    if not CompareControlValue ( edMotorStatsHallRing, FSaveValues ) then
      FBike.Motor.StatsHallRing := GetControlValue ( edMotorStatsHallRing, FSaveValues );
    if not CompareControlValue ( edMotorStatsHallLost, FSaveValues ) then
      FBike.Motor.StatsHallLost := GetControlValue ( edMotorStatsHallLost, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

(******************************************************************************)

procedure TfrmBionXMain.ReadSensor;
begin
  FBike.KeepAlive := false;
  try
    // Config
    SetControlValue ( cbSensorConfigMode, FBike.Sensor.ConfigMode, FSaveValues );
    SetControlValue ( edSensorConfigGaugeGain, FBike.Sensor.ConfigGaugeGain, FSaveValues );
    SetControlValue ( edSensorConfigRampUpSteps, FBike.Sensor.ConfigRampUpSteps, FSaveValues );
    SetControlValue ( edSensorConfigDecayDelay, FBike.Sensor.ConfigDecayDelay, FSaveValues );
    SetControlValue ( edSensorConfigDecaySteps, FBike.Sensor.ConfigDecaySteps, FSaveValues );
    SetControlValue ( edSensorConfigSpeedThreshold, FBike.Sensor.ConfigSpeedThreshold, FSaveValues );
    SetControlValue ( cbSensorConfigRampActiveOverThreshold, ord(FBike.Sensor.ConfigRampActiveOverThreshold), FSaveValues );
    SetControlValue ( edSensorConfigInputOffset, FBike.Sensor.ConfigInputOffset, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

procedure TfrmBionXMain.WriteSensor;
begin
  FBike.KeepAlive := false;
  try
    // Config
    if not CompareControlValue ( cbSensorConfigMode, FSaveValues ) then
      FBike.Sensor.ConfigMode := GetControlValue ( cbSensorConfigMode, FSaveValues );
    if not CompareControlValue ( edSensorConfigGaugeGain, FSaveValues ) then
      FBike.Sensor.ConfigGaugeGain := GetControlValue ( edSensorConfigGaugeGain, FSaveValues );
    if not CompareControlValue ( edSensorConfigRampUpSteps, FSaveValues ) then
      FBike.Sensor.ConfigRampUpSteps := GetControlValue ( edSensorConfigRampUpSteps, FSaveValues );
    if not CompareControlValue ( edSensorConfigDecayDelay, FSaveValues ) then
      FBike.Sensor.ConfigDecayDelay := GetControlValue ( edSensorConfigDecayDelay, FSaveValues );
    if not CompareControlValue ( edSensorConfigDecaySteps, FSaveValues ) then
      FBike.Sensor.ConfigDecaySteps := GetControlValue ( edSensorConfigDecaySteps, FSaveValues );
    if not CompareControlValue ( edSensorConfigSpeedThreshold, FSaveValues ) then
      FBike.Sensor.ConfigSpeedThreshold := GetControlValue ( edSensorConfigSpeedThreshold, FSaveValues );
    if not CompareControlValue ( cbSensorConfigRampActiveOverThreshold, FSaveValues ) then
      FBike.Sensor.ConfigRampActiveOverThreshold := GetControlValue ( cbSensorConfigRampActiveOverThreshold, FSaveValues )=1;
    if not CompareControlValue ( edSensorConfigInputOffset, FSaveValues ) then
      FBike.Sensor.ConfigInputOffset := GetControlValue ( edSensorConfigInputOffset, FSaveValues );

  finally
    FBike.KeepAlive := true;
  end;
end;

(**** Main routines ***********************************************************)

procedure TfrmBionXMain.btnConnectClick(Sender: TObject);
var
  CANAdapter : TCANAdapter;

begin
  case cbAdapter.ItemIndex of
    0 : CANAdapter := TTinyCANAdapter.Create ( @LogMsg );
    1 : CANAdapter := TFileCANAdapter.Create;
  end;

  FBike := TBionXBike.Create;
  try
    try
      {$ifndef UNIX}
      Screen.Cursor := crHourGlass;
      {$endif}
      if FBike.Connect ( CANAdapter ) then
      begin
        EnableControls ( true );
        ReadSettings;
        ReadTuning;
//        EnableControls ( true );
      end;
    finally
      {$ifndef UNIX}
      Screen.Cursor := crDefault;
      {$endif}
    end;
  except
    on E:Exception do
    begin
      FreeAndNil ( FBike );
      raise;
    end;
  end;
end;

procedure TfrmBionXMain.btnDisconnectClick(Sender: TObject);
begin
  EnableControls ( false );
  if assigned ( FBike ) then
    FreeAndNil ( FBike );
end;

procedure TfrmBionXMain.btnShutdownClick ( Sender: TObject ) ;
begin
  if assigned ( FBike ) then
    FBike.Battery.Shutdown;
  btnDisconnectClick(nil);
end;

// at this time for debugging/analyzing purpose only
// copies the registers to file, which may be opened
// again by selecting adapter type "file". With this
// we can do debugging offline
// Start program with option -d to enable this feature
procedure TfrmBionXMain.btnSaveToFileClick(Sender: TObject);
var
  FCA : TFileCANAdapter;

  procedure ReadValues ( Component : TBionXComponent );
  var
    i   : integer;
    v   : byte;
  begin
    for i := 0 to 255 do
    begin
      try
        v := Component.ByteValue[i];
        FCA.WriteByte ( Component.CANId, i,  v );
        ShowValue ( Format ( 'Reg 0x%0.2x (%3d) = 0x%0.2x / %3d', [i, i, v, v] ));
      except
        on E:Exception do
          ShowValue ( Format ( 'Reg 0x%0.2x (%3d) no data', [ i, i ] ));
      end;
    end;
  end;

begin
  FCA := TFileCANAdapter.Create;
  try
    if FCA.Connect then
    begin
      try
        ShowValue ( 'Reading console' );
        ReadValues ( FBike.Console );
        ShowValue ( '' );
        ShowValue ( 'Reading motor' );
        ReadValues ( FBike.Motor );
        ShowValue ( '' );
        ShowValue ( 'Reading battery' );
        ReadValues ( FBike.Battery );
        ShowValue ( '' );
      finally
        FCA.Disconnect;
      end;
    end;
  finally
    FCA.Free;
  end;
end;

procedure TfrmBionXMain.btnAboutClick ( Sender: TObject ) ;
begin
  MessageDlg ( 'Info',
                 '(c) 2013 by Thorsten Schmidt www.ts-soft.de'#13'Send notes and bugreports to bikeinfo@ts-soft.de.'#13#13
               + 'Portions of this software are taken from BigXionFlasher (c) by Thomas König, www.bigxionflasher.org.'#13#13
               + 'The included TinyCAN library is copyrighted by MHS Elektronik (www.mhs-elektronik.de). The library'#13
               + 'is distributed together with this program with courtesy of MHS Elektronik.'#13#13
               + 'This program is published without any warranty. Improper settings may damage your system.'#13'Use at your own risk.'#13#13
               + 'Increasing the speed limits may be against your local law and limit the use of your bike to non public areas.'#13#13
               + 'Start program with option -x for more, but eXperimental setting features.'#13#13
               + 'Enjoy and have a safe ride.',
               mtInformation, [mbOK], 0, mbOK );
end;

(******************************************************************************)

procedure TfrmBionXMain.EditfieldChange(Sender: TObject);
var
  cmp : boolean;
begin
  if Sender is TSpinEdit then
    Cmp := CompareControlValue ( TSpinEdit(Sender), FSaveValues );
  if Sender is TFloatSpinEdit then
    Cmp := CompareControlValue ( TFloatSpinEdit(Sender), FSaveValues );
  if Sender is TCombobox then
    Cmp := CompareControlValue ( TCustomCombobox(Sender), FSaveValues );
  if Sender is TCustomEdit then
    Cmp := CompareControlValue ( TCustomEdit(Sender), FSaveValues );
(*
  if Sender is TZVDateTimePicker then
    Cmp := CompareControlValue ( TZVDateTimePicker(Sender), FSaveValues );
*)
  if not Cmp then
    TWinControl(Sender).Font.Color := clRed
  else
    TWinControl(Sender).Font.Color := clDefault;
end;

(**** tsInfo routines *********************************************************)

procedure TfrmBionXMain.btnInfoConsoleClick(Sender: TObject);
begin
  ShowConsoleSettings;
end;

procedure TfrmBionXMain.btnInfoBatteryClick(Sender: TObject);
begin
  ShowBatterySettings;
end;

procedure TfrmBionXMain.btnInfoMotorClick(Sender: TObject);
begin
  ShowMotorSettings;
end;

procedure TfrmBionXMain.btnInfoSensorClick ( Sender: TObject ) ;
begin
  ShowSensorSettings;
end;

procedure TfrmBionXMain.btnInfoSaveClick ( Sender : TObject ) ;
begin
  if sdInfo.Execute then
    mmInfo.Lines.SaveToFile( UTF8ToSys( sdInfo.Filename ) );
end;

procedure TfrmBionXMain.btnInfoClearClick(Sender: TObject);
begin
  mmInfo.Clear;
end;

(**** tsSettings routines *****************************************************)

procedure TfrmBionXMain.btnReadSettingsClick(Sender: TObject);
begin
  ReadSettings;
end;

procedure TfrmBionXMain.btnApplySettingsClick ( Sender: TObject ) ;
begin
  WriteSettings;
end;

(**** tsTuning routines ********************************************************)

procedure TfrmBionXMain.btnReadTuningClick ( Sender: TObject ) ;
begin
  ReadTuning;
end;

procedure TfrmBionXMain.btnApplyTuningClick ( Sender: TObject ) ;
begin
  WriteTuning;
end;

(**** tsConsole routines ******************************************************)

procedure TfrmBionXMain.CodesEditClick(Sender: TObject);
var
  Codes : longword;
  CodesRW : longword;
begin
  Codes := StrToIntDef ( '$'+edConsolePreferenceCodes.Text, 0 );
  CodesRW := StrToIntDef ( '$'+edConsolePreferenceCodesRW.Text, 0 );
  if EditCodes ( self, Codes, CodesRW ) then
  begin
    edConsolePreferenceCodes.Text := IntToHex ( Codes, 8 );
    edConsolePreferenceCodesRW.Text := IntToHex ( CodesRW, 8 );
  end;
end;

procedure TfrmBionXMain.btnReadConsoleClick ( Sender: TObject ) ;
begin
  try
    FBike.Console.CheckDeviceReady;
    ReadConsole;
    EnableConsoleControls ( true );
  except
    EnableConsoleControls ( false );
    raise;
  end;
end;

procedure TfrmBionXMain.btnApplyConsoleClick ( Sender: TObject ) ;
begin
  WriteConsole
end;

procedure TfrmBionXMain.EnableConsoleControls ( Enable : boolean );
begin
  EnableEditControls ( scbConsole, Enable );
  btnApplyConsole.Enabled := Enable;
end;

(**** tsBattery routines ******************************************************)

procedure TfrmBionXMain.btnReadBatteryClick ( Sender: TObject ) ;
begin
  try
    FBike.Battery.CheckDeviceReady;
    ReadBattery;
    EnableBatteryControls ( true );
  except
    EnableBatteryControls ( false );
    raise;
  end;
end;

procedure TfrmBionXMain.EnableBatteryControls ( Enable : boolean );
begin
  EnableEditControls ( scbBattery, Enable );
  btnApplyBattery.Enabled := Enable;
end;

procedure TfrmBionXMain.btnApplyBatteryClick ( Sender: TObject ) ;
begin
  WriteBattery;
end;

procedure TfrmBionXMain.btnBatteryUnlockClick ( Sender: TObject ) ;
begin
  FBike.Battery.unLockProtection;
end;

procedure TfrmBionXMain.btnBatteryLockClick ( Sender: TObject ) ;
begin
  FBike.Battery.LockProtection;
end;

(**** tsMotor routines ********************************************************)

procedure TfrmBionXMain.btnReadMotorClick ( Sender : TObject ) ;
begin
  try
    FBike.Motor.CheckDeviceReady;
    ReadMotor;
    EnableMotorControls ( true );
  except
    EnableMotorControls ( false );
    raise;
  end;
end;

procedure TfrmBionXMain.EnableMotorControls ( Enable : boolean );
begin
  EnableEditControls ( scbMotor, Enable );
  btnApplyMotor.Enabled := Enable;
end;


procedure TfrmBionXMain.btnApplyMotorClick ( Sender : TObject ) ;
begin
  WriteMotor;
end;

procedure TfrmBionXMain.btnMotorLockClick(Sender: TObject);
begin
  FBike.Motor.LockProtection;
end;

procedure TfrmBionXMain.btnMotorUnlockClick(Sender: TObject);
begin
  FBike.Motor.UnLockProtection;
end;


(**** tsSensor routines *******************************************************)

procedure TfrmBionXMain.btnReadSensorClick ( Sender: TObject ) ;
begin
  try
    FBike.Sensor.CheckDeviceReady;
    ReadSensor;
    EnableSensorControls ( false );
  except
    EnableSensorControls ( true );
    raise;
  end;
end;

procedure TfrmBionXMain.EnableSensorControls ( Enable : boolean );
begin
  EnableEditControls ( scbSensor, Enable );
  btnApplySensor.Enabled := Enable;
end;


procedure TfrmBionXMain.btnApplySensorClick ( Sender: TObject ) ;
begin
  WriteSensor;
end;

(**** tsTests routines ********************************************************)

procedure TfrmBionXMain.btnLogClearClick(Sender: TObject);
begin
  mmLog.Clear;
end;

// for debugging/analyzing purpose only
procedure TfrmBionXMain.Button1Click(Sender: TObject);
type
  TByteSet = set of byte;

const
  console_validRegisters  = [ 80..83, 100..120, 122..126, 128..142, 160..171, 173..199, 208..212, 215..220, 222..223 ];

  procedure ShowRegisters ( BionXComponent : TBionxComponent; Regs : TByteSet );
  var
    i : integer;
    v : byte;
    Last : integer;
  begin
    Last := 0;
    for i := 0 to 255 do
    begin
      if i in Regs then
      begin
        if i-Last > 1 then
          ShowValue ( '' );
        try
          v := BionXComponent.ByteValue[i];
          ShowValue ( Format ( 'Reg 0x%0.2x = 0x%0.2x / %3d', [i, v, v] ));
        except
          on E:Exception do
            ShowValue ( Format ( 'Reg 0x%0.2x error: %s', [ i, E.Message ] ));
        end;
        Last := i;
      end;
    end;
  end;

begin
//  ShowRegisters ( FBike.Console, console_validRegisters );
//  ShowRegisters ( FBike.Motor, [0..255] );
//  FBike.Console.NIP := 'abcd';
//  FBike.Battery.CellMonBalancerEnabled := false;
end;

procedure TfrmBionXMain.BitBtn1Click ( Sender: TObject ) ;
begin
  Edit1.Text := Format ( '%d', [ Screen.PixelsPerInch ] );
//  Edit1.Text := Format ( '%0.3f', [ FBike.Battery.ChargerFinalVoltage ] );
end;

function MsgToStr ( Msg : TCanMsg ) : string;
begin
//  Result := Format ( 'Id=%0.2x, Flags=%0.4x, Data=%0.2x %0.2x %0.2x %0.2x %0.2x %0.2x %0.2x %0.2x', [Msg.Id, Msg.Flags, Msg.Data.Bytes[0], Msg.Data.Bytes[1], Msg.Data.Bytes[2], Msg.Data.Bytes[3], Msg.Data.Bytes[4], Msg.Data.Bytes[5], Msg.Data.Bytes[6], Msg.Data.Bytes[7]] );
  Result := Format ( '%4s, Flags=%0.4x, Data=%0.2x %0.2x %0.2x %0.2x %0.2x %0.2x %0.2x %0.2x', [GetCANIdName(Msg.Id), Msg.Flags, Msg.Data.Bytes[0], Msg.Data.Bytes[1], Msg.Data.Bytes[2], Msg.Data.Bytes[3], Msg.Data.Bytes[4], Msg.Data.Bytes[5], Msg.Data.Bytes[6], Msg.Data.Bytes[7]] );
end;

function XltMsgToStr ( Msg : TCanMsg ) : string;
begin
//  Result :=
  Result := Format ( '%-0.4s, Flags=%0.4x, Data=%0.2x %0.2x %0.2x %0.2x %0.2x %0.2x %0.2x %0.2x', [GetCANIdName(Msg.Id), Msg.Flags, Msg.Data.Bytes[0], Msg.Data.Bytes[1], Msg.Data.Bytes[2], Msg.Data.Bytes[3], Msg.Data.Bytes[4], Msg.Data.Bytes[5], Msg.Data.Bytes[6], Msg.Data.Bytes[7]] );
end;

procedure TfrmBionXMain.HandleCANMessage ( Msg : PCANMsg );
begin
  LogMsg ( MsgToStr ( Msg^ ));
  LogMsg ( XltMsgToStr ( Msg^ ));
end;

var
  TCA : TTinyCANAdapter;

procedure TfrmBionXMain.Button2Click ( Sender: TObject ) ;
begin
  if not assigned ( TCA ) then
  begin
    TCA := TTinyCANAdapter.Create ( @LogMsg );
    if TCA.Connect then
    begin
      TCA.StartMessageCapturing ( @HandleCANMessage );
    end
    else
    begin
      TCA.Free;
      TCA := nil;
    end;
  end
  else
  begin
    TCA.StopMessageCapturing;
    TCA.Free;
    TCA := nil;
  end;
end;


end.

