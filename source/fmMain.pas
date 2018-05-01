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

uses
  Classes, SysUtils, FileUtil, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  Spin, Buttons, Menus,
  BionX, CANAdapter, TinyCANAdapter, FileCANAdapter, BionXProfiles;


type
  TSettingsStore = record
    WheelCircumference : word;
    ConsoleSwitches    : byte;
    MetricUnits        : byte;
    AutoShutdownDelay  : word;
    ShowRemainingDistanceAndTime : integer;

    BrakeSensor        : byte;
    BrakeRekuLevel     : byte;

    TorqueSensorGain              : double;
    TorqueSensorSpeed             : byte;
    TorqueSensorExtraGain         : double;
    TorqueSensorExtraGainMaxSpeed : double;

    BoostLevel                    : integer;

    AssistMaxSpeed   : double;
    AssistMinSpeed   : double;
    ThrottleMaxSpeed : double;

    InitialAssistLevel  : byte;
    AssistLevel1        : double;
    AssistLevel2        : double;
    AssistLevel3        : double;
    AssistLevel4        : double;
    AssistLevelMountain : double;

    Odometer : double;
  end;

{ TfrmBionXMain }
type
  TfrmBionXMain = class(TForm)
    btnAbout: TBitBtn;
    btnsaveTuningProfile: TBitBtn;
    btnApplyTuning: TBitBtn;
    btnReadTuning: TBitBtn;
    btnLoadTuningProfile: TBitBtn;
    cbShowRemainingDistanceAndTime: TComboBox;
    edAssistLevel1: TSpinEdit;
    edAssistLevel2: TSpinEdit;
    edAssistLevel3: TSpinEdit;
    edAssistLevel4: TSpinEdit;
    edAssistLevelMountain: TSpinEdit;
    edAssistMaxSpeed: TFloatSpinEdit;
    edAssistMinSpeed: TFloatSpinEdit;
    edInitialAssistLevel: TSpinEdit;
    edOdometer: TFloatSpinEdit;
    edThrottleMaxSpeed: TFloatSpinEdit;
    edTorqueSensorExtraGain: TFloatSpinEdit;
    edTorqueSensorExtraGainMaxSpeed: TFloatSpinEdit;
    edTorqueSensorGain: TFloatSpinEdit;
    edTorqueSensorSpeed: TSpinEdit;
    edBoostLevel: TSpinEdit;
    gbAssistanceLevel: TGroupBox;
    gbSpeedLimits: TGroupBox;
    gbTorqueSensor: TGroupBox;
    gbBoost: TGroupBox;
    Label1: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label2: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label3: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label38: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    mmLog: TMemo;
    odProfile: TOpenDialog;
    sdProfile: TSaveDialog;
    tsTuning: TTabSheet;
    tsTests: TTabSheet;
    pnlMain: TPanel;
    pcMain: TPageControl;
    tsInfo: TTabSheet;
    tsSettings: TTabSheet;
    StatusBar1: TStatusBar;
    btnReadSettings: TBitBtn;
    pnlInfo: TPanel;
    mmInfo: TMemo;
    gbBrakes: TGroupBox;
    Label13: TLabel;
    Label30: TLabel;
    cbBrakeSensor: TComboBox;
    edBrakeRekuLevel: TSpinEdit;
    Label21: TLabel;
    Label31: TLabel;
    gbMiscSettings: TGroupBox;
    Label12: TLabel;
    Label10: TLabel;
    Label9: TLabel;
    edWheelCircumference: TSpinEdit;
    cbMetricUnits: TComboBox;
    cbConsoleSwitches: TComboBox;
    Label20: TLabel;
    Label19: TLabel;
    Label18: TLabel;
    btnApplySettings: TBitBtn;
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
    btnInfoConsole : TBitBtn;
    btnInfoBattery : TBitBtn;
    btnInfoMotor : TBitBtn;
    pnlTest : TPanel;
    btnLogClear : TButton;
    Button1 : TButton;
    procedure btnAboutClick ( Sender: TObject ) ;
    procedure btnsaveTuningProfileClick ( Sender: TObject ) ;
    procedure btnApplySettingsClick ( Sender: TObject ) ;
    procedure btnApplyTuningClick ( Sender: TObject ) ;
    procedure btnReadTuningClick ( Sender: TObject ) ;
    procedure btnShutdownClick ( Sender: TObject ) ;
    procedure Button1Click(Sender: TObject);
    procedure btnLogClearClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnLoadTuningProfileClick ( Sender: TObject ) ;
    procedure FormCreate ( Sender: TObject ) ;
    procedure btnReadSettingsClick(Sender: TObject);
    procedure btnInfoConsoleClick(Sender: TObject);
    procedure btnInfoBatteryClick(Sender: TObject);
    procedure btnInfoClearClick(Sender: TObject);
    procedure btnInfoMotorClick(Sender: TObject);
    procedure btnSaveToFileClick(Sender: TObject);
    procedure btnInfoSaveClick ( Sender : TObject ) ;
  private
    { private declarations }
    FBike : TBionXBike;
    FSettings : TSettingsStore;

    procedure HandleException ( Sender : TObject; E : Exception);

    procedure EnableControls ( Connected : boolean );
    procedure LogMsg ( const s : string );

    procedure ShowValue ( const Value : string ); overload;
    procedure ShowValue ( const ValueName : string; const Fmt : string; Args : array of const ); overload;

    procedure ShowConsoleSettings;
    procedure ShowBatterySettings;
    procedure ShowMotorSettings;

    procedure ReadSettings;
    procedure RememberSettingValues;
    procedure WriteSettings;

    procedure ReadTuning;
    procedure RememberTuningValues;
    procedure WriteTuning;
    procedure LoadTuningProfile ( const Filename : string );
    procedure SaveTuningProfile ( const Filename : string );

  protected
  public
    { public declarations }
  end;

var
  frmBionXMain: TfrmBionXMain;

implementation
{$R *.lfm}

const
  Version = 50;

{ TfrmBionXMain }

procedure TfrmBionXMain.FormCreate ( Sender: TObject ) ;
begin
  Caption := Format ( Caption, [Version div 100, Version mod 100] );
  EnableControls ( false );

  pcMain.ActivePageIndex := 0;

  if not FindCmdLineSwitch ( 'd', true ) then
  begin
    pnlFileAdapter.Visible := false;
    tsTests.TabVisible := false;
  end;

  Application.OnException := @HandleException;
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
  btnInfoConsole.Enabled := Connected;
  btnInfoBattery.Enabled := Connected;
  btnInfoMotor.Enabled := Connected;
  tsSettings.Enabled := Connected;
  tsTuning.Enabled := Connected;
  btnSaveToFile.Enabled := Connected;
end;

procedure TfrmBionXMain.LogMsg ( const s : string );
begin
  mmLog.Lines.Add ( s );
end;

procedure TfrmBionXMain.ShowValue ( const Value : string );
begin
  mmInfo.Lines.Add ( Value );
end;

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


procedure TfrmBionXMain.ShowConsoleSettings;
var
  hwVersion : integer;
  swVersion : integer;
  BrakeSensorType : byte;
begin
  hwVersion := FBike.Console.HardwareVersion;
  if (hwVersion = 0) then
    ShowValue ( 'Console not responding' )
  else
  begin
    swVersion := FBike.Console.SoftwareVersion;
    // Partinfo
    ShowValue ( 'Console information:' );
    ShowValue ( 'Hardware version', '%0.2d', [ hwVersion ] );
    ShowValue ( 'Software version', '%0.2d', [ swVersion ] );
    ShowValue ( 'Part number', '%0.5d', [ FBike.Console.PartNumber ] );
    ShowValue ( 'Item number', '%0.5d', [ FBike.Console.ItemNumber ] );
    ShowValue ( 'Manufacturing date', '%s', [ DateToStr ( FBike.Console.ProductionDate ) ] );
    ShowValue ( '' );

    // Misc settings
    ShowValue ( 'Wheel circumference', '%d mm', [ FBike.Console.WheelCircumference] );
    ShowValue ( 'Flipped switches', '%s', [ BoolToStr ( FBike.Console.FlippedSwitches )] );
    ShowValue ( 'Metric units', '%s', [ BoolToStr ( FBike.Console.MetricUnits ) ] );
    ShowValue ( 'Show remaining distance and time', '%s', [ BoolToStr ( FBike.Console.ShowRemainigDistanceAndTime ) ] );
    ShowValue ( '' );

    // Assistlevels
    ShowValue ( 'Initial assist level', '%d', [ FBike.Console.InitialAssistLevel] );
    ShowValue ( 'Assist level 1', '%0.2f%%', [ FBike.Console.AssistLevel1] );
    ShowValue ( 'Assist level 2', '%0.2f%%', [ FBike.Console.AssistLevel2] );
    ShowValue ( 'Assist level 3', '%0.2f%%', [ FBike.Console.AssistLevel3] );
    ShowValue ( 'Assist level 4', '%0.2f%%', [ FBike.Console.AssistLevel4] );
    if (swVersion >= 59) then
      ShowValue ( 'Assist level mountain mode', '%0.2f%%', [ FBike.Console.MountainAssistLevel ] );
    ShowValue ( '' );

    // ASSIST speed limit
    ShowValue ( 'Assist max speed limited', '%s', [ BoolToStr ( FBike.Console.AssistMaxSpeedFlag )] );
    ShowValue ( 'Assist max speed', '%0.2f Km/h', [ FBike.Console.AssistMaxSpeed ]);

    // MIN speed limit
    ShowValue ( 'Assist min speed limited', '%s', [ BoolToStr ( FBike.Console.AssistMinSpeedFlag ) ] );
    ShowValue ( 'Assist min speed', '%0.2f Km/h', [ FBike.Console.AssistMinSpeed ] );

    // THROTTLE speed limit
    ShowValue ( 'Throttle max speed limited', '%s', [ BoolToStr ( FBike.Console.ThrottleMaxSpeedFlag ) ] );
    ShowValue ( 'Throttle max speed', '%0.2f Km/h', [ FBike.Console.ThrottleMaxSpeed ]);

    ShowValue ( '' );

    // Brake info
    ShowValue ( 'Brake sensor aktiv', '%s', [ BoolToStr ( FBike.Console.BrakeSensorFlag )] );
    BrakeSensorType := FBike.Console.BrakeSensorType;
    ShowValue ( 'Brake sensor type', '%d = %s', [ BrakeSensorType, BrakeSensorTypeToStr(BrakeSensorType) ] );
    ShowValue ( 'Brake reku level', '%2d', [ FBike.Console.BrakeRekuperationLevel ] );
    ShowValue ( '' );

    // Sensor gain
    ShowValue ( 'Torque sensor gain', '%0.2f', [ FBike.Console.TorqueSensorGain ] );
    ShowValue ( 'Torque sensor speed', '%d', [ FBike.Console.TorqueSensorSpeed ] );
    ShowValue ( 'Torque sensor extra gain', '%0.2f', [ FBike.Console.TorqueSensorExtraGain ] );
    ShowValue ( 'Torque sensor extra max speed', '%0.2f', [ FBike.Console.TorqueSensorExtraGainMaxSpeed ] );
    ShowValue ( 'Boost level', '%d', [ FBike.Console.BoostLevel ] );
    ShowValue ( '' );

    // Odo values
    ShowValue ( 'Odometer', '%0.1f Km', [ FBike.Console.Odometer] );
    ShowValue ( 'Distance', '%0.1f Km', [ FBike.Console.Distance] );
    ShowValue ( 'Avarage speed', '%0.1f Km/h', [ FBike.Console.AverageSpeed] );
    ShowValue ( 'Chrono', '%0s', [ TimeToStr(FBike.Console.Chrono)] );
  end;
  ShowValue ( '' );
end;

procedure TfrmBionXMain.ShowBatterySettings;
var
  hwVersion : integer;

  procedure printChargeStats;
  var
    i   : integer;
    Sum : integer;
    Cnt : integer;
  begin
    Sum := 0;
    for i := 1 to 10 do
    begin
      Cnt := FBike.Battery.ChargeTimes[i];
      inc ( Sum, Cnt );
      ShowValue ( Format ( '  Charge level @ %3d%%', [i*10]), '%d', [ Cnt ] );
    end;
    ShowValue ( '  Total # of charges', '%d', [ Sum ]);
    ShowValue ( '' );
  end;

  procedure printBatteryStats;
  var
    i            : integer;
    packSerial   : integer;
    packParallel : integer;
  begin
    ShowValue ( 'Balancer enabled', '%s', [ BoolToStr(FBike.Battery.BalancerEnabled)] );

    packSerial := FBike.Battery.PackSerial;
    packParallel := FBike.Battery.PackParallel;

    for i := 1 to packSerial do
      ShowValue ( Format ( 'Voltage cell #%2d', [i] ), '%.3fV', [ FBike.Battery.CellVoltage[i] ]);

    for i := 1 to packParallel do
      ShowValue ( Format ( 'Temperature pack #%d', [i] ), '%dC', [ FBike.Battery.PackTemperature [ i ] ]);
    ShowValue ( '' );
  end;

begin
  hwVersion := FBike.Battery.HardwareVersion;
  if (hwVersion = 0) then
    ShowValue ( 'Battery not responding' )
  else
  begin
    // Partinfo
    ShowValue ( 'Battery information:' );
    ShowValue ( 'Hardware version', '%0.2d', [hwVersion] );
    ShowValue ( 'Software version', '%0.2d', [ FBike.Battery.SoftwareVersion]);
    ShowValue ( 'Part number', '%0.5d', [ FBike.Battery.PartNumber ] );
    ShowValue ( 'Item number', '%0.5d', [ FBike.Battery.ItemNumber ] );
    ShowValue ( 'Manufacturing date', '%s', [ DateToStr ( FBike.Battery.ProductionDate ) ] );
    ShowValue ( '' );

    // Misc settings
    ShowValue ( 'Auto power down delay', '%ds', [ FBike.Battery.AutoShutdownDelay ] );
    ShowValue ( '' );

    // Charge info
    ShowValue ( 'Charge time worst', '%d', [ FBike.Battery.ChargeTimeWorst ] );
    ShowValue ( 'Charge time mean', '%d', [ FBike.Battery.ChargeTimeMean ] );

    ShowValue ( 'Charge cycles', '%d', [ FBike.Battery.ChargeCycles ] );
    ShowValue ( 'Full charge cycles', '%d', [ FBike.Battery.FullChargeCycles ] );
    ShowValue ( 'Power cycles', '%d', [ FBike.Battery.PowerCycles ] );

    printChargeStats;

    // Voltages
    ShowValue ( 'Voltage', '%0.2fV', [ FBike.Battery.Voltage ]);
    ShowValue ( 'Input voltage', '%0.2fV', [ FBike.Battery.InputVoltage ]);
    ShowValue ( 'Battery level', '%0.2f%%', [ FBike.Battery.ChargeLevel ] );

    ShowValue ( 'Maximum voltage', '%0.2f%%', [ FBike.Battery.VoltageMax ] );
    ShowValue ( 'Minimum voltage', '%0.2f%%', [ FBike.Battery.VoltageMin ] );
    ShowValue ( 'Mean voltage', '%0.2f%%', [ FBike.Battery.VoltageMean ] );


    ShowValue ( 'Battery temp max', '%dC', [ FBike.Battery.TemperatureMax ] );
    ShowValue ( 'Battery temp min', '%dC', [ FBike.Battery.TemperatureMin ] );
    ShowValue ( '' );

    if (hwVersion >= 60) then
      printBatteryStats;

    ShowValue ( 'Resets', '%d', [ FBike.Battery.Resets ] );
//    ShowValue ( 'ggjrCalib ...............: %0d', [ getValue(BATTERY, BATTERY_STSTS_GGJSRCALIB) ] );
    ShowValue ( 'vctrlShorts', '%d', [ FBike.Battery.VCtrlShorts ] );
    ShowValue ( 'LMD', '%0.2fAh', [ FBike.Battery.LMD ] );
    ShowValue ( 'Cell capacity', '%0.2fAh', [ FBike.Battery.CellCapacity ] );

  end;
  ShowValue ( '' );
end;

procedure TfrmBionXMain.ShowMotorSettings;
var
  hwVersion : integer;
begin
  hwVersion := FBike.Motor.HardwareVersion;
  if (hwVersion = 0) then
    ShowValue ( 'Motor not responding' )
  else
  begin
    // Partinfo
    ShowValue ( 'Motor information:' );
    ShowValue ( 'hardware version', '%0.2d', [hwVersion] );
    ShowValue ( 'software version', '%0.2d', [ FBike.Motor.SoftwareVersion ] );
    ShowValue ( 'part number', '%0.5d', [ FBike.Motor.PartNumber ] );
    ShowValue ( 'item number', '%0.5d', [ FBike.Motor.ItemNumber ] );
    ShowValue ( 'manufacturing date', '%s', [ DateToStr ( FBike.Motor.ProductionDate ) ] );
    ShowValue ( '' );

    // Settings
    ShowValue ( 'wheel circumference', '%d mm', [ FBike.Motor.WheelCircumference ]);
    ShowValue ( 'speed limit', '%0.2f Km/h', [ FBike.Motor.MaxSpeed ] );
    ShowValue ( '' );

    // misc
//    ShowValue ( 'temperature', '%dC', [ FBike.Motor.Temperature ] );which register?

  end;
  ShowValue ( '' );
end;

procedure TfrmBionXMain.RememberSettingValues;
begin
  // misc settings
  FSettings.WheelCircumference := edWheelCircumference.Value;
  FSettings.ConsoleSwitches    := cbConsoleSwitches.ItemIndex;
  FSettings.MetricUnits        := cbMetricUnits.ItemIndex;
  FSettings.AutoShutdownDelay  := edAutoShutdownDelay.Value;
  FSettings.Odometer := edOdometer.Value;
  FSettings.ShowRemainingDistanceAndTime := cbShowRemainingDistanceAndTime.ItemIndex;

  // brake
  FSettings.BrakeSensor        := cbBrakeSensor.ItemIndex;
  FSettings.BrakeRekuLevel     := edBrakeRekuLevel.Value;

end;

procedure TfrmBionXMain.ReadSettings;
begin
  // misc settings
  edWheelCircumference.Value := FBike.Console.WheelCircumference;
  cbConsoleSwitches.ItemIndex := ord ( FBike.Console.FlippedSwitches );
  cbMetricUnits.ItemIndex := ord ( FBike.Console.MetricUnits );
  edAutoShutdownDelay.Value := FBike.Battery.AutoShutdownDelay;
  edOdometer.Value := FBike.Console.Odometer;
  cbShowRemainingDistanceAndTime.ItemIndex := ord ( FBike.Console.ShowRemainigDistanceAndTime );

  // brakes
  if FBike.Console.BrakeSensorFlag then
    cbBrakeSensor.ItemIndex := FBike.Console.BrakeSensorType+1
  else
    cbBrakeSensor.ItemIndex := 0;
  edBrakeRekuLevel.Value := FBike.Console.BrakeRekuperationLevel;

  RememberSettingValues;
end;

procedure TfrmBionXMain.WriteSettings;
begin
  // compare the curent values with remembered values and
  // write only differences to bike
  // preferable do not write to the bike components direct. Write and
  // use Bike.Set... procedures, in which also the valudation is done

  // misc settings
  if edWheelCircumference.Value <> FSettings.WheelCircumference then
    FBike.SetWheelCircumference ( edWheelCircumference.Value );
  if cbConsoleSwitches.ItemIndex <> FSettings.ConsoleSwitches then
    FBike.SetConsoleSwitchFlipped ( cbConsoleSwitches.ItemIndex = 1 );
  if cbMetricUnits.ItemIndex <> FSettings.MetricUnits then
    FBike.SetMetricUnits ( cbMetricUnits.ItemIndex = 1 );
  if edAutoShutdownDelay.Value <> FSettings.AutoShutdownDelay then
    FBike.SetAutoShutdownDelay ( edAutoShutdownDelay.Value );
  if edOdometer.Value <> FSettings.Odometer then
    FBike.SetOdometer ( edOdometer.Value );
  if cbShowRemainingDistanceAndTime.ItemIndex <> FSettings.ShowRemainingDistanceAndTime then
    FBike.SetShowRemainingDistanceAndTime ( cbShowRemainingDistanceAndTime.ItemIndex=1 );

  { nice idea, but doesn't work, the registers are not writeable
    if edDistance.Value <> FSettings.Distance then
      FBike.SetDistance ( edDistance.Value );
    if edAvgSpeed.Value <> FSettings.AvgSpeed then
      FBike.SetAverageSpeed ( edAvgSpeed.Value );
    if edChrono.Text <> FSettings.Chrono then
      FBike.SetChrono ( StrToTime ( edChrono.Text ));
  }

  // brake
  if cbBrakeSensor.ItemIndex <> FSettings.BrakeSensor then
    FBike.SetBrakeSensor( cbBrakeSensor.ItemIndex > 0, cbBrakeSensor.ItemIndex - 1 );
  if edBrakeRekuLevel.Value <> FSettings.BrakeRekuLevel then
    FBike.SetBrakeRekuperationLevel ( edBrakeRekuLevel.Value );

  RememberSettingValues;
end;

procedure TfrmBionXMain.RememberTuningValues;
begin
  // speed limits
  FSettings.AssistMaxSpeed := edAssistMaxSpeed.Value;
  FSettings.AssistMinSpeed := edAssistMinSpeed.Value;
  FSettings.ThrottleMaxSpeed := edThrottleMaxSpeed.Value;

  // assist levels
  FSettings.InitialAssistLevel := edInitialAssistLevel.Value;
  FSettings.AssistLevel1 := edAssistLevel1.Value;
  FSettings.AssistLevel2 := edAssistLevel2.Value;
  FSettings.AssistLevel3 := edAssistLevel3.Value;
  FSettings.AssistLevel4 := edAssistLevel4.Value;
  FSettings.AssistLevelMountain := edAssistLevelMountain.Value;

  // torque sensor
  FSettings.TorqueSensorGain := edTorqueSensorGain.Value;
  FSettings.TorqueSensorSpeed := edTorqueSensorSpeed.Value;
  FSettings.TorqueSensorExtraGain := edTorqueSensorExtraGain.Value;
  FSettings.TorqueSensorExtraGainMaxSpeed := edTorqueSensorExtraGainMaxSpeed.Value;

  FSettings.BoostLevel := edBoostLevel.Value;
end;

procedure TfrmBionXMain.ReadTuning;
begin
  // speed limits
  edAssistMaxSpeed.Value := FBike.Console.AssistMaxSpeed;
  edAssistMinSpeed.Value := FBike.Console.AssistMinSpeed;
  edThrottleMaxSpeed.Value := FBike.Console.ThrottleMaxSpeed;

  // assist levels
  edInitialAssistLevel.Value := FBike.Console.InitialAssistLevel;
  edAssistLevel1.Value := FBike.Console.AssistLevel1;
  edAssistLevel2.Value := FBike.Console.AssistLevel2;
  edAssistLevel3.Value := FBike.Console.AssistLevel3;
  edAssistLevel4.Value := FBike.Console.AssistLevel4;
  edAssistLevelMountain.Value := FBike.Console.MountainAssistLevel;

  // torque sensor
  edTorqueSensorGain.Value := FBike.Console.TorqueSensorGain;
  edTorqueSensorSpeed.Value := FBike.Console.TorqueSensorSpeed;
  edTorqueSensorExtraGain.Value := FBike.Console.TorqueSensorExtraGain;
  edTorqueSensorExtraGainMaxSpeed.Value := FBike.Console.TorqueSensorExtraGainMaxSpeed;

  edBoostLevel.Value := FBike.Console.BoostLevel;

  RememberTuningValues;
end;

procedure TfrmBionXMain.WriteTuning;
begin
  // compare the curent values with remembered values and
  // write only differences to bike
  // preferable do not write to the bike components direct. Write and
  // use Bike.Set... procedures, in which also the valudation is done

  // speed limits
  if edAssistMaxSpeed.Value <> FSettings.AssistMaxSpeed then
    FBike.SetAssistMaxSpeed ( edAssistMaxSpeed.Value );
  if edAssistMinSpeed.Value <> FSettings.AssistMinSpeed then
    FBike.SetAssistMinSpeed ( edAssistMinSpeed.Value );
  if edThrottleMaxSpeed.Value <> FSettings.ThrottleMaxSpeed then
    FBike.SetThrottleMaxSpeed ( edThrottleMaxSpeed.Value );

  // assist levels
  if edInitialAssistLevel.Value <> FSettings.InitialAssistLevel then
    FBike.SetAssistInitialLevel( edInitialAssistLevel.Value );
  if edAssistLevel1.Value <> FSettings.AssistLevel1 then
    FBike.SetAssistLevel1( edAssistLevel1.Value );
  if edAssistLevel2.Value <> FSettings.AssistLevel2 then
    FBike.SetAssistLevel2( edAssistLevel2.Value );
  if edAssistLevel3.Value <> FSettings.AssistLevel3 then
    FBike.SetAssistLevel3( edAssistLevel3.Value );
  if edAssistLevel4.Value <> FSettings.AssistLevel4 then
    FBike.SetAssistLevel4( edAssistLevel4.Value );
  if edAssistLevelMountain.Value <> FSettings.AssistLevelMountain then
    FBike.SetAssistLevelMountain( edAssistLevelMountain.Value );

  // torque sensor
  if edTorqueSensorGain.Value <> FSettings.TorqueSensorGain then
    FBike.SetTorqueSensorGain ( edTorqueSensorGain.Value );
  if edTorqueSensorSpeed.Value <> FSettings.TorqueSensorSpeed then
    FBike.SetTorqueSensorSpeed ( edTorqueSensorSpeed.Value );
  if edTorqueSensorExtraGain.Value <> FSettings.TorqueSensorExtraGain then
    FBike.SetTorqueSensorExtraGain ( edTorqueSensorExtraGain.Value );
  if edTorqueSensorExtraGainMaxSpeed.Value <> FSettings.TorqueSensorExtraGainMaxSpeed then
    FBike.SetTorqueSensorExtraGainMaxSpeed ( edTorqueSensorExtraGainMaxSpeed.Value );

  if edBoostLevel.Value <> FSettings.BoostLevel then
    FBike.SetBoostLevel ( edBoostLevel.Value );

  RememberTuningValues;
end;

procedure TfrmBionXMain.LoadTuningProfile ( const Filename : string );
var
  ProfileFile : TBionXProfileFile;

//  procedure SetValue (
begin
  ProfileFile := TBionXProfileFile.Create ( Filename );
  try
    if ProfileFile.AssistMinSpeed >= 0 then
      edAssistMinSpeed.Value := ProfileFile.AssistMinSpeed;
    if ProfileFile.AssistMaxSpeed >= 0 then
      edAssistMaxSpeed.Value := ProfileFile.AssistMaxSpeed;
    if ProfileFile.ThrottleMaxSpeed >= 0 then
      edThrottleMaxSpeed.Value := ProfileFile.ThrottleMaxSpeed;

    if ProfileFile.InitialAssistLevel >= 0 then
      edInitialAssistLevel.Value := ProfileFile.InitialAssistLevel;
    if ProfileFile.AssistLevel1 >= 0 then
      edAssistLevel1.Value := ProfileFile.AssistLevel1;
    if ProfileFile.AssistLevel2 >= 0 then
      edAssistLevel2.Value := ProfileFile.AssistLevel2;
    if ProfileFile.AssistLevel3 >= 0 then
      edAssistLevel3.Value := ProfileFile.AssistLevel3;
    if ProfileFile.AssistLevel4 >= 0 then
      edAssistLevel4.Value := ProfileFile.AssistLevel4;
    if ProfileFile.AssistLevelMountain >= 0 then
      edAssistLevelMountain.Value := ProfileFile.AssistLevelMountain;

    if ProfileFile.TorquesensorGain >= 0 then
      edTorqueSensorGain.Value := ProfileFile.TorquesensorGain;
    if ProfileFile.TorquesensorSpeed >= 0 then
      edTorqueSensorSpeed.Value := ProfileFile.TorquesensorSpeed;
    if ProfileFile.TorquesensorExtraGain >= 0 then
      edTorqueSensorExtraGain.Value := ProfileFile.TorquesensorExtraGain;
    if ProfileFile.TorquesensorExtraGainMaxSpeed >= 0 then
      edTorqueSensorExtraGainMaxSpeed.Value := ProfileFile.TorquesensorExtraGainMaxSpeed;

    if ProfileFile.BoostLevel >= 0 then
      edBoostLevel.Value := ProfileFile.BoostLevel;
  finally
    ProfileFile.Free;
  end;
end;

procedure TfrmBionXMain.SaveTuningProfile ( const Filename : string );
var
  ProfileFile : TBionXProfileFile;
begin
  ProfileFile := TBionXProfileFile.Create ( Filename );
  try
    ProfileFile.AssistMinSpeed := edAssistMinSpeed.Value;
    ProfileFile.AssistMaxSpeed := edAssistMaxSpeed.Value;
    ProfileFile.ThrottleMaxSpeed := edThrottleMaxSpeed.Value;

    ProfileFile.InitialAssistLevel := edInitialAssistLevel.Value;
    ProfileFile.AssistLevel1 := edAssistLevel1.Value;
    ProfileFile.AssistLevel2 := edAssistLevel2.Value;
    ProfileFile.AssistLevel3 := edAssistLevel3.Value;
    ProfileFile.AssistLevel4 := edAssistLevel4.Value;
    ProfileFile.AssistLevelMountain := edAssistLevelMountain.Value;

    ProfileFile.TorquesensorGain := edTorqueSensorGain.Value;
    ProfileFile.TorquesensorSpeed := edTorqueSensorSpeed.Value;
    ProfileFile.TorquesensorExtraGain := edTorqueSensorExtraGain.Value;
    ProfileFile.TorquesensorExtraGainMaxSpeed := edTorqueSensorExtraGainMaxSpeed.Value;

    edBoostLevel.Value := ProfileFile.BoostLevel;
  finally
    ProfileFile.Free;
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
    if FBike.Connect ( CANAdapter ) then
    begin
      ReadSettings;
      ReadTuning;
      EnableControls ( true );
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
        ShowValue ( Format ( 'Reg %0.2x (%3d) = %0.2x / %3d', [i, i, v, v] ));
      except
        on E:Exception do
          ShowValue ( Format ( 'Reg %0.2x (%3d) no data', [ i, i ] ));
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
               + 'This program is published without any warranty. Improper settings may damage your system.'#13'Use at your own risk.'#13#13
               + 'Enjoy and have a safe ride.',
               mtInformation, [mbOK], 0, mbOK );
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

procedure TfrmBionXMain.btnInfoSaveClick ( Sender : TObject ) ;
begin
  if sdInfo.Execute then
    mmInfo.Lines.SaveToFile( sdInfo.Filename );
end;

procedure TfrmBionXMain.btnInfoClearClick(Sender: TObject);
begin
  mmInfo.Clear;
end;

procedure TfrmBionXMain.btnLogClearClick(Sender: TObject);
begin
  mmLog.Clear;
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

procedure TfrmBionXMain.btnLoadTuningProfileClick ( Sender: TObject ) ;
begin
  if odProfile.Execute then
    LoadTuningProfile ( odProfile.Filename );
end;

procedure TfrmBionXMain.btnsaveTuningProfileClick ( Sender: TObject ) ;
begin
  if sdProfile.Execute then
    SaveTuningProfile ( sdProfile.Filename );
end;

(**** tsTests routines ********************************************************)

// for debugging/analyzing purpose only
procedure TfrmBionXMain.Button1Click(Sender: TObject);
type
  TByteSet = set of byte;

const
  console_validRegisters  = [ 80..83, 100..120, 122..126, 128..142, 160..171, 173..199, 208..212, 215..220, 222..223 ];
  console_unknownRegisters = [ 104..108, 112..113, 122..126, 128, 140, 167..171, 173..175, 181..194, 197, 199, 210..212, 215..220, 222..223  ];

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
          ShowValue ( Format ( 'Reg %0.2x (%3d) = %0.2x / %3d', [i, i, v, v] ));
        except
          on E:Exception do
            ShowValue ( Format ( 'Reg %0.2x (%3d) error: %s', [ i, i, E.Message ] ));
        end;
        Last := i;
      end;
    end;
  end;

begin
  ShowRegisters ( FBike.Console, console_validRegisters );

//  ShowRegisters ( FBike.Motor, [0..255] );
end;



end.

