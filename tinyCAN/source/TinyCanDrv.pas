{ *********** TINY - CAN Treiber **************                          }
{ Copyright (C) 2009 - 2012 Klaus Demlehner (klaus@mhs-elektronik.de)    }
{     www.mhs-elektronik.de                                              }
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

{ ts 2014/05/20 adapted to Unix i.e. Raspberry Pi                        }
{ ts 2013/08/08 adapted to Lararus                                       }

unit TinyCanDrv;

interface
{$WARN SYMBOL_DEPRECATED OFF}

{$ifdef FPC}
  {$mode delphi}
{$endif}

uses
  SysUtils,
  {$ifdef FPC}
  DynLibs,
  {$else}
  Windows, 
  {$endif}
  {$ifndef UNIX}
  Registry,
  {$endif}
  Forms, Classes;
  
const

  FlagsCanLength: DWORD = ($0000000F);
  FlagsCanRTR: DWORD    = ($00000040);
  FlagsCanEFF: DWORD    = ($00000080);

  FilFlagsEFF: DWORD    = ($00000080);
  FilFlagsEnable: DWORD = ($80000000);

  INDEX_FIFO_PUFFER_MASK: DWORD = ($0000FFFF);
  INDEX_SOFT_FLAG: DWORD        = ($02000000);
  INDEX_RXD_TXT_FLAG: DWORD     = ($01000000);
  INDEX_CAN_KANAL_MASK: DWORD   = ($000F0000);
  INDEX_CAN_DEVICE_MASK: DWORD  = ($00F00000);

  INDEX_CAN_KANAL_A: DWORD      = ($00000000);
  INDEX_CAN_KANAL_B: DWORD      = ($00010000);

  CAN_CMD_NONE: Word              = ($0000);
  CAN_CMD_RXD_OVERRUN_CLEAR: Word = ($0001);
  CAN_CMD_RXD_FIFOS_CLEAR: Word   = ($0002);
  CAN_CMD_TXD_OVERRUN_CLEAR: Word = ($0004);
  CAN_CMD_TXD_FIFOS_CLEAR: Word   = ($0008);
  CAN_CMD_HW_FILTER_CLEAR: Word   = ($0010);
  CAN_CMD_SW_FILTER_CLEAR: Word   = ($0020);
  CAN_CMD_TXD_PUFFERS_CLEAR: Word = ($0040);

  CAN_CMD_ALL_CLEAR: Word         = ($0FFF);

  // SetEvent
  EVENT_ENABLE_PNP_CHANGE: Word          = ($0001);
  EVENT_ENABLE_STATUS_CHANGE: Word       = ($0002);
  EVENT_ENABLE_RX_FILTER_MESSAGES: Word  = ($0004);
  EVENT_ENABLE_RX_MESSAGES: Word         = ($0008);
  EVENT_ENABLE_ALL: Word                 = ($00FF);

  EVENT_DISABLE_PNP_CHANGE: Word         = ($0100);
  EVENT_DISABLE_STATUS_CHANGE: Word      = ($0200);
  EVENT_DISABLE_RX_FILTER_MESSAGES: Word = ($0400);
  EVENT_DISABLE_RX_MESSAGES: Word        = ($0800);
  EVENT_DISABLE_ALL: Word                = ($FF00);

  CanSpeedTab: array[0..8] of Word = (10,    // 10 kBit/s
                                      20,    // 20 kBit/s
                                      50,    // 50 kBit/s
                                      100,   // 100 kBit/s
                                      125,   // 125 kBit/s
                                      250,   // 250 kBit/s
                                      500,   // 500 kBit/s
                                      800,   // 800 kBit/s
                                      1000); // 1 MBit/s

   BaudRateTab: array[0..16] of DWord = (4800,
                                         9600,
                                         10400,
                                         14400,
                                         19200,
                                         28800,
                                         38400,
                                         57600,
                                         115200,                                        
                                         125000,
                                         153600,
                                         230400,
                                         250000,
                                         460800,
                                         500000,
                                         921600,
                                         1000000);                                      

  {$ifdef UNIX}
  API_DRIVER_LIB              = 'libmhstcan.so';
  API_DRIVER_EXT              = '.so';
  ETC_TINY_CAN_API_CONF       = '/etc/TinyCanApi.conf';
  ETC_TINY_CAN_API_PATH_ENTRY = 'PATH';
  {$else}
  API_DRIVER_LIB              = 'mhstcan.dll';
  API_DRIVER_EXT              = '.dll';
  REG_TINY_CAN_API            = 'Software\Tiny-CAN\API\';
  REG_TINY_CAN_API_PATH_ENTRY = 'PATH';
  {$endif}


type
EDllLoadError = class(Exception);

// CAN Übertragungsgeschwindigkeit
TCanSpeed = (CAN_10K_BIT, CAN_20K_BIT, CAN_50K_BIT, CAN_100K_BIT, CAN_125K_BIT,
             CAN_250K_BIT, CAN_500K_BIT, CAN_800K_BIT, CAN_1M_BIT);

TSerialBaudRate = (SER_4800_BAUD, SER_9600_BAUD, SER_10k4_BAUD, SER_14k4_BAUD,
                   SER_19k2_BAUD, SER_28k8_BAUD, SER_38k4_BAUD, SER_57k6_BAUD,
                   SER_115k2_BAUD, SER_125k_BAUD, SER_153k6_BAUD, SER_230k4_BAUD,
                   SER_250k_BAUD, SER_460k8_BAUD, SER_500k_BAUD, SER_921k6_BAUD,
                   SER_1000k_BAUD);

TEventMask = (PNP_CHANGE_EVENT, STATUS_CHANGE_EVENT, RX_FILTER_MESSAGES_EVENT,
              RX_MESSAGES_EVENT);
TEventMasks = set of TEventMask;
TInterfaceType = (INTERFACE_USB, INTERFACE_SERIEL);
TLogFlag = (LOG_F0, LOG_F1, LOG_F2, LOG_F3, LOG_F4, LOG_F5, LOG_F6, LOG_F7);
TLogFlags = set of TLogFlag;


// CAN Bus Mode
TCanMode = (OP_CAN_NONE,              // 0 = keine Änderung
            OP_CAN_START,             // 1 = Startet den CAN Bus
            OP_CAN_STOP,              // 2 = Stopt den CAN Bus
            OP_CAN_RESET,             // 3 = Reset CAN Controller
            OP_CAN_START_LOM,         //4 = Startet den CAN-Bus im Silent Mode (Listen Only Mode)
            OP_CAN_START_NO_RETRANS); //5 = Startet den CAN-Bus im Automatic Retransmission disable Mode            
PCanMode = ^TCanMode;

// DrvStatus
TDrvStatus = (DRV_NOT_LOAD,             // 0 = Die Treiber DLL wurde noch nicht geladen
              DRV_STATUS_NOT_INIT,      // 1 = Treiber noch nicht Initialisiert
              DRV_STATUS_INIT,          // 2 = Treiber erfolgrich Initialisiert
              DRV_STATUS_PORT_NOT_OPEN, // 3 = Die Schnittstelle wurde geöffnet
              DRV_STATUS_PORT_OPEN,     // 4 = Die Schnittstelle wurde nicht geöffnet
              DRV_STATUS_DEVICE_FOUND,  // 5 = Verbindung zur Hardware wurde Hergestellt
              DRV_STATUS_CAN_OPEN,      // 6 = Device wurde geöffnet und erfolgreich Initialisiert
              DRV_STATUS_CAN_RUN_TX,    // 7 = CAN Bus RUN nur Transmitter (wird nicht verwendet !)
              DRV_STATUS_CAN_RUN);      // 8 = CAN Bus RUN
PDrvStatus = ^TDrvStatus;

// CanStatus
TCanStatus = (CAN_STATUS_OK,            // 0 = CAN-Controller: Ok
              CAN_STATUS_ERROR,         // 1 = CAN-Controller: CAN Error
              CAN_STATUS_WARNING,       // 2 = CAN-Controller: Error warning
              CAN_STATUS_PASSIV,        // 3 = CAN-Controller: Error passiv
              CAN_STATUS_BUS_OFF,       // 4 = CAN-Controller: Bus Off
              CAN_STATUS_UNBEKANNT);    // 5 = CAN-Controller: Status Unbekannt
PCanStatus = ^TCanStatus;

// Fifo Status
TCanFifoStatus = (CAN_FIFO_OK,                // 0 = Fifo-Status: Ok
                  CAN_FIFO_HW_OVERRUN,        // 1 = Fifo-Status: Überlauf
                  CAN_FIFO_SW_OVERRUN,        // 2 = Fifo-Status: Überlauf
                  CAN_FIFO_HW_SW_OVERRUN,     // 3 = Fifo-Status: Überlauf
                  CAN_FIFO_STATUS_UNBEKANNT); // 4 = Fifo-Status: Unbekannt
PCanFifoStatus = ^TCanFifoStatus;

{/******************************************/}
{/*            CAN Message Type            */}
{/******************************************/}
TCanData = packed record
  case Integer of
    0: (Chars: array[0..7] of Char);
    1: (Bytes: array[0..7] of Byte);
    2: (Words: array[0..3] of Word);
    3: (Longs: array[0..1] of DWORD);
  end;
PCanData = ^TCanData;

TCanTime = packed record
  Sec: DWORD;
  USec: DWORD;
  end;
PCanTime = ^TCanTime;

TCanMsg = packed record
  Id: DWORD;
  Flags: DWORD;
  Data: TCanData;
  Time: TCanTime;
  end;
 PCanMsg = ^TCanMsg;

{/******************************************/}
{/*         CAN Message Filter Type        */}
{/******************************************/}
TMsgFilter = packed record
  Maske: DWORD;
  Code: DWORD;
  Flags: DWORD;
  Data: TCanData;
  end;
PMsgFilter = ^TMsgFilter;


{/******************************************/}
{/*             Device Status              */}
{/******************************************/}
TDeviceStatus = record
  DrvStatus: TDrvStatus;
  CanStatus: TCanStatus;
  FifoStatus: TCanFifoStatus;
  end;
PDeviceStatus = ^TDeviceStatus;


TDeviceStatusDrv = packed record
  DrvStatus: Integer;
  CanStatus: Byte;
  FifoStatus: Byte;
  end;
PDeviceStatusDrv = ^TDeviceStatusDrv;



{/***************************************************************/}
{/*  Treiber Callback-Funktionen                                */}
{/***************************************************************/}
TF_CanPnPEventCallback = procedure(index: DWORD; status: Integer); stdcall;
PF_CanPnPEventCallback = ^TF_CanPnPEventCallback;

TF_CanStatusEventCallback = procedure(index: DWORD; device_status_drv: PDeviceStatusDrv); stdcall;
PF_CanStatusEventCallback = ^TF_CanStatusEventCallback;

TF_CanRxEventCallback = procedure(index: DWORD; msg: PCanMsg; count: Integer); stdcall;
PF_CanRxEventCallback = ^TF_CanRxEventCallback;

{/***************************************************************/}
{/*  Funktionstypen                                             */}
{/***************************************************************/}
TF_CanInitDriver = function(options: PAnsiChar): Integer; stdcall;
TF_CanDownDriver = procedure; stdcall;
TF_CanSetOptions = function(options: PAnsiChar): Integer; stdcall;
TF_CanDeviceOpen = function(index: DWORD; parameter: PAnsiChar): Integer; stdcall;
TF_CanDeviceClose = function(index: DWORD): Integer; stdcall;
TF_CanApplaySettings = function(index: DWORD): Integer; stdcall;

TF_CanSetMode = function(index: DWORD; can_op_mode: Byte; can_command: Word): Integer; stdcall;
TF_CanSet = function(index: DWORD; obj_index: Word; obj_sub_index: Word; data: Pointer; size: Integer): Integer; stdcall;
TF_CanGet = function(index: DWORD; obj_index: Word; obj_sub_index: Word; data: Pointer; size: Integer): Integer; stdcall;

TF_CanTransmit = function(index: DWORD; msg: PCanMsg; count: Integer): Integer; stdcall;
TF_CanTransmitClear = procedure(index: DWORD); stdcall;
TF_CanTransmitGetCount = function(index: DWORD): DWORD; stdcall;
TF_CanTransmitSet = function(index: DWORD; cmd: Word; time: DWORD): Integer; stdcall;
TF_CanReceive = function(index: DWORD; msg: PCanMsg; count: Integer): Integer; stdcall;
TF_CanReceiveClear = procedure(index: DWORD); stdcall;
TF_CanReceiveGetCount = function(index: DWORD): DWORD; stdcall;

TF_CanSetSpeed = function(index: DWORD; speed: Word): Integer; stdcall;
TF_CanDrvInfo = function: PAnsiChar; stdcall;
TF_CanDrvHwInfo = function(index: DWORD): PAnsiChar; stdcall;
TF_CanSetFilter = function(index: DWORD; msg_filter: PMsgFilter): Integer; stdcall;

TF_CanGetDeviceStatus = function(index: DWORD; status: PDeviceStatusDrv): Integer; stdcall;

TF_CanSetPnPEventCallback = procedure(event_proc: PF_CanPnPEventCallback); stdcall;
TF_CanSetStatusEventCallback = procedure(event_proc: PF_CanStatusEventCallback); stdcall;
TF_CanSetRxEventCallback = procedure(event_proc: PF_CanRxEventCallback); stdcall;

TF_CanSetEvents = procedure(events: Word); stdcall;
TF_CanEventStatus = function:DWORD; stdcall;

TOnCanPnPEvent = procedure(Sender: TObject; index: DWORD; status: Integer) of Object;
TOnCanStatusEvent = procedure(Sender: TObject; index: DWORD; device_status: TDeviceStatus) of Object;
TOnCanRxDEvent = procedure(Sender: TObject; index: DWORD; msg: PCanMsg; count: Integer) of Object;

TTinyCAN = class(TComponent)
  private
    FTreiberName: String;
    FPort: Integer;
    FBaudRate: TSerialBaudRate;
    FCanSpeed: TCANSpeed;
    FEventMasks: TEventMasks;
    FInterfaceType: TInterfaceType;
    FPnPEnable: boolean;
    FAutoReOpen: boolean;
    FRxDFifoSize: Word;
    FTxDFifoSize: Word;
    FCanRxDBufferSize: Word;

    FDeviceSnr: String;
    FCfgFile: String;
    FLogFile: String;
    FLogFlags: TLogFlags;
    FInitParameterStr: String;
    FOptionsStr: String;
    FOpenStr: String;
    function GetLogFlags: DWORD;
    function TestApi(name: String): Boolean;
    function GetApiDriverWithPath(driver_file: String): String;
    procedure SetCanSpeed(speed: TCanSpeed);
    procedure SetEventMasks(value: TEventMasks);
  public
    { Events}
    pmCanPnPEvent: TOnCanPnPEvent;
    pmCanStatusEvent: TOnCanStatusEvent;
    pmCanRxDEvent: TOnCanRxDEvent;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanInitDriver: Integer;
    procedure CanDownDriver;
    function CanSetOptions: Integer;
    function CanDeviceOpen: Integer;
    function CanDeviceClose: Integer;
    function CanApplaySettings: Integer;

    function CanSetMode(index: DWORD; can_op_mode: TCanMode; can_command: Word): Integer;
    function CanSet(index: DWORD; obj_index: Word; obj_sub_index: Word; data: Pointer; size: Integer): Integer;
    function CanGet(index: DWORD; obj_index: Word; obj_sub_index: Word; data: Pointer; size: Integer): Integer;

    function CanTransmit(index: DWORD; msg: PCanMsg; count: Integer): Integer;
    procedure CanTransmitClear(index: DWORD);
    function CanTransmitGetCount(index: DWORD): DWORD;
    function CanTransmitSet(index: DWORD; cmd: Word; time: DWORD): Integer;
    function CanReceive(index: DWORD; msg: PCanMsg; count: Integer): Integer;
    procedure CanReceiveClear(index: DWORD);
    function CanReceiveGetCount(index: DWORD): DWORD;

    function CanSetSpeed(index: DWORD; speed: TCanSpeed): Integer;
    function CanDrvInfo: PAnsiChar;
    function CanDrvHwInfo(index: DWORD): PAnsiChar;
    function CanSetFilter(index: DWORD; msg_filter: PMsgFilter): Integer;

    function CanGetDeviceStatus(index: DWORD; status: PDeviceStatus): Integer;

    procedure CanSetPnPEventCallback(event_proc: PF_CanPnPEventCallback);
    procedure CanSetStatusEventCallback(event_proc: PF_CanStatusEventCallback);
    procedure CanSetRxEventCallback(event_proc: PF_CanRxEventCallback);

    procedure CanSetEvents(events: TEventMasks);
    function CanEventStatus: DWORD;

    function LoadDriver: Integer;
    procedure DownDriver;
  published
    { Published-Deklarationen }
    property TreiberName: String read FTreiberName write FTreiberName;
    property Port: Integer read FPort write FPort default 0;
    property BaudRate: TSerialBaudRate read FBaudRate write FBaudRate default SER_921k6_BAUD;
    property CanSpeed: TCanSpeed read FCanSpeed write SetCanSpeed default CAN_125K_BIT;
    property EventMasks: TEventMasks read FEventMasks write SetEventMasks default [];
    property InterfaceType: TInterfaceType read FInterfaceType write FInterfaceType default INTERFACE_USB;
    property PnPEnable: boolean read FPnPEnable write FPnPEnable default true;
    property AutoReOpen: boolean read FAutoReOpen write FAutoReOpen default true;
    property RxDFifoSize: Word read FRxDFifoSize write FRxDFifoSize default 4096;
    property TxDFifoSize: Word read FTxDFifoSize write FTxDFifoSize default 255;
    property CanRxDBufferSize: Word read FCanRxDBufferSize write FCanRxDBufferSize default 50;

    property DeviceSnr: String read FDeviceSnr write FDeviceSnr;
    property CfgFile: String read FCfgFile write FCfgFile;
    property LogFile: String read FLogFile write FLogFile;
    property LogFlags: TLogFlags read FLogFlags write FLogFlags default [];
    property InitParameterStr: String read FInitParameterStr write FInitParameterStr;
    property OptionsStr: String read FOptionsStr write FOptionsStr;
    property OpenStr: String read FOpenStr write FOpenStr;
    { Events }
    property OnCanPnPEvent: TOnCanPnPEvent read pmCanPnPEvent write pmCanPnPEvent;
    property OnCanStatusEvent: TOnCanStatusEvent read pmCanStatusEvent write pmCanStatusEvent;
    property OnCanRxDEvent: TOnCanRxDEvent read pmCanRxDEvent write pmCanRxDEvent;
  end;


procedure Register;
procedure CanPnPEventCallback(index: DWORD; status: Integer); stdcall;
procedure CanStatusEventCallback(index: DWORD; device_status_drv: PDeviceStatusDrv); stdcall;
procedure CanRxEventCallback(index: DWORD; msg: PCanMsg; count: Integer); stdcall;


implementation
type
  {$ifdef FPC}
  HLIBRARY= TLibHandle;
  {$ELSE}
  HLIBRARY= HMODULE;
  {$endif}
var
  DrvDLLWnd: HLIBRARY = 0;

  pmCanInitDriver: TF_CanInitDriver = nil;
  pmCanDownDriver: TF_CanDownDriver = nil;
  pmCanSetOptions: TF_CanSetOptions = nil;
  pmCanDeviceOpen: TF_CanDeviceOpen = nil;
  pmCanDeviceClose: TF_CanDeviceClose = nil;
  pmCanApplaySettings: TF_CanApplaySettings = nil;
  pmCanSetMode: TF_CanSetMode = nil;
  pmCanSet: TF_CanSet = nil;
  pmCanGet: TF_CanGet = nil;
  pmCanTransmit: TF_CanTransmit = nil;
  pmCanTransmitClear: TF_CanTransmitClear = nil;
  pmCanTransmitGetCount: TF_CanTransmitGetCount = nil;
  pmCanTransmitSet: TF_CanTransmitSet = nil;
  pmCanReceive: TF_CanReceive = nil;
  pmCanReceiveClear: TF_CanReceiveClear = nil;
  pmCanReceiveGetCount: TF_CanReceiveGetCount = nil;
  pmCanSetSpeed: TF_CanSetSpeed = nil;
  pmCanDrvInfo: TF_CanDrvInfo = nil;
  pmCanDrvHwInfo: TF_CanDrvHwInfo = nil;
  pmCanSetFilter: TF_CanSetFilter = nil;
  pmCanGetDeviceStatus: TF_CanGetDeviceStatus = nil;
  pmCanSetPnPEventCallback: TF_CanSetPnPEventCallback = nil;
  pmCanSetStatusEventCallback: TF_CanSetStatusEventCallback = nil;
  pmCanSetRxEventCallback: TF_CanSetRxEventCallback = nil;
  pmCanSetEvents: TF_CanSetEvents = nil;
  pmCanEventStatus: TF_CanEventStatus = nil;

  FTinyCAN : TTinyCAN;


{**************************************************************}
{* Object erzeugen                                            *}
{**************************************************************}
constructor TTinyCAN.Create(AOwner: TComponent);

begin;
inherited Create(AOwner);
FTinyCAN := self;
FTreiberName := '';
FPort := 0;
FBaudRate := SER_921k6_BAUD;
FCanSpeed := CAN_125K_BIT;
FEventMasks := [];
FInterfaceType := INTERFACE_USB;
FPnPEnable := true;
FAutoReOpen := true;
FRxDFifoSize := 4096;
FTxDFifoSize := 255;
FCanRxDBufferSize := 50;

FDeviceSnr:='';
FCfgFile:='';
FLogFile:='';
FLogFlags:=[];
FInitParameterStr:='';
FOptionsStr:='';
FOpenStr:='';
end;


{**************************************************************}
{* Object löschen                                             *}
{**************************************************************}
destructor TTinyCAN.Destroy;

begin
FTinyCAN := nil;
inherited Destroy;
end;


{**************************************************************}
{* Treiber DLL suchen                                         *}
{**************************************************************}
function TTinyCAN.TestApi(name: String): Boolean;
var dll_wnd: HLIBRARY;

begin;
result := False;
if length(name) > 0 then
  begin;
  dll_wnd := LoadLibrary(PChar(Name));
  if dll_wnd <> 0 then
    begin;
    if GetProcAddress(dll_wnd, 'CanDrvInfo') <> nil then
      result := True;
    FreeLibrary(dll_wnd);
    end;
  end;
end;

{$ifdef UNIX}
function EtcReadStringEntry(config, entry: String): String;
var
  sl : TStringlist;
begin
  sl := TStringlist.Create;
  try
    try
      sl.LoadFromFile ( config );
      Result := sl.Values[ entry ];
    except
      Result := '';
    end;
  finally
    sl.Free;
  end;
end;
{$else}
function RegReadStringEntry(path, entry: String): String;
var R: TRegistry;

begin;
result := '';
{Registry Komponente erzeugen}
R := TRegistry.Create(KEY_READ);
R.RootKey := HKEY_LOCAL_MACHINE;
if R.OpenKey(path, False) then
  begin;
  result := R.ReadString(entry);
  end;
{Registry Komponente freigeben}
R.CloseKey;
R.Free;
end;
{$endif}


function TTinyCAN.GetApiDriverWithPath(driver_file: String): String;
var file_name: String;

begin;
result := '';
if driver_file = '' then
  driver_file := API_DRIVER_LIB
else
  begin;
  if ExtractFileExt(driver_file) = '' then
    driver_file := driver_file + API_DRIVER_EXT;
  if ExtractFilePath(driver_file) <> '' then
    begin;
    result := driver_file;
    exit;
    end;
  end;
{$ifdef UNIX}
file_name := EtcReadStringEntry(ETC_TINY_CAN_API_CONF, ETC_TINY_CAN_API_PATH_ENTRY);
{$else}
file_name := RegReadStringEntry(REG_TINY_CAN_API, REG_TINY_CAN_API_PATH_ENTRY);
{$endif}
if length(file_name) > 0 then
  begin;
  file_name := IncludeTrailingPathDelimiter ( file_name ) + driver_file;
  if TestApi(file_name) then
    result := file_name;
  end;
if length(result) = 0 then
  begin;
  file_name := ExtractFilePath(Application.ExeName) + driver_file;
  if TestApi(file_name) then
    result := file_name;
  end;
end;



{**************************************************************}
{*  Events                                                    *}
{**************************************************************}
procedure CanPnPEventCallback(index: DWORD; status: Integer); stdcall;

begin;
  if Assigned(FTinyCAN.pmCanPnPEvent) then
    FTinyCAN.pmCanPnPEvent(FTinyCAN, Index, Status);
end;

procedure CanStatusEventCallback(index: DWORD; device_status_drv: PDeviceStatusDrv); stdcall;

var
  DeviceStatus: TDeviceStatus;
begin;
  DeviceStatus.DrvStatus := TDrvStatus(device_status_drv^.DrvStatus);
  DeviceStatus.CanStatus := TCanStatus(device_status_drv^.CanStatus);
  DeviceStatus.FifoStatus := TCanFifoStatus(device_status_drv^.FifoStatus);
  if Assigned(FTinyCAN.pmCanStatusEvent) then
    FTinyCAN.pmCanStatusEvent(FTinyCAN, Index, DeviceStatus);
end;

procedure CanRxEventCallback(index: DWORD; msg: PCanMsg; count: Integer); stdcall;

begin;
  if Assigned(FTinyCAN.pmCanRxDEvent) then
    FTinyCAN.pmCanRxDEvent(FTinyCAN, Index, msg, Count);
end;

{**************************************************************}
{* Property Set Funktionen                                    *}
{**************************************************************}
procedure TTinyCAN.SetCanSpeed(speed: TCanSpeed);

begin;
if speed <> FCanSpeed then
  begin;
  FCanSpeed := speed;
  if not (csDesigning in ComponentState) then
    CanSetSpeed(0, speed);
  end;
end;


procedure TTinyCAN.SetEventMasks(value: TEventMasks);

begin;
if value <> FEventMasks then
  begin;
  FEventMasks := value;
  if not (csDesigning in ComponentState) then
    CanSetEvents(FEventMasks);
  end;
end;


function TTinyCan.GetLogFlags: DWORD;

begin
result := 0;
if LOG_F0 in FLogFlags then
  result := result or $01;
if LOG_F1 in FLogFlags then
  result := result or $02;
if LOG_F2 in FLogFlags then
  result := result or $04;
if LOG_F3 in FLogFlags then
  result := result or $08;
if LOG_F4 in FLogFlags then
  result := result or $10;
if LOG_F5 in FLogFlags then
  result := result or $20;
if LOG_F6 in FLogFlags then
  result := result or $40;
if LOG_F7 in FLogFlags then
  result := result or $80;
end;


{**************************************************************}
{* Treiber Funktionen                                         *}
{**************************************************************}
function TTinyCAN.CanInitDriver: Integer;
var Str: String;

begin;
result := -1;
if FCanRxDBufferSize > 0 then
  Str := Format('CanRxDMode=1;CanRxDFifoSize=%u;CanTxDFifoSize=%u;CanRxDBufferSize=%u',
              [FRxDFifoSize, FTxDFifoSize, FCanRxDBufferSize])
else
  Str := Format('CanRxDMode=0;CanRxDFifoSize=%u;CanTxDFifoSize=%u',
              [FRxDFifoSize, FTxDFifoSize]);
if length(FCfgFile) > 0 then
  Str := Str + ';CfgFile=' + FCfgFile;
if length(FLogFile) > 0 then
  begin;
  Str := Str + ';LogFile=' + FLogFile;
  Str := Str + Format(';LogFlags=%u', [GetLogFlags]);
  end;
if FInterfaceType = INTERFACE_SERIEL then
  Str := Str + ';ComDrvType=0'
else  
  Str := Str + ';ComDrvType=1'; 
if length(FInitParameterStr) > 0 then
  Str := Str + ';' + FInitParameterStr;
if Assigned(pmCanInitDriver) then
  result := pmCanInitDriver(PAnsiChar(AnsiString(Str)));
end;


procedure TTinyCAN.CanDownDriver;

begin;
if Assigned(pmCanDownDriver) then
  pmCanDownDriver;
end;


function TTinyCAN.CanSetOptions: Integer;
var Str: String;

begin;
// Treiber Optionen setzen
result := -1;
if FPnPEnable then
  begin;
  if FAutoReOpen then
    Str := 'AutoConnect=1;AutoReopen=1'
  else
    Str := 'AutoConnect=1'
  end
else
  Str := 'AutoConnect=0';
if length(FOptionsStr) > 0 then
  Str := Str + ';' + FOptionsStr;
if Assigned(pmCanSetOptions) then
  result := pmCanSetOptions(PAnsiChar(AnsiString(Str)));
end;


function TTinyCAN.CanDeviceOpen: Integer;
var Str: String;

begin;
result := -1;
if FInterfaceType = INTERFACE_USB then
  begin;
  if length(FDeviceSnr) > 0 then
    Str := 'Snr=' + FDeviceSnr;
  end  
else                     
  Str := Format('Port=%u;BaudRate=%u',[FPort, BaudRateTab[ord(FBaudRate)]]);                     
if length(FOpenStr) > 0 then
  Str := Str + ';' + FOpenStr;
if Assigned(pmCanDeviceOpen) then
  result := pmCanDeviceOpen(0, PAnsiChar(AnsiString(Str)));
end;


function TTinyCAN.CanDeviceClose: Integer;

begin;
result := -1;
if Assigned(pmCanDeviceClose) then
  result := pmCanDeviceClose(0);
end;


function TTinyCAN.CanApplaySettings: Integer;

begin;
result := -1;
if Assigned(pmCanApplaySettings) then
  result := pmCanApplaySettings(0);
end;


function TTinyCAN.CanSetMode(index: DWORD; can_op_mode: TCanMode; can_command: Word): Integer;

begin;
result := -1;
if Assigned(pmCanSetMode) then
  result := pmCanSetMode(index, ord(can_op_mode), can_command);
end;


function TTinyCAN.CanSet(index: DWORD; obj_index: Word; obj_sub_index: Word; data: Pointer; size: Integer): Integer;

begin;
result := -1;
if Assigned(pmCanSet) then
  result := pmCanSet(index, obj_index, obj_sub_index, data, size);
end;


function TTinyCAN.CanGet(index: DWORD; obj_index: Word; obj_sub_index: Word; data: Pointer; size: Integer): Integer;

begin;
result := -1;
if Assigned(pmCanGet) then
  result := pmCanGet(index, obj_index, obj_sub_index, data, size);
end;


function TTinyCAN.CanTransmit(index: DWORD; msg: PCanMsg; count: Integer): Integer;

begin;
result := -1;
if Assigned(pmCanTransmit) then
  result := pmCanTransmit(index, msg, count);
end;


procedure TTinyCAN.CanTransmitClear(index: DWORD);

begin;
if Assigned(pmCanTransmitClear) then
  pmCanTransmitClear(index);
end;


function TTinyCAN.CanTransmitGetCount(index: DWORD): DWORD;

begin;
result := 0;
if Assigned(pmCanTransmitGetCount) then
  result := pmCanTransmitGetCount(index);
end;


function TTinyCAN.CanTransmitSet(index: DWORD; cmd: Word; time: DWORD): Integer;

begin;
result := -1;
if Assigned(pmCanTransmitSet) then
  result := pmCanTransmitSet(index, cmd, time);
end;


function TTinyCAN.CanReceive(index: DWORD; msg: PCanMsg; count: Integer): Integer;

begin;
result := -1;
if Assigned(pmCanReceive) then
  result := pmCanReceive(index, msg, count);
end;


procedure TTinyCAN.CanReceiveClear(index: DWORD);

begin;
if Assigned(pmCanReceiveClear) then
  pmCanReceiveClear(index);
end;


function TTinyCAN.CanReceiveGetCount(index: DWORD): DWORD;

begin;
result := 0;
if Assigned(pmCanReceiveGetCount) then
  result := pmCanReceiveGetCount(index);
end;


function TTinyCAN.CanSetSpeed(index: DWORD; speed: TCanSpeed): Integer;
var speed_value: Word;

begin;
result := -1;
speed_value := CanSpeedTab[ord(speed)];
if Assigned(pmCanSetSpeed) then
  result := pmCanSetSpeed(index, speed_value);
end;


function TTinyCAN.CanDrvInfo: PAnsiChar;

begin;
if Assigned(pmCanDrvInfo) then
  result := pmCanDrvInfo
else
  result := nil;
end;


function TTinyCAN.CanDrvHwInfo(index: DWORD): PAnsiChar;

begin;
if Assigned(pmCanDrvHwInfo) then
  result := pmCanDrvHwInfo(index)
else
  result := nil;
end;


function TTinyCAN.CanSetFilter(index: DWORD; msg_filter: PMsgFilter): Integer;

begin;
result := -1;
if Assigned(pmCanSetFilter) then
  result := pmCanSetFilter(index, msg_filter);
end;


function TTinyCAN.CanGetDeviceStatus(index: DWORD; status: PDeviceStatus): Integer;
var status_drv: TDeviceStatusDrv;

begin;
result := -1;
if Assigned(pmCanGetDeviceStatus) then
  result := pmCanGetDeviceStatus(index, @status_drv);
status^.DrvStatus := TDrvStatus(status_drv.DrvStatus);
status^.CanStatus := TCanStatus(status_drv.CanStatus);
status^.FifoStatus := TCanFifoStatus(status_drv.FifoStatus);
end;


procedure TTinyCAN.CanSetPnPEventCallback(event_proc: PF_CanPnPEventCallback);
begin;
if Assigned(pmCanSetPnPEventCallback) then
  pmCanSetPnPEventCallback(event_proc);
end;


procedure TTinyCAN.CanSetStatusEventCallback(event_proc: PF_CanStatusEventCallback);
begin;
if Assigned(pmCanSetStatusEventCallback) then
  pmCanSetStatusEventCallback(event_proc);
end;


procedure TTinyCAN.CanSetRxEventCallback(event_proc: PF_CanRxEventCallback);
begin;
if Assigned(pmCanSetRxEventCallback) then
  pmCanSetRxEventCallback(event_proc);
end;


procedure TTinyCAN.CanSetEvents(events: TEventMasks);
var e: Word;

begin;
e := 0;
if PNP_CHANGE_EVENT in events then
  e := e or EVENT_ENABLE_PNP_CHANGE
else
  e := e or EVENT_DISABLE_PNP_CHANGE;
if STATUS_CHANGE_EVENT in events then
  e := e or EVENT_ENABLE_STATUS_CHANGE
else
  e := e or EVENT_DISABLE_STATUS_CHANGE;
if RX_FILTER_MESSAGES_EVENT in events then
  e := e or EVENT_ENABLE_RX_FILTER_MESSAGES
else
  e := e or EVENT_DISABLE_RX_FILTER_MESSAGES;
if RX_MESSAGES_EVENT in events then
  e := e or EVENT_ENABLE_RX_MESSAGES
else
  e := e or EVENT_DISABLE_RX_MESSAGES;
if Assigned(pmCanSetEvents) then
  pmCanSetEvents(e);
end;


function TTinyCAN.CanEventStatus: DWORD;

begin;
result := 1;
if Assigned(pmCanEventStatus) then
  result := pmCanEventStatus;
end;


{**************************************************************}
{* DLL Treiber laden                                          *}
{**************************************************************}
function TTinyCAN.LoadDriver: Integer;

begin;
try
  if DRVDLLWnd <> 0 then
    DownDriver;
  {Hardware Treiber laden}
  DRVDLLWnd:=LoadLibrary(PChar(GetApiDriverWithPath(FTreiberName)));
  if DRVDLLWnd=0 then raise EDllLoadError.create('');
  pmCanInitDriver := GetProcAddress(DrvDLLWnd, 'CanInitDriver');
  pmCanDownDriver := GetProcAddress(DrvDLLWnd, 'CanDownDriver');
  pmCanSetOptions := GetProcAddress(DrvDLLWnd, 'CanSetOptions');
  pmCanDeviceOpen := GetProcAddress(DrvDLLWnd, 'CanDeviceOpen');
  pmCanDeviceClose := GetProcAddress(DrvDLLWnd, 'CanDeviceClose');
  pmCanApplaySettings := GetProcAddress(DrvDLLWnd, 'CanApplaySettings');
  pmCanSetMode := GetProcAddress(DrvDLLWnd, 'CanSetMode');
  pmCanSet := GetProcAddress(DrvDLLWnd, 'CanSet');
  pmCanGet := GetProcAddress(DrvDLLWnd, 'CanGet');
  pmCanTransmit := GetProcAddress(DrvDLLWnd, 'CanTransmit');
  pmCanTransmitClear := GetProcAddress(DrvDLLWnd, 'CanTransmitClear');
  pmCanTransmitGetCount := GetProcAddress(DrvDLLWnd, 'CanTransmitGetCount');
  pmCanTransmitSet := GetProcAddress(DrvDLLWnd, 'CanTransmitSet');
  pmCanReceive := GetProcAddress(DrvDLLWnd, 'CanReceive');
  pmCanReceiveClear := GetProcAddress(DrvDLLWnd, 'CanReceiveClear');
  pmCanReceiveGetCount := GetProcAddress(DrvDLLWnd, 'CanReceiveGetCount');
  pmCanSetSpeed := GetProcAddress(DrvDLLWnd, 'CanSetSpeed');
  pmCanDrvInfo := GetProcAddress(DrvDLLWnd, 'CanDrvInfo');
  pmCanDrvHwInfo := GetProcAddress(DrvDLLWnd, 'CanDrvHwInfo');
  pmCanSetFilter := GetProcAddress(DrvDLLWnd, 'CanSetFilter');
  pmCanGetDeviceStatus := GetProcAddress(DrvDLLWnd, 'CanGetDeviceStatus');
  pmCanSetPnPEventCallback := GetProcAddress(DrvDLLWnd, 'CanSetPnPEventCallback');
  pmCanSetStatusEventCallback := GetProcAddress(DrvDLLWnd, 'CanSetStatusEventCallback');
  pmCanSetRxEventCallback := GetProcAddress(DrvDLLWnd, 'CanSetRxEventCallback');
  pmCanSetEvents := GetProcAddress(DrvDLLWnd, 'CanSetEvents');
  pmCanEventStatus := GetProcAddress(DrvDLLWnd, 'CanEventStatus');

  if @pmCanInitDriver = nil then raise EDllLoadError.create('');
  if @pmCanDownDriver = nil then raise EDllLoadError.create('');
  if @pmCanSetOptions = nil then raise EDllLoadError.create('');
  if @pmCanDeviceOpen = nil then raise EDllLoadError.create('');
  if @pmCanDeviceClose = nil then raise EDllLoadError.create('');
  if @pmCanApplaySettings = nil then raise EDllLoadError.create('');
  if @pmCanSetMode = nil then raise EDllLoadError.create('');
  if @pmCanSet = nil then raise EDllLoadError.create('');
  if @pmCanGet = nil then raise EDllLoadError.create('');
  if @pmCanTransmit = nil then raise EDllLoadError.create('');
  if @pmCanTransmitClear = nil then raise EDllLoadError.create('');
  if @pmCanTransmitGetCount = nil then raise EDllLoadError.create('');
  if @pmCanTransmitSet = nil then raise EDllLoadError.create('');
  if @pmCanReceive = nil then raise EDllLoadError.create('');
  if @pmCanReceiveClear = nil then raise EDllLoadError.create('');
  if @pmCanReceiveGetCount = nil then raise EDllLoadError.create('');
  if @pmCanSetSpeed = nil then raise EDllLoadError.create('');
  if @pmCanDrvInfo = nil then raise EDllLoadError.create('');
  if @pmCanDrvHwInfo = nil then raise EDllLoadError.create('');
  if @pmCanSetFilter = nil then raise EDllLoadError.create('');
  if @pmCanGetDeviceStatus = nil then raise EDllLoadError.create('');
  if @pmCanSetPnPEventCallback = nil then raise EDllLoadError.create('');
  if @pmCanSetStatusEventCallback = nil then raise EDllLoadError.create('');
  if @pmCanSetRxEventCallback = nil then raise EDllLoadError.create('');
  if @pmCanSetEvents = nil then raise EDllLoadError.create('');
  if @pmCanEventStatus = nil then raise EDllLoadError.create('');
  // Treiber Initialisieren
  if CanInitDriver <> 0 then raise EDllLoadError.create('');
  // Callback-Funktionen setzen
  CanSetPnPEventCallback(@CanPnPEventCallback);
  CanSetStatusEventCallback(@CanStatusEventCallback);
  CanSetRxEventCallback(@CanRxEventCallback);
  // Events freigeben
  CanSetEvents(FEventMasks);
  CanSetSpeed(0, FCanSpeed);
  // Treiber Optionen setzen
  CanSetOptions;

  if CanDeviceOpen <> 0 then
    result := -2
  else
    result := 0;
except
  DownDriver;
  result := -1;
  end;
end;


procedure TTinyCAN.DownDriver;

begin;
CanSetEvents([]);   // Alle Events löschen
while CanEventStatus = 0 do
  Application.ProcessMessages;
CanDownDriver;

pmCanInitDriver := nil;
pmCanDownDriver := nil;
pmCanSetOptions := nil;
pmCanDeviceOpen := nil;
pmCanDeviceClose := nil;
pmCanApplaySettings := nil;
pmCanSetMode := nil;
pmCanSet := nil;
pmCanGet := nil;
pmCanTransmit := nil;
pmCanTransmitClear := nil;
pmCanTransmitGetCount := nil;
pmCanTransmitSet := nil;
pmCanReceive := nil;
pmCanReceiveClear := nil;
pmCanReceiveGetCount := nil;
pmCanSetSpeed := nil;
pmCanDrvInfo := nil;
pmCanDrvHwInfo := nil;
pmCanSetFilter := nil;
pmCanGetDeviceStatus := nil;
pmCanSetPnPEventCallback := nil;
pmCanSetStatusEventCallback := nil;
pmCanSetRxEventCallback := nil;
pmCanSetEvents := nil;
pmCanEventStatus := nil;
if DrvDLLWnd<>0 then
  begin;
  FreeLibrary(DRVDLLWnd);
  DrvDLLWnd:=0;
  end;
end;


procedure Register;
begin
  RegisterComponents('MHS', [TTinyCAN]);
end;

end.

