{ Copyright (C) 2018 Thorsten Schmidt (tschmidt@ts-soft.de)              }
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

unit XLBCANAdapter;
interface
uses
  SynaSer,
  CANAdapter;

type
  TXLBCANAdapter = class ( TCANAdapter )
  private
    FSerialPort : TBlockSerial;
    FFirmwareVersion : integer;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    function Connect : boolean; override;
    procedure Disconnect; override;
    procedure ClearCANRxBuffer; override;
    function SendCANMsg ( Id : DWORD; Len : byte; CANData : PCANData ) : boolean; override;
    function ReadCANMsg ( out Id : DWORD; out Len : byte; CANData : PCANData ) : boolean; override;
  end;

implementation
uses
  SysUtils,
  IniFiles,
  fmSerialPort;

const
  XLBPreamble : array[0..1] of char = 'CM';

const
  section_XLBCAN            = 'XLBCAN';
    entry_Port              = 'Port';
    default_Port            = 'COM1';
type
  TXLBCANMsg = packed record
    Id   : DWORD;
    Len  : byte;
    Data : TCANData;
  end;

constructor TXLBCANAdapter.Create;
begin
  inherited;
  FSerialPort := TBlockSerial.Create;
end;

destructor TXLBCANAdapter.Destroy;
begin
  FSerialPort.Free;
  inherited;
end;

function TXLBCANAdapter.Connect : boolean;
var
  IniFile : TIniFile;
  PortId : string;
  s : string;
begin
  Result := false;
  IniFile := TIniFile.Create ( GetAppConfigFile(true) );
  try
    PortId := IniFile.ReadString ( section_XLBCAN, entry_Port, default_Port );
    if GetSerialPortname ( PortId ) then
//    if InputQuery( 'Select serial port', 'Port', PortId ) then
    begin
      IniFile.WriteString ( section_XLBCAN, entry_Port, PortId );

      FSerialPort.Connect ( PortId );
      if FSerialPort.LastError = 0 then
      begin
        // setup serial parameters
        FSerialPort.Config ( 115200, 8, 'N', SB1, false, False);
        if FSerialPort.LastError = 0 then
        begin
          // the connect has reseted the Arduino
          // give it some time to restart and send
          // the Init Message
          if FSerialPort.CanRead(5000) then
            ;

          // read the Init message
          s := FSerialPort.RecvString( 200 );
          if s <> 'XLB Adapter ready!' then
            raise Exception.CreateFmt ( 'Adapter reported: %s', [ s ] );

          // request the Firmware Version
          FSerialPort.SendString ( 'V' );
          s := FSerialPort.RecvString( 200 );
          FFirmwareVersion := StrToIntDef ( s, 0 );

          // enter gateway mode
          FSerialPort.SendString ( 'G' );

          // read the gateway mode message
          s := FSerialPort.RecvString( 1000 );
          Result := s = 'start gateway mode';
        end
        else
          raise Exception.CreateFmt ( 'configuration of port %s failed'#13'%s', [ PortId, FSerialPort.LastErrorDesc] );
      end
      else
        raise Exception.CreateFmt ( 'could not open serial port %s'#13'%s', [ PortId, FSerialPort.LastErrorDesc] );
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TXLBCANAdapter.Disconnect;
begin
  // terminate bridge mode
  if FSerialPort.InstanceActive then
  begin
    FSerialPort.SendString( 'Q' );
    // clear the 'end gateway mode' message from serial buffer
    FSerialPort.RecvString( 1000 );
  end;
end;

procedure TXLBCANAdapter.ClearCANRxBuffer;
begin
  // clean the serial buffer
  FSerialPort.Purge;
end;

function TXLBCANAdapter.SendCANMsg ( Id : DWORD; Len : byte; CANData : PCANData ) : boolean;
var
  Msg : TXLBCANMsg;
begin
  Msg.Id := Id;
  Msg.Len := Len;
  Msg.Data := CANData^;
  FSerialPort.SendString( 'wuppdich' )         ;
  Result := ( FSerialPort.SendBuffer( @XLBPreamble,sizeof(XLBPreamble)) = sizeof(XLBPreamble )) and
            ( FSerialPort.SendBuffer( @Msg, sizeof ( TXLBCANMsg ) ) = sizeof ( TXLBCANMsg ) );
end;

function TXLBCANAdapter.ReadCANMsg ( out Id : DWORD; out Len : byte; CANData : PCANData ) : boolean;
var
  Msg : TXLBCANMsg;
  l : integer;

  // read next char from serial
  // return true, if this is the awaited char
  function RecvChar ( WaitChar : char ) : boolean;
  var
    InChar : char;
  begin
    if ( FSerialPort.RecvBufferEx( @InChar, sizeof ( InChar ), 0 ) = sizeof ( InChar ) ) then
      Result := InChar=WaitChar
    else
      Result := false;
  end;

begin
  l := FSerialPort.WaitingData;
  // wait for enough bytes for a complete XLBCANMsg + preamble
  if l{FSerialPort.WaitingData} >= sizeof(TXLBCANMsg)+sizeof(XLBPreamble) then
  begin
    // now verify preamble
    if RecvChar (XLBPreamble[0] ) then
    begin
      if RecvChar (XLBPreamble[1] ) then
      begin
        // preamble ok, read the message
        // the timeout may probably set to 0, since we made sure, we have enough
        // byte in buffer above. But is doesn't bother as the Recv will return
        // immediately in this case.
        Result := FSerialPort.RecvBufferEx( @Msg, sizeof ( TXLBCANMsg ), 100 ) = sizeof ( TXLBCANMsg );
        if Result then
        begin
          // pass the message data into the out params
          Id := Msg.Id;
          Len := Msg.Len;
          CANData^ := Msg.Data;
        end;
      end
      else
        Result := false;
    end
    else
      Result := false;
  end
  else
    Result := false;
end;


end.
