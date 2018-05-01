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

unit TinyCANAdapter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  TinyCANDrv,
  CANAdapter;


  // implementation of a CAN bus driver wrapper using the TinyCAN with it's
  // library dll as adapter.
  // Write your own descandant, when using other hardware. Implement at least
  // the virtual abstract methods Connect, Disconnect, ReadByte and WriteByte
type
  TTinyCANAdapter = class ( TCANAdapter )
  private
    FTinyCAN : TTinyCAN;
    FMessageHandler : TCANMessageHandler;
    procedure TinyCanRxDEvent (Sender: TObject; index: DWORD; msg: PCanMsg; count: Integer);

  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    function Connect : boolean; override;
    procedure Disconnect; override;
    procedure ClearCANRxBuffer; override;
    function SendCANMsg ( Id : DWORD; Len : byte; CANData : PCANData ) : boolean; override;
    function ReadCANMsg ( out Id : DWORD; out Len : byte; CANData : PCANData ) : boolean; override;

    function StartCapturing ( aMessageHandler : TCANMessageHandler ) : boolean; override;
    procedure StopCapturing; override;
  end;

implementation
uses
  SysUtils;


constructor TTinyCANAdapter.Create;
begin
  inherited Create;
  FTinyCAN := TTinyCAN.Create ( nil );
end;

destructor TTinyCANAdapter.Destroy;
begin
  FTinyCAN.Free;
  inherited;
end;

function TTinyCANAdapter.Connect : boolean;
var
  errcode : integer;
  status : TDeviceStatus;
begin
  Result := false;
  errcode := FTinyCAN.LoadDriver;
  if errcode >= 0 then
  begin
    try
      // start CAN bus, clear all FIFO and Errors
      errcode := FTinyCAN.CanSetMode( 0, OP_CAN_START, CAN_CMD_ALL_CLEAR );
      if errcode = 0 then
      begin
        errcode := FTinyCAN.CanGetDeviceStatus(0, @status);
        if errcode = 0 then
        begin
          // enshure the device is open
          if (Status.DrvStatus >= DRV_STATUS_CAN_OPEN) then
          begin
            if (Status.CanStatus = CAN_STATUS_BUS_OFF) then
              FTinyCAN.CanSetMode(0, OP_CAN_RESET, CAN_CMD_ALL_CLEAR);
            Result := true;
          end
          else
            raise ECANError.Create ( 'cannot not open device' );
        end
        else
          raise ECANError.CreateFmt ( 'cannot get device status', [errcode] );
      end
      else
        raise ECANError.CreateFmt ( 'cannot start CAN bus', [errcode] );
    except
      Disconnect;
      raise;
    end;
  end
  else
  begin
    if errcode = -2 then
      raise ECANError.Create ( 'cannot open port' )
    else
      if errcode = -1 then
        raise ECANError.Create ( 'cannot load driver dll' );
  end;
end;

procedure TTinyCANAdapter.Disconnect;
begin
  FTinyCAN.CanSetMode( 0, OP_CAN_STOP, CAN_CMD_ALL_CLEAR );
  FTinyCAN.DownDriver;
end;

procedure TTinyCANAdapter.ClearCANRxBuffer;
begin
  // the TinyCAN allows this smarter way to clear RX buffers
  FTinyCAN.CanSetMode( 0, OP_CAN_NONE, CAN_CMD_RXD_FIFOS_CLEAR );
  FTinyCAN.CanSetMode( 0, OP_CAN_NONE, CAN_CMD_RXD_OVERRUN_CLEAR );
//  inherited;
end;

function TTinyCANAdapter.SendCANMsg ( Id : DWORD; Len : byte; CANData : PCANData ) : boolean;
var
  TimeoutTime : TDateTime;
  errcode     : integer;
  TinyCANMsg  : TCANMsg;
begin
  Result := false;
  TimeoutTime := Now + 500 * MilliSecond;
  // note:
  // previous dll versions returned 0 on CanTransmit success,
  // from 4.09 the function returns the number of messages sent,
  // 0, if the TX-FIFO is full or an errorcode < 0
  // Therefore, we wait for the FIFO emtpty and then may test the
  // CanTransmit success >= 0 on all dll versions
  // >0 will be true with new dll
  // =0 will be true with old dll
  while ( FTinyCAN.CanTransmitGetCount(0) > 0 ) and
        ( TimeoutTime > Now )  do
    sleep ( 1 );

  fillchar ( TinyCANMsg, sizeof ( TCANMsg ), 0 );
  TinyCANMsg.Id := Id;
  TinyCANMsg.Flags := Len;
  TinyCANMsg.Data.Bytes := CANData^;

  errcode := FTinyCAN.CanTransmit ( 0, @TinyCANMsg, 1 );
  if ( errcode >= 0 ) then
  begin
    repeat
      sleep ( 1 );
      Result := FTinyCAN.CanTransmitGetCount(0) = 0;
    until Result or ( TimeoutTime < Now );
  end
  else
    raise ECANError.CreateFmt ( 'cannot transmit message (%d)', [errcode] );
end;


function TTinyCANAdapter.ReadCANMsg ( out Id : DWORD; out Len : byte; CANData : PCANData ) : boolean;
var
  TinyCANMsg  : TCANMsg;
  errcode     : integer;
begin
  Result := false;
  if (FTinyCAN.CanReceiveGetCount(0) > 0) then
  begin
    errcode := FTinyCAN.CanReceive(0, @TinyCANMsg, 1);
    if errcode = 1 then
    begin
      Id := TinyCANMsg.Id;
      Len := TinyCANMsg.Flags and FlagsCanLength;
      CANData^ := TinyCANMsg.Data.Bytes;
//      LogCANMessage ( md_rx, Id, Len, CANData );
      Result := true;
    end
    else
      raise ECANError.CreateFmt ( 'cannot receive message (%d)', [errcode] );
  end;
end;

procedure TTinyCANAdapter.TinyCanRxDEvent (Sender: TObject; index: DWORD; msg: PCanMsg; count: Integer);
var
  i : integer;
begin
  if index = 0 then
  begin
    for i:=1 to count do
    begin
      FMessageHandler ( msg^.Id, msg^.Flags and $0F, @msg^.Data );
      inc(msg);
    end;
  end
end;


function TTinyCANAdapter.StartCapturing ( aMessageHandler : TCANMessageHandler ) : boolean;
begin
  FMessageHandler := aMessageHandler;
  FTinyCAN.OnCanRxDEvent:= @TinyCanRxDEvent;
  FTinyCAN.CanSetEvents( [RX_MESSAGES_EVENT] );
  Result := true;
end;


procedure TTinyCANAdapter.StopCapturing;
begin
  FTinyCAN.CanSetEvents( [] );
  FTinyCAN.OnCanRxDEvent:= nil;
end;



end.

