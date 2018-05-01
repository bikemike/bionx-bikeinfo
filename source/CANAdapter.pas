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

unit CANAdapter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

  // this is a partly abstract wrapper class for a CAN bus adapter.
  // It's methods are used by the bionxcomponents to access the bus.
  // If you have another adapter than the tinyCAN, you must write
  // your own descandant and implement at least the abstract methods below.
  // See the class TinyCANAdapter for example.

type
  TCANData = array[0..7] of byte;
  PCANData = ^TCANData;

type
  TCANMessageHandler = procedure ( Id : DWORD; Len : byte; CANData : PCANData ) of object;

type
  { TCANAdapter }
  TCANAdapter = class ( TObject )
  private
    FCaptureThread : TThread;
  public
    // make Create virtual to get the CANInterface.Create ( AdapterClass : TCANAdapterClass ) working
    constructor Create; virtual;
    // override at least the abstract methods in your own adapter implementation
    function Connect : boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure ClearCANRxBuffer; virtual;
    function SendCANMsg ( Id : DWORD; Len : byte; CANData : PCANData ) : boolean; virtual; abstract;
    function ReadCANMsg ( out Id : DWORD; out Len : byte; CANData : PCANData ) : boolean; virtual; abstract;

    function StartCapturing ( aMessageHandler : TCANMessageHandler ) : boolean; virtual;
    procedure StopCapturing; virtual;
  end;


type
  ECANError = class ( Exception );

type
  TCANAdapterClass = class of TCANAdapter;

const
  MilliSecond = 1.0 / MSecsPerDay;

implementation

type
  // this Thread continuously reads messages from the adapter
  // and appends them into the fifo buffer
  TCaptureThread = class ( TThread )
  private
    FCANAdapter        : TCANAdapter;
    FCANMessageHandler : TCANMessageHandler;
  protected
    procedure Execute; override;
  public
    constructor Create ( CANAdapter : TCANAdapter; MsgHandler : TCANMessageHandler );
  end;

constructor TCaptureThread.Create ( CANAdapter : TCANAdapter; MsgHandler : TCANMessageHandler );
begin
  inherited Create ( false );
  FCANAdapter := CANAdapter;
  FCANMessageHandler := MsgHandler;
end;

procedure TCaptureThread.Execute;
var
  Id   : DWORD;
  Len  : byte;
  Data : TCANData;
begin
  while not Terminated do
  begin
    if FCANAdapter.ReadCANMsg ( Id, Len, @Data ) then
      FCANMessageHandler ( Id, Len, @Data )
    else
      sleep(1);
  end;
end;

(******************************************************************************)

constructor TCANAdapter.Create;
begin
  inherited;
  FCaptureThread := nil;
end;

procedure TCANAdapter.ClearCANRxBuffer;
var
  Id      : DWORD;
  Len     : byte;
  CANData : TCANData;
begin
  while ReadCANMsg ( Id, Len, @CANData ) do
    ;
end;

function TCANAdapter.StartCapturing ( aMessageHandler : TCANMessageHandler ) : boolean;
begin
  FCaptureThread := TCaptureThread.Create ( self, aMessageHandler );
  Result := true;
end;

procedure TCANAdapter.StopCapturing;
begin
  if assigned ( FCaptureThread ) then
  begin
    FCaptureThread.Terminate;
    FCaptureThread.WaitFor;
    FreeAndNil(FCaptureThread);
  end;
end;

end.

