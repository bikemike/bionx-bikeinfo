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

unit CANInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  CANAdapter;

type
  TLogMsg = procedure ( const LogMsg : string ) of object;
  TMsgDirection = ( md_rx, md_tx );
  TLogCANMessage = procedure ( Dir : TMsgDirection; Id, Len : DWORD; CANData : PCANData ) of object;

type
  PCANInterface = ^TCANInterface;
  TCANInterface = class ( TObject )
  private
    FLastError  : string;
    FCANAdapter : TCANAdapter;
    FDLLHandle  : THandle;
    FLogMsg        : TLogMsg;
    FLogCANMessage : TLogCANMessage;
    FCapturingQueueRunner : TThread;
    procedure LogMsg ( const Msg : string );
    procedure LogCANMessage ( Dir : TMsgDirection; Id, len : DWORD; CANData : PCANData );
    procedure MessageHandler ( Id : DWORD; Len : byte; CANData : PCANData );
    procedure InitCANData ( CANData : PCANData; Reg : byte; Value : byte = 0 );
    function WaitForReply ( ReplyId : DWORD; Reg : byte; out Value : byte; Timeout : DWORD ) : boolean;
  protected

  public
    constructor Create;
    constructor Create ( AdapterClass : TCANAdapterClass );
    constructor Create ( CANAdapter : TCANAdapter );
    constructor Create ( const DLLName : string );
    destructor Destroy; override;

    // high level routines called by TBionxBike and TBionXCompnents or main form
    function Connect : boolean;
    procedure Disconnect;

    function ReadByte ( Id : byte; Reg : byte; out Value : byte ) : boolean;
    function WriteByte ( Id : byte; Reg : byte; Value : byte ) : boolean;

    function StartMessageCapturing : boolean;
    procedure StopMessageCapturing;

    // midlevel routines. should be private or protected
    // but they are sometimes helpfull for debugging the adapter
    function SendCANMsg ( Id : DWORD; Len : byte; CANData : PCANData ) : boolean;
    function ReadCANMsg ( out Id : DWORD; out Len : byte; CANData : PCANData ) : boolean;

    property LastError : string read FLastError;

    property OnLogMsg : TLogMsg read FLogMsg write FLogMsg;
    property OnLogCANMessage : TLogCANMessage read FLogCANMessage write FLogCANMessage;
  end;

type
  ECANError = CANAdapter.ECANError;

implementation
uses
  DynLibs;


type
  TCreateAdapterProc = function (): TCANAdapter; stdcall;

type
  TBionXCANMsg = packed record
    Id   : DWORD;
    Len  : byte;
    Data : TCANData;
  end;
  PBionXCANMsg = ^TBionXCANMsg;

function XltMsgToStr ( Id : DWORD; Len : byte; CANData : PCANData ) : string;
begin
  Len := Len and $0F;
  case Len of
    2 :
      Result := Format ( 'Id=%0.2x, Len=%0.2d, Data=%0.2x %0.2x', [Id, Len, CANData^[0], CANData^[1]] );
    4 :
      Result := Format ( 'Id=%0.2x, Len=%0.2d, Data=%0.2x %0.2x %0.2x %0.2x', [Id, Len, CANData^[0], CANData^[1], CANData^[2], CANData^[3]] );
    else
      Result := Format ( 'Id=%0.2x, Flags=%0.4x, Data=%0.2x %0.2x %0.2x %0.2x %0.2x %0.2x %0.2x %0.2x', [Id, Len, CANData^[0], CANData^[1], CANData^[2], CANData^[3], CANData^[4], CANData^[5], CANData^[6], CANData^[7]] );
  end;
end;

(******************************************************************************)

// this Thread continuously tries to read the first messages from
// the fifo buffer and sends it to the main programs logging procedure
type
  TCapturingQueueRunner = class ( TThread )
  private
    FCANMsgFifo : TThreadlist;
    FCANIntf    : TCANInterface;
    FMsg        : TBionXCANMsg;
    procedure QueueMessage ( Id : DWORD; Len : byte; CANData : PCANData );
    function FetchMessage : boolean;
    procedure LogMessage;
  protected
    procedure Execute; override;
  public
    constructor Create ( aCANIntf : TCANInterface );
    destructor Destroy; override;
  end;

constructor TCapturingQueueRunner.Create ( aCANIntf : TCANInterface );
begin
  inherited Create ( false );
  FCANMsgFifo := TThreadlist.Create;
  FCANIntf    := aCANIntf;
end;

destructor TCapturingQueueRunner.Destroy;
var
  List : TList;
  i    : integer;
begin
  // free memory of any left entry in fifo

  List := FCANMsgFifo.Locklist;
  try
    for i := 0 to List.Count-1 do
      FreeMem ( List.Items[i] );
  finally
    FCANMsgFifo.Unlocklist;
  end;

  FCANMsgFifo.Free;
  inherited;
end;

// append a new CAN message to the fifo
procedure TCapturingQueueRunner.QueueMessage ( Id : DWORD; Len : byte; CANData : PCANData );
var
  pMsg : PBIONXCANMsg;
begin
  pMsg := GetMem ( sizeof(TBIONXCANMsg) );
  pMsg^.Id := Id;
  pMsg^.Len := Len;
  pMsg^.Data := CANData^;
  // just Add is ok, it handles the locking itself
  FCANMsgFifo.Add(pMsg);
end;

// read and remove the first message from fifo
function TCapturingQueueRunner.FetchMessage : boolean;
var
  pMsg : PBIONXCANMsg;
  List : TList;
begin
  List := FCANMsgFifo.Locklist;
  try
    if List.Count > 0 then
    begin
      // copy message int a private variable
      pMsg := List.Items[0];
      FMsg := pMsg^;
      // remove message and free its memory
      List.Delete(0);
      FreeMem ( pMsg );
      Result := true;
    end
    else
      Result := false;
  finally
    FCANMsgFifo.Unlocklist;
  end;
end;

// forward the fetched message to then Interface logging method
// Since the message is probably sent to a GUI control, this method is
// called via Synchronize from the threads main loop. It is not allowed
// to access controls from any other than the main programs thread.
// Synchronize handles this for us
procedure TCapturingQueueRunner.LogMessage;
begin
  FCANIntf.LogCANMessage ( md_rx, FMsg.Id, FMsg.Len, @FMsg.Data );
end;

procedure TCapturingQueueRunner.Execute;
begin
  while not Terminated do
  begin
    if FetchMessage then
      Synchronize ( @LogMessage )
    else
      sleep(1);
  end;
end;

(******************************************************************************)

constructor TCANInterface.Create;
begin
  inherited;
  FCANAdapter := nil;
  FDLLHandle  := NilHandle;
end;

constructor TCANInterface.Create ( CANAdapter : TCANAdapter );
begin
  Create;
  FCANAdapter := CANAdapter;
end;

// create an adaapter by its class
constructor TCANInterface.Create ( AdapterClass : TCANAdapterClass );
begin
  Create;
  FCANAdapter := AdapterClass.Create;
end;

// create an adapter from DLL
constructor TCANInterface.Create ( const DLLName : string );
var
  CreateAdapterProc : TCreateAdapterProc;
begin
  FDLLHandle := LoadLibrary ( dllName );
  if FDLLHandle <> NilHandle then
  begin
    pointer(CreateAdapterProc) := GetProcedureAddress ( FDLLHandle, 'CreateAdapter' );
    if assigned ( CreateAdapterProc ) then
    begin
      FCANAdapter := CreateAdapterProc()
    end
    else
    begin
      UnloadLibrary ( FDLLHandle );
      FDLLHandle := NilHandle;
      raise Exception.CreateFmt ( 'could not find Entrypoint in %s', [dllName] );
    end;
  end
  else
    raise Exception.CreateFmt ( 'could not load %s', [dllName] );
end;

destructor TCANInterface.Destroy;
begin
  // free adapter
  if assigned ( FCANAdapter ) then
    FCANAdapter.Free;
  // unload DLL, if exists
  if FDLLHandle <> NilHandle then
    UnloadLibrary ( FDLLHandle );
  inherited;
end;

procedure TCANInterface.LogMsg ( const Msg : string );
begin
  if assigned ( FLogMsg ) then
    FLogMsg ( Msg );
end;

procedure TCANInterface.LogCANMessage ( Dir : TMsgDirection; Id, len : DWORD; CANData : PCANData );
begin
  if assigned ( FLogCANMessage ) then
    FLogCANMessage ( Dir, Id, Len, CANData )
  else
    if assigned ( FLogMsg ) then
    begin
      case Dir of
        md_rx :
          LogMsg ( 'Rx: ' + XltMsgToStr( Id, len, CANData ));
        md_tx :
          LogMsg ( 'Tx: ' + XltMsgToStr( Id, len, CANData ));
      end;
    end;
end;

procedure TCANInterface.MessageHandler ( Id : DWORD; Len : byte; CANData : PCANData );
begin
  if assigned ( FCapturingQueueRunner ) then
    TCapturingQueueRunner(FCapturingQueueRunner).QueueMessage( Id, Len, CANData );
end;

// setup candata with register and value
procedure TCANInterface.InitCANData ( CANData : PCANData; Reg : byte; Value : byte = 0 );
begin
  fillchar ( CANData^, sizeof ( TCANData ), 0 );
  CANData^[1] := Reg;
  CANData^[3] := Value;
end;

// wait for a reply for a limited time
// the received CAN message must match ReplyId and Reg
// return true, if a matching reply message was found
function TCANInterface.WaitForReply ( ReplyId : DWORD; Reg : byte; out Value : byte; Timeout : DWORD ) : boolean;
var
  TimeoutTime : TTime;
  Id : DWORD;
  Len : byte;
  CANData : TCANData;
begin
  Result := false;
  TimeoutTime := Now + Timeout * MilliSecond;

  while ( TimeoutTime > Now ) and ( not Result ) do
  begin
    if ReadCANMsg ( Id, Len, @CANData ) then
    begin
      Result := ( ((Len and $0F) = 4) and (CANData[1] = Reg) and ( Id = ReplyId ) );
      if Result then
        Value := CANData[3];
    end
    else
      Sleep ( 1 );
  end;
end;

// connect the adapter
// return true on success
function TCANInterface.Connect: boolean;
begin
  FLastError := '';
  try
    Result := FCANAdapter.Connect;
  except
    on E:Exception do
    begin
      FLastError := E.Message;
      Result := false;
    end;
  end;
end;

// disconnect adapter
procedure TCANInterface.Disconnect;
begin
  FLastError := '';
  try
    StopMessageCapturing;
    FCANAdapter.Disconnect;
  except
    on E:Exception do
    begin
      FLastError := E.Message;
    end;
  end;
end;

// do the logging and send message via adapter
// return true on success
function TCANInterface.SendCANMsg ( Id : DWORD; Len : byte; CANData : PCANData ) : boolean;
begin
  LogCANMessage ( md_tx, Id, Len, CANData );
  Result := FCANAdapter.SendCANMsg( Id, Len, CANData );
end;

// try to read a message via adapter
// return true when a message was available and do the logging
function TCANInterface.ReadCANMsg ( out Id : DWORD; out Len : byte; CANData : PCANData ) : boolean;
begin
  Result := FCANAdapter.ReadCANMsg( Id, Len, CANData );
  if Result then
    LogCANMessage ( md_rx, Id, Len, CANData );

end;

// high level routine to read a value from a node / register
// send a request message to the node id and wait for a reply
// return true, when the node has send its registers data
function TCANInterface.ReadByte ( Id : byte; Reg : byte; out Value : byte ) : boolean;
var
  CANData : TCANData;
  RxId    : byte;
begin
  FLastError := '';
  try
    // ugly hack:
    // BionX replies requests to console ( $48 ) to the BiB ( $58 )
    // Requests to battery and motor are answered to Id $08
    // We should use the const from BionX here, but they are
    // private to the BionX unit :(
    if Id = $48 then
      RxId := $58
    else
      RxId := $08;

    // setup CAN data for a request message to Reg
    InitCANData ( @CANData, Reg );

    try
      // cleanup RX buffers before reading any reply
      FCANAdapter.ClearCANRxBuffer;

      // send request message to the bus
      if SendCANMsg ( Id, 2, @CANData ) then
      begin
        // wait for a matching reply
        if WaitForReply ( RxId, Reg, Value, 1500 ) then
        begin
          Result := true;
        end
        else
          raise ECANError.CreateFmt ( 'no response from node %0.2x', [Id] );
      end
      else
        raise ECANError.CreateFmt ( 'could not send request to node %0.2x', [Id] );
    except
      on E:Exception do
        raise ECANError.CreateFmt ( 'could not read register %0.2x from node %0.2x'#13+E.Message, [Reg, Id] );
    end;
  except
    on E:Exception do
    begin
      FLastError := E.Message;
      Result := false;
    end;
  end;
end;

// high level routine to write a value to a node / register
// send a message with the new value to the node id / register
// return true on success
function TCANInterface.WriteByte ( Id : byte; Reg : byte; Value : byte ) : boolean;
var
  CANData : TCANData;
begin
  FLastError := '';
  try
    try
      // setup CAN data for a set message to write Value to Reg
      InitCANData ( @CANData, Reg, Value );
      if SendCanMsg ( Id, 4, @CANData ) then
        Result := true
      else
        raise ECANError.CreateFmt ( 'could not send value to node %0.2x', [Id]);
    except
      on E:Exception do
        raise ECANError.CreateFmt ( 'could not write register %0.2x to node %0.2x'#13+E.Message, [Reg, Id] );
    end;
  except
    on E:Exception do
    begin
      FLastError := E.Message;
      Result := false;
    end;
  end;
end;

function TCANInterface.StartMessageCapturing : boolean;
begin
  FCapturingQueueRunner := TCapturingQueueRunner.Create ( self );
  Result := FCANAdapter.StartCapturing ( @MessageHandler );
  if not Result then
    StopMessageCapturing;
end;

procedure TCANInterface.StopMessageCapturing;
begin
  FCANAdapter.StopCapturing;

  if assigned ( FCapturingQueueRunner ) then
  begin
    FCapturingQueueRunner.Terminate;
    FCapturingQueueRunner.WaitFor;
    FreeAndNil(FCapturingQueueRunner);
  end;

end;

end.

