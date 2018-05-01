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

unit CANAdapter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  // this is an abstract wrapper class for a CAN bus adapter.
  // It's methods are used by the bionxcomponents to access the bus.
  // If you have another adapter than the tinyCAN, you must write
  // your own descandant and implement at least the virtual methods below.
  // See the class TinyCANAdapter for example.

type
  TLogMsg = procedure ( const LogMsg : string ) of object;

type
  { TCANAdapter }
  PCANAdapter = ^TCANAdapter;
  TCANAdapter = class ( TObject )
  private
    FLogMsg : TLogMsg;
  protected
    procedure LogMsg ( const LogMsg : string );
    property OnLogMsg : TLogMsg read FLogMsg write FLogMsg;
  public
    function Connect : boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;

    function ReadByte ( Id : byte; Reg : byte ) : byte; virtual; abstract;
    procedure WriteByte ( Id : byte; Reg : byte; Value : byte ); virtual; abstract;
  end;


type
  ECANError = class ( Exception );

implementation

procedure TCANAdapter.LogMsg ( const LogMsg : string );
begin
  if assigned ( FLogMsg ) then
    FLogMsg ( LogMsg );
end;

end.

