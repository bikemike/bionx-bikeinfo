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

unit FileCANAdapter;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  Dialogs,
  CANAdapter;

// file adapter class used for debugging the programm
// when no real hardware is connected.
// Together with the SaveToFile button it's possible to
// have some usefull values for testing.
type
  TFileCANAdapter = class ( TCANAdapter )
  private
    FMem : array[0..255, 0..255] of byte;
    FFilename : string;
  protected
  public
    function Connect : boolean; override;
    procedure Disconnect; override;

    function ReadByte ( Id : byte; Reg : byte ) : byte; override;
    procedure WriteByte ( Id : byte; Reg : byte; Value : byte ); override;

    property Filename : string read FFilename write FFilename;
  end;


implementation


(******************************************************************************)

function TFileCANAdapter.Connect : boolean;
var
  od : TOpenDialog;
  fs : TFileStream;
begin
  fillchar ( FMem, sizeof ( FMem ), 0 );

  od := TOpenDialog.Create(nil);
  try
    od.Title := 'Select file';
    od.Options := od.Options + [ofHideReadOnly];
    od.DefaultExt:= '.can';
    od.Filter := 'CanData (*.can)|*.can';
    if od.Execute then
    begin
      FFilename := od.Filename;
      if fileexists ( FFilename ) then
      begin
        fs := TFileStream.Create ( FFilename, fmOpenReadWrite );
        try
          fs.Read ( FMem, sizeof ( FMem ) );
        finally
          fs.Free;
        end;
      end;
      Result := true;
    end
    else
      Result := false;
  finally
    od.Free;
  end;
end;

procedure TFileCANAdapter.Disconnect;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create ( FFilename, fmCreate );
  try
    fs.Write ( FMem, sizeof ( FMem ) );
  finally
    fs.Free;
  end;
end;

function TFileCANAdapter.ReadByte ( Id : byte; Reg : byte ) : byte;
begin
  Result := FMem[Id,Reg];
end;

procedure TFileCANAdapter.WriteByte ( Id : byte; Reg : byte; Value : byte );
begin
  FMem[Id,Reg] := Value;
end;

end.

