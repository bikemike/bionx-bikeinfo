{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit tinycan;

interface

uses
  TinyCanDrv, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TinyCanDrv', @TinyCanDrv.Register);
end;

initialization
  RegisterPackage('tinycan', @Register);
end.
