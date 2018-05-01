program BikeInfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, laz_synapse, fmMain, BionX, CANAdapter,
  TinyCANAdapter, TinyCanDrv, fmCodes, CANInterface, BikeInfoIni;

{$R *.res}

begin
  Application.Scaled := True;
//  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmBionXMain, frmBionXMain);
  Application.Run;
end.

