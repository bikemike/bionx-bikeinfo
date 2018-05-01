program BikeInfo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fmMain, BionX, CANAdapter, TinyCANAdapter, FileCANAdapter, TinyCanDrv,
  BionXProfiles
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmBionXMain, frmBionXMain);
  Application.Run;
end.

