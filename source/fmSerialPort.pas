unit fmSerialPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TfrmSerialPort }

  TfrmSerialPort = class ( TForm )
    cbPortName: TComboBox;
    Label1: TLabel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure FormCreate ( Sender: TObject ) ;
  private
    { private declarations }
  public
    { public declarations }
  end;

function GetSerialPortname ( var PortName : string ) : boolean;

implementation
uses
  SynaSer;

{$R *.lfm}

function GetSerialPortname ( var PortName : string ) : boolean;
var
  F : TfrmSerialPort;
begin
  F := TfrmSerialPort.Create( Application.MainForm );
  try
    F.cbPortName.Text := PortName;
    if F.ShowModal = mrOK then
    begin
      PortName := F.cbPortName.Text;
      Result := true
    end
    else
      Result := false;
  finally
    F.Free;
  end;
  Application.ProcessMessages;
end;

{ TfrmSerialPort }

procedure TfrmSerialPort.FormCreate ( Sender: TObject ) ;
begin
  cbPortname.Items.DelimitedText := GetSerialPortNames;
end;

end.

