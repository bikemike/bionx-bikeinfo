unit fmCodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls;

type

  { TCodes }

  TCodes = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbCodes: TCheckGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click ( Sender: TObject ) ;
  private
    { private declarations }
    FCodes : longword;
    FCodesRW : longword;
  public
    { public declarations }
  end;

var
  Codes: TCodes;

function EditCodes ( AParent : TComponent; var ACodes, ACodesRW : longword ) : boolean;

implementation
uses
  BionX;

{$R *.lfm}

function EditCodes ( AParent : TComponent; var ACodes, ACodesRW : longword ) : boolean;
var
  F : TCodes;
begin
  F := TCodes.Create ( AParent );
  try
    F.FCodes := ACodes;
    F.FCodesRW := ACodesRW;
    F.ShowModal;
    Result := F.ModalResult = mrOK;
    ACodes := F.FCodes;
    ACodesRW := F.FCodesRW;
  finally
    F.Free;
  end;
end;

{ TCodes }

procedure TCodes.FormShow(Sender: TObject);
var
  i : byte;
  Mask : longword;
begin
  Mask := 1;
  cbCodes.Items.Clear;
  for i := 0 to 31 do
  begin
    cbCodes.Items.Add ( '' );
    cbCodes.Items.Add ( ConsoleCodeBitString ( i ));
    cbCodes.Checked[i*2] := FCodes and Mask <> 0;
    cbCodes.Checked[i*2+1] := FCodesRW and Mask <> 0;
    Mask := Mask shl 1;
  end;
end;

procedure TCodes.FormCreate(Sender: TObject);
begin
  ScaleBy ( Screen.PixelsPerInch, 96 );
end;

procedure TCodes.BitBtn1Click ( Sender: TObject ) ;
var
  i : byte;
  Mask : longword;
begin
  // keep the upper 3 unused bits unchanged
  FCodes := FCodes and $E0000000;
  FCodesRw := FCodesRW and $E0000000;
  Mask := 1;
  for i := 0 to 28 do
  begin
    if cbCodes.Checked[i*2] then
      FCodes := FCodes or Mask;
    if cbCodes.Checked[i*2+1] then
      FCodesRW := FCodesRW or Mask;
    Mask := Mask shl 1;
  end;
end;

end.

