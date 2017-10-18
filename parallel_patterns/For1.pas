unit For1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmFor = class(TForm)
    btnCountSerial: TButton;
    btnCountParallel: TButton;
    procedure btnCountSerialClick(Sender: TObject);
    procedure btnCountParallelClick(Sender: TObject);
  private
    function IsPrime(i: integer): boolean;
  public
  end;

var
  frmFor: TfrmFor;

implementation

{$R *.dfm}

uses
  System.Threading,
  System.SyncObjs;

const
  CHigh = 3080970;

procedure TfrmFor.btnCountParallelClick(Sender: TObject);
var
  i        : integer;
  numPrimes: integer;
begin
  numPrimes := 0;
  TParallel.For(1, CHigh,
    procedure (i: integer)
    begin
      if IsPrime(i) then
        TInterlocked.Increment(numPrimes);
    end);

  ShowMessageFmt('%d primes from 1 to %d', [numPrimes, CHigh]);
end;

procedure TfrmFor.btnCountSerialClick(Sender: TObject);
var
  i        : integer;
  numPrimes: integer;
begin
  numPrimes := 0;
  for i := 1 to CHigh do
    if IsPrime(i) then
      Inc(numPrimes);

  ShowMessageFmt('%d primes from 1 to %d', [numPrimes, CHigh]);
end;

function TfrmFor.IsPrime(i: integer): boolean;
var
  j: integer;
begin
  Result := false;
  if i <= 1 then
    Exit;
  for j := 2 to Round(Sqrt(i)) do
    if (i mod j) = 0 then
      Exit;
  Result := true;
end;

end.
