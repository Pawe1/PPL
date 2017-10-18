unit Future1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  FMX.Controls.Presentation, FMX.StdCtrls,
  System.Threading;

type
  TfrmFuture = class(TForm)
    btnGetPrime: TButton;
    lbLog: TListBox;
    btnStartCalculation: TButton;
    btnGetValue: TButton;
    btnCalculateAndNotify: TButton;
    procedure btnCalculateAndNotifyClick(Sender: TObject);
    procedure btnGetPrimeClick(Sender: TObject);
    procedure btnGetValueClick(Sender: TObject);
    procedure btnStartCalculationClick(Sender: TObject);
  private
    FCalculation: IFuture<integer>;
    function CalcNthPrime(n: integer): integer;
  public
  end;

var
  frmFuture: TfrmFuture;

implementation

uses
  System.Diagnostics;

{$R *.fmx}

function IsPrime(i: integer): boolean;
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

procedure TfrmFuture.btnGetPrimeClick(Sender: TObject);
var
  candidate: Integer;
  numPrimes: Integer;
begin
  candidate := 1;
  numPrimes := 0;
  repeat
    if IsPrime(candidate) then
      Inc(numPrimes);
    Inc(candidate);
  until numPrimes = 200000;
  lbLog.Items.Add(Format('200000th prime is %d', [candidate]));
end;

function TfrmFuture.CalcNthPrime(n: integer): integer;
var
  candidate: Integer;
  numPrimes: Integer;
begin
  candidate := 1;
  numPrimes := 0;
  repeat
    if IsPrime(candidate) then begin
      Inc(numPrimes);
      if numPrimes = n then
        Exit(candidate);
    end;
    Inc(candidate);
  until false;
end;

procedure TfrmFuture.btnGetValueClick(Sender: TObject);
var
  start: TStopwatch;
begin
  start := TStopwatch.StartNew;
  lbLog.Items.Add(Format('200000th prime is %d; waiting %d ms', [FCalculation.Value, start.ElapsedMilliseconds]));
  FreeAndNil(start);
  FCalculation := nil;
end;

procedure TfrmFuture.btnStartCalculationClick(Sender: TObject);
begin
  FCalculation := TTask.Future<integer>(
    function: integer
    begin
      Result := CalcNthPrime(200000);
    end);
end;

procedure TfrmFuture.btnCalculateAndNotifyClick(Sender: TObject);
begin
  FCalculation := TTask.Future<integer>(
    function: integer
    begin
      Result := CalcNthPrime(200000);
      TThread.Queue(nil,
        procedure begin
          btnGetValueClick(nil);
        end);
    end);
end;


end.
