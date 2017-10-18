unit Map1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  OtlParallel;

type
  TfrmMap = class(TForm)
    btnConvertSerial: TButton;
    btnConvertParallel: TButton;
    procedure btnConvertSerialClick(Sender: TObject);
    procedure btnConvertParallelClick(Sender: TObject);
  private
    procedure InitializeData(var numbers: TArray<integer>);
    function MapOdds(const source: integer; var dest: string): boolean;
    procedure ShowResults(const result: TArray<string>);
  public
  end;

var
  frmMap: TfrmMap;

implementation

{$R *.dfm}

uses
  DSiWin32;

const
  CSourceSize = 1000000;

procedure TfrmMap.btnConvertParallelClick(Sender: TObject);
var
  numbers: TArray<integer>;
  odds   : TArray<string>;
begin
  InitializeData(numbers);

  // implement parallel version
  odds := Parallel.Map<integer, string>
    .NumTasks(4)
    .Source(numbers)
    .Execute(MapOdds)
    .Result;

  ShowResults(odds);
end;

procedure TfrmMap.btnConvertSerialClick(Sender: TObject);
var
  i,j    : integer;
  numbers: TArray<integer>;
  odds   : TArray<string>;
begin
  InitializeData(numbers);

  SetLength(odds, Length(numbers));
  j := Low(odds);
  for i := Low(numbers) to High(numbers) do
  begin
    if MapOdds(numbers[i], odds[j]) then
      Inc(j);
  end;
  SetLength(odds, j);

  ShowResults(odds);
end;

procedure TfrmMap.InitializeData(var numbers: TArray<integer>);
var
  i: integer;
begin
  SetLength(numbers, CSourceSize);
  for i := Low(numbers) to High(numbers) do
    numbers[i] := i;
end;

function TfrmMap.MapOdds(const source: integer; var dest: string): boolean;
begin
  Result := Odd(source);
  if Result then
    dest := IntTostr(source);
end;

procedure TfrmMap.ShowResults(const result: TArray<string>);
begin
  ShowMessageFmt('%d values, first is %s and last is %s',
    [Length(result), result[Low(result)], result[High(result)]]);
end;

end.
