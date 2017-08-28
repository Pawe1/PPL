unit UnitFormMain;

{===============================================================================
 Demo for TThreadedQueue

 This code shows how to use a TThreadedQueue in a Producer - Consumer pattern.

 You would normally use a Master thread to start the production and
 consumer tasks as well (Master-Worker), but in this sample we use TTimers
 to do that part of the work.

 Author: Danny Wind
 License: Creative Commons CC-BY
===============================================================================}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Generics.Collections;

type
  TItemPair = TPair<string, Int64>;
  TFormMain = class(TForm)
    TrackBarProduction: TTrackBar;
    Label1: TLabel;
    TrackBarConsumption: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelOutOfStock: TLabel;
    LabelSold: TLabel;
    LabelMade: TLabel;
    TimerProduction: TTimer;
    TimerConsumption: TTimer;
    Label6: TLabel;
    LabelOverStock: TLabel;
    TimerDisplay: TTimer;
    Label7: TLabel;
    LabelInStock: TLabel;
    LabelProdPerSec: TLabel;
    LabelConsPerSec: TLabel;
    procedure TrackBarProductionChange(Sender: TObject);
    procedure TrackBarConsumptionChange(Sender: TObject);
    procedure TimerProductionTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerConsumptionTimer(Sender: TObject);
    procedure TimerDisplayTimer(Sender: TObject);
  private
    { Private declarations }
    ItemsMade, ItemsOverStock: Int64; {Production}
    ItemsSold, ItemsOutOfStock: Int64; {Consumption}
    ItemsQueue : TThreadedQueue<TItemPair>;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.SyncObjs, System.Threading;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {Maximum stock is 1024, wait time is 10 ms}
  ItemsQueue := TThreadedQueue<TItemPair>.Create(1024, 10, 10);
end;

procedure TFormMain.TimerConsumptionTimer(Sender: TObject);
begin
  TTask.Run(
    procedure
    var
      lItemPair: TItemPair;
    begin
      {Buying item takes some time}
      Sleep(Random(10));
      if (ItemsQueue.PopItem(lItemPair) = TWaitResult.wrSignaled) then
        TInterlocked.Add(ItemsSold, 1)
      else
        TInterlocked.Add(ItemsOutOfStock, 1)
    end);
end;

procedure TFormMain.TimerDisplayTimer(Sender: TObject);
var
  lItemsMade, lItemsOverStock: Int64; {Production}
  lItemsSold, lItemsOutOfStock: Int64; {Consumption}
  lCurrentStock: Int64;
begin
  lItemsMade := TInterlocked.Read(ItemsMade);
  lItemsOverStock := TInterlocked.Read(ItemsOverStock);
  lItemsSold := TInterlocked.Read(ItemsSold);
  lItemsOutOfStock := TInterlocked.Read(ItemsOutOfStock);
  {Display in GUI}
  LabelMade.Text := lItemsMade.ToString;
  LabelOverStock.Text := lItemsOverStock.ToString;
  LabelSold.Text := lItemsSold.ToString;
  LabelOutOfStock.Text := lItemsOutOfStock.ToString;

  lCurrentStock := ItemsQueue.QueueSize;
  LabelInStock.Text := lCurrentStock.ToString;
end;

procedure TFormMain.TimerProductionTimer(Sender: TObject);
begin
  TTask.Run(
    procedure
    var
      lItemPair: TItemPair;
    begin
      lItemPair.Key := 'Item' + Random(1000).ToString;
      lItemPair.Value := Random(1000);
      if (ItemsQueue.PushItem(lItemPair) = TWaitResult.wrSignaled) then
        TInterlocked.Add(ItemsMade, 1)
      else
        TInterlocked.Add(ItemsOverstock, 1)
    end);
end;

procedure TFormMain.TrackBarConsumptionChange(Sender: TObject);
var
  lValue: Integer;
begin
  lValue := Round((Sender as TTrackbar).Value);
  LabelConsPerSec.Text := lValue.ToString + ' / sec.';
  if lValue = 0 then
    TimerConsumption.Enabled := False
  else
  begin
    {100% = 10 ms, 1% = 1000 ms}
    TimerConsumption.Interval := Round(1000 / lValue);
    TimerConsumption.Enabled := True;
  end;
end;

procedure TFormMain.TrackBarProductionChange(Sender: TObject);
var
  lValue: Integer;
begin
  lValue := Round((Sender as TTrackbar).Value);
  LabelProdPerSec.Text := lValue.ToString + ' / sec.';
  if lValue = 0 then
    TimerProduction.Enabled := False
  else
  begin
    {100% = 10 ms, 1% = 1000 ms}
    TimerProduction.Interval := Round(1000 / lValue);
    TimerProduction.Enabled := True;
  end;
end;

end.
