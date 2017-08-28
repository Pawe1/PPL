unit UnitFormMain;

{===============================================================================
 This code shows how to use the parallel for loop in combination with
 TInterlocked

 The parallel For loop runs in parallel to the main thread, and divides pieces
 of the for loop into a number of tasks.

 Any changes to an Int64 may become "sheared" unless you lock it. Changes
 are not as slim as you may think; as this example illustrates.

 Its best to just always use TInterlocked operations when updating simple
 types from multiple-threads.

 Author: Danny Wind
 License: Creative Commons CC-BY
===============================================================================}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    ButtonStartParallelFor: TButton;
    LabelSum: TLabel;
    TimerMonitor: TTimer;
    procedure ButtonStartParallelForClick(Sender: TObject);
    procedure TimerMonitorTimer(Sender: TObject);
  private
    { Private declarations }
    //FSum: Integer; <- this one is atomic, but be aware of out-of-order execution issues
    //[Align(8)] <- correctly aligning Int64 makes it atomic, but not for all instructions
    FSum: Int64;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Threading, System.SyncObjs;

procedure TFormMain.ButtonStartParallelForClick(Sender: TObject);
begin
  //FSum := 0;
  TInterlocked.Exchange(FSum, 0);
  TTask.Run(
    procedure
    begin
      TParallel.For(0, 10000,
        procedure (Index: Integer)
        begin
          Sleep(1);
          //FSum := FSum + Index;
          TInterlocked.Add(FSum, Index);
        end);
    end);
end;

procedure TFormMain.TimerMonitorTimer(Sender: TObject);
var
  lSum: Int64;
begin
  lSum := TInterlocked.Read(FSum);
  LabelSum.Text := lSum.ToString;
end;

end.
