unit UnitFormMain;

{===============================================================================
 Demo for Parallel For

 This code shows how to use the parallel for loop.

 The parallel For loop runs in parallel to the main thread, and divides pieces
 of the for loop into a number of tasks. This means that sometimes the results
 will be in a random order, instead of the expected 0..9.

 Author: Danny Wind
 License: Creative Commons CC-BY
===============================================================================}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.Controls.Presentation, FMX.ScrollBox;

type
  TFormMain = class(TForm)
    Memo1: TMemo;
    ButtonStartParallelFor: TButton;
    procedure ButtonStartParallelForClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Threading;

procedure TFormMain.ButtonStartParallelForClick(Sender: TObject);
begin
  Memo1.Lines.Clear;
  TTask.Run(
    procedure
    begin
      TParallel.For(0, 9,
        procedure (Index: Integer)
        begin
          Sleep(200);
          TThread.Queue(TThread.CurrentThread,
            procedure
            begin
              Memo1.Lines.Add(Index.ToString);
            end);
        end);
    end);
end;

end.
