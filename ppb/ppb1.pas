unit ppb1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,

  System.Threading,

  GpStuff,
  OtlComm,
  OtlTask,
  OtlTaskControl,
  OtlParallel;

const
  WM_UPDATE_PROGRESS = WM_USER;

type
  TfrmUpdateProgressBar = class(TForm)
    btnOTLWithMessage: TButton;
    btnOTLWithTimer: TButton;
    btnPPLMessage: TButton;
    btnPPLWithQueue: TButton;
    btnPPLWithTimer: TButton;
    pbParallel: TProgressBar;
    tmrUpdateProgress: TTimer;
    btnPPLAsyncAwait: TButton;
    btnTThreadAsyncAwait: TButton;
    procedure btnOTLWithMessageClick(Sender: TObject);
    procedure btnOTLWithTimerClick(Sender: TObject);
    procedure btnPPLAsyncAwaitClick(Sender: TObject);
    procedure btnPPLMessageClick(Sender: TObject);
    procedure btnPPLWithQueueClick(Sender: TObject);
    procedure btnPPLWithTimerClick(Sender: TObject);
    procedure btnTThreadAsyncAwaitClick(Sender: TObject);
    procedure tmrUpdateProgressTimer(Sender: TObject);
  private
    FParallelFor: IOmniParallelSimpleLoop;
    FProcessed: TGp4AlignedInt;
    FProcessedPct: TGp4AlignedInt;
    FProgress: integer;
    FTask: ITask;
    procedure AfterTest;
    procedure BeforeTest;
    procedure UpdateProgressBar(var msg: TMessage); message WM_UPDATE_PROGRESS;
  end;

var
  frmUpdateProgressBar: TfrmUpdateProgressBar;

implementation

uses
  System.SyncObjs;

{$R *.dfm}

// PPL version of Async/Await.

procedure AsyncAwait(async, await: TProc);
begin
  TTask.Run(
    procedure
    begin
      async();

      TThread.Queue(nil,
        procedure
        begin
          await();
        end
      ); // TThread.Queue
    end
  ); //TTask.Run
end;

// TThread version of Async/Await.

procedure AsyncAwaitTh(async, await: TProc);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      async();

      TThread.Queue(nil,
        procedure
        begin
          await();
        end
      ); // TThread.Queue
    end
  ).Start;
end;

procedure TfrmUpdateProgressBar.AfterTest;
var
  i: integer;
begin
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TButton then
      TButton(Controls[i]).Enabled := true;
  FTask := nil;
  FParallelFor := nil;
end;

procedure TfrmUpdateProgressBar.BeforeTest;
var
  i: integer;
begin
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TButton then
      TButton(Controls[i]).Enabled := false;
  pbParallel.Position := 0;
end;

procedure TfrmUpdateProgressBar.btnOTLWithMessageClick(Sender: TObject);
var
  handle: THandle;
begin
  BeforeTest;

  handle := frmUpdateProgressBar.Handle;

  FParallelFor := Parallel.For(1, 1000)
    .NoWait
    .TaskConfig(Parallel.TaskConfig.OnTerminated(AfterTest));

  FParallelFor.Execute(
    procedure (i: integer)
    var
      new: integer;
    begin
      Sleep(10); //do the work
      new := FProcessed.Increment; // increment the shared counter
      if new mod 10 = 0 then
        PostMessage(handle, WM_UPDATE_PROGRESS, Round(new / 1000 * 100), 0);
    end
  ); // Parallel.For
end;

procedure TfrmUpdateProgressBar.btnOTLWithTimerClick(Sender: TObject);
begin
  BeforeTest;
  FProcessed.Value := 0;
  FProcessedPct.Value := 0;
  tmrUpdateProgress.Enabled := true;

  FParallelFor := Parallel.For(1, 1000)
    .NoWait
    .TaskConfig(Parallel.TaskConfig.OnTerminated(
      procedure
      begin
        // Disable the timer when everything is processed.
        pbParallel.Position := 100;
        tmrUpdateProgress.Enabled := false;
        AfterTest;
      end));

  FParallelFor.Execute(
    procedure (i: integer)
    var
      new: integer;
    begin
      Sleep(10); //do the work
      new := FProcessed.Increment; // increment the shared counter
      if new mod 10 = 0 then
        FProcessedPct.Value := Round(new / 1000 * 100);
    end
  ); // Parallel.For
end;

procedure TfrmUpdateProgressBar.btnPPLAsyncAwaitClick(Sender: TObject);
begin
  BeforeTest; //disable buttons

  AsyncAwait(
    procedure
    begin
      Sleep(1000);
    end,
    procedure
    begin
      AfterTest; //enable buttons
    end);
end;

procedure TfrmUpdateProgressBar.btnPPLMessageClick(Sender: TObject);
var
  handle: THandle;
begin
  BeforeTest;

  // Local copy of a form handle. As I've said in the presentation - don't access the
  // UI from a thread! (Although in this case that will not cause any problems - but
  // it is always best to stay on the safe side.)

  handle := frmUpdateProgressBar.Handle;

  // While the parallel code is running, main form must process messages, so we have
  // to push a TParallel.For into background by wrapping it inside a task.

  FTask := TTask.Run(
    procedure
    var
      processed: integer; // shared counter
      total    : integer; // total number of items to be processed
    begin
      processed := 0;
      total := 1000;
      TParallel.For(1, 1000,
        procedure (i: integer)
        var
          new: integer;
        begin
          Sleep(10); //do the work
          new := TInterlocked.Increment(processed); // increment the shared counter
          if (new mod 10) = 0 then // update the progress bar every 10 processed items
            PostMessage(handle, WM_UPDATE_PROGRESS, Round(new / total * 100), 0);
        end
      ); // TParallel.For

      // Update the UI
      TThread.Queue(nil, AfterTest);
    end
  ); // TTask.Run
end;

procedure TfrmUpdateProgressBar.btnPPLWithQueueClick(Sender: TObject);
begin
  BeforeTest;

  // While the parallel code is running, main form must process messages, so we have
  // to push a TParallel.For into background by wrapping it inside a task.

  FTask := TTask.Run(
    procedure
    var
      processed: integer; // shared counter
      total    : integer; // total number of items to be processed
    begin
      processed := 0;
      total := 1000;
      TParallel.For(1, 1000,
        procedure (i: integer)
        var
          new: integer;
        begin
          Sleep(10); //do the work
          new := TInterlocked.Increment(processed); // increment the shared counter
          if (new mod 10) = 0 then // update the progress bar every 10 processed items
            TThread.Queue(nil,
              procedure
              begin
                pbParallel.Position := Round(new / total * 100);
              end
            ); //TThread.Queue
        end
      ); // TParallel.For

      // Update the UI
      TThread.Queue(nil, AfterTest);
    end
  ); // TTask.Run
end;

procedure TfrmUpdateProgressBar.btnPPLWithTimerClick(Sender: TObject);
begin
  BeforeTest;
  FProgress := 0;
  tmrUpdateProgress.Enabled := true;

  // While the parallel code is running, main form must process messages, so we have
  // to push a TParallel.For into background by wrapping it inside a task.

  FTask := TTask.Run(
    procedure
    var
      processed: integer; // shared counter
      total    : integer; // total number of items to be processed
    begin
      processed := 0;
      total := 1000;
      TParallel.For(1, 1000,
        procedure (i: integer)
        var
          new: integer;
        begin
          Sleep(10); //do the work
          new := TInterlocked.Increment(processed); // increment the shared counter
          if (new mod 10) = 0 then // update the progress bar every 10 processed items
            // Even with one reader and one writer accessing an 'integer' is not atomic!
            TInterlocked.Exchange(FProgress, Round(new / total * 100));
        end
      ); // TParallel.For

      // Disable the timer when everything is processed.
      TThread.Queue(nil,
        procedure
        begin
          // We have no idea if timer proc 'saw' FProgress = 100 so let's force the last
          // update.
          pbParallel.Position := 100;
          tmrUpdateProgress.Enabled := false;
          AfterTest;
        end
      ); //TThread.Queue
    end
  ); // TTask.Run
end;

procedure TfrmUpdateProgressBar.btnTThreadAsyncAwaitClick(Sender: TObject);
begin
  BeforeTest; //disable buttons

  AsyncAwaitTh(
    procedure
    begin
      Sleep(1000);
    end,
    procedure
    begin
      AfterTest; //enable buttons
    end);
end;

procedure TfrmUpdateProgressBar.tmrUpdateProgressTimer(Sender: TObject);
begin
  if assigned(FParallelFor) then // OTL demo
    pbParallel.Position := FProcessedPct.Value
  else // PPL demo
    pbParallel.Position := TInterlocked.Exchange(FProgress, FProgress);
end;

procedure TfrmUpdateProgressBar.UpdateProgressBar(var msg: TMessage);
begin
  pbParallel.Position := msg.WParam;
end;

end.
