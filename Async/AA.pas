unit AA;

interface

uses
  System.SysUtils;

procedure AsyncAwait(async, await: TProc);
procedure AsyncAwait2(async, await: TProc);

implementation

uses
  System.Classes,
  System.Threading;

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
          //task := nil;
        end
      ); // TThread.Queue
    end
  ); //TTask.Run
end;

procedure AsyncAwait2(async, await: TProc);
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

end.
