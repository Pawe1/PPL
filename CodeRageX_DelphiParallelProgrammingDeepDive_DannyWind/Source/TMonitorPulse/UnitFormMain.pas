{===============================================================================
 TMonitor using Wait and Pulse

 Shows how to use TMonitor to mark instances of TObject as locked. Only code that
 also uses TMonitor will respect that "lock-mark".
 Also shows the use of Wait and Pulse.

 Note: the existing FMX/VCL framework uses TMonitor internally; for some existing
       classes you should not use TMonitor on the class, but use a helper instead.

 Author: Danny Wind
 License: Creative Commons CC-BY
===============================================================================}

unit UnitFormMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.Controls.Presentation, FMX.ScrollBox;

type
  TFormMainMonitor = class(TObject);
  TFormMain = class(TForm)
    Memo1: TMemo;
    ButtonStartWait: TButton;
    ButtonStartPulse: TButton;
    procedure ButtonStartWaitClick(Sender: TObject);
    procedure ButtonStartPulseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FormMainMonitor: TFormMainMonitor;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Threading;

procedure TFormMain.ButtonStartPulseClick(Sender: TObject);
begin
  TMonitor.Pulse(FormMainMonitor);
end;

procedure TFormMain.ButtonStartWaitClick(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      TMonitor.Enter(FormMainMonitor);
      try
        TMonitor.Wait(FormMainMonitor, INFINITE);
        TThread.Synchronize(nil,
          procedure
          begin
            Memo1.Lines.Add('The wait is over...');
          end);
      finally
        TMonitor.Exit(FormMainMonitor);
      end;
    end);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FormMainMonitor := TFormMainMonitor.Create;
end;

end.
