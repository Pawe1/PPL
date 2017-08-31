object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 311
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 56
    Width = 627
    Height = 16
    TabOrder = 1
  end
  object Button2: TButton
    Left = 104
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ActivityIndicator1: TActivityIndicator
    Left = 64
    Top = 112
    IndicatorSize = aisLarge
  end
  object Button3: TButton
    Left = 208
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 152
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 5
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 336
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Async / Await'
    TabOrder = 6
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 480
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Timer'
    TabOrder = 7
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 552
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Button7'
    TabOrder = 8
    OnClick = Button7Click
  end
  object tmrUpdateProgress: TTimer
    Interval = 100
    Left = 488
    Top = 200
  end
end
