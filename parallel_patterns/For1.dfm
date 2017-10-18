object frmFor: TfrmFor
  Left = 0
  Top = 0
  Caption = 'For'
  ClientHeight = 336
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnCountSerial: TButton
    Left = 16
    Top = 16
    Width = 241
    Height = 65
    Caption = 'Count primes - Serial'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btnCountSerialClick
  end
  object btnCountParallel: TButton
    Left = 16
    Top = 104
    Width = 241
    Height = 65
    Caption = 'Count primes - Parallel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnCountParallelClick
  end
end
