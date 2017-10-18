object frmMap: TfrmMap
  Left = 0
  Top = 0
  Caption = 'Map'
  ClientHeight = 336
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 19
  object btnConvertSerial: TButton
    Left = 16
    Top = 16
    Width = 209
    Height = 56
    Caption = 'Convert - serial'
    TabOrder = 0
    OnClick = btnConvertSerialClick
  end
  object btnConvertParallel: TButton
    Left = 16
    Top = 88
    Width = 209
    Height = 56
    Caption = 'Convert - parallel'
    TabOrder = 1
    OnClick = btnConvertParallelClick
  end
end
