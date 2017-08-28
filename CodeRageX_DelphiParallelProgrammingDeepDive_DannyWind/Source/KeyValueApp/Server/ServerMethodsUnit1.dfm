object ServerMethods1: TServerMethods1
  OldCreateOrder = False
  Height = 277
  Width = 392
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=C:\Database\IBKEYVALUE.IB'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'CharacterSet=UTF8'
      'DriverID=IB')
    LoginPrompt = False
    Left = 48
    Top = 32
  end
  object FDQuerySelect: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT AValue, ADateTime'
      'FROM AStore'
      'WHERE AKey = :AKey')
    Left = 48
    Top = 88
    ParamData = <
      item
        Name = 'AKEY'
        DataType = ftWideString
        ParamType = ptInput
        Size = 128
        Value = Null
      end>
  end
  object FDQueryInsert: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'INSERT INTO AStore'
      '(AKey, AValue, ADatetime)'
      'VALUES'
      '(:AKey, :AValue, :ADatetime)')
    Left = 152
    Top = 88
    ParamData = <
      item
        Name = 'AKEY'
        DataType = ftWideString
        ParamType = ptInput
        Size = 128
        Value = Null
      end
      item
        Name = 'AVALUE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 512
        Value = Null
      end
      item
        Name = 'ADATETIME'
        DataType = ftTimeStamp
        ParamType = ptInput
        Value = Null
      end>
  end
  object FDQueryUpdate: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'UPDATE AStore'
      'SET AValue = :AValue, ADatetime = :ADatetime'
      'WHERE AKey = :AKey')
    Left = 248
    Top = 88
    ParamData = <
      item
        Name = 'AVALUE'
        DataType = ftWideString
        ParamType = ptInput
        Size = 128
        Value = Null
      end
      item
        Name = 'ADATETIME'
        DataType = ftTimeStamp
        ParamType = ptInput
        Value = Null
      end
      item
        Name = 'AKEY'
        DataType = ftWideString
        ParamType = ptInput
        Size = 512
        Value = Null
      end>
  end
end
