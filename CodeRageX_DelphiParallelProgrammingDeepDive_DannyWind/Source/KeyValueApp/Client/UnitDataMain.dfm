object DataModuleMain: TDataModuleMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 355
  Width = 451
  object TimerRefresh: TTimer
    OnTimer = TimerRefreshTimer
    Left = 376
    Top = 24
  end
end
