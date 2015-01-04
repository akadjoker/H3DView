object Form3: TForm3
  Left = 590
  Top = 148
  Width = 674
  Height = 489
  Caption = 'Debug Log'
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
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 658
    Height = 451
    Align = alClient
    PopupMenu = PopupMenu
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object PopupMenu: TPopupMenu
    Left = 264
    Top = 88
    object clear1: TMenuItem
      Caption = 'clear'
      OnClick = clear1Click
    end
  end
end
