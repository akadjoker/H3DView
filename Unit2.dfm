object Form2: TForm2
  Left = 410
  Top = 139
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Debug Vertex Bones'
  ClientHeight = 473
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object RichEdit: TRichEdit
    Left = 8
    Top = 8
    Width = 457
    Height = 409
    Lines.Strings = (
      'RichEdit')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button: TButton
    Left = 176
    Top = 440
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 1
    OnClick = ButtonClick
  end
end
