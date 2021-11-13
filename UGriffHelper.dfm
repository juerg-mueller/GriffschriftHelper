object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Griffschrift Helper'
  ClientHeight = 295
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 56
    Top = 120
    Width = 492
    Height = 52
    Caption = 'Drop MuseScore file here.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -43
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 56
    Top = 233
    Width = 116
    Height = 13
    Caption = 'Druckbalken Dicke in mm'
  end
  object CheckBox1: TCheckBox
    Left = 56
    Top = 252
    Width = 163
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Notenfarben entfernen'
    TabOrder = 0
  end
  object edtDicke: TEdit
    Left = 178
    Top = 230
    Width = 41
    Height = 21
    TabOrder = 1
    Text = '3.5'
    OnExit = edtDickeExit
    OnKeyPress = edtDickeKeyPress
  end
end
