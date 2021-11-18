object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Griffschrift Helper'
  ClientHeight = 374
  ClientWidth = 679
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
  DesignSize = (
    679
    374)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 89
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
    Left = 49
    Top = 326
    Width = 121
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Pressure beam thickness '
  end
  object lbInstr: TLabel
    Left = 318
    Top = 326
    Width = 53
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Instrument'
  end
  object lblTranspose: TLabel
    Left = 318
    Top = 302
    Width = 92
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Transpose (primes)'
  end
  object cbxRemoveColor: TCheckBox
    Left = 49
    Top = 276
    Width = 178
    Height = 17
    Alignment = taLeftJustify
    Anchors = [akLeft, akBottom]
    Caption = 'Remove note colors'
    TabOrder = 0
  end
  object edtDicke: TEdit
    Left = 186
    Top = 323
    Width = 41
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Text = '3.5'
    OnExit = edtDickeExit
    OnKeyPress = edtDickeKeyPress
  end
  object cbxInstr: TComboBox
    Left = 382
    Top = 323
    Width = 242
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 5
  end
  object cbxKlingend: TCheckBox
    Left = 318
    Top = 276
    Width = 163
    Height = 17
    Alignment = taLeftJustify
    Anchors = [akLeft, akBottom]
    Caption = 'Insert sounding notes anew'
    TabOrder = 3
    OnClick = cbxKlingendClick
  end
  object cbxTranspose: TComboBox
    Left = 431
    Top = 299
    Width = 50
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemIndex = 11
    TabOrder = 4
    Text = '0'
    Items.Strings = (
      '-11'
      '-10'
      '-9'
      '-8'
      '-7'
      '-6'
      '-5'
      '-4'
      '-3'
      '-2'
      '-1'
      '0'
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9'
      '10'
      '11'
      '12'
      '13'
      '14'
      '15')
  end
  object cxbPressure: TCheckBox
    Left = 49
    Top = 301
    Width = 178
    Height = 17
    Alignment = taLeftJustify
    Anchors = [akLeft, akBottom]
    Caption = 'Insert pressure beams'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = cxbPressureClick
  end
  object Memo1: TMemo
    Left = 238
    Top = 8
    Width = 185
    Height = 106
    Lines.Strings = (
      '<?xml version="1.0" '
      'encoding="UTF-8"?>'
      '<container>'
      '  <rootfiles>'
      '    <rootfile full-path="%s.mscx">'
      '      </rootfile>'
      '    </rootfiles>'
      '  </container>')
    TabOrder = 6
    Visible = False
  end
end
