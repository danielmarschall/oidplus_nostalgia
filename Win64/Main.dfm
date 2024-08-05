object Form1: TForm1
  Left = 221
  Top = 118
  Caption = 'ViaThinkSoft OIDplus for Windows'
  ClientHeight = 420
  ClientWidth = 717
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 440
    Top = 0
    Width = 277
    Height = 420
    Align = alClient
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnChange = TreeView1Change
    OnKeyDown = TreeView1KeyDown
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 440
    Height = 420
    Align = alLeft
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'OID'
      TabVisible = False
      DesignSize = (
        432
        410)
      object Label1: TLabel
        Left = 272
        Top = 0
        Width = 48
        Height = 13
        Caption = 'ASN.1 ID:'
      end
      object Label3: TLabel
        Left = 0
        Top = 112
        Width = 23
        Height = 13
        Caption = 'Title:'
      end
      object Label4: TLabel
        Left = 0
        Top = 8
        Width = 22
        Height = 13
        Caption = 'OID:'
      end
      object Label5: TLabel
        Left = 0
        Top = 48
        Width = 18
        Height = 13
        Caption = 'RA:'
      end
      object Label6: TLabel
        Left = 192
        Top = 8
        Width = 40
        Height = 13
        Caption = 'Created:'
      end
      object Label7: TLabel
        Left = 192
        Top = 48
        Width = 44
        Height = 13
        Caption = 'Updated:'
      end
      object Label15: TLabel
        Left = 0
        Top = 152
        Width = 103
        Height = 13
        Caption = 'Additional information:'
      end
      object Label16: TLabel
        Left = 135
        Top = 8
        Width = 42
        Height = 13
        Alignment = taRightJustify
        Caption = 'Filename'
      end
      object Edit3: TEdit
        Left = 0
        Top = 128
        Width = 425
        Height = 21
        TabOrder = 0
      end
      object Memo1: TMemo
        Left = 0
        Top = 168
        Width = 425
        Height = 201
        Anchors = [akLeft, akTop, akBottom]
        ScrollBars = ssVertical
        TabOrder = 7
      end
      object Edit4: TEdit
        Left = 0
        Top = 24
        Width = 177
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 4
      end
      object CheckBox1: TCheckBox
        Left = 0
        Top = 88
        Width = 97
        Height = 17
        Caption = 'Draft OID'
        TabOrder = 5
        OnClick = CheckBox1Click
      end
      object ComboBox1: TComboBox
        Left = 0
        Top = 64
        Width = 177
        Height = 21
        Style = csDropDownList
        TabOrder = 6
      end
      object Edit5: TEdit
        Left = 192
        Top = 24
        Width = 73
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 13
      end
      object Edit6: TEdit
        Left = 193
        Top = 64
        Width = 73
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 14
      end
      object ListBox1: TListBox
        Left = 272
        Top = 16
        Width = 153
        Height = 73
        ItemHeight = 13
        TabOrder = 12
        OnKeyDown = ListBox1KeyDown
      end
      object Edit7: TEdit
        Left = 272
        Top = 96
        Width = 73
        Height = 21
        TabOrder = 1
        OnKeyPress = Edit7KeyPress
      end
      object Button1: TButton
        Left = 344
        Top = 96
        Width = 41
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = Button1Click
      end
      object Button3: TButton
        Left = 384
        Top = 96
        Width = 41
        Height = 25
        Caption = 'Del'
        TabOrder = 3
        OnClick = Button3Click
      end
      object Button2: TButton
        Left = 3
        Top = 378
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Save'
        TabOrder = 8
        OnClick = Button2Click
      end
      object Button4: TButton
        Left = 269
        Top = 378
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Create child:'
        TabOrder = 11
        OnClick = Button4Click
      end
      object Edit1: TEdit
        Left = 349
        Top = 378
        Width = 73
        Height = 21
        Anchors = [akLeft, akBottom]
        TabOrder = 10
        OnKeyPress = Edit1KeyPress
      end
      object Button6: TButton
        Left = 123
        Top = 378
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Delete'
        TabOrder = 9
        OnClick = Button6Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'RA'
      ImageIndex = 1
      TabVisible = False
      object Label2: TLabel
        Left = 24
        Top = 16
        Width = 18
        Height = 13
        Caption = 'RA:'
      end
      object Label8: TLabel
        Left = 216
        Top = 16
        Width = 40
        Height = 13
        Caption = 'Created:'
      end
      object Label9: TLabel
        Left = 24
        Top = 83
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object Label10: TLabel
        Left = 24
        Top = 115
        Width = 32
        Height = 13
        Caption = 'E-Mail:'
      end
      object Label11: TLabel
        Left = 24
        Top = 147
        Width = 34
        Height = 13
        Caption = 'Phone:'
      end
      object Label12: TLabel
        Left = 312
        Top = 16
        Width = 44
        Height = 13
        Caption = 'Updated:'
      end
      object Label17: TLabel
        Left = 159
        Top = 16
        Width = 42
        Height = 13
        Alignment = taRightJustify
        Caption = 'Filename'
      end
      object Button8: TButton
        Left = 126
        Top = 200
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 7
        OnClick = Button8Click
      end
      object Edit9: TEdit
        Left = 24
        Top = 32
        Width = 177
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
      end
      object Edit10: TEdit
        Left = 216
        Top = 32
        Width = 73
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
      end
      object Edit11: TEdit
        Left = 88
        Top = 80
        Width = 297
        Height = 21
        TabOrder = 3
      end
      object Edit12: TEdit
        Left = 88
        Top = 112
        Width = 297
        Height = 21
        TabOrder = 4
      end
      object Edit13: TEdit
        Left = 88
        Top = 144
        Width = 297
        Height = 21
        TabOrder = 5
      end
      object Button9: TButton
        Left = 22
        Top = 200
        Width = 75
        Height = 25
        Caption = 'Save'
        TabOrder = 6
        OnClick = Button9Click
      end
      object Edit14: TEdit
        Left = 312
        Top = 32
        Width = 73
        Height = 21
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 2
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'OID Intro'
      ImageIndex = 2
      TabVisible = False
      object Label13: TLabel
        Left = 16
        Top = 16
        Width = 191
        Height = 28
        Caption = 'Object Identifiers'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Arial Black'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object Button5: TButton
        Left = 16
        Top = 64
        Width = 75
        Height = 25
        Caption = 'Create child:'
        TabOrder = 1
        OnClick = Button4Click
      end
      object Edit2: TEdit
        Left = 96
        Top = 64
        Width = 73
        Height = 21
        TabOrder = 0
        OnKeyPress = Edit2KeyPress
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'RA Intro'
      ImageIndex = 3
      TabVisible = False
      object Label14: TLabel
        Left = 16
        Top = 16
        Width = 262
        Height = 28
        Caption = 'Registration Authorities'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -20
        Font.Name = 'Arial Black'
        Font.Style = [fsItalic]
        ParentFont = False
      end
      object Edit8: TEdit
        Left = 96
        Top = 64
        Width = 233
        Height = 21
        TabOrder = 0
        OnKeyPress = Edit8KeyPress
      end
      object Button7: TButton
        Left = 16
        Top = 64
        Width = 75
        Height = 25
        Caption = 'Create RA:'
        TabOrder = 1
        OnClick = Button7Click
      end
    end
  end
end
