object FormSettings: TFormSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080' Weather'
  ClientHeight = 193
  ClientWidth = 257
  Color = 4079166
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 46
    Top = 31
    Width = 126
    Height = 19
    Caption = #1052#1077#1089#1090#1086#1087#1086#1083#1086#1078#1077#1085#1080#1077
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8750469
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LabelError: TLabel
    Left = 41
    Top = 12
    Width = 173
    Height = 16
    Caption = #1052#1077#1089#1090#1086#1087#1086#1083#1086#1078#1077#1085#1080#1077' '#1085#1077' '#1085#1072#1081#1076#1077#1085#1086
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9408493
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object Label2: TLabel
    Left = 46
    Top = 83
    Width = 158
    Height = 19
    Caption = #1062#1074#1077#1090' '#1080#1082#1086#1085#1086#1082' '#1080' '#1090#1077#1082#1089#1090#1072
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8750469
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object LabelExColor: TLabelEx
    Left = 46
    Top = 108
    Width = 164
    Height = 30
    Cursor = crHandPoint
    Brush.Color = 10053222
    Pen.Color = clSilver
    Shape = stRoundRect
    Caption = ''
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    TextFormat = [tfCenter, tfSingleLine, tfVerticalCenter, tfWordBreak, tfWordEllipsis]
    IgnorBounds = True
    EllipseRectVertical = False
    OnClick = LabelExColorClick
  end
  object ButtonFlatSetCancel: TButtonFlat
    Left = 186
    Top = 156
    Width = 24
    Height = 24
    Caption = ''
    ColorNormal = 4079166
    ColorOver = 4934475
    ColorPressed = 3223857
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    FontOver.Charset = DEFAULT_CHARSET
    FontOver.Color = clWindowText
    FontOver.Height = -13
    FontOver.Name = 'Tahoma'
    FontOver.Style = []
    FontDown.Charset = DEFAULT_CHARSET
    FontDown.Color = clWindowText
    FontDown.Height = -13
    FontDown.Name = 'Tahoma'
    FontDown.Style = []
    IgnorBounds = True
    ImageIndentLeft = 0
    ImageIndex = 1
    Images = ImageList24
    OnClick = ButtonFlatSetCancelClick
    RoundRectParam = 0
    ShowFocusRect = False
    TabOrder = 0
    TabStop = True
    TextFormat = [tfCenter, tfSingleLine, tfVerticalCenter]
    SubTextFont.Charset = DEFAULT_CHARSET
    SubTextFont.Color = clWhite
    SubTextFont.Height = -13
    SubTextFont.Name = 'Tahoma'
    SubTextFont.Style = []
  end
  object ButtonFlatSetOk: TButtonFlat
    Left = 156
    Top = 156
    Width = 24
    Height = 24
    Caption = ''
    ColorNormal = 4079166
    ColorOver = 4934475
    ColorPressed = 3223857
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    FontOver.Charset = DEFAULT_CHARSET
    FontOver.Color = clWindowText
    FontOver.Height = -13
    FontOver.Name = 'Tahoma'
    FontOver.Style = []
    FontDown.Charset = DEFAULT_CHARSET
    FontDown.Color = clWindowText
    FontDown.Height = -13
    FontDown.Name = 'Tahoma'
    FontDown.Style = []
    IgnorBounds = True
    ImageIndentLeft = 0
    ImageIndex = 0
    Images = ImageList24
    OnClick = ButtonFlatSetOkClick
    RoundRectParam = 0
    ShowFocusRect = False
    TabOrder = 1
    TabStop = True
    TextFormat = [tfCenter, tfSingleLine, tfVerticalCenter]
    SubTextFont.Charset = DEFAULT_CHARSET
    SubTextFont.Color = clWhite
    SubTextFont.Height = -13
    SubTextFont.Name = 'Tahoma'
    SubTextFont.Style = []
  end
  object EditCity: TEdit
    Left = 46
    Top = 50
    Width = 164
    Height = 27
    BevelInner = bvSpace
    BevelKind = bkSoft
    BevelOuter = bvSpace
    BorderStyle = bsNone
    Color = 5460819
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    TextHint = #1053#1072#1079#1074#1072#1085#1080#1077', ZIP '#1080#1083#1080' '#1082#1086#1086#1088#1076#1080#1085#1072#1090#1099
  end
  object ImageList24: TImageList
    ColorDepth = cd32Bit
    Height = 24
    Width = 24
    Left = 24
    Top = 144
    Bitmap = {
      494C010102006400700018001800FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000600000001800000001002000000000000024
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000030C080E0D25192D0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000080C1F27141E4D5F02020609000000000000
      0000000000000000000002040D0F151E4F5F080C1F2600000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000006140D1843B078D44FCE8CF71E50
      3760000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000C102C35354BBFE63A53D4FF293B99B802040E110000
      000000000000040713172C40A3C53A53D4FF354ABFE60A102932000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000007160F1C40AA74CC50D390FE51D491FF4FCE
      8CF71C4C345B0001000100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000002023147B7DC3A53D4FF3A53D4FF3952D3FE283B96B40204
      0E11040713172B3E9FC13A53D4FF3A53D4FF3A53D4FF3045B2D6000001020000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000006140D1840AA74CC51D491FF51D491FF51D491FF51D4
      91FF4DCB8BF51C4C345B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000101243486A33952D3FE3A53D4FF3A53D4FF3952D3FE293C
      99B92C40A3C53A53D4FF3A53D4FF3A53D4FF3952D3FE202F7891000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000006140D1843B078D451D491FF51D491FF51D491FF51D491FF51D4
      91FF51D491FF4FCE8CF71E503760000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000010105062333829E3A53D4FF3A53D4FF3A53D4FF3A53
      D4FF3A53D4FF3A53D4FF3A53D4FF3952D3FE202E768F00000202000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000007160F1C40AA74CC50D390FE51D491FF51D491FF4FD08EFB4CC989F251D4
      91FF51D491FF51D491FF4FCE8CF71C4C345B0001000100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000103080A2333829E3952D3FE3A53D4FF3A53
      D4FF3A53D4FF3A53D4FF3952D3FE202E768F0101050600000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000379063AE51D491FF51D491FF51D491FF50D390FE296E4B84122E203949C0
      83E750D390FE51D491FF51D491FF4DCB8BF51C4C345B00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000004061216354CC1EA3A53D4FF3A53
      D4FF3A53D4FF3A53D4FF3249BBE102030A0C0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001F5439654ECE8DF851D491FF4FD08FFB296E4B8400000000000000000D25
      192D49C083E751D491FF51D491FF51D491FF4FCE8CF71E503760000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000040713172C40A3C53A53D4FF3A53D4FF3A53
      D4FF3A53D4FF3A53D4FF3A53D4FF293B99B802040E1100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000001010221593D6B47B97EDE286C49820103020400000000000000000000
      00000F2B1D3448BB81E351D491FF51D491FF51D491FF4FCE8CF71C4C345B0001
      0001000000000000000000000000000000000000000000000000000000000000
      00000000000000000000040713172B3E9FC13A53D4FF3A53D4FF3A53D4FF3A53
      D4FF3A53D4FF3A53D4FF3A53D4FF3952D3FE283B96B402040E11000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F2B1D3449C083E750D390FE51D491FF51D491FF4DCB8BF51C4C
      345B000000000000000000000000000000000000000000000000000000000000
      000000000000000001012B3FA0C03A53D4FF3A53D4FF3A53D4FF3952D3FE2331
      7D9825358AA63952D3FE3A53D4FF3A53D4FF3952D3FE273890AF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000D25192D49C083E751D491FF51D491FF51D491FF3B9D
      6BBD000000000000000000000000000000000000000000000000000000000000
      000000000000000001012E43ABCE3A53D4FF3A53D4FF3952D3FE202E768F0000
      0202010105062333829E3A53D4FF3A53D4FF3A53D4FF2A3D9DBC000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000F2B1D3448BB81E351D491FF48BB80E10F2A
      1D33000000000000000000000000000000000000000000000000000000000000
      00000000000000000000070A1C233044B0D33952D3FE202E768F010105060000
      0000000000000103080A2333829E3952D3FE2E41A7C80608161B000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000010291C323183599D0E271A2F0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000507151A0E13333E00000202000000000000
      00000000000000000000010105060E13333F0406101500000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000060000000180000000100010000000000200100000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF000000000000FFFFFFFF
      FFFF000000000000FFFFFFFFFFFF000000000000FFFFFFFFFFFF000000000000
      FFFFFFFFFFFF000000000000FF9FFFFE3C7F000000000000FF0FFFFC183F0000
      00000000FE03FFF8001F000000000000FC03FFF8003F000000000000F801FFFC
      003F000000000000F0007FFE007F000000000000F0007FFF00FF000000000000
      F0603FFE007F000000000000F0700FFC003F000000000000FFF80FF8003F0000
      00000000FFFC0FF8003F000000000000FFFE0FFC183F000000000000FFFF1FFE
      3C7F000000000000FFFFFFFFFFFF000000000000FFFFFFFFFFFF000000000000
      FFFFFFFFFFFF000000000000FFFFFFFFFFFF000000000000FFFFFFFFFFFF0000
      00000000FFFFFFFFFFFF00000000000000000000000000000000000000000000
      000000000000}
  end
  object ColorDialog: TColorDialog
    Options = [cdFullOpen]
    Left = 80
    Top = 144
  end
end
