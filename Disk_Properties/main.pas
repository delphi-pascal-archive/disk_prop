unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ImgList, HSObjectList;

type
  TDriveType = (dtFloppy, dtFixed, dtCDROM, dtNetwork, dtRemovable, dtRamDisk,
    dtError);

  TDriveInfo = class
  private
    FDriveType: TDriveType;
    FDriveName: string;
    FFileSystemName: string;
    FVolumeName: string;
    FFileSystemFlags: DWORD;
    FInformationValid: Boolean;
    FFreeBytes: TLargeInteger;
    FTotalBytes: TLargeInteger;
    FTotalFree: TLargeInteger;
    FSerialNumber: DWORD;
    function GetDisplayName: string;
    function GetDefaultVolumeName: string;
    function GetPercentUsed: Integer;
  public
    procedure FillInformation;
    property DisplayName: string read GetDisplayName;
    property PercentUsed: Integer read GetPercentUsed;
    property DriveType: TDriveType read FDriveType write FDriveType;
    property DriveName: string read FDriveName write FDriveName;
    property FileSystemName: string read FFileSystemName;
    property FileSystemFlags: DWORD read FFileSystemFlags;
    property InformationValid: Boolean read FInformationValid;
    property VolumeName: string read FVolumeName;
    property TotalBytes: TLargeInteger read FTotalBytes;
    property FreeBytes: TLargeInteger read FFreeBytes;
    property TotalFree: TLargeInteger read FTotalFree;
    property SerialNumber: DWORD read FSerialNumber;
  end;

  TDriveInfoList = class(THSObjectList)
  private
    function GetItems(I: Integer): TDriveInfo;
  public
    property Items[I: Integer]: TDriveInfo read GetItems; default;
  end;

  TfMain = class(TForm)
    lbDrives: TListBox;
    Splitter: TSplitter;
    pnlClient: TPanel;
    DriveImageList: TImageList;
    gbVolumeInfo: TGroupBox;
    Label1: TLabel;
    lbFileSystem: TLabel;
    SmallDrivesImageList: TImageList;
    lbSpace: TListBox;
    SpaceImage: TImage;
    shFree: TShape;
    shUsed: TShape;
    Label2: TLabel;
    lbSerialNo: TLabel;
    lbUsed: TLabel;
    lbFree: TLabel;
    lbFlags: TListBox;
    lbFileSystemFlags: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lbDrivesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure SplitterMoved(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbDrivesClick(Sender: TObject);
    procedure lbSpaceDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormResize(Sender: TObject);
    procedure lbFlagsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    FDriveInfoList: TDriveInfoList;
    // Для того, чтобы процедура рисования элемента ListBox не рисовала
    // рамку фокуса, необходимо самостоятельно обрабатывать сообщение
    // CN_DRAWITEM, так как при стандартной обработке этого сообщения в VCL
    // рамка фокуса рисуется после вызова пользователького метода рисования
    // элемента ListBox.
    FOldListBoxWndProc: TWndMethod;
    FOldFlagsListBoxWndProc: TWndMethod;
    procedure ListBoxWndProc (var Message: TMessage);
    procedure FlagsListBoxWndProc (var Message: TMessage);
    procedure RebuildDriveList;
    procedure DisplayDriveInfo (DriveInfo: TDriveInfo);
  end;

var
  fMain: TfMain;

implementation
uses
  Math, HSLUtils;

//Константы для расчета высоты элемента ListBox, исходя из того, что в элемент
// входит пиктограмма 32х32 и надпись. Между пиктограммой и верхним краем
// элемента должен быть зазор в 8 пикселей, между пиктограммой и надписью -
// зазор в 7 пикселей и между надписью и нижнем краем элемента ListBox - зазор
// в 8 пикселей.
// Так как изображение на пиктограмме не занимает всю ее ширину, пиктограмма при
// рисовании будет сдвинута вправо для центрирования ее изображения.
const
  TopSpacing = 8;
  MiddleSpacing = 7;
  BottomSpacing = 8;
  LeftSpacing = 13;

type
  TSimpleGaugeKind = (sgkBar, sgkPie);

  TSimpleGauge = class
  private
    FColors: array[0..3] of TColor;
    FPercent: Integer;
    FCanvas: TCanvas;
    FBoundsRect: TRect;
    FKind: TSimpleGaugeKind;
    function GetColor(const Index: Integer): TColor;
    procedure SetColor(const Index: Integer; const Value: TColor);
    procedure PaintBar;
    procedure PaintPie;
  public
    constructor Create (ABoundsRect: TRect);
    procedure Paint (ACanvas: TCanvas);
    property BoundsRect: TRect read FBoundsRect;
    property Canvas: TCanvas read FCanvas;
    property Kind: TSimpleGaugeKind read FKind write FKind default sgkBar;
    property Percent: Integer read FPercent write FPercent;
    property UsedColor: TColor index 0 read GetColor write SetColor
      default clBlue;
    property FreeColor: TColor index 1 read GetColor write SetColor
      default clFuchsia;
    property TextColor: TColor index 2 read GetColor write SetColor
      default clWhite;
    property FrameColor: TColor index 3 read GetColor write SetColor
      default clBlack;
  end;

{$R *.dfm}

function HSGetDriveInfo (const DriveName: char): TDriveInfo;
var
  DrivePath: string;
  Buffer: array[0..Pred(MAX_PATH)] of char;
begin
  Result := TDriveInfo.Create;
  Result.DriveName := DriveName;
  try
    DrivePath := DriveName + ':\';
    case GetDriveType(PChar(DrivePath)) of
    DRIVE_FIXED:
      Result.DriveType := dtFixed;
    DRIVE_REMOTE:
      Result.DriveType := dtNetwork;
    DRIVE_CDROM:
      Result.DriveType := dtCDROM;
    DRIVE_RAMDISK:
      Result.DriveType := dtRamDisk;
    DRIVE_REMOVABLE:
      begin
        System.Delete (DrivePath, 3, 1);
        if QueryDosDevice (PChar(DrivePath), Buffer, SizeOf(Buffer)) = 0 then
          Result.DriveType := dtError
        else if (SameText(Buffer, '\Device\Floppy0')) then
          Result.DriveType := dtFloppy
        else
          Result.DriveType := dtRemovable;
      end;
    else
      Result.DriveType := dtError;
    end;
    Result.FillInformation;
  except
    Result.Free;
    raise;
  end;
end;

{ TfMain }

procedure TfMain.RebuildDriveList;
var
  Drive: Char;
  DriveInfo: TDriveInfo;
begin
  FDriveInfoList.Clear;
  lbDrives.Clear;
  lbSpace.Clear;
  for Drive:='A' to 'Z' do begin
    DriveInfo := HSGetDriveInfo(Drive);
    if DriveInfo.DriveType <> dtError then begin
      FDriveInfoList.Add(DriveInfo);
      lbDrives.Items.AddObject(DriveInfo.DisplayName, DriveInfo);
      lbSpace.Items.AddObject(DriveInfo.DisplayName, DriveInfo);
    end else
      DriveInfo.Free;
  end;
  if lbDrives.Items.Count <> 0 then
    lbDrives.ItemIndex := 0;
end;

procedure TfMain.FormCreate(Sender: TObject);
var
  ACanvas: TControlCanvas;
begin
  FOldListBoxWndProc := lbDrives.WindowProc;
  lbDrives.WindowProc := ListBoxWndProc;
  FOldFlagsListBoxWndProc := lbFlags.WindowProc;
  lbFlags.WindowProc := FlagsListBoxWndProc;
  FDriveInfoList := TDriveInfoList.Create;
  ACanvas := TControlCanvas.Create;
  try
    ACanvas.Control := lbDrives;
    lbDrives.ItemHeight := ACanvas.TextHeight('A') + DriveImageList.Height +
      TopSpacing + MiddleSpacing + BottomSpacing;
  finally
    ACanvas.Free;
  end;
  RebuildDriveList;
end;

procedure TfMain.lbDrivesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  Icon: TIcon;
  CaptionRect: TRect;
  Focused: Boolean;
  DriveInfo: TDriveInfo;
begin
  DriveInfo := lbDrives.Items.Objects[Index] as TDriveInfo;
  // При рисовании, выделенный элемент (он же текущий, так как в примере ListBox
  // не допускает множественного выбора, будет рисоваться отличным от
  // рисования невыделенных элементов способом.
  Focused := [odFocused,odSelected] * State <> [];
  if Focused then begin
    lbDrives.Canvas.Font.Color := lbDrives.Font.Color;
    lbDrives.Canvas.Font.Style := lbDrives.Canvas.Font.Style + [fsBold];
    lbDrives.Canvas.Pen.Color := clBtnShadow;
    lbDrives.Canvas.Brush.Color := lbDrives.Color;
    lbDrives.Canvas.RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom,
      5, 5);
  end else
    lbDrives.Canvas.FillRect(Rect);
  // Отобразим нужную для логического диска пиктограмму в центре элемента
  // ListBox
  Icon := TIcon.Create;
  try
    DriveImageList.GetIcon(Integer(DriveInfo.DriveType), Icon);
    lbDrives.Canvas.Draw ((Rect.Right - Rect.Left) div 2 -
      DriveImageList.Width + LeftSpacing,
      Rect.Top+TopSpacing, Icon);
  finally
    Icon.Free;
  end;
  // Надпись (имя логического диска) должна быть расположена под пиктограммой
  // по центру элемента.
  SetBkMode (lbDrives.Canvas.Handle, TRANSPARENT);
  CaptionRect := Rect;
  Inc(CaptionRect.Top, TopSpacing + MiddleSpacing + DriveImageList.Height);
  Dec(CaptionRect.Bottom, BottomSpacing);
  DrawTextEx (lbDrives.Canvas.Handle, PChar(lbDrives.Items[Index]), -1,
    CaptionRect, DT_CENTER or DT_SINGLELINE or DT_END_ELLIPSIS or DT_VCENTER,
    nil);
end;

type
  TListBoxAccess = class(TCustomListBox);
  
procedure ListBoxCNDrawItem(Control: TListBox; var Message: TWmDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Control.Canvas.Handle := hDC;
    Control.Canvas.Font := Control.Font;
    Control.Canvas.Brush := Control.Brush;
    if (Integer(itemID) >= 0) and (odSelected in State) then begin
      Control.Canvas.Brush.Color := clHighlight;
      Control.Canvas.Font.Color := clHighlightText
    end;
    if Integer(itemID) >= 0 then
      TListBoxAccess(Control).DrawItem(itemID, rcItem, State)
    else
      Control.Canvas.FillRect(rcItem);
//      if odFocused in State then DrawFocusRect(hDC, rcItem);
    Control.Canvas.Handle := 0;
  end;
end;

procedure TfMain.ListBoxWndProc(var Message: TMessage);
begin
  if Message.Msg = CN_DRAWITEM then
    ListBoxCNDrawItem(lbDrives, TWMDrawItem(Message))
  else
    FOldListBoxWndProc (Message);
end;

procedure TfMain.SplitterMoved(Sender: TObject);
begin
  lbDrives.Invalidate;
  lbSpace.Invalidate;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FDriveInfoList.Free;
end;

function FormatDiskSize (Value: TLargeInteger): string;
const
  SizeUnits: array[1..5] of string = (' Bytes', ' KB', ' MB', ' GB', 'TB');
var
  SizeUnit: Integer;
  Temp: TLargeInteger;
  Size: Integer;
begin
  SizeUnit := 1;
  if Value < 1024 then
    Result := IntToStr(Value)
  else begin
    Temp := Value;
    while (Temp >= 1000*1024) and (SizeUnit <= 5) do begin
      Temp := Temp shr 10; //div 1024
      Inc(SizeUnit);
    end;
    Inc(SizeUnit);
    Size := (Temp shr 10); //div 1024
    Temp := Temp - (Size shl 10);
    if Temp > 1000 then
      Temp := 999;
    if Size > 100 then
      Result := IntToStr(Size)
    else if Size > 10 then
      Result := Format('%d%s%.1d', [Size, DecimalSeparator, Temp div 100])
    else
      Result := Format('%d%s%.2d', [Size, DecimalSeparator,
        Temp div 10])
  end;
  Result := Result + SizeUnits[SizeUnit];
end;

{ Новые флажки файловой системы, Platform SDK, October 2002 }
const
 FILE_VOLUME_QUOTAS = $00000020;
 FILE_SUPPORTS_SPARSE_FILES   = $00000040;
 FILE_SUPPORTS_REPARSE_POINTS = $00000080;
 FILE_SUPPORTS_OBJECT_IDS = $00010000;
 FILE_SUPPORTS_ENCRYPTION = $00020000;
 FS_FILE_ENCRYPTION = FILE_SUPPORTS_ENCRYPTION;
 FILE_NAMED_STREAMS = $00040000;
 FILE_READ_ONLY_VOLUME = $00080000;

procedure DrawFileSystemFlags (ListBox: TListBox; FileSystemFlags: DWORD);
begin
  ListBox.Clear;
  with ListBox.Items do begin
    AddObject('FILE_NAMED_STREAMS',
      TObject((FileSystemFlags and FILE_NAMED_STREAMS) <> 0));
    AddObject('FILE_READ_ONLY_VOLUME',
      TObject((FileSystemFlags and FILE_READ_ONLY_VOLUME) <> 0));
    AddObject('FILE_SUPPORTS_OBJECT_IDS',
      TObject((FileSystemFlags and FILE_SUPPORTS_OBJECT_IDS) <> 0));
    AddObject('FILE_SUPPORTS_REPARSE_POINTS',
      TObject((FileSystemFlags and FILE_SUPPORTS_REPARSE_POINTS) <> 0));
    AddObject('FILE_SUPPORTS_SPARSE_FILES',
      TObject((FileSystemFlags and FILE_SUPPORTS_SPARSE_FILES) <> 0));
    AddObject('FILE_VOLUME_QUOTAS',
      TObject((FileSystemFlags and FILE_VOLUME_QUOTAS) <> 0));
    AddObject('FS_CASE_IS_PRESERVED',
      TObject((FileSystemFlags and FS_CASE_IS_PRESERVED) <> 0));
    AddObject('FS_CASE_SENSITIVE',
      TObject((FileSystemFlags and FS_CASE_SENSITIVE) <> 0));
    AddObject('FS_FILE_COMPRESSION',
      TObject((FileSystemFlags and FS_FILE_COMPRESSION) <> 0));
    AddObject('FS_FILE_ENCRYPTION',
      TObject((FileSystemFlags and FS_FILE_ENCRYPTION) <> 0));
    AddObject('FS_PERSISTENT_ACLS',
      TObject((FileSystemFlags and FS_PERSISTENT_ACLS) <> 0));
    AddObject('FS_UNICODE_STORED_ON_DISK',
      TObject((FileSystemFlags and FS_UNICODE_STORED_ON_DISK) <> 0));
    AddObject('FS_VOL_IS_COMPRESSED',
      TObject((FileSystemFlags and FS_VOL_IS_COMPRESSED) <> 0));
  end;
end;

procedure TfMain.DisplayDriveInfo(DriveInfo: TDriveInfo);
var
  Bitmap: TBitmap;
  SpaceGauge: TSimpleGauge;
  SerialNo: string;
begin
  lbSerialNo.Visible := DriveInfo.InformationValid;
  lbUsed.Visible := DriveInfo.InformationValid;
  lbFree.Visible := DriveInfo.InformationValid;
  ShUsed.Visible := DriveInfo.InformationValid;
  ShFree.Visible := DriveInfo.InformationValid;
  lbFlags.Visible := DriveInfo.InformationValid;
  lbFileSystemFlags.Visible := DriveInfo.InformationValid;
  if DriveInfo.InformationValid then begin
    lbFileSystem.Caption := DriveInfo.FileSystemName;
    SerialNo := Format('%.8x', [DriveInfo.SerialNumber]);
    System.Insert('-', SerialNo, 5);
    lbSerialNo.Caption := SerialNo;
    lbFree.Caption := 'Free: '+FormatDiskSize (DriveInfo.FreeBytes);
    lbUsed.Caption := 'Used: '+FormatDiskSize (DriveInfo.TotalBytes -
      DriveInfo.FreeBytes);
    DrawFileSystemFlags (lbFlags, DriveInfo.FileSystemFlags);
    SpaceGauge := TSimpleGauge.Create(SpaceImage.ClientRect);
    SpaceGauge.Kind := sgkPie;
    SpaceGauge.Percent := DriveInfo.PercentUsed;
    try
      Bitmap := TBitmap.Create;
      try
        with Bitmap do begin
          Height := SpaceImage.Height;
          Width := SpaceImage.Width;
          Canvas.Brush.Color := gbVolumeInfo.Color;
          Canvas.FillRect (Rect(0, 0, Width, Height));
          SpaceGauge.Paint(Canvas);
        end;
        SpaceImage.Picture.Assign (Bitmap);
      finally
        Bitmap.Free;
      end;
    finally
      SpaceGauge.Free;
    end;
  end else begin
    SpaceImage.Picture := nil;
    lbFileSystem.Caption := '???';
  end;
end;

procedure TfMain.FlagsListBoxWndProc(var Message: TMessage);
begin
  if Message.Msg = CN_DRAWITEM then
    ListBoxCNDrawItem(lbFlags, TWMDrawItem(Message))
  else
    FOldFlagsListBoxWndProc (Message);
end;

{ TDriveInfo }

procedure TDriveInfo.FillInformation;
var
  DrivePath: string;
  OldErrorMode: UINT;
  AVolumeName, AFileSystemName : array [0..Pred(MAX_PATH)] of char;
  ComponentLength: DWORD;
begin
  if DriveType = dtError then
    Exit;
  DrivePath := DriveName + ':\';
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  FVolumeName := GetDefaultVolumeName;
  try
    if GetVolumeInformation(PChar(DrivePath), AVolumeName,
        SizeOf(AVolumeName),
        @SerialNumber, ComponentLength, FFileSystemFlags,
        AFileSystemName, SizeOf(AFileSystemName)) then begin
      FInformationValid := true;
      if AVolumeName[0] <> #0 then
        FVolumeName := AVolumeName;
      FFileSystemName := AFileSystemName;
    end;
    if not GetDiskFreeSpaceEx(PChar(DrivePath), FFreeBytes,
        FTotalBytes, @FTotalFree) then begin
      FFreeBytes := 0;
      FTotalBytes := 0;
      FTotalFree := 0;
    end;
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

function TDriveInfo.GetDefaultVolumeName: string;
begin
  case FDriveType of
  dtFloppy:
    Result := 'Floppy';
  dtFixed:
    Result := 'Fixed Drive';
  dtCDROM:
    Result := 'CDROM';
  dtNetwork:
    Result := 'Network Drive';
  dtRemovable:
    Result := 'Removable Drive';
  dtRamDisk:
    Result := 'RAM Drive';
  end;
end;

function TDriveInfo.GetDisplayName: string;
begin
  Result := VolumeName + ' ('+DriveName + ':)';
end;

procedure TfMain.lbDrivesClick(Sender: TObject);
begin
  DisplayDriveInfo(TDriveInfo(lbDrives.Items.Objects[lbDrives.ItemIndex]));
end;

procedure TfMain.lbSpaceDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  DriveInfo: TDriveInfo;
  ListBox: TListBox;
  Icon: TIcon;
  TextRect, GaugeRect: TRect;
  SpaceGauge: TSimpleGauge;
begin
  ListBox := Control as TListBox;
  DriveInfo := ListBox.Items.Objects[Index] as TDriveInfo;
  if [odFocused,odSelected] * State <> [] then begin
    ListBox.Canvas.Brush.Color := ListBox.Color;
    ListBox.Canvas.Font.Color := ListBox.Font.Color;
  end;
  ListBox.Canvas.FillRect(Rect);
  Icon := TIcon.Create;
  try
    SmallDrivesImageList.GetIcon(Integer(DriveInfo.DriveType), Icon);
    ListBox.Canvas.Draw (Rect.Left + 2, Rect.Top + 2, Icon);
  finally
    Icon.Free;
  end;
  TextRect := Rect;
  Inc(TextRect.Left, 8 + SmallDrivesImageList.Width);
  TextRect.Right := TextRect.Left + Min(lbDrives.Width, 150);
  DrawTextEx (ListBox.Canvas.Handle, PChar(ListBox.Items[Index]), -1,
    TextRect, DT_LEFT + DT_SINGLELINE + DT_VCENTER + DT_END_ELLIPSIS, nil);
  if DriveInfo.InformationValid then begin
    //Нарисуем шкалу свободного места на диске в процентах
    GaugeRect := Rect;
    GaugeRect.Left := TextRect.Right + 4;
    Inc(GaugeRect.Top, 2);
    Dec(GaugeRect.Bottom, 2);
    Dec(GaugeRect.Right, 4);
    SpaceGauge := TSimpleGauge.Create(GaugeRect);
    try
      SpaceGauge.Percent := DriveInfo.PercentUsed;
      SpaceGauge.Paint (ListBox.Canvas);
    finally
      SpaceGauge.Free;
    end;
  end;
end;

function TDriveInfo.GetPercentUsed: Integer;
begin
  if FInformationValid and (FTotalBytes <> 0) then begin
    Result := 100 - ((FFreeBytes * 100) div FTotalBytes);
  end else
    Result := 0;
end;

{ TDriveInfoList }

function TDriveInfoList.GetItems(I: Integer): TDriveInfo;
begin
  Result := TDriveInfo(inherited Items[I]);
end;

procedure TfMain.FormResize(Sender: TObject);
begin
  lbSpace.Invalidate;
end;

{ TSimpleGauge }

constructor TSimpleGauge.Create(ABoundsRect: TRect);
begin
  FBoundsRect := ABoundsRect;
  FKind := sgkBar;
  FColors[0] := clBlue;
  FColors[1] := clFuchsia;
  FColors[2] := clWhite;
  FColors[3] := clBlack;
end;

function TSimpleGauge.GetColor(const Index: Integer): TColor;
begin
  Result := FColors[Index];
end;

procedure TSimpleGauge.Paint(ACanvas: TCanvas);
begin
  FCanvas := ACanvas;
  if FKind = sgkBar then
    PaintBar
  else
    PaintPie;
end;

procedure TSimpleGauge.PaintBar;
var
  UsedRect, FreeRect: TRect;
begin
  UsedRect := FBoundsRect;
  FreeRect := FBoundsRect;
  UsedRect.Right := UsedRect.Left + ((FBoundsRect.Right - FBoundsRect.Left) *
    FPercent) div 100;
  FreeRect.Left := UsedRect.Right - 1;
  FCanvas.Brush.Color := UsedColor;
  FCanvas.FillRect(UsedRect);
  FCanvas.Brush.Color := FreeColor;
  FCanvas.FillRect(FreeRect);
  FCanvas.Brush.Color := FrameColor;
  FCanvas.FrameRect(UsedRect);
  FCanvas.FrameRect(FreeRect);
  FCanvas.Font.Color := TextColor;
  SetBkMode (FCanvas.Handle, TRANSPARENT);
  DrawTextEx(FCanvas.Handle, PChar(Format('%d%%', [FPercent])), -1,
    FBoundsRect, DT_CENTER + DT_SINGLELINE + DT_VCENTER, nil);
end;

function GetDarkColor (const SourceColor: TColor): TColor;
var
  H, S, L: Double;
begin
  RGBtoHSL (SourceColor, H, S, L);
  Result := HSLtoRGB(H, S, L - 0.2);
end;

procedure TSimpleGauge.PaintPie;
var
  PieRect, PieBottomRect: TRect;
  Angle, DPercent: Double;
  DarkUsedColor, DarkFreeColor: TColor;
  BottomLeftRgn, BottomRightRgn: HRGN;
  RectRgn, BottomEllipticRgn, TopEllipticRgn: HRGN;
  PieRectWidth, PieRectHeight: Integer;
  MiddleX: Integer;
begin
  DPercent := Percent;
  Angle := 2 * PI * DPercent / 100.0;
  PieRect := FBoundsRect;
  Dec(PieRect.Bottom, 25);
  BottomRightRgn := 0;
  PieRectWidth := PieRect.Right - PieRect.Left;
  PieRectHeight := PieRect.Bottom - PieRect.Top;
  MiddleX := PieRect.Left + PieRectWidth div 2
    - Trunc(PieRectWidth / 2 * cos(Angle));
  PieBottomRect := PieRect;
  OffsetRect (PieBottomRect, 0, 10);
  RectRgn := CreateRectRgnIndirect(Rect(PieRect.Left,
    PieRect.Top + PieRectHeight div 2,
    PieRect.Right, PieBottomRect.Bottom - PieRectHeight div 2));
  BottomEllipticRgn := CreateEllipticRgnIndirect(PieBottomRect);
  TopEllipticRgn := CreateEllipticRgnIndirect(PieRect);
  BottomLeftRgn := CreateRectRgn (0, 0, 0, 0);
  CombineRgn(BottomLeftRgn, RectRgn, BottomEllipticRgn, RGN_OR);
  CombineRgn(BottomLeftRgn, BottomLeftRgn, TopEllipticRgn, RGN_DIFF);
  DeleteObject(RectRgn);
  DeleteObject(BottomEllipticRgn);
  DeleteObject(TopEllipticRgn);
  if Percent > 50 then begin
    // Необходимо разделить нижнюю область круговой диаграммы на две области
    // разного цвета.
    // Создадим прямоугольный регион, охватывающий правую область диаграммы.
    RectRgn := CreateRectRgn (MiddleX, FBoundsRect.Top, FBoundsRect.Right,
      FBoundsRect.Bottom);
    // Создадим регион, который будет комбинацией из нижнего региона и правой
    // области диаграммы.
    // Для начала создадим пустой регион.
    BottomRightRgn := CreateRectRgn (0, 0, 0, 0);
    // И скопируем в него пересечение нижней области и правой области диаграммы.
    CombineRgn (BottomRightRgn, BottomLeftRgn, RectRgn, RGN_AND);
    DeleteObject(RectRgn);
    // Создадим прямоугольный регион, охватывающий левую область диаграммы.
    // Чтобы в месте пересечения регионов не получалось двойной линии обводки,
    // регион левой части намеренно сделан на один пиксель шире, чем требуется.
    RectRgn := CreateRectRgn (FBoundsRect.Left, FBoundsRect.Top, Succ(MiddleX),
      FBoundsRect.Bottom);
    // Скопируем в регион нижней части пересечение его и прямоугольной левой
    // части.
    CombineRgn (BottomLeftRgn, BottomLeftRgn, RectRgn, RGN_AND);
    DeleteObject(RectRgn);
  end;
  FCanvas.Pen.Color := FrameColor;
  DarkFreeColor := GetDarkColor (FreeColor);
  DarkUsedColor := GetDarkColor (UsedColor);
  // Нарисуем нижнюю область диаграммы (левую часть)
  FCanvas.Brush.Color := DarkFreeColor;
  FillRgn (FCanvas.Handle, BottomLeftRgn, FCanvas.Brush.Handle);
  FCanvas.Brush.Color := FrameColor;
  FrameRgn (FCanvas.Handle, BottomLeftRgn, FCanvas.Brush.Handle, 1, 1);
  if BottomRightRgn <> 0 then begin
    // Нарисуем нижнюю область диаграммы (правую часть)
    FCanvas.Brush.Color := DarkUsedColor;
    FillRgn (FCanvas.Handle, BottomRightRgn, FCanvas.Brush.Handle);
    FCanvas.Brush.Color := FrameColor;
    FrameRgn (FCanvas.Handle, BottomRightRgn, FCanvas.Brush.Handle, 1, 1);
  end;
  FCanvas.Brush.Color := FreeColor;
  FCanvas.Ellipse(PieRect);
  //TODO: To HSGdi.pas
  SetArcDirection (FCanvas.Handle, AD_CLOCKWISE);
  FCanvas.Brush.Color := UsedColor;
  FCanvas.Pie(PieRect.Left, PieRect.Top, PieRect.Right, PieRect.Bottom,
    PieRect.Left,
    PieRect.Top + PieRectHeight div 2,
    PieRect.Left + PieRectWidth div 2
      - Trunc(PieRectWidth / 2 * cos(Angle)),
    PieRect.Top + PieRectHeight div 2
      - Trunc((PieRectHeight / 2) * sin(Angle))
    );
  DeleteObject (BottomLeftRgn);
  if BottomRightRgn <> 0 then
    DeleteObject(BottomRightRgn);
end;

procedure TSimpleGauge.SetColor(const Index: Integer; const Value: TColor);
begin
  FColors[Index] := Value;
end;

{
  Рисование марки CheckBox'а.
  Идея взята из FlatStyle controls, автор Maik Porkert (www.flatstyle2k.com)
}
procedure DrawCheckBoxMark (Canvas: TCanvas; const Rect: TRect; Color: TColor);
begin
  with Canvas do begin
    FillRect(Rect);
    Pen.Color := Color;
    PenPos := Point(Rect.Left+2, Rect.Top+4);
    LineTo(Rect.Left+6, Rect.Top+8);
    PenPos := Point(Rect.Left+2, Rect.Top+5);
    LineTo(Rect.Left+5, Rect.Top+8);
    PenPos := Point(Rect.Left+2, Rect.Top+6);
    LineTo(Rect.Left+5, Rect.Top+9);
    PenPos := Point(Rect.Left+8, Rect.Top+2);
    LineTo(Rect.Left+4, Rect.Top+6);
    PenPos := Point(Rect.Left+8, Rect.Top+3);
    LineTo(Rect.Left+4, Rect.Top+7);
    PenPos := Point(Rect.Left+8, Rect.Top+4);
    LineTo(Rect.Left+5, Rect.Top+7);
  end;
end;

procedure TfMain.lbFlagsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  ListBox: TListBox;
  Checked: Boolean;
  CheckBoxRect: TRect;
begin
  ListBox := Control as TListBox;
  if [odFocused,odSelected] * State <> [] then begin
    ListBox.Canvas.Brush.Color := ListBox.Color;
    ListBox.Canvas.Font.Color := ListBox.Font.Color;
  end;
  ListBox.Canvas.FillRect(Rect);
  Checked := ListBox.Items.Objects[Index] <> nil;
  CheckBoxRect := Rect;
  CheckBoxRect.Right := Rect.Left + Rect.Bottom - Rect.Top; {Сделать квадратным}
  Inc(Rect.Left, CheckBoxRect.Right - CheckBoxRect.Left + 2);{Место для текста}
  if Checked then begin
    { Разместить CheckBox Mark по центру его места }
    Inc(CheckBoxRect.Left, ((CheckBoxRect.Right - CheckBoxRect.Left) - 6)
      div 2 - 2);
    Inc(CheckBoxRect.Top, ((CheckBoxRect.Bottom - CheckBoxRect.Top) - 6)
      div 2 - 2);
    DrawCheckBoxMark(ListBox.Canvas, CheckBoxRect, clMaroon);
  end;
  DrawText(ListBox.Canvas.Handle, PChar(ListBox.Items[Index]), -1, Rect,
    DT_SINGLELINE or DT_LEFT or DT_VCENTER);
end;

end.
