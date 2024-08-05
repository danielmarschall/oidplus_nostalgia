unit Main;

interface

uses
  WinTypes, WinProcs, SysUtils, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, Grids, Outline, ExtCtrls, OidFile, RaFile;

type
  TForm1 = class(TForm)
    Outline1: TOutline;
    Notebook1: TNotebook;
    Memo1: TMemo;
    Label2: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Button8: TButton;
    Edit9: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Button9: TButton;
    Edit14: TEdit;
    Label13: TLabel;
    Button5: TButton;
    Edit2: TEdit;
    Label14: TLabel;
    Edit8: TEdit;
    Button7: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Edit5: TEdit;
    Edit6: TEdit;
    ListBox1: TListBox;
    Edit7: TEdit;
    Button1: TButton;
    Button3: TButton;
    Panel2: TPanel;
    Button2: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Button6: TButton;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    procedure Outline1Change(Sender: TObject; Node: integer);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Edit8KeyPress(Sender: TObject; var Key: Char);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Edit7KeyPress(Sender: TObject; var Key: Char);
    procedure Edit2KeyPress(Sender: TObject; var Key: Char);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Outline1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Outline1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure ShowOID(oid: string; oiddb: POID; nod: integer);
    procedure ShowRA(radb_idx: PRA; nod: integer);
    function DBPath: string;
    function GetAsn1Ids(onlyfirst: boolean): string;
    procedure SaveChangesIfRequired;
    procedure ShowError(msg: string);
    function RemoveAllOidFiles(const parent_oid: string): string;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  SortStr, Funcs;

const
  TITLE_OID = 'Object Identifiers';
  TITLE_RA = 'Registration Authorities';

procedure Split(Delimiter: string; Str: string; ListOfStrings: TStrings) ;
var
  p: integer;
begin
  ListOfStrings.Clear;
  p := Pos(Delimiter, Str);
  while p > 0 do
  begin
    ListOfStrings.Add(Copy(Str, 1, p-1));
    Delete(Str, 1, p);
    p := Pos(Delimiter, Str);
  end;
  if Str <> '' then ListOfStrings.Add(Str);
end;

procedure ExpandNodeAndParents(nod: TOutlineNode);
begin
  if nod.Parent <> nil then ExpandNodeAndParents(nod.Parent);
  nod.Expand;
end;

type
  TWorkItem = class(TObject)
  public
    sectionName: string;
    oiddb: POID;
    ownsoiddb: boolean;
    nod: integer;
  end;

procedure TForm1.ShowOID(oid: string; oiddb: POID; nod: integer);
var
  i: integer;
  asn1id1: string;
  oiddb2: POID;
  ownsoiddb: boolean;
  l: TList;
  workItem: TWorkItem;
begin
  l := TList.Create;

  (* We need this work item queue, otherwise we get stack overflow if we call this method recursively *)
  workItem := TWorkItem.Create;
  workItem.sectionName := oid;
  workItem.oiddb := oiddb;
  workItem.ownsoiddb := false;
  workItem.nod := nod;
  l.Add(workItem);

  while l.Count > 0 do
  begin
    workItem := l.Items[l.Count-1];
    oid := workItem.sectionName;
    oiddb := workItem.oiddb;
    nod := workItem.nod;
    ownsoiddb := workItem.ownsoiddb;
    workItem.Free;
    l.Delete(l.Count-1);

    if oid = 'OID:' then
    begin
      nod := Outline1.AddChild(nod, TITLE_OID);
      Outline1.Items[nod].Data := nil;
    end
    else
    begin
      if oiddb^.ASNIds.Count = 0 then
        asn1id1 := ''
      else
        asn1id1 := oiddb^.ASNIds[0];
      if oiddb^.draft then
        nod := Outline1.AddChild(nod, Trim(oid+' '+asn1id1)+' [DRAFT]')
      else
        nod := Outline1.AddChild(nod, Trim(oid+' '+asn1id1));
      Outline1.Items[nod].Data := Pointer(StrToInt(oiddb^.FileId));
    end;
    for i := oiddb^.SubIds.Count-1 downto 0 do { need to go back in order for the ordering to be correct again }
    begin
      CreateOidDef(oiddb2);
      ReadOidFile(DBPath+FileIdPart(oiddb^.SubIds.Strings[i])+'.OID', oiddb2);
      if oiddb2^.FileId <> '' then
      begin
        workItem := TWorkItem.Create;
        workItem.sectionName := 'OID:'+DotNotationPart(oiddb^.SubIds.Strings[i]);
        workItem.oiddb := oiddb2;
        workItem.ownsoiddb := true;
        workItem.nod := nod;
        l.Add(workItem);
      end;
    end;
    if (oid = 'OID:') or (oiddb^.SubIds.Count < 125) then
      ExpandNodeAndParents(Outline1.Items[nod]);
    if ownsoiddb then FreeOidDef(oiddb);
  end;
  l.Free;
end;

procedure TForm1.ShowRA(radb_idx: PRA; nod: integer);
var
  i: integer;
  sectionName, personname: string;
  sl: TStringList;
  radb: PRA;
begin
  nod := Outline1.AddChild(nod, TITLE_RA);
  sl := TStringList.Create;
  for i := 0 to radb_idx^.SubIds.Count-1 do
  begin
    sectionName := Copy(radb_idx^.SubIds[i],9,9999);
    if sectionName = '' then continue;
    sl.Add(Trim(sectionName + #1 + Copy(radb_idx^.SubIds[i],1,8)));
  end;
  SortSL(sl);
  for i := 0 to sl.Count-1 do
  begin
    Outline1.Items[Outline1.AddChild(nod, 'RA:'+Copy(sl.Strings[i],1,Pos(#1,sl.Strings[i])-1))].Data
      := Pointer(StrToInt(Copy(sl.Strings[i],1+Pos(#1,sl.Strings[i]),9999)));
    ComboBox1.Items.Add(Copy(sl.Strings[i],1,Pos(#1,sl.Strings[i])-1));
  end;
  sl.Free;
  ExpandNodeAndParents(Outline1.Items[nod]);
end;

procedure TForm1.Outline1Change(Sender: TObject; Node: integer);
var
  oiddb: POID;
  radb: PRA;
  i: integer;
begin
  SaveChangesIfRequired;

  if Copy(Outline1.Items[Outline1.SelectedItem].Text, 1, 4) = 'OID:' then
  begin
    Notebook1.PageIndex := 0;
    CreateOidDef(oiddb);
    try
      ReadOidFile(DBPath+LeftPadStr(IntToStr(Integer(Outline1.Items[Outline1.SelectedItem].Data)),8,'0')+'.OID', oiddb);
      Label16.Caption := LeftPadStr(IntToStr(Integer(Outline1.Items[Outline1.SelectedItem].Data)),8,'0')+'.OID';
      Edit4.Text := Copy(Outline1.Items[Outline1.SelectedItem].Text, 1,
        Pos(' ',Outline1.Items[Outline1.SelectedItem].Text+' ')-1);
      ListBox1.Clear;
      for i := 0 to oiddb^.ASNIDs.Count-1 do
        ListBox1.Items.Add(oiddb^.ASNIDs.Strings[i]);
      CheckBox1.Checked := oiddb^.draft;
      Memo1.Text := oiddb^.Description;
      Edit3.Text := Memo1.Lines.Strings[0];
      Memo1.Lines.Delete(0);
      Memo1.Modified := false;
      ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(oiddb^.ra);
      Edit5.Text := JpnDateToStr(oiddb^.createddate);
      Edit6.Text := JpnDateToStr(oiddb^.updateddate);
    finally
      FreeOidDef(oiddb);
    end;
    Edit7.Text := '';
    Edit1.Text := '';
  end;
  if Copy(Outline1.Items[Outline1.SelectedItem].Text, 1, 3) = 'RA:' then
  begin
    Notebook1.PageIndex := 1;
    CreateRaDef(radb);
    try
      ReadRaFile(DBPath+LeftPadStr(IntToStr(Integer(Outline1.Items[Outline1.SelectedItem].Data)),8,'0')+'.RA_', radb);
      Label17.Caption := LeftPadStr(IntToStr(Integer(Outline1.Items[Outline1.SelectedItem].Data)),8,'0')+'.RA_';
      Edit9.Text := Copy(Outline1.Items[Outline1.SelectedItem].Text, 1+Length('RA:'), 9999);
      Edit10.Text := JpnDateToStr(radb^.createddate);
      Edit11.Text := radb^.name;
      Edit12.Text := radb^.email;
      Edit13.Text := radb^.phone;
      Edit14.Text := JpnDateToStr(radb^.updateddate);
    finally
      FreeRaDef(radb);
    end;
  end;
  if Outline1.Items[Outline1.SelectedItem].Text = TITLE_OID then
  begin
    Notebook1.PageIndex := 2;
    Edit2.Text := '';
  end;
  if Outline1.Items[Outline1.SelectedItem].Text = TITLE_RA then
  begin
    Notebook1.PageIndex := 3;
    Edit8.Text := '';
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  nod, raroot: integer;
  oiddb: POID;
  radb_idx: PRA;
begin
  ComboBox1.Clear;
  Outline1.Clear;
  nod := 0;

  CreateOidDef(oiddb);
  try
    if FileExists(DBPath+'00000000.OID') then
      ReadOidFile(DBPath+'00000000.OID', oiddb);
    ShowOID('OID:', oiddb, nod);
  finally
    FreeOidDef(oiddb);
  end;

  CreateRaDef(radb_idx);
  try
    if FileExists(DBPath+'00000000.RA_') then
      ReadRaFile(DBPath+'00000000.RA_', radb_idx);
    ShowRa(radb_idx, nod);
  finally
    FreeRaDef(radb_idx);
  end;

  Outline1Click(Outline1);
end;

function Asn1IdValid(asn1id: string): boolean;
var
  i: integer;
begin
  if asn1id = '' then
  begin
    result := false;
    exit;
  end;

  if not (asn1id[1] in ['a'..'z']) then
  begin
    result := false;
    exit;
  end;

  for i := 2 to Length(asn1id) do
  begin
    if not (asn1id[1] in ['a'..'z', 'A'..'Z', '0'..'9', '-']) then
    begin
      result := false;
      exit;
    end;
  end;

  result := true;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  asn1id: string;
  i: integer;
begin
  asn1id := Edit7.Text;
  if asn1id = '' then exit;
  for i := 0 to ListBox1.Items.Count-1 do
  begin
    if ListBox1.Items.Strings[i] = asn1id then ShowError('Item already exists');
  end;
  if not Asn1IdValid(asn1id) then ShowError('Invalid alphanumeric identifier');
  ListBox1.Items.Add(asn1id);
  if CheckBox1.Checked then
    Outline1.Items[Outline1.SelectedItem].Text := Trim(Edit4.Text + ' ' + GetAsn1Ids(true)) + ' [DRAFT]'
  else
    Outline1.Items[Outline1.SelectedItem].Text := Trim(Edit4.Text + ' ' + GetAsn1Ids(true));
  Edit7.Text := '';
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if (ListBox1.Items.Count > 0) and ListBox1.Selected[ListBox1.ItemIndex] then
  begin
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;
  if CheckBox1.Checked then
    Outline1.Items[Outline1.SelectedItem].Text := Trim(Edit4.Text + ' ' + GetAsn1Ids(true)) + ' [DRAFT]'
  else
    Outline1.Items[Outline1.SelectedItem].Text := Trim(Edit4.Text + ' ' + GetAsn1Ids(true));
end;

function IsPositiveNumber(str: string): boolean;
var
  i: integer;
begin
  if (str = '') then
  begin
    result := false;
    exit;
  end;

  result := true;
  for i := 1 to Length(str) do
  begin
    if not (str[i] in ['0'..'9']) then
    begin
      result := false;
      exit;
    end;
  end;

  if (str[1] = '0') and (str <> '0') then
  begin
    result := false;
    exit;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  i, di: integer;
  oid, parent_oid, new_value: string;
  nod: integer;
  candidate: string;
  oiddb_parent, oiddb_this: POID;
  nextid: string;
begin
  if Notebook1.PageIndex = 0 then new_value := Edit1.Text;
  if Notebook1.PageIndex = 2 then new_value := Edit2.Text;

  new_value := Trim(new_value);
  if new_value = '' then exit;

  if not IsPositiveNumber(new_value) then ShowError('Not a valid number');

  if Notebook1.PageIndex = 0 then
  begin
    oid := Edit4.Text + '.' + new_value;
    parent_oid := Edit4.Text;
  end
  else
  begin
    oid := 'OID:' + new_value;
    parent_oid := 'OID:';
  end;

  if Outline1.Items[Outline1.SelectedItem].HasItems then
  for i := Outline1.Items[Outline1.SelectedItem].GetFirstChild to Outline1.Items[Outline1.SelectedItem].GetLastChild do
  begin
    candidate := Copy(Trim(Outline1.Lines[i-1]), 1, Pos(' ',Trim(Outline1.Lines[i-1])+' ')-1);
    if oid = candidate then ShowError('Item already exists');
  end;

  if (parent_oid = 'OID:') and (StrToInt(new_value) > 2) then ShowError('Number must not exceed 2');
  if (parent_oid = 'OID:0') and (StrToInt(new_value) > 39) then ShowError('Number must not exceed 39');
  if (parent_oid = 'OID:1') and (StrToInt(new_value) > 39) then ShowError('Number must not exceed 39');

  nextid := NextPossibleFileID(DBPath, 'OID');
  (* when creating the first OID, the root (00000000.OID) might not yet be created *)
  if nextid = '00000000' then nextid := '00000001';

  CreateOidDef(oiddb_parent);
  try
    ReadOidFile(DBPath+LeftPadStr(IntToStr(Integer(Outline1.Items[Outline1.SelectedItem].Data)),8,'0')+'.OID', oiddb_parent);
    if oiddb_parent^.FileId = '' then oiddb_parent^.FileId := '00000000';
    if oiddb_parent^.ParentFileId = '' then oiddb_parent^.ParentFileId := '00000000';
    oiddb_parent^.SubIds.Add(nextid+Copy(oid,1+Length('OID:'),9999));
    WriteOidFile(DBPath+LeftPadStr(IntToStr(Integer(Outline1.Items[Outline1.SelectedItem].Data)),8,'0')+'.OID', oiddb_parent);
    ComboBox1.Text := oiddb_parent^.ra;
  finally
    FreeOidDef(oiddb_parent);
  end;

  nod := Outline1.AddChild(Outline1.SelectedItem, oid);
  Outline1.Items[nod].Data := Pointer(StrToInt(nextid));
  ExpandNodeAndParents(Outline1.Items[Outline1.SelectedItem]);
  Outline1.Refresh;

  CreateOidDef(oiddb_this);
  try
    ReadOidFile(DBPath+LeftPadStr(IntToStr(Integer(Outline1.Items[nod].Data)),8,'0')+'.OID', oiddb_this);
    oiddb_this^.FileId := LeftPadStr(IntToStr(Integer(Outline1.Items[nod].Data)),8,'0');
    oiddb_this^.DotNotation := Copy(oid,1+Length('OID:'),9999);
    oiddb_this^.createddate := Date;
    oiddb_this^.ra := ComboBox1.Text;
    oiddb_this^.ParentFileId := LeftPadStr(IntToStr(Integer(Outline1.Items[Outline1.SelectedItem].Data)),8,'0');
    oiddb_this^.ParentDotNotation := Copy(parent_oid,1+Length('OID:'),9999);
    WriteOidFile(DBPath+LeftPadStr(IntToStr(Integer(Outline1.Items[nod].Data)),8,'0')+'.OID', oiddb_this);
  finally
    FreeOidDef(oiddb_this);
  end;

  if Notebook1.PageIndex = 0 then Edit1.Text := '';
  if Notebook1.PageIndex = 2 then Edit2.Text := '';

  Outline1.Items[Outline1.SelectedItem].Expand;
  Outline1.SelectedItem := nod;

  ShowMessage('Created: ' + oid);
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  nod: integer;
  new_value, candidate: string;
  i: integer;
  radb: PRA;
  nextid: string;
  radb_idx: PRA;
begin
  new_value := Edit8.Text;
  new_value := Trim(new_value);
  if new_value = '' then exit;

  if Outline1.Items[Outline1.SelectedItem].HasItems then
  for i := Outline1.Items[Outline1.SelectedItem].GetFirstChild to Outline1.Items[Outline1.SelectedItem].GetLastChild do
  begin
    candidate := Trim(Outline1.Lines[i-1]);
    if 'RA:'+new_value = candidate then ShowError('Item already exists');
  end;

  nextid := NextPossibleFileID(DBPath, 'RA_');
  (* when creating the first RA, the index file (00000000.RA_) might not yet be created *)
  if nextid = '00000000' then nextid := '00000001';

  CreateRaDef(radb);
  try
    radb^.SelfId := nextid;
    radb^.SelfName := new_value;
    radb^.ParentId := '00000000';
    radb^.ParentName := '';
    radb^.name := '';
    radb^.email := '';
    radb^.phone := '';
    radb^.createddate := Date;
    radb^.updateddate := 0;
    WriteRaFile(DBPath+nextid+'.RA_', radb);
  finally
    FreeRaDef(radb);
  end;

  CreateRaDef(radb_idx);
  try
    if FileExists(DBPath+'00000000.RA_') then
    begin
      ReadRaFile(DBPath+'00000000.RA_', radb_idx);
      radb^.updateddate := Date;
    end
    else
    begin
      radb_idx^.SelfId := '00000000';
      radb_idx^.SelfName := '';
      radb_idx^.ParentId := '00000000';
      radb_idx^.ParentName := '';
      radb_idx^.createddate := Date;
    end;
    radb_idx^.SubIds.Add(nextid+new_value);
    WriteRaFile(DBPath+'00000000.RA_', radb_idx);
  finally
    FreeRaDef(radb_idx);
  end;

  nod := Outline1.AddChild(Outline1.SelectedItem, 'RA:'+new_value);
  Outline1.Items[nod].Data := Pointer(StrToInt(nextid));
  Outline1.SelectedItem := nod;
  Outline1.Refresh;

  ComboBox1.Items.Add(new_value);

  Edit8.Text := '';

  ShowMessage('Created: ' + new_value);
end;

procedure IniReadSections(ini: TIniFile; Strings: TStrings);
const
  BufSize = 16384;
var
  Buffer, P: PChar;
  FFileName: string;
begin
  GetMem(Buffer, BufSize);
  try
    Strings.BeginUpdate;
    try
      Strings.Clear;
      FFileName := ini.FileName;
      if GetPrivateProfileString(nil, nil, nil, Buffer, BufSize,
        @FFileName[1]) <> 0 then
      begin
        P := Buffer;
        while P^ <> #0 do
        begin
          Strings.Add(StrPas(P));
          Inc(P, StrLen(P) + 1);
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

function TForm1.RemoveAllOidFiles(const parent_oid: string): string;
var
  DirInfo: TSearchRec;
  oiddb: POID;
begin
  FindFirst(DBPath+'????????.OID', faAnyFile, DirInfo);
  repeat
    CreateOidDef(oiddb);
    try
      ReadOidFile(DBPath+dirInfo.Name, oiddb);
      if Copy(oiddb^.DotNotation, 1, Length(parent_oid+'.')) = parent_oid+'.' then
      begin
        SysUtils.DeleteFile(DBPath+DirInfo.Name);
      end;
    finally
      FreeOidDef(oiddb);
    end;
  until FindNext(DirInfo) <> 0;
  FindClose(DirInfo);
end;

procedure TForm1.Button6Click(Sender: TObject);
var
  nod: TOutlineNode;
  parent_oid, this_oid: string;
  i: integer;
  sl: TStringList;
  oiddb_this, oiddb_parent: POID;
begin
  if MessageDlg('Are you sure?', mtConfirmation, mbYesNoCancel, 0) <> mrYes then exit;

  Button6.Tag := 1;
  try
    this_oid := Edit4.Text;

    if Outline1.Items[Outline1.SelectedItem].Parent.Text = TITLE_OID then
      parent_oid := 'OID:'
    else
      parent_oid := Copy(Outline1.Items[Outline1.SelectedItem].Parent.Text, 1,
                         Pos(' ', Outline1.Items[Outline1.SelectedItem].Parent.Text+' ')-1);

    nod := Outline1.Items[Outline1.SelectedItem];

    SysUtils.DeleteFile(DBPath+LeftPadStr(IntToStr(Integer(nod.Data)),8,'0')+'.OID');

    (* Delete children *)
    RemoveAllOidFiles(Copy(this_oid,1+Length('OID:'),9999));

    CreateOidDef(oiddb_parent);
    try
      ReadOidFile(DBPath+LeftPadStr(IntToStr(Integer(nod.Parent.Data)),8,'0')+'.OID', oiddb_parent);
      for i := 0 to oiddb_parent^.SubIds.Count-1 do
      begin
        if 'OID:'+DotNotationPart(oiddb_parent^.SubIds.Strings[i]) = this_oid then
        begin
          oiddb_parent^.SubIds.Delete(i);
          break;
        end;
      end;
      WriteOidFile(DBPath+LeftPadStr(IntToStr(Integer(nod.Parent.Data)),8,'0')+'.OID', oiddb_parent);
    finally
      FreeOidDef(oiddb_parent);
    end;

    Outline1.SelectedItem := nod.Parent.Index;
    Outline1.Delete(nod.Index);
  finally
    Button6.Tag := 0;
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  nod: TOutlineNode;
  radb_idx: PRA;
  nam, sectionName: string;
  i: integer;
begin
  if MessageDlg('Are you sure?', mtConfirmation, mbYesNoCancel, 0) <> mrYes then exit;

  Button8.Tag := 1;
  try
    CreateRaDef(radb_idx);
    try
      if FileExists(DBPath+'00000000.RA_') then
        ReadRaFile(DBPath+'00000000.RA_', radb_idx);
      nam := LeftPadStr(IntToStr(Integer(Outline1.Items[Outline1.SelectedItem].Data)),8,'0');
      for i := 0 to radb_idx^.SubIds.Count-1 do
      begin
        if Copy(radb_idx^.SubIds[i], 1, 8) = nam then
        begin
          sectionName := Copy(radb_idx^.SubIds[i], 9, 9999);
          radb_idx^.SubIds.Delete(i);
          radb_idx^.updateddate := Date;
          WriteRaFile(DBPath+'00000000.RA_', radb_idx);
          break;
        end;
      end;
    finally
      FreeRaDef(radb_idx);
    end;

    SysUtils.DeleteFile(DBPath+LeftPadStr(IntToStr(Integer(Outline1.Items[Outline1.SelectedItem].Data)),8,'0')+'.RA_');

    nod := Outline1.Items[Outline1.SelectedItem];
    Outline1.SelectedItem := nod.Parent.Index;
    Outline1.Delete(nod.Index);

    ComboBox1.Items.Delete(ComboBox1.Items.IndexOf(sectionName));
  finally
    Button8.Tag := 0;
  end;
end;

function RandomStr(len: integer): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to len do
  begin
    result := result + Chr(ord('A') + Random(26));
  end;
end;

function StringListToStr(sl: TStrings): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to sl.Count-1 do
  begin
    result := result + sl.Strings[i] + #13#10;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  modified: boolean;
  oiddb: POID;
  sl: TStringList;
  i: integer;
begin
  (* Attention: Do not rely on TreeView1.Selected.Text, because Button2.Click *)
  (* will be called in TreeView1OnChange()!                                   *)

  CreateOidDef(oiddb);
  sl := TStringList.Create;
  try
    ReadOidFile(DBPath+Label16.Caption, oiddb);

    modified := false;

    if oiddb^.ra <> ComboBox1.Text then
    begin
      modified := true;
      oiddb^.ra := ComboBox1.Text;
    end;

    if oiddb^.draft <> CheckBox1.Checked then
    begin
      modified := true;
      oiddb^.draft := CheckBox1.Checked;
    end;

    sl.Add(Edit3.Text);
    sl.AddStrings(Memo1.Lines);
    if Trim(oiddb^.description) <> Trim(StringListToStr(sl)) then
    begin
      modified := true;
      oiddb^.Description := StringListToStr(sl);
    end;

    if Trim(StringListToStr(oiddb^.ASNIds)) <> Trim(StringListToStr(ListBox1.Items)) then
    begin
      modified := true;
      oiddb^.ASNIds.Clear;
      for i := 0 to ListBox1.Items.Count-1 do
        oiddb^.ASNIds.Add(ListBox1.Items[i]);
    end;

    if modified then
    begin
      oiddb^.updateddate := Date;
      WriteOidFile(DBPath+Label16.Caption, oiddb);
    end;
  finally
    FreeOidDef(oiddb);
    sl.Free;
  end;
end;

function TForm1.GetAsn1Ids(onlyfirst: boolean): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to ListBox1.Items.Count-1 do
  begin
    if result = '' then
      result := ListBox1.Items.Strings[i]
    else if not onlyfirst then
      result := result + ',' + ListBox1.Items.Strings[i];
  end;
end;

function IniValueExists(ini: TIniFile; const Section, Ident: string): Boolean;
var
  S: TStrings;
begin
  S := TStringList.Create;
  try
    ini.ReadSection(Section, S);
    Result := S.IndexOf(Ident) > -1;
  finally
    S.Free;
  end;
end;

var
  MkDirTriedOnce: boolean; { Avoid that the debugger always shows the exception }
procedure MakeDirIfRequired(dirname: string);
begin
  if dirname[Length(dirname)] = '\' then dirname := Copy(dirname, 1, Length(dirname)-1);

  if not MkDirTriedOnce then
  begin
    try
      MkDir(dirname);
    except
    end;
    MkDirTriedOnce := true;
  end;
end;

function TForm1.DBPath: string;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create('.\OIDPLUS.INI');
  try
    if not IniValueExists(ini, 'SETTINGS', 'DATAPATH') then
    begin
      result := '';
      ini.WriteString('SETTINGS', 'DATAPATH', result);
      { ini.UpdateFile; }
    end
    else
    begin
      result := ini.ReadString('SETTINGS', 'DATAPATH', '');
    end;
    if result <> '' then MakeDirIfRequired(result);
  finally
    ini.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Notebook1.PageIndex := 2;
  Randomize;
end;

procedure TForm1.SaveChangesIfRequired;
begin
  if Button6.Tag = 1 then exit; (* Do not save the OID child data if it was just deleted *)
  if Button8.Tag = 1 then exit; (* Do not save the RA child data if it was just deleted *)
  if Notebook1.PageIndex = 0 then Button2.Click; { Save changes }
  if Notebook1.PageIndex = 1 then Button9.Click; { Save changes }
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveChangesIfRequired;
  CanClose := true;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    Outline1.Items[Outline1.SelectedItem].Text := Trim(Edit4.Text+' '+GetAsn1Ids(true))+' [DRAFT]'
  else
    Outline1.Items[Outline1.SelectedItem].Text := Trim(Edit4.Text+' '+GetAsn1Ids(true));
end;

procedure TForm1.ShowError(msg: string);
begin
  MessageDlg(msg, mtError, [mbOk], 0);
  Abort;
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  modified: boolean;
  radb: PRA;
begin
  (* Attention: Do not rely on TreeView1.Selected.Text, because Button9.Click *)
  (* will be called in TreeView1OnChange()!                                   *)

  CreateRaDef(radb);
  try
    ReadRaFile(DBPath+Label17.Caption, radb);

    modified := false;

    if radb^.name <> Edit11.Text then
    begin
      modified := true;
      radb^.name := Edit11.Text;
    end;

    if radb^.email <> Edit12.Text then
    begin
      modified := true;
      radb^.email := Edit12.Text;
    end;

    if radb^.phone <> Edit13.Text then
    begin
      modified := true;
      radb^.phone := Edit13.Text;
    end;

    if modified then
    begin
      radb^.updateddate := Date;
      WriteRaFile(DBPath+Label17.Caption, radb);
    end;
  finally
    FreeRaDef(radb);
  end;
end;

procedure TForm1.Edit8KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Button7.Click;
    Key := #0;
    Exit;
  end;
  if Key = #8(*backspace*) then exit;
  if Key in ['a'..'z'] then Key := UpCase(Key);
  if not (Key in ['A'..'Z', '-']) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
end;

procedure TForm1.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  If Key = 46(*DEL*) then
  begin
    Button3.Click;
    Key := 0;
  end;
end;

procedure TForm1.Edit7KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Button1.Click;
    Key := #0;
  end;
end;

procedure TForm1.Edit2KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Button5.Click;
    Key := #0;
  end;
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Button4.Click;
    Key := #0;
  end;
end;

procedure TForm1.Outline1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 46(*DEL*) then
  begin
    if Copy(Outline1.Items[Outline1.SelectedItem].Text, 1, 4) = 'OID:' then
    begin
      Button6.Click;
    end
    else if Copy(Outline1.Items[Outline1.SelectedItem].Text, 1, 3) = 'RA:' then
    begin
      Button8.Click;
    end
    else
    begin
      MessageBeep(0);
    end;

    Key := 0;
  end;
end;

procedure TForm1.Outline1Click(Sender: TObject);
begin
  Outline1Change(Sender, Outline1.SelectedItem);
end;

end.
