unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IniFiles, ComCtrls, OidFile, RaFile;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Edit3: TEdit;
    Label3: TLabel;
    Memo1: TMemo;
    Edit4: TEdit;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label5: TLabel;
    Edit5: TEdit;               
    Label6: TLabel;
    Edit6: TEdit;
    Label7: TLabel;
    ListBox1: TListBox;
    Edit7: TEdit;
    Button1: TButton;
    Button3: TButton;
    Button2: TButton;
    Button4: TButton;
    Edit1: TEdit;
    TabSheet3: TTabSheet;
    Button5: TButton;
    Edit2: TEdit;
    TabSheet4: TTabSheet;
    Button6: TButton;
    Edit8: TEdit;
    Button7: TButton;
    Button8: TButton;
    Edit9: TEdit;
    Label2: TLabel;
    Edit10: TEdit;
    Label8: TLabel;
    Edit11: TEdit;
    Label9: TLabel;
    Edit12: TEdit;
    Label10: TLabel;
    Edit13: TEdit;
    Label11: TLabel;
    Button9: TButton;
    Edit14: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormShow(Sender: TObject);
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
    procedure TreeView1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    function ShowOID(oid: string; oiddb: POID; nod: TTreeNode): integer;
    procedure ShowRA(radb_idx: PRA; nod: TTreeNode);
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
  SortStrings, Funcs;

resourcestring
  TITLE_OID = 'Object Identifiers';
  TITLE_RA = 'Registration Authorities';
  SItemAlreadyExists = 'Item already exists';
  SInvalidAlphaNumId = 'Invalid alphanumeric identifier';
  SAreYouSure = 'Are you sure?';
  SCreated_S = 'Created: %s';
  SNumberMustNotExceed_D = 'Number must not exceed %d';
  SNotAValidNumber = 'Not a valid number';

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

function TForm1.ShowOID(oid: string; oiddb: POID; nod: TTreeNode): integer;
var
  i: integer;
  asn1id1: string;
  oiddb2: POID;
begin
  result := 0;
  if oid = 'OID:' then
  begin
    nod := TreeView1.Items.AddChild(nod, TITLE_OID);
		nod.Data := nil;
  end
  else
  begin
    if oiddb^.ASNIds.Count = 0 then
      asn1id1 := ''
    else
      asn1id1 := oiddb^.ASNIds[0];
    if oiddb^.draft then
      nod := TreeView1.Items.AddChild(nod, Trim(oid+' '+asn1id1)+' [DRAFT]')
    else
      nod := TreeView1.Items.AddChild(nod, Trim(oid+' '+asn1id1));
    nod.Data := Pointer(StrToInt(oiddb^.FileId));
  end;
  for i := 0 to oiddb^.SubIds.Count-1 do
  begin
    CreateOidDef(oiddb2);
    try
      Inc(result);
      ReadOidFile(DBPath+FileIdPart(oiddb^.SubIds.Strings[i])+'.OID', oiddb2);
      if oiddb2^.FileId <> '' then
        (*result := result + *)ShowOid('OID:'+DotNotationPart(oiddb^.SubIds.Strings[i]), oiddb2, nod);
    finally
      FreeOidDef(oiddb2);
    end;
  end;
  if (oid = 'OID:') or (result < 125) then
    nod.Expand(false);
end;

procedure TForm1.ShowRA(radb_idx: PRA; nod: TTreeNode);
var
  i: integer;
  sectionName, personname: string;
  sl: TStringList;
  radb: PRA;
  nod2: TTreeNode;
begin
  nod := TreeView1.Items.AddChild(nod, TITLE_RA);
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
    nod2 := TreeView1.Items.AddChild(nod, 'RA:'+Copy(sl.Strings[i],1,Pos(#1,sl.Strings[i])-1));
    nod2.Data := Pointer(StrToInt(Copy(sl.Strings[i],1+Pos(#1,sl.Strings[i]),9999)));
    ComboBox1.Items.Add(Copy(sl.Strings[i],1,Pos(#1,sl.Strings[i])-1));
  end;
  sl.Free;
  nod.Expand(false);
end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  oiddb: POID;
  radb: PRA;
begin
  SaveChangesIfRequired;

  if Copy(TreeView1.Selected.Text, 1, 4) = 'OID:' then
  begin
    PageControl1.ActivePage := TabSheet1;
    CreateOidDef(oiddb);
    try
      ReadOidFile(DBPath+LeftPadStr(IntToStr(Integer(TreeView1.Selected.Data)),8,'0')+'.OID', oiddb);
      Label16.Caption := LeftPadStr(IntToStr(Integer(TreeView1.Selected.Data)),8,'0')+'.OID';
      Edit4.Text := Copy(TreeView1.Selected.Text, 1, Pos(' ',TreeView1.Selected.Text+' ')-1);
      Listbox1.Items.Text := oiddb^.ASNIds.Text;
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
  if Copy(TreeView1.Selected.Text, 1, 3) = 'RA:' then
  begin
    PageControl1.ActivePage := TabSheet2;
    CreateRaDef(radb);
    try
      ReadRaFile(DBPath+LeftPadStr(IntToStr(Integer(TreeView1.Selected.Data)),8,'0')+'.RA_', radb);
      Label17.Caption := LeftPadStr(IntToStr(Integer(TreeView1.Selected.Data)),8,'0')+'.RA_';
      Edit9.Text := Copy(TreeView1.Selected.Text, 1+Length('RA:'), 9999);
      Edit10.Text := JpnDateToStr(radb^.createddate);
      Edit11.Text := radb^.name;
      Edit12.Text := radb^.email;
      Edit13.Text := radb^.phone;
      Edit14.Text := JpnDateToStr(radb^.updateddate);
    finally
      FreeRaDef(radb);
    end;
  end;
  if TreeView1.Selected.Text = TITLE_OID then
  begin
    PageControl1.ActivePage := TabSheet3;
    Edit2.Text := '';
  end;
  if TreeView1.Selected.Text = TITLE_RA then
  begin
    PageControl1.ActivePage := TabSheet4;
    Edit8.Text := '';
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  nod, raroot: TTreeNode;
  oiddb: POID;
  radb_idx: PRA;
begin
  ComboBox1.Clear;
  TreeView1.Items.Clear;
  nod := TTreeNode.Create(Treeview1.Items);

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

  TreeView1.Selected := TreeView1.Items[0];
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
    if ListBox1.Items.Strings[i] = asn1id then ShowError(SItemAlreadyExists);
  end;
  if not Asn1IdValid(asn1id) then ShowError(SInvalidAlphaNumId);
  ListBox1.Items.Add(asn1id);
  if CheckBox1.Checked then
    TreeView1.Selected.Text := Trim(Edit4.Text + ' ' + GetAsn1Ids(true)) + ' [DRAFT]'
  else
    TreeView1.Selected.Text := Trim(Edit4.Text + ' ' + GetAsn1Ids(true));
  Edit7.Text := '';
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if (ListBox1.Items.Count > 0) and ListBox1.Selected[ListBox1.ItemIndex] then
  begin
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;
  if CheckBox1.Checked then
    TreeView1.Selected.Text := Trim(Edit4.Text + ' ' + GetAsn1Ids(true)) + ' [DRAFT]'
  else
    TreeView1.Selected.Text := Trim(Edit4.Text + ' ' + GetAsn1Ids(true));
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
  nod: TTreeNode;
  candidate: string;
  oiddb_parent, oiddb_this: POID;
  nextid: string;
begin
  if PageControl1.ActivePage = TabSheet1 then new_value := Edit1.Text;
  if PageControl1.ActivePage = TabSheet3 then new_value := Edit2.Text;

  new_value := Trim(new_value);
  if new_value = '' then exit;

  if not IsPositiveNumber(new_value) then ShowError(SNotAValidNumber);

  if PageControl1.ActivePage = TabSheet1 then
  begin
    oid := Edit4.Text + '.' + new_value;
    parent_oid := Edit4.Text;
  end
  else
  begin
    oid := 'OID:' + new_value;
    parent_oid := 'OID:';
  end;

  for i := 0 to TreeView1.Selected.Count-1 do
  begin
    candidate := Copy(TreeView1.Selected.Item[i].Text, 1, Pos(' ',TreeView1.Selected.Item[i].Text+' ')-1);
    if oid = candidate then ShowError(SItemAlreadyExists);
  end;

  if (parent_oid = 'OID:') and (StrToInt(new_value) > 2) then ShowError(Format(SNumberMustNotExceed_D, [2]));
  if (parent_oid = 'OID:0') and (StrToInt(new_value) > 39) then ShowError(Format(SNumberMustNotExceed_D, [39]));
  if (parent_oid = 'OID:1') and (StrToInt(new_value) > 39) then ShowError(Format(SNumberMustNotExceed_D, [39]));

  nextid := NextPossibleFileID(DBPath, 'OID');
  if nextid = '00000000' then nextid := '00000001'; // when creating the first OID, the root (00000000.OID) might not yet be created

  CreateOidDef(oiddb_parent);
  try
    ReadOidFile(DBPath+LeftPadStr(IntToStr(Integer(TreeView1.Selected.Data)),8,'0')+'.OID', oiddb_parent);
    if oiddb_parent^.FileId = '' then oiddb_parent^.FileId := '00000000';
    if oiddb_parent^.ParentFileId = '' then oiddb_parent^.ParentFileId := '00000000';
    oiddb_parent^.SubIds.Add(nextid+Copy(oid,1+Length('OID:'),9999));
    WriteOidFile(DBPath+LeftPadStr(IntToStr(Integer(TreeView1.Selected.Data)),8,'0')+'.OID', oiddb_parent);
    ComboBox1.Text := oiddb_parent^.ra;
  finally
    FreeOidDef(oiddb_parent);
  end;

  nod := TreeView1.Items.AddChild(TreeView1.Selected, oid);
  nod.Data := Pointer(StrToInt(nextid));

  CreateOidDef(oiddb_this);
  try
    ReadOidFile(DBPath+LeftPadStr(IntToStr(Integer(nod.Data)),8,'0')+'.OID', oiddb_this);
    oiddb_this^.FileId := LeftPadStr(IntToStr(Integer(nod.Data)),8,'0');
    oiddb_this^.DotNotation := Copy(oid,1+Length('OID:'),9999);
    oiddb_this^.createddate := Date;
    oiddb_this^.ra := ComboBox1.Text;
    oiddb_this^.ParentFileId := LeftPadStr(IntToStr(Integer(TreeView1.Selected.Data)),8,'0');
    oiddb_this^.ParentDotNotation := Copy(parent_oid,1+Length('OID:'),9999);
    WriteOidFile(DBPath+LeftPadStr(IntToStr(Integer(nod.Data)),8,'0')+'.OID', oiddb_this);
  finally
    FreeOidDef(oiddb_this);
  end;

  if PageControl1.ActivePage = TabSheet1 then Edit1.Text := '';
  if PageControl1.ActivePage = TabSheet3 then Edit2.Text := '';

  TreeView1.Selected := nod;

  ShowMessageFmt(SCreated_S, [oid]);
end;

procedure TForm1.Button7Click(Sender: TObject);
var
  nod: TTreeNode;
  new_value, candidate: string;
  i: integer;
  radb: PRA;
  nextid: string;
  radb_idx: PRA;
begin
  new_value := Edit8.Text;
  new_value := Trim(new_value);
  if new_value = '' then exit;

  for i := 0 to TreeView1.Selected.Count-1 do
  begin
    candidate := TreeView1.Selected.Item[i].Text;
    if 'RA:'+new_value = candidate then ShowError(SItemAlreadyExists);
  end;

  nextid := NextPossibleFileID(DBPath, 'RA_');
  if nextid = '00000000' then nextid := '00000001'; // when creating the first RA, the index file (00000000.RA_) might not yet be created

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
      radb_idx^.updateddate := Date;
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

  nod := TreeView1.Items.AddChild(TreeView1.Selected, 'RA:'+new_value);
  nod.Data := Pointer(StrToInt(nextid));
  TreeView1.Selected := nod;

  ComboBox1.Items.Add(new_value);

  Edit8.Text := '';

  ShowMessageFmt(SCreated_S, [new_value]);
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
  nod: TTreeNode;
  parent_oid, this_oid: string;
  i: integer;
  sl: TStringList;
  oiddb_this, oiddb_parent: POID;
begin
  if MessageDlg(SAreYouSure, mtConfirmation, mbYesNoCancel, 0) <> idYes then exit;

  Button6.Tag := 1;
  try

    this_oid := Edit4.Text;
    if TreeView1.Selected.Parent.Text = TITLE_OID then
      parent_oid := 'OID:'
    else
      parent_oid := Copy(TreeView1.Selected.Parent.Text, 1, Pos(' ', TreeView1.Selected.Parent.Text+' ')-1);

    nod := TreeView1.Selected;

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

    TreeView1.Selected := nod.Parent;
    TreeView1.Items.Delete(nod);
  finally
    Button6.Tag := 0;
  end;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  nod: TTreeNode;
  radb_idx: PRA;
  nam, sectionName: string;
  i: integer;
begin
  if MessageDlg(SAreYouSure, mtConfirmation, mbYesNoCancel, 0) <> idYes then exit;

  Button8.Tag := 1;
  try
    CreateRaDef(radb_idx);
    try
      if FileExists(DBPath+'00000000.RA_') then
        ReadRaFile(DBPath+'00000000.RA_', radb_idx);
      nam := LeftPadStr(IntToStr(Integer(TreeView1.Selected.Data)),8,'0');
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

    SysUtils.DeleteFile(DBPath+LeftPadStr(IntToStr(Integer(TreeView1.Selected.Data)),8,'0')+'.RA_');

    nod := TreeView1.Selected;
    TreeView1.Selected := nod.Parent;
    TreeView1.Items.Delete(nod);

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

procedure TForm1.Button2Click(Sender: TObject);
var
  modified: boolean;
  oiddb: POID;
  sl: TStringList;
begin
  // Attention: Do not rely on TreeView1.Selected.Text, because Button2.Click
  // will be called in TreeView1OnChange()!

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
    if Trim(oiddb^.description) <> Trim(sl.Text) then
    begin
      modified := true;
      oiddb^.Description := sl.Text;
    end;

    if Trim(oiddb^.ASNIds.Text) <> Trim(ListBox1.Items.Text) then
    begin
      modified := true;
      oiddb^.ASNIds.Text := ListBox1.Items.Text;
    end;

    if modified then
    begin
      oiddb^.updateddate := Date;
      WriteOidFile(DBPath+Label16.Caption, oiddb);
    end;
  finally
    FreeOidDef(oiddb);
    FreeAndNil(sl);
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

function TForm1.DBPath: string;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create('.\OIDPLUS.INI');
  try
    if not ini.ValueExists('SETTINGS', 'DATAPATH') then
    begin
      result := '';
      ini.WriteString('SETTINGS', 'DATAPATH', result);
      ini.UpdateFile;
    end
    else
    begin
      result := ini.ReadString('SETTINGS', 'DATAPATH', '');
    end;
    if (result <> '') and not DirectoryExists(result) then MkDir(result);
    if (result <> '') and not DirectoryExists(result) then
    begin
      ShowError('Cannot create database directory '+result);
    end;
  finally
    ini.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePage := TabSheet3;
  Randomize;
end;

procedure TForm1.SaveChangesIfRequired;
begin
  if Button6.Tag = 1 then exit; // Do not save the OID child data if it was just deleted
  if Button8.Tag = 1 then exit; // Do not save the RA child data if it was just deleted
  if PageControl1.ActivePage = TabSheet1 then Button2.Click; // Save changes
  if PageControl1.ActivePage = TabSheet2 then Button9.Click; // Save changes
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  SaveChangesIfRequired;
  CanClose := true;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    TreeView1.Selected.Text := Trim(Edit4.Text+' '+GetAsn1Ids(true))+' [DRAFT]'
  else
    TreeView1.Selected.Text := Trim(Edit4.Text+' '+GetAsn1Ids(true));
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
  // Attention: Do not rely on TreeView1.Selected.Text, because Button9.Click
  // will be called in TreeView1OnChange()!

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
    Beep;
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

procedure TForm1.TreeView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 46(*DEL*) then
  begin
    if Copy(TreeView1.Selected.Text, 1, 4) = 'OID:' then
    begin
      Button6.Click;
    end
    else if Copy(TreeView1.Selected.Text, 1, 3) = 'RA:' then
    begin
      Button8.Click;
    end
    else
    begin
      Beep;
    end;

    Key := 0;
  end;
end;

end.
