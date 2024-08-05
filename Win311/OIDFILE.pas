unit OIDFILE;

interface

uses
  SysUtils, Classes;

type
  POID = ^TOID;
  TOID = record
    FileId: string;
    DotNotation: string;
    ASNIds: TStringList;
    UnicodeLabels: TStringList;
    Description: string;
    SubIds: TStringList; (* first 8 chars are FileId, followed by DotNotation *)
    ParentFileId: string;
    ParentDotNotation: string;
    ra: string;
    draft: boolean;
    createddate: TDateTime;
    updateddate: TDateTime;
    UnknownLines: TStringList;
  end;

procedure CreateOidDef(var oid: POid);
procedure FreeOidDef(oid: POid);
procedure ClearOidDef(oid: POid);
function WriteOidFile(filename: string; oid: POid): boolean;
function ReadOidFile(filename: string; oid: POid): boolean;

(* For the strings in the list "SubIds": *)
function FileIdPart(s: string): string;
function DotNotationPart(s: string): string;

implementation

uses
  Funcs;

const
  WANT_VERS = '2022';

procedure CreateOidDef(var oid: POid);
begin
  oid := nil;
  New(oid);

  if oid <> nil then
  begin
    oid^.FileId := '';
    oid^.DotNotation := '';
    oid^.Description := '';
    oid^.ParentFileId := '';
    oid^.ParentDotNotation := '';
    oid^.ASNIds := TStringList.Create;
    oid^.UnicodeLabels := TStringList.Create;
    oid^.SubIds := TStringList.Create;
    oid^.ra := '';
    oid^.draft := false;
    oid^.createddate := 0;
    oid^.updateddate := 0;
    oid^.UnknownLines := TStringList.Create;
  end
  else
  begin
    raise Exception.Create('CreateOidDef failed! (GetMem returned nil)');
  end;
end;

procedure FreeOidDef(oid: POid);
begin
  if oid <> nil then
  begin
    oid^.ASNIds.Free;
    oid^.UnicodeLabels.Free;
    oid^.SubIds.Free;
    oid^.UnknownLines.Free;
    Dispose(oid);
    oid := nil;
  end
  else
  begin
    raise Exception.Create('FreeOidDef failed! (Argument is nil)');
  end;
end;

procedure ClearOidDef(oid: POid);
begin
  oid^.FileId := '';
  oid^.DotNotation := '';
  oid^.Description := '';
  oid^.ParentFileId := '';
  oid^.ParentDotNotation := '';
  oid^.ASNIds.Clear;
  oid^.UnicodeLabels.Clear;
  oid^.SubIds.Clear;
  oid^.ra := '';
  oid^.draft := false;
  oid^.createddate := 0;
  oid^.updateddate := 0;
  oid^.UnknownLines.Clear;
end;

procedure ListBubbleSortSubIds(oid: POid);
var
  n, i: integer;
  a, b: string;
  swapped: boolean;
begin
  n := oid^.SubIds.Count;
  while n>1 do
  begin
    i := 0;
    swapped := false;
    while i<n-1 do
    begin
      a := DotNotationPart(oid^.SubIds.Strings[i]);
      b := DotNotationPart(oid^.SubIds.Strings[i+1]);
      if CompareOID(a, b) > 0 then
      begin
        oid^.SubIds.Exchange(i, i+1);
        swapped := true;
      end;
      Inc(i);
    end;
    if not swapped then break;
    Dec(n);
  end;
end;

function WriteOidFile(filename: string; oid: POid): boolean;
var
  f: Text;
  i: integer;
  lines: TStringList;
  sTmp: string;
  desc: string;
begin
  Assign(f, filename);
  try
    {$I-}
    Rewrite(f);
    {$I+}
    if IoResult <> 0 then
    begin
      WriteOidFile := false;
      (* Must not call Close(f) if file was never opened *)
      Exit;
    end;

    WriteLn(f, 'VERS' + WANT_VERS);

    WriteLn(f, 'SELF' + oid^.FileId + oid^.DotNotation);

    WriteLn(f, 'SUPR' + oid^.ParentFileId + oid^.ParentDotNotation);

    (* Sort sub IDs *)
    ListBubbleSortSubIds(oid);

    for i := 0 to oid^.SubIds.Count-1 do
    begin
      sTmp := oid^.SubIds.Strings[i];
      WriteLn(f, 'CHLD' + sTmp);
    end;

    for i := 0 to oid^.AsnIds.Count-1 do
    begin
      sTmp := oid^.AsnIds.Strings[i];
      WriteLn(f, 'ASN1' + sTmp);
    end;

    for i := 0 to oid^.UnicodeLabels.Count-1 do
    begin
      sTmp := oid^.UnicodeLabels.Strings[i];
      WriteLn(f, 'UNIL' + sTmp);
    end;

    desc := Trim(oid^.Description);
    if desc <> '' then
    begin
      lines := TStringList.Create;
      SplitStrToList(desc, lines, #13#10);
      for i := 0 to lines.Count-1 do
      begin
        sTmp := lines.Strings[i];
        WriteLn(f, 'DESC' + sTmp);
      end;
      lines.Free;
    end;

    if oid^.ra <> '' then
      WriteLn(f, 'RA__' + oid^.ra);

    if oid^.draft then
      WriteLn(f, 'DRFT' + '1')
    else
      WriteLn(f, 'DRFT' + '0');

    if oid^.createddate > 1 then
      WriteLn(f, 'CDAT' + JpnDateToStr(oid^.createddate));

    if oid^.updateddate > 1 then
      WriteLn(f, 'UDAT' + JpnDateToStr(oid^.updateddate));

    for i := 0 to oid^.UnknownLines.Count-1 do
    begin
      WriteLn(f, oid^.UnknownLines.Strings[i]);
    end;
  finally
    Close(f);
  end;

  WriteOidFile := true;
end;

function ReadOidFile(filename: string; oid: POid): boolean;
var
  f: Text;
  line, cmd: string;
  version: string;
begin
  ClearOidDef(oid);
  version := '';

  Assign(f, filename);
  {$I-}
  Reset(f);
  {$I+}
  if IoResult <> 0 then
  begin
    ReadOidFile := false;
    (* Must not call Close(f) if file was never opened *)
    Exit;
  end;

  while not EOF(f) do
  begin
    ReadLn(f, line);
    cmd := Copy(line,1,4);
    Delete(line,1,4);

    if cmd = 'VERS' then
    begin
      version := line;
    end
    else if cmd = 'SELF' then
    begin
      oid^.FileId := Copy(line,1,8);
      Delete(line,1,8);
      oid^.DotNotation := line;
    end
    else if cmd = 'SUPR' then
    begin
      oid^.ParentFileId := FileIdPart(line);
      oid^.ParentDotNotation := DotNotationPart(line);
    end
    else if cmd = 'CHLD' then
    begin
      oid^.SubIds.Add(line);
    end
    else if cmd = 'ASN1' then
    begin
      oid^.ASNIds.Add(line);
    end
    else if cmd = 'UNIL' then
    begin
      oid^.UnicodeLabels.Add(line);
    end
    else if cmd = 'DESC' then
    begin
      oid^.Description := oid^.Description + line + #13#10;
    end
    else if cmd = 'RA__' then
    begin
      oid^.ra := line;
    end
    else if cmd = 'DRFT' then
    begin
      oid^.draft := line = '1';
    end
    else if cmd = 'CDAT' then
    begin
      oid^.createddate := JpnStrToDate(line);
    end
    else if cmd = 'UDAT' then
    begin
      oid^.updateddate := JpnStrToDate(line);
    end
    else
    begin
      oid^.UnknownLines.Add(cmd + line);
    end;
  end;

  (* Sort sub IDs *)
  ListBubbleSortSubIds(oid);

  (* Remove last CRLF *)
  oid^.Description := Copy(oid^.Description, 1, Length(oid^.Description)-Length(#13#10));

  (* Check if everything is correct *)
  ReadOidFile := (version = WANT_VERS) and (oid^.FileId <> '');

  Close(f);
end;

function FileIdPart(s: string): string;
begin
  FileIdPart := Copy(s,1,8);
end;

function DotNotationPart(s: string): string;
begin
  Delete(s,1,8);
  DotNotationPart := s;
end;

end.

