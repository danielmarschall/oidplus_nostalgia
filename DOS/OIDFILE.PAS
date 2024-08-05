unit OIDFILE;

(************************************************)
(* OIDFILE.PAS                                  *)
(* Author:   Daniel Marschall                   *)
(* Revision: 2024-08-04                         *)
(* License:  Apache 2.0                         *)
(* This file contains:                          *)
(* - Functions to handle an OID ASCII format    *)
(************************************************)

interface

uses
  StrList;

type
  POID = ^TOID;
  TOID = packed record
    FileId: string;
    DotNotation: string;
    ASNIds: PStringList;
    UnicodeLabels: PStringList;
    Description: string;
    SubIds: PStringList; (* first 8 chars are FileId, followed by DotNotation *)
    ParentFileId: string;
    ParentDotNotation: string;
    ra: string;
    draft: boolean;
    createddate: string;
    updateddate: string;
    UnknownLines: PStringList;
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
  VtsFuncs, OidUtils, Crt;

const
  WANT_VERS = '2022';

procedure CreateOidDef(var oid: POid);
begin
  oid := nil;
  GetMem(oid, SizeOf(TOID));

  if oid <> nil then
  begin
    oid^.FileId := '';
    oid^.DotNotation := '';
    oid^.Description := '';
    oid^.ParentFileId := '';
    oid^.ParentDotNotation := '';
    CreateList(oid^.ASNIds);
    CreateList(oid^.UnicodeLabels);
    CreateList(oid^.SubIds);
    oid^.ra := '';
    oid^.draft := false;
    oid^.createddate := '1900-01-01';
    oid^.updateddate := '1900-01-01';
    CreateList(oid^.UnknownLines);
  end
  else
  begin
    Beep;
    WriteLn('CreateOidDef failed! (GetMem returned nil)');
    ReadKey;
  end;
end;

procedure FreeOidDef(oid: POid);
begin
  if oid <> nil then
  begin
    FreeList(oid^.ASNIds);
    FreeList(oid^.UnicodeLabels);
    FreeList(oid^.SubIds);
    FreeList(oid^.UnknownLines);
    FreeMem(oid, SizeOf(TOID));
    oid := nil;
  end
  else
  begin
    Beep;
    WriteLn('FreeOidDef failed! (Argument is nil)');
    ReadKey;
  end;
end;

procedure ClearOidDef(oid: POid);
begin
  oid^.FileId := '';
  oid^.DotNotation := '';
  oid^.Description := '';
  oid^.ParentFileId := '';
  oid^.ParentDotNotation := '';
  ListClear(oid^.ASNIds);
  ListClear(oid^.UnicodeLabels);
  ListClear(oid^.SubIds);
  oid^.ra := '';
  oid^.draft := false;
  oid^.createddate := '1900-01-01';
  oid^.updateddate := '1900-01-01';
  ListClear(oid^.UnknownLines);
end;

procedure ListBubbleSortSubIds(oid: POid);
var
  n, i: integer;
  a, b: string;
  swapped: boolean;
begin
  n := ListCount(oid^.SubIds);
  while n>1 do
  begin
    i := 0;
    swapped := false;
    while i<n-1 do
    begin
      a := DotNotationPart(ListGetElement(oid^.SubIds, i));
      b := DotNotationPart(ListGetElement(oid^.SubIds, i+1));
      if CompareOID(a, b) > 0 then
      begin
        ListSwapElement(oid^.SubIds, i, i+1);
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
  lines: PStringList;
  sTmp: string;
  desc: string;
begin
  Assign(f, filename);

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

  for i := 0 to ListCount(oid^.SubIds)-1 do
  begin
    sTmp := ListGetElement(oid^.SubIds, i);
    WriteLn(f, 'CHLD' + sTmp);
  end;

  for i := 0 to ListCount(oid^.AsnIds)-1 do
  begin
    sTmp := ListGetElement(oid^.AsnIds, i);
    WriteLn(f, 'ASN1' + sTmp);
  end;

  for i := 0 to ListCount(oid^.UnicodeLabels)-1 do
  begin
    sTmp := ListGetElement(oid^.UnicodeLabels, i);
    WriteLn(f, 'UNIL' + sTmp);
  end;

  desc := Trim(oid^.Description);
  if desc <> '' then
  begin
    CreateList(lines);
    SplitStrToList(desc, lines, #13#10);
    for i := 0 to ListCount(lines)-1 do
    begin
      sTmp := ListGetElement(lines, i);
      WriteLn(f, 'DESC' + sTmp);
    end;
    FreeList(lines);
  end;

  if oid^.ra <> '' then
    WriteLn(f, 'RA__', + oid^.ra);

  if oid^.draft then
    WriteLn(f, 'DRFT', '1')
  else
    WriteLn(f, 'DRFT', '0');

  if oid^.createddate <> '' then
    WriteLn(f, 'CDAT', oid^.createddate);

  if oid^.updateddate <> '' then
    WriteLn(f, 'UDAT', oid^.updateddate);

  for i := 0 to ListCount(oid^.UnknownLines)-1 do
  begin
    WriteLn(f, ListGetElement(oid^.UnknownLines, i));
  end;

  Close(f);

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
      ListAppend(oid^.SubIds, line);
    end
    else if cmd = 'ASN1' then
    begin
      ListAppend(oid^.ASNIds, line);
    end
    else if cmd = 'UNIL' then
    begin
      ListAppend(oid^.UnicodeLabels, line);
    end
    else if cmd = 'DESC' then
    begin
      if Length(oid^.Description) + Length(line) + 2 <= 255 then
      begin
        oid^.Description := oid^.Description + line + #13#10;
      end;
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
      oid^.createddate := line;
    end
    else if cmd = 'UDAT' then
    begin
      oid^.updateddate := line;
    end
    else
    begin
      ListAppend(oid^.UnknownLines, cmd + line);  
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
