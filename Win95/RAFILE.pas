unit RAFILE;

interface

uses
  SysUtils, Classes;

type
  PRA = ^TRA;
  TRA = record
    SelfId: string;
    SelfName: string;
    ParentId: string;
    ParentName: string;
    SubIds: TStringList;
    name: string;
    email: string;
    phone: string;
    createddate: TDateTime;
    updateddate: TDateTime;
    UnknownLines: TStringList;
  end;

procedure CreateRaDef(var ra: PRA);
procedure FreeRaDef(ra: PRA);
procedure ClearRaDef(ra: PRA);
function WriteRaFile(filename: string; ra: PRA): boolean;
function ReadRaFile(filename: string; ra: PRA): boolean;

implementation

uses
  Funcs, Math;

const
  WANT_VERS = '2024';

procedure CreateRaDef(var ra: PRA);
begin
  ra := nil;
  New(ra);

  if ra <> nil then
  begin
    ra^.SelfId := '';
    ra^.SelfName := '';
    ra^.ParentId := '';
    ra^.ParentName := '';
    ra^.SubIds := TStringList.Create;
    ra^.name := '';
    ra^.email := '';
    ra^.phone := '';
    ra^.createddate := 0;
    ra^.updateddate := 0;
    ra^.UnknownLines := TStringList.Create;
  end
  else
  begin
    raise Exception.Create('CreateRaDef failed! (GetMem returned nil)');
  end;
end;

procedure FreeRaDef(ra: PRA);
begin
  if ra <> nil then
  begin
    FreeAndNil(ra^.SubIds);
    FreeAndNil(ra^.UnknownLines);
    Dispose(ra);
    ra := nil;
  end
  else
  begin
    raise Exception.Create('FreeRaDef failed! (Argument is nil)');
  end;
end;

procedure ClearRaDef(ra: PRA);
begin
  ra^.SelfId := '';
  ra^.SelfName := '';
  ra^.ParentId := '';
  ra^.ParentName := '';
  ra^.SubIds.Clear;
  ra^.name := '';
  ra^.email := '';
  ra^.phone := '';
  ra^.createddate := 0;
  ra^.updateddate := 0;
  ra^.UnknownLines.Clear;
end;

function WriteRaFile(filename: string; ra: PRA): boolean;
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
      WriteRaFile := false;
      (* Must not call Close(f) if file was never opened *)
      Exit;
    end;

    WriteLn(f, 'VERS' + WANT_VERS);

    WriteLn(f, 'SELF' + ra^.SelfId + ra^.SelfName);

    WriteLn(f, 'SUPR' + ra^.ParentId + ra^.ParentName);

    for i := 0 to ra^.SubIds.Count-1 do
    begin
      sTmp := ra^.SubIds.Strings[i];
      WriteLn(f, 'CHLD' + sTmp);
    end;

    if ra^.Name <> '' then
      WriteLn(f, 'NAME' + ra^.Name);

    if ra^.EMail <> '' then
      WriteLn(f, 'MAIL' + ra^.EMail);

    if ra^.Phone <> '' then
      WriteLn(f, 'PHON' + ra^.Phone);

    if CompareValue(ra^.createddate,0) <> 0 then
      WriteLn(f, 'CDAT' + JpnDateToStr(ra^.createddate));

    if CompareValue(ra^.updateddate,0) <> 0 then
      WriteLn(f, 'UDAT' + JpnDateToStr(ra^.updateddate));

    for i := 0 to ra^.UnknownLines.Count-1 do
    begin
      WriteLn(f, ra^.UnknownLines.Strings[i]);
    end;
  finally
    Close(f);
  end;

  WriteRaFile := true;
end;

function ReadRaFile(filename: string; ra: PRA): boolean;
var
  f: Text;
  line, cmd: string;
  version: string;
begin
  ClearRaDef(ra);
  version := '';

  Assign(f, filename);
  {$I-}
  Reset(f);
  {$I+}
  if IoResult <> 0 then
  begin
    ReadRaFile := false;
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
      ra^.SelfId := Copy(line,1,8);
      Delete(line,1,8);
      ra^.SelfName := line;
    end
    else if cmd = 'SUPR' then
    begin
      ra^.ParentId := Copy(line,1,8);
      Delete(line,1,8);
      ra^.ParentName := line;
    end
    else if cmd = 'CHLD' then
    begin
      ra^.SubIds.Add(line);
    end
    else if cmd = 'NAME' then
    begin
      ra^.name := line;
    end
    else if cmd = 'MAIL' then
    begin
      ra^.email := line;
    end
    else if cmd = 'PHON' then
    begin
      ra^.phone := line;
    end
    else if cmd = 'CDAT' then
    begin
      ra^.createddate := JpnStrToDate(line);
    end
    else if cmd = 'UDAT' then
    begin
      ra^.updateddate := JpnStrToDate(line);
    end
    else
    begin
      ra^.UnknownLines.Add(cmd + line);
    end;
  end;

  (* Check if everything is correct *)
  ReadRaFile := (version = WANT_VERS);

  Close(f);
end;

end.

