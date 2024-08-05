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
  i: integer;
  lines: TStringList;
  sTmp: string;
  desc: string;
  slOut: TStringList;
begin
  slOut := TStringList.Create;
  try
    slOut.Add('[1.3.6.1.4.1.37476.2.5.3.2]'); // DOS/311/95 ignore this, but it is important, because DOS/311/95 cannot handle a BOM in front of "VERS".

    slOut.Add('VERS' + WANT_VERS);

    slOut.Add('SELF' + ra^.SelfId + ra^.SelfName);

    slOut.Add('SUPR' + ra^.ParentId + ra^.ParentName);

    for i := 0 to ra^.SubIds.Count-1 do
    begin
      sTmp := ra^.SubIds.Strings[i];
      slOut.Add('CHLD' + sTmp);
    end;

    if ra^.Name <> '' then
      slOut.Add('NAME' + ra^.Name);

    if ra^.EMail <> '' then
      slOut.Add('MAIL' + ra^.EMail);

    if ra^.Phone <> '' then
      slOut.Add('PHON' + ra^.Phone);

    if CompareValue(ra^.createddate,0) <> 0 then
      slOut.Add('CDAT' + JpnDateToStr(ra^.createddate));

    if CompareValue(ra^.updateddate,0) <> 0 then
      slOut.Add('UDAT' + JpnDateToStr(ra^.updateddate));

    for i := 0 to ra^.UnknownLines.Count-1 do
    begin
      slOut.Add(ra^.UnknownLines.Strings[i]);
    end;

    try
      slOut.SaveToFile(filename, TEncoding.UTF8);
      result := true;
    except
      result := false;
    end;
  finally
    FreeAndNil(slOut);
  end;
end;

function ReadRaFile(filename: string; ra: PRA): boolean;
var
  line, cmd, tmp: string;
  version: string;
  slIn: TStringList;
begin
  ClearRaDef(ra);
  version := '';

  slIn := TStringList.Create;
  try
    try
      if not FileExists(filename) then exit(false);
      slIn.LoadFromFile(filename);
    except
      exit(false);
    end;

    for tmp in slIn do
    begin
      line := tmp;
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
        if cmd <> '[1.3' then
          ra^.UnknownLines.Add(cmd + line);
      end;
    end;

    (* Check if everything is correct *)
    ReadRaFile := (version = WANT_VERS);
  finally
    FreeAndNil(slIn);
  end;
end;

end.

