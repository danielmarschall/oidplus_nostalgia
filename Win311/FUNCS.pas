unit FUNCS;

interface

uses
  SysUtils, Classes;

function CompareOID(a, b: string): integer;
procedure SplitStrToList(str: string; list: TStringList; separator: string);
function LeftPadStr(s: string; n: integer; ch: char): string;
function NextPossibleFileID(const path: string; const ext: string): string;
function Trim(const S: string): string;
function JpnStrToDate(const DateStr: string): TDateTime;
function JpnDateToStr(Date: TDateTime): string;

implementation

procedure SplitStrToList(str: string; list: TStringList; separator: string);
var
  p: integer;
begin
  repeat
    p := Pos(separator, str);
    if p = 0 then
    begin
      list.Add(str);
      Exit;
    end
    else
    begin
      list.Add(Copy(str, 1, p-1));
      str := copy(str, p+Length(separator), Length(str)-p);
    end;
  until str = '';
end;

procedure OIDtoArcList(oid: string; list: TStringList);
begin
  SplitStrToList(oid, list, '.');
end;

function LeftPadStr(s: string; n: integer; ch: char): string;
begin
  while Length(s) < n do
  begin
    s := ch + s;
  end;
  LeftPadStr := s;
end;

function CompareEqualLengthString(a, b: string): integer;
var
  ao, bo, i: integer;
begin
  CompareEqualLengthString := 0;
  for i := 1 to Length(a) do
  begin
    ao := Ord(a[i]);
    bo := Ord(b[i]);
    if ao > bo then
    begin
      CompareEqualLengthString := 1;
      break;
    end;
    if ao < bo then
    begin
      CompareEqualLengthString := -1;
      break;
    end;
  end;
end;

function CompareNumericString(a, b: string): integer;
var
  EqualLength: integer;
begin
  if Length(b) > Length(a) then
    EqualLength := Length(b)
  else
    EqualLength := Length(a);
  a := LeftPadStr(a, EqualLength, '0');
  b := LeftPadStr(b, EqualLength, '0');
  CompareNumericString := CompareEqualLengthString(a, b);
end;

function CompareOIDArcList(a, b: TStringList): integer;
var
  x, y: string;
  i: integer;
  tmp: integer;
  maxcount: integer;
begin
  if b.Count > a.Count then
    maxcount := b.Count
  else
    maxcount := a.Count;
  for i := 0 to maxcount - 1 do
  begin
    if i >= a.Count then
      x := ''
    else
      x := a.Strings[i];

    if i >= b.Count then
      y := ''
    else
      y := b.Strings[i];

    if (x = '') and (y <> '') then
    begin
      CompareOIDArcList := -1;
      exit;
    end;

    if (x <> '') and (y = '') then
    begin
      CompareOIDArcList := 1;
      exit;
    end;

    if (x = '') and (y = '') then
    begin
      CompareOIDArcList := 0;
      exit;
    end;

    tmp := CompareNumericString(x, y);

    if tmp <> 0 then
    begin
      CompareOIDArcList := tmp;
      exit;
    end;
  end;
end;

function CompareOID(a, b: string): integer;
var
  la, lb: TStringList;
begin
  la := TStringList.Create;
  lb := TStringList.Create;
  OIDtoArcList(a, la);
  OIDtoArcList(b, lb);
  CompareOID := CompareOIDArcList(la, lb);
  la.Free;
  lb.Free;
end;

function ZeroPad(i: LongInt; n: integer): string;
var
  s: string;
begin
  s := IntToStr(i);
  ZeroPad := LeftPadStr(s, n, '0');
end;

function IsPositiveIntegerOrZero(s: string): boolean;
var
  i: integer;
begin
  IsPositiveIntegerOrZero := false;

  if Length(s) = 0 then exit;
  (*if (s[1] = '0') and (s <> '0') then exit;*)
  for i := 1 to Length(s) do
  begin
    if not (s[i] in ['0'..'9']) then exit;
  end;

  IsPositiveIntegerOrZero := true;
end;

function NextPossibleFileID(const path: string; const ext: string): string;
var
  DirInfo: TSearchRec;
  iId, imax: LongInt;
  sId: string;
begin
  FindFirst(path+'????????.'+ext, faAnyFile, DirInfo);
  imax := -1;
  repeat
    sId := Copy(DirInfo.Name, 1, 8);
    if IsPositiveIntegerOrZero(sId) then
    begin
      iId := StrToInt(sId);
      if iId > iMax then iMax := iId;
    end;
  until FindNext(DirInfo) <> 0;
  FindClose(DirInfo);
  result := ZeroPad(iMax+1, 8);
end;

{ Source: Delphi 4 }
function Trim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function JpnStrToDate(const DateStr: string): TDateTime;
var
  Year, Month, Day: Word;
begin
  (* DateStr must be YYYY-MM-DD *)
  Year := StrToInt(Copy(DateStr, 1, 4));
  Month := StrToInt(Copy(DateStr, 6, 2));
  Day := StrToInt(Copy(DateStr, 9, 2));
  Result := EncodeDate(Year, Month, Day);
end;

function JpnDateToStr(Date: TDateTime): string;
var
  Year, Month, Day: Word;
begin
  DecodeDate(Date, Year, Month, Day);
  Result := Format('%.4d-%.2d-%.2d', [Year, Month, Day]);
end;

end.
