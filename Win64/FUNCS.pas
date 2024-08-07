unit FUNCS;

interface

uses
  SysUtils, Classes, Math;

function CompareOID(a, b: string): integer;
procedure SplitStrToList(str: string; list: TStringList; separator: string);
function LeftPadStr(s: string; n: integer; ch: char): string;
function NextPossibleFileID(const path: string; const ext: string): string;
function JpnStrToDate(const DateStr: string): TDateTime;
function JpnDateToStr(Date: TDateTime): string;
procedure Split(Delimiter: string; Str: string; ListOfStrings: TStrings);
function Asn1IdValid(asn1id: string): boolean;
function UnicodeLabelValid(const arc: string; allowNumeric: boolean): boolean;
function IsPositiveNumber(str: string): boolean;
function RandomStr(len: integer): string;
function IsNumericString(const S: string): Boolean;

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
  EqualLength := Max(Length(a), Length(b));
  a := LeftPadStr(a, EqualLength, '0');
  b := LeftPadStr(b, EqualLength, '0');
  CompareNumericString := CompareEqualLengthString(a, b);
end;

function CompareOIDArcList(a, b: TStringList): integer;
var
  x, y: string;
  i: integer;
  tmp: integer;
begin
  for i := 0 to Max(a.Count, b.Count) - 1 do
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
  FreeAndNil(la);
  FreeAndNil(lb);
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

procedure Split(Delimiter: string; Str: string; ListOfStrings: TStrings);
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

function IsNumericString(const S: string): Boolean;
var
  I: Integer;
begin
  if S = '' then exit(false);
  Result := True;  // Assume the string is numeric
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ['0'..'9']) then
    begin
      Result := False;  // If any character is not a digit, the string is not numeric
      Break;
    end;
  end;
end;

function iri_char_valid(c: Char; firstchar, lastchar: boolean): boolean;
var
  v: integer;
begin
  // Please keep in sync with https://misc.daniel-marschall.de/oid-repository/api/oid_utils.inc.phps

	// see Rec. ITU-T X.660, clause 7.5

	if ((firstchar or lastchar) and (c = '-')) then exit(false);

	if (c = '-') then exit(true);
	if (c = '.') then exit(true);
	if (c = '_') then exit(true);
	if (c = '~') then exit(true);
	if ((c >= '0') and (c <= '9') and not firstchar) then exit(true);
	if ((c >= 'A') and (c <= 'Z')) then exit(true);
	if ((c >= 'a') and (c <= 'z')) then exit(true);

	v := Ord(c);

	if ((v >= $000000A0) and (v <= $0000DFFE)) then exit(true);
	if ((v >= $0000F900) and (v <= $0000FDCF)) then exit(true);
	if ((v >= $0000FDF0) and (v <= $0000FFEF)) then exit(true);
	if ((v >= $00010000) and (v <= $0001FFFD)) then exit(true);
	if ((v >= $00020000) and (v <= $0002FFFD)) then exit(true);
	if ((v >= $00030000) and (v <= $0003FFFD)) then exit(true);
	if ((v >= $00040000) and (v <= $0004FFFD)) then exit(true);
	if ((v >= $00050000) and (v <= $0005FFFD)) then exit(true);
	if ((v >= $00060000) and (v <= $0006FFFD)) then exit(true);
	if ((v >= $00070000) and (v <= $0007FFFD)) then exit(true);
	if ((v >= $00080000) and (v <= $0008FFFD)) then exit(true);
	if ((v >= $00090000) and (v <= $0009FFFD)) then exit(true);
	if ((v >= $000A0000) and (v <= $000AFFFD)) then exit(true);
	if ((v >= $000B0000) and (v <= $000BFFFD)) then exit(true);
	if ((v >= $000C0000) and (v <= $000CFFFD)) then exit(true);
	if ((v >= $000D0000) and (v <= $000DFFFD)) then exit(true);
	if ((v >= $000E1000) and (v <= $000EFFFD)) then exit(true);

	// Note: Rec. ITU-T X.660, clause 7.5.3 would also forbid ranges which are marked in ISO/IEC 10646 as "(This position shall not be used)"
	// But tool implementers should be tolerate them, since these limitations can be removed in future.

	exit(false);
end;

function UnicodeLabelValid(const arc: string; allowNumeric: boolean): boolean;
var
  i: integer;
begin
  // Please keep in sync with https://misc.daniel-marschall.de/oid-repository/api/oid_utils.inc.phps

	if (arc = '') then exit(false);

  if allowNumeric and IsNumericString(arc) then exit(true);

	// Question: Should we strip RTL/LTR characters?

	if (Copy(arc, 3, 2) = '--') then exit(false); // see Rec. ITU-T X.660, clause 7.5.4

  for i := 1 to Length(arc) do
  begin
		if not iri_char_valid(arc[i], i=1, i=Length(arc)) then exit(false);
	end;

	exit(true);
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

end.
