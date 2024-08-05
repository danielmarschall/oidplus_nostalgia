unit WEID_Delphi;

(*
 * WEID<=>OID Converter for Delphi
 * (c) Webfan.de, ViaThinkSoft
 * Revision 2023-08-10
 *)

(*
 * What is a WEID?
 *     A WEID (WEhowski IDentifier) is an alternative representation of an
 *     OID (Object IDentifier) defined by Till Wehowski.
 *     In OIDs, arcs are in decimal base 10. In WEIDs, the arcs are in base 36.
 *     Also, each WEID has a check digit at the end (called WeLuhn Check Digit).
 *
 * The full specification can be found here: https://weid.info/spec.html
 *
 * This converter supports WEID as of Spec Change #11
 *
 * A few short notes:
 *     - There are several classes of WEIDs which have different OID bases:
 *           "Class A" WEID:  weid:root:2-RR-?
 *                            oid:2.999
 *                            WEID class base OID: (OID Root)
 *           "Class B" WEID:  weid:pen:SX0-7PR-?
 *                            oid:1.3.6.1.4.1.37476.9999
 *                            WEID class base OID: 1.3.6.1.4.1
 *           "Class C" WEID:  weid:EXAMPLE-?
 *                            oid:1.3.6.1.4.1.37553.8.32488192274
 *                            WEID class base OID: 1.3.6.1.4.1.37553.8
 *           "Class D" WEID:  weid:example.com:TEST-? is equal to weid:9-DNS-COM-EXAMPLE-TEST-?
 *                            Since the check digit is based on the OID, the check digit is equal for both notations.
 *                            oid:1.3.6.1.4.1.37553.8.9.17704.32488192274.16438.1372205
 *                            WEID class base OID: 1.3.6.1.4.1.37553.8.9.17704
 *     - The last arc in a WEID is the check digit. A question mark is the wildcard for an unknown check digit.
 *       In this case, the converter will return the correct expected check digit for the input.
 *     - The namespace (weid:, weid:pen:, weid:root:) is case insensitive.
 *     - Padding with '0' characters is valid (e.g. weid:000EXAMPLE-3)
 *       The paddings do not count into the WeLuhn check digit.
 *)

interface

(*
Translates a weid to an oid
"weid:EXAMPLE-3" becomes "1.3.6.1.4.1.37553.8.32488192274"
If it failed (e.g. wrong namespace, wrong checksum, etc.) then false is returned.
If the weid ends with '?', then it will be replaced with the checksum,
e.g. weid:EXAMPLE-? becomes weid:EXAMPLE-3
*)
function WeidToOid(var weid: string): string;

(*
Converts an OID to WEID
"1.3.6.1.4.1.37553.8.32488192274" becomes "weid:EXAMPLE-3"
*)
function OidToWeid(oid: string): string;

implementation

uses
  SysUtils;

function LastCharPos(const S: string; const Chr: char): integer;
var
  i: Integer;
begin
  for i := length(S) downto 1 do
  begin
    if S[i] = Chr then
    begin
      result := i;
      Exit;
    end;
  end;
  result := 0;
  Exit;
end;

function base_convert_bigint(numstring: string; frombase, tobase: integer): string;
var
  i: Integer;
  frombase_str: string;
  tobase_str: string;
  len: Integer;
  number: string;
  divide: Integer;
  newlen: Integer;
  res: string;
begin
  frombase_str := '';
  for i := 0 to frombase-1 do
  begin
    if i < 10 then
      frombase_str := frombase_str + IntToStr(i)
    else
      frombase_str := frombase_str + Chr(Ord('A') + (i-10));
  end;

  tobase_str := '';
  for i := 0 to tobase-1 do
  begin
    if i < 10 then
      tobase_str := tobase_str + IntToStr(i)
    else
      tobase_str := tobase_str + Chr(Ord('A') + (i-10));
  end;

  len := Length(numstring);
  result := '';
  number := numstring; (* this is a fake "Int8" array (implemented with chars) *)
  for i := 0 to len-1 do
  begin
    number[i+1] := Chr(Pos(UpCase(numstring[i+1]), frombase_str)-1);
  end;
  res := '';
  repeat (* Loop until whole number is converted *)
    divide := 0;
    newlen := 0;
    for i := 0 to len-1 do (* Perform division manually (which is why this works with big numbers) *)
    begin
      divide := divide * frombase + Ord(number[i+1]);
      if (divide >= tobase) then
      begin
        number[newlen+1] := Chr(divide div tobase);
        Inc(newlen);
        divide := divide mod tobase;
      end
      else if newlen > 0 then
      begin
        number[newlen+1] := #0;
        Inc(newlen);
      end;
    end;
    len := newlen;
    res := tobase_str[divide+1] + res; (* Divide is basically "numstring mod tobase" (i.e. the new character) *)
  until newlen = 0;
  result := res;
end;

function weLuhnGetCheckDigit(s: string): integer;
var
  p: integer;
  wrkstr: string;
  c: Char;
  i: Integer;
  sum: integer;
  nbdigits: Integer;
  parity: Integer;
  n: Integer;
  digit: Integer;
begin
  (* Padding zeros don't count to the check digit (December 2021) *)
  s := '-' + s + '-';
  while Pos('-0', s) > 0 do
  begin
    s := StringReplace(s, '-0-', #1, [rfReplaceAll]);
    s := StringReplace(s, '-0', '-', [rfReplaceAll]);
  end;
  s := StringReplace(s, #1, '-0-', [rfReplaceAll]);
  s := Copy(s, 2, Length(s)-2);

  (* remove separators of the WEID string *)
  wrkstr := StringReplace(s, '-', '', [rfReplaceAll]);

  (* Replace 'a' with '10', 'b' with '11', etc. *)
  for c := 'A' to 'Z' do
  begin
    wrkstr := StringReplace(wrkstr, c, IntToStr(Ord(c)-Ord('A')+10), [rfReplaceAll]);
  end;

  (* At the end, wrkstr should only contain digits! Verify it! *)
  for i := 1 to Length(wrkstr) do
  begin
    if not (wrkstr[i] in ['0'..'9']) then
    begin
      result := -1;
      exit;
    end;
  end;

  (* Now do the standard Luhn algorithm *)
  nbdigits := Length(wrkstr);
  parity := nbdigits and 1; (* mod 2 *)
  sum := 0;
  for n := nbdigits-1 downto 0 do
  begin
    digit := StrToInt(wrkstr[n+1]);
    if (n and 1) <> parity then digit := digit * 2;
    if digit > 9 then digit := digit - 9;
    sum := sum + digit;
  end;

  if sum mod 10 = 0 then
    result := 0
  else
    result := 10 - (sum mod 10);
end;

function WeidToOid(var weid: string): string;
var
  base: string;
  namespace: string;
  p: integer;
  rest: string;
  actual_checksum: string;
  expected_checksum: integer;
  complete: string;
  oidstr: string;
  arc: string;
  domainpart: string;
  tmp: string;
begin
  p := LastCharPos(weid,':');
  namespace := Copy(weid, 1, p);
  rest := Copy(weid, p+1, Length(weid)-p);

  namespace := LowerCase(namespace); (* namespace is case insensitive *)

  if Copy(namespace, 1, 5) = 'weid:' then
  begin
    tmp := Copy(namespace, 1, Length(namespace)-1);
    p := Pos(':', tmp, 5);
    Delete(tmp, 1, p);
    if pos('.', tmp) > 0 then
    begin
      (* Spec Change 10: Class D / Domain-WEID *)
      if pos(':', tmp) > 0 then
      begin
        result := '';
        exit;
      end;
      domainpart := '';
      while tmp <> '' do
      begin
        p := Pos('.', tmp);
        if p = 0 then
        begin
          domainpart := tmp + '-' + domainpart;
          break;
        end
        else
        begin
          domainpart := Copy(tmp, 1, p-1) + '-' + domainpart;
          Delete(tmp, 1, p);
        end;
      end;
      weid := 'weid:9-DNS-' + UpperCase(Domainpart) + Rest;
      result := WeidToOid(weid);
      exit;
    end;
  end;

  if Copy(namespace, 1, 7) = 'weid:x-' then
  begin
  	(* Spec Change 11: Proprietary Namespaces *)
    result := '[Proprietary WEID Namespace]';
    Exit;
  end
  else if namespace = 'weid:' then
  begin
    (* Class C *)
    base := '1-3-6-1-4-1-SZ5-8';
  end
  else if namespace = 'weid:pen:' then
  begin
    (* Class B *)
    base := '1-3-6-1-4-1';
  end
  else if namespace = 'weid:root:' then
  begin
    (* Class A *)
    base := '';
  end
  else
  begin
    (* Wrong namespace *)
    result := '';
    Exit;
  end;

  weid := rest;

  if base <> '' then
    complete := base + '-' + weid
  else
    complete := weid;
  p := LastCharPos(complete, '-');
  actual_checksum := Copy(complete, p+1, 1);
  complete := Copy(complete, 1, p-1);
  expected_checksum := weLuhnGetCheckDigit(complete);
  if (actual_checksum <> '?') then
  begin
    if actual_checksum <> IntToStr(expected_checksum) then
    begin
      result := ''; (* wrong checksum *)
      Exit;
    end;
  end
  else
  begin
    (* If checksum is '?', it will be replaced by the actual checksum, *)
    (* e.g. weid:EXAMPLE-? becomes weid:EXAMPLE-3                      *)
    weid := StringReplace(weid, '?', IntToStr(expected_checksum), [rfReplaceAll]);
  end;

  oidstr := '';
  while true do
  begin
    p := Pos('-', complete);
    if p = 0 then p := Length(complete)+1;
    arc := Copy(complete, 1, p-1);
    Delete(complete, 1, p);
    oidstr := oidstr + base_convert_bigint(arc, 36, 10) + '.';
    if complete = '' then break;
  end;
  oidstr := Copy(oidstr, 1, Length(oidstr)-1);

  weid := LowerCase(namespace) + UpperCase(weid); (* add namespace again *)

  result := oidstr;
end;

function OidToWeid(oid: string): string;
var
  is_class_a: boolean;
  is_class_b: boolean;
  is_class_c: boolean;
  weidstr: string;
  checksum: string;
  namespace: string;
  p: Integer;
  cd: Integer;
  res: string;
begin
  if Copy(oid,1,1) = '.' then
    Delete(oid,1,1); (* remove leading dot *)

  if oid <> '' then
  begin
    weidstr := '';
    while true do
    begin
      p := Pos('.', oid);
      if p = 1 then
      begin
        Delete(oid, 1, 1);
      end
      else if p > 0 then
      begin
        weidstr := weidstr + base_convert_bigint(Copy(oid, 1, p-1),10,36) + '-';
        Delete(oid, 1, p);
      end
      else
      begin
        weidstr := weidstr + base_convert_bigint(oid,10,36) + '-';
        break;
      end;
    end;
    weidstr := Copy(weidstr, 1, Length(weidstr)-1);
  end
  else
  begin
    weidstr := '';
  end;

  is_class_c := (Pos('1-3-6-1-4-1-SZ5-8-', weidstr) = 1) or
                (weidstr = '1-3-6-1-4-1-SZ5-8');
  is_class_b := ((Pos('1-3-6-1-4-1-', weidstr) = 1) or
                (weidstr = '1-3-6-1-4-1'))
                and not is_class_c;
  is_class_a := not is_class_b and not is_class_c;

  cd := weLuhnGetCheckDigit(weidstr);
  if cd < 0 then
  begin
    result := weidstr;
    exit;
  end;
  checksum := IntToStr(cd);

  if is_class_c then
  begin
    Delete(weidstr, 1, Length('1-3-6-1-4-1-SZ5-8-'));
    namespace := 'weid:';
  end
  else if is_class_b then
  begin
    Delete(weidstr, 1, Length('1-3-6-1-4-1-'));
    namespace := 'weid:pen:';
  end
  else if is_class_a then
  begin
    (* weidstr stays *)
    namespace := 'weid:root:';
  end
  else
  begin
    (* should not happen *)
    result := '';
    Exit;
  end;

  res := namespace;
  if weidstr = '' then
    res := res + checksum
  else
    res := res + weidstr + '-' + checksum;
  result := res;
end;

end.
