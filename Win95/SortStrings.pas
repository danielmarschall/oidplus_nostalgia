unit SortStrings;

interface

uses Classes;

procedure SortSL(sl: TStrings);

implementation

uses
  Math;

function CompareStr(Str1,Str2: string):Integer; // Source: https://www.experts-exchange.com/questions/23086281/Natural-Order-String-Sort-Compare-in-Delphi.html
var Num1,Num2:Double;
    pStr1,pStr2:PChar;
  Function IsNumber(ch:Char):Boolean;
  begin
     Result:=ch in ['0'..'9'];
  end;
  Function GetNumber(var pch:PChar):Double;
    var FoundPeriod:Boolean;
        Count:Integer;
  begin
     FoundPeriod:=False;
     Result:=0;
     While (pch^<>#0) and (IsNumber(pch^) or ((not FoundPeriod) and (pch^='.'))) do
     begin
        if pch^='.' then
        begin
          FoundPeriod:=True;
          Count:=0;
        end
        else
        begin
           if FoundPeriod then
           begin
             Inc(Count);
             Result:=Result+(ord(pch^)-ord('0'))*Power(10,-Count);
           end
           else Result:=Result*10+ord(pch^)-ord('0');
        end;
        Inc(pch);
     end;
  end;
begin
    pStr1:=@Str1[1]; pStr2:=@Str2[1];
    Result:=0;
    While not ((pStr1^=#0) or (pStr2^=#0)) do
    begin
       if IsNumber(pStr1^) and IsNumber(pStr2^) then
       begin
          Num1:=GetNumber(pStr1); Num2:=GetNumber(pStr2);
          if Num1<Num2 then Result:=-1
          else if Num1>Num2 then Result:=1;
          Dec(pStr1);Dec(pStr2);
       end
       else if pStr1^<>pStr2^ then
       begin
          if pStr1^<pStr2^ then Result:=-1 else Result:=1;
       end;
       if Result<>0 then Break;
       Inc(pStr1); Inc(pStr2);
    end;
    Num1:=length(Str1); Num2:= length(Str2);
    if (Result=0) and (Num1<>Num2) then
    begin
       if Num1<Num2 then Result:=-1 else Result:=1;
    end;
end;

function BubbleSort( list: TStrings ): TStrings; // Source: https://delphi.fandom.com/wiki/Bubble_sort
var
  i, j: Integer;
  temp: string;
begin
  for i := 0 to list.Count - 1 do begin
    for j := 0 to ( list.Count - 1 ) - i do begin
      // Condition to handle i=0 & j = 9. j+1 tries to access x[10] which
      // is not there in zero based array
      if ( j + 1 = list.Count ) then
        continue;
      if CompareStr(list.Strings[j], list.Strings[j+1]) > 0 then
      begin
        temp              := list.Strings[j];
        list.Strings[j]   := list.Strings[j+1];
        list.Strings[j+1] := temp;
      end; // endif
    end; // endwhile
  end; // endwhile
  Result := list;
end;

procedure SortSL(sl: TStrings);
begin
  if sl.Count > 1 then
  begin
    BubbleSort(sl);
  end;
end;

end.
