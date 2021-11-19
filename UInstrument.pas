unit UInstrument;

interface

uses
  SysUtils, Classes,
  UMyMemoryStream, Ujson;

type

  TPitchArray = array [0..15] of byte;
  TColArray = array [1..5] of TPitchArray;

  TVocalArray =
    record
      Col: TColArray;
      procedure Transpose(delta: integer);
      procedure CopyJson(Node: Tjson);
    end;

  TInstrument =
    record
      Name: string;
      Columns: integer;
      Push: TVocalArray;
      Pull: TVocalArray;

      procedure Transpose(delta: integer);
      function UseJson(Root: Tjson): boolean;
      function GetPitch(Row, Index: Integer; Push_: boolean): byte;

    end;
  PInstrument = ^TInstrument;



function GetPitchLine(pitch: byte): integer;

implementation

const
  CDur : array [0..6] of byte = (52,53,55,57,59,60,62); // e, f, g, a, h, c, d

function GetPitchLine(pitch: byte): integer;
begin
  result := 0;
  if pitch < 50 then
    exit;

  dec(pitch, 50);  // Index = 0 für pitch 50 (d)
  while pitch >= 12 do
  begin
    inc(result, 7);
    dec(pitch, 12);
  end;

  case pitch of
    0: begin end;           // d
    1, 2: inc(result, 1);   // es, e
    3:    inc(result, 2);   // f
    4, 5: inc(result, 3);   // ges, g
    6, 7: inc(result, 4);   // as, a
    8, 9: inc(result, 5);   // b, h
    10:   inc(result, 6);   // c
    11:   inc(result, 7);   // des
    else begin end;
  end;
end;

procedure TVocalArray.Transpose(delta: integer);
var
  i, j: integer;
  d: integer;
begin
  for i := Low(Col) to High(Col) do
    for j := 0 to Length(Col[i])-1 do
      if Col[i][j] > 0 then
      begin
        d := Col[i][j] + delta;
        if (d > 0) and (d < 128) then
          Col[i][j] := d;
      end;
end;


procedure TVocalArray.CopyJson(Node: Tjson);
var
  i, max: integer;

  function GetPitch(Note: string): integer;
  var
    i: integer;
  begin
    Note := LowerCase(Note);
    case AnsiChar(Note[1]) of
      'c': result := 0;
      'd': result := 2;
      'e': result := 4;
      'f': result := 5;
      'g': result := 7;
      'a': result := 9;
      'b': result := 11;
      else result := 0;
    end;
    if Copy(Note, 2, 2) = 'es' then
      dec(result)
    else
    if Copy(Note, 2, 2) = 'is' then
      inc(result);
    inc(result, 48);
    i := Length(Note);
    while (i > 0) and (AnsiChar(Note[i]) in ['''', ',']) do
    begin
      if Note[i] = '''' then
        inc(result, 12)
      else
        dec(result, 12);
      dec(i);
    end;
  end;

  procedure FillPitchArray(var Arr: TPitchArray; Values: Tjson);
  var
    i, max: integer;
  begin
    if Values = nil then
      max := -1
    else
      max := Length(Values.List)-1;
    if max > 15 then
      max := 15;
    for i := 0 to max do
      Arr[i] := GetPitch(Values.List[i].Value);
    for i := max + 1 to 15 do
      Arr[i] := 0;
  end;

begin
  max := Length(Node.List);
  if max > 5 then
    max := 5;
  for i := 1 to max do
    FillPitchArray(Col[i], Node.List[i-1]);
  for i := max+1 to 5 do
    FillPitchArray(Col[i], nil);
end;

procedure TInstrument.Transpose(delta: integer);
begin
  Push.Transpose(delta);
  Pull.Transpose(delta);
end;

function TInstrument.UseJson(Root: Tjson): boolean;
var
  Node: Tjson;
begin
  result := false;
  Node := Root.FindInList('steirDescription');
  if Node <> nil then
    Name := Node.Value;
  Node := Root.FindInList('steirMapping');
  if (Node <> nil) and (Length(Node.List) = 2) then
  begin
    Columns := Length(Node.List[0].List);
    Push.CopyJson(Node.List[1]);
    Pull.CopyJson(Node.List[0]);
    result := true;
  end;
end;

function TInstrument.GetPitch(Row, Index: Integer; Push_: boolean): byte;
begin
  result := 0;
  if (Row < Low(Push.Col)) or (Row > High(Push.Col)) or
     (Index > High(Push.Col[1])) or (Index < 0) then
    exit;

  if (Row >= 3) and (Index > 0) then
    dec(Index);

  case Row of
    1..5: if Push_ then
            result := Push.Col[Row, Index]
          else
            result := Pull.Col[Row, Index];
  end;
end;

end.

