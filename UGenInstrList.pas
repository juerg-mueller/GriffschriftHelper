unit UGenInstrList;

interface
uses
  SysUtils, Classes, IOUtils,
  UInstrument, Ujson, UMyMemoryStream;


implementation

var
  InstrumentList: array of TInstrument;

const
  FlatNotes  : array [0..11] of string = ('C', 'Des', 'D', 'Es', 'E', 'F', 'Ges', 'G', 'As', 'A', 'B', 'H');
  SharpNotes : array [0..11] of string = ('C', 'Cis', 'D', 'Dis', 'E', 'F', 'Fis', 'G', 'Gis', 'A', 'B', 'H');

function MidiOnlyNote(Pitch: byte; Sharp: boolean): string;
begin
  if Sharp then
    result := Format('%s%d', [SharpNotes[Pitch mod 12], Pitch div 12])
  else
    result := Format('%s%d', [FlatNotes[Pitch mod 12], Pitch div 12])
end;

procedure PrintInstrument(var stream: TMyMemoryStream; Instrument: TInstrument; Nr: integer);
var
  j: integer;
  Instr: string;

  procedure PrintPitchArr(const Arr: TPitchArray; const gap: string);
  var
    i: integer;
  begin
    for i := 0 to High(Arr) do
    begin
      stream.WriteString(Format('%2d', [Arr[i]]));
      if i < High(Arr) then
        stream.WriteString(',');
    end;
    stream.WriteString(gap);
    for i := 0 to High(Arr) do
      if Arr[i] >= 12 then
        stream.WriteString(Format('%5s', [MidiOnlyNote(Arr[i], false)]));
    stream.writeln;
  end;

  procedure PrintVocal(const Vocal: TVocalArray);
  begin
    stream.WriteString(  '      Col: (('); PrintPitchArr(Vocal.Col[1], '),  //');
    stream.WriteString(  '            ('); PrintPitchArr(Vocal.Col[2], '),  //');
    stream.WriteString(  '            ('); PrintPitchArr(Vocal.Col[3], '),  //');
    stream.WriteString(  '            ('); PrintPitchArr(Vocal.Col[4], '),  //');
    stream.WriteString(  '            ('); PrintPitchArr(Vocal.Col[5], ')   //');
    stream.WritelnString('           );');
  end;

begin
  Instr := 'Instr_' + IntToStr(Nr);

  stream.WritelnString(Format('  %s : TInstrument = (', [Instr]));
  stream.WritelnString(Format('    Name: ''%s'';', [Instrument.Name]));
  stream.WritelnString('    Columns: ' + IntToStr(Instrument.Columns) + ';');
  stream.WritelnString('    Push: ('); PrintVocal(Instrument.Push);
  stream.WritelnString('           );');
  stream.WritelnString('    Pull: ('); PrintVocal(Instrument.Pull);
  stream.WritelnString('           );');
  stream.Writeln;
  stream.WritelnString('  );');
end;

var
  NewInstrument: TInstrument;

procedure AddInstrument(FileName: string);
var
  Root: Tjson;
  Instrument: TInstrument;
begin
  if TjsonParser.LoadFromJsonFile(FileName, Root) then
  begin
    Instrument := NewInstrument;
    if Instrument.UseJson(Root) then
    begin
      SetLength(InstrumentList, Length(InstrumentList)+1);
      InstrumentList[Length(InstrumentList)-1] := Instrument;
    end;
  end;
end;

procedure GenerateInstrList(Path: string);
var
  Instrument: TInstrument;
  SR      : TSearchRec;
  DirList : TStringList;
  i: integer;
  stream: TMyMemoryStream;
begin
  SetLength(InstrumentList, 0);
  DirList := TStringList.Create;
  stream := TMyMemoryStream.Create;
  try
    if FindFirst(Path + '*.json', faArchive, SR) = 0 then
    begin
      repeat
        DirList.Add(SR.Name); //Fill the list
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;

    for i := 0 to DirList.Count-1 do
      AddInstrument(Path + DirList[i]);


    stream.WritelnString('unit UInstrumentList;');
    stream.Writeln;
    stream.WritelnString('interface');
    stream.Writeln;
    stream.WritelnString('uses UInstrument;');
    stream.Writeln;
     stream.WritelnString('const');
    stream.Writeln;

    for i := 0 to Length(InstrumentList)-1 do
      PrintInstrument(stream, InstrumentList[i], i);

    stream.WritelnString('type');
    stream.WritelnString('  TInstrumentsList = array [0..' + IntToStr(Length(InstrumentList)-1) + '] of PInstrument;');

    stream.Writeln;
    stream.WritelnString('const');
    stream.WritelnString('  InstrumentsList : TInstrumentsList = (');
    for i := 0 to Length(InstrumentList)-1 do
    begin
      if i > 0 then
        stream.WritelnString(',');
      stream.WriteString('    @Instr_' + IntToStr(i));
    end;
    stream.WriteString('  );');
    stream.Writeln;

    stream.Writeln;
    stream.WritelnString('implementation');

    stream.WritelnString('end.');

    stream.SaveToFile('UInstrumentList.pas');
  finally
    DirList.Free;
    stream.Free;
  end;
end;

begin
//  GenerateInstrList('json/');
end.

