unit UGriffHelper;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ShellApi, Vcl.StdCtrls;

var
  BellowsWidth: double = 0.5;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    cbxRemoveColor: TCheckBox;
    edtDicke: TEdit;
    Label2: TLabel;
    cbxInstr: TComboBox;
    cbxKlingend: TCheckBox;
    lbInstr: TLabel;
    cbxTranspose: TComboBox;
    lblTranspose: TLabel;
    cxbPressure: TCheckBox;
    Memo1: TMemo;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure FormCreate(Sender: TObject);
    procedure edtDickeExit(Sender: TObject);
    procedure edtDickeKeyPress(Sender: TObject; var Key: Char);
    procedure cbxKlingendClick(Sender: TObject);
    procedure cxbPressureClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    function InsertBellows(FileName: string): boolean;
  end;

  THeader = record
    quarterNoteTicks: integer;
    measureNom: integer;
    measureDenom: integer;
    function GetChordTicks(fraction, dots: integer): integer;
    function TicksPerMeasure: integer;
  end;


var
  Form1: TForm1;

const MuseScoreTPC : array [0..11] of byte =
      (14, 14, 16, 16, 18, 13, 13, 15, 15, 17, 19, 19);


implementation

{$R *.dfm}

uses
  UXmlNode, UXmlParser, UInstrument, UInstrumentList;


const
  NoteNames: array [0..7] of string =
    ('whole', 'half', 'quarter', 'eighth', '16th', '32nd', '64th', '128th');

procedure TForm1.cbxKlingendClick(Sender: TObject);
begin
  cbxInstr.Enabled := cbxKlingend.Checked;
  lbInstr.Enabled := cbxKlingend.Checked;
  lblTranspose.Enabled := cbxKlingend.Checked;
  cbxTranspose.Enabled := cbxKlingend.Checked;
end;

procedure TForm1.cxbPressureClick(Sender: TObject);
begin
  edtDicke.Enabled := cxbPressure.Checked;
  Label2.Enabled := cxbPressure.Checked;
end;

procedure TForm1.edtDickeExit(Sender: TObject);
begin
  BellowsWidth := StrToFloatDef(edtDicke.Text, 3.5);
  if BellowsWidth < 0.2 then
    BellowsWidth := 0.2
  else
  if BellowsWidth > 4 then
    BellowsWidth := 4;

  edtDicke.Text := FloatToStr(BellowsWidth);
end;

procedure TForm1.edtDickeKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    self.SelectNext(Sender as TWinControl, true, true);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
begin
  DragAcceptFiles(Self.Handle, true);
  edtDicke.Text := FloatToStr(BellowsWidth);

  for i := 0 to Length(InstrumentsList)-1 do
    cbxInstr.Items.Add(InstrumentsList[i].Name);
  cbxInstr.ItemIndex := 1;

  cbxKlingendClick(nil);
  cxbPressureClick(nil);
end;

procedure TForm1.WMDropFiles(var Msg: TWMDropFiles);
var
  DropH: HDROP;               // drop handle
  DroppedFileCount: Integer;  // number of files dropped
  FileNameLength: Integer;    // length of a dropped file name
  FileName: string;           // a dropped file name
  i: integer;
  ext: string;
begin
  inherited;

  DropH := Msg.Drop;
  try
    DroppedFileCount := DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    if (DroppedFileCount > 0) then
    begin
      edtDickeExit(nil);
      for i := 0 to DroppedFileCount-1 do
      begin
        FileNameLength := DragQueryFile(DropH, i, nil, 0);
        SetLength(FileName, FileNameLength);
        DragQueryFile(DropH, i, PChar(FileName), FileNameLength + 1);
        ext := ExtractFileExt(Filename);
        if (LowerCase(ext) = '.mscz') or
           (LowerCase(ext) = '.mscx') then
        begin
          InsertBellows(FileName);
        end;
      end;
    end;
  finally
    DragFinish(DropH);
  end;
  Msg.Result := 0;
end;

function GetFraction(const sLen: string): integer;
var
  idx: integer;
begin
  result := 128;
  for idx := High(NoteNames) downto 0 do
    if sLen = NoteNames[idx] then
      break
    else
      result := result shr 1;
end;

function GetChild(Name: string; var Child: KXmlNode; Parent: KXmlNode): boolean;
var
  k: integer;
begin
  k := 0;
  result := false;
  while not result and (Parent <> nil) and (k < Parent.Count) do
  begin
    Child := Parent.ChildNodes[k];
    inc(k);
    result := Child.Name = Name;
  end;
  if not result then
    Child := nil;
end;

function THeader.TicksPerMeasure: integer;
begin
  result := 4*quarterNoteTicks*measureNom div measureDenom;
end;

function THeader.GetChordTicks(fraction, dots: integer): integer;
var
  h: integer;
begin
  result := 0;
  if fraction > 0 then
  begin
    result := 4*quarterNoteTicks div fraction;
    h := result;
    while dots > 0 do
    begin
      h := h div 2;
      inc(result, h);
      dec(dots);
    end;
  end;
end;

procedure AddSubtype(TextLine: KXmlNode; Sub: string; Y: string);
var
  Segment, Child: KXmlNode;
begin
  Segment := TextLine.AppendChildNode('Segment');
  Segment.AppendChildNode('subtype', Sub);
  Child := Segment.AppendChildNode('offset');
  Child.AppendAttr('x', '0');
  Child.AppendAttr('y', Y);
  Child := Segment.AppendChildNode('off2');
  Child.AppendAttr('x', '0');
  Child.AppendAttr('y', '0');
end;

procedure CheckNote(const Instrument: TInstrument; Note: KXmlNode);
var
  Cross, Pull: boolean;
  Symbol, color, Events, Event, pitchNode, tpc, Child: KXmlNode;
  pitchIdx: integer;
  Line: integer;
  Pitch, klingPitch: integer;
  row: integer;
begin
  Cross := GetChild('Symbol', Symbol, Note);
  if not Cross then
  begin
    if GetChild('head', Symbol, Note) and
       ((Symbol.Value = 'cross') or (Symbol.Value = 'x')) then
      Cross := true;
  end;

  Pull := GetChild('color', color, Note);  // !!!!! not
  GetChild('Events', Events, Note);
  GetChild('pitch', pitchNode, Note);
  pitchIdx := Note.GetChildIndex(pitchNode);
  GetChild('tpc', tpc, Note);
  if Events = nil then
  begin
    Events := KXmlNode.Create;
    Events.Name := 'Events';
    Note.InsertChildNode(pitchIdx, Events);
    inc(pitchIdx);
  end;
  if not GetChild('Event', Event, Events) then
    Event := Events.AppendChildNode('Event');
  if not GetChild('pitch', Child, Event) then
    Event.AppendChildNode('pitch');

  if tpc = nil then
  begin
    tpc := KXmlNode.Create;
    tpc.Name := 'tpc';
    PitchNode.InsertChildNode(pitchIdx+1, tpc);
  end;
  Pitch := StrToInt(pitchNode.Value);
  Line := GetPitchLine(Pitch);
  if Line > 0 then
    dec(Line);
  if odd(Line) then
    row := 2
  else
    row := 1;
  if Cross then
    inc(row, 2);

  klingPitch := Instrument.GetPitch(row, Line div 2, not Pull);
  if klingPitch = 0 then
    klingPitch := Pitch;
  tpc.Value := IntToStr(MuseScoreTPC[Pitch mod 12]);
  Child := Event.HasChild('pitch');
  if klingPitch <> Pitch then
  begin
    if Child = nil then
      Child := Event.AppendChildNode('pitch');
    Child.Value := IntToStr(klingPitch - Pitch);
  end else
  if Child <> nil then
    Event.RemoveChild(Child);
end;

function TForm1.InsertBellows(FileName: string): boolean;
var
  Root: KXmlNode;
  Score, Staff, Measure, Voice, Chord, Note, Child, Child1, Child2: KXmlNode;
  NextSpanner: KXmlNode;
  Spanner, TextLine: KXmlNode;
  offset: integer;
  SpannerOffset: integer;
  Header: THeader;

  procedure SetHeader;
  var
    Child, Child1: KXmlNode;
  begin
    with Header do
    begin
      quarterNoteTicks := 120;
      measureNom := 4;
      measureDenom := 4;
    end;
    if GetChild('Division', Child, Score) then
      Header.quarterNoteTicks := StrToIntDef(Child.Value, 120);
    if GetChild('Measure', Child, Staff) and
       GetChild('voice', Child, Child) and
       GetChild('TimeSig', Child, Child) then
    begin
      if GetChild('sigN', Child1, Child) then
        Header.measureNom := StrToIntDef(Child1.Value, 4);
      if GetChild('sigD', Child1, Child) then
        Header.measureDenom := StrToIntDef(Child1.Value, 4);
    end;
    with Header do
    begin
      if quarterNoteTicks <= 0 then
        quarterNoteTicks := 120;
      if measureNom < 0 then
        measureNom := 4;
      if measureDenom <= 0 then
        measureDenom := 4;
    end;
  end;

  procedure SetPrevSpanner;
  var
    off, mes, fra, quot: integer;
    Child1, Child2: KXmlNode;
  begin
    if NextSpanner <> nil then
    begin
      off := offset - SpannerOffset;
      mes := off div Header.TicksPerMeasure;
      fra := off mod Header.TicksPerMeasure;
      fra := 8*fra div Header.quarterNoteTicks;  // 32nd
      Child1 := NextSpanner.AppendChildNode('next');
      Child1 := Child1.AppendChildNode('location');
      if mes > 0 then
      begin
        Child1.AppendChildNode('measures', IntToStr(mes));
      end;
      if fra > 0 then
      begin
        quot := 32;
        while not odd(fra) do
        begin
          quot := quot div 2;
          fra := fra div 2;
        end;
        Child1.AppendChildNode('fractions', IntToStr(fra) + '/' + IntToStr(quot));
      end;

      Child1 := Spanner.AppendChildNode('prev');
      Child1 := Child1.AppendChildNode('location');
      if mes > 0 then
      begin
        Child1.AppendChildNode('measures', IntToStr(-mes));
      end;
      if fra > 0 then
      begin
        Child1.AppendChildNode('fractions', IntToStr(-fra) + '/' + IntToStr(quot));
      end;
      NextSpanner := nil;
    end;
  end;

var
  IsPull: boolean;
  duration, dots: integer;
  hasColor: boolean;
  i, j, v, iMeasure: integer;
  removeColor: boolean;
  Instrument: TInstrument;
  useInstrument: boolean;
  hasColors, hasTextLines: boolean;
  InsertBellows: boolean;
begin
  result := false;
  useInstrument := false;
  removeColor := cbxRemoveColor.Checked;
  InsertBellows := cxbPressure.Checked;
  if not KXmlParser.ParseFile(FileName, Root) then
    exit;

  if cbxKlingend.Checked and
     (cbxInstr.ItemIndex >= 0) then
  begin
    useInstrument := true;
    Instrument := InstrumentsList[cbxInstr.ItemIndex]^;
    if cbxTranspose.ItemIndex <> 11 then
      Instrument.Transpose(cbxTranspose.ItemIndex - 11);
  end;

  Score := Root.ChildNodes[Root.Count-1];
  if (Score.Name <> 'Score') or
     not GetChild('Staff', Staff, Score) then
  begin
    Application.MessageBox('Error in MuseScore file!', 'Error');
    exit;
  end;

  SetHeader;

  // find textline and color
  hasColors := false;
  hasTextLines := false;
  for i := 0 to Staff.Count-1 do
  begin
    Measure := Staff.ChildNodes[i];
    if Measure.Name = 'Measure' then
    begin
      Measure.DeleteAttribute('id');
      if GetChild('voice', Voice, Measure) then
      begin
        v := 0;
        while v < Voice.Count-1 do
        begin
          Child := Voice.ChildNodes[v];
          if (Child.Name = 'Spanner') and
             (Child.Attributes['type'] = 'TextLine')  then
            hasTextLines := true;
          if (Child.Name = 'Chord') and
             GetChild('Note', Child1, Child) and
             GetChild('color', Child2, Child1) then
            hasColors := true;
          inc(v);
        end;
      end;
    end;
  end;

  // reinsert colors
  if hasTextLines and not hasColors then
  begin
    IsPull := true;
    for i := 0 to Staff.Count-1 do
    begin
      Measure := Staff.ChildNodes[i];
      if Measure.Name = 'Measure' then
      begin
        Measure.DeleteAttribute('id');
        if GetChild('voice', Voice, Measure) then
        begin
          v := 0;
          while v < Voice.Count do
          begin
            Child := Voice.ChildNodes[v];
            if (Child.Name = 'Spanner') and
               (Child.Attributes['type'] = 'TextLine')  then
            begin
              IsPull := GetChild('prev', Child1, Child);
            end;
            if IsPull and
               (Child.Name = 'Chord') then
            begin
              for j := 0 to Child.Count-1 do
              begin
                Child1 := Child.ChildNodes[j];
                if Child1.Name = 'Note' then
                begin
                  Child2 := KXmlNode.Create;
                  Child2.Name := 'color';
                  Child2.AppendAttr('r', '0');
                  Child2.AppendAttr('g', '0');
                  Child2.AppendAttr('b', '255');
                  Child2.AppendAttr('a', '255');
                  Child1.InsertChildNode(0, Child2);
                end;
              end;
            end;
            inc(v);
          end;
        end;
      end;
    end;
    hasColors := true;
  end;

  if hasColors then
  begin
    // remove all "spanner  with type textline"
    if hasTextLines then
      for i := 0 to Staff.Count-1 do
      begin
        Measure := Staff.ChildNodes[i];
        if Measure.Name = 'Measure' then
        begin
          Measure.DeleteAttribute('id');
          if GetChild('voice', Voice, Measure) then
          begin
            v := 0;
            while v < Voice.Count do
            begin
              Child := Voice.ChildNodes[v];
              if (Child.Name = 'Spanner') and
                 (Child.Attributes['type'] = 'TextLine')  then
                Voice.PurgeChild(v)
              else
                inc(v);
            end;
          end;
        end;
      end;

    IsPull := true;
    iMeasure := 0;
    offset := 0;
    v := 0;
    for i := 0 to Staff.Count-1 do
    begin
      Measure := Staff.ChildNodes[i];
      if Measure.Name = 'Measure' then
      begin
        inc(iMeasure);
        Measure.AppendAttr('id', IntToStr(iMeasure));
        if GetChild('voice', Voice, Measure) then
        begin
          v := 0;
          while v < Voice.Count do
          begin
            Chord := Voice.ChildNodes[v];
            if GetChild('durationType', Child, Chord) then
            begin
              duration := GetFraction(Child.Value);
              dots := 0;
              if GetChild('dots', Child, Chord) then
                dots := StrToIntDef(Child.Value, 0);
            end;
            if Chord.Name = 'Chord' then
            begin
              for j := 0 to Chord.Count-1 do
              begin
                Note := Chord.ChildNodes[j];
                if Note.Name = 'Note' then
                begin
                  hasColor := GetChild('color', Child1, Note);
                  if (IsPull <> hasColor) and
                     InsertBellows then
                  begin
                    IsPull := hasColor;
                    Spanner := KXmlNode.Create;
                    Spanner.Name := 'Spanner';
                    Spanner.AppendAttr('type', 'TextLine');
                    if not IsPull then
                    begin
                      SpannerOffset := offset;
                      NextSpanner := Spanner;
                      Voice.InsertChildNode(v, Spanner);
                      TextLine := Spanner.AppendChildNode('TextLine');
                      TextLine.AppendChildNode('placement', 'below');
                      TextLine.AppendChildNode('lineWidth', Format('%g', [BellowsWidth]));
                      AddSubtype(TextLine, '0', '3.5');
                      AddSubtype(TextLine, '3', '3.5');
                    end else begin
                      Voice.InsertChildNode(v, Spanner);
                      SetPrevSpanner;
                    end;
                    inc(v);
                  end; // IsPull <> hasColor
                  if useInstrument then
                    CheckNote(Instrument, Note);
                end;
              end; // for j := 0 to Chord.Count-1 do
              inc(offset, Header.GetChordTicks(duration, dots));
            end;
            inc(v);
          end;
        end
      end;
    end;

    if (NextSpanner <> nil) and
       InsertBellows then
    begin
      Spanner := KXmlNode.Create;
      Spanner.Name := 'Spanner';
      Spanner.AppendAttr('type', 'TextLine');

      if GetChild('voice', Voice, Staff.ChildNodes[Staff.Count-1]) then
      begin
        Voice.InsertChildNode(v, Spanner);

        SetPrevSpanner;
      end;
    end;

    // remove blue colors
    if removeColor then
    begin
      for i := 0 to Staff.Count-1 do
      begin
        Measure := Staff.ChildNodes[i];
        if Measure.Name = 'Measure' then
        begin
          Measure.DeleteAttribute('id');
          if GetChild('voice', Voice, Measure) then
          begin
            for v := 0 to Voice.Count-1 do
            begin
              Child := Voice.ChildNodes[v];
              if (Child.Name = 'Chord') then
              begin
                for j := 0 to Child.Count-1 do
                begin
                  Child1 := Child.ChildNodes[j];
                  if Child1.Name = 'Note' then
                    while GetChild('color', Child2, Child1) do
                      Child1.RemoveChild(Child2);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end; // hasColor

  SetLength(Filename, Length(FileName)-Length(ExtractFileExt(FileName)));
  result := Root.SaveToMsczFile(FileName + '_balg.mscx');
end;

end.


