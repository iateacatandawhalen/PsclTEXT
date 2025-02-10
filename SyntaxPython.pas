unit SyntaxPython;

interface
procedure PrintPythonSyntaxHighlighting(line: string);

implementation
uses crt, SysUtils;

const
  PythonKeywords: array[0..9] of string =
    ('def', 'return', 'if', 'else', 'elif', 'for', 'while', 'import', 'from', 'class');

  BuiltInFunctions: array[0..5] of string = ('print', 'len', 'int', 'str', 'float', 'range');

procedure PrintPythonSyntaxHighlighting(line: string);
var
  i, j: Integer;
  word: string;
  isKeyword, isBuiltIn: Boolean;
begin
  i := 1;
  while i <= Length(line) do
  begin
    word := '';
    
    while (i <= Length(line)) and (line[i] <> ' ') do
    begin
      word := word + line[i];
      Inc(i);
    end;

    isKeyword := False;
    isBuiltIn := False;
    
    for j := 0 to High(PythonKeywords) do
      if word = PythonKeywords[j] then
      begin
        TextColor(LightMagenta);
        Write(word);
        TextColor(White);
        isKeyword := True;
        Break;
      end;

    if not isKeyword then
      for j := 0 to High(BuiltInFunctions) do
        if word = BuiltInFunctions[j] then
        begin
          TextColor(LightGreen);
          Write(word);
          TextColor(White);
          isBuiltIn := True;
          Break;
        end;

    if not isKeyword and not isBuiltIn then
      Write(word);
    
    Write(' ');
    Inc(i);
  end;
  WriteLn;
end;

end.
