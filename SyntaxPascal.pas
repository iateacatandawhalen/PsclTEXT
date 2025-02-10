unit SyntaxPascal;

interface
procedure PrintPascalSyntaxHighlighting(line: string);

implementation
uses crt, SysUtils;

const
  PascalKeywords: array[0..14] of string = (
    'program', 'begin', 'end', 'var', 'const', 
    'procedure', 'function', 'uses', 'if', 'then', 
    'else', 'while', 'for', 'repeat', 'until'
  );

  PascalOperators: array[0..5] of string = ('+', '-', ':=', '=', '<>', '*');

  PascalDelimiters: array[0..5] of string = (';', ':', '.', '(', ')', ',');

procedure PrintPascalSyntaxHighlighting(line: string);
var
  i, j: Integer;
  word: string;
  isKeyword, isOperator, isDelimiter, isComment: Boolean;
begin
  i := 1;
  isComment := False;

  while i <= Length(line) do
  begin
    word := '';

    // Handle comments (single-line // and multi-line (* ... *))
    if (i < Length(line) - 1) and ((line[i] = '/') and (line[i + 1] = '/')) then
    begin
      TextColor(DarkGray);
      Write(Copy(line, i, Length(line) - i + 1));
      TextColor(White);
      Break; // Everything after // is a comment
    end;

    if (i < Length(line) - 1) and ((line[i] = '(') and (line[i + 1] = '*')) then
    begin
      TextColor(DarkGray);
      Write('(*');
      i := i + 2;
      isComment := True;
    end;

    if isComment then
    begin
      while (i <= Length(line)) do
      begin
        Write(line[i]);
        if (i > 1) and (line[i - 1] = '*') and (line[i] = ')') then
        begin
          isComment := False;
          Break;
        end;
        Inc(i);
      end;
      TextColor(White);
      Continue;
    end;

    // Extract words or symbols
    while (i <= Length(line)) and not (line[i] in [' ', ';', ':', '.', '(', ')', ',']) do
    begin
      word := word + line[i];
      Inc(i);
    end;

    // Check if it's a Pascal keyword
    isKeyword := False;
    for j := 0 to High(PascalKeywords) do
      if CompareText(word, PascalKeywords[j]) = 0 then
      begin
        TextColor(LightCyan);
        Write(word);
        TextColor(White);
        isKeyword := True;
        Break;
      end;

    // Check if it's an operator
    isOperator := False;
    for j := 0 to High(PascalOperators) do
      if word = PascalOperators[j] then
      begin
        TextColor(Yellow);
        Write(word);
        TextColor(White);
        isOperator := True;
        Break;
      end;

    // Check if it's a delimiter
    isDelimiter := False;
    for j := 0 to High(PascalDelimiters) do
      if word = PascalDelimiters[j] then
      begin
        TextColor(LightRed);
        Write(word);
        TextColor(White);
        isDelimiter := True;
        Break;
      end;

    // If it's not a keyword, operator, or delimiter, print normally
    if not (isKeyword or isOperator or isDelimiter) then
      Write(word);

    // Print spaces and delimiters
    if (i <= Length(line)) and (line[i] in [';', ':', '.', '(', ')', ',']) then
    begin
      TextColor(LightRed);
      Write(line[i]);
      TextColor(White);
      Inc(i);
    end;

    Write(' '); // Maintain spacing
    Inc(i);
  end;
  WriteLn;
end;

end.
