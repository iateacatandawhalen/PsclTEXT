unit SyntaxMarkdown;

interface
procedure PrintMarkdownSyntaxHighlighting(line: string);

implementation
uses crt, SysUtils;

const
  MarkdownKeywords: array[0..5] of string = ('#', '##', '###', '**', '_', '`');

procedure PrintMarkdownSyntaxHighlighting(line: string);
var
  i, j: Integer;
  word: string;
  isKeyword: Boolean;
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
    for j := 0 to High(MarkdownKeywords) do
    begin
      if word = MarkdownKeywords[j] then
      begin
        TextColor(LightBlue);
        Write(word);
        TextColor(White);
        isKeyword := True;
        Break;
      end;
    end;

    if not isKeyword then
      Write(word);
    
    Write(' ');
    Inc(i);
  end;
  WriteLn;
end;

end.
