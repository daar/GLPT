program extract_API;

uses
  Classes,
  SysUtils,
  IniFiles;

type
  OS = record
    Name: string;
    win: string;
    linux: string;
    mac: string;
  end;

var
  api: TStringList;
  list: TStringList;
  i: integer = 0;
  s: string;
  p, j: integer;
  api_list: array of OS;
  ini: TIniFile;

  function asIcon(status: string): string;
  begin
    case status of
      'Working': Result :=
          '<img src="https://github.com/daar/GLPT/blob/master/doc/green.svg">';
      'Not implemented': Result :=
          '<img src="https://github.com/daar/GLPT/blob/master/doc/red.svg">';
      'Partially implemented': Result :=
          '<img src="https://github.com/daar/GLPT/blob/master/doc/orange.svg">';
      'Not applicable': Result :=
          '<img src="https://github.com/daar/GLPT/blob/master/doc/gray.svg">';
    end;
  end;

begin
  api := TStringList.Create;
  api.LoadFromFile('../GLPT.pas');

  list := TStringList.Create;

  //skip until 'interface' is found
  while Trim(api[i]) <> 'interface' do
    Inc(i);

  //stop when 'implementation' is found
  while Trim(api[i]) <> 'implementation' do
  begin
    if pos('function', LowerCase(api[i])) = 1 then
    begin
      p := Pos('(', api[i]);
      if p = 0 then
        p := Pos(':', api[i]);

      s := Trim(Copy(api[i], 9, p - 9));
      list.Add(s);
    end;

    if pos('procedure', LowerCase(api[i])) = 1 then
    begin
      p := Pos('(', api[i]);
      if p = 0 then
        p := Pos(';', api[i]);

      s := Trim(Copy(api[i], 10, p - 10));
      list.Add(s);
    end;

    Inc(i);
  end;

  //open the ini file
  ini := TIniFile.Create('GLPT.ini');

  //create the api_list
  list.Sort;
  SetLength(api_list, list.Count);
  for j := 0 to list.Count - 1 do
  begin
    api_list[j].Name := list[j];
    api_list[j].linux := ini.ReadString(list[j], 'Linux', 'Not implemented');
    api_list[j].mac := ini.ReadString(list[j], 'MacOSX', 'Not implemented');
    api_list[j].win := ini.ReadString(list[j], 'Windows', 'Not implemented');
  end;

  //print API list
  writeln('| API                       | Linux (X11)     | Mac OSX (Cocoa) | Windows (GDI)   | ');
  writeln('|---------------------------|-----------------|-----------------|-----------------|');
  for j := 0 to list.Count - 1 do
  begin
    writeln('| ', api_list[j].Name: 25, ' | ', asIcon(api_list[j].linux),
      ' | ', asIcon(api_list[j].mac), ' | ', asIcon(api_list[j].win), ' | ');
  end;

  //write INI
  for j := 0 to list.Count - 1 do
  begin
    ini.WriteString(api_list[j].Name, 'Linux', api_list[j].linux);
    ini.WriteString(api_list[j].Name, 'MacOSX', api_list[j].mac);
    ini.WriteString(api_list[j].Name, 'Windows', api_list[j].win);
  end;

  ini.Free;
  list.Free;
  api.Free;
end.
