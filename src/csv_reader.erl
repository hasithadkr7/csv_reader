-module(csv_reader).

-export([get_data/1]).


get_data(FilePath) ->
  case file:open(FilePath, [read]) of
    {ok, Device} ->
      try get_all_lines(Device, [])
      after file:close(Device)
      end;
    _ ->
      []
  end.

get_all_lines(eof, []) ->
  [];
get_all_lines(eof, [_Line]) ->
  [];
get_all_lines(eof, [_Header|Lines]) ->
  Lines;
get_all_lines(Device, Lines) ->
  case io:get_line(Device, "") of
    eof  ->
      get_all_lines(eof, lists:reverse(Lines));
    Line ->
      case format_line(Line) of
        [] ->
          get_all_lines(Device, Lines);
        List ->
          get_all_lines(Device, [List|Lines])
      end
  end.

format_line(Line) ->
  case string:split(Line, ",", all) of
    [NationalId, Name, PhoneNumber, Amount] ->
      try
        #{<<"amount">> => format_value(Amount),
          <<"name">> => format_value(Name),
          <<"national_id">> => format_value(NationalId),
          <<"phone_number">> => format_value(PhoneNumber)}
      catch
        Exception:Reason ->
           io:format("csv_reader|format_line|Exception:~p~n", [Exception]),
           io:format("csv_reader|format_line|Reason:~p~n", [Reason]),
           io:format("csv_reader|format_line|Line:~p~n", [Line]),
           []
      end;
    _ ->
      []
  end.

format_value(Value) ->
  Value1 = string:strip(string:chomp(Value), both),
  unicode:characters_to_binary(Value1).