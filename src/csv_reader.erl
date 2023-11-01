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

%%%{"amount" => "5000", "name" => "John Doy", "national_id" => "123456789", "phone_number" => "+97142244224"}

format_line(Line) ->
  case string:split(Line, ",", all) of
    [NationalId, Name, PhoneNumber, Amount] ->
      #{<<"amount">> => list_to_binary(string:strip(Amount, both, $\n)),
        <<"name">> => list_to_binary(string:strip(Name, both, $\n)),
        <<"national_id">> => list_to_binary(string:strip(NationalId, both, $\n)),
        <<"phone_number">> => list_to_binary(string:strip(PhoneNumber, both, $\n))};
    _ ->
      []
  end.