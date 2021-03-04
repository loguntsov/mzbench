-module(mzb_string).

-compile({no_autoimport, [ length/1 ]}).

-export([
    format/1, format/2,
    join/2,
    merge/1,
    indent/2, indent/3,
    trim_right/2,
    length/1,
    to_binary/1,
    char_substitute/3,
    wildcard_to_regexp/1,
    iso_8601_fmt/1,
    parse_iso_8601/1,
    str_to_bstr/1,
    unescape_ascii/1,
    list_to_number/1
]).



-type str() :: binary().
-export_type([
  str/0
]).

-spec unescape_ascii(Escaped :: string()) -> Unescaped :: string().
unescape_ascii([$\\, X | T]) ->
    [case X of
        $a -> 16#07;
        $b -> 16#08;
        $f -> 16#0C;
        $n -> 16#0A;
        $r -> 16#0D;
        $t -> 16#09;
        $v -> 16#0B;
        R -> R end
    | unescape_ascii(T)];
unescape_ascii([H | T]) -> [H | unescape_ascii(T)];
unescape_ascii([]) -> [].

-spec format(Format :: string(), Args :: [term()]) -> FlatString :: string().
format(Format, Args) ->
    unicode:characters_to_binary(lists:flatten(io_lib:format(Format, Args))).

format(Formats) ->
    iolist_to_binary(lists:map(fun({Format, Args}) -> format(Format, Args) end, Formats)).

join([], _Sep) ->
  [];
join([H|T], Sep) ->
  iolist_to_binary([ H, [[Sep, X ] || X <- T]]).

merge(List) ->
  iolist_to_binary(List).

-spec char_substitute(string(), char(), char()) -> string().
char_substitute(String, OldChar, NewChar) ->
    lists:map(fun(Char) when Char =:= OldChar -> NewChar;
        (Char) -> Char end, String).

indent(<<>>, N, Default) -> indent(Default, N);
indent(Str, N, _) -> indent(Str, N).

indent(Str, N) ->
    Spaces = iolist_to_binary(lists:duplicate(N, <<" ">>)),
    join([[ Spaces, Line ] || Line <- binary:split(Str, <<"\n">>, [ global ])], <<"\n">>).

trim_right(<<>>, _) -> <<>>;
trim_right(Bin, Byte) ->
    case binary:last(Bin) =:= Byte of
        true ->
            trim_right(binary:part(Bin, { 0, byte_size(Bin) - 1 }), Byte);
            _ -> Bin
    end.

length(Bin) ->
    byte_size(Bin).

wildcard_to_regexp(Wildcard) ->
    "^" ++ lists:flatten(lists:map(
        fun (X) when X == $* -> ".*";
            (X) when X == $? -> $.;
            (X) when X == $. -> "\\.";
            (X) -> X end, Wildcard)) ++ "$".

iso_8601_fmt(Seconds) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time({Seconds div 1000000, Seconds rem 1000000, 0}),
    format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ", [Year, Month, Day, Hour, Min, Sec]).

parse_iso_8601(String) ->
    case io_lib:fread("~4d-~2d-~2dT~2d:~2d:~2d", String) of
        {ok, [Year, Month, Day, Hour, Min, Sec], _} ->
            DateTime = {{Year, Month, Day}, {Hour, Min, Sec}},
            calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200;
        _ -> erlang:error(date_parsing_error)
    end.

str_to_bstr([]) -> [];
str_to_bstr(T) when is_map(T) ->
    maps:from_list([{to_binary(K), str_to_bstr(V)} || {K, V} <- maps:to_list(T)]);
str_to_bstr(T) when is_list(T) ->
    try io_lib:printable_unicode_list(unicode:characters_to_list(list_to_binary(T))) of
            true -> unicode:characters_to_binary(list_to_binary(T));
            false -> [str_to_bstr(X) || X <- T]
        catch _:_ -> [str_to_bstr(X) || X <- T]
    end;

str_to_bstr(T) -> T.

-spec list_to_number(string()) -> integer() | float().
list_to_number(String) ->
    try
        list_to_float(String)
    catch
        _:_ -> list_to_integer(String)
    end.

to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom);
to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(List) when is_list(List) -> iolist_to_binary(List).
%%
%%str_to_bstr([]) -> [];
%%
%%str_to_bstr([Num|_] = String) when is_integer(Num) ->
%%    iolist_to_binary(String);
%%
%%str_to_bstr([[Num|_] = _Arg1 |_] = List) when is_integer(Num) ->
%%    [str_to_bstr(X) || X <- List];
%%
%%str_to_bstr(T) when is_map(T) ->
%%    maps:from_list([{to_binary(K), str_to_bstr(V)} || {K, V} <- maps:to_list(T)]);
%%str_to_bstr(T) when is_list(T) ->
%%    [str_to_bstr(X) || X <- T];
%%%%    try io_lib:printable_unicode_list(unicode:characters_to_list(list_to_binary(T))) of
%%%%        true -> unicode:characters_to_binary(list_to_binary(T));
%%%%        false -> [str_to_bstr(X) || X <- T]
%%%%    catch
%%%%        _:R:ST ->
%%%%            io:format("BSTR Error ~p ~p ~p ~n", [ T, R, ST ]),
%%%%            [str_to_bstr(X) || X <- T]
%%%%    end;
%%
%%str_to_bstr(T) when is_atom(T) -> atom_to_list(T);
%%str_to_bstr(Int) when is_integer(Int) -> Int;
%%str_to_bstr(Bin) when is_binary(Bin) -> Bin;
%%str_to_bstr(Float) when is_float(Float) -> Float.
