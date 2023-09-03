-module(parse).
-export([parse/1]).

is_end_of_comment(Ch) ->
    string: chr("\n", Ch) =/= 0.

is_end_of_string(Ch) ->
    string:chr("\"", Ch) =/= 0.

char_type(Ch) ->
    IsSW = string:chr(" \t\n\r", Ch),
    IsC = string:chr(";", Ch),
    IsS = string:chr("\"", Ch),
    if
        IsSW =/= 0 -> split_word;
        IsC =/= 0 -> start_comment;
        IsS =/= 0 -> start_string;
        true -> normal
    end.

parse([], Output, _)->
    {[], Output};
parse([H|T], Output, comment) ->
    case is_end_of_comment(H) of
        true ->
            parse(T, Output, new_word);
        _ ->
            parse(T, Output, comment)
    end;
parse([H|T], Output, string) ->
    Last = lists:last(Output),
    NewOutput = lists:sublist(Output, length(Output) - 1) ++ [<<Last/binary, <<H>>/binary>>],
    case is_end_of_string(H) of
        true ->
            parse(T, NewOutput, new_word);
        _ ->
            parse(T, NewOutput, string)
    end;
parse([$( | T], Output, _) ->
    {Remain, Sub} = parse(T, [], new_word),
    parse(Remain, Output ++ [Sub], new_word);
parse([$) | T], Output, _) ->
    {T, Output};
parse([H|T], Output, Status) ->
    case {char_type(H), Status} of
        {split_word, _} ->
            parse(T, Output, new_word);
        {start_comment, _} ->
            parse(T, Output, comment);
        {start_string, _} ->
            parse(T, Output ++ [<<H>>], string);
        {normal, new_word} ->
            parse(T, Output ++ [<<H>>], not_complete);
        {normal, not_complete} ->
            Last = lists:last(Output),
            parse(T, lists:sublist(Output, length(Output) - 1) ++ [<<Last/binary, <<H>>/binary>>], not_complete)
    end.

parse(Input) ->
    {_, Output} = parse(Input, [<<"begin">>], new_word),
    Output.
