-module(global_frame).

-export([global_frame/0, to_string/1, make_list/1]).

plus_(X, []) ->
    {num_val, X};
plus_(X, [{num_val, Y}|T]) ->
    plus_(X + Y, T).
plus([{num_val, X}|T]) ->
    plus_(X, T).

minus_(X, []) ->
    {num_val, X};
minus_(X, [{num_val, Y}|T]) ->
    minus_(X - Y, T).
minus([{num_val, X}]) ->
    {num_val, -X};
minus([{num_val, X}|T]) ->
    minus_(X, T).

mul_(X, []) ->
    {num_val, X};
mul_(X, [{num_val, Y}|T]) ->
    mul_(X * Y, T).
mul([{num_val, X}|T]) ->
    mul_(X, T).

divide(X, Y) when is_integer(X) andalso is_integer(Y) ->
    X div Y;
divide(X, Y) ->
    X / Y.

display(X) ->
    io:format("~ts", [to_string(X)]).

pair_to_chars({pair_val, []}) ->
    [];
pair_to_chars({pair_val, [H|T]}) ->
    to_chars(H) ++
    case T of
        {pair_val, []} ->
            [];
        {pair_val, _} ->
            " " ++ pair_to_chars(T);
        _ ->
            " . " ++ to_chars(T)
    end.

to_chars({pair_val, _} = X) ->
    "(" ++ pair_to_chars(X) ++ ")";
to_chars({symbol, V}) ->
    io_lib:format("~ts", [V]);
to_chars({_, V}) ->
    io_lib:format("~p", [V]);
to_chars(X) ->
    io_lib:format("~p", [X]).

to_string(X) ->
    lists:flatten(to_chars(X)).

cons(X, Y) ->
    {pair_val, [X|Y]}.

nil() ->
    {pair_val, []}.

car({pair_val, X}) ->
    hd(X).

cdr({pair_val, X}) ->
    tl(X).

make_list([]) ->
    nil();
make_list([H|T]) ->
    cons(H, make_list(T)).

global_frame() ->
    [
        {'+', {mul_args, fun plus/1}},
        {'-', {mul_args, fun minus/1}},
        {'*', {mul_args, fun mul/1}},
        {'/', {binf, fun({num_val, X}, {num_val, Y}) -> {num_val, divide(X, Y)} end}},
        {'%', {binf, fun({num_val, X}, {num_val, Y}) -> {num_val, X rem Y} end}},
        {'<', {binf, fun({num_val, X}, {num_val, Y}) -> {bool_val, X < Y} end}},
        {'>', {binf, fun({num_val, X}, {num_val, Y}) -> {bool_val, X > Y} end}},
        {'=', {binf, fun(X, Y) -> {bool_val, X =:= Y} end}},
        {'#', {binf, fun(X, Y) -> {bool_val, X =/= Y} end}},
        {'not', {binf, fun({bool_val, X}) -> {bool_val, not X} end}},
        {'number?', {binf, fun({Type, _}) -> {bool_val, Type =:= num_val} end}},
        {'cons', {binf, fun cons/2}},
        {'car', {binf, fun car/1}},
        {'cdr', {binf, fun cdr/1}},
        {'cadr', {binf, fun(X) -> car(cdr(X)) end}},
        {'cddr', {binf, fun(X) -> cdr(cdr(X)) end}},
        {'caddr', {binf, fun(X) -> car(cdr(cdr(X))) end}},
        {'cadddr', {binf, fun(X) -> car(cdr(cdr(cdr(X)))) end}},
        {'nil', nil()},
        {'null?', {binf, fun(X) -> {bool_val, X =:= nil()} end}},
        {'list', {mul_args, fun make_list/1}},
        {'pair?', {binf, fun({Type, _}) -> {bool_val, Type =:= pair_val} end}},
        {'symbol?', {binf, fun({Type, _}) -> {bool_val, Type =:= symbol} end}},
        {'eq?', {binf, fun({symbol, X}, {symbol, X}) -> {bool_val, true};
                            (_, _) -> {bool_val, false} end}},
        {display, {binf, fun display/1}},
        {newline, {binf, fun() -> io:format("~n") end}},
        {'display-line', {binf, fun(X) -> display(X), io:format("~n") end}}
    ].