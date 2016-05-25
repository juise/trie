-module(trie).

%% API exports
-export([new/0,
         add_leaf/2,
         search_leaf/2,
         search_prefix_leaf/2]).

-type tree()    :: {'root',            [leaf()]}.

-type leaf()    :: {'leaf',            []      } |
                   {nonempty_string(), [leaf()]}.

-export_type([tree/0]).

-define(END, {leaf, []}).

-define(is_integer(X, Y), is_integer(X) andalso is_integer(Y)).

%%====================================================================
%% API functions
%%====================================================================

-spec new() -> tree().
new() ->
    {root, []}.

-spec add_leaf(nonempty_string(), tree()) -> tree().
add_leaf(Str, {root, Leaves}) ->
    {root, add(Str, Leaves)}.

-spec search_leaf(nonempty_string(), tree()) -> 'undefined' | nonempty_string().
search_leaf(Str, {root, Leaves}) ->
    search(Str, Leaves).

-spec search_prefix_leaf(nonempty_string(), tree()) -> 'undefined' | nonempty_string().
search_prefix_leaf(Str, {root, Leaves}) ->
    search_prefix(Str, Leaves).

%%====================================================================
%% Internal functions
%%====================================================================

-spec add(nonempty_string(), [leaf()]) -> [leaf()].
add(Str, Leaves) ->
    add(Str, [], Leaves).

-spec add(nonempty_string(), [leaf()], [leaf()]) -> [leaf()].
add(Str, LeftLeaves, []) ->
    lists:reverse([{Str, [?END]} | LeftLeaves]);

add(Str, LeftLeaves, [{Key, _} | _] = RightLeaves) when Str == Key ->
    lists:flatten([lists:reverse(LeftLeaves), RightLeaves]);

add(Str, LeftLeaves, [{Key, Leaves} = Leaf | TailLeaves] = RightLeaves) ->
    case compare(Str, Key) of
        eq ->
            case factorize(Str, Key) of
                %% In this case, child Leaves has no common parts
                [CommonPrefix, RPrefix] when CommonPrefix == Str ->
                    lists:flatten([lists:reverse(LeftLeaves), {CommonPrefix, [?END, {RPrefix, Leaves}]} | TailLeaves]);
                %% In this case, child Leaves has common parts
                [CommonPrefix, RPrefix] when CommonPrefix == Key ->
                    lists:flatten([lists:reverse(LeftLeaves), {CommonPrefix, add(RPrefix, Leaves)} | TailLeaves]);
                [CommonPrefix, LPrefix, RPrefix] ->
                    LeftLeaf = lists:flatten(CommonPrefix, LPrefix),
                    RightLeaf = lists:flatten(CommonPrefix, RPrefix),
                    case Key of
                        LeftLeaf ->
                            lists:flatten([lists:reverse(LeftLeaves), {CommonPrefix, [{LPrefix, Leaves}, {RPrefix, [?END]}]} | TailLeaves]);
                        RightLeaf ->
                            lists:flatten([lists:reverse(LeftLeaves), {CommonPrefix, [{LPrefix, [?END]}, {RPrefix, Leaves}]} | TailLeaves])
                    end
            end;
        lt ->
            lists:flatten([lists:reverse(LeftLeaves), {Str, [?END]} | RightLeaves]);
        gt ->
            add(Str, [Leaf | LeftLeaves], TailLeaves)
    end.


-spec search(nonempty_string(), [leaf()]) -> 'undefined' | nonempty_string().
search(Str, Leaves) ->
    search(Str, Leaves, _SearchPrefix = false).

-spec search_prefix(nonempty_string(), [leaf()]) -> 'undefined' | nonempty_string().
search_prefix([], _) ->
    undefined;

search_prefix([_ | Tail] = Str, Leaves) ->
    case search(Str, Leaves, _SearchPrefix = true) of
        undefined ->
            search_prefix(Tail, Leaves);
        Prefix ->
            Prefix
    end.

-spec search(nonempty_string(), [leaf()], boolean()) -> 'undefined' | nonempty_string().
search(Str, Leaves, SearchPrefix) ->
    search(Str, Leaves, SearchPrefix, []).

-spec search(nonempty_string(), [leaf()], boolean(), []) -> 'undefined' | nonempty_string().
search(Str, [{Key, [?END | _]} | _], _, Acc) when Str == Key ->
    lists:flatten(lists:reverse([Str | Acc]));

search(Str, [{Key, _} | _], false, _) when Str == Key ->
    undefined;

search(_, [?END | _], true, Acc) ->
    lists:flatten(lists:reverse(Acc));

search(Str, [?END | TailLeaves], false, Acc) ->
    search(Str, TailLeaves, false, Acc);

search(_, Leaves, _, _) when Leaves == [] orelse Leaves == [?END] ->
    undefined;

search(Str, [{Key, ChildLeaves} | TailLeaves], SearchPrefix, Acc) ->
    case compare(Str, Key) of
        eq ->
            case factorize(Str, Key) of
                [CommonPrefix, RPrefix] ->
                    search(RPrefix, ChildLeaves, SearchPrefix, [CommonPrefix | Acc]);
                [_, _, _] ->
                    undefined
            end;
        lt ->
            undefined;
        gt ->
            search(Str, TailLeaves, SearchPrefix, Acc)
    end.


-spec compare(nonempty_string(), 'leaf' | nonempty_string()) -> 'eg' | 'gt' | 'lt'.
compare(_, leaf) ->
    gt;

compare([X | _], [Y | _]) when ?is_integer(X, Y) andalso X == Y ->
    eq;

compare([X | _], [Y | _]) when ?is_integer(X, Y) andalso X > Y ->
    gt;

compare([X | _], [Y | _]) when ?is_integer(X, Y) andalso X < Y ->
    lt.


-spec factorize(nonempty_string(), nonempty_string()) -> list().
factorize(Xs, Ys) ->
    lists:reverse(factorize(Xs, Ys, [])).

factorize([], [], Acc) ->
    Acc;

factorize([], Ys, Acc) ->
    [Ys, lists:reverse(Acc)];

factorize(Xs, [], Acc) ->
    [Xs, lists:reverse(Acc)];

factorize([X | _] = Xs, [Y | _] = Ys, []) when X < Y ->
    [Ys, Xs];

factorize([X | _] = Xs, [Y | _] = Ys, []) when X > Y ->
    [Xs, Ys];

factorize([X | _] = Xs, [Y | _] = Ys, Acc) when X < Y ->
    [Ys, Xs, lists:reverse(Acc)];

factorize([X | _] = Xs, [Y | _] = Ys, Acc) when X > Y ->
    [Xs, Ys, lists:reverse(Acc)];

factorize([X | Xs1], [Y | Ys1], Acc) when X == Y ->
    factorize(Xs1, Ys1, [X | Acc]).

