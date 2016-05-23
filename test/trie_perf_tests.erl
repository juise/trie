-module(trie_perf_tests).

-include_lib("eunit/include/eunit.hrl").


perf_test_() ->
    [{timeout, 30, [{"Performance test", fun search_perf_tests/0}]}].

search_perf_tests() ->
    Words = words(),
    Target = target(),

    S = measure(fun() -> (sequential(Target, Words) =/= undefined) orelse throw(nomatch) end, 1000),
    Pattern = pattern(Words),
    R = measure(fun() -> (regexp(Target, Pattern) =/= undefined) orelse throw(nomatch) end, 1000),
    Tree = tree(Words),
    T = measure(fun() -> (trie(Target, Tree) =/= undefined) orelse throw(nomatch) end, 1000),

    ?debugFmt("~n------------------------------------------------------------------------------------------------~n" ++
              " Sequential prefix search\t~p~n Regexp prefix search\t\t~p~n Trie prefix search\t\t~p" ++
              "~n------------------------------------------------------------------------------------------------~n", [S, R, T]),
    ok.


-spec sequential(nonempty_string(), list()) -> 'undefined' | nonempty_string().
sequential(_, []) ->
    undefined;

sequential(Target, [X | Xs]) ->
    case string:str(Target, X) of
        0 ->
            sequential(Target, Xs);
        Result ->
            Result
    end.

-spec regexp(nonempty_string(), list()) -> 'undefined' | nonempty_string().
regexp(Target, Pattern) ->
    case re:run(Target, Pattern, [global, {capture, first, list}]) of
        nomatch ->
            undefined;
        {match, [Result | _]} ->
            Result
    end.

-spec pattern([nonempty_string()]) -> re:mp().
pattern(Words) ->
    {ok, MP} = re:compile(string:join(Words, "|"), [unicode, caseless]),
    MP.

-spec trie(nonempty_string(), list()) -> 'undefined' | nonempty_string().
trie(Target, Tree) ->
    Result = trie:search_prefix_leaf(Target, Tree),
    Result.

-spec tree([nonempty_string()]) -> trie:tree().
tree(Words) ->
    lists:foldl(fun(X, Acc) -> trie:add_leaf(X, Acc) end, trie:new(), Words).


measure(F, Iter) ->
    N = lists:seq(1, Iter),
    R = [T || {T, _} <- [timer:tc(F) || _ <- N]],
    #{itr => Iter, mean_t_mcs => lists:sum(R) div Iter, total_t_msc => lists:sum(R)}.


words() ->
    ["peered","initials","showed","paused","minerva",
     "flicked","minutely","conditional","explain",
     "envelope","competitions","covered","tissue","mechanical",
     "talking","swallowed","parchment","materials","bitterly",
     "afraid","scientific","bought","guilty","existing","deputy",
     "taught","briefly","spoons","lectures","fiction","snatch",
     "pushing","downstairs","wanted","pretty",
     "sponsored","repeated","family","escaping","considers",
     "whomsoever","higher","entered","standard","method",
     "recently","breath","thought","silence","schools",
     "reflecting","learned","current","advance","received",
     "prettier","impossible","falsification","arbiter",
     "whispered","opened","firmly","helpful",
     "quietly","parents","highly","creeping","affords",
     "turned","imagine","wizardry","unnerving","reasonable",
     "directed","working","unstamped","tutors","shocked",
     "children","starving","insane","tapping","doctor",
     "philosophers","lengths","fathers","called","calling",
     "interested","insanity","addressed","observation","living",
     "letterbox","testing","potter","neighbours","mentally",
     "arguments","physics","understand","sending",
     "dangerous","awfulness","wedding","oxford","expected",
     "careful","biochemistry","writing","occupied","impressed",
     "thinks","stupid","literature","grimacing","garden",
     "pulled","mentioned","madness","raised","layers","filled",
     "fighting","concern","realise","peculiar","listed","excuse",
     "couldn","stumped","students","verres","trembled",
     "hogwarts","ground","thinking","advice","wizard",
     "adopted","respect","endless","embarrassing","attention",
     "theory","certainty","penknife","climbed","seemingly",
     "parent","witchcraft","finding","absolutely","shouting",
     "gathering","absurd","puzzled","effort","christmas",
     "begged","hairnet","gently","discarded","announced",
     "requires","frightened","provided","familiar","university",
     "shelves","putative","propped","emerald","person","candle",
     "bizarre","twisted","handle","tested","darling","convinced",
     "forehead","folded","pulling","pencil","visited","gained",
     "equipment","conceived","husband","bemused","arrived",
     "joking","remember","middle","making","extremely",
     "expectant","annoyance","shrugged","sincerely",
     "looked","kitchen","interest","feynman","demonstrate",
     "change","address","worries","yellowish","dismissive",
     "agreed","sounded","reading","uncivilised","stacked",
     "finally","shouted","report","uncertain","prediction",
     "babysitter","stairs","smiled","descend","tables",
     "reassuring","michael","wrinkled","resolve","excuses",
     "cleared","believed","matter","overflowing","calmly",
     "trained","acceptance","strange","resting","graphite",
     "utterly","presume","potion","paperback","neighbouring",
     "graduated","disgraceful","beautiful","stretched","petunia",
     "important","windows","future","weight","letter",
     "eminent","improbable","encouraged","science","disappeared",
     "closed","centaur","loving","expecting","dudley",
     "unscientific","primary","caught","treated","settle",
     "steeled","experimental","attending","gestured",
     "hypothesis","forgotten","obtain","mcgonagall","holding",
     "associating","sitting","history","slightest","occasional",
     "universe","representative","instant","secret","scream",
     "argument","preparing","hardback","calligraphy","arguing",
     "sister","chapter","bending","strangled","rubbed",
     "ascending","magical","answering","stretching","sharply",
     "grizzled","ceiling","brought","vernon","question","things",
     "speaking","testable","happen","teaching","hopeless",
     "bedroom","hoping","favourite","suspicious",
     "mother","listen","bookcase","millimetre","headmistress",
      "ridiculous","dursley","bookshelves","rolled",
     "consideration","telling","stopped","genetic", "sceptical"].

target() ->
    "Now, just to be clear, Harry said, if the professor does levitate you, Dad, when you know you haven't been attached to any wires, that's going to be sufficient evidence. You're not going to turn around and say that it's a magician's trick. That wouldn't be fair play. If you feel that way, you should say so now, and we can figure out a different experiment instead. Harry's father, Professor his eyes. And you, Mum, your says that the professor should be able to do this, and if that doesn't happen, you'll admit you're mistaken. Nothing about how magic doesn't work when peoples are sceptical of it, or anything like that.".
