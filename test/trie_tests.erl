-module(trie_tests).

-include_lib("eunit/include/eunit.hrl").


compare_test_() ->
    [{timeout, 1, [{"Check for eq", fun compare_eq_tests/0}]},
     {timeout, 1, [{"Check for lt", fun compare_lt_tests/0}]},
     {timeout, 1, [{"Check for gt", fun compare_gt_tests/0}]}].

compare_eq_tests() ->
    ?assertEqual(eq, trie:compare("a",      "a")),
    ?assertEqual(eq, trie:compare("a",      "abc")),
    ?assertEqual(eq, trie:compare("a",      "abcdef")),
    ok.

compare_lt_tests() ->
    ?assertEqual(lt, trie:compare("a",      "b")),
    ?assertEqual(lt, trie:compare("a",      "bcd")),
    ?assertEqual(lt, trie:compare("abcdef", "bcd")),
    ok.

compare_gt_tests() ->
    ?assertEqual(gt, trie:compare("a",      leaf)),
    ?assertEqual(gt, trie:compare("abc",    leaf)),

    ?assertEqual(gt, trie:compare("b",      "a")),
    ?assertEqual(gt, trie:compare("bcd",    "abc")),
    ?assertEqual(gt, trie:compare("bcd",    "abcdef")),
    ok.

factorize_test() ->
    ?assertEqual("a",     trie:factorize("a", "a")),
    ?assertEqual("hello", trie:factorize("hello", "hello")),

    ?assertEqual(["hello", "ween"], trie:factorize("hello", "helloween")),
    ?assertEqual(["hello", "ween"], trie:factorize("helloween", "hello")),

    ?assertEqual(["hel", "lo", "p"], trie:factorize("hello", "help")),
    ?assertEqual(["hel", "lo", "p"], trie:factorize("help", "hello")),

    ?assertEqual(["a", "b"], trie:factorize("a", "b")),
    ?assertEqual(["a", "b"], trie:factorize("b", "a")),

    ?assertEqual(["green", "yellow"], trie:factorize("green", "yellow")),
    ?assertEqual(["green", "yellow"], trie:factorize("yellow", "green")),
    ok.

add_test_() ->
    [{timeout, 1,  [{"Add words simple", fun add_simple_tests/0}]},
     {timeout, 30, [{"Add words stress", fun add_stress_tests/0}]}].

add_simple_tests() ->
    ?assertEqual([{"hello", [{leaf, []}]}], trie:add("hello", [])),
    ?assertEqual([{"hello", [{leaf, []}]}], trie:add("hello", [{"hello", [{leaf, []}]}])),

    ?assertEqual([{"hello", [{leaf, []}]},
                  {"world", [{leaf, []}]}], trie:add("world", [{"hello", [{leaf, []}]}])),

    ?assertEqual([{"hello", [{leaf, []}, {"ween", [{leaf, []}]}]},
                  {"world", [{leaf, []}]}], trie:add("helloween", [{"hello", [{leaf, []}]},
                                                                   {"world", [{leaf, []}]}])),

    ?assertEqual([{"hello", [{leaf, []}, {"ween", [{leaf, []}]}]},
                  {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}], trie:add("work", [{"hello", [{leaf, []}, {"ween", [{leaf, []}]}]},
                                                                                           {"world", [{leaf, []}]}])),

    ?assertEqual([{"hello", [{leaf, []}, {"ween", [{leaf, []}]}]},
                  {"lovely", [{leaf, []}]},
                  {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}], trie:add("lovely", [{"hello", [{leaf, []}, {"ween", [{leaf, []}]}]},
                                                                                             {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}])),

    ?assertEqual([{"hell", [{leaf, []}, {"o", [{leaf, []}, {"ween", [{leaf, []}]}]}]},
                  {"lovely", [{leaf, []}]},
                  {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}], trie:add("hell", [{"hello", [{leaf, []}, {"ween", [{leaf, []}]}]},
                                                                                           {"lovely", [{leaf, []}]},
                                                                                           {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}])),

    ?assertEqual([{"hell", [{leaf, []}, {"o", [{leaf,[]}, {"w", [{leaf, []}, {"een", [{leaf, []}]}]}]}]},
                  {"lovely", [{leaf, []}]},
                  {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}], trie:add("hellow", [{"hell", [{leaf, []}, {"o", [{leaf, []}, {"ween", [{leaf, []}]}]}]},
                                                                                             {"lovely", [{leaf, []}]},
                                                                                             {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}])),
    ok.

add_stress_tests() ->
    [?assertEqual(tree(), lists:foldl(fun(X, Acc) -> trie:add(X, Acc) end, [], shuffle(words()))) || _ <- lists:seq(1, 1000)],
    ok.

search_test_() ->
    [{timeout,  1, [{"Search word in Trie",         fun search_tests/0}]},
     {timeout,  1, [{"Search prefix word in Trie",  fun search_prefix_tests/0}]}].

search_tests() ->
    Leaves = [{"hell", [{leaf, []}, {"o", [{leaf,[]}, {"w", [{leaf, []}, {"een", [{leaf, []}]}]}]}]},
              {"lovely", [{leaf, []}]},
              {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}],

    ?assertEqual("hell",        trie:search("hell",      Leaves)),
    ?assertEqual("hello",       trie:search("hello",     Leaves)),
    ?assertEqual("hellow",      trie:search("hellow",    Leaves)),
    ?assertEqual("helloween",   trie:search("helloween", Leaves)),


    ?assertEqual("work",        trie:search("work",      Leaves)),
    ?assertEqual("world",       trie:search("world",     Leaves)),

    ?assertEqual("lovely",      trie:search("lovely",    Leaves)),

    ?assertEqual(undefined,     trie:search("hellween",  Leaves)),
    ?assertEqual(undefined,     trie:search("love",      Leaves)),
    ?assertEqual(undefined,     trie:search("wor",       Leaves)),
    ok.

search_prefix_tests() ->
    Leaves = [{"hell", [{leaf, []}, {"o", [{leaf,[]}, {"w", [{leaf, []}, {"een", [{leaf, []}]}]}]}]},
              {"lovely", [{leaf, []}]},
              {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}],

    ?assertEqual("hell",        trie:search_prefix("hell",      Leaves)),
    ?assertEqual("hell",        trie:search_prefix("hello",     Leaves)),
    ?assertEqual("hell",        trie:search_prefix("helloween", Leaves)),

    ?assertEqual("work",        trie:search_prefix("work",      Leaves)),
    ?assertEqual("world",       trie:search_prefix("world",     Leaves)),

    ?assertEqual("lovely",      trie:search_prefix("lovely",    Leaves)),
    ok.

new_test() ->
    ?assertEqual({root, []}, trie:new()),
    ok.

add_leaf_test() ->
    ?assertEqual({root, [{"hello", [{leaf, []}]}]}, trie:add_leaf("hello", trie:new())),

    ?assertEqual({root, [{"hello", [{leaf, []}, {"ween", [{leaf, []}]}]}]}, trie:add_leaf("helloween",
                                                                                          trie:add_leaf("hello", trie:new()))),

    ?assertEqual({root, [{"hell", [{leaf, []}, {"o", [{leaf, []}, {"ween", [{leaf, []}]}]}]}]}, trie:add_leaf("hell",
                                                                                                              trie:add_leaf("helloween",
                                                                                                                           trie:add_leaf("hello", trie:new())))),
    ok.

search_leaf_test() ->
    Tree = {root, [{"hell", [{leaf, []}, {"o", [{leaf,[]}, {"w", [{leaf, []}, {"een", [{leaf, []}]}]}]}]},
                   {"lovely", [{leaf, []}]},
                   {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}]},

    ?assertEqual("hell",        trie:search_leaf("hell",      Tree)),
    ?assertEqual("hello",       trie:search_leaf("hello",     Tree)),
    ?assertEqual("hellow",      trie:search_leaf("hellow",    Tree)),
    ?assertEqual("helloween",   trie:search_leaf("helloween", Tree)),
    ok.

search_prefix_leaf_test() ->
    Tree = {root, [{"hell", [{leaf, []}, {"o", [{leaf,[]}, {"w", [{leaf, []}, {"een", [{leaf, []}]}]}]}]},
                   {"lovely", [{leaf, []}]},
                   {"wor", [{"k", [{leaf, []}]}, {"ld", [{leaf, []}]}]}]},

    ?assertEqual("hell",        trie:search_prefix_leaf("hell",         Tree)),
    ?assertEqual("hell",        trie:search_prefix_leaf("hello",        Tree)),
    ?assertEqual("hell",        trie:search_prefix_leaf("helloween",    Tree)),
    ok.


shuffle(Xs) ->
    lists:foldl(fun(_, Acc) -> lists:sort(fun(X, Y) -> rand:uniform() > 0.5 end, Acc) end, Xs, lists:seq(1, 10)).

words() ->
    ["ac","ac","ac","ac","ac","ac","accumsan","adipiscing","aenean","aliquam","aliquam","aliquam","aliquam","aliquam","aliquam","aliquam","aliquam","aliquam","aliquam","aliquam","aliquet","aliquet","amet","amet","amet","amet","amet","amet","amet","amet","ante","ante","arcu","arcu","arcu","arcu","arcu","at","at","at","at","augue","augue","augue","augue","augue","augue","augue","augue",
     "bibendum","bibendum","blandit",
     "commodo","commodo","condimentum","condimentum","congue","congue","consectetur","consequat","consequat","consequat","curabitur",
     "dapibus","dapibus","dapibus","dapibus","dapibus","dapibus","diam","diam","diam","dictum","dictum","dictum","dictum","dictum","dignissim","dignissim","dignissim","dignissim","dolor","dolor","donec","donec","donec","donec","donec","donec","dui","dui","dui","dui","duis","duis",
     "efficitur","efficitur","efficitur","efficitur","efficitur","egestas","egestas","egestas","egestas","egestas","eget","eget","eget","eget","eget","eget","eget","eget","eleifend","eleifend","eleifend","eleifend","eleifend","elementum","elementum","elementum","elementum","elit","elit","elit","elit","elit","elit","enim","enim","enim","enim","enim","enim","erat","erat","erat","erat","eros","est","est","et","et","et","etiam","eu","eu","eu","eu","eu","eu","eu","eu","eu","eu","euismod","euismod","euismod","euismod","ex","ex",
     "facilisis","facilisis","faucibus","faucibus","faucibus","faucibus","fermentum","fermentum","feugiat","feugiat","feugiat","feugiat","finibus","finibus","fringilla","fringilla","fringilla","fusce","fusce",
     "gravida","gravida",
     "hendrerit","hendrerit","hendrerit",
     "iaculis","iaculis","iaculis","iaculis","iaculis","iaculis","id","id","id","id","imperdiet","in","in","in","in","in","in","in","in","in","in","in","in","in","integer","integer","ipsum","ipsum","ipsum","ipsum",
     "justo","justo","justo",
     "lacinia","lectus","lectus","lectus","leo","ligula","ligula","ligula","ligula","lobortis","lobortis","lobortis","lobortis","lorem","lorem","lorem","luctus","luctus","luctus","luctus",
     "maecenas","magna","magna","magna","magna","magna","magna","malesuada","massa","massa","massa","massa","massa","mattis","mattis","mattis","mauris","mauris","mauris","mauris","mauris","mauris","maximus","maximus","maximus","metus","metus","metus","mi","mi","mi","molestie","molestie","molestie","morbi","morbi","morbi",
     "nam","nec","nec","nec","nec","nec","neque","neque","neque","neque","neque","neque","neque","nibh","nibh","nibh","nibh","nibh","nisi","nisi","nisi","nisi","nisi","nisi","nisl","non","non","non","non","non","nulla","nulla","nulla","nulla","nulla","nulla","nulla","nullam","nullam","nullam","nullam","nunc","nunc","nunc","nunc","nunc",
     "orci","orci","orci","orci","orci","orci","ornare","ornare","ornare","ornare",
     "pellentesque","pellentesque","pellentesque","pellentesque","pellentesque","pharetra","phasellus","phasellus","phasellus","phasellus","phasellus","placerat","porta","porta","porta","porttitor","posuere","potenti","praesent","praesent","praesent","praesent","pretium","proin","proin","proin","pulvinar","pulvinar","purus","purus",
     "quam","quam","quis","quis","quis","quis","quis","quis","quisque","quisque","quisque",
     "rhoncus","rhoncus","rhoncus","rhoncus","risus","risus","risus","risus","rutrum","rutrum","rutrum",
     "sagittis","sagittis","sagittis","sagittis","sagittis","sagittis","sagittis","sagittis","sapien","sapien","sapien","scelerisque","scelerisque","scelerisque","sed","sed","sed","sed","sed","sed","sed","sed","sed","sed","sem","sem","sem","semper","semper","sit","sit","sit","sit","sit","sit","sit","sit","sodales","sodales","sodales","sodales","sollicitudin","sollicitudin","sollicitudin","sollicitudin","suscipit","suscipit","suscipit","suspendisse","suspendisse","suspendisse","suspendisse",
     "tellus","tellus","tellus","tellus","tempor","tempor","tempus","tempus","tempus","tempus","tempus","tincidunt","tincidunt","tincidunt","tincidunt","tortor","tortor","tortor","tristique","turpis","turpis",
     "ullamcorper","ultrices","ultrices","ultrices","urna","urna","urna","urna","urna","urna","urna","ut","ut","ut","ut","ut","ut","ut","ut","ut","ut",
     "varius","varius","vehicula","vehicula","vehicula","vehicula","vel","vel","vel","velit","velit","velit","venenatis","venenatis","venenatis","vestibulum","vestibulum","vestibulum","vitae","vitae","vitae","vitae","viverra","viverra","volutpat","volutpat","vulputate","vulputate","vulputate","vulputate"].

tree() ->
    [{"a",
      [{"c",[{leaf,[]},{"cumsan",[{leaf,[]}]}]},
       {"dipiscing",[{leaf,[]}]},
       {"enean",[{leaf,[]}]},
       {"liqu",[{"am",[{leaf,[]}]},{"et",[{leaf,[]}]}]},
       {"met",[{leaf,[]}]},
       {"nte",[{leaf,[]}]},
       {"rcu",[{leaf,[]}]},
       {"t",[{leaf,[]}]},
       {"ugue",[{leaf,[]}]}]},
     {"b",[{"ibendum",[{leaf,[]}]},{"landit",[{leaf,[]}]}]},
     {"c",
      [{"o",
        [{"mmodo",[{leaf,[]}]},
         {"n",
          [{"dimentum",[{leaf,[]}]},
           {"gue",[{leaf,[]}]},
           {"se",[{"ctetur",[{leaf,[]}]},{"quat",[{leaf,[]}]}]}]}]},
       {"urabitur",[{leaf,[]}]}]},
     {"d",
      [{"apibus",[{leaf,[]}]},
       {"i",
        [{"am",[{leaf,[]}]},
         {"ctum",[{leaf,[]}]},
         {"gnissim",[{leaf,[]}]}]},
       {"o",[{"lor",[{leaf,[]}]},{"nec",[{leaf,[]}]}]},
       {"ui",[{leaf,[]},{"s",[{leaf,[]}]}]}]},
     {"e",
      [{"fficitur",[{leaf,[]}]},
       {"ge",[{"stas",[{leaf,[]}]},{"t",[{leaf,[]}]}]},
       {"l",
        [{"e",[{"ifend",[{leaf,[]}]},{"mentum",[{leaf,[]}]}]},
         {"it",[{leaf,[]}]}]},
       {"nim",[{leaf,[]}]},
       {"r",[{"at",[{leaf,[]}]},{"os",[{leaf,[]}]}]},
       {"st",[{leaf,[]}]},
       {"t",[{leaf,[]},{"iam",[{leaf,[]}]}]},
       {"u",[{leaf,[]},{"ismod",[{leaf,[]}]}]},
       {"x",[{leaf,[]}]}]},
     {"f",
      [{"a",[{"cilisis",[{leaf,[]}]},{"ucibus",[{leaf,[]}]}]},
       {"e",[{"rmentum",[{leaf,[]}]},{"ugiat",[{leaf,[]}]}]},
       {"inibus",[{leaf,[]}]},
       {"ringilla",[{leaf,[]}]},
       {"usce",[{leaf,[]}]}]},
     {"gravida",[{leaf,[]}]},
     {"hendrerit",[{leaf,[]}]},
     {"i",
      [{"aculis",[{leaf,[]}]},
       {"d",[{leaf,[]}]},
       {"mperdiet",[{leaf,[]}]},
       {"n",[{leaf,[]},{"teger",[{leaf,[]}]}]},
       {"psum",[{leaf,[]}]}]},
     {"justo",[{leaf,[]}]},
     {"l",
      [{"acinia",[{leaf,[]}]},
       {"e",[{"ctus",[{leaf,[]}]},{"o",[{leaf,[]}]}]},
       {"igula",[{leaf,[]}]},
       {"o",[{"bortis",[{leaf,[]}]},{"rem",[{leaf,[]}]}]},
       {"uctus",[{leaf,[]}]}]},
     {"m",
      [{"a",
        [{"ecenas",[{leaf,[]}]},
         {"gna",[{leaf,[]}]},
         {"lesuada",[{leaf,[]}]},
         {"ssa",[{leaf,[]}]},
         {"ttis",[{leaf,[]}]},
         {"uris",[{leaf,[]}]},
         {"ximus",[{leaf,[]}]}]},
       {"etus",[{leaf,[]}]},
       {"i",[{leaf,[]}]},
       {"o",[{"lestie",[{leaf,[]}]},{"rbi",[{leaf,[]}]}]}]},
     {"n",
      [{"am",[{leaf,[]}]},
       {"e",[{"c",[{leaf,[]}]},{"que",[{leaf,[]}]}]},
       {"i",
        [{"bh",[{leaf,[]}]},
         {"s",[{"i",[{leaf,[]}]},{"l",[{leaf,[]}]}]}]},
       {"on",[{leaf,[]}]},
       {"u",
        [{"lla",[{leaf,[]},{"m",[{leaf,[]}]}]},
         {"nc",[{leaf,[]}]}]}]},
     {"or",[{"ci",[{leaf,[]}]},{"nare",[{leaf,[]}]}]},
     {"p",
      [{"ellentesque",[{leaf,[]}]},
       {"ha",[{"retra",[{leaf,[]}]},{"sellus",[{leaf,[]}]}]},
       {"lacerat",[{leaf,[]}]},
       {"o",
        [{"rt",[{"a",[{leaf,[]}]},{"titor",[{leaf,[]}]}]},
         {"suere",[{leaf,[]}]},
         {"tenti",[{leaf,[]}]}]},
       {"r",
        [{"aesent",[{leaf,[]}]},
         {"etium",[{leaf,[]}]},
         {"oin",[{leaf,[]}]}]},
       {"u",[{"lvinar",[{leaf,[]}]},{"rus",[{leaf,[]}]}]}]},
     {"qu",
      [{"am",[{leaf,[]}]},{"is",[{leaf,[]},{"que",[{leaf,[]}]}]}]},
     {"r",
      [{"honcus",[{leaf,[]}]},
       {"isus",[{leaf,[]}]},
       {"utrum",[{leaf,[]}]}]},
     {"s",
      [{"a",[{"gittis",[{leaf,[]}]},{"pien",[{leaf,[]}]}]},
       {"celerisque",[{leaf,[]}]},
       {"e",
        [{"d",[{leaf,[]}]},{"m",[{leaf,[]},{"per",[{leaf,[]}]}]}]},
       {"it",[{leaf,[]}]},
       {"o",[{"dales",[{leaf,[]}]},{"llicitudin",[{leaf,[]}]}]},
       {"us",[{"cipit",[{leaf,[]}]},{"pendisse",[{leaf,[]}]}]}]},
     {"t",
      [{"e",
        [{"llus",[{leaf,[]}]},
         {"mp",[{"or",[{leaf,[]}]},{"us",[{leaf,[]}]}]}]},
       {"incidunt",[{leaf,[]}]},
       {"ortor",[{leaf,[]}]},
       {"ristique",[{leaf,[]}]},
       {"urpis",[{leaf,[]}]}]},
     {"u",
      [{"l",[{"lamcorper",[{leaf,[]}]},{"trices",[{leaf,[]}]}]},
       {"rna",[{leaf,[]}]},
       {"t",[{leaf,[]}]}]},
     {"v",
      [{"arius",[{leaf,[]}]},
       {"e",
        [{"hicula",[{leaf,[]}]},
         {"l",[{leaf,[]},{"it",[{leaf,[]}]}]},
         {"nenatis",[{leaf,[]}]},
         {"stibulum",[{leaf,[]}]}]},
       {"i",[{"tae",[{leaf,[]}]},{"verra",[{leaf,[]}]}]},
       {"olutpat",[{leaf,[]}]},
       {"ulputate",[{leaf,[]}]}]}].

