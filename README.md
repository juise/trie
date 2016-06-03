trie
====

[![Build Status](https://travis-ci.org/juise/trie.svg?branch=master)](https://travis-ci.org/juise/trie)

The Trie (Prefix Tree / Radix Tree) — https://en.wikipedia.org/wiki/Trie

Build
-----

    $ make

Tests
-----

    $ make test

Usage
-----

    $ make run
    1> application:load(trie).
    ok
    2> Tree = lists:foldl(fun(X, Acc) -> trie:add_leaf(X, Acc) end, trie:new(), ["help", "hello", "helloween"]).
    {root, [
        {"hel", [
            {"lo", [
                {leaf, []},
                {"ween", [
                    {leaf, []}
                ]}
            ]},
            {"p", [
                {leaf,[]}
            ]}
        ]}
    ]}
    3> trie:search_leaf("helloween", Tree).
    "helloween"
    4> trie:search_prefix_leaf("helloween", Tree).
    "hello"

