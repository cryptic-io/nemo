Periodically
===========

A minimal erlang library which allows for periodically calling a function. Built on top of
`gen_server` so it's integratable with supervisor the same as any other `gen_server`

Usage
------
The library comes with a Makefile, but it's just a single erl file so you can integrate it 
however you like. I prefer sticking it in deps/ and running erl with `-pa deps/*/ebin/` but
it's up to you.

As far as using it in code, there's two methods, `periodically:start` and `periodically:start_link`.
The difference between them is analogous to `gen_server`. They take three arguments, a `gen_server`
(passed straight in as the first argument to `gen_server:start(_link)/4`), a period in milliseconds,
and a function. The function can either be an anonymous function or in the form `{Module,Method,Args}`.

Example
------

    %Starts a gen_server named thing1, runs the anonymous function every 3 seconds
    periodically:start({local,thing1},3000,fun() -> io:fwrite("thing1 says hi!\n") end),

    %Starts a gen_server named thing2 (and links it), runs ?MODULE:thing2_test([]) every
    %2 seconds
    periodically:start_link({local,thing2},2000,{?MODULE,thing2_test,[]}).


