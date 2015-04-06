go2
=====

Erlang implementation of http://golang-challenge.com/go-challenge2/

Build
-----

```
$ rebar3 release -n go2
$ rebar3 release -n go2_client
```

Run
-----

### Server

```
$ RELX_REPLACE_OS_VARS=true RELNAME=go2 _build/default/rel/go2/bin/go2 console
```

### Client

```
$ RELX_REPLACE_OS_VARS=true RELNAME=go2_client _build/default/rel/go2_client/bin/go2_client console
> go2_client:start(<<"hello there">>, 8080).
hello there
ok
```
