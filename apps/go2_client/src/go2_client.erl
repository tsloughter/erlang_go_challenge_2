%%%-------------------------------------------------------------------
%% @doc go2 client public API
%% @end
%%%-------------------------------------------------------------------

-module(go2_client).


%% Application callbacks
-export([start/2]).

%%====================================================================
%% API
%%====================================================================

start(Msg, Port) ->
    #{public := Pub
     ,secret := Sec} = enacl:box_keypair(),

    {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, [binary, {active,false}, {packet, line}]),
    Nonce = enacl:randombytes(24),
    gen_tcp:send(Socket, <<"pub:", Pub/binary, "\n">>),

    {ok, <<"pub:", TheirPub:32/binary, "\n">>} = gen_tcp:recv(Socket, 0),
    SharedKey = enacl:box_beforenm(TheirPub, Sec),

    Box = enacl:box_afternm(Msg, Nonce, SharedKey),
    gen_tcp:send(Socket, <<"nonce:", Nonce/binary, "\n">>),
    gen_tcp:send(Socket, <<"msg:", Box/binary, "\n">>),

    {ok, <<"msg:", BoxRecv/binary>>} = gen_tcp:recv(Socket, 0),
    BoxRecv1 = binary:split(BoxRecv, <<"\n">>, []),
    {ok, Result} = enacl:box_open_afternm(BoxRecv1, Nonce, SharedKey),

    io:format("~s~n", [Result]).

%%====================================================================
%% Internal functions
%%====================================================================
