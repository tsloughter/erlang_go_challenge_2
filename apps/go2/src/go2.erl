-module(go2).

-behaviour(ranch_protocol).

-export([start_link/4]).

-export([init/4
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3]).

-record(state, {socket
               ,transport

               ,pub
               ,sec

               ,nonce
               ,shared_key
               }).


start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% ------------------------------------------------------------------

init(Ref, Socket, Transport, _Opts) ->
    #{public := Pub
     ,secret := Sec} = enacl:box_keypair(),

    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, {packet, line}]),
    gen_server:enter_loop(?MODULE, [], #state{socket=Socket
                                             ,transport=Transport
                                             ,pub=Pub
                                             ,sec=Sec}).

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_, Socket, <<"pub:", TheirPub:32/binary, "\n">>}, State=#state{socket=Socket
                                                                           ,transport=Transport
                                                                           ,pub=Pub
                                                                           ,sec=Sec}) ->
    %% Provide the client with the server's public key
    gen_tcp:send(Socket, <<"pub:", Pub/binary, "\n">>),

    %% Precompute shared key for faster unboxing later
    SharedKey = enacl:box_beforenm(TheirPub, Sec),

    Transport:setopts(Socket, [{active, once}, {packet, line}]),
    {noreply, State#state{shared_key=SharedKey}};
handle_info({_, Socket, <<"nonce:", Nonce:24/binary, "\n">>}, State=#state{socket=Socket
                                                                          ,transport=Transport}) ->
    Transport:setopts(Socket, [{active, once}]),
    {noreply, State#state{nonce=Nonce}};
handle_info({_, Socket, <<"msg:", Msg/binary>>}, State=#state{socket=Socket
                                                             ,transport=Transport
                                                             ,nonce=Nonce
                                                             ,shared_key=SharedKey}) ->
    %% Strip trailing newline and unbox
    Msg1 = binary:split(Msg, <<"\n">>, []),
    {ok, Result} = enacl:box_open_afternm(Msg1, Nonce, SharedKey),

    %% Echo message back to client
    Box = enacl:box_afternm(Result, Nonce, SharedKey),
    gen_tcp:send(Socket, <<"msg:", Box/binary, "\n">>),

    Transport:setopts(Socket, [{active, once}, {packet, line}]),
    {noreply, State};
handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, normal, State#state{socket=undefined}}.

terminate(Reason, #state{socket=undefined}) ->
    ok;
terminate(Reason, #state{socket=Socket, transport=Transport}) ->
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
