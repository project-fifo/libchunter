%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2013, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2013 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(libchunter_dtrace_server).

-behaviour(gen_server).

%% API
-export([start_link/3,
         dtrace/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, walk/1, walk/2, consume/1, close/1]).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 1000).

-record(state, {socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Server, Port, Script) ->
    gen_server:start_link(?MODULE, [Server, Port, Script], []).

dtrace(Server, Port, Script) ->
    supervisor:start_child(libchunter_dtrace_sup, [Server, Port, Script]).

consume(Pid) ->
    gen_server:call(Pid, consume).

walk(Pid) ->
    gen_server:call(Pid, {walk, fun (X) -> X end}).

walk(Pid, Fn) ->
    gen_server:call(Pid, {walk, Fn}).

close(Pid) ->
    gen_server:cast(Pid, close).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Server, Port, Script]) ->
    case gen_tcp:connect(Server, Port, [binary, {active, false}, {packet, 4}], 100) of
        {ok, Socket} ->
            R = gen_tcp:send(Socket, term_to_binary({dtrace, Script})),
            io:format("open: ~p~n", [R]),
            {ok, #state{socket = Socket}};
        _ ->
            {stop, connection_failed}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(consume, _From, State = #state{socket = Socket}) ->
    Ref = make_ref(),
    case gen_tcp:send(Socket, term_to_binary({consume, Ref})) of
        ok ->
            case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
                {ok, Bin} ->
                    {reply, binary_to_term(Bin), State};
                E ->
                    {reply, {error, Ref, E}, State}
            end;
        E ->
            {reply, {error, Ref, E}, State}
    end;


handle_call({walk, Fn}, _From, State = #state{socket = Socket}) ->
    Ref = make_ref(),

    Now1 = now(),
    case gen_tcp:send(Socket, term_to_binary({walk, Ref, Fn})) of
        ok ->
            Now2 = now(),
            RefBin = term_to_binary({ok, Ref}),
            case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
                {ok, RefBin0} when RefBin0 =:= RefBin ->
                    Now3 = now(),
                    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
                        {ok, Bin} when  RefBin == Bin->
                            {reply, binary_to_term(Bin), State};
                        {error, timeout} = E ->
                            lager:warning("Timeout in rcv: ~p + ~p", [timer:now_diff(now(), Now3)]),
                            {reply, {error, rcv, Ref, E}, State};
                        E ->
                            {reply, {error, rcv, Ref, E}, State}
                    end;
                {error, timeout} = E ->
                    lager:warning("Timeout in ok- rcv ok: ~p", [timer:now_diff(now(), Now2)]),
                    {reply, {error, rcv, Ref, E}, State};
                E ->
                    {reply, {error, rcv, Ref, E}, State}
            end;


        {error, timeout} = E ->
            lager:warning("Timeout in send: ~p", [timer:now_diff(now(), Now1)]),
            {reply, {error, send, Ref, E}, State};
        E ->
            {reply, {error, send, Ref, E}, State}
    end;




handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(close, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({tcp_closed, _Socket}, State) ->
    {stop, closed, State};

handle_info(Info, State) ->
    io:format("info: ~p.~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
