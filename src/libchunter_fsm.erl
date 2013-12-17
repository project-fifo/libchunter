%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  6 Oct 2012 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(libchunter_fsm).

-behaviour(gen_fsm).

%% API
-export([
         call/4,
         cast/3,
         start_link/4
        ]).

%% gen_fsm callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([
         connecting/2,
         sending/2,
         rcving/2,
         closing/2
        ]).

-define(SERVER, ?MODULE).

-record(state, {server, port, command, from, socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link(Server, Handler, Command, From) -> {ok, Pid}
%%                                                   | ignore
%%                                                   | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link(Server, Port, Command, From) ->
    gen_fsm:start_link(?MODULE, [Server, Port, Command, From], []).

call(Server, Port, Command, From) ->
    supervisor:start_child(libchunter_fsm_sup, [Server, Port, Command, From]).

cast(Server, Port, Command) ->
    supervisor:start_child(libchunter_fsm_sup, [Server, Port, Command, undefined]).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([Server, Port, Command, From]) ->
    {ok, connecting, #state{
           server = Server,
           command = Command,
           port = Port,
           from = From}, 0}.

connecting(_Event, #state{server=Server,
                          port=Port,
                          from=From} = State) ->
    case gen_tcp:connect(Server, Port, [binary, {active, false}, {packet, 4}], 100) of
        {ok, Socket} ->
            {next_state, sending, State#state{socket = Socket}, 0};
        _ ->
            gen_server:reply(From, {error, connection_failed}),
            {next_state, closing, State, 0}
    end.

sending(_Event, #state{socket=Socket,
                       command = Command,
                       from=From} = State) ->
    case gen_tcp:send(Socket, term_to_binary(Command)) of
        ok ->
            {next_state, rcving, State, 0};
        _ ->
            gen_server:reply(From, {error, connection_failed}),
            {next_state, closing, State, 0}
    end.


rcving(_Event, #state{socket=Socket, from=undefined} = State) ->
    gen_tcp:recv(Socket, 0),
    {next_state, closing, State, 0};

rcving(_Event, #state{socket=Socket, from=From} = State) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Res} ->
            case binary_to_term(Res) of
                {reply, R} ->
                    gen_server:reply(From, R);
                R ->
                    gen_server:reply(From, R)
            end,
            {next_state, closing, State, 0};
        _ ->
            gen_server:reply(From, {error, connection_failed}),
            {next_state, closing, State, 0}
    end.

closing(_Event, #state{socket=undefined} = State) ->
    {stop, normal, State};
closing(_Event, #state{socket=Socket} = State) ->
    gen_tcp:close(Socket),
    {stop, normal, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
