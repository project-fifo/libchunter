%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc This module provides an interface to the chunter server.
%%%
%%% @end
%%% Created : 11 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(libchunter).

%% API
-export([
	 delete_machine/3,
	 create_machine/4,
	 start_machine/3,
	 start_machine/4,
	 stop_machine/3,
	 reboot_machine/3,
	 start/0
	]).

%% @type uuid() = binary().
%-type uuid() ::
%	binary().


%% @type user() = uuid().
%-type user() :: uuid().

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(lager),
    application:start(libchunter).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), machine()) -> ok
%%
%% @doc Starts a machine.
%%
%% This command is asyncronous.
%% @end
%%--------------------------------------------------------------------


start_machine(Server, Port, UUID) ->
    chunter_cast(Server, Port, {machines, start, UUID}).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), machine()) -> ok
%%
%% @doc Deletes a machine.
%%
%% This command is asyncronous.
%% @end
%%--------------------------------------------------------------------

delete_machine(Server, Port, UUID) ->
    chunter_cast(Server, Port, {machines, delete, UUID}).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), 
%%        binary(), binary(), binary(), 
%%        [{binary(),binary()}], [{binary(),binary()}]) -> machine()
%%
%% @doc Creates a new machine.
%%
%% @end
%%--------------------------------------------------------------------

create_machine(Server, Port, UUID, Spec) ->
    chunter_cast(Server, Port, {machines, create, UUID, Spec}).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), 
%%        machine(), binary()) -> ok.
%%
%% @doc Starts a KVM virtual machine from a iso image.
%%
%% @end
%%--------------------------------------------------------------------

start_machine(Server, Port, UUID, Image) ->
    chunter_cast(Server, Port, {machines, start, UUID, Image}).

stop_machine(Server, Port, UUID) ->
    chunter_cast(Server, Port, {machines, stop, UUID}).

reboot_machine(Server, Port, UUID) ->
    chunter_cast(Server, Port, {machines, reboot, UUID}).
    

%%%===================================================================
%%% Internal functions
%%%===================================================================

%chunter_call(Server, Port,{Auth, _}, Call) ->    
%    chunter_call(Server, Port, Call);

%chunter_call(Server, Port, Call) ->
%    try 
%	gen_server:call(Server, Port,{call, Auth, Call})
%    catch
%	T:E ->
%	    lager:error([{fifi_component, libchunter}, {user, Auth}],
%			"libchunter:call - Error ~p:~p, Call: ~p.",
%			[T, E, Call]),
%	    {error, cant_call}
%    end.

chunter_cast(Server, Port, Call) ->
    libchunter_server:cast(Server, Port,Call).
