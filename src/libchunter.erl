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
	 create_machine/6,
	 start_machine/3,
	 start_machine/4,
	 stop_machine/3,
	 reboot_machine/3,
	 start/0,
	 ping/2
	]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(lager),
    application:start(libchunter).

-spec(ping(Server::inet:ip_address() | inet:hostname(),
	   Port::inet:port_number()) -> pong).
ping(Server, Port) ->
    libchunter_server:call(Server, Port, ping).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), machine()) -> ok
%%
%% @doc Starts a machine.
%%
%% This command is asyncronous.
%% @end
%%--------------------------------------------------------------------

-spec start_machine(Server::inet:ip_address() | inet:hostname(),
		    Port::inet:port_number(),
		    UUID::fifi:uuid()) -> ok.

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

-spec delete_machine(Server::inet:ip_address() | inet:hostname(),
		     Port::inet:port_number(),
		     UUID::fifo:uuid()) -> ok.

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

-spec create_machine(Server::inet:ip_address() | inet:hostname(),
		     Port::inet:port_number(),
		     UUID::fifo:uuid(),
		     PSpec::fifo:package(),
		     DSpec::fifo:dataset(),
		     Config::fifo:package()
		    ) -> ok.

create_machine(Server, Port, UUID, PSpec, DSpec, Config) ->
    chunter_cast(Server, Port, {machines, create, UUID, PSpec, DSpec, Config}).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(),
%%        machine(), binary()) -> ok.
%%
%% @doc Starts a KVM virtual machine from a iso image.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_machine(Server::inet:ip_address() | inet:hostname(),
		    Port::inet:port_number(),
		    UUID::fifo:uuid(),
		    Imge::binary()) -> ok.

start_machine(Server, Port, UUID, Image) ->
    chunter_cast(Server, Port, {machines, start, UUID, Image}).

-spec stop_machine(Server::inet:ip_address() | inet:hostname(),
		   Port::inet:port_number(),
		   UUID::fifo:uuid()) -> ok.

stop_machine(Server, Port, UUID) ->
    chunter_cast(Server, Port, {machines, stop, UUID}).

-spec reboot_machine(Server::inet:ip_address() | inet:hostname(),
		     Port::inet:port_number(),
		     UUID::fifo:uuid()) -> ok.

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

-spec chunter_cast(Server::inet:ip_address() | inet:hostname(),
		   Port::inet:port_number(),
		   Cast::fifo:chunter_message()) -> ok.

chunter_cast(Server, Port, Cast) ->
    libchunter_server:cast(Server, Port, Cast).
