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
         stop_machine/4,
         reboot_machine/3,
         reboot_machine/4,
         update_machine/5,
         console_open/3,
         console_open/4,
         console_send/2,
         snapshot/4,
         delete_snapshot/4,
         rollback_snapshot/4,
         store_snapshot/5,
         start/0,
         ping/2
        ]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(lager),
    application:start(libchunter).

-spec ping(Server::inet:ip_address() | inet:hostname(),
           Port::inet:port_number()) -> pong |
                                        {'error', 'connection_failed'}.
ping(Server, Port) ->
    libchunter_server:call(Server, Port, ping).

%%%===================================================================
%%% Console commands
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Opens a new console connection. A process is spawned all
%%    output from the repote console is send to the current process in
%%    the form {data, BinData}.
%% @end
%%--------------------------------------------------------------------

-spec console_open(Server::inet:ip_address() | inet:hostname(),
                   Port::inet:port_number(), VM::fifo:vm_id()) ->
                          {error, timeout} |
                          {ok, pid()}.
console_open(Server, Port, VM) ->
    console_open(Server, Port, VM, self()).

%%--------------------------------------------------------------------
%% @doc Same as console_open/3 but allows specifying the process that
%%    gets the output data.
%% @end
%%--------------------------------------------------------------------
-spec console_open(Server::inet:ip_address() | inet:hostname(),
                   Port::inet:port_number(), VM::fifo:vm_id(),
                   Proc::pid()) ->
                          {error, timeout} |
                          {ok, pid()}.
console_open(Server, Port, VM, Proc) ->
    libchunter_server:console(Server, Port, VM, Proc).


%%--------------------------------------------------------------------
%% @doc Send data to a remote console.
%% @end
%%--------------------------------------------------------------------
-spec console_send(Console::pid(), Data::binary()) ->
                          ok.
console_send(Console, Data) ->
    libchunter_console_server:send(Console, Data).

%%--------------------------------------------------------------------
%% @doc Creates a new snapshot with the given ID.
%% @end
%%--------------------------------------------------------------------
-spec snapshot(Server::inet:ip_address() | inet:hostname(),
               Port::inet:port_number(),
               UUID::fifo:vm_id(),
               SnapID::fifo:uuid()) ->
                      {error, timeout} |
                      ok.
snapshot(Server, Port, UUID, SnapID) ->
    libchunter_server:call(Server, Port, {machines, snapshot, UUID, SnapID}).

%%--------------------------------------------------------------------
%% @doc Deletes the snapshot of the given ID.
%% @end
%%--------------------------------------------------------------------
-spec delete_snapshot(Server::inet:ip_address() | inet:hostname(),
                      Port::inet:port_number(),
                      UUID::fifo:vm_id(),
                      SnapID::fifo:uuid()) ->
                             {error, timeout} |
                             ok.
delete_snapshot(Server, Port, UUID, SnapID) ->
    libchunter_server:call(Server, Port, {machines, snapshot, delete, UUID, SnapID}).

%%--------------------------------------------------------------------
%% @doc Rolls back the snapshot with the given ID, beware that all
%%   snapshots between the current state and the rollded back
%%   snapshot will be deleted!
%% @end
%%--------------------------------------------------------------------
-spec rollback_snapshot(Server::inet:ip_address() | inet:hostname(),
                        Port::inet:port_number(),
                        UUID::fifo:vm_id(),
                        SnapID::fifo:uuid()) ->
                               {error, timeout} |
                               ok.
rollback_snapshot(Server, Port, UUID, SnapID) ->
    libchunter_server:call(Server, Port, {machines, snapshot, rollback, UUID, SnapID}).


%%--------------------------------------------------------------------
%% @doc Creates a new snapshot with the given ID.
%% @end
%%--------------------------------------------------------------------
-spec store_snapshot(Server::inet:ip_address() | inet:hostname(),
                     Port::inet:port_number(),
                     UUID::fifo:vm_id(),
                     SnapID::fifo:uuid(),
                     Img::fifo:uuid()) ->
                      {error, timeout} |
                      ok.
store_snapshot(Server, Port, UUID, SnapID, Img) ->
    libchunter_server:call(Server, Port, {machines, snapshot, store, UUID, SnapID, Img}).

%%--------------------------------------------------------------------
%% @doc Starts a machine.
%%
%% This command is asyncronous.
%% @end
%%--------------------------------------------------------------------
-spec start_machine(Server::inet:ip_address() | inet:hostname(),
                    Port::inet:port_number(),
                    UUID::fifo:vm_id()) -> ok.

start_machine(Server, Port, UUID) ->
    chunter_cast(Server, Port, {machines, start, UUID}).

%%--------------------------------------------------------------------
%% @doc Deletes a machine.
%%
%% This command is asyncronous.
%% @end
%%--------------------------------------------------------------------
-spec delete_machine(Server::inet:ip_address() | inet:hostname(),
                     Port::inet:port_number(),
                     UUID::fifo:vm_id()) -> ok.
delete_machine(Server, Port, UUID) ->
    chunter_cast(Server, Port, {machines, delete, UUID}).

%%--------------------------------------------------------------------
%% @doc Creates a new machine.
%%
%% This command is asyncronous.
%% @end
%%--------------------------------------------------------------------
-spec create_machine(Server::inet:ip_address() | inet:hostname(),
                     Port::inet:port_number(),
                     UUID::fifo:vm_id(),
                     PSpec::fifo:package(),
                     DSpec::fifo:dataset(),
                     Config::fifo:config()) -> ok.
create_machine(Server, Port, UUID, PSpec, DSpec, Config) ->
    chunter_cast(Server, Port, {machines, create, UUID, PSpec, DSpec, Config}).

%%--------------------------------------------------------------------
%% @doc Updates a mchine.
%%
%% This command is asyncronous.
%% @end
%%--------------------------------------------------------------------
-spec update_machine(Server::inet:ip_address() | inet:hostname(),
                     Port::inet:port_number(),
                     UUID::fifo:vm_id(),
                     Package::fifo:package(),
                     Config::fifo:config()) -> ok.
update_machine(Server, Port, UUID, Package, Config) ->
    chunter_cast(Server, Port, {machines, update, UUID, Package, Config}).

%%--------------------------------------------------------------------
%% @doc Starts a KVM virtual machine from a iso image.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_machine(Server::inet:ip_address() | inet:hostname(),
                    Port::inet:port_number(),
                    UUID::fifo:vm_id(),
                    Imge::binary()) -> ok.
start_machine(Server, Port, UUID, Image) ->
    chunter_cast(Server, Port, {machines, start, UUID, Image}).

%%--------------------------------------------------------------------
%% @doc Stops a virtual machine.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_machine(Server::inet:ip_address() | inet:hostname(),
                   Port::inet:port_number(),
                   UUID::fifo:vm_id()) -> ok.

stop_machine(Server, Port, UUID) ->
    stop_machine(Server, Port, UUID, []).

%%--------------------------------------------------------------------
%% @doc Stops a virtual machine allowing optional options like force.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_machine(Server::inet:ip_address() | inet:hostname(),
                   Port::inet:port_number(),
                   UUID::fifo:vm_id(),
                   Options::[atom()|{atom(), term()}]) -> ok.

stop_machine(Server, Port, UUID, [force]) ->
    chunter_cast(Server, Port, {machines, stop, force, UUID});

stop_machine(Server, Port, UUID, []) ->
    chunter_cast(Server, Port, {machines, stop, UUID}).

%%--------------------------------------------------------------------
%% @doc Reboots a virtual machine.
%%
%% @end
%%--------------------------------------------------------------------
-spec reboot_machine(Server::inet:ip_address() | inet:hostname(),
                     Port::inet:port_number(),
                     UUID::fifo:vm_id()) -> ok.

reboot_machine(Server, Port, UUID) ->
    reboot_machine(Server, Port, UUID, []).


%%--------------------------------------------------------------------
%% @doc Reboots a virtual machine allowing optional options like
%%   force.
%%
%% @end
%%--------------------------------------------------------------------
-spec reboot_machine(Server::inet:ip_address() | inet:hostname(),
                     Port::inet:port_number(),
                     UUID::fifo:vm_id(),
                     Options::[atom()|{atom(), term()}]) -> ok.

reboot_machine(Server, Port, UUID, [force]) ->
    chunter_cast(Server, Port, {machines, reboot, force, UUID});

reboot_machine(Server, Port, UUID, []) ->
    chunter_cast(Server, Port, {machines, reboot, UUID}).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec chunter_cast(Server::inet:ip_address() | inet:hostname(),
                   Port::inet:port_number(),
                   Cast::fifo:chunter_message()) -> ok.

chunter_cast(Server, Port, Cast) ->
    libchunter_server:cast(Server, Port, Cast).
