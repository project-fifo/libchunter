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
         lock/3,
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
         backup/5,
         backup/10,
         backup/11,
         service_enable/4,
         service_disable/4,
         service_clear/4,
         service_enable/3,
         service_disable/3,
         service_clear/3,
         restore_backup/5,
         restore_backup/9,
         restore_backup/10,
         delete_backup/4,
         start/0,
         update/2,
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
    chunter_call(Server, Port, ping).

update(Server, Port) ->
    chunter_cast(Server, Port, update).

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
%% @doc Enable a service for a zone.
%% @end
%%--------------------------------------------------------------------
-spec service_enable(Server::inet:ip_address() | inet:hostname(),
                     Port::inet:port_number(),
                     UUID::fifo:vm_id(),
                     Service::binary()) ->
                            {error, timeout} |
                            ok.

service_enable(Server, Port, UUID, Service) ->
    chunter_cast(Server, Port, {machines, service, enable, UUID, Service}).

%%--------------------------------------------------------------------
%% @doc Disables a service for a zone.
%% @end
%%--------------------------------------------------------------------
-spec service_disable(Server::inet:ip_address() | inet:hostname(),
                      Port::inet:port_number(),
                      UUID::fifo:vm_id(),
                      Service::binary()) ->
                             {error, timeout} |
                             ok.

service_disable(Server, Port, UUID, Service) ->
    chunter_cast(Server, Port, {machines, service, disable, UUID, Service}).

%%--------------------------------------------------------------------
%% @doc Clears a service that is in maintaiance or degraded state
%% @end
%%--------------------------------------------------------------------
-spec service_clear(Server::inet:ip_address() | inet:hostname(),
                    Port::inet:port_number(),
                    UUID::fifo:vm_id(),
                    Service::binary()) ->
                           {error, timeout} |
                           ok.

service_clear(Server, Port, UUID, Service) ->
    chunter_cast(Server, Port, {machines, service, clear, UUID, Service}).

%%--------------------------------------------------------------------
%% @doc Enable a service for a zone.
%% @end
%%--------------------------------------------------------------------
-spec service_enable(Server::inet:ip_address() | inet:hostname(),
                     Port::inet:port_number(),
                     Service::binary()) ->
                            {error, timeout} |
                            ok.

service_enable(Server, Port, Service) ->
    chunter_cast(Server, Port, {service, enable, Service}).

%%--------------------------------------------------------------------
%% @doc Disables a service for a zone.
%% @end
%%--------------------------------------------------------------------
-spec service_disable(Server::inet:ip_address() | inet:hostname(),
                      Port::inet:port_number(),
                      Service::binary()) ->
                             {error, timeout} |
                             ok.

service_disable(Server, Port, Service) ->
    chunter_cast(Server, Port, {service, disable, Service}).

%%--------------------------------------------------------------------
%% @doc Clears a service that is in maintaiance or degraded state
%% @end
%%--------------------------------------------------------------------
-spec service_clear(Server::inet:ip_address() | inet:hostname(),
                    Port::inet:port_number(),
                    Service::binary()) ->
                           {error, timeout} |
                           ok.

service_clear(Server, Port, Service) ->
    chunter_cast(Server, Port, {service, clear, Service}).

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
    chunter_cast(Server, Port, {machines, snapshot, UUID, SnapID}).

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
    chunter_cast(Server, Port, {machines, snapshot, delete, UUID, SnapID}).

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
    chunter_call(Server, Port, {machines, snapshot, rollback, UUID, SnapID}).

%%--------------------------------------------------------------------
%% @doc Uploads a snapshot to s3.
%% @end
%%--------------------------------------------------------------------
backup(Server, Port, UUID, SnapId, S3Server, S3Port, Bucket, AKey,
       SKey, Bucket) ->
    backup(Server, Port, UUID, SnapId, S3Server, S3Port, Bucket, AKey,
           SKey, Bucket, []).

backup(Server, Port, UUID, SnapId, S3Server, S3Port, Bucket, AKey,
       SKey, Bucket, Opts) ->
    Opts1 = [{access_key, AKey},
             {secret_key, SKey},
             {s3_host, S3Server},
             {s3_port, S3Port},
             {s3_bucket, Bucket} | Opts],
    backup(Server, Port, UUID, SnapId, Opts1).

backup(Server, Port, UUID, SnapId, Opts) ->
    chunter_cast(Server, Port, {machines, backup, UUID, SnapId, Opts}).

%%--------------------------------------------------------------------
%% @doc Downlaods a snapshot from s3.
%% @end
%%--------------------------------------------------------------------
restore_backup(Server, Port, UUID, SnapId, S3Server, S3Port, Bucket, AKey,
               SKey) ->
    restore_backup(Server, Port, UUID, SnapId, S3Server, S3Port, Bucket, AKey,
                   SKey, []).

restore_backup(Server, Port, UUID, SnapId, S3Server, S3Port, Bucket, AKey,
               SKey, Opts) ->
    Opts1 = [{access_key, AKey},
             {secret_key, SKey},
             {s3_host, S3Server},
             {s3_port, S3Port},
             {s3_bucket, Bucket} | Opts],
    restore_backup(Server, Port, UUID, SnapId, Opts1).

restore_backup(Server, Port, UUID, SnapId, Opts) ->
    chunter_cast(Server, Port, {machines, backup, restore, UUID, SnapId, Opts}).

delete_backup(Server, Port, UUID, SnapID) ->
    chunter_cast(Server, Port, {machines, backup, delete, UUID, SnapID}).

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

-spec lock(Server::inet:ip_address() | inet:hostname(),
           Port::inet:port_number(),
           UUID::fifo:vm_id()) ->
                  ok | failed.

lock(Server, Port, UUID) ->
    chunter_call(Server, Port, {lock, UUID}).

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
                     Config::fifo:config()) -> ok | {error, lock}.
create_machine(Server, Port, UUID, PSpec, DSpec, Config) ->
    chunter_call(Server, Port, {machines, create, UUID, PSpec, DSpec, Config}).

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

chunter_call(Server, Port, Call) ->
    libchunter_server:call(Server, Port, Call).
