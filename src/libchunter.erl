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
-export([list_machines/2,
         delete_machine/3,
	 get_machine/3,
	 get_machine_info/3,
	 create_machine/7,
	 start_machine/3,
	 start_machine/4,
	 stop_machine/3,
	 reboot_machine/3,
	 list_packages/2,
	 list_datasets/2,
	 get_dataset/3,
	 get_memory_info/2,
	 list_keys/2]).

%% @type uuid() = binary().
-type uuid() ::
	binary().

%% @type machine() = uuid().
-type machine() ::
	uuid().


%% @type user() = uuid().
-type user() :: uuid().

%% @type permission() = atom() | binary().
-type permission() ::
	atom() |
	binary().

%% @type permissions() = [permission()].
-type permissions() ::
	[permission()].



%% @type cached_auth() = {user(), permissions()}.
-type cached_auth() :: 
	{user(), permissions()}.

%% @type auth() = system | user() | cached_auth().
-type auth() ::
	system | 
	user() |
	cached_auth().

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @spec (pid(), auth()) -> [machine()]
%%
%% @doc Lists all machines on a server visible to the user.
%%
%% @end
%%--------------------------------------------------------------------

list_machines(Pid, Auth) ->
    chunter_call(Pid, Auth, {machines, list}).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), machine()) -> [term()]
%%
%% @doc Lists all machines on a server visible to the user.
%%
%% This command is pretty much equal to vmadm get machine.
%% @end
%%--------------------------------------------------------------------

get_machine(Pid, Auth, UUID) ->
    chunter_call(Pid, Auth, {machines, get, UUID}).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), machine()) -> [term()]
%%
%% @doc Gets the runtime info of a KVM machine.
%%
%% This command is pretty much equal to vmadm info machine.
%% @end
%%--------------------------------------------------------------------

get_machine_info(Pid, Auth, UUID) ->
    chunter_call(Pid, Auth, {machines, info, UUID}).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), machine()) -> ok
%%
%% @doc Starts a machine.
%%
%% This command is asyncronous.
%% @end
%%--------------------------------------------------------------------


start_machine(Pid, Auth, UUID) ->
    chunter_cast(Pid, Auth, {machines, start, UUID}).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), machine()) -> ok
%%
%% @doc Deletes a machine.
%%
%% This command is asyncronous.
%% @end
%%--------------------------------------------------------------------

delete_machine(Pid, Auth, UUID) ->
    chunter_cast(Pid, Auth, {machines, delete, UUID}).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), 
%%        binary(), binary(), binary(), 
%%        [{binary(),binary()}], [{binary(),binary()}]) -> machine()
%%
%% @doc Creates a new machine.
%%
%% @end
%%--------------------------------------------------------------------

create_machine(Pid, {Auth, _}, Name, PackageUUID, DatasetUUID, Metadata, Tags) ->
    create_machine(Pid, Auth, Name, PackageUUID, DatasetUUID, Metadata, Tags);
create_machine(Pid, Auth, Name, PackageUUID, DatasetUUID, Metadata, Tags) ->
    chunter_cast(Pid, Auth, {machines, create, Name, PackageUUID, DatasetUUID, Metadata, Tags}).

%%--------------------------------------------------------------------
%% @spec (pid(), auth(), 
%%        machine(), binary()) -> ok.
%%
%% @doc Starts a KVM virtual machine from a iso image.
%%
%% @end
%%--------------------------------------------------------------------

start_machine(Pid, Auth, UUID, Image) ->
    chunter_cast(Pid, Auth, {machines, start, UUID, Image}).

stop_machine(Pid, Auth, UUID) ->
    chunter_cast(Pid, Auth, {machines, stop, UUID}).

reboot_machine(Pid, Auth, UUID) ->
    chunter_cast(Pid, Auth, {machines, stop, UUID}).

list_packages(Pid, Auth) ->
    chunter_call(Pid, Auth, {packages, list}).

list_datasets(Pid, Auth) ->
    chunter_call(Pid, Auth, {datasets, list}).

get_dataset(Pid, Auth, UUID) ->
    chunter_call(Pid, Auth, {datasets, get, UUID}).

list_keys(Pid, Auth) ->
    chunter_call(Pid, Auth, {keys, list}).
    
get_memory_info(Pid, Auth) ->
    chunter_call(Pid, Auth, {info, memory}).
    

%%%===================================================================
%%% Internal functions
%%%===================================================================

chunter_call(Pid, {Auth, _}, Call) ->    
    chunter_call(Pid, Auth, Call);

chunter_call(Pid, Auth, Call) ->
    try 
	gen_server:call(Pid, {call, Auth, Call})
    catch
	T:E ->
	    lager:error([{fifi_component, libchunter}, {user, Auth}],
			"libchunter:call - Error ~p:~p, Call: ~p.",
			[T, E, Call]),
	    {error, cant_call}
    end.

chunter_cast(Pid, {Auth, _}, Call) ->
    chunter_cast(Pid, Auth, Call);

chunter_cast(Pid, Auth, Call) ->
    try
	gen_server:cast(Pid, {cast, Auth, Call})
    catch
	T:E ->
	    lager:error([{fifi_component, libchunter}, {user, Auth}],
			"libchunter:cast - Error ~p:~p, Cast: ~p.",
			[T, E, Call]),
	    {error, cant_call}
    end.
