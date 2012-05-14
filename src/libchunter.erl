%%%-------------------------------------------------------------------
%%% @author Heinz N. Gies <heinz@licenser.net>
%%% @copyright (C) 2012, Heinz N. Gies
%%% @doc
%%%
%%% @end
%%% Created : 11 May 2012 by Heinz N. Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(libchunter).

%% API
-export([list_machines/2,
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
	 list_keys/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

list_machines(Pid, Auth) ->
    chunter_call(Pid, Auth, {machines, list}).

get_machine(Pid, Auth, UUID) ->
    chunter_call(Pid, Auth, {machines, get, UUID}).

get_machine_info(Pid, Auth, UUID) ->
    chunter_call(Pid, Auth, {machines, info, UUID}).

start_machine(Pid, Auth, UUID) ->
    chunter_cast(Pid, Auth, {machines, start, UUID}).

create_machine(Pid, Auth, Name, PackageUUID, DatasetUUID, Metadata, Tags) ->
    chunter_call(Pid, Auth, {machines, create, Name, PackageUUID, DatasetUUID, Metadata, Tags}).

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
    

%%%===================================================================
%%% Internal functions
%%%===================================================================

chunter_call(Pid, {Auth, _}, Call) ->
    chunter_call(Pid, Auth, Call);

chunter_call(Pid, Auth, Call) ->
    try 
	gen_server:call(Pid, {call, Auth, Call})
    catch
	_T:_E ->
	    {error, cant_call}
    end.



chunter_cast(Pid, {Auth, _}, Call) ->
    chunter_cast(Pid, Auth, Call);

chunter_cast(Pid, Auth, Call) ->
    try
	gen_server:cast(Pid, {cast, Auth, Call})
    catch
	_T:_E ->
	    {error, cant_call}
    end.
