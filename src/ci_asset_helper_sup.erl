
-module(ci_asset_helper_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [webmachine_child_spec()],
    {ok, { {one_for_one, 5, 10}, Children} }.

read_config(Filename) ->
    {ok, [Config]} = file:consult(filename:join(
                                  [filename:dirname(code:which(?MODULE)),
                                   "..", "config", Filename])),
    Config.

webmachine_child_spec() ->
    {webmachine_mochiweb,
     {
       webmachine_mochiweb, start, [read_config("webmachine.config")]
     },
     permanent, 5000, worker, dynamic
    }.
