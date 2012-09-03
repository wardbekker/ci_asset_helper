-module(ci_asset_helper_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ci_asset_helper_sup:start_link().

stop(_State) ->
    ok.
