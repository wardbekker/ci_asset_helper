%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% Resource for returning static assets
%% @end
-module(static_file_resource).

-define(STORE_PATH, "uploaded_assets").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         allowed_methods/2,
         resource_exists/2,
         content_types_provided/2,
         provide_content/2,
         last_modified/2,
         generate_etag/2]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

init([]) -> {ok, undefined}.

allowed_methods(ReqData, Context) ->
    {['HEAD', 'GET'], ReqData, Context}.

resource_exists(ReqData, Ctx) ->
    {true, ReqData, Ctx}.

content_types_provided(ReqData, Ctx) ->
    Parts = wrq:path_tokens(ReqData),
    Path = file_path(Parts),
    {[{webmachine_util:guess_mime(Path), provide_content}], ReqData, Ctx}.

provide_content(ReqData, Context) ->
    Parts = wrq:path_tokens(ReqData),
    Path = file_path(Parts),
    {ok, Content } = file:read_file(Path),
    {Content, ReqData, Context}.

file_path(Parts) ->
    filename:join(
      [filename:dirname(code:which(?MODULE)),
       "..", ?STORE_PATH] ++ Parts).

last_modified(ReqData, Context) ->
    Parts = wrq:path_tokens(ReqData),
    Path = file_path(Parts),
    LMod = filelib:last_modified(Path),
    {LMod, ReqData, Context}.

generate_etag(ReqData, Context) ->
    Parts = wrq:path_tokens(ReqData),
    Path = file_path(Parts),
    {ok, Content } = file:read_file(Path),
    Etag = mochihex:to_hex(binary_to_list(crypto:sha(Content))),
    {Etag, ReqData, Context}.
