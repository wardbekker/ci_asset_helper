%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% Upload resource
%% @end
-module(upload_resource).

-define(STORE_PATH, "uploaded_assets").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([init/1, to_html/2, allowed_methods/2, process_post/2]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['POST', 'GET'], ReqData, State}.

to_html(ReqData, State) ->
    StorePath = filename:join(
                  [filename:dirname(code:which(?MODULE)), "..", ?STORE_PATH]
                 ),
    {ok, FileNames } = file:list_dir(StorePath),
    Data = [{files, lists:sort(FileNames) }],
    {ok, Content} = files_dtl:render(Data),
    {Content, ReqData, State}.

process_post(ReqData, State) ->
    ContentType = wrq:get_req_header("content-type", ReqData),
    Boundary = string:substr(ContentType, string:str(ContentType, "boundary=") 
                             + length("boundary=")),
    {FileName, _FileSize, Content} = get_streamed_body(
                                      webmachine_multipart:stream_parts(
                                        wrq:stream_req_body(ReqData, 1024), 
                                        Boundary), [],[]),
    StorePath = filename:join(
                  [filename:dirname(code:which(?MODULE)), "..", ?STORE_PATH, unique_file_name() ++ "_" ++ FileName]
                 ),
    io:format("path: ~p", [StorePath]),
    ok = file:write_file(StorePath, Content),
    Reply = "upload ok\n",
    ReqData1 = wrq:set_resp_body(Reply, ReqData),
    {true, ReqData1, State}.

get_streamed_body(done_parts, FileName, Acc) ->
    Bin = iolist_to_binary(lists:reverse(Acc)),
    {FileName, size(Bin)/1024.0, Bin};
get_streamed_body({{"filedata", {Params, _Hdrs}, Content}, Next}, Props, Acc) ->
    FileName = binary_to_list(proplists:get_value(<<"filename">>, Params)),
    get_streamed_body(Next(),[FileName|Props],[Content|Acc]).

unique_file_name() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    Timestamp = MegaSecs*1000*1000*1000*1000 + Secs * 1000 * 1000 + MicroSecs,
    integer_to_list(Timestamp).

