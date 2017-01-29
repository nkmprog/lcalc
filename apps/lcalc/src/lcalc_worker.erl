-module(lcalc_worker).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([parse/1]).
-export([eval/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

parse(String) ->
    gen_server:call(?SERVER, {parse, String}).

eval(ParseTree) ->
    gen_server:call(?SERVER, {eval, ParseTree}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({eval, Tree}, _From, State) ->
    Reply = naive_parser:eval(Tree),
    {reply, Reply, State};
handle_call({parse, String}, _From, State) ->
    Reply = naive_parser:parse_internal(String),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
