-module(naive_test).

-include_lib("eunit/include/eunit.hrl").

naive_test() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{"Comprehensive", fun comprehensive_test/0}]}.

%%====================================================================
%% Fixture Functions
%%====================================================================
setup() ->
    ok.

teardown(#{}) ->
    ok.

%%====================================================================
%% Test cases
%%====================================================================
comprehensive_test() ->
    LExpression = "(((lambda x.lambda y.lambda z.(x (y z)) lambda x0.x0) lambda x1.x1) 2)",
    {ok, {Tree, _}} = naive_parser:parse(LExpression),
    Result = naive_interpreter:eval(Tree),
    ?assert(is_map(Result)),
    ?assertEqual(#{type => name, value => <<"2">>}, Result).

%%====================================================================
%% Helper functions
%%====================================================================
