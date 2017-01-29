-module(naive_parser).

%% API
-export([parse/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse(Input)
  when is_list(Input) ->
    parse(list_to_binary(Input));
parse(Input)
  when is_binary(Input) ->
    {ok, ll1(expression, Input)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ll1(application, Input0) ->
    {ok, Input1} = match(<<"(">>, Input0),
    {FunctionExpression, Input2} = ll1(expression, Input1),
    {ArgumentExpression, Input3} = ll1(expression, Input2),
    {ok, Input4} = match(<<")">>, Input3),
    Expression = #{type     => application,
                   fun_expr => FunctionExpression,
                   arg_expr => ArgumentExpression},
    {Expression, Input4};
ll1(lambda, Input0) ->
    {ok, Input1} = match(<<"lambda">>, Input0),
    {Name, Input2} = ll1(name, Input1),
    {ok, Input3} = match(<<".">>, Input2),
    {Body, Input4} = ll1(expression, Input3),
    Expression = #{type => lambda,
                   name => Name,
                   body => Body},
    {Expression, Input4};
ll1(name, Input0) ->
    case lookahead(Input0) of
        {<<"lambda">>, _Input} ->
            {error, reserved_word};
        {<<"(">>, _Input} ->
            {error, reserved_word};
        {Name, Input} ->
            {#{type => name, value => Name}, Input}
    end;
ll1(expression, Input0) ->
    Attempts = lists:map(fun(Type) -> try_ll1(Type, Input0) end,
                         [application, lambda, name]),
    Successes = lists:filter(fun({Expr, Input})
                                   when is_map(Expr),
                                        is_binary(Input) -> true;
                                (_) -> false
                             end, Attempts),
    case Successes of
        [R] -> R;
        _Err ->
            error(invalid_input)
    end.

try_ll1(Type, Input) ->
    try ll1(Type, Input)
    catch Error:Pattern -> {Error, Pattern}
    end.

lookahead(Input)
  when is_binary(Input) ->
    lookahead_token(Input, <<"">>).

-spec lookahead_token(binary(), binary()) -> {binary(), binary()}.
lookahead_token(<<" ", Rest/binary>>, <<"">> = Acc) ->
    %% trim whitespaces
    lookahead_token(Rest, Acc);
lookahead_token(<<"(", Rest/binary>>, <<"">>) ->
    {<<"(">>, Rest};
lookahead_token(<<"(", _/binary>> =Rest, Acc) ->
    {Acc, Rest};
lookahead_token(<<")", Rest/binary>>, <<"">>) ->
    {<<")">>, Rest};
lookahead_token(<<")", _/binary>> =Rest, Acc) ->
    {Acc, Rest};
lookahead_token(<<".", Rest/binary>>, <<"">>) ->
    {<<".">>, Rest};
lookahead_token(<<".", _/binary>> =Rest, Acc) ->
    {Acc, Rest};
lookahead_token(<<" ", Rest/binary>>, Acc) ->
    {Acc, Rest};
lookahead_token(<<Char:1/binary, Rest/binary>>, Acc0) ->
    Acc = <<Acc0/binary, Char/binary>>,
    lookahead_token(Rest, Acc);
lookahead_token(<<>>, Acc) ->
    {Acc, <<>>}.

match(Expected, Input0)
  when is_binary(Expected) ->
    case lookahead(Input0) of
        {Expected, Input} -> {ok, Input};
        {_Token, _Rest} -> {error, bad_match}
    end.
