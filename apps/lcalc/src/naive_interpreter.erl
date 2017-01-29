-module(naive_interpreter).

%% API
-export([eval/1]).

%%%===================================================================
%%% API
%%%===================================================================
eval(#{type     := application,
       fun_expr := FunctionExpression0 = #{type := lambda},
       arg_expr := ArgumentExpression})  ->
    FunctionExpression = normal_order_beta_reduction(FunctionExpression0, ArgumentExpression),
    eval(FunctionExpression);
eval(#{type     := application,
       fun_expr := FunctionExpression0 = #{type := application}
      }=Application) ->
    FunctionExpression = eval(FunctionExpression0),
    eval(Application#{fun_expr:=FunctionExpression});
eval(#{type     := application,
       fun_expr := #{type := name}
      }=Application) ->
    Application;
eval(#{type     := lambda,
       body     := Body}=Lambda0) ->
    %% Normal order reduction has already occured
    Lambda0#{body := eval(Body)};
eval(Expression) ->
    Expression.

%%%===================================================================
%%% Internal functions
%%%===================================================================
normal_order_beta_reduction(#{type := lambda,
                              name := Name,
                              body := Body}, ArgumentExpression) ->
    normal_order_beta_reduction(Name, Body, ArgumentExpression).

normal_order_beta_reduction(BoundVariable,
                            #{type     := application,
                              fun_expr := FunctionExpression0,
                              arg_expr := ArgumentExpression0}=Application,
                            ReplaceWith) ->
    FunctionExpression = normal_order_beta_reduction(BoundVariable, FunctionExpression0, ReplaceWith),
    ArgumentExpression = normal_order_beta_reduction(BoundVariable, ArgumentExpression0, ReplaceWith),
    Application#{fun_expr := FunctionExpression,
                 arg_expr := ArgumentExpression};
normal_order_beta_reduction(BoundVariable,
                            #{type := lambda,
                              body := Body0}=Lambda,
                            ReplaceWith) ->
    Body = normal_order_beta_reduction(BoundVariable, Body0, ReplaceWith),
    Lambda#{body:=Body};
normal_order_beta_reduction(#{type  := name,
                              value := BoundVariable},
                            #{type  := name,
                              value := BoundVariable},
                            ReplaceWith) ->
    ReplaceWith;
normal_order_beta_reduction(_BoundVariable,
                            #{type := name}=Name,
                            _ReplaceWith) ->
    Name.
