%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% @doc Adds positive integers A and B in a given base and prints a textual representation of the addition.
%% <div class="example">```
%% === Example ===
%% 1> start(50, 70, 10).
%%   1 0 0
%%   -----
%%     5 0 
%%     7 0
%%   -----
%%   1 2 0'''
%% </div>
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().


start(A,B, Base) ->
    Eval = getCarryOuts(A,B,Base,0,0,0),
    {S,L} = lists:unzip(Eval),
    print(A,B,{S,L}).

print(A,B,{S,L}) ->
    Sum = utils:intersperse(" ", lists:map(fun utils:digit_to_ascii/1, S)),
    Couts = utils:intersperse(" ", lists:map(fun utils:digit_to_ascii/1, L)),
    AStr = utils:intersperse(" ", integer_to_list(A)),
    BStr = utils:intersperse(" ", integer_to_list(B)),
    Width = length(Sum),
    [CoutsFmt, AStrFmt, BStrFmt, SumFmt] = lists:map(fun(X) -> utils:pad_list(left, " ", Width - length(X), X) end, [Couts, AStr, BStr, Sum]),
    io:format(CoutsFmt ++ "~n"
	      ++ utils:repeat("-", Width) ++ "~n"
	      ++ AStrFmt ++ "~n" 
	      ++ BStrFmt ++ "~n" 
	      ++ utils:repeat("-", Width) ++ "~n"
	      ++ SumFmt ++ "~n"),
    ok.    
    

%% @doc Same as {@link start/3} but with additional options available.
%% Options:
%%
%%   Splits - 
%%    Splits the calculation into multiple processes where 'Splits' is the number of processes.
%%
%%   {Min, Max} - 
%%    Used for debugging. Randomly sleeps the processes of the program for a minimum of 'Min' ms and a maximum of 'Max' ms.
%%
%%   spec - Turns on speculative mode when already using option Splits.
%%
%%  === Examples ===
%%  <div class="example">```
%%  1> start(A, B, Base, [Splits]).
%%  2> start(A, B, Base, [Splits, {Min, Max}]).
%%  3> start(A, B, Base, [Splits, {Min, Max}, spec]).
%%  4> start(A, B, Base, [Splits, spec]).'''
%% </div>

-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::integer() | atom() | tuple(),
      Options::[Option].    

start(A,B,Base,[Splits , {Min,Max}]) ->

    %%Make A and B the same length.
    AList = integer_to_list(A),
    BList = integer_to_list(B),
    Longest = max(length(AList),length(BList)),
    Az = utils:pad_list(left, $0, Longest - length(AList), AList),
    Bz = utils:pad_list(left, $0, Longest - length(BList), BList),

    ASubs = utils:split(Az, Splits),
    BSubs = utils:split(Bz, Splits),
    Master = self(),
    LastProcess = distribute(ASubs, BSubs, Base, Master, Master,Min,Max),
    LastProcess ! {carry, 0},
    R = listener([]),
    {Sums, Carries} = lists:unzip(R),
    print(A,B,{lists:flatten(Sums), lists:flatten(Carries)});

start(A,B,Base, [Splits, spec]) ->
    start(A,B,Base, [Splits, {0, 0}, spec]);
start(A,B,Base, [Splits, {Min, Max}, spec]) ->
    %%Make A and B the same length.
    AList = integer_to_list(A),
    BList = integer_to_list(B),
    Longest = max(length(AList),length(BList)),
    Az = utils:pad_list(left, '0', Longest - length(AList), AList),
    Bz = utils:pad_list(left, '0', Longest - length(BList), BList),

    ASubs = utils:split(Az, Splits),
    BSubs = utils:split(Bz, Splits),
    Master = self(),
    LastProcess = distribute(ASubs, BSubs, Base, Master, Master,Min,Max,spec),
    LastProcess ! {carry, 0},
    R = listener([]),
    {Sums, Carries} = lists:unzip(R),
    print(A,B,{lists:flatten(Sums), lists:flatten(Carries)});    

start(A,B,Base, Splits) ->
    start(A,B,Base,[Splits, {0, 0}]).


%%Starts all process's and Returns last process so we can send carry=0 to it.
distribute([A], [B], Base, Next,Master,Min,Max) ->
    spawn( fun() -> otherProcess(A, B, Base,Next, Master,Min,Max) end);

distribute([A|Ax], [B|Bx], Base, Next,Master,Min,Max) ->
    SpawnedProcess = spawn( fun() -> otherProcess(A, B, Base, Next, Master,Min,Max) end),
    distribute(Ax,Bx,Base,SpawnedProcess,Master,Min,Max).

distribute([A], [B], Base, Next,Master,Min,Max,spec) ->
    spawn( fun() -> otherProcess(A, B, Base,Next, Master,Min,Max,spec) end);

distribute([A|Ax], [B|Bx], Base, Next,Master,Min,Max, spec) ->
    SpawnedProcess = spawn( fun() -> otherProcess(A, B, Base, Next, Master,Min,Max,spec) end),
    distribute(Ax,Bx,Base,SpawnedProcess,Master,Min,Max,spec).

otherProcess(A,B,Base, Next, Master,Min,Max) ->
    receive
	{carry, CarryIn} ->
	    {CarryOut, Sum,Carries} = addThis(list_to_integer(A),list_to_integer(B),Base,CarryIn,Min,Max),
	    Master ! {sum, Sum,Carries},
	    Next ! {carry, CarryOut}
    end.

%%Speculative mode version of otherProcess
otherProcess(A, B, Base, Next, Master,Min, Max, spec) ->
    Parent = self(),
    CarryChild = spawn(fun() -> speculativeProcess(A, B, Base, 1, Min, Max, Parent) end),
    NoCarryChild = spawn(fun() -> speculativeProcess(A, B, Base, 0, Min, Max, Parent) end),
    {CarryOut, Sum, Carries} = otherProcessLoop(NoCarryChild, CarryChild, undecided, undecided, undecided),
    Master ! {sum, Sum,Carries},
    Next ! {carry, CarryOut}.


%%Stateful loop for figuring out which speculative result to use
otherProcessLoop(NoCarryChild, CarryChild, CarryIn, CarryResult, NoCarryResult) ->
    receive
    	{carry, 1} when CarryResult =:= undecided ->
    	    exit(NoCarryChild, kill), 		    
    	    R = otherProcessLoop(NoCarryChild, CarryChild, 1, CarryResult, NoCarryResult);
    	{carry, 1} ->
    	    exit(NoCarryChild, kill),
    	    R = CarryResult;
    	{carry, 0} when NoCarryResult =:= undecided ->
    	    exit(CarryChild, kill),
    	    R = otherProcessLoop(NoCarryChild, CarryChild, 0, CarryResult, NoCarryResult);
    	{carry, 0} ->
    	    exit(CarryChild, kill),
    	    R = NoCarryResult;
    	{spec, CarryIn, Result} ->
    	    R = Result;
    	{spec, 0, Result} ->
    	    R = otherProcessLoop(NoCarryChild, CarryChild, CarryIn, CarryResult, Result);
    	{spec, 1, Result} ->
    	    R = otherProcessLoop(NoCarryChild, CarryChild, CarryIn, Result, NoCarryResult)
    end,
    R.

speculativeProcess(A, B, Base, CarryIn, Min, Max, Parent) ->
    Result = addThis(list_to_integer(A),list_to_integer(B),Base,CarryIn,Min,Max),
    Parent ! {spec, CarryIn, Result}.

%%Gathers Sums    
listener(Sums) ->
    receive
	{sum, Sum,Carries} ->
	    listener([{Sum,Carries}|Sums]);
	{carry, CarryOut} ->
	    [{CarryOut,CarryOut}|Sums]
    end.


%% @doc Creates a list of carry outs from an arithmetic addition.
-spec getCarryOuts(A, B, Base, Cin,Min,Max) -> [{Sum, Cout}] when
      A::integer(),
      B::integer(), 
      Base::integer(),
      Cin::integer(),
      Sum::integer(),
      Cout::integer(),
      Min::integer(),
      Max::integer().

getCarryOuts(0, 0, _Base, 0, _, _) ->
    [{0, 0}];
 
getCarryOuts(A, B, Base, Cin,Min,Max) ->
    {Q,W,E} = now(),
    random:seed(Q,W,E),
    lists:reverse(getCarryOutsAux(A, B, Base,Cin,Min,Max)).

getCarryOutsAux(0, 0, _Base, Cin,_,_) ->
    case Cin of
	0 -> [];
	1 -> [{1,1}]
    end;

getCarryOutsAux(A, B, Base, Cin,0,0) ->
    {N, Cout} = addDigits(A rem 10, B rem 10, Base, Cin),
    [{N, Cin} | getCarryOutsAux(A div 10, B div 10, Base, Cout,0,0)];

getCarryOutsAux(A, B, Base, Cin,Min,Max) ->
    SleepTime = random:uniform(Max-Min)+Min,
    timer:sleep(SleepTime),
    {N, Cout} = addDigits(A rem 10, B rem 10, Base, Cin),
    [{N, Cin} | getCarryOutsAux(A div 10, B div 10, Base, Cout,Min,Max)].


%% Adds two digits (0-9) and a carry in.
-spec addDigits(A, B, Base, Cin) -> {Sum, Cout} when
      A::integer(),
      B::integer(),
      Base::integer(),
      Cin::integer(),
      Sum::integer(),
      Cout::integer().


addDigits(A, B, Base, Cin) ->
    Sum = (A + B + Cin) rem Base,
    Cout = (A + B + Cin) div Base,
    {Sum, Cout}.

%Returns {COUT, SUM}::{int, int}
addThis(A,B,Base,Cin,Min,Max) ->
    {[SumHead|SumT], Carries} = lists:unzip(getCarryOuts(A,B,Base,Cin,Min,Max)),
    LongestCount = length(integer_to_list(max(A,B))),
    if
	length([SumHead|SumT]) > LongestCount ->
	    {SumHead, SumT,lists:nthtail(1,Carries)};
	true ->
	    {0, [SumHead|SumT], Carries}
    end.
