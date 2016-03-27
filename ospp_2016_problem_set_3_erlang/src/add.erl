%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4]).

%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().


%TODO: Break into lesser and more specific functions
start(A,B, Base) ->
    Eval = getCarryOuts(A,B,Base,0,0,0),
    {S,L} = lists:unzip(Eval),
    Sum = utils:intersperse(" ", lists:map(fun utils:digit_to_ascii/1, S)),
    Couts = utils:intersperse(" ", lists:map(fun utils:digit_to_ascii/1, L)),
    AStr = utils:intersperse(" ", integer_to_list(A)),
    BStr = utils:intersperse(" ", integer_to_list(B)),
    Width = length(Sum),
    [Str1, Str2, Str3, Str4] = lists:map(fun(X) -> utils:pad_list(left, " ", Width - length(X), X) end, [Couts, AStr, BStr, Sum]),
    io:format(Str1 ++ "~n"
	      ++ utils:repeat("-", Width) ++ "~n"
	      ++ Str2 ++ "~n" 
	      ++ Str3 ++ "~n" 
	      ++ utils:repeat("-", Width) ++ "~n"
	      ++ Str4 ++ "~n"),
    ok.

%% @doc TODO: add documentation
-spec start(A,B,Base, Options) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer(),
      Option::integer() | atom() | tuple(),
      Options::[Option].


%TODO: Assumes A, B is same length: (prepend 0's to shorter int to solve this)

start(A,B,Base,[Splits , {Min,Max}]) ->
    ASubs = utils:split(integer_to_list(A), Splits),
    BSubs = utils:split(integer_to_list(B), Splits),
    Master = self(),
    LastProcess = distribute(ASubs, BSubs, Base, Master, Master,Min,Max),
    LastProcess ! {carry, 0},
    listener([]);

start(A,B,Base, Splits) ->
    ASubs = utils:split(integer_to_list(A), Splits),
    BSubs = utils:split(integer_to_list(B), Splits),
    Master = self(),
    LastProcess = distribute(ASubs, BSubs, Base, Master, Master,0,0),
    LastProcess ! {carry, 0},
    listener([]).


%%Starts all process's and Returns last process so we can send carry=0 to it.
distribute([A], [B], Base, Next,Master,Min,Max) ->
    spawn( fun() -> otherProcess(A, B, Base,Next, Master,Min,Max) end);

distribute([A|Ax], [B|Bx], Base, Next,Master,Min,Max) ->
    SpawnedProcess = spawn( fun() -> otherProcess(A, B, Base, Next, Master,Min,Max) end),
    distribute(Ax,Bx,Base,SpawnedProcess,Master,Min,Max).


otherProcess(A,B,Base, Next, Master,Min,Max) ->
    receive
	{carry, CarryIn} ->
	    {CarryOut, Sum} = addThis(list_to_integer(A),list_to_integer(B),Base,CarryIn,Min,Max),
	    Master ! {sum, Sum},
	    Next ! {carry, CarryOut}
    end.

%%Gathers Sums    
listener(Sums) ->
    receive
	{sum, Sum} ->
	    listener([Sum|Sums]);
	{carry, CarryOut} ->
	    lists:flatten([CarryOut|Sums])
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

%% @doc Adds two digits (0-9) and a carry in.
%% TODO: Add guards for valid A, B, Base and Cin
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
addThis(A,B,Base,CIN,Min,Max) ->
    {[SumHead|SumT], _} = lists:unzip(getCarryOuts(A,B,Base,CIN,Min,Max)),
    LongestCount = length(integer_to_list(max(A,B))),
    if
	length([SumHead|SumT]) > LongestCount ->
	    {SumHead, SumT};
	true ->
	    {0, [SumHead|SumT]}
    end.
	    
	
    
