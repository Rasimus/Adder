%% @doc Erlang mini project.
-module(add).
-export([start/3, start/4, getCarryOuts/4, addDigits/4]).


%%%%%%%%Borde ligga i utls

%% @doc takes list of ints and returns int
%% === Example ===
%% utils:list_to_int(0,[1,2,3]).
%% 123
-spec list_to_int(Acc::integer(), L::[integer()]) -> integer().

list_to_int(Acc,[]) ->
    Acc;
list_to_int(Acc, [L]) ->
    Acc*10 + L;
list_to_int(Acc, [H|L]) ->
    NewAcc = Acc*10 + H,
    list_to_int(NewAcc, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%% @doc TODO: add documentation
-spec start(A,B,Base) -> ok when 
      A::integer(),
      B::integer(), 
      Base::integer().
%TODO: Break into lesser and more specific functions
start(A,B, Base) ->
    %L = getCarryOuts(A, B, Base, 0),
    %Sum = utils:intersperse(" ", integer_to_list(A + B)),
    Eval = getCarryOuts(A,B,Base,0),
    
    {S,L} = lists:unzip(Eval),
    
    
    Sum = utils:intersperse(" ", integer_to_list(list_to_int(0,S))),

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
      Option::atom() | tuple(),
      Options::[Option].

start(A,B,Base, Options) ->
    tbi.


%% @doc Creates a list of carry outs from an arithmetic addition.
-spec getCarryOuts(A, B, Base, Cin) -> [{Sum, Cout}] when
      A::integer(),
      B::integer(), 
      Base::integer(),
      Cin::integer(),
      Sum::integer(),
      Cout::integer().
 
getCarryOuts(A, B, Base, Cin) ->
    lists:reverse(getCarryOutsAux(A, B, Base,Cin)).

getCarryOutsAux(0, 0, _Base, Cin) ->
    case Cin of
	0 -> [];
	1 -> [{1,1}]
    end;
getCarryOutsAux(A, B, Base, Cin) ->
    {N, Cout} = addDigits(A rem 10, B rem 10, Base, Cin),
    [{N, Cin} | getCarryOutsAux(A div 10, B div 10, Base, Cout)].

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
