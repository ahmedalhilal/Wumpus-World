% These are predicates some have 1 aurguments, while others have 2

:- dynamic ([
	     agentloc/1,
	     goldloc/1,
	     pitloc/1,
	     game_t/1,
	     game_s/1,
	     visited/1,
	     visited_cells/1,
	     size/1,
	     wumpusloc/1,
             pit/2,
             wumpus/2,
             gold/2
	    ]).

% The procedure to start the game

start :-
    init,
    format('Lets play the game!~n', []),
    room([[1,1]]).

% The current location of the agent where it perceives it's surroundings

room(VisitedList) :-
    sense(Perception),
    agentloc(AL),
    format('I\'m in the room ~p, and : ~p~n', [AL,Perception]),
    updateKB(Perception),
    ask_KB(VisitedList, Action),
    format('I will move to : ~p~n', [Action]),
    update_time,
    update_game_s,
    agentloc(Aloc),
    VL = [Aloc|VisitedList],
    status,
    result(VL).

% The result of the game whether the agent fails or win the game

result(VisitedList) :-
    agentloc(AL),
    goldloc(GL),
    wumpusloc(WL),
    game_s(S),
    game_t(T),

    ( AL=GL -> writeln('Congratulations!'), format('game_s: ~p,~n Time: ~p', [S,T])
    ; AL=WL -> format('Woups,you lost~n', []),
               format('game_s: ~p,~n Time: ~p', [S,T])
    ; room(VisitedList)
    ).

% updating time: 

update_time :-
    game_t(T),
    NewTime is T+1,
    retractall( game_t(_) ),
    assert( game_t(NewTime) ).

update_game_s :-
    agentloc(AL),
    goldloc(GL),
    wumpusloc(WL),
    update_game_s(AL, GL, WL).

update_game_s(P) :-
    game_s(S),
    Newgame_s is S+P,
    retractall( game_s(_) ),
    assert( game_s(Newgame_s) ).

update_game_s(AL, AL, _) :-
    update_game_s(1000).

update_game_s(_,_,_) :-
    update_game_s(-1).

update_agentloc(NewAL) :-
    retractall( agentloc(_) ),
    assert( agentloc(NewAL) ).

is_pit(no,  X) :-
    \+ pitloc(X).
is_pit(yes, X) :-
    pitloc(X).

% The perceptions the agent makes

perception([_Stench,_Breeze,_Glitter]) :-
    agentloc(AL),
    isStinky(AL),
    isBleezie(AL),
    isGlittering(AL).

test_perception :-
	sense(Percept),
	format('I can sense ~p, ',[Percept]).

sense([Stench,Breeze,Glitter]) :-
	smelly(Stench),
	bleezy(Breeze),
	glittering(Glitter).

% This is for displaying the status of the game:

status :-
    wumpusloc(WL),
    goldloc(GL),
    agentloc(AL),

    ( is_pit(yes, AL) -> format('Boom, you havefallen into a pit ~n', []),
      fail
    ; stnd(AL, GL, WL)
      %\+ pitloc(yes, Al),
    ).

stnd(_, _, _) :-
    format('I am still in the game, I have to play~n', []).

stnd(AL, _, AL) :-
    format('Oh oh! The wumpus ate you', []),
    fail.

stnd(AL, AL, _) :-
    format('You found the gold,grab it', []),
    true.

% This is for initializing the game

init :-
    init_game,
    init_land,
    init_agent,
    init_wumpus.

init_game :-
    retractall( game_t(_) ),
    assert( game_t(0) ),

    retractall( score(_) ),
    assert( score(0) ),

    retractall( visited(_) ),
    assert( visited(1) ),

    retractall( wumpus(_,_) ),
    retractall( gold(_,_) ),

    retractall( visited_cells(_) ),
    assert( visited_cells([]) ).

init_land :-
    retractall( size(_) ),
    assert( size(4) ),

    retractall( goldloc(_) ),
    assert( goldloc([3,2]) ),

    retractall( pitloc(_) ),
    assert( pitloc([4,4]) ),
    assert( pitloc([3,3]) ),
    assert( pitloc([1,3]) ).

init_agent :-
    retractall( agentloc(_) ),
    assert( agentloc([1,1]) ),

    visit([1,1]).

init_wumpus :-
    retractall( wumpusloc(_) ),
    assert( wumpusloc([4,1]) ).

visit(Xs) :-
    visited_cells(Ys),
    retractall( visited_cells(_) ),
    assert( visited_cells([Ys|Xs]) ).

% The knowledge base

updateKB( [Stench,Breeze,Glitter] ) :-
    addWumpusToKB(Stench),
    addPitToKB(Breeze),
    addGoldToKB(Glitter).

addWumpusToKB(no) :-

    agentloc([X,Y]),
    size(_),


    Z1 is Y+1, assume_wumpus(no,[X,Z1]),
    Z2 is Y-1, assume_wumpus(no,[X,Z2]),
    Z3 is X+1, assume_wumpus(no,[Z3,Y]),
    Z4 is X-1, assume_wumpus(no,[Z4,Y]).

addPitToKB(no) :-
    agentloc([X,Y]),
    Z1 is Y+1, assume_pit(no,[X,Z1]),
    Z2 is Y-1, assume_pit(no,[X,Z2]),
    Z3 is X+1, assume_pit(no,[Z3,Y]),
    Z4 is X-1, assume_pit(no,[Z4,Y]).

addPitToKB(yes) :-
    agentloc([X,Y]),
    Z1 is Y+1, assume_pit(yes,[X,Z1]),
    Z2 is Y-1, assume_pit(yes,[X,Z2]),
    Z3 is X+1, assume_pit(yes,[Z3,Y]),
    Z4 is X-1, assume_pit(yes,[Z4,Y]).

addGoldToKB(no) :-
    goldloc(GL),
    assume_gold(no, GL).

addGoldToKB(yes) :-
    goldloc([X1,Y1]),
    agentloc([X2,Y2]),
    X1 = X2, Y1 = Y2,
    assume_gold(yes, [X1,Y1]).

assume_wumpus(no, L) :-
    retractall( isWumpus(_, L) ),
    assert( isWumpus(no, L) ),
    format('The agent knows ~p - no Wumpus there!~n', [L]).

assume_wumpus(yes, L) :-

    retractall( isWumpus(_, L) ),
    assert( isWumpus(yes, L) ),
    format('The agents knows ~p - the Wumpus is in the adjacent room!~n', [L]).

assume_pit(no, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(no, L) ),
    format('The agent knows ~p - there\'s no Pit there!~n', [L]).

assume_pit(yes, L) :-
    retractall( isPit(_, L) ),
    assert( isPit(yes, L) ),
    format('The agent knows ~p - there is a Pit!~n', [L]).

assume_gold(no, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(no, L) ),
    format('The agent knows ~p - there\'s no gold here!~n', [L]).

assume_gold(yes, L) :-
    retractall( isGold(_, L) ),
    assert( isGold(yes, L) ),
    format('The agent knows that ~p - he found the gold!!!~n', [L]).

permitted([X,Y]) :-
    size(WS),
    0 < X, X < WS+1,
    0 < Y, Y < WS+1.

ask_KB(VisitedList, Action) :-
    isWumpus(no, L),
    isPit(no, L),
    permitted(L),
    not_member(L, VisitedList),
    update_agentloc(L),
    Action = L.

not_member(_, []).
not_member([X,Y], [[U,V]|Ys]) :-
    ( X=U,Y=V -> fail
    ; not_member([X,Y], Ys)
    ).

% Safe rooms

safe(1,2).
safe(2,1).
safe(2,3).
safe(3,2).
safe(3,4).
safe(4,3).

% Adjacency predicate

adjacentTo( [X1, Y1], [X2, Y2] ) :-
    ( X1 = X2, safe( Y1, Y2 )
    ; Y1 = Y2, safe( X1, X2 )
    ).


isSmelly(Ls1) :-
    wumpusloc( Ls2 ),
    adjacentTo( Ls1, Ls2 ).

isBleezy(Ls1) :-
    pitloc( Ls2 ),
    adjacentTo( Ls1, Ls2 ).

isGlittering( [X1, Y1] ) :-
    goldloc( [X2, Y2] ),
    X1 = X2,
    Y1 = Y2.

bleezy(yes) :-
    agentloc(AL),
    isBleezy(AL).
bleezy(no).

smelly(yes) :-
    agentloc(AL),
    isSmelly(AL).
smelly(no).

glittering(yes) :-
    agentloc(AL),
    isGlittering(AL).
glittering(no).
