%% opn( go(Agent, From, To),
%%      [at(Agent, From)],
%%      [at(Agent, From)],
%%      [at(Agent, To)]).

opn( push(B, X, Y),
     [at(B, X),
      on(B, floor),
      at(monkey, X),
      on(monkey, floor)],
     [at(monkey, X),
      at(B, X)],
     [at(monkey, Y),
      at(B, Y)]).

%% opn( climb_on(B),
%%      [at(monkey, X),
%%       at(B, X),
%%       on(monkey, floor),
%%       on(B, floor)],
%%      [on(monkey, floor)],
%%      [on(monkey, B)]).

%% opn( grab(B),
%%      [on(monkey, box),
%%       at(box, X),
%%       at(B, X),
%%       status(B, hanging)],
%%      [status(B, hanging)],
%%      [status(B, grabbed)]).

notmember(X, S):-
    not(memberchk(X, S)).

precons_present(P, S):-
    subset(P, S).

solve(State, Goal, Plan, Plan):-
    subset(Goal, State).

solve(State, Goal, Sofar, Plan):-
    opn(Op, Precons, Delete, Add),
    notmember(Op, Sofar),
    precons_present(Precons, State),
    subtract(State, Delete, Remainder),
    append(Add, Remainder, NewState),
    solve(NewState, Goal, [Op|Sofar], Plan).

test(Goal, Plan):-
    solve([on(monkey, floor),
           on(box, floor),
           at(monkey, loc_a),
           at(box, loc_a)
           %at(bananas, loc_b),
           %status(bananas, hanging)
          ],
          Goal,
          [],
          Plan).

%?- test([status(bananas, grabbed)], Plan).
