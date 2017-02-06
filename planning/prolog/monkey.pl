opn( go(Agent, From, To),
     [at(Agent, From)],
     [at(Agent, From)],
     [at(Agent, To)]).

opn( push(B, X, Y),
     [at(monkey, X),
      at(B, X),
      on(monkey, floor),
      on(B, floor)],
     [at(monkey, X),
      at(B, X)],
     [at(monkey, Y),
      at(B, Y)]).

opn( climb_on(B),
     [at(monkey, X),
      at(B, X),
      on(monkey, floor),
      on(B, floor)],
     [on(monkey, floor)],
     [on(monkey, B)]).

opn( grab(B),
     [on(monkey, box),
      at(box, X),
      at(B, X),
      status(B, hanging)],
     [status(B, hanging)],
     [status(B, grabbed)]).

notmember(X, S):-
    not(member(X, S)).

%% list:subset does not backtrack
%% because it's based on http://www.swi-prolog.org/pldoc/man?predicate=memberchk/2
is_subset([], _) :- !.
is_subset([E|R], Set) :-
    member(E, Set),
    is_subset(R, Set).

precons_present(P, S):-
    is_subset(P, S).

solve(State, Goal, Plan, Plan):-
    subset(Goal, State).

solve(State, Goal, Sofar, Plan):-
    opn(Op, Precons, Delete, Add),
    %write(Op), nl,
    notmember(Op, Sofar),
    precons_present(Precons, State),
    is_set(Precons),
    subtract(State, Delete, Remainder),
    append(Add, Remainder, NewState),
    solve(NewState, Goal, [Op|Sofar], Plan).

test(Goal, Plan):-
    solve([at(monkey, loc_a),
           on(monkey, floor),

           at(box, loc_b),
           on(box, floor),

           at(bananas, loc_c),
           status(bananas, hanging)
          ],
          Goal,
          [],
          Plan).

% To run:
% > cd [THIS-DIR]
% > /usr/local/Cellar/swi-prolog/7.2.3_2/bin/swipl
% [monkey].
% test([status(bananas, grabbed)], Plan).
% Plan = [grab(bananas), climb_on(box), push(box, loc_b, loc_c), go(monkey, loc_a, loc_b)] .
