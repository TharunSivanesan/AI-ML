:- dynamic vacuum_in/1.
:- dynamic dirty/1.
:- dynamic clean/1.

dirty(room1).
dirty(room2).

vacuum_in(room1).

makedirtyroom(Room) :-
    retract(clean(Room)),
    assert(dirty(Room)),
    format('~w is dirty again.',[Room]).

suck(Room) :-
    dirty(Room),
    retract(dirty(Room)),
    assert(clean(room)),
    format('Sucked dirt in ~w.~n',[Room]).

left :-
    (
        vacuum_in(room1) ->format('Already in room1.~n');
        retractall(vacuum_in(room2)),
        assert(vacuum_in(room1)),
        format('Moved left to room1.~n')
    ).

right :-
    (
        vacuum_in(room2)->
        format('Already in room2.~n')
        ;
        retractall(vacuum_in(room1)),
        assert(vacuum_in(room2)),
        format('Moved right to room2.~n')
    ).

clean_room(Room):-
    (
        dirty(Room)->
        suck(Room)
        ;
        format('No dirt in ~w.~n',[Room])
    ).

run_vacuum :-
    (vacuum_in(room1)->(
        clean_room(room1),
        right,
        clean_room(room2),
        left
    );
    (
        clean_room(room2),
        left,
        clean_room(room1)
    )
    ),
    format('Vacuum Cleaner off.~n').