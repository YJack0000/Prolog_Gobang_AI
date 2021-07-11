:- initialization main, halt.
:- dynamic black/2.
:- dynamic white/2.
:- dynamic step/3.

read_lines([H|T]):-
    read_line_to_codes(user_input, H), H \= end_of_file,
    string_codes(Input, H), term_string(Term, Input), assert(Term),
    read_lines(T).
read_lines([]).

% Count number of white
count_white(X) :-
    findall(N, white(N, _), Ns),
    length(Ns, X).

% Count number of black
count_black(X) :-
    findall(N, black(N, _), Ns),
    length(Ns, X).

is_empty(Col, Row):- 
    \+black(Col, Row), \+white(Col, Row).
    
bool_to_num(Fact, Num):-
    Fact -> Num = 1 ; Num = 0. 

search_line(Col, Row, _, _, "black", Num):- 
    \+black(Col, Row) -> Num is 0.
search_line(Col, Row, _, _, "white", Num):- 
    \+white(Col, Row) -> Num is 0.

search_line(Col, Row, DirC, DirR, "black", Num):-
    black(Col, Row) -> NextC is Col+DirC, NextR is Row+DirR, search_line(NextC, NextR, DirC, DirR, "black", Num1), Num is Num1+1.
search_line(Col, Row, DirC, DirR, "white", Num):-
    white(Col, Row) -> NextC is Col+DirC, NextR is Row+DirR, search_line(NextC, NextR, DirC, DirR, "white", Num1), Num is Num1+1.

near_two(Col, Row, DirCol, DirRow, "black"):-
    Col1 is Col + 1*DirCol,
    Row1 is Row + 1*DirRow,
    black(Col1, Row1).

near_two(Col, Row, DirCol, DirRow, "white"):-
    Col1 is Col + 1*DirCol,
    Row1 is Row + 1*DirRow,
    white(Col1, Row1).

jump_two(Col, Row, DirCol, DirRow, "black"):-
    Col2 is Col + 2*DirCol,
    Row2 is Row + 2*DirRow,
    black(Col2, Row2).

jump_two(Col, Row, DirCol, DirRow, "white"):-
    Col2 is Col + 2*DirCol,
    Row2 is Row + 2*DirRow,
    white(Col2, Row2).

two_count(Col, Row, Color, Count):-
    (near_two(Col, Row, -1, -1, Color);
    near_two(Col, Row, -1,  0, Color);
    near_two(Col, Row, -1,  1, Color);
    near_two(Col, Row,  0, -1, Color);
    near_two(Col, Row,  0,  1, Color);
    near_two(Col, Row,  1, -1, Color);
    near_two(Col, Row,  1,  0, Color);
    near_two(Col, Row,  1,  1, Color))
    -> Count is 2 ;
    (jump_two(Col, Row, -1, -1, Color);
    jump_two(Col, Row, -1,  0, Color);
    jump_two(Col, Row, -1,  1, Color);
    jump_two(Col, Row,  0, -1, Color);
    jump_two(Col, Row,  0,  1, Color);
    jump_two(Col, Row,  1, -1, Color);
    jump_two(Col, Row,  1,  0, Color);
    jump_two(Col, Row,  1,  1, Color))
    -> Count is 1 ;
    Count is 0.

three(Col, Row, DirCol, DirRow, "black"):-
    (   % WBXbbX
        ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, white(ColX1, RowX1),
        ColX2 is Col + 1*DirCol, RowX2 is Row + 1*DirRow, is_empty(ColX2, RowX2),
        ColX3 is Col + 4*DirCol, RowX3 is Row + 4*DirRow, is_empty(ColX3, RowX3),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, black(Col2, Row2), 
        Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, black(Col3, Row3)
    );( % WBbbX
        ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, white(ColX1, RowX1),
        ColX2 is Col + 3*DirCol, RowX2 is Row + 3*DirRow, is_empty(ColX2, RowX2),
        Col2 is Col + 1*DirCol, Row2 is Row + 1*DirRow, black(Col2, Row2), 
        Col3 is Col + 2*DirCol, Row3 is Row + 2*DirRow, black(Col3, Row3)
    );( % WBbXbX
        ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, white(ColX1, RowX1),
        ColX2 is Col + 2*DirCol, RowX2 is Row + 2*DirRow, is_empty(ColX2, RowX2),
        ColX3 is Col + 4*DirCol, RowX3 is Row + 4*DirRow, is_empty(ColX3, RowX3),
        Col2 is Col + 1*DirCol, Row2 is Row + 1*DirRow, black(Col2, Row2), 
        Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, black(Col3, Row3)
    );( % WbBXbX
        ColX1 is Col - 2*DirCol, RowX1 is Row - 2*DirRow, white(ColX1, RowX1),
        ColX2 is Col + 1*DirCol, RowX2 is Row + 1*DirRow, is_empty(ColX2, RowX2),
        ColX3 is Col + 3*DirCol, RowX3 is Row + 3*DirRow, is_empty(ColX3, RowX3),
        Col2 is Col - 1*DirCol, Row2 is Row - 1*DirRow, black(Col2, Row2), 
        Col3 is Col + 2*DirCol, Row3 is Row + 2*DirRow, black(Col3, Row3)
    ).

three(Col, Row, DirCol, DirRow, "white"):-
    (   % WBXbbX
        ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, black(ColX1, RowX1),
        ColX2 is Col + 1*DirCol, RowX2 is Row + 1*DirRow, is_empty(ColX2, RowX2),
        ColX3 is Col + 4*DirCol, RowX3 is Row + 4*DirRow, is_empty(ColX3, RowX3),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, white(Col2, Row2), 
        Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, white(Col3, Row3)
    );( % WBbbX
        ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, black(ColX1, RowX1),
        ColX2 is Col + 3*DirCol, RowX2 is Row + 3*DirRow, is_empty(ColX2, RowX2),
        Col2 is Col + 1*DirCol, Row2 is Row + 1*DirRow, white(Col2, Row2), 
        Col3 is Col + 2*DirCol, Row3 is Row + 2*DirRow, white(Col3, Row3)
    );( % WBbXbX
        ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, black(ColX1, RowX1),
        ColX2 is Col + 2*DirCol, RowX2 is Row + 2*DirRow, is_empty(ColX2, RowX2),
        ColX3 is Col + 4*DirCol, RowX3 is Row + 4*DirRow, is_empty(ColX3, RowX3),
        Col2 is Col + 1*DirCol, Row2 is Row + 1*DirRow, white(Col2, Row2), 
        Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, white(Col3, Row3)
    );( % WbBXbX
        ColX1 is Col - 2*DirCol, RowX1 is Row - 2*DirRow, black(ColX1, RowX1),
        ColX2 is Col + 1*DirCol, RowX2 is Row + 1*DirRow, is_empty(ColX2, RowX2),
        ColX3 is Col + 3*DirCol, RowX3 is Row + 3*DirRow, is_empty(ColX3, RowX3),
        Col2 is Col - 1*DirCol, Row2 is Row - 1*DirRow, white(Col2, Row2), 
        Col3 is Col + 2*DirCol, Row3 is Row + 2*DirRow, white(Col3, Row3)
    ).

three_count(Col, Row, Color, Count):-
    (three(Col, Row, -1, -1, Color);
    three(Col, Row, -1,  0, Color);
    three(Col, Row, -1,  1, Color);
    three(Col, Row,  0, -1, Color);
    three(Col, Row,  0,  1, Color);
    three(Col, Row,  1, -1, Color);
    three(Col, Row,  1,  0, Color);
    three(Col, Row,  1,  1, Color))
    -> Count is 1 ; Count is 0.

live_three(Col, Row, DirCol, DirRow, "black"):-
    (
        ( % XBXbbX
            ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, is_empty(ColX1, RowX1),
            ColX2 is Col + 1*DirCol, RowX2 is Row + 1*DirRow, is_empty(ColX2, RowX2),
            ColX3 is Col + 4*DirCol, RowX3 is Row + 4*DirRow, is_empty(ColX3, RowX3),
            Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, black(Col2, Row2), 
            Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, black(Col3, Row3)
        );( % XBbbX
            ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, is_empty(ColX1, RowX1),
            ColX2 is Col + 3*DirCol, RowX2 is Row + 3*DirRow, is_empty(ColX2, RowX2),
            Col2 is Col + 1*DirCol, Row2 is Row + 1*DirRow, black(Col2, Row2), 
            Col3 is Col + 2*DirCol, Row3 is Row + 2*DirRow, black(Col3, Row3)
        );( % XBbXbX
            ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, is_empty(ColX1, RowX1),
            ColX2 is Col + 2*DirCol, RowX2 is Row + 2*DirRow, is_empty(ColX2, RowX2),
            ColX3 is Col + 4*DirCol, RowX3 is Row + 4*DirRow, is_empty(ColX3, RowX3),
            Col2 is Col + 1*DirCol, Row2 is Row + 1*DirRow, black(Col2, Row2), 
            Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, black(Col3, Row3)
        );( % XbBXbX
            ColX1 is Col - 2*DirCol, RowX1 is Row - 2*DirRow, is_empty(ColX1, RowX1),
            ColX2 is Col + 1*DirCol, RowX2 is Row + 1*DirRow, is_empty(ColX2, RowX2),
            ColX3 is Col + 3*DirCol, RowX3 is Row + 3*DirRow, is_empty(ColX3, RowX3),
            Col2 is Col - 1*DirCol, Row2 is Row - 1*DirRow, black(Col2, Row2), 
            Col3 is Col + 2*DirCol, Row3 is Row + 2*DirRow, black(Col3, Row3)
        );( % XbBbX
            ColX1 is Col - 2*DirCol, RowX1 is Row - 2*DirRow, is_empty(ColX1, RowX1),
            ColX2 is Col + 2*DirCol, RowX2 is Row + 2*DirRow, is_empty(ColX2, RowX2),
            Col2 is Col - 1*DirCol, Row2 is Row - 1*DirRow, black(Col2, Row2), 
            Col3 is Col + 1*DirCol, Row3 is Row + 1*DirRow, black(Col3, Row3)
        );false
    ).

live_three(Col, Row, DirCol, DirRow, "white"):-
    (
        ( % XBXbbX
            ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, is_empty(ColX1, RowX1),
            ColX2 is Col + 1*DirCol, RowX2 is Row + 1*DirRow, is_empty(ColX2, RowX2),
            ColX3 is Col + 4*DirCol, RowX3 is Row + 4*DirRow, is_empty(ColX3, RowX3),
            Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, white(Col2, Row2), 
            Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, white(Col3, Row3)
        );( % XBbbX
            ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, is_empty(ColX1, RowX1),
            ColX2 is Col + 3*DirCol, RowX2 is Row + 3*DirRow, is_empty(ColX2, RowX2),
            Col2 is Col + 1*DirCol, Row2 is Row + 1*DirRow, white(Col2, Row2), 
            Col3 is Col + 2*DirCol, Row3 is Row + 2*DirRow, white(Col3, Row3)
        );( % XBbXbX
            ColX1 is Col - 1*DirCol, RowX1 is Row - 1*DirRow, is_empty(ColX1, RowX1),
            ColX2 is Col + 2*DirCol, RowX2 is Row + 2*DirRow, is_empty(ColX2, RowX2),
            ColX3 is Col + 4*DirCol, RowX3 is Row + 4*DirRow, is_empty(ColX3, RowX3),
            Col2 is Col + 1*DirCol, Row2 is Row + 1*DirRow, white(Col2, Row2), 
            Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, white(Col3, Row3)
        );( % XbBXbX
            ColX1 is Col - 2*DirCol, RowX1 is Row - 2*DirRow, is_empty(ColX1, RowX1),
            ColX2 is Col + 1*DirCol, RowX2 is Row + 1*DirRow, is_empty(ColX2, RowX2),
            ColX3 is Col + 3*DirCol, RowX3 is Row + 3*DirRow, is_empty(ColX3, RowX3),
            Col2 is Col - 1*DirCol, Row2 is Row - 1*DirRow, white(Col2, Row2), 
            Col3 is Col + 2*DirCol, Row3 is Row + 2*DirRow, white(Col3, Row3)
        );( % XbBbX
            ColX1 is Col - 2*DirCol, RowX1 is Row - 2*DirRow, is_empty(ColX1, RowX1),
            ColX2 is Col + 2*DirCol, RowX2 is Row + 2*DirRow, is_empty(ColX2, RowX2),
            Col2 is Col - 1*DirCol, Row2 is Row - 1*DirRow, white(Col2, Row2), 
            Col3 is Col + 1*DirCol, Row3 is Row + 1*DirRow, white(Col3, Row3)
        );false
    ).
    
live_three_count(Col, Row, Color, Count):-
    (( live_three(Col, Row, -1, -1, Color), bool_to_num(live_three(Col, Row, -1, -1, Color), Num1) ); % /
    ( bool_to_num(live_three(Col, Row,  1,  1, Color), Num1) )),
    (( live_three(Col, Row,  1,  0, Color), bool_to_num(live_three(Col, Row,  1,  0, Color), Num2) ); % -
    ( bool_to_num(live_three(Col, Row, -1,  0, Color), Num2) )),
    (( live_three(Col, Row,  1, -1, Color), bool_to_num(live_three(Col, Row,  1, -1, Color), Num3) ); % \
    ( bool_to_num(live_three(Col, Row, -1,  1, Color), Num3) )),
    (( live_three(Col, Row,  0, -1, Color), bool_to_num(live_three(Col, Row,  0, -1, Color), Num4) ); % I
    ( bool_to_num(live_three(Col, Row,  0,  1, Color), Num4) )),
    Count is Num1 + Num2 + Num3 + Num4.

four(Col, Row, DirCol, DirRow, "black"):-
    ( % BbbbX
        Col4 is Col + 4*DirCol, Row4 is Row + 4*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, black(Col1, Row1),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, black(Col2, Row2),
        Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, black(Col3, Row3)
    );( % XBbbb
        Col4 is Col - 1*DirCol, Row4 is Row - 1*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, black(Col1, Row1),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, black(Col2, Row2),
        Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, black(Col3, Row3)
    );( % BXbbb
        Col4 is Col + 1*DirCol, Row4 is Row + 1*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 2*DirCol, Row1 is Row + 2*DirRow, black(Col1, Row1),
        Col2 is Col + 3*DirCol, Row2 is Row + 3*DirRow, black(Col2, Row2),
        Col3 is Col + 4*DirCol, Row3 is Row + 4*DirRow, black(Col3, Row3)
    );( % BbXbb
        Col4 is Col + 2*DirCol, Row4 is Row + 2*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, black(Col1, Row1),
        Col2 is Col + 3*DirCol, Row2 is Row + 3*DirRow, black(Col2, Row2),
        Col3 is Col + 4*DirCol, Row3 is Row + 4*DirRow, black(Col3, Row3)
    );( % BbbXb
        Col4 is Col + 3*DirCol, Row4 is Row + 3*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, black(Col1, Row1),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, black(Col2, Row2),
        Col3 is Col + 4*DirCol, Row3 is Row + 4*DirRow, black(Col3, Row3)
    );( % XbBbb
        Col4 is Col - 2*DirCol, Row4 is Row - 2*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, black(Col1, Row1),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, black(Col2, Row2),
        Col3 is Col - 1*DirCol, Row3 is Row - 1*DirRow, black(Col3, Row3)
    );( % bBbbX
        Col4 is Col + 3*DirCol, Row4 is Row + 3*DirRow, is_empty(Col4, Row4),
        Col3 is Col - 1*DirCol, Row3 is Row - 1*DirRow, black(Col3, Row3),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, black(Col1, Row1),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, black(Col2, Row2)
    );( % bBXbb
        Col4 is Col + 1*DirCol, Row4 is Row + 1*DirRow, is_empty(Col4, Row4),
        Col3 is Col - 1*DirCol, Row3 is Row - 1*DirRow, black(Col3, Row3),
        Col1 is Col + 2*DirCol, Row1 is Row + 2*DirRow, black(Col1, Row1),
        Col2 is Col + 3*DirCol, Row2 is Row + 3*DirRow, black(Col2, Row2)
    );( % bBbXb
        Col4 is Col + 2*DirCol, Row4 is Row + 2*DirRow, is_empty(Col4, Row4),
        Col3 is Col - 1*DirCol, Row3 is Row - 1*DirRow, black(Col3, Row3),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, black(Col1, Row1),
        Col2 is Col + 3*DirCol, Row2 is Row + 3*DirRow, black(Col2, Row2)
    );( % bbBXb
        Col4 is Col + 1*DirCol, Row4 is Row + 1*DirRow, is_empty(Col4, Row4),
        Col3 is Col - 1*DirCol, Row3 is Row - 1*DirRow, black(Col3, Row3), 
        Col1 is Col - 2*DirCol, Row1 is Row - 2*DirRow, black(Col1, Row1), 
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, black(Col2, Row2)
    ).
  
four(Col, Row, DirCol, DirRow, "white"):- 
    ( % BbbbX
        Col4 is Col + 4*DirCol, Row4 is Row + 4*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, white(Col1, Row1),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, white(Col2, Row2),
        Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, white(Col3, Row3)
    );( % XBbbb
        Col4 is Col - 1*DirCol, Row4 is Row - 1*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, white(Col1, Row1),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, white(Col2, Row2),
        Col3 is Col + 3*DirCol, Row3 is Row + 3*DirRow, white(Col3, Row3)
    );( % BXbbb
        Col4 is Col + 1*DirCol, Row4 is Row + 1*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 2*DirCol, Row1 is Row + 2*DirRow, white(Col1, Row1),
        Col2 is Col + 3*DirCol, Row2 is Row + 3*DirRow, white(Col2, Row2),
        Col3 is Col + 4*DirCol, Row3 is Row + 4*DirRow, white(Col3, Row3)
    );( % BbXbb
        Col4 is Col + 2*DirCol, Row4 is Row + 2*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, white(Col1, Row1),
        Col2 is Col + 3*DirCol, Row2 is Row + 3*DirRow, white(Col2, Row2),
        Col3 is Col + 4*DirCol, Row3 is Row + 4*DirRow, white(Col3, Row3)
    );( % BbbXb
        Col4 is Col + 3*DirCol, Row4 is Row + 3*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, white(Col1, Row1),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, white(Col2, Row2),
        Col3 is Col + 4*DirCol, Row3 is Row + 4*DirRow, white(Col3, Row3)
    );( % XbBbb
        Col4 is Col - 2*DirCol, Row4 is Row - 2*DirRow, is_empty(Col4, Row4),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, white(Col1, Row1),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, white(Col2, Row2),
        Col3 is Col - 1*DirCol, Row3 is Row - 1*DirRow, white(Col3, Row3)
    );( % bBbbX
        Col4 is Col + 3*DirCol, Row4 is Row + 3*DirRow, is_empty(Col4, Row4),
        Col3 is Col - 1*DirCol, Row3 is Row - 1*DirRow, white(Col3, Row3),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, white(Col1, Row1),
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, white(Col2, Row2)
    );( % bBXbb
        Col4 is Col + 1*DirCol, Row4 is Row + 1*DirRow, is_empty(Col4, Row4),
        Col3 is Col - 1*DirCol, Row3 is Row - 1*DirRow, white(Col3, Row3),
        Col1 is Col + 2*DirCol, Row1 is Row + 2*DirRow, white(Col1, Row1),
        Col2 is Col + 3*DirCol, Row2 is Row + 3*DirRow, white(Col2, Row2)
    );( % bBbXb
        Col4 is Col + 2*DirCol, Row4 is Row + 2*DirRow, is_empty(Col4, Row4),
        Col3 is Col - 1*DirCol, Row3 is Row - 1*DirRow, white(Col3, Row3),
        Col1 is Col + 1*DirCol, Row1 is Row + 1*DirRow, white(Col1, Row1),
        Col2 is Col + 3*DirCol, Row2 is Row + 3*DirRow, white(Col2, Row2)
    );( % bbBXb
        Col4 is Col + 1*DirCol, Row4 is Row + 1*DirRow, is_empty(Col4, Row4),
        Col3 is Col - 1*DirCol, Row3 is Row - 1*DirRow, white(Col3, Row3), 
        Col1 is Col - 2*DirCol, Row1 is Row - 2*DirRow, white(Col1, Row1), 
        Col2 is Col + 2*DirCol, Row2 is Row + 2*DirRow, white(Col2, Row2)
    ).

four_count(Col, Row, Color, Count):-
    (four(Col, Row, -1, -1, Color) -> (Cnt1 == 1 -> Cnt2 is 1 ; Cnt1 is 1) ; true),
    (four(Col, Row, -1,  0, Color) -> (Cnt1 == 1 -> Cnt2 is 1 ; Cnt1 is 1) ; true),
    (four(Col, Row, -1,  1, Color) -> (Cnt1 == 1 -> Cnt2 is 1 ; Cnt1 is 1) ; true),
    (four(Col, Row,  0, -1, Color) -> (Cnt1 == 1 -> Cnt2 is 1 ; Cnt1 is 1) ; true),
    (four(Col, Row,  0,  1, Color) -> (Cnt1 == 1 -> Cnt2 is 1 ; Cnt1 is 1) ; true),
    (four(Col, Row,  1, -1, Color) -> (Cnt1 == 1 -> Cnt2 is 1 ; Cnt1 is 1) ; true),
    (four(Col, Row,  1,  0, Color) -> (Cnt1 == 1 -> Cnt2 is 1 ; Cnt1 is 1) ; true),
    (four(Col, Row,  1,  1, Color) -> (Cnt1 == 1 -> Cnt2 is 1 ; Cnt1 is 1) ; true),
    ( Cnt2 == 1 -> Count is 2 % 至少雙四 
    ; Cnt1 == 1 -> Count is 1 % 四
    ; Count is 0 ).

live_four(Col, Row, DirCol, DirRow, "black"):-
    (   % XBbbbX
        Col1 is Col - 2*DirCol, Row1 is Row - 2*DirRow, is_empty(Col1, Row1),
        Col6 is Col + 4*DirCol, Row6 is Row + 4*DirRow, is_empty(Col6, Row6),
        Col3 is Col + 1*DirCol, Row3 is Row + 1*DirRow, black(Col3, Row3),
        Col4 is Col + 2*DirCol, Row4 is Row + 2*DirRow, black(Col4, Row4),
        Col5 is Col + 3*DirCol, Row5 is Row + 3*DirRow, black(Col5, Row5)
    );( % XbBbbX
        Col1 is Col - 2*DirCol, Row1 is Row - 2*DirRow, is_empty(Col1, Row1),
        Col6 is Col + 3*DirCol, Row6 is Row + 3*DirRow, is_empty(Col6, Row6),
        Col2 is Col - 1*DirCol, Row2 is Row - 1*DirRow, black(Col2, Row2),
        Col4 is Col + 1*DirCol, Row4 is Row + 1*DirRow, black(Col4, Row4),
        Col5 is Col + 2*DirCol, Row5 is Row + 2*DirRow, black(Col5, Row5)
    ).

live_four(Col, Row, DirCol, DirRow, "white"):-
    (   % XBbbbX
        Col1 is Col - 2*DirCol, Row1 is Row - 2*DirRow, is_empty(Col1, Row1),
        Col6 is Col + 4*DirCol, Row6 is Row + 4*DirRow, is_empty(Col6, Row6),
        Col3 is Col + 1*DirCol, Row3 is Row + 1*DirRow, white(Col3, Row3),
        Col4 is Col + 2*DirCol, Row4 is Row + 2*DirRow, white(Col4, Row4),
        Col5 is Col + 3*DirCol, Row5 is Row + 3*DirRow, white(Col5, Row5)
    );( % XbBbbX
        Col1 is Col - 2*DirCol, Row1 is Row - 2*DirRow, is_empty(Col1, Row1),
        Col6 is Col + 3*DirCol, Row6 is Row + 3*DirRow, is_empty(Col6, Row6),
        Col2 is Col - 1*DirCol, Row2 is Row - 1*DirRow, white(Col2, Row2),
        Col4 is Col + 1*DirCol, Row4 is Row + 1*DirRow, white(Col4, Row4),
        Col5 is Col + 2*DirCol, Row5 is Row + 2*DirRow, white(Col5, Row5)
    ).

live_four_count(Col ,Row, Color, Count):-
    (live_four(Col, Row, -1, -1, Color);
     live_four(Col, Row, -1,  0, Color);
     live_four(Col, Row, -1,  1, Color);
     live_four(Col, Row,  0, -1, Color);
     live_four(Col, Row,  0,  1, Color);
     live_four(Col, Row,  1, -1, Color);
     live_four(Col, Row,  1,  0, Color);
     live_four(Col, Row,  1,  1, Color))
    -> Count is 1 ; Count is 0.

assert_C("black", Col, Row):- assert(black(Col, Row)).
assert_C("white", Col, Row):- assert(white(Col, Row)).

retract_C("black", Col, Row):- retractall(black(Col, Row)).
retract_C("white", Col, Row):- retractall(white(Col, Row)).

five(Col, Row, Color, Count):-
    assert_C(Color, Col, Row),
    (   
    	((search_line(Col, Row, 1, 0, Color, Num1), search_line(Col, Row, -1, 0, Color, Num2), Num1 + Num2 - 2 >= 4);
        (search_line(Col, Row, 1, 1, Color, Num3), search_line(Col, Row, -1, -1, Color, Num4), Num3 + Num4 - 2 >= 4);
        (search_line(Col, Row, 0, 1, Color, Num5), search_line(Col, Row, 0, -1, Color, Num6),  Num5 + Num6 - 2 >= 4);
        (search_line(Col, Row, -1, 1, Color, Num7), search_line(Col, Row, 1, -1, Color, Num8), Num7 + Num8 - 2 >= 4))
        -> Count is 1 ; Count is 0
    )
    ,retract_C(Color, Col, Row).

too_long(Col, Row, "black"):-
    assert(black(Col, Row)),
    (search_line(Col, Row, 1, 0, "black", Num1), search_line(Col, Row, -1, 0, "black", Num2), Num1 + Num2 - 2 >= 5);
    (search_line(Col, Row, 1, 1, "black", Num1), search_line(Col, Row, -1, -1, "black", Num2), Num1 + Num2 - 2 >= 5);
    (search_line(Col, Row, 0, 1, "black", Num1), search_line(Col, Row, 0, -1, "black", Num2), Num1 + Num2 - 2 >= 5);
    (search_line(Col, Row, -1, 1, "black", Num1), search_line(Col, Row, 1, -1, "black", Num2), Num1 + Num2 - 2 >= 5)
    ,retractall(black(Col, Row)).

black_is_banned(Col, Row):-
    too_long(Col, Row, "black"); 
    four_count(Col, Row, "black", Count), Count >=2; 
    live_three_count(Col, Row, "black", Count), Count>=2.

dfs(225, 15, 15).

dfs(N, MX, MY):-
    Next is N + 1, 
    
    dfs(Next, PreMX, PreMY),
    %format("~w ~w ~w\n", [Next, PreMX, PreMY]),

    X is N div 15 + 1, Y is N mod 15 + 1,
    step(X, Y, Score), step(PreMX, PreMY, PreMScore),
    %format("(~w, ~w): ~w / (~w, ~w): ~w\n", [X, Y, Score, PreMX, PreMY, PreMScore]),

    (
        (PreMScore < Score) 
            -> MX is X, MY is Y
            ; MX is PreMX, MY is PreMY
    ).

find_max_step(MX, MY, MScore):-
    dfs(1, MX, MY), step(MX, MY, MScore).

simulate(Color, RX, RY):-
    (Color == 0
    ->  (
            %format('finding black step...\n'),
            forall((member(Col, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]), member(Row, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])),
                (
                    \+is_empty(Col, Row) -> assert(step(Col, Row, -1000000))
                    ;
                    black_is_banned(Col, Row) -> assert(step(Col, Row, -1000000)) % black not allow
                    ;

                    two_count(Col, Row, "white", WCount1), 
                    three_count(Col, Row, "white", WCount2),
                    live_three_count(Col, Row, "white", WCount3), 
                    four_count(Col, Row, "white", WCount4), 
                    live_four_count(Col, Row, "white", WCount5),
                    five(Col, Row, "white", WCount6), 

                    two_count(Col, Row, "black", BCount1), 
                    three_count(Col, Row, "black", BCount2),
                    live_three_count(Col, Row, "black", BCount3),  
                    four_count(Col, Row, "black", BCount4), 
                    live_four_count(Col, Row, "black", BCount5),
                    five(Col, Row, "black", BCount6), 

                    %(
                    %    forall((member(Col1, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]), member(Row1, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])),
                    %        is_empty(Col1, Row1) -> retractall(black(Col1, Row1)), retractall(white(Col1, Row1))
                    %    )
                    %),
                    %format("~w ~w: ~w ~w ~w ~w ~w ~w\n", [Col, Row, BCount1, BCount2, BCount3, BCount4, BCount5, BCount6]),

                    Score is BCount1*4 + BCount2*40 + BCount3*400 + BCount4*750 + BCount5*4000 + BCount6*40000 + WCount1*2 + WCount2*20 + WCount3*200 + WCount4*200 + WCount5*2000 + WCount6*20000,
                    assert(step(Col, Row, Score)) -> format('~w ~w ~w\n', [Col, Row, Score])
                    ; true
                )
            )
    	); 
        (
            %format('finding white step...\n'),
            forall((member(Col, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]), member(Row, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])),
                (
                    \+is_empty(Col, Row) -> assert(step(Col, Row, -1000000))
                    ;
                    black_is_banned(Col, Row) -> assert(step(Col, Row, 0))
                    ;

                    two_count(Col, Row, "white", WCount1), 
                    three_count(Col, Row, "white", WCount2),
                    live_three_count(Col, Row, "white", WCount3), 
                    four_count(Col, Row, "white", WCount4), 
                    live_four_count(Col, Row, "white", WCount5),
                    five(Col, Row, "white", WCount6), 
                    %format("~w ~w ~w ~w ~w ~w\n", [WCount1, WCount2, WCount3, WCount4, WCount5, WCount6]),

                    two_count(Col, Row, "black", BCount1), 
                    three_count(Col, Row, "black", BCount2),
                    live_three_count(Col, Row, "black", BCount3),  
                    four_count(Col, Row, "black", BCount4), 
                    live_four_count(Col, Row, "black", BCount5),
                    five(Col, Row, "black", BCount6), 
                    %format("~w ~w ~w ~w ~w ~w\n", [BCount1, BCount2, BCount3, BCount4, BCount5, BCount6]),

                    Score is WCount1*4 + WCount2*40 + WCount3*400 + WCount4*750 + WCount5*4000 + WCount6*40000 + BCount1*2 + BCount2*20 + BCount3*200 + BCount4*200 + BCount5*2000 + BCount6*20000,
                    assert(step(Col, Row, Score)) -> format('~w ~w ~w\n', [Col, Row, Score])
                    ; true
                )
            )
        )),
    find_max_step(MX, MY, MScore), RX is MX, RY is MY.
    %format('~w ~w: ~w\n', [MX, MY, MScore]).
    %dfs(225, MX, MY, MScore), format("~w ~w ~w", [MX, MY, MScore]).

count_ki(Num):-
    findall(_, black(_, _), L1), length(L1, Black_count),
    findall(_, white(_, _), L2), length(L2, White_count),
    Num is Black_count + White_count.

the_black_magic:-
    /*count_ki(N7), N7 == 10 -> 
        format("black(6, 7)\n")
    ; count_ki(N8), N8 == 9, \+is_empty(8, 8) -> 
        format("white(6, 9)\n")
    ; count_ki(N9), N9 == 21 -> 
        format("white(8, 6)\n")
    ; count_ki(N10),N10 == 11 -> 
        format("white(12, 12)\n")
    ; count_ki(N19), N19 == 22 -> 
        format("black(3, 8)\n")
    ; count_ki(N20), N20 == 12, \+is_empty(11, 11) -> 
        format("black(12, 12)\n")
    ; */process.

process:-
    count_black(Nblack),
    Nblack == 0 -> format("black(8, 8)\n");

    count_white(Nwhite),
    Nwhite == 0 -> format("white(8, 9)\n");

    count_black(Nblack1),
    count_white(Nwhite1),

    Next_Player is ((Nblack1+Nwhite1) mod 2),

    simulate(Next_Player, RX, RY),
    (
        Next_Player =:= 0
        -> format("black(~w, ~w)\n", [RX, RY])
         ; format("white(~w, ~w)\n", [RX, RY])
    ).

main:-
    read_lines(_),
    the_black_magic,
    halt.
