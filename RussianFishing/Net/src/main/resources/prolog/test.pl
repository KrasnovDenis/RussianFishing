my_char_type(46,period) :- !.
my_char_type(X,alphanumeric) :- X >= 65, X =< 90, !.
my_char_type(X,alphanumeric) :- X >= 97, X =< 123, !.
my_char_type(X,alphanumeric) :- X >= 48, X =< 57, !.
my_char_type(X,whitespace) :- X =< 32, !.
my_char_type(X,punctuation) :- X >= 33, X =< 47, !.
my_char_type(X,punctuation) :- X >= 58, X =< 64, !.
my_char_type(X,punctuation) :- X >= 91, X =< 96, !.
my_char_type(X,punctuation) :- X >= 123, X =< 126, !.
my_char_type(_,special).

lower_case(X,Y) :-
	X >= 65,
	X =< 90,
	Y is X + 32, !.

lower_case(X,X).

read_lc_string(InputParam,Answer) :-
	string_to_list(InputParam, Answer).

extract_word([C|Chars],Rest,[C|RestOfWord]) :-
	my_char_type(C,Type),
	extract_word_aux(Type,Chars,Rest,RestOfWord).

extract_word_aux(special,Rest,Rest,[]) :- !.

extract_word_aux(Type,[C|Chars],Rest,[C|RestOfWord]) :-
	my_char_type(C,Type), !,
	extract_word_aux(Type,Chars,Rest,RestOfWord).

extract_word_aux(_,Rest,Rest,[]).   % if previous clause did not succeed.


remove_initial_blanks([C|Chars],Result) :-
	my_char_type(C,whitespace), !,
	remove_initial_blanks(Chars,Result).

remove_initial_blanks(X,X).   % if previous clause did not succeed.

digit_value(48,0).
digit_value(49,1).
digit_value(50,2).
digit_value(51,3).
digit_value(52,4).
digit_value(53,5).
digit_value(54,6).
digit_value(55,7).
digit_value(56,8).
digit_value(57,9).

string_to_number(S,N) :-
	string_to_number_aux(S,0,N).

string_to_number_aux([D|Digits],ValueSoFar,Result) :-
	digit_value(D,V),
	NewValueSoFar is 10*ValueSoFar + V,
	string_to_number_aux(Digits,NewValueSoFar,Result).

string_to_number_aux([],Result,Result).

string_to_atomic([C|Chars],Number) :-
	string_to_number([C|Chars],Number), !.

string_to_atomic(String,Atom) :- name(Atom,String).

extract_atomics(String,ListOfAtomics) :-
	remove_initial_blanks(String,NewString),
	extract_atomics_aux(NewString,ListOfAtomics).

extract_atomics_aux([C|Chars],[A|Atomics]) :-
	extract_word([C|Chars],Rest,Word),
	string_to_atomic(Word,A),       % <- this is the only change
	extract_atomics(Rest,Atomics).

extract_atomics_aux([],[]).

clean_string([C|Chars],L) :-
	my_char_type(C,punctuation),
	clean_string(Chars,L), !.
clean_string([C|Chars],[C|L]) :-
	clean_string(Chars,L), !.
clean_string([C|[]],[]) :-
	my_char_type(C,punctuation), !.
clean_string([C|[]],[C]).


read_atomics(String,ListOfAtomics) :-
	read_lc_string(String,PreparedString),
	clean_string(PreparedString,Cleanstring),
	extract_atomics(Cleanstring,ListOfAtomics).

isalist([_|_]).

last_member(End,List) :- append(_,[End],List).

findnth([E|_],1,E).
findnth([_|T],N,T1) :- V is N - 1,
                       findnth(T,V,T1).


replace(_,[],_,[]).
replace(X,[H|T],A,[A|T2]) :- nonvar(H), H = X, !, replace(X,T,A,T2).
replace(X,[H|T],A,[H|T2]) :- replace(X,T,A,T2).


simplify(List,Result) :- sr(List,Result,X,Y), !,
			 simplify(X,Y).
simplify([W|Words],[W|NewWords]) :- simplify(Words,NewWords).
simplify([],[]).


match(A,C) :- match_aux1(A,C),!.
match(A,C) :- match_aux2(A,C).

match_aux1(A,C) :-
	member([*|T],A),
	nonvar(T),
	member(Tm,T),
	nonvar(Tm),
	replace([*|T],A,Tm,B),
	match_aux2(B,C),
	!, last_member(L,T), L = Tm.

match_aux2([],[]).
match_aux2([Item|Items],[Word|Words]) :-
	match_aux3(Item,Items,Word,Words),!.
match_aux2([Item1,Item2|Items],[Word|Words]) :-
	var(Item1),
	nonvar(Item2),
	Item2 == Word,!,
	match_aux2([Item1,Item2|Items],[[],Word|Words]).
match_aux2([Item1,Item2|Items],[Word|Words]) :-
	var(Item1),
	var(Item2),!,
	match_aux2([Item1,Item2|Items],[[],Word|Words]).
match_aux2([[]],[]).

match_aux3(Word,Items,Word,Words) :-
	match_aux2(Items,Words), !.
match_aux3([Word|Seg],Items,Word,Words0) :-
	append(Seg,Words1,Words0),
	match_aux2(Items,Words1).

makecomment([[your,2]|T],InputList,Comment) :-
	assertz(mem(InputList)),
	rules([[your,2],Reassembly]),
	mc_aux([[your,2]|T],Reassembly,InputList,Comment),!.
makecomment([[memory,0]|T],_,Comment) :-
	retract(mem(I2)),
	retractall(mem(I2)),
	rules([[memory,0],Reassembly]),
	mc_aux([[memory,0]|T],Reassembly,I2,Comment),!.
makecomment([[memory,0]|T],InputList,Comment) :-
	\+ retract(mem(_)),!,
	makecomment(T,InputList,Comment).
makecomment([Keyword|T],InputList,Comment) :-
	rules([Keyword,Reassembly]),
	mc_aux([Keyword|T],Reassembly,InputList,Comment),!.
makecomment([_|T],InputList,Comment) :-
	makecomment(T,InputList,Comment),!.


mc_aux(KeyWordList,[[DRuleNum,MatchRule,N|T]|_],InputList,Comment) :-
	match(MatchRule,InputList),
	mc_aux2(KeyWordList,DRuleNum,N,T,InputList,Comment),!.
mc_aux(KeyWordList,[_|T],InputList,Comment) :-
	mc_aux(KeyWordList,T,InputList,Comment).
mc_aux(_,[],_,_) :- !,fail.

mc_aux2(KeyWordList,DRuleNum,N,T,InputList,Comment) :-
	length(T,TLen),
	N < TLen, !,
	NewN is N + 1,
	findnth(T,NewN,Mn),
	mc_aux3(KeyWordList,DRuleNum,N,NewN,Mn,InputList,Comment).
mc_aux2(KeyWordList,DRuleNum,N,T,InputList,Comment) :-
	member(Mn,T),
	mc_aux3(KeyWordList,DRuleNum,N,0,Mn,InputList,Comment).

mc_aux3([Keyword|T],DRuleNum,N,NewN,[equal,MnT],InputList,Comment) :-
	!,
	updaterule(Keyword,DRuleNum,N,NewN),
	makecomment([MnT|T],InputList,Comment).
mc_aux3([Keyword|T],DRuleNum,N,NewN,[newkey],InputList,Comment) :-
	!,
	updaterule(Keyword,DRuleNum,N,NewN),
	makecomment(T,InputList,Comment).
mc_aux3([Keyword|_],DRuleNum,N,NewN,Mn,_,Mn) :-
	updaterule(Keyword,DRuleNum,N,NewN).

process_input([],L,L).
process_input(['.'|_],L,L) :- findkeywords(L,K), length(K,Kl), Kl >= 3,!.
process_input(['.'|T],_,L) :- !, process_input(T,[],L).
process_input([','|_],L,L) :- findkeywords(L,K), length(K,Kl), Kl >= 3,!.
process_input([','|T],_,L) :- !, process_input(T,[],L).
process_input([H|T],S,L) :- append(S,[H],S2), process_input(T,S2,L).

findkeywords([],[[memory,0],[none,0]]).
findkeywords([H|T],[[H,I]|T1]) :- rules([[H,I]|_]), !, findkeywords(T,T1).
findkeywords([_|T],T1) :- findkeywords(T,T1).

sortkeywords(X,Y) :- sort_aux(X,A,1), !, sortkeywords(A,Y).
sortkeywords(X,Y) :- sort_aux(X,Y,_).

sort_aux([],[],0).
sort_aux([X],[X],0).
sort_aux([[A,X],[B,Y]|T],[[B,Y],[A,X]|T],1) :- X < Y.
sort_aux([X,Y|T],[X|T2],S) :- sort_aux([Y|T],T2,S).

updaterule(KeyList,DRuleNum,N,NewN) :-
	retract(rules([KeyList,Rt])),
	replace([DRuleNum,A,N|T],Rt,[DRuleNum,A,NewN|T],Rt2),
	assertz(rules([KeyList,Rt2])).

writecomment([]).
writecomment(['I'|T]) :- !, write('I'), writecomment_aux(T).
writecomment([H|T]) :- !,
	name(H,[C|L]),
	D is C - 32,
        name(Z,[D|L]),
	write(Z),
	writecomment_aux(T).

writecomment_aux([]).
writecomment_aux([H|T]) :-
	name(H,[C]),
	my_char_type(C,punctuation), !,
	write(H),
	writecomment_aux(T).
writecomment_aux([H|T]) :-
	write(' '),
	write(H),
	writecomment_aux(T).

quittime(X) :- member('quit',X).

eliza(InputFromUser,Comment) :-
	retractall(mem(_)),
	   read_atomics(InputFromUser,Input),
           process_input(Input,[],Input2),
           simplify(Input2,Input3),
           findkeywords(Input3,KeyWords),
           sortkeywords(KeyWords,KeyWords2),
           write(KeyWords2),
           makecomment(KeyWords2,Input3,Comment),!.




sr([spinning|X],[udochka|Y],X,Y).
sr([udilishhe|X],[udochka|Y],X,Y).
sr([rihbak|X],[ulov|Y],X,Y).
sr([rihba|X],[ulov|Y],X,Y).
sr([klev|X],[ulov|Y],X,Y).
sr([dobihcha|X],[ulov|Y],X,Y).
sr([setjh|X],[ulov|Y],X,Y).
sr([nazhivka|X],[ulov|Y],X,Y).
sr([eda|X],[rihba|Y],X,Y).
sr([more|X],[rihba|Y],X,Y).
sr([cheshuja|X],[rihba|Y],X,Y).
sr([karasjh|X],[rihba|Y],X,Y).
sr([karp|X],[rihba|Y],X,Y).
sr([zapakh|X],[rihba|Y],X,Y).
sr([kostochki|X],[rihba|Y],X,Y).
sr([govjadina|X],[mjaso|Y],X,Y).
sr([svinina|X],[mjaso|Y],X,Y).
sr([baranina|X],[mjaso|Y],X,Y).
sr([kurica|X],[mjaso|Y],X,Y).
sr([sup|X],[ukha|Y],X,Y).
sr([eda|X],[ukha|Y],X,Y).%%
sr([kotelok|X],[ukha|Y],X,Y).
sr([khleb|X],[ukha|Y],X,Y).
sr([vozdukh|X],[ukha|Y],X,Y).
sr([pokhlebka|X],[ukha|Y],X,Y).
sr([rihba|X],[ukha|Y],X,Y).   %%
sr([priroda|X],[otdihkh|Y],X,Y).
sr([druzjhja|X],[otdihkh|Y],X,Y).
sr([osenjh|X],[otdihkh|Y],X,Y).
sr([vozdukh|X],[otdihkh|Y],X,Y).
sr([son|X],[otdihkh|Y],X,Y).
sr([more|X],[otdihkh|Y],X,Y).
sr([pljazh|X],[otdihkh|Y],X,Y).
sr([kanikulih|X],[otdihkh|Y],X,Y).
sr([leto|X],[otdihkh|Y],X,Y).
sr([ozero|X],[rechka|Y],X,Y).
sr([voda|X],[rechka|Y],X,Y).
sr([techenie|X],[rechka|Y],X,Y).
sr([bereg|X],[rechka|Y],X,Y).
sr([prud|X],[rechka|Y],X,Y).
sr([molljusk|X],[rechka|Y],X,Y).
sr([rak|X],[rechka|Y],X,Y).
sr([plavanie|X],[rechka|Y],X,Y).
sr([motihljh|X],[chervjak|Y],X,Y).
sr([krjuchok|X],[chervjak|Y],X,Y).
sr([zemlja|X],[chervjak|Y],X,Y).
sr([nazhivka|X],[chervjak|Y],X,Y).
sr([jabloko|X],[chervjak|Y],X,Y).
sr([dozhdjh|X],[chervjak|Y],X,Y).
sr([chervjh|X],[chervjak|Y],X,Y).
sr([krjuchok|X],[nazhivka|Y],X,Y).
sr([leska|X],[nazhivka|Y],X,Y).
sr([karasjh|X],[nazhivka|Y],X,Y).
sr([mormihshka|X],[nazhivka|Y],X,Y).
sr([primanka|X],[nazhivka|Y],X,Y).
sr([podkormka|X],[nazhivka|Y],X,Y).
sr([voskhod|X],[utro|Y],X,Y).
sr([solnce|X],[utro|Y],X,Y).
sr([kholod|X],[utro|Y],X,Y).
sr([svezhestjh|X],[utro|Y],X,Y).
sr([rosa|X],[utro|Y],X,Y).
sr([svet|X],[utro|Y],X,Y).
sr([kofe|X],[utro|Y],X,Y).
sr([budiljhnik|X],[utro|Y],X,Y).
sr([zarjadka|X],[utro|Y],X,Y).
sr([posuda|X],[kotelok|Y],X,Y).
sr([chugun|X],[pokhod|Y],X,Y).
sr([pishha|X],[utro|Y],X,Y).
sr([koster|X],[utro|Y],X,Y).
sr([vkusno|X],[utro|Y],X,Y).
sr([palatka|X],[pokhod|Y],X,Y).
sr([turizm|X],[pokhod|Y],X,Y).
sr([osenjh|X],[pokhod|Y],X,Y).
sr([pesnja|X],[pokhod|Y],X,Y).
sr([shashlihk|X],[pokhod|Y],X,Y).
sr([rjukzak|X],[pokhod|Y],X,Y).
sr([tushenka|X],[pokhod|Y],X,Y).
sr([les|X],[priroda|Y],X,Y).
sr([reka|X],[priroda|Y],X,Y).
sr([pole|X],[priroda|Y],X,Y).
sr([krasota|X],[priroda|Y],X,Y).
sr([zhiznjh|X],[priroda|Y],X,Y).
sr([derevo|X],[priroda|Y],X,Y).
sr([otdihkh|X],[priroda|Y],X,Y).
sr([derevnja|X],[priroda|Y],X,Y).
sr([peyzazh|X],[priroda|Y],X,Y).
sr([khvoja|X],[les|Y],X,Y).
sr([derevjhja|X],[les|Y],X,Y).
sr([ugli|X],[koster|Y],X,Y).
sr([plamja|X],[koster|Y],X,Y).
sr([teplo|X],[koster|Y],X,Y).
sr([ujut|X],[koster|Y],X,Y).
sr([pesnja|X],[koster|Y],X,Y).
sr([gitara|X],[koster|Y],X,Y).
sr([dihm|X],[kopchenie|Y],X,Y).
sr([ugoljh|X],[kopchenie|Y],X,Y).
sr([shashlihk|X],[kopchenie|Y],X,Y).
sr([ozero|X],[lodka|Y],X,Y).
sr([veslo|X],[lodka|Y],X,Y).
sr([volna|X],[lodka|Y],X,Y).
sr([rihbak|X],[lodka|Y],X,Y).
sr([motor|X],[lodka|Y],X,Y).
sr([sudno|X],[barzha|Y],X,Y).
sr([kater|X],[barzha|Y],X,Y).
sr([malek|X],[prorubjh|Y],X,Y).
sr([kreshhenie|X],[prorubjh|Y],X,Y).
sr([baklan|X],[sadok|Y],X,Y).
sr([primanka|X],[sadok|Y],X,Y).
sr([khranitjh|X],[sadok|Y],X,Y).
sr([nozh|X],[cheshuja|Y],X,Y).
sr([blesk|X],[cheshuja|Y],X,Y).
sr([slizjh|X],[cheshuja|Y],X,Y).
sr([karasjh|X],[cheshuja|Y],X,Y).
sr([zashhita|X],[cheshuja|Y],X,Y).
sr([tvorog|X],[belok|Y],X,Y).
sr([jayca|X],[belok|Y],X,Y).
sr([jayco|X],[belok|Y],X,Y).
sr([zdorovjhe|X],[vitaminih|Y],X,Y).
sr([zozh|X],[vitaminih|Y],X,Y).
sr([sport|X],[vitaminih|Y],X,Y).
sr([ovoshhi|X],[vitaminih|Y],X,Y).
sr([rastenija|X],[vitaminih|Y],X,Y).
sr([poljhza|X],[vitaminih|Y],X,Y).
sr([polezno|X],[vitaminih|Y],X,Y).
sr([sok|X],[vitaminih|Y],X,Y).
sr([kaljhciy|X],[vitaminih|Y],X,Y).
sr([lekarstvo|X],[zhir|Y],X,Y).
sr([boleznjh|X],[zhir|Y],X,Y).
sr([deti |X],[zhir|Y],X,Y).
sr([neprijaznjh|X],[zhir|Y],X,Y).
sr([otvrashhenie|X],[zhir|Y],X,Y).
sr([zhir|X],[zhir|Y],X,Y).
sr([detstvo|X],[zhir|Y],X,Y).
sr([skelet|X],[kosti|Y],X,Y).
sr([kostochki|X],[kosti|Y],X,Y).
sr([boloto|X],[komarih|Y],X,Y).
sr([mukhi|X],[komarih|Y],X,Y).
sr([ukus|X],[komarih|Y],X,Y).
sr([nasekomihe|X],[komarih|Y],X,Y).
sr([ballonchik|X],[sprey|Y],X,Y).
sr([mazjh|X],[sprey|Y],X,Y).
sr([plastinih|X],[sprey|Y],X,Y).
sr([chas|X],[vremja|Y],X,Y).
sr([minuta|X],[vremja|Y],X,Y).
sr([sekunda|X],[vremja|Y],X,Y).
sr([ruzhjhe|X],[okhota|Y],X,Y).
sr([dichjh|X],[okhota|Y],X,Y).
sr([dobihcha|X],[okhota|Y],X,Y).
sr([leto|X],[vremja|Y],X,Y).
sr([osenjh|X],[vremja|Y],X,Y).
sr([vesna|X],[vremja|Y],X,Y).
sr([zima|X],[vremja|Y],X,Y).
sr([janvarjh|X],[vremja|Y],X,Y).
sr([fevraljh|X],[vremja|Y],X,Y).
sr([apreljh|X],[vremja|Y],X,Y).
sr([may|X],[vremja|Y],X,Y).
sr([ijunjh|X],[vremja|Y],X,Y).
sr([ijuljh|X],[vremja|Y],X,Y).
sr([avgust|X],[vremja|Y],X,Y).
sr([sentjabrjh|X],[vremja|Y],X,Y).
sr([oktjabrjh|X],[vremja|Y],X,Y).
sr([nojabrjh|X],[vremja|Y],X,Y).
sr([dekabrjh|X],[vremja|Y],X,Y).
sr([uvlechenie|X],[khobbi|Y],X,Y).
sr([tvorchestvo|X],[khobbi|Y],X,Y).
sr([interes|X],[khobbi|Y],X,Y).
sr([dosug|X],[khobbi|Y],X,Y).
sr([trjasina|X],[boloto|Y],X,Y).
sr([sapogi|X],[boloto|Y],X,Y).
sr([topjh|X],[boloto|Y],X,Y).
sr([grjazjh|X],[boloto|Y],X,Y).
sr([gramm|X],[kilogramm|Y],X,Y).
sr([santimetr|X],[kilogramm|Y],X,Y).
sr([dlina|X],[kilogramm|Y],X,Y).
sr([boljhshoy|X],[kilogramm|Y],X,Y).
sr([boljhshaja|X],[kilogramm|Y],X,Y).
sr([boljhshoe|X],[kilogramm|Y],X,Y).
sr([boljhshie|X],[kilogramm|Y],X,Y).
sr([boljhshe|X],[kilogramm|Y],X,Y).
sr([sovet|X],[pomoshhjh|Y],X,Y).
sr([podskazka|X],[pomoshhjh|Y],X,Y).
sr([podmoga|X],[pomoshhjh|Y],X,Y).
sr([podmoga|X],[selo|Y],X,Y).
sr([derevnja|X],[selo|Y],X,Y).
sr([rechka|X],[selo|Y],X,Y).
sr([ogorod|X],[selo|Y],X,Y).
sr([ozero|X],[selo|Y],X,Y).
sr([dom|X],[dacha|Y],X,Y).
sr([khodjhba|X],[sport|Y],X,Y).
sr([beg|X],[sport|Y],X,Y).
sr([tennis|X],[sport|Y],X,Y).
sr([velosport|X],[sport|Y],X,Y).
sr([rihbalka|X],[sport|Y],X,Y).
sr([badminton|X],[sport|Y],X,Y).
sr([zozh|X],[sport|Y],X,Y).
sr([forma|X],[sport|Y],X,Y).
sr([zhir|X],[sport|Y],X,Y).
sr([ozhirenie|X],[sport|Y],X,Y).
sr([kholesterin|X],[sport|Y],X,Y).
sr([vred|X],[sport|Y],X,Y).
sr([ruljh|X],[velosiped|Y],X,Y).
sr([pedali|X],[velosiped|Y],X,Y).
sr([pihljh|X],[grjazjh|Y],X,Y).
sr([luzhi|X],[grjazjh|Y],X,Y).
sr([sljakotjh|X],[grjazjh|Y],X,Y).
sr([sljakotjh|X],[grjazjh|Y],X,Y).
sr([moroz|X],[kholod|Y],X,Y).
sr([zima|X],[kholod|Y],X,Y).
sr([sneg|X],[kholod|Y],X,Y).
sr([shhebet|X],[pticih|Y],X,Y).
sr([polet|X],[pticih|Y],X,Y).
sr([penie|X],[pticih|Y],X,Y).
sr([shhebetanie|X],[pticih|Y],X,Y).
sr([zhivotnihe|X],[zveri|Y],X,Y).
sr([navihk|X],[ucheba|Y],X,Y).
sr([sposob|X],[ucheba|Y],X,Y).
sr([obuchenie|X],[ucheba|Y],X,Y).
sr([uchenie|X],[ucheba|Y],X,Y).
sr([mukhomor|X],[grib|Y],X,Y).
sr([lisichka|X],[grib|Y],X,Y).
sr([maslenok|X],[grib|Y],X,Y).
sr([gribnica|X],[grib|Y],X,Y).
sr([bojaznjh|X],[strakh|Y],X,Y).
sr([smelostjh|X],[strakh|Y],X,Y).
sr([uzhas|X],[strakh|Y],X,Y).
sr([fobija|X],[strakh|Y],X,Y).
sr([panika|X],[strakh|Y],X,Y).
sr([ispug|X],[strakh|Y],X,Y).
sr([zmei|X],[strakh|Y],X,Y).
sr([pauki|X],[strakh|Y],X,Y).
sr([dikiy|X],[strakh|Y],X,Y).
sr([ladno|X],[okey|Y],X,Y).
sr([davay|X],[okey|Y],X,Y).
sr([ona|X],[on|Y],X,Y).
sr([ono|X],[on|Y],X,Y).
sr([nedostatochno|X],[malo|Y],X,Y).
sr([mnogo|X],[malo|Y],X,Y).
sr([nenavizhu|X],[ljublju|Y],X,Y).
sr([predpochitaju|X],[ljublju|Y],X,Y).
sr([normaljhno|X],[ljublju|Y],X,Y).
sr([plokho|X],[ljublju|Y],X,Y).
sr([otlichno|X],[ljublju|Y],X,Y).
sr([super|X],[ljublju|Y],X,Y).
sr([klassno|X],[ljublju|Y],X,Y).
sr([velikolepno|X],[ljublju|Y],X,Y).
sr([somniteljhno|X],[ljublju|Y],X,Y).
sr([luchshe|X],[ljublju|Y],X,Y).
sr([khuzhe|X],[ljublju|Y],X,Y).
sr([zdravstvuy|X],[privet|Y],X,Y).
sr([zdravstvuyte|X],[privet|Y],X,Y).
sr([privetstvuju|X],[privet|Y],X,Y).
sr([moy|X],[on|Y],X,Y).
sr([tvoy|X],[on|Y],X,Y).
sr([nash|X],[on|Y],X,Y).
sr([vash|X],[on|Y],X,Y).
sr([ikh|X],[on|Y],X,Y).
sr([kurica|X],[mjaso|Y],X,Y).
sr([svinina|X],[mjaso|Y],X,Y).
sr([govjadina|X],[mjaso|Y],X,Y).
sr([baranina|X],[mjaso|Y],X,Y).
sr([karasjh|X],[rihba|Y],X,Y).
sr([lososjh|X],[rihba|Y],X,Y).
sr([foreljh|X],[rihba|Y],X,Y).
sr([treska|X],[rihba|Y],X,Y).
sr([okunjh|X],[rihba|Y],X,Y).
sr([vkusno|X],[eda|Y],X,Y).
sr([appetitno|X],[eda|Y],X,Y).
sr([sochno|X],[eda|Y],X,Y).


:- dynamic(rules/1).

rules([[udochka,5],[
	[1,[_],0,
		[chto,vih,znaete,ob,udochkakh,'?'],
        [a,kakie,raznovidnosti,udochek,vih,znaete,'?']]]]).

rules([[ulov,3],[
	[1,[_],0,
		[chto,vih,sdelaete,s,rihboy,',',kotoruju,poymaete,'?'],
        [kak,vih,schitaete,',',estjh,kakie-libo,obshhie,principih,uspeshnoy,lovli,',',mozhete,nazvatjh,'?']]]]).

rules([[rihba,2],[
	[1,[_],0,
        [kakuju,rihbu,predpochitaete,'?'],
        [kak,vih,schitaete,lovitjh,rihbu,',',a,zatem,ee,otpuskatjh,gumanno,i,pochemu,'?']]]]).

rules([[mjaso,2],[
	[1,[_,ljublju,X],0,
	    [ja,tozhe,ljublju,X,',',osobenno,esli,praviljhno,prigotovitjh,kak,vih,schitaete,polezno,li,estjh,[kljuchevoe,slovo],dlja,zdorovjhja,'?']]]]).

rules([[ukha,4],[
	[1,[_],0,
	    [chto,dumaete,ob,supe,ukhe,'?'],
        [govorjat,',',khoroshaja,khozjayka,i,iz,petukha,ukhu,svarit,',',a,iz,kakikh,eshhe,ingredientov,mozhno,svaritjh,ukhu,'?']]]]).

rules([[otdihkh,6],[
	[1,[_],0,
		[ja,ljublju,chisto,na,prirode,otdokhnutjh,',',nu,rihbalka,tam,',',kosterok,',',palatki,a,vih,kak,provodite,otdihkh,na,prirode,'?'],
            [ochenjh,vazhen,dlja,organizma,osobenno,zdorovihy,son,i,praviljhnoe,pitanie,',',kak,vih,schitaete,'?'],
            [kak,vih,ljubite,otdihkhatjh,na,prirode,'?'],
            [kak,provesti,otdihkh,poleno,i,prijatno,'?',chto,dlja,ehtogo,nuzhno,delatjh,'?']]]]).

rules([[rechka,1],[
	[1,[_],0,
		[goroda,vsegda,stroili,rjadom,s,rekoy,pochemu,'?'],
        [znoynihm,letom,kupatjhsja,polezno,da,',',no,vazhno,ne,zaboletjh,i,ne,natknutjhsja,na,zmeju,v,vode,',',a,kak,eshhe,mozhno,predosterechjhsja,ot,opasnostey,'?']]]]).

rules([[chervjak,1],[
	[1,[_],0,
	    [na,chto,kak,vih,schitaete,luchshe,kljuet,rihba,v,obshhem,sluchae,'?']]]]).

rules([[nazhivka,1],[
	[1,[_],0,
        [kakie,v,principe,nazhivki,znaete,'?']]]]).
rules([[utro,1],[
	[1,[_],0,
		[na,rihbalku,nado,vstavatjh,rano,',',pochemu,'?']]]]).

rules([[kotelok,1],[
	[1,[_],0,
	    [kotelok,mmm,ja,ljublju,pishhu,prigotovlennuju,na,otkrihtom,ogne,',',tem,bolee,',',chto,ona,poleznee,',',a,vih,chto,dumaete,ob,ehtom,'?']]]]).

rules([[pokhod,2],[
	[1,[_],0,
	    [mnogie,ljubjat,pokhodih,na,prirodu,imenno,za,polevuju,kukhnju,',',khodjat,mnogie,sporih,o,vkusnote,i,poleznosti,pishhi,',',prigotovlennoy,na,ogne,',',kakova,vasha,pozicija,'?']]]]).

rules([[priroda,3],[
	[1,[_],0,
        [vih,vihkhodite,na,prirodu,s,blizkimi,ili,druzjhjami,'?',esli,da,',',to,chto,tam,delaete,'?'],
        [otdihkh,na,prirode,',',ehto,polezno,',',da,',',no,mnogie,ljudi,ne,umejut,sebja,vesti,v,lesu,',',naprimer,',',vih,znaete,kak,nuzhno,vesti,sebja,'?',rasskazhite,',',pozhaluysta]]]]).

rules([[les,2],[
	[1,[_],0,
	    [gorodskim,zhiteljam,polezno,vihbiratjhsja,za,gorod,inogda,vih,kak,dumaete,'?'],
     [kak,v,lesu,mozhno,provesti,vremja,s,poljhzoy,i,veselo,'?']]]]).

rules([[koster,2],[
	[1,[_],0,
	    [s,chem,u,vas,associiruetsja,koster,'?'],
        [kakie,mihsli,ili,zhelanija,u,vas,voznikajut,sidja,u,kostra,'?']]]]).

rules([[belok,2],[
	[1,[_],0,
		[belok,-,ehto,nash,stroiteljhnihy,material,',',kak,govoritsja,a,gde,on,soderzhitsja,v,boljhshom,kolichestve,'?']]]]).

rules([[vitaminih,3],[
	[1,[_],0,
		[v,rihbe,ogromnoe,kolichestvo,vitaminov,',',kak,dumaete,',',vredno,li,perestatjh,estjh,rihbu,'?']]]]).

rules([[zhir,2],[
	[1,[_],0,
		[zachem,organizmu,vitaminih,'?'],
        [rihbiy,zhir,lekarstvo,nomer, odin,dlja,profilaktiki,prostudih,kakoe,vashe,otnoshenie,k,rihbjhemu,zhiru,'?']]]]).

rules([[kosti,2],[
	[1,[_],0,
		[kostochki,odna,iz,prichin,ne,estjh,rihbu,',',osobenno,detjam,i,sobakam,oni,mogut,podavitjhsja,!,zachem,voobshhe,estjh,rihbu,esli,ona,takaja,opasnaja,'?']]]]).

rules([[komarih,2],[
	[1,[_],0,
		[komarih,ehto,zhutjh,ne,ljublju,ikh,kak,vih,boritesjh,s,nimi,'?']]]]).

rules([[sprey,2],[
	[1,[_],0,
	[aehrozoljhnihe,sprei,sposobstvujut,razrusheniju,ozonovogo,sloja,zemli,',',kak,vih,dumaete,ehta,problema,razduta,',',ili,deystviteljhno,ochenjh,vazhna,'?']]]]).

rules([[vremja,2],[
	[1,[_],0,
		[ja,uveren,',',chto,',',poroy,',',mozhno,vihdelitjh,denjh,na,otdihkh,na,prirode,',',osobenno,kogda,sezon,',',vih,vihdeljaete,vremja,na,otdihkh,na,prirode,'?'],
        [kak,vih,dumaete,kak,izmenjaetsja,klev,v,techenii,goda,'?'],
		[kakoe,vashe,ljubimoe,vremja,goda,'?',pochemu,'?'],
        [delu,vremja,',',potekhe,chas,otdihkh,vazhen,'?',kak,vih,schitaete,'?']]]]).


rules([[okhota,2],[
	[1,[_],0,
		[ja,okhotitjhsja,ne,ljublju,',',zhalko,ubivatjh,zhivotnihkh,',',a,vih,kak,k,ehtomu,otnositesjh,'?']]]]).

rules([[sezon,2],[
	[1,[_],0,
		[kstati,',',naschet,sezona,pochemu,neljhzja,lovitjh,rihbu,vesnoy,'?']]]]).

rules([[khobbi,3],[
	[1,[_],0,
		[na,vash,vzgljad,rihbalka,ehto,khoroshee,khobbi,i,pochemu,'?'],
        [kak,vih,razvlekaetesjh,na,prirode,'?']]]]).

rules([[boloto,2],[
	[1,[_],0,
		[ja,znaju,',',chto,v,bolote,nikto,ne,obitaet,',',inache,ehto,ozero,',',tam,rihbachitjh,bessmihslenno,rasskazhite,pro,vashi,poezdki,na,'?']]]]).

rules([[kilogramm,2],[
	[1,[_],0,
		[ogo,',',kakoy,ogromnihy,',',chto,potom,bihlo,'?']]]]).
rules([[pomoshhjh,2],[
	[1,[_],0,
		[vih,kogda-nibudjh,terjalisjh,v,lesu,'?',esli,da,',',to,rasskazhite,kak,vihbralisjh,'?']]]]).

rules([[selo,3],[
	[1,[_],0,
		[vih,chasto,vihezzhaete,za,gorod,'?'],
        [chto,obihchno,delaete,za,predelami,goroda,'?']]]]).

rules([[dacha,2],[
	[1,[_],0,
        [chto,vih,delaete,na,dache,'?'],
        [obihchno,ljudi,ljubjat,uedinitjhsja,podaljhshe,ot,gorodskoy,suetih,na,dache,',',chto,dvizhet,vami,'?']]]]).

rules([[velosiped,3],[
	[1,[_],0,
		[vih,uvlekaetesjh,velosportom,'?'],
        [kak,vih,nauchilisjh,katatjhsja,na,velosipede,'?']]]]).

rules([[mashina,2],[
	[1,[_],0,
		[v,bezdorozhnoy,mestnosti,ne,oboytisjh,bez,prokhodimoy,mashinih,',',kakoy,dolzhna,bihtjh,mashina,dlja,kempinga,i,otdihkha,na,prirode,'?']]]]).

rules([[grjazjh,2],[
	[1,[_],0,
		[v,lesu,zachastuju,plokhie,dorogi,',',kak,luchshe,vsego,peredvigatjhsja,po,lesu,'?']]]]).

rules([[sport,3],[
	[1,[_],0,
		[pochemu,vazhno,zanimatjhsja,sportom,'?',estjh,mnenie,',',chto,ehto,atavizm]]]]).


rules([[kholod,2],[
	[1,[_],0,
	[ne,ljublju,kholod,',',mozhno,zaprosto,zaboletjh,',',da,i,rihbachitjh,neudobno,',',vih,kak,schitaete,'?']]]]).

rules([[pticih,2],[
	[1,[_],0,
	[kak,vih,otnositesjh,k,peniju,ptic,'?']]]]).

rules([[zveri,3],[
	[1,[_],0,
		[zhivotnihe,v,lesu,dovoljhno,opasnih,',',kakie,merih,predostorozhnosti,nuzhno,sobljudatjh,'?']]]]).

rules([[ucheba,2],[
	[1,[_],0,
        [esli,vih,ljubite,rihbachitjh,',',to,rasskazhite,o,vashem,pervom,opihte],
        [kak,vih,nauchilisjh,rihbachitjh,'?']]]]).

rules([[grib,2],[
	[1,[_],0,
		[poroy,ja,ljublju,proguljatjhsja,v,lesu,sobiraja,gribih,a,vih,'?']]]]).

rules([[strakh,2],[
	[1,[_],0,
		[esli,u,vas,estjh,strakhi,svjazannihe,s,rihbalkoy,',',ili,v,celom,vihkhodom,na,prirodu,',',opishite,ikh]]]]).

rules([[opasnostjh,3],[
	[1,[_],0,
		[kakie,opasnosti,tait,les,'?',chego,stoit,opasatjhsja,'?'],
        [kak,izbezhatjh,neprijatnostey,v,lesu,'?']]]]).

rules([[eda,5],[
	[1,[_],0,
		[vsjakaja,li,rihba,polezna,i,pochemu,'?',chto,v,ney,poleznogo,'?'],
        [pochemu,vazhno,sobljudatjh,zdorovoe,pitanie,'?'],
        [estjh,takoy,podkhod,k,pitaniju,moy,organizm,ne,pomoyka,',',chto,vih,slihshali,o,nem,'?']]]]).

rules([[vegetarianstvo,3],[
	[1,[_],0,
		[chto,dumaete,o,vegetariancakh,'?'],
        [pochemu,vegetariancih,ne,edjat,mjaso,'?'],
        [kak,vih,dumaete,',',vegetariancih,poluchajut,vse,poleznihe,veshhestva,bez,mjasa,'?']]]]).

rules([[rihbak,2],[
	[1,[_],0,
		[u,vas,mnogo,znakovihkh,rihbolovov,'?']]]]).

rules([[klev,2],[
	[1,[_],0,
		[kak,dumaete,',',chto,pomogaet,dobitjhsja,khoroshego,kleva,'?']]]]).

rules([[prikormka,2],[
	[1,[_],0,
		[kak,vih,dumaete,zachem,nuzhna,prikormka,v,rihbnoy,lovle,'?']]]]).

rules([[primanka,2],[
	[1,[_],0,
		[sushhestvuet,ogromnoe,kolichestvo,sposobov,primanitjh,rihbu,',',kakie,po,vashemu,mneniju,sposobih,primanitjh,rihbu,'?']]]]).

rules([[rihbalka,2],[
	[1,[_],0,
		[dlja,menja,slovo,rihbalka,sinonim,slova,spokoystvie,',',rihbalka,pomogaet,rasslabitjhsja,'?']]]]).


rules([[lovushka,2],[
	[1,[_],0,
	[estjh,takaja,poslovica,rihbaka,setjh,kormit],
    [v,rihbnoy,lovle,estjh,kucha,lovushek,',',kak,dumaete,kakoy,u,nikh,princip,'?']]]]).

rules([[vihlovitjh,2],[
	[1,[_],0,
		[esli,vih,novichok,',',to,ochenjh,vazhno,nauchitjhsja,podsekatjh,rihbu,',',kak,vih,dumaete,pochemu,ehto,vazhno,'?']]]]).

rules([[druzjhja,2],[
	[1,[_],0,
		[druzjhja,-,ehto,prekrasno,',',vsegda,nuzhnih,ljudi,rjadom,kak,vashi,druzjhja,otnosjatsja,k,rihbalke,'?']]]]).

rules([[brakonjher,2],[
	[1,[_],0,
		[brakonjherih,opasnihe,prestupniki,',',kak,vih,schitaete,'?'],
        [iz-za,brakonjherov,ischezli,mnogie,vidih,zhivotnihkh,',',kak,vih,dumaete,naskoljhko,siljhnihy,ushherb,nanes,chelovek,prirode,'?']]]]).


rules([[meditacija,2],[
	[1,[_],0,
		[vazhno,nayti,svoy,sposob,relaksa,',',tak,kak,vihsokiy,temp,zhizni,sposobstvuet,nervoznosti,kak,vih,schitaete,aktivnihy,otdihkh,na,prirode,sposobstvuet,rasslableniju,'?',pochemu,'?'],
        [yoga,i,meditacija,ehto,ochenjh,zdorovo,',',planiruju,zanjatjhsja,ehtim,v,budushhem,',',a,vih,chto,dumaete,ob,ehtom,'?']]]]).

rules([[kleshhi,2],[
	[1,[_],0,
		[kleshhi,opasnihe,parazitih,',',obitajushhie,v,trave,',',kak,obezopasitjh,sebja,ot,nikh,'?']]]]).

rules([[piknik,3],[
	[1,[_],0,
	[ja,ljublju,pikniki,',',osobenno,v,mae,',',shashlihki,',',igrih,',',druzjhja,',',leto,vih,kak,k,ehtomu,otnositesjh,'?'],
    [poslednee,vremja,v,lesu,postojanno,obnaruzhivaju,ostavlennihe,butihlki,i,prochee,kak,vih,dumaete,po,ehtomu,povodu,'?']]]]).

rules([[magazin,2],[
	[1,[_],0,
		[u,menja,estjh,ljubimihy,magazin,v,moey,derevne,nazihvaetsja,nakhodka,tam,prodajut,vse,vidih,snastey,ot,malo,do,velika,vopros,v,tom,',',chto,mnogie,rihbolovih,predpochitajut,zavodskim,snastjam,sdelannihe,sobstvennoruchno,kak,dumaete,oni,delajut,ehto,iz,chuvstv,nostaljhgii,ili,po,kakim-to,svoim,soobrazhenijam,'?']]]]).

rules([[snasti,2],[
	[1,[_],0,
		[ja,khochu,skazatjh,',',chto,v,poslednie,20,let,pojavilasjh,uyma,vidov,snastey,raznihkh,vidov,vot,vih,',',k,primeru,',',o,kakikh,slihshali,',',mozhet,bihtjh,slihshali,ili,chto,vih,voobshhe,znaete,o,snastjakh,'?']]]]).

rules([[nauchilsja,2],[
	[1,[_],0,
		[ja,nauchilsja,rihbachitjh,blagodarja,moemu,ljubimomu,dedushku,',',a,vash,dedushka,uchil,vas,rihbachitjh,'?',rasskazhite,'?'],
        [ne,vsjak,rihbak,',',kto,raz,rihbu,poymal,',',chem,otlichaetsja,rihbak,ot,ne,rihbaka,po,vashemu,mneniju,'?',vsjakiy,li,chelovek,javljaetsja,rihbakom,po,pravu,rozhdenija,'?',kak,vih,ponimaete,poslovicu,'?']]]]).

rules([[lovkostjh,3],[
	[1,[_],0,
		[dlja,menja,uchitsja,rihbalke,bihlo,srodni,uchebe,programmirovaniju,',',bihlo,veselo,',',radostno,',',ja,dazhe,poymal,okunja,"(",ili,napisal,programmu,")"],
        [dlja,vas,process,obuchenija,bihl,takim,zhe,prijatnihm,'?']]]]).

rules([[otpustitjh,2],[
	[1,[_],0,
		[mnogie,"(",v,tom,chisle,i,ja,")",ljubjat,rihbalku,chisto,iz,sportivnogo,interesa,i,otpuskajut,rihbu,obratno,v,vodu,a,dlja,vas,rihbalka,"—",ehto,chto,'?']]]]).


rules([[vodoem,3],[
	[1,[_],0,
		[khoroshiy,rihbak,dolzhen,po,klevu,znatjh,',',kak,rihbu,zvatjh,kak,vih,dumaete,ot,vodoema,ili,chego,voobshhe,zavisit,khoroshiy,klev,'?',ili,zhe,delo,toljhko,v,rihbake],
        [kakoy,vodoem,dolzhen,bihtjh,',',chtobih,klev,bihl,khoroshim,'?']]]]).


rules([[poterjatjhsja,3],[
	[1,[_],0,
		[chto,neobkhodimo,sdelatjh,v,pervuju,ocheredjh,esli,vih,poterjalisjh,v,lesu,'?'],
        [skazhite,',',chto,na,vash,vzgljad,nuzhno,imetjh,bagazhe,znaniy,chtobih,ne,poterjatjhsja,v,poterjatjhsja,v,lesu]]]]).

rules([[lekarstva,2],[
	[1,[_],0,
		[v,lesu,polno,opasnostey,',',osobenno,esli,les,dzhungli,vazhno,sdelatjh,privivki,zaranee,i,vzjatjh,s,soboy,kak,vih,dumaete,kakie,lekarstva,prigodjatsja,',',naprimer,',',v,pokhode,'?']]]]).

rules([[sorvalasjh,2],[
	[1,[_],0,
		[kak,govoritsja,rihbak,dushu,ne,morit,rihbih,net,',',tak,chay,varit,ochenjh,vazhno,nauchitjhsja,podsekatjh,rihbu,',',chtob,ne,uplihla]]]]).

rules([[ehkologija,2],[
	[1,[_],0,
		[v,21,veke,obstanovka,s,musorom,deystviteljhno,stanovitsja,katastroficheskoy,',',chto,po,vashemu,mneniju,nuzhno,predprinjatjh,',',chtobih,izbezhatjh,tragedii,'?']]]]).


rules([[musor,3],[
	[1,[_],0,
		[v,atlanticheskom,okeane,uzhe,pojavilsja,celihy,musornihy,kontinent,razmerom,s,franciju,',',a,musor,v,vode,',',kak,izvestno,ne,razlagaetsja,ehto,deystviteljhno,pechaljhno,kak,vih,schitaete,'?'],
        [kachestvennaja,pererabotka,musora,ehto,uzhe,tekhnologii,nashego,i,budushhego,vremeni,inache,reki,i,ozera,stanut,neprigodnih,dlja,zhizni,kak,vih,polagaete,chto,mozhet,pomochjh,ochistitjh,vodoemih,'?']]]]).

rules([[pozhar,2],[
	[1,[_],0,
		[chto,nuzhno,delatjh,ljudjam,chtobih,predotvratitjh,lesnihe,pozharih,'?']]]]).

rules([[blagotvoriteljhnostjh,2],[
	[1,[_],0,
		[lichno,ja,schitaju,spravedlivoy,pogovorku,ne,davay,golodajushhemu,rihbu,',',day,emu,udochku,',',pochemu,vih,ne,soglasnih,ili,soglasnih,'?'],
        [pomogatjh,obezdolennihm,ehto,vesjhma,blagorodno,',',vih,kak,to,pomogaete,'?']]]]).

rules([[net,1],[
	[1,[_],0,
	    [i, dont, got, you]]]]).

rules([[proshhayte,9],[
	[1,[_],0,
	    [spasibo, za, dialog, uvidimsja, pozzhe]]]]).

rules([[prosto,1],[
	[1,[_],0,
	    [net, ehto, ne, prostoy, vopros]]]]).

rules([[znaju,1],[
	[1,[_],0,
	    [ladno, a, chto, vih, eshhe, znaete, pro, rihbalku, '?']]]]).

rules([[okey,1],[
	[1,[_],0,
	    [khorosho, a, chto, vih, eshhe, znaete, o, rihbalke, '?']]]]).

rules([[malo,1],[
	[1,[_],0,
	    [khm, a, davayte, ja, vam, rasskazhu, o, rihbalke, '?', chto, vih, khoteli, bih, uznatjh, '?']]]]).

rules([[ljublju,1],[
	[1,[_],0,
	    [ot, chego, zhe, '?']]]]).

rules([[on,0],[
	[1,[_],0,
		[o, kom, idet, rechjh, '?']]]]).

rules([[memory,0],[
	[1,[_],0,
		[ne, ponjala, povtorite, pogromche]]]]).

rules([[none,0],[
	[1,[_],0,
        [nichego, ne, ponimaju]]]]).

rules([[privet,0],[
	[1,[_],0,
        [privet, chto, dumaeshjh, o, rihbalke, '?']]]]).
