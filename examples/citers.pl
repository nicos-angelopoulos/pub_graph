
:- use_module( library(pub_graph) ).

:- debug( citers ).

citers :-
	TarGet = 19274049,
	citers( TarGet ).

citers( TarGet ) :-
	get_cited_by( TarGet, By ),
	length( By, ByLen ),
	pub_graph_summary_info( TarGet, Res, [] ),
	memberchk( 'Title'-Title, Res ),
	debug( citers, 'There are: ~d papers citing: ~d (~w).', [ByLen,TarGet,Title] ),
	get_cited_by_summs( By, TarGet, Summs ),

	pub_graph_summary_display_info( Summs, _ ).

get_cited_by_summs( By, TarGet, Summs ) :-
	sort( By, Order ),
	atomic_list_concat( [cite,TarGet,summs], '_', CsumsPname ),
	Goal =.. [CsumsPname,Summs],
	get_cited_by_summs_pname( CsumsPname, Goal, TarGet, Order, Summs ).

get_cited_by_summs_pname( CsumsPname, Goal, TarGet, _Order, _Summs ) :-
	current_predicate( CsumsPname/1 ),
	debug( citers, 'Using stored summaries of citers of paper: ~d, from predicate: ~w/~d.', [TarGet,CsumsPname,1] ),
	call( Goal ),
	!.
get_cited_by_summs_pname( CsumsPname, Goal, TarGet, Order, Summs ) :-
	pub_graph_summary_info( Order, Summs, [] ), 
	debug( citers, 'Storing summaries of citers of paper: ~d, in predicate: ~w/~d.', [TarGet,CsumsPname,1] ),
	assert( Goal ).

get_cited_by( TarGet, By ) :-
	atomic_list_concat( [cite,TarGet], '_', CitePname ),
	Goal =.. [CitePname,By],
	get_cited_by_pname( CitePname, Goal, TarGet, By ).

get_cited_by_pname( CitePname, Goal, TarGet, _By ) :-
	current_predicate( CitePname/1 ),
	debug( citer, 'Using stored citers of paper: ~d, from predicate: ~w/~d.', [TarGet,CitePname,1] ),
	call( Goal ),
	!.
get_cited_by_pname( CitePname, Goal, TarGet, By ) :-
 	pub_graph_cited_by( TarGet, By ), 
	debug( citer, 'Storing citers of paper: ~d, in predicate: ~w/~d.', [TarGet,CitePname,1] ),
	assert( Goal ).
