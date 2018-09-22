
:- ensure_loaded( library(pub_graph) ).
:- debug( pub_graph ).

pm_id_alias( kemp, 12075665 ).

info :-
     Names = ['Author','Title','Source','Pages','PubDate',
              'Volume','Issue','ISSN','PmcRefCount','PubType','FullJournalName'],
     pub_graph_summary_info( 12075665, Results, names(Names) ),
     findall( _, (member(R,Results),write(R),nl), _ ).

info( File ) :-
     Names = ['Author','PmcRefCount','Title'],
     Opts = [tmp_file(File),names(Names)],
     pubmed_summary_info( 12075665, Results, Opts ),
     findall( _, (member(R,Results),write(R),nl), _ ).

pubmed :-
     % Id = '11780146',
     Id = '12075665',    % Graham Kemp's  paper
     pubmed_cited_by( Id, Ids ),
     write( cited_by(Ids) ), nl.

doc_1 :-
    St = (journal=science,[breast,cancer],pdat=2008),
    pubmed_search( St, Ids, verbose(true) ),
    length( Ids, Len ), write( number_of:Len ), nl.

author_search( First, Last ) :-
	% usually this is too restrictive.
	% the standard should be to search by last (with infinite returns)
	% and then go through searching
	pub_graph_search( [First,Last], Ids ), 
	length( Ids, Len ), 
	assert( as_all(Ids) ),
	sub_atom( First, 0, 1, _, LowInitial ),
	upcase_atom( LowInitial, Initial ),
	capitol( Last, Lasted ),
	atomic_list_concat( [Lasted,Initial], ' ', Match ),
	SIopts = names('Author'),
	findall( Id, (member(Id,Ids),
	                pub_graph_summary_info(Id,[_-Auths],SIopts),
				 memberchk(Match,Auths)
			   ), Goods ),
	assert( as_good(Goods) ),
	length( Goods, Gen ),
	write( 'Lengths ratio':Gen/Len ), nl.

capitol( Last, Lasted ) :-
	sub_atom( Last, 0, 1, Ren, Lowist ),
	sub_atom( Last, 1, Ren, 0, Right ),
	upcase_atom( Lowist, Initial ),
	atom_concat( Initial, Right, Lasted ).

author_abstracts :-
	%  pub_graph_search( ([jimmy,jacob]), Ids )
	pub_graph_search( author=giamas, Ids ),
	assert( giamas(Ids) ),
	pub_graph_abstracts( Ids, Abs ),
	assert( giamas_abstracts(Abs) ),
	findall( Part, ( member(_-Lns,Abs),
	                 atomic_list_concat(Lns,' ',Part) ), Parts ),
	atomic_list_concat( Parts, Single ),
	assert( giamas_abstraction(Single) ),
	tell( ggiamas.txt ),
	findall( _, (member(_-Lns,Abs),member(Ln,Lns),write(Ln),nl), _ ),
	told.

/*
http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=science\[journal\]+AND+breast+cancer+AND+2008\[pdat\]
process_create(path(curl),[-o,/tmp/pl_25601_1,http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=science\[journal\]+AND+breast+cancer+AND+2008\[pdat\]],[])
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  3008    0  3008    0     0   4592      0 --:--:-- --:--:-- --:--:--  5400
tmp_file(/tmp/pl_25601_1)
ids([19008416,18927361,18787170,18487186,18239126,18239125])
number_of:6
St = (journal=science, [breast, cancer], pdat=2008),
Ids = ['19008416', '18927361', '18787170', '18487186', '18239126', '18239125'],
Len = 6.
*/


doc_2 :-
     date(Date), 
     write( date:Date ), nl,
     pubmed_search( prolog, Ids ),
     length( Ids, Len ), 
     write( number_of:Len ), 
     nl.

/*
number_of:100
Date = date(2012, 7, 10),
Ids = ['22586414', '22462194', '22215819', '21980276', '21499053', '21353661', '20123506', '20123505', '19408879'|...],
Len = 100.
*/

doc_3 :- 
    date(Date), write( date:Date ), nl,
    pubmed_search( prolog, Ids, retmax(200) ),
    length( Ids, Len ), write( number_of:Len ), nl.

/*
number_of:120
Date = date(2012, 7, 10),
Ids = ['22586414', '22462194', '22215819', '21980276', '21499053', '21353661', '20123506', '20123505', '19408879'|...],
Len = 120.
*/

doc_4 :-
     date(Date),  write( date:Date ), nl,
     pubmed_search((programming,'Prolog'), Ids), 
     Ids = [A,B,C|_], 
     pubmed_summary_display( [A,B,C] ).

/*
----
0:22215819
[Evaluating bacterial gene-finding HMM structures as probabilistic logic programs.]
[Mørk S,Holmes I]
----
1:21980276
[War of ontology worlds: mathematics, computer code, or Esperanto?]
[Rzhetsky A,Evans JA]
----
2:15360781
[Medical expert systems developed in j.MD, a Java based expert system shell: application in clinical laboratories.]
[Van Hoof V,Wormek A,Schleutermann S,Schumacher T,Lothaire O,Trendelenburg C]
----
Date = date(2012, 7, 10),
Ids = ['22215819', '21980276', '15360781', '11809317', '9783213', '9293715', '9390313', '8996790', '15048396'|...],
A = '22215819',
B = '21980276',
C = '15360781'.

*/

doc_5 :-
     date(D), write( date:D ), nl, 
     pubmed_cited_by( 12075665, By ),
     write( cited_by(12075665,By) ), nl.

/*    D = date(2012, 7, 9),
      By = ['19497389'].
*/

doc_6 :- 
  date(Date),  Opts = names(['Author','PmcRefCount','Title']),
  pubmed_summary_info( 12075665, Results, Opts ),
  write( date:Date ), nl, member( R, Results ), write( R ), nl, fail.

/*
date:date(2012,7,9)
Author-[Kemp GJ,Angelopoulos N,Gray PM]
Title-[Architecture of a mediator for a bioinformatics database federation.]
Source-[IEEE Trans Inf Technol Biomed]
Pages-[116-22]
PubDate-[2002 Jun]
Volume-[6]
Issue-[2]
ISSN-[1089-7771]
PmcRefCount-[1]
PubType-[Journal Article]
FullJournalName-[IEEE transactions on information technology in biomedicine : a publication of the IEEE Engineering in Medicine and Biology Society]
false.
*/

doc_7 :-
     Id = 20195494,
     pubmed_summary_display( Id ),
     date(D), pubmed_cites( Id, Cites ), length( Cites, Len ), write( D:Len ), nl, fail.

/*
----
0:20195494
[Stochastic model of integrin-mediated signaling and adhesion dynamics at the leading edges of migrating cells.]
[Cirit M,Krajcovic M,Choi CK,Welf ES,Horwitz AF,Haugh JM]
----
date(2012,9,25):35
false.
*/

doc(8) :-
     pm_id_alias( kemp, KG ),
     Opts = [verbose(true),cache(csv)],
     pub_graph_cited_by_graph( KG, Graph, Opts ),
     write( graph(Graph) ), nl.
