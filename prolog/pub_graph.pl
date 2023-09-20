:- module( pub_graph,
               [ 
                    pub_graph_abstracts/2,
                    pub_graph_cited_by/2, pub_graph_cited_by/3,
                    pub_graph_cites/2, pub_graph_cites/3,
                    pub_graph_cache_open/5,
                    pub_graph_cache_save/4,
                    pub_graph_cited_by_graph/3,
                    pub_graph_cited_by_treadmill/3,
                    pub_graph_summary_info/3,
                    pub_graph_search/2, pub_graph_search/3,
                    pub_graph_summary_display/1, pub_graph_summary_display/2,
                    pub_graph_summary_display/3,
                    pub_graph_summary_display_info/2,
                    pub_graph_table/3,
                    pub_graph_id/2,
                    pub_graph_version/2
                   ]
         ).

:- use_module(library(sgml)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).          % ord_add_element/3.
:- use_module(library(http/http_open)).   % http_open/3
:- use_module(library(http/json)).        % json_read/2,3
:- use_module(library(http/html_write) ).

% stoics.org.uk  libs:
:- use_module( library(options) ).

:- dynamic pub_graph_cache:cited_by/3.
:- dynamic pub_graph_cache:info/4.
:- dynamic pub_graph_cache:info_date/2.

/**  <module> Access, cache and visualise citation relations in publications servers.

A simple library for communicating with publication information servers: pub med and semantic scholar.<br>
Currently allows (a) searching on conjunctions and disjunctions, (b) fetching the details of a paper <br>
(c) the publications citing a paper, (d) publications cited by a paper, (e) simple reporting of fetched information
and (f) storing fethed information to local databases.

Since version 0.1 the library supports caching of the paper information on Prolog term or csv data files<br>
and  odbc connected or sqlite databases. Also as of 0.1 pub_graph is debug/1 aware. To see information regarding<br>
the progress of execution, use

==
    ?- debug(pub_graph). 
==

The pack requires the curl executable to be in the path. Only tested on Linux.<br>
It is being developed on SWI-Prolog 6.1.8 and it should also work on Yap Prolog.

To install under SWI simply do 

==
    ?- pack_install(pub_graph).
    % and load with
    ?- use_module(library(pub_graph)).
==

The storing of paper and citation depends on db_facts and for sqlite connectivity
on proSQlite (both available as SWI packs and from http://stoics.org.uk/~nicos/sware/)

@version 0.1.0 2014/7/22 (was pubmed)
@version 1.0   2018/9/22
@version 1.1   2018/9/23, wrap/hide caching libs errors
@version 1.2   2023/9/20, added Gap, much better search for title + affiliation + title/abstract
@license MIT
@author Nicos Angelopoulos
@see http://stoics.org.uk/~nicos/sware/pub_graph
@see http://www.ncbi.nlm.nih.gov/books/NBK25500/
@see http://api.semanticscholar.org
@see files in examples/ directory
@see sources at http://stoics.org.uk/~nicos/sware/pub_graph/
@tbd  currently the info tables are wastefull in the interest of simplicity. 
     Eg they are of the form info(ID,Key,O,Value). 
     But Key is really a type of information.  so we could split this to a number of tables
          =|(info:)key(Id,O,Value)|=. Alternatively you could make
          key an enumerate type, which will save loads of space

*/

% This does (no longer ?) work: [try abs_fname/n]
:- ( catch(use_module(library(prosqlite)),_,fail) -> true
     ;  
     debug(pub_graph, 'proSQLite not available. Caching through prosqlite disabled', [] ) 
   ).

:- ( catch(use_module(library(odbc)),_,fail) -> true
     ;  
     debug(pub_graph, 'proSQLite not available. Caching through odbc disabled', [] ) 
   ).
:- ( catch(use_module(library(db_facts)),_,fail) -> true
     ;  
     debug(pub_graph, 'pack(db_facts) not available. Caching is disabled', [] ) 
   ).

% Section: defaults, shortcuts.

file_type_extension(csv, csv).
file_type_extension(prolog, pl).
file_type_extension(sqlite, sqlite).

url_eutils('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/').
url_efetch_pubmed('efetch.fcgi?db=pubmed').

url_semscholar('http://api.semanticscholar.org/v1/paper/').

default_names( Names ) :- 
    default_names( ncbi, Names ).
default_names( semscholar, Names ) :-
    Names = [arxivId,authors,doi,title,topics,venue,year].
    % citationVelocity, citations, influentialCitationCount, paperId, references, url
default_names( ncbi, Names ) :-
     Names = ['Author','Title','Source','Pages','PubDate',
              'Volume','Issue','ISSN','PmcRefCount',
              'PubType','FullJournalName'].

pub_graph_graph(true).  % needed for options_append/3
pub_graph_graph_defaults( [depth(0),verbose(false),update(true),date(AgesAgo),flat(false)] ) :-
     a_month_ago( AgesAgo ).

/** pub_graph_id( +Id, -IdType ).

True if Id corresponds to a paper identifier from server typed by IdType.<br>
Currently =ncbi= (https://www.ncbi.nlm.nih.gov/pubmed/) and =semscholar= (http://semanticscholar.org/) are the known IdTypes.

The predicate does not connect to the server, it only type checks the shape of Id.<br>
If Id is an integer or an atom that can be turned to an integer, then IdType is instantiated to =ncbi=.<br>
There are three term forms for =semscholar=.
  * hex
     such as cbd251a03b1a29a94f7348f4f5c2f830ab80a909
  * doi
    presented as, doi:'10.1109/TITB.2002.1006298' (doi is stripped before request is posted)
  * arXiv
     as, arXiv:1705.10311 (arXiv forms part of the semanticscholar.org request)
    

The following two ids correspond to the same paper.

==
?- 
    pub_graph_id( 12075665, Type ).

Type = ncbi.

?-
    pub_graph_id( cbd251a03b1a29a94f7348f4f5c2f830ab80a909, Type ).

Type = semscholar.
==

@author nicos angelopoulos
@version  0.1 2018/9/11
@see https://www.ncbi.nlm.nih.gov/pubmed/
@see http://semanticscholar.org

*/
pub_graph_id( Id, IdType ) :-
    ( integer(Id) ; (catch(atom_number(Id,Numb),_,fail),integer(Numb)) ),
    !,
    IdType = ncbi.
pub_graph_id( Term, IdType ) :-
    (
        catch( hex_bytes(Term,_Bytes), _, fail )
        ;
        Term = doi:_
        ;
        Term = arXiv
    ),
    !,
    IdType = semscholar.

% Section: interface predicates

/** pub_graph_version( +Version, +Date ).

Get version information and date of publication.

==
?-
    pub_graph_version(V,D).

V = 1:2:0,
D = date(2023, 9, 20).
==

*/
pub_graph_version( 1:2:0, date(2023,9,20) ).
% pub_graph_version( 1:1:0, date(2018,9,23) ).
% pub_graph_version( 1:0:0, date(2018,9,22) ).
% pub_graph_version( 0:0:3, date(2012,08,15) ).

pub_graph_search_defaults( Defs ) :- 
               Defs = [
                        gap(0),
                        retmax(100),
                        quote_value(true),
                        tmp_keep(false),
                        verbose(false)
                        ].

/** pub_graph_search( +STerm, -Ids ).
    pub_graph_search( +STerm, -Ids, +Options ).

This is currently only implemented for ncbi ids as there is no means for searching
in the semantic scholar API.

Search in pub_graph for terms in the search term =STerm=.
In this, conjunction is marked by _|,|_ (comma) and 
disjunction by _|;|_ (semi-column). '-' pair terms are considered as 
Key-Value and interpreted as Value[Key] in the query.
List are thought to be flat conjoint search terms  with no pair values in them which are 
interpreted by pub_graph also as OR operations.
(See example below.)
Known keys are : =journal=, =pdat=. =au=, =|All Fields|=
The predicate constructs a query that is posted via the http API provided
by NCBI (http://www.ncbi.nlm.nih.gov/books/NBK25500/).
  
Options should be a term or list of terms from: 
  * gap(Gap=0)
    Gap allowed for approximate reasoning in =ncbi= terms: Title, Title/Abstract and Affiliation. 
    The higher the number the looser the match. The default allows for no intervening words, so only
    exact sub-matches will be returned (see example: fixme below)
    see: https://pubmed.ncbi.nlm.nih.gov/help/#proximity-searching
  * maxdate(Xdat)
    see mindate Option
    For instance, taking an example from the url we show how to find 
    all breast cancer articles that were published in Science in 2008.
  * mindate(Ndat)
    Date range used to limit a link operation by the date specified by datetype. 
    These two parameters (mindate, maxdate) must be used together to specify an arbitrary date range. 
    The general date format is YYYY/MM/DD, and these variants are also allowed: YYYY, YYYY/MM.
  * qtranslation(QTrans) 
    return in =QTrans= the actual query ran on the 
    the pub_graph server.
  * quote_value(Qv=true)
    whether to quote values of K=V search terms
  * reldate(Rdat)
    When reldate is set to an integer n, ELink returns only those items 
    that have a date specified by datetype within the last n days.
  * retmax(RetMax)
    the maximum number of records that will be returned def: 100
  * tmp_file(Tmp)  
    file to use, or when =Tmp= is variable the file that was used
    to receive the results from pub_graph.
  * tmp_keep(Keep) 
    keep the file with the xml result iff =|Keep==true|=
  * verbose(Verbose) 
    if =|Verbose == true|= then the predicate is verbose about its progress by,
    for instance, requesting query is printed on current output stream.

==
?-
    St = (journal=science,[breast,cancer],pdat=2008),
    pub_graph_search( St, Ids, [verbose(true),qtranslation(QTrans)] ),
    length( Ids, Len ), write( number_of:Len ), nl,
    pub_graph_summary_display( Ids, _, display(all) ).

https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmax=100&term=science[journal]+AND+breast+cancer+AND+2008[pdat]
tmp_file(/tmp/swipl_3884_9)
number_of:6
----
1:19008416
	Author=[Varambally S,Cao Q,Mani RS,Shankar S,Wang X,Ateeq B,Laxman B,Cao X,Jing X,Ramnarayanan K,Brenner JC,Yu J,Kim JH,Han B,Tan P,Kumar-Sinha C,Lonigro RJ,Palanisamy N,Maher CA,Chinnaiyan AM]
	Title=Genomic loss of microRNA-101 leads to overexpression of histone methyltransferase EZH2 in cancer.
	Source=Science
	Pages=1695-9
	PubDate=2008 Dec 12
	Volume=322
	Issue=5908
	ISSN=0036-8075
	PmcRefCount=352
	PubType=Journal Article
	FullJournalName=Science (New York, N.Y.)
----
2:18927361
	Author=Couzin J
	Title=Genetics. DNA test for breast cancer risk draws criticism.
	Source=Science
...
...
...
6:18239125
	Author=[Silva JM,Marran K,Parker JS,Silva J,Golding M,Schlabach MR,Elledge SJ,Hannon GJ,Chang K]
	Title=Profiling essential genes in human mammary cells by multiplex RNAi screening.
	Source=Science
	Pages=617-20
	PubDate=2008 Feb 1
	Volume=319
	Issue=5863
	ISSN=0036-8075
	PmcRefCount=132
	PubType=Journal Article
	FullJournalName=Science (New York, N.Y.)
----
St =  (journal=science, [breast, cancer], pdat=2008),
Ids = ['19008416', '18927361', '18787170', '18487186', '18239126', '18239125'],
QTrans = ['("Science"[Journal] OR "Science (80- )"[Journal] OR "J Zhejiang Univ Sci"[Journal]) AND ("breast neoplasms"[MeSH Terms] OR ("breast"[All Fields] AND "neoplasms"[All Fields]) OR "breast neoplasms"[All Fields] OR ("breast"[All Fields] AND "cancer"[All Fields]) OR "breast cancer"[All Fields]) AND 2008[pdat]'],
Len = 6.

?- 
     date(Date),
     St = (author='Borst Piet'),
     pub_graph_search( St, Ids, verbose(true) ),
     length( Ids, Len ), write( number_of:Len ), nl.

https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&retmax=100&term=Borst%20Piet\[author\]
tmp_file(/tmp/swipl_18703_0)
number_of:83
Date = date(2018, 9, 22),
St =  (author='Borst Piet'),
Ids = ['29894693', '29256493', '28821557', '27021571', '26774285', '26530471', '26515061', '25799992', '25662217'|...],
Len = 83.

?-
    date(Date), pub_graph_search( prolog, Ids ), 
    length( Ids, Len ), write( number_of:Len ), nl.

number_of:100
Date = date(2018, 9, 22),
Ids = ['30089663', '28647861', '28486579', '27684214', '27142769', '25509153', '24995073', '22586414', '22462194'|...],
Len = 100.

?- 
    date(Date), pub_graph_search( prolog, Ids, retmax(200) ),
    length( Ids, Len ), write( number_of:Len ), nl.

number_of:127
Date = date(2018, 9, 22),
Ids = ['30089663', '28647861', '28486579', '27684214', '27142769', '25509153', '24995073', '22586414', '22462194'|...],
Len = 127.

?-
   St = ('breast','cancer','Publication Type'='Review'),
   date(Date), pub_graph_search( St, Ids, reldate(30) ),
   length( Ids, Len ).

Date = date(2018, 9, 22),
Ids = ['30240898', '30240537', '30240152', '30238542', '30238005', '30237735', '30236642', '30236594', '30234119'|...],
Len = 100.

?- 
    pub_graph_summary_display( 30243159, _, true ).
----
1:30243159
	Author=[Wang K,Yee C,Tam S,Drost L,Chan S,Zaki P,Rico V,Ariello K,Dasios M,Lam H,DeAngelis C,Chow E]
	Title=Prevalence of pain in patients with breast cancer post-treatment: A systematic review.
----
true.
==

Version 0:3 (pub_graph_version(1:2:0,_D)).
==
?- 
     date(Date), pub_graph_search(title='Bayesian networks elucidate', Ids, true), length(Ids,Len).
Date = date(2023, 9, 20),
Ids = ['35379892'],
Len = 1.

?- 
     date(Date), pub_graph_search(title='Bayesian elucidate', Ids, true), length(Ids,Len).

Date = date(2023, 9, 20),
Ids = [],
Len = 0.

?- 
     date(Date), pub_graph_search(title='Bayesian elucidate', Ids, gap(1)), length(Ids, Len),  pub_graph_summary_display(Ids, _, true).

----
1:35379892
	Author=[Angelopoulos N,Chatzipli A,Nangalia J,Maura F,Campbell PJ]
	Title=Bayesian networks elucidate complex genomic landscapes in cancer.
----
Date = date(2023, 9, 20),
Ids = ['35379892'],
Len = 1.

?-
     date(D),
     write('Appears in abstract: "explainable Artificial Intelligence models"'), nl,
     pub_graph_search('Title/Abstract'='explainable Artificial Intelligence models', Ids, true),
     pub_graph_summary_display(Ids).

1
...
10:32417928
	Author=[Payrovnaziri SN,Chen Z,Rengifo-Moreno P,Miller T,Bian J,Chen JH,Liu X,He Z]
	Title=Explainable artificial intelligence models using real-world electronic health record data: a systematic scoping review.

?- 
     date(D), pub_graph_search('Title/Abstract'='explainable Intelligence models', Ids, true).

D = date(2023, 9, 20),
Ids = [].

?- 
     date(D), pub_graph_search((tiab='explainable Intelligence models',affiliation=sanger), Ids, gap(1)).

D = date(2023, 9, 20),
Ids = ['35379892'].
==

Also 0:3 added quote_value(Qv). Compare:
==
?- date(Date), pub_graph_search(title='Bayesian networks elucidate', Ids, true), length(Ids,Len).
Date = date(2023, 9, 20),
Ids = ['35379892'],
Len = 1.

?- date(Date), pub_graph_search(title='Bayesian networks elucidate', Ids, quote_value(false)), length(Ids,Len).
Date = date(2023, 9, 20),
Ids = ['35923659', '35379892', '32609725', '29055062', '27303742', '26362267'],
Len = 6.
==

@author nicos angelopoulos
@version  0:1 2012/07/15
@version  0:2 2018/09/22, small update on \ escape on eutils, ncbi, queries
@version  0:3 2023/09/20, added Gap, much better search for title + affiliation + title/abstract

*/

pub_graph_search( STerm, Ids ) :-
     pub_graph_search( STerm, Ids, [] ).

pub_graph_search( Sterm, Ids, Args ) :-
    options_append( pub_graph_search, Args, Opts ),
    url_eutils( Eutils ),
    ( ground(Sterm) -> true; type_error(ground,Sterm) ),
    options( gap(Gap), Opts ),
    options( quote_value(Qv), Opts ),
    search_term_to_query( Sterm, Gap, Qv, Query ),
    memberchk( retmax(Ret), Opts ), 
    pub_graph_search_period_opts( '', Period, Opts ),
    atomic_list_concat( [Eutils,'esearch.fcgi?db=pubmed&retmax=',Ret,Period,'&term=',Query], Url ),
    memberchk_optional( tmp_file(Tmp), Opts ),
    memberchk( verbose(Verb), Opts ),
    true_writes( Verb, Url ),
    debug( pub_graph, 'Get url is: ~w', Url ),
    get_url_in_file( Url, Verb, Tmp ),
    true_writes( Verb, tmp_file(Tmp) ),
    load_xml_file( Tmp, Xml ),
    ( (memberchk(qtranslation(QTrans),Opts),
        QT = 'QueryTranslation',
        search_element_in_list(Xml,QT,[],element(_,_,QTrans))) -> true; true
    ),
    all_subs_in_xml_single( Xml, 'IdList', 'Id', NastyIds ),
    flatten( NastyIds, Ids ),
    memberchk_optional( tmp_keep(Keep), Opts ),
    true_atom_keeps_file( Keep, Tmp ).

/** pub_graph_summary_display( +Ids ).

     Short for =|pub_graph_summary_display( Ids, _Summary, [] ).|=
     
*/
pub_graph_summary_display( Ids ) :-
     pub_graph_summary_display( Ids, _Summary, [] ).

/** pub_graph_summary_display( +Ids, -Summary ).

     Short for =|pub_graph_summary_display( Ids, Summary, [] ).|=
*/
pub_graph_summary_display( Ids, Summary ) :-
     pub_graph_summary_display( Ids, Summary, [] ).

% pub_graph_summary_display_defaults( [display(['Title','Author']),names(Names)] ) :-
%     default_names( Names ).
pub_graph_summary_display_defaults( [display([title,'Title',authors,'Author'])] ).

/** pub_graph_summary_display( +IdS, -Summaries, +Opts ).

A wrapper around pub_graph_summary_info/3. It call this predicate with same arguments before 
displaying the Summary information. Opts can be a single term option or a list of such terms.
In addition to pub_graph_summary_info/3 options this wrapper also recognises the term: 

Opts
  * display(Disp) 
     a list of article information keys that will displayed one on a line for each Id in =Ids=.
     Disp values of var(Disp), '*' and 'all', list all available values.

==
?-  
    date(Date), 
    pub_graph_search((programming,'Prolog'), Ids), 
    length( Ids, Len), 
    Ids = [A,B,C|_], pub_graph_summary_display( [A,B,C] ).

----
1:28486579
    Author=[Holmes IH,Mungall CJ]
    Title=BioMake: a GNU make-compatible utility for declarative workflow management.
----
2:24995073
    Author=[Melioli G,Spenser C,Reggiardo G,Passalacqua G,Compalati E,Rogkakou A,Riccio AM,Di Leo E,Nettis E,Canonica GW]
    Title=Allergenius, an expert system for the interpretation of allergen microarray results.
----
3:22215819
    Author=[Mørk S,Holmes I]
    Title=Evaluating bacterial gene-finding HMM structures as probabilistic logic programs.
----
Date = date(2018, 9, 22),
Ids = ['28486579', '24995073', '22215819', '21980276', '15360781', '11809317', '9783213', '9293715', '9390313'|...],
Len = 43.
A = '28486579',
B = '24995073',
C = '22215819'.
==

==
?- 
    pub_graph_summary_display( 30235570, _, display(*) ).

----
1:30235570
    Author=[Morgan CC,Huyck S,Jenkins M,Chen L,Bedding A,Coffey CS,Gaydos B,Wathen JK]
    Title=Adaptive Design: Results of 2012 Survey on Perception and Use.
    Source=Ther Innov Regul Sci
    Pages=473-481
    PubDate=2014 Jul
    Volume=48
    Issue=4
    ISSN=2168-4790
    PmcRefCount=0
    PubType=Journal Article
    FullJournalName=Therapeutic innovation & regulatory science
----
==

==
?-
     pub_graph_cited_by( 20195494, These ), 
     pub_graph_summary_display( These, _, [display(['Title','Author','PubDate'])] ).

----
1:29975690
    Author=[Tang K,Boudreau CG,Brown CM,Khadra A]
    Title=Paxillin phosphorylation at serine 273 and its effects on Rac, Rho and adhesion dynamics.
    PubDate=2018 Jul
----
2:29694862
    Author=[McKenzie M,Ha SM,Rammohan A,Radhakrishnan R,Ramakrishnan N]
    Title=Multivalent Binding of a Ligand-Coated Particle: Role of Shape, Size, and Ligand Heterogeneity.
    PubDate=2018 Apr 24
----
3:29669897
    Author=[Padmanabhan P,Goodhill GJ]
    Title=Axon growth regulation by a bistable molecular switch.
    PubDate=2018 Apr 25
...
...
26:20473365
    Author=[Welf ES,Haugh JM]
    Title=Stochastic Dynamics of Membrane Protrusion Mediated by the DOCK180/Rac Pathway in Migrating Cells.
    PubDate=2010 Mar 1
----
These = [29975690, 29694862, 29669897, 28752950, 27939309, 27588610, 27276271, 25969948, 25904526|...].


?- 
    pub_graph_summary_display( 20195494, _Res, true ).

----
1:20195494
    Author=[Cirit M,Krajcovic M,Choi CK,Welf ES,Horwitz AF,Haugh JM]
    Title=Stochastic model of integrin-mediated signaling and adhesion dynamics at the leading edges of migrating cells.
----
true.

?- 
    pub_graph_summary_display( cbd251a03b1a29a94f7348f4f5c2f830ab80a909, _, display(all) ).

----
1:cbd251a03b1a29a94f7348f4f5c2f830ab80a909
	arxivId=[]
	authors=[Graham J. L. Kemp,Nicos Angelopoulos,Peter M. D. Gray]
	doi=10.1109/TITB.2002.1006298
	title=Architecture of a mediator for a bioinformatics database federation
	topics=[]
	venue=IEEE Transactions on Information Technology in Biomedicine
	year=2002
----
true.

==

*/

pub_graph_summary_display( IdS, Summary, Args ) :-
    % pub_graph_summary_display_defaults( Defs ),
    % options_append( Opts, Defs, All ),
    non_var_list( IdS, Ids ),
    options_append( pub_graph_summary_display, Args, Opts ),
    options( display(Disp), Opts ),
    pub_graph_summary_info( Ids, Summary, Opts ),
    pub_graph_summary_display_info( Summary, Disp ).

%% pub_graph_summary_display_info( +Summaries, +Entries ).
%
%  Display the Entries information for Summaries, which should be a list of summaries.
%  If Entries is a variable all info will be printed.
%
pub_graph_summary_display_info( SummaryIn, Disp ) :-
     pg_en_list( SummaryIn, Summary ),
     write( '----' ), nl,
     nth1( N, Summary, Id-Rec ),
     write( N:Id ), nl,
     findall( _, (member(D-Val,Rec),once((var(Disp);Disp=all;Disp='*';member(D,Disp))),put(0'\t), write(D=Val),nl), _ ), 
     write( '----' ), nl, fail.
pub_graph_summary_display_info( _Summary, _Disp ).

%
% Redirects to pub_graph_cited_by( Id, Ids, [] ).
%
pub_graph_cited_by( Id, Ids) :-
     pub_graph_cited_by( Id, Ids, [] ).

/** pub_graph_cited_by( +Id, -Ids ).
    pub_graph_cited_by( +Id, -Ids, +Options ). 

Ids is the list of pub_graph ids that cite Id. 

Options is a term option or list of terms from the following;

  * verbose(Verb=false)
     be verbose

  * cache(Type,Handle,Date,Update)   
     use cache with Handle and Type, cutting off 
     cached items that are (strictly) older than Date. For Update = =|true|=
     update the cache if you do an explicit retrieval.

==
?-
     date(D), pub_graph_cited_by( 12075665, By ), length( By, Len ).

D = date(2018, 9, 22),
By = [25825659, 19497389, 19458771],
Len = 3.

?- 
    date(D), pub_graph_cited_by( cbd251a03b1a29a94f7348f4f5c2f830ab80a909, By ), length( By, Len ).

D = date(2018, 9, 22),
By = ['2e1f686c2357cead711c8db034ff9aa2b7509621', '6f125881788967e1eec87e78b3d2db61d1a8d0ac'|...],
Len = 12.
==

*/  
pub_graph_cited_by( Id, Ids, Args ) :-
     options_append( pub_graph_graph, Args, Opts ),
     pub_graph_cited_by_1( Id, Ids, Opts ).

pub_graph_cited_by_1( Id, Ids, Opts ) :-
     ( ( memberchk(cache(Type,Handle,Date,_Upd), Opts),
         pub_graph_date_cached(Type,Handle,cited_by,Date,Id,Ids) ) ->
          ( memberchk(verbose(true),Opts) ->
               write( got_from_cache(Id,Ids) ), nl
               ;
               true
          )
          ;
          pub_graph_cited_by_uncached( Id, Ids, Opts ),
          ( (memberchk(cache(Type,Handle,_Date,Upd),Opts),Upd==true) ->
               pub_graph_update_cache( Type, Handle, cited_by, Id, Ids )
               ;
               true
          )
     ).

pub_graph_cited_by_uncached( Id, Ids, Opts ) :-
    pub_graph_id( Id, IdType ),
    pub_graph_cited_by_uncached( IdType, Id, Ids, Opts ).

pub_graph_cited_by_uncached( semscholar, Id, Ids, _Verb ) :-
    semscholar_id_json( Id, Json ),
    memberchk( citations=Citations, Json ),
    findall( ACit, (    member(json(Sub),Citations),
                        member(paperId=ACit,Sub),
                        ACit \== ''
                   ),
                        Ids ).
pub_graph_cited_by_uncached( ncbi, Id, Ids, Opts ) :-
     ( memberchk(verbose(Verb),Opts) -> true; Verb = false ),
     url_eutils( Eutils ),
     Query = 'elink.fcgi?report=xml&mode=text&tool=curl&db=pubmed&cmd=neighbor&linkname=pubmed_pubmed_citedin&id=',
     atomic_list_concat( [Eutils,Query,Id], Url ),
     get_url_in_file( Url, Verb, Tmp ),
     ( pub_graph_cited_by_parse_file(Tmp, Ids) -> 
          true
          ;
          Ids = []
     ),
     delete_file( Tmp ).

%% pub_graph_id_journal( +Id, +Provisional, -Journal ).
%
% 
% Allows for "correcting" Journal entries of Id. Journal is Provisional, except 
% if user:id_has_journal/2 exists and id_has_journal( Id, Jounal ) succeeds (det.)
% 
pub_graph_id_journal( IdPrv, _Provisional, Journal ) :-
    current_predicate( user:id_has_journal/2 ),
    to_number( IdPrv, Id ),
    user:id_has_journal( Id, Journal ),
    !.
pub_graph_id_journal( _Id, Journal, Journal ).

%
% Redirects to pub_graph_cites( Id, Ids, [] ).
%
pub_graph_cites( Id, Ids ) :-
     pub_graph_cites( Id, Ids, [] ).

/** pub_graph_cites( +Id, -Ids ).
    pub_graph_cites( +Id, -Ids, +Options ). 

Ids is the list of pub_graph Ids (pub_graph_id/2) that are cited by Id. 

Options is a term option or list of terms from the following;
  * verbose(Verb)
     be verbose

==
?-
    date(D), 
    pub_graph_cites( 20195494, Ids ), 
    length( Ids, Len ), write( D:Len ), nl.

date(2018,9,22):38
D = date(2018, 9, 22),
Ids = ['19160484', '19118212', '18955554', '18800171', '18586481'|...],
Len = 38.

% pubmed does not have references cited by the following paper:

?- 
    date(D), 
    pub_graph_cites( 12075665, Ids ), 
    length( Ids, Len ), write( D:Len ), nl.

false.

% whereas, semanticscholar.org finds 17 (non '') of the 21:
?- 
    date(D), 
    pub_graph_cites( cbd251a03b1a29a94f7348f4f5c2f830ab80a909, Ids ), 
    length( Ids, Len ), write( D:Len ), nl.

date(2018,9,22):17
D = date(2018, 9, 22),
Ids = ['6477792829dd059c7d318927858d307347c54c2e', '1448901572d1afd0019c86c42288108a94f1fb25', |...],
Len = 17.

?- 
    pub_graph_summary_display( 12075665, Results, true ).

----
1:12075665
    Author=[Kemp GJ,Angelopoulos N,Gray PM]
    Title=Architecture of a mediator for a bioinformatics database federation.
----
Results = [12075665-['Author'-['Kemp GJ', 'Angelopoulos N', 'Gray PM'], ... - ...|...]].
==

@author nicos angelopoulos
@version  0:1 2018/9/22
@see pub_graph_id/2

*/
pub_graph_cites( Id, Ids, OptsIn ) :-
    non_var_list( OptsIn, Opts ),
    pub_graph_id( Id, IdType ),
    ( memberchk(verbose(Verb),Opts) -> true; Verb = false ),
    pub_graph_id_type_cites( IdType, Id, Ids, Verb ).

pub_graph_id_type_cites( ncbi, Id, Ids, Verb ) :-
     url_eutils( Eutils ),
     % Query = 'elink.fcgi?report=xml&mode=text&tool=curl&db=pmc&DbFrom=pubmed&Cmd=link&linkname=pubmed_pmc_refs&id=',
     Query = 'elink.fcgi?report=xml&mode=text&tool=curl&db=pubmed&Cmd=neighbor&linkname=pubmed_pubmed_refs&id=',
     atomic_list_concat( [Eutils,Query,Id], Url ),
     get_url_in_file( Url, Verb, Tmp ),
     load_xml_file( Tmp, Xml ),
     once( search_element_in_list( Xml, 'LinkSetDb', [], element(_,_,LXml) ) ),
     findall( CId, search_element_in_list(LXml,'Id',[],element(_,_,[CId])),Ids ),
     delete_file( Tmp ).
pub_graph_id_type_cites( semscholar, Id, Ids, _Verb ) :-
    semscholar_id_json( Id, Json ),
    memberchk( references=Citations, Json ),
    findall( ACit, (    member(json(Sub),Citations),
                        member(paperId=ACit,Sub),
                        ACit \== ''
                   ),
                        Ids ).

pub_graph_table_defaults( Defs ) :-
    NoSearch = 'No search term available',
    Defs = [ include_if(false), output(csv), stem([]), spy([]), search(NoSearch) ].

/** pub_graph_table( +Ids, -Rows, +Opts ).

Create a table of information relating to IDs.

Can include journal impact factor if jif/6 is provided.

Output rows contain #citing, [IF ,] Date, Journal, Title, Author, (Title urled to pubmed/$id)

Opts
  * include_if(IF=false)
     whether to include Impact Factor (IF) column (if true requires jif/6)

  * missing_if(MIF=throw)
     what to do when a journal has no impact factor: [throw,has(Val),quite(Val)]

  * output(Type=html)
     type of output, if file is expected (see stem), in [csv,?pdf?,html]

  * search(Search='No search term available')
     search term corresponding to the Ids

  * spy(Spy=[])
     A number of ids to spy (list of atoms)

  * stem(Stem)
     when present a file <Stem>.<Type> is created

==
     ?- 
     pub_graph_table
==

@author nicos angelopoulos
@version  0:1 2018/9/20

*/
pub_graph_table( Ids, Rows, Args ) :-
    options_append( pub_graph_table, Args, Opts ),
    OTerms = [include_if(Iif),missing_if(Mif),output(Form),stem(Stem),spy(SpY)],
    options( OTerms, Opts ),
    pg_en_list( SpY, Spy ),
    % ground( Iif ), % i think options checks.
    maplist( pub_graph_table_id_row(Iif,Mif,Spy), Ids, Rows ),
    options( search(Search), Opts ),
    pub_graph_table_output( Stem, Form, Iif, Search, Rows ).

pub_graph_table_id_row( Iif, Mif, Spy, Id, Row ) :-
    ( memberchk(Id,Spy) -> trace; true ),
    pub_graph_table_id_main_args( Id, Journal, Args ), 
    pub_graph_table_iif( Iif, Mif, Args, Journal, Irgs ),
    Row =.. [row|Irgs].

pub_graph_table_id_main_args( Id, Journal, Args ) :-
    pub_graph_table_id_main_args_all( Id, Journal, Args ),
    !.
pub_graph_table_id_main_args( Id, Journal, Args ) :-
    throw( could_not_find_all_info_for_table_row_of(Id,Journal,Args) ).

pub_graph_table_id_main_args_all( Id, Journal, Args ) :-
    pub_graph_summary_info( Id, Summ, [] ),
    memberchk( 'PmcRefCount'-PRCount, Summ ),
    memberchk( 'FullJournalName'-JournalPrv, Summ ),
    memberchk( 'Title'-Title, Summ ),
    memberchk( 'Author'-AuthorS, Summ ),
    pg_en_list( AuthorS, Authors ),
    atomic_list_concat( Authors, ', ', Author ),
    memberchk( 'PubDate'-Pdate, Summ ),
    pub_graph_id_journal( Id, JournalPrv, JournalPrv2 ),
    ( pub_graph_journal_synonym(JournalPrv2,Journal) -> true; Journal = JournalPrv2 ),
    Args = [PRCount,Pdate,Journal,Title,Author,Id].

pub_graph_table_iif( false, _Mif, Args, _J, Args ).
pub_graph_table_iif( true, Mif, [A|Rgs], J, [A,Jif|Rgs] ) :-
    downcase_atom( J, DownJ ),
    pub_graph_table_jif( DownJ, J, Mif, Jif ).

pub_graph_table_jif( DownJ, _J, _Mif, Jif ) :-
    user:jif( _, DownJ, _, JifGot, _, _ ),
    ( number(JifGot) -> JifGot = Jif; Jif is 0 ),
    !.
pub_graph_table_jif( DownJ, _J, _Mif, Jif ) :-
    pub_graph_jif_alternative( DownJ, AlteJ ),
    user:jif( _, AlteJ, _, JifGot, _, _ ),
    ( number(JifGot) -> JifGot = Jif; Jif is 0 ),
    !.
pub_graph_table_jif( DownJ, _J, _Mif, Jif ) :-
    missing_jif( DownJ, Jif ),
    !.
pub_graph_table_jif( _DownJ, J, Mif, Jif ) :-
    pub_graph_table_jif_missing( Mif, J, Jif ),
    !. % i dont think the above leaves b.points

pub_graph_table_jif_missing( has(Val), J, Val ) :-
    debug( pub_graph, 'No IF for: ~w', J ).
pub_graph_table_jif_missing( quiet(Val), _J, Val ).
pub_graph_table_jif_missing( throw, J, _Jif ) :-
    throw( no_jif_for_journal(J) ).

pub_graph_jif_alternative( Jname, Alternative ) :-
    member( Mfx, [', ',' : ',' and ','. '] ),
    atom_length( Mfx, Len ),
    sub_atom( Jname, Bef, Len, Aft, Mfx ),
    sub_atom( Jname, 0, Bef, _, Pfx ), 
    ToAft is Bef + Len,
    sub_atom( Jname, ToAft, Aft, 0, Psf ),
    member( Nfx, ['-',' ',' & '] ),
    atomic_list_concat( [Pfx,Nfx,Psf], Alternative ).
pub_graph_jif_alternative( Jname, Alternative ) :-
    member( Bp, [' : ','; '] ),
    atom_length( Bp, Len ),
    sub_atom( Jname, Bef, Len, _Aft, ' : ' ),
    sub_atom( Jname, 0, Bef, _, Alternative ).
pub_graph_jif_alternative( Jname, Alternative ) :-
    atom_concat( 'the ', Alternative, Jname ).

missing_jif( metabolites, 0 ).
missing_jif( 'genomics, proteomics & bioinformatics', 0 ).
missing_jif( 'frontiers in pharmacology', 0 ). % http://www.frontiersin.org/news/Frontiers_Impact_Factors_2013/875
missing_jif( 'advances in experimental medicine and biology', 2.012 ).  % http://www.springer.com/series/5584
missing_jif( 'expert review of clinical pharmacology', -2 ). % don't think this has IF in general
missing_jif( 'dental research journal', -2 ). % don't think this has IF in general
missing_jif( 'brain and nerve = shinkei kenkyu no shinpo', -2 ). % only in Japanese
missing_jif( 'critical reviews in biomedical engineering', -2 ). % discontinued i think
missing_jif( 'yao xue xue bao = acta pharmaceutica sinica', -2 ). % in Chinese
missing_jif( 'gan to kagaku ryoho. cancer & chemotherapy', -2 ). % in Japanese
missing_jif( 'chinese journal of cancer', 0 ). % http://www.researchgate.net/journal/1944-446X_Chinese_journal_of_cancer
missing_jif( 'seminars in oncology nursing', -2 ). % http://www.researchgate.net/journal/1878-3449_Seminars_in_Oncology_Nursing
missing_jif( 'critical reviews in oncogenesis', -2 ). % http://www.researchgate.net/journal/0893-9675_Critical_reviews_in_oncogenesis
missing_jif( 'praxis', -2 ). % in German


pub_graph_journal_synonym( 'Cellular and molecular life sciences : CMLS', 'CELLULAR AND MOLECULAR LIFE SCIENCES' ).
pub_graph_journal_synonym( 'Tumour biology : the journal of the International Society for Oncodevelopmental Biology and Medicine', 'TUMOR BIOLOGY' ). % http://www.ncbi.nlm.nih.gov/nlmcatalog/8409922
pub_graph_journal_synonym( 'American journal of physiology. Heart and circulatory physiology', 'AMERICAN JOURNAL OF PHYSIOLOGY-HEART AND CIRCULATORY PHYSIOLOGY' ). % http://www.ncbi.nlm.nih.gov/nlmcatalog/8409922
pub_graph_journal_synonym( 'The journals of gerontology. Series A, Biological sciences and medical sciences', 'JOURNALS OF GERONTOLOGY SERIES A-BIOLOGICAL SCIENCES AND MEDICAL SCIENCES' ).
pub_graph_journal_synonym( 'Proteomics. Clinical applications', 'Proteomics Clinical applications' ).
pub_graph_journal_synonym( 'Future oncology (London, England)', 'Future Oncology' ).
pub_graph_journal_synonym( 'The oncologist', 'Oncologist' ).
pub_graph_journal_synonym( 'Cancer control : journal of the Moffitt Cancer Center', 'Cancer Control' ).
pub_graph_journal_synonym( 'Breast (Edinburgh, Scotland)', 'Breast' ).
pub_graph_journal_synonym( 'Cancer journal (Sudbury, Mass.)', 'Cancer Journal' ).
pub_graph_journal_synonym( 'Hematology / the Education Program of the American Society of Hematology. American Society of Hematology. Education Program', 'Hematology-American Society of Hematology Education Program' ).

pub_graph_table_output( [], _Form,  _Iif, _Search, _Rows ) :- !.
pub_graph_table_output( Stem, Form, Iif, Search, Rows ) :-
    file_name_extension( Stem, Form, OutF ),
    pub_graph_table_file_output( Form, OutF, Iif, Search, Rows ),
    !.
pub_graph_table_output( _Stem, Form, _Iif, _Search, _Rows ) :-
    write( user_error, 'Dont know how to write onto: ~w', [Form] ), nl( user_error ). % fixme

pub_graph_table_file_output( html, OutF, _Iif, Search, Rows ) :-
    tell( OutF ),
    maplist( pub_graph_table_html_row, Rows, HtmlRows ),
    term_string( Search, SearchString ),
    atom_string( SearchAtom, SearchString ),
    atom_concat( 'search term: ', SearchAtom, HtmlH1 ),
    reply_html_page( title(HtmlH1), [h1(HtmlH1),table(HtmlRows)] ).
pub_graph_table_file_output( csv, OutF, Iif, _Search, Rows ) :-
    pub_graph_iif_header( Iif, Hdr ),
    csv_write_file( OutF, [Hdr|Rows] ).

pub_graph_table_html_row( row(C,I,P,J,T,A,B), Html ) :-
    atomic_list_concat( ['https://www.ncbi.nlm.nih.gov/pubmed/?term=',B], Href ),
    Anc = a(href(Href),T),
    atomic_list_concat( PParts, ' ', P ),
    atomic_list_concat( PParts, '_', U ),
    Html = tr( [
                   td(C),
                td(I),
                td(U),
                td(J),
                td(Anc),
                td(A),
                td(B)
            ] ).

pub_graph_iif_header( true, Hdr ) :- 
    Hdr = row('Cited','IF','Published','Journal','Title','Author','Pubmed').
pub_graph_iif_header( false, Hdr ) :- 
    Hdr = row('Cited','Published','Journal','Title','Author','Pubmed').

/** pub_graph_summary_info( +IdS, -Summaries, +Opts ).
 
Summaries is the summary information for pub_graph id(s) IdS.<br>
The form of results depends on whether IdS is a single PubMed Id, <br>
in which case Summaries is a list of Name-Value pairs.<br>
Whereas, when IdS is a list, Summaries is a list Id-Info pairs, where Info<br>
is a Name-Value list. The predicate fetches the information with curn<br>
via the http interface Summaries are deposited in local temporary files which are
subsequently parsed.
  
Options is a single term, or list of the following terms:
  * names(Names)  
     list of info slot names to be found in the xml file
 
  * retmax(100) 
     the maximum number of records that will be returned

  * tmp_file(Tmp)  
     temporary file to be used for saving xml files. If Tmp is a variable, or option is missing, a temporary file is created with tmp_file_stream/3.
 
  * tmp_keep(false)  
     if true, keep the temporary xml file, otherwise, and by default, delete it.

  * verbose(Verb) 
     When =true= be verbose.

  * cache(Type,Handle,Update)  
     Use a cache with Type and Handle. Update should be boolean, set to =|false|=
     if you dont want the cache to be updated with newly downloaded information.
 
==
?- 
  date(Date), 
  Opts = names(['Author','PmcRefCount','Title']),
  pub_graph_summary_info( 12075665, Results, Opts ),
  write( date:Date ), nl, 
  member( R, Results ), write( R ), nl, 
  fail.

date:date(2018,9,22)
Author-[Kemp GJ,Angelopoulos N,Gray PM]
PmcRefCount-3
Title-Architecture of a mediator for a bioinformatics database federation.
false.


?- 
    pub_graph_summary_info(12075665,Res,[]), 
    member(R,Res), write( R ), nl, 
    fail.

Author-[Kemp GJ,Angelopoulos N,Gray PM]
Title-Architecture of a mediator for a bioinformatics database federation.
Source-IEEE Trans Inf Technol Biomed
Pages-116-22
PubDate-2002 Jun
Volume-6
Issue-2
ISSN-1089-7771
PmcRefCount-3
PubType-Journal Article
FullJournalName-IEEE transactions on information technology in biomedicine : a publication of the IEEE Engineering in Medicine and Biology Society

?- 
    pub_graph_summary_info( cbd251a03b1a29a94f7348f4f5c2f830ab80a909, Res, true ), 
    member( R, Res ), write( R ), nl, 
    fail.

arxivId-[]
authors-[Graham J. L. Kemp,Nicos Angelopoulos,Peter M. D. Gray]
doi-10.1109/TITB.2002.1006298
title-Architecture of a mediator for a bioinformatics database federation
topics-[]
venue-IEEE Transactions on Information Technology in Biomedicine
year-2002
false.
==
*/
pub_graph_summary_info( IdS, Results, OptS ) :-
    non_var_list( OptS, Opts ),
    non_var_list( IdS, Ids ),
     !,
     maplist( pub_graph_summary_info_single(Opts), Ids, UnsResults ),
     ( memberchk(sort_by(SortField,Order,Nums),Opts) ->
               pub_graph_summary_sort( UnsResults, SortField, Order, Nums, OrdResults )
               ;
               UnsResults=OrdResults
     ),
    de_kv_list_on( OrdResults, IdS, Results ).
pub_graph_summary_info( Id, Results, OptS ) :-
    non_var_list( OptS, Opts ),
     pub_graph_summary_info_single( Opts, Id, Id-Results ).

pub_graph_summary_info_single( Opts, Id, Id-Results ) :-
     memberchk( cache(Type,Handle,_Update), Opts ),
     pub_graph_summary_info_cached( Type, Handle, Id, Results, Opts ),
     debug( pub_graph, 'Summary from cache for: ~w', Id ),
     !.
pub_graph_summary_info_single( Opts, Id, Id-Info ) :-
     % fixme use _defaults
     debug( pub_graph, 'Summary not in cache for: ~w', Id ),
     pub_graph_id( Id, IdType ),
     ( memberchk(names(Names),Opts) -> true; default_names(IdType,Names) ),
     ( memberchk(tmp_file(Tmp),Opts) -> true; true ),
     ( memberchk(cache(Type,Handle,true),Opts) -> 
               summary_info( Tmp, Id, all, ResAll, Opts ) ,
               findall( Name-Val, (member(Name,Names),memberchk(Name-Val,ResAll)), Info ),
               pub_graph_summary_info_cached_update( Type, Handle, Id, ResAll )
               ;
               summary_info( Tmp, Id, Names, Info, Opts)
     ).

pub_graph_summary_info_cached( csv, Handle, Id, Results, Opts ) :-
     pub_graph_summary_info_cached( in_mem, Handle, Id, Results, Opts ).
pub_graph_summary_info_cached( prolog, Handle, Id, Results, Opts ) :-
     pub_graph_summary_info_cached( in_mem, Handle, Id, Results, Opts ).
pub_graph_summary_info_cached( sqlite, Handle, Id, Results, Opts ) :-
     pub_graph_summary_info_cached( db_facts, Handle, Id, Results, Opts ).
pub_graph_summary_info_cached( odbc, Handle, Id, Results, Opts ) :-
     pub_graph_summary_info_cached( db_facts, Handle, Id, Results, Opts ).
pub_graph_summary_info_cached( db_facts, Handle, Id, Results, Opts ) :-
     ( memberchk(names(Names),Opts) -> true; default_names(Names) ),
     findall(K-(O-V),(member(K,Names),db_holds(Handle,info(Id,K,O,V))), OPRes),
     OPRes \== [],
     keysort( OPRes, OPRord ),
     % kv_decompose_vs( OPRord, PRord ),
     nest_pair_flatten_removes( OPRord, PRord ),
     PRord = [K1-V1|PRTail],
     kvs_to_unique_k_v_as_list( PRTail, K1, [V1], Results ).
pub_graph_summary_info_cached( in_mem, _Handle, Id, Results, Opts ) :-
     ( memberchk(names(Names),Opts) -> true; default_names(Names) ),
     findall( K-V, (member(K,Names),pub_graph_cache_info:info(Id,K,V)), PRes ),
     PRes \== [],
     keysort( PRes, PRord ),
     PRord = [K1-V1|PRTail],
     kvs_to_unique_k_v_as_list( PRTail, K1, [V1], Results ).

pub_graph_summary_info_cached_update( csv, Alias, Id, KVs ) :-
     pub_graph_summary_info_cached_update( in_mem, Alias, Id, KVs ).
pub_graph_summary_info_cached_update( prolog, Alias, Id, KVs ) :-
     pub_graph_summary_info_cached_update( in_mem, Alias, Id, KVs ).
pub_graph_summary_info_cached_update( in_mem, Alias, Id, KVs ) :-
     retractall( info(Id,_K,_O,_V) ),
     retractall( info_date(Id,_) ),
     date( Date ), 
     assert( info_date(Id,Date) ),
     findall( _, ( member(K-V,KVs), (is_list(V)->nth1(N,V,Ve);Ve=V,N=1),assert(Alias, info(Id,K,N,Ve),_) ), _ ).
pub_graph_summary_info_cached_update( sqlite, Alias, Id, KVs ) :-
     pub_graph_summary_info_cached_update( db_facts, Alias, Id, KVs ).
pub_graph_summary_info_cached_update( odbc, Alias, Id, KVs ) :-
     pub_graph_summary_info_cached_update( db_facts, Alias, Id, KVs ).
pub_graph_summary_info_cached_update( db_facts, Alias, Id, KVs ) :-
     db_retractall( Alias, info(Id,_K,_O,_V), _Aff1 ),
     db_retractall( Alias, info_date(Id,_), _Aff2 ),
     date( Date ), 
     db_date_sql_atom( Date, SqlDate ),
     db_assert( Alias, info_date(Id,SqlDate), _ ),
     findall( _, ( member(K-V,KVs), (is_list(V)->nth1(N,V,Ve);Ve=V,N=1),db_assert(Alias, info(Id,K,N,Ve),_) ), _ ).

%% pub_graph_abstracts( +IdS, -IdsAbs ).
%
% For a list of IdS get all their respective IdAbs (ID-Abstracts) pairs.
% If IdS is a single PubMed Id then IDsAbs is simply the abstract (not a pair).
% Abstracts are returned as lists of atom, representing lines in the original reply.
%
%==
%  ?- pub_graph_abstracts( 24939894, Abs ).
%  Abs = ['Lemur tyrosine kinase 3 (LMTK3) is associated with cell proliferation and',...].
%==
%
%@tbd add option for returning the full response of the querny (includes sections for,
% Citation, Title, Aurhors, Affiliation and PMCID if one exists (last is in PMID section).
%
pub_graph_abstracts( IdS, Abstracts ) :-
    non_var_list( IdS, Ids ),
    pub_graph_get_abstracts( Ids, Ibs ),
    de_kv_list_on( Ibs, IdS, Abstracts ).

pub_graph_get_abstracts( Ids, Ibs ) :-
    atomic_list_concat( Ids, ',', IdsAtom ),
    atomic_list_concat( [id,IdsAtom], '=', IdsReq ),
    url_eutils( Eutils ),
    url_efetch_pubmed( Efetch ),
    atom_concat( Eutils, Efetch, EEReq ),
    RmdReq = 'retmode=text',
    RtpReq = 'rettype=abstract',
    atomic_list_concat( [EEReq,IdsReq,RmdReq,RtpReq], '&', Url ),
    get_url_in_file( Url, true, Tmp ),
    % write( file_is(Tmp) ), nl,
    file_abstracts( Tmp, Ibs ).

file_abstracts( File, Ibs ) :-
    read_file_to_codes( File, Codes, [] ),
    Codes = [0'\n|Tcodes],
    codes_abstracts( Tcodes, Ibs ).

% Each abstract-entry has sections seperated by \n.
codes_abstracts( Codes, Ibs ) :-
    % write( codes(Codes) ), nl,
    codes_abstracts_sections( Sections, [], Codes, [] ),
    % length( Sections, Len ),
    % maplist( write_codes_ln, Sections ),
    % write( sections_length(Len) ), nl,
    code_sections_abstracts( Sections, Ibs ).

write_codes_ln( Codes ) :-
    atom_codes( Atom, Codes ),
    write( Atom ), nl.

codes_abstracts_sections( Sects, Acc ) -->
    "\n\n", consume_new_line,
    !,
    { ( Acc==[] -> Sects=TSects; 
        reverse(Acc,Sect),Sects=[Sect|TSects]
         %, atom_codes( Att, Sect), write( att_acc(Att,Acc) ), nl 
      ) },
    codes_abstracts_sections( TSects, [] ).
codes_abstracts_sections( Sects, Acc ) -->
    [C],
    !,
    codes_abstracts_sections( Sects, [C|Acc] ).
codes_abstracts_sections( Sects, Acc ) -->
    { ( Acc == [] -> Sects = []
                 ; reverse( Acc, Sect ), 
                % atom_codes( SectAtom, Sect ),
                % write( section(SectAtom,Acc) ), nl,
                 Sects = [Sect] ) }.

consume_new_line --> "\n", {!}.
consume_new_line --> [].

code_sections_abstracts( [], [] ).
code_sections_abstracts( [_A,_B,_C,_D,_Comm,E,_Copy,F|T], [Id-Elns|Ibs] ) :-
    atom_codes( Fat, F ),
    debug( pub_graph, 'Parsing abstract from line:~w', Fat ),
    code_section_pub_med_id( Id, F, _ ),
    debug( pub_graph, 'Parsing abstract for id:~w', Id ),
    code_section_lines( E, Elns ),
    !,
    code_sections_abstracts( T, Ibs ).
code_sections_abstracts( [_Ref,_Ttl,_Ath,_Ainf,Abs,Copy,Pmi|T], [Id-AbsLns|Ibs] ) :-
    copyright_line( Copy, _ ),
    atom_codes( Pat, Pmi ),
    debug( pub_graph, 'Parsing abstract from line:~w', Pat ),
    code_section_pub_med_id( Id, Pmi, _ ),
    debug( pub_graph, 'Parsing abstract for id:~w', Id ),
    code_section_lines( Abs, AbsLns ),
    !,
    code_sections_abstracts( T, Ibs ).
code_sections_abstracts( [_Ref,_Ttl,_Ath,_Ainf,_Cmnt,Abs,Pmi|T], [Id-AbsLns|Ibs] ) :-
    atom_codes( Pat, Pmi ),
    debug( pub_graph, 'Parsing abstract from line:~w', Pat ),
    code_section_pub_med_id( Id, Pmi, _ ),
    !,
    debug( pub_graph, 'Parsing abstract for id:~w', Id ),
    code_section_lines( Abs, AbsLns ),
    code_sections_abstracts( T, Ibs ).
code_sections_abstracts( [_Ref,_Ttl,_Ath,_Anf,Abs,Pmi|T], [Id-AbsLns|Ibs] ) :-
    atom_codes( Pat, Pmi ),
    debug( pub_graph, 'Parsing abstract from line:~w', Pat ),
    code_section_pub_med_id( Id, Pmi, _ ),
    !,
    debug( pub_graph, 'Parsing abstract for id:~w', Id ),
    code_section_lines( Abs, AbsLns ),
    code_sections_abstracts( T, Ibs ).
code_sections_abstracts( [_Ref,_Ttl,_Ath,Pmi|T], [Id-[]|Ibs] ) :-
    atom_codes( Pat, Pmi ),
    debug( pub_graph, 'Parsing abstract from line:~w', Pat ),
    code_section_pub_med_id( Id, Pmi, _ ),
    debug( pub_graph, 'Parsing abstract for id:~w', Id ),
    !,
    code_sections_abstracts( T, Ibs ).
code_sections_abstracts( [_A,_B,_C,_D,F|T], [Id-Elns|Ibs] ) :-
    atom_codes( Fat, F ),
    debug( pub_graph, 'Parsing non-abstract from line:~w', Fat ),
    code_section_pub_med_id( Id, F, _ ),
    Elns = [],
    code_sections_abstracts( T, Ibs ).

/*
code_sections_abstracts( [_Ref,_Ttl,_Ath,Abs,Copy,Pmi|T], [Id-AbsLns|Ibs] ) :-
    copyright_line( Copy, _ ),
    atom_codes( Pat, Pmi ),
    debug( pub_graph, 'Parsing abstract from line:~w', Pat ),
    code_section_pub_med_id( Id, Pmi, _ ),
    !,
    debug( pub_graph, 'Parsing abstract for id:~w', Id ),
    code_section_lines( Abs, AbsLns ),
    code_sections_abstracts( T, Ibs ).
code_sections_abstracts( [_Ref,_Ttl,_Ath,_Anf,Abs,Pmi|T], [Id-AbsLns|Ibs] ) :-
    atom_codes( Pat, Pmi ),
    debug( pub_graph, 'Parsing abstract from line:~w', Pat ),
    code_section_pub_med_id( Id, Pmi, _ ),
    !,
    debug( pub_graph, 'Parsing abstract for id:~w', Id ),
    code_section_lines( Abs, AbsLns ),
    code_sections_abstracts( T, Ibs ).
code_sections_abstracts( [_Ref,_Ttl,_Ath,Pmi|T], [Id-[]|Ibs] ) :-
    atom_codes( Pat, Pmi ),
    debug( pub_graph, 'Parsing abstract from line:~w', Pat ),
    code_section_pub_med_id( Id, Pmi, _ ),
    !,
    debug( pub_graph, 'Parsing abstract for id:~w', Id ),
    code_sections_abstracts( T, Ibs ).
code_sections_abstracts( [_A,_B,_C,_D,F|T], [Id-Elns|Ibs] ) :-
    atom_codes( Fat, F ),
    debug( pub_graph, 'Parsing non-abstract from line:~w', Fat ),
    code_section_pub_med_id( Id, F, _ ),
    Elns = [],
    code_sections_abstracts( T, Ibs ).
    */

code_section_lines( E, Elns ) :-
    code_section_code_lines( EcLns, [], E, [] ),
    maplist( atom_codes, Elns, EcLns ).

code_section_code_lines( Lns, Acc ) -->
    "\n",
    !,
    { Acc == [] -> Lns = TLns; reverse( Acc, Ln ), Lns = [Ln|TLns] },
    code_section_code_lines( TLns, [] ).
code_section_code_lines( Lns, Acc ) -->
    [C],
    !,
    code_section_code_lines( Lns, [C|Acc] ).
code_section_code_lines( Lns, Acc ) -->
    { Acc == [] -> Lns = []; reverse(Acc,Ln), Lns = [Ln] }.

copyright_line -->
    "Copyright", !.
copyright_line -->
    "©", !.

code_section_pub_med_id( Id ) -->
    "PMID: ",
    !,
    code_section_in_pub_med_id( IdCs ),
    {number_codes(Id,IdCs)}.
code_section_pub_med_id( Id ) -->
    [_C],
    code_section_pub_med_id( Id ).

code_section_in_pub_med_id( [C|Cs] ) -->
    [C],
    { 0'0 =< C, C =< 0'9},
    !,
    code_section_in_pub_med_id( Cs ).
code_section_in_pub_med_id( [] ) --> [].

pub_graph_summary_sort( Summ, By, Ord, Nums, Sorted ) :-
     summary_items_have( Summ, By, Nums, Have, Havenots ),
     sort( Have, Sorto ),
     ( Ord == asc -> Sorto = Ordo; reverse(Sorto,Ordo) ),
     kv_decompose_vs( Ordo, Disc ),
     % disconnect_double_minus( Ordo, Disc ),
     append( Disc, Havenots, Sorted ).

disconnect_double_minus( [], [] ).
disconnect_double_minus( [_K1-K-V|T], [K-V|M] ) :-
     disconnect_double_minus( T, M ).

summary_items_have( [], _By, _Nums, [], [] ).
summary_items_have( [K-List|T], By, Nums, Haves, Havenots ) :-
     ( memberchk(By-ByItem,List) ->
          ( Nums == true ->
               to_number( ByItem, ByItemNum ),
               Haves = [ByItemNum-(K-List)|THaves],
               Havenots = THavenots
               ;
               Haves = [ByItem-(K-List)|THaves],
               Havenots = THavenots
          )
          ;
          Haves = THaves,
          Havenots = [K-List|THavenots]
     ),
     summary_items_have( T, By, Nums, THaves, THavenots ).

/** pub_graph_cited_by_graph( +Ids, -Graph, +Opts ).

Graph of all ancestors reaching Ids within Depth moves.
The graph grows upwards from the roots (Ids) to find the papers that 
cite the growing bag of papers recursively. 

Options is a single term, or list of the following terms:
  * cache(Type)
     use cache of Type. =|Type == false|= or absent to turn caching off
  * location(Object)   
     if using cache, which location should be used
  * date(CoffDate)  
     if caching is used, at what date do cache expires. Default: 1 month ago.
  * depth(Depth)  
     maximum depth to chase
  * ext(Ext)  
     superseed the extension on Object.
  * flat(Flat)  
     Boolean value. If csv cited_by should be one per line or of the form Id1;Id2;
  * flat_input(InpFlat)  
     should the input cache be imported as flat (def. = Flat).
  * verbose(Verbose)  
     prints progress messages if =|true|=

Type is one of =|csv,prolog,sqlite|= and =|odbc|=. In the first 3 cases, Object should be a filename
and for =|odbc|= it should be a DSN token. In the case of filenames, the default value for Object
is formed as, <type>_<id1>{_<id2>}.<type_ext>.
<type_ext> is either set to Ext or if this is missing it is deduced from Type. It can be set to =|''|=
if you want no extension added.

Graph is compatible with the graph representation of Prolog unweighted graphs.
That is, all vertices should appear in a keysorted list as V-Ns pairs,
where V is the vertex and Ns is the sorted list of all its neighbours. 
Ns is the empty list if V has no neighbours, although this should
only be the case here, if one of the input Ids has no citing papers 
or for the nodes at the edge of Depth.
  
==
?- 
     pub_graph_cited_by_graph( 12075665, G, cache(sqlite) ).
==

*/
pub_graph_cited_by_graph( IdS, Graph, Args ) :-
    % non_var_list( OptS, Opts ),
     % pub_graph_graph_defaults( Defs ),
     options_append( pub_graph_graph, Args, Opts ),
     options( depth(Depth), Opts ),
     ( Depth =:= 0 -> Ds = inf, De = 0; integer(Depth), Ds = bound, De = Depth ),
     non_var_list( IdS, Ids ),
     maplist( kv_elem_set(0), Ids, IDs ),
    options( verbose(Pgs), Opts ),
     opts_list_opens_cby_cache( Opts, Ids, COpts ), % maybe listed(Opts) instead of All ?
     pub_graph_cited_by_graph_depth( IDs, [], Ds, De, Pgs, [], COpts, Orph, Graph1 ),
     opts_list_closes_cby_cache( COpts ),
     sort( Orph, Overts ),
     keysort( Graph1, GraphOrd ),
     add_vertices( GraphOrd, Overts, Graph ).

opts_list_opens_cby_cache( Opts, Ids, Copts ) :-
     ( (memberchk(cache(Type),Opts),\+Type==false) ->
          ( memberchk(ext(Ext),Opts) -> true ; file_type_extension(Type,Ext) ),
          ( memberchk(location(Loc),Opts) ->
               ext_file( Loc, Ext, Obj )
               ;
               ( Ids = [A,B|_] -> IList = [A,B]; Ids = [A|_], IList = [A] ),
               atomic_list_concat( [Type|IList], '_', FStem ),
               file_name_extension( FStem, Ext, Obj )
          ),
          memberchk( date(Date), Opts ),
          memberchk( update(Upd), Opts ),
          pub_graph_cache_open( Type, Obj, cited_by, Handle, Opts ),
          Copts = [cache(Type,Handle,Date,Upd)|Opts]
          ;
          Copts = Opts
     ).

opts_list_closes_cby_cache( COpts ) :-
     memberchk( cache(Type,Handle,_,_) ,COpts ),
     !,
     pub_graph_cache_save( Type, Handle, cited_by/3, [] ).
opts_list_closes_cby_cache( _ ).

pub_graph_cited_by_treadmill_defaults( Defs ) :-
          Defs      =  [ depth(5),
                         ext(pl),
                         file(graph_treadmilling),
                         single_file(false)
                       ].

/** pub_graph_cited_by_treadmill( +Ids, -Graph, +Opts ).

     Use iterative increase of depth limit on pumed_cited_by_graph/3 with 
     until to the overall Depth is reached. Results are saved to a cache
     file before proceeding to rerun the whole thing with an unit increase on 
     the depth limit. Previous results will be fished out from the cache files.

     Options is a single term or list of the following:
          * file(File)  file to use for storage
          * single_file(Single)  boolean value, def. is =|true|=. 
                                  if =|false|= seperate (aggregating) files are created
                                  at each iteration
          * depth(D)   the overall depth limit 

     @tbd use ODBC 

*/
pub_graph_cited_by_treadmill( Ids, Graph, Args ) :-
     % pub_graph_treadmill_defaults( Defs ),
     options_append( pub_graph_cited_by_treadmill, Args, Opts ),
     options( file(File), Opts ),
     options( single_file(Single), Opts ),
     options( depth(D), Opts ),
     options( ext(Ext), Opts ),
     ext_file( File, Ext, PlFile ),
     ( exists_file(PlFile) -> 
          consult( pub_graph_cache:PlFile )
          ;
          true
     ),
     Oterm = topts(D,File,Single),
     pub_graph_cited_by_treadmill_deepening( 1, Oterm, Ids, _, Graph ).


pub_graph_cited_by_treadmill_deepening( Di, topts(Dlim,_,_), _Ids, Graph, Graph ) :-
     Di > Dlim,
     !, 
     % write( overall_depth_limit_reached(Dlim) ), nl.
     true.
pub_graph_cited_by_treadmill_deepening( Di, Topts, Ids, _Current, Graph ) :-
     Topts = topts(Dlim,File,Single),
     Opts = [verbose(true),cache(prolog,_,date(2012,09,01),true),depth(Di),verbose(true)],
     pub_graph_cited_by_graph( Ids, GraphI, Opts ),
     % length( GraphI, LenGI ),
     % write( population_at_depth(Di,LenGI) ), nl,
     pub_graph_treadmill_file_name( Single, File, Di, Dlim, Fname ),
     % write( onto_file(Fname) ), nl,
     pub_graph_cache_save( prolog, Fname, cited_by/3, [] ),
     Dstar is Di + 1,
     pub_graph_cited_by_treadmill_deepening( Dstar, Topts, Ids, GraphI, Graph ).
     

/** pub_graph_cache_open( +Type, +File, +Which, -Handle, Opts ).

     Open a pub_graph File of a given Type. A Handle is returned if appropriate.
     Currently =|csv,prolog,odbc|= and =|sqlite|= files are recognised. 
     The former two are consulted into module =|pub_graph_cache|=, and Handle is therofore not used.
     For =|odbc/sqlite|= files the lookups and database access is via the odbc and prosqlite libraries respectively.
     Handle can be named to an alias of choise, otherwise a opaque atom is returned with which the db is accessed.
     Which, should either be =|cited_by|= or =|info|= .

     Options is a term or list of terms from:
          
               * ext(Ext)   extension to try on the file. Use the empty atom if you do not want the library to 
                            use the default extension for the type of cache used.
     
     Options are also passed to the underlying open operations for the type chosen. So for instance
     you can provide the username and passward for the odbc connection with user(U) and password(P).
     
*/
pub_graph_cache_open( csv, FileIn, Which, Handle, Opts ) :-
     memberchk( Which, [cited_by,info] ),
     % atom_concat( pub_graph_cache_, Which, Module ),
     pub_graph_cache_open_csv_file( FileIn, pub_graph_cache, Which, Handle, Opts ).
pub_graph_cache_open( prolog, FileIn, Which, Handle, Opts ) :-
     memberchk( Which, [cited_by,info] ),
     % atom_concat( pub_graph_cache_, Which, Module ),
     pub_graph_cache_open_pl_file( FileIn, pub_graph_cache, Handle, Opts ).
pub_graph_cache_open( sqlite, FileIn, Which, Alias, Opts ) :-
     memberchk( Which, [cited_by,info] ),
     pub_graph_cache_open_sqlite( FileIn, Alias, Which,  Opts ).
pub_graph_cache_open( odbc, Dsn, _Which, Alias, Opts ) :-
     ( db_current_connection(Alias,Type) -> 
          Fail = failed_to_open_odbc_on_existing_alias(Alias,Type,Dsn),
          write( user_error, Fail ), nl( user_error ), fail
          ;
          Defs = [alias(Alias),user(pub_graph),password(pmed123)],
          append( Opts, Defs, All ),
          odbc_connect( Dsn, Alias, All )
     ).
     
pub_graph_cache_open_csv_file( FileIn, Module, Pname, CbyF, Opts ) :-
     append( Opts, [ext(csv)], All ),
     options( ext(Ext), All ),
     ext_file( FileIn, Ext, CbyF ),
     (exists_file(CbyF) -> 
               csv_read_file( CbyF, Rows, [functor(Pname)] ),
               ( memberchk(flat_input(IFlat),Opts) -> true; memberchk(flat(IFlat),Opts) ),
               ( IFlat==true -> 
                         sort(Rows,Ord),
                         ord_facts_aggregate_arg(Ord,3,Agr)
                         ;
                         maplist( csv_row_to_nest_row, Rows, Agr ) 
               ),
               maplist( assert_at(Module), Agr )
               ;
               true
     ).
pub_graph_cache_open_pl_file( FileIn, Module, CbyF, Opts ) :-
     append( Opts, [ext(pl)], All ),
     memberchk( ext(Ext), All ),
     ext_file( FileIn, Ext, CbyF ),
     (exists_file(CbyF) -> call( Module:consult(CbyF) ) ; true ).

pub_graph_cache_open_sqlite( FileIn, Alias, Type,  Opts ) :-
     append( Opts, [ext(sqlite)], All ),
     memberchk( ext(Ext), All ),
     ext_file( FileIn, Ext, SqliteF ),
     ( exists_file(SqliteF) ->
          true
          ;
          atomic_list_concat( ['Creating pub_graph',Type,'SQLite database at'], ' ', Create ),
          write( Create:SqliteF ), nl,
          sqlite_connect( SqliteF, HandleC, exists(false) ),
          findall( _, 
                         ( pub_graph_create_sqlite_type_statement( Type, Statement ),
                           prosqlite:sqlite_query( HandleC, Statement, _Row )
                         ),
                                   _ ),

          sqlite_disconnect( HandleC )  % fixme: make sure prosqlite closes with handles as well as with aliases.
     ),
     % atom_concat( pub_graph_cache_, Type, Mod ),
     Mod = pub_graph_cache,
     ( var(Alias) -> Alias = HandleD ; Ao = [alias(Alias)] ),
     OptsD = [as_predicates(true),at_module(Mod)|Ao],
     sqlite_connect( SqliteF, HandleD, OptsD ).

pub_graph_create_sqlite_type_statement( cited_by, 'CREATE TABLE cited_by (pubmed_id bigint(20), ret_date date, citer bigint(20), Primary Key (pubmed_id,citer) );' ).
pub_graph_create_sqlite_type_statement( info, 'CREATE TABLE info_date (pubmed_id bigint(20), rec_date date, Primary Key (pubmed_id) );' ).
pub_graph_create_sqlite_type_statement( info, 'CREATE TABLE info (pubmed_id bigint(20), key_name tinytext, nth integer, key_value smalltext, Primary Key (pubmed_id,key_name,nth) );' ).

/** pub_graph_cache_save( +Type, +FileinORHandle, What, Opts ).

Close or save a cache to a file. Currently Types csv, `prolog', `odbc' and `sqlite' are recognised.
In the case of prolog,  the list of predicates What is dumped to the prolog file Filein. Likewise
for `csv' but as data rows.  The predicates are looked for in module `pub_graph_cache'.
Once the preds are saved, their retracted from memory.

Opts a term or list of terms from: 
  * flat(Flat)  
     should csv and prolog rows be compressed by third argument ? 
*/
pub_graph_cache_save( prolog, FileIn, WhatIf, Opts ) :-
     pub_graph_cache_save( in_mem,  prolog, FileIn, WhatIf, Opts ).
pub_graph_cache_save( csv, FileIn, WhatIf, Opts ) :-
     pub_graph_cache_save_in_mem( csv, FileIn, WhatIf, Opts ).
pub_graph_cache_save( sqlite, Handle, _, _Opts ) :-
     sqlite_disconnect( Handle ).
pub_graph_cache_save( odbc, Handle, _, _Opts ) :-
     odbc_disconnect( Handle ).
pub_graph_cache_save( db_facts, Handle, _, Opts ) :-
     db_current_connection( Handle, Type ),
     Type \== db_facts,
     pub_graph_cache_save( Type, Handle, _, Opts ).

pub_graph_cache_save_in_mem( Which, File, WhatIf, Opts ) :-
     pg_en_list( WhatIf, What ),
     /* ( file_name_extension(_,'',FileIn) ->
          file_name_extension( FileIn, pl, File ) ; File = FileIn),
     */
     ( (memberchk(flat(true),Opts),Which=_Something/3) -> Flat = true; Flat = false ),
     open( File, write, Out ),
     pub_graph_cache_dump( Which, What, Out, Flat ),
     close( Out ),
     forall( member(W,What), (W=Pname/Arity,functor(G,Pname,Arity),retractall(pub_graph_cache:G)) ).

pub_graph_cache_dump( Which, What, Out, Flat ) :-
     member( Name/Arity, What ),
     functor( Goal, Name, Arity ),
     once(pl_cache_predicate_module(Name,Arity,Module) ),
     forall( call(Module:Goal), record_in_mem_clause(Which,Flat,Out,Goal) ).

record_in_mem_clause( Which, Flat, Out, Fact ) :-
     ( Flat == true ->
          arg( 3, Fact, Third ),
          ( Third == [] -> 
               arg( 1, Fact, First ), arg( 2, Fact, Second ),
               functor( Fact, Name, 3 ),
               functor( New, Name, 3 ),
               arg( 1, New, First ), arg( 2, New, Second ), arg( 3, New, 0 )
               ;
               ( is_list(Third) ->
                    arg( 1, Fact, First ), arg( 2, Fact, Second ),
                    functor( Fact, Name, 3 ),
                    functor( New, Name, 3 ),
                    arg( 1, New, First ), arg( 2, New, Second ),
                    forall( member(Elem,Third), (arg(3,New,Elem),record_fact(Which,Out,New)) )
                    ;
                    record_fact( Which, Out, Fact )
               )
          )
          ;
          record_fact( Which, Out, Fact )
     ).
     /* wouldn't be cool to have portray clause/2 write on csvs automatically ?
        then we need open_csv to register the csv files 
     */
record_fact( prolog, Out, Fact ) :-
     portray_clause( Out, Fact ).
record_fact( csv, Out, Fact ) :-
     % not-checked !
     % see if we can get away with the []
     mapargs( pl_term_csv_atom, Fact, Csv ),
     csv_write_stream( Out, [Csv], [] ).

pl_cache_predicate_module( cited_by, 3, pub_graph_cache ).
pl_cache_predicate_module( info, 3, pub_graph_cache ).
pl_cache_predicate_module( info_date, 2, pub_graph_cache ).

% Section: non-interface predicates...

pub_graph_cited_by_graph_depth( [], _Seen, _Ds, _De, _Pgs, Graph, _Opts, [], Graph ).
pub_graph_cited_by_graph_depth( [Id-D|T], Seen, Ds, De, Pgs,  Acc, Opts, Orph, Graph ) :-
     ( Pgs==true -> length(T,Len), 
                    writeq(id(Id,Len)), write('.'), nl
                    ; true ),
     ( ord_memberchk(Id,Seen) -> 
          Pairs = T,
          NxSeen = Seen,
          Nxt = Acc,
          Orph = TOrph
          ;
          pub_graph_cited_by( Id, IdAnc, Opts ),
          ( Pgs==true -> 
               write( has(Id,IdAnc) ), write( '.' ), nl
               ;
               true
          ),
          D1 is D + 1,
          ( depth_limit_reached(Ds,De,D1) -> 
               Pairs = T,
               NxSeen = Seen,
               Nxt = Acc,
               Orph = [Id|TOrph]
               ;
               ord_add_element( Seen, Id, NxSeen ),
               maplist( kv_elem_set(D1), IdAnc, New ),
               append( T, New, Pairs ),
               sort( IdAnc, AncSort ),
               Nxt = [Id-AncSort|Acc],
               Orph = TOrph
          )
     ),
     pub_graph_cited_by_graph_depth( Pairs, NxSeen, Ds, De, Pgs, Nxt, Opts, TOrph, Graph ).

depth_limit_reached( bound, Lim, Val ) :-
     Lim < Val.

kv_elem_set( Val, Key, Key-Val ).
          
summary_info( Tmp, Id, WhichIn, Results, OptS ) :-
    pub_graph_id( Id, IdType ),
    non_var_list( OptS, Opts ),
    ( WhichIn == all -> default_names(IdType,Which) ;  pg_en_list(WhichIn,Which) ),
    summary_info( IdType, Tmp, Id, Which, Results, Opts ).

summary_info( ncbi, Tmp, Id, Which, Results, Opts ) :-
     url_eutils( Eutils ),
     ( memberchk(retmax(RMax),Opts) -> true; RMax = 100 ),
     atomic_list_concat( ['esummary.fcgi?report=xml&mode=text&tool=wget&retmax=',RMax,'&db=pubmed&id='], Query ),
     atomic_list_concat( [Eutils,Query,Id], Url ),
     get_url_in_file( Url, false, Tmp ), % fixme
     load_xml_file( Tmp, Xml ),
     Elem = element(_,_,[Entry]),
     findall( Name-Info, ( 
                           member( Name, Which ),
                           findall( Entry,
                             search_element_in_list(Xml, 'Item', ['Name'=Name], Elem ),
                                   PInfo ),
                           ( PInfo = [Info] -> true; PInfo = Info )
                         ), Results ),
     ( memberchk(tmp_keep(true),Opts) -> true; delete_file(Tmp) ).
summary_info( semscholar, _Tmp, Id, Which, Results, _Opts ) :-
    semscholar_id_json( Id, Json ),
    findall( Name-Info, ( member(Name,Which),
                          member(Name=InfoPrv,Json),
                          json_value(InfoPrv,Info)
                        ),
                            Results
           ).

pub_graph_date_cached( prolog, Explicit, Default, Cutoff, Id, Return ) :-
     pub_graph_date_cached( in_mem, Explicit, Default, Cutoff, Id, Return ).
pub_graph_date_cached( csv, Explicit, Default, Cutoff, Id, Return ) :-
     pub_graph_date_cached( in_mem, Explicit, Default, Cutoff, Id, Return ).
pub_graph_date_cached( in_mem, _Handle, Pname, Cutoff, Id, Return ) :-
     % ( var(Explicit) -> Explicit=Default; true ),
     G =.. [Pname,Id,Date,Return],
     once(pl_cache_predicate_module(Pname,3,Module) ),
     call( Module:G ),
     Cutoff @=< Date.
pub_graph_date_cached( sqlite, Handle, Pname, Cutoff, Id, Return ) :-
     pub_graph_date_cached( db_facts, Handle, Pname, Cutoff, Id, Return ).
pub_graph_date_cached( odbc, Handle, Pname, Cutoff, Id, Return ) :-
     pub_graph_date_cached( db_facts, Handle, Pname, Cutoff, Id, Return ).
pub_graph_date_cached( db_facts, Handle, Pname, Cutoff, Id, Return ) :-
     G =.. [Pname,Id,SQLDate,R],
     findall( R, db_holds(Handle,G), PrvReturn ),
     PrvReturn \== [],
     once( db_holds(Handle,G) ),
     ( atom(SQLDate) -> 
          db_date_sql_atom( Date, SQLDate )
          ;
          % odbc translates this, so should sqlite
          Date = SQLDate
     ),

     ( PrvReturn = [0] -> Return = [] ; Return = PrvReturn ),
     Cutoff @=< Date.

/* commenting this out probably makes as_predicates(true) obsolete
     G =.. [PredName,Id,SQLDate,R],
     once(pl_cache_predicate_module(PredName,3,Module) ),
     findall( R, call(Module:G), PrvReturn ),
     PrvReturn \== [],
     once( Module:G ), 
     date_sql_atom( Date, SQLDate ),
     ( PrvReturn = [0] -> Return = [] ; Return = PrvReturn ),
     Cutoff @=< Date.
     */

pub_graph_update_cache( csv, Explicit, Default, Id, Return ) :-
     pub_graph_update_cache( in_mem, Explicit, Default, Id, Return ).
pub_graph_update_cache( prolog, Explicit, Default, Id, Return ) :-
     pubned_update_cache( in_mem, Explicit, Default, Id, Return ).
pub_graph_update_cache( in_mem, _Handle, Pname, Id, Return ) :-
     % ( var(Explicit) -> Explicit=Default; true ),
     date(Date),
     R =.. [Pname,Id,_,_],
     retractall( pub_graph_cache:R ),
     G =.. [Pname,Id,Date,Return],
     assert( pub_graph_cache:G ).

pub_graph_update_cache( sqlite, Handle, PredName, Id, PrvReturn ) :-
     pub_graph_update_cache( db_facts, Handle, PredName, Id, PrvReturn ).
pub_graph_update_cache( odbc, Handle, PredName, Id, PrvReturn ) :-
     pub_graph_update_cache( db_facts, Handle, PredName, Id, PrvReturn ).
pub_graph_update_cache( db_facts, Handle, PredName, Id, PrvReturn ) :-
     % ( var(Explicit) -> Explicit=Default; true ),
     date(Date),
     R =.. [PredName,Id,_,_],
     db_retractall( Handle, R, _ ),
     db_date_sql_atom( Date, SqlDate ),
     G =.. [PredName,Id,SqlDate,Ret],
     ( PrvReturn = [] -> Return = [0]; Return = PrvReturn ),
     findall( Ret, (member(Ret,Return),db_assert(Handle,G,_)), _ ).

pub_graph_cited_by_parse_file(Tmp, Ids) :-
     load_xml_file( Tmp, Xml ),
     once( search_element_in_list( Xml, 'LinkSetDb', [], element(_,_,LXml) ) ),
     findall( CId, 
               ( search_element_in_list(LXml,'Id',[],element(_,_,[CIdA])),
                 atom_codes(CIdA,CIdCs), number_codes(CId,CIdCs)
               ),
               Ids ).

% Section: auxiliaries

%% pub_graph_search_period_opts( +Pfx, -Period, +Opts ).
%
% Create a Period atom DateKey=DateValue separated by '&' for each of 
% the DateKey(DateValue) terms in Opts for which pub_graph_elink_date_option(DateKey) 
% holds.
% 
%==
% ?- pub_graph_search_period_opts( '', Period, [reldate(60)] ).
% Period = 'reldate=60'.
%
% ?- pub_graph_search_period_opts( '', Period, [mindate(2014),mindate(2015)] ).
% Period = 'mindate=2014&mindate=2015'.
%
% ?- pub_graph_search_period_opts( '', Period, [mindate(2014),maxdate(2015)] ).
% Period = 'mindate=2014&maxdate=2015'.
%
% 
%==
%
pub_graph_search_period_opts( Pfx, Period, Opts ) :-
    select( Opt, Opts, Rpts ),
    functor( Opt, Oname, 1 ),
    pub_graph_elink_date_option( Oname ),
    arg( 1, Opt, Arg ),
    !,
    atomic_list_concat( [Oname,Arg], '=', Atom ),
    atomic_list_concat( [Pfx,Atom], '&', Rfx ),
    pub_graph_search_period_opts( Rfx, Period, Rpts ).
pub_graph_search_period_opts( Period, Period, _Opts ).

pub_graph_elink_date_option( reldate ).
pub_graph_elink_date_option( mindate ).
pub_graph_elink_date_option( maxdate ).

%% search_term_to_query(+Sterm, +Gap, +Qv, -Query).
%
%  Convert a pubmed term to an atomic query that can be passed through http.
%
%  Gap is an integer, used for fields that take approximate search (0 is strictest for allowing 0 interving words)
%  Qv is the quote_value(Qv) option value.
%
%
search_term_to_query( (A,B), Gap, Qv, Query ) :-
     !,
     search_term_to_query( A, Gap, Qv, Aq ),
     search_term_to_query( B, Gap, Qv, Bq ),
     atomic_list_concat( [Aq,'+AND+',Bq], Query ).
search_term_to_query( (A;B), Gap, Qv, Query ) :-
     !,
     search_term_to_query( A, Gap, Qv, Aq ),
     search_term_to_query( B, Gap, Qv, Bq ),
     atomic_list_concat( [Aq,'OR',Bq], '+', Query ).
search_term_to_query( (A=B), Gap, Qv, Query ) :-
     !,
     maplist( quote_curl_atom, [A,B], [Aq,Bq] ),
     search_term_proximity_field( A, Gap, Pxf ),
     ( Qv == false ->   % true is defaulty
          atomic_list_concat( [Bq,'[',Aq,Pxf,']'], Query )
          ;
          atomic_list_concat( ['%22',Bq,'%22','[',Aq,Pxf,']'], Query )
     ).
search_term_to_query( C, _Gap, _Qv, Query ) :-
     pg_en_list( C, Clist ),
     maplist( quote_curl_atom, Clist, Qlist ),
     atomic_list_concat( Qlist, '+', Query ).

search_term_proximity_field( A, Gap, Pxf ) :-
     search_term_proximity_field( A ),
     !,
     atomic_list_concat( [':~',Gap], Pxf ).
search_term_proximity_field( _A, _Gap, '' ).

search_term_proximity_field('Affiliation').
search_term_proximity_field(affiliation).
search_term_proximity_field('Title').
search_term_proximity_field(title).
search_term_proximity_field('Title/Abstract').
search_term_proximity_field('title/abstract').
% abbreviations
search_term_proximity_field(ti).   % Title
search_term_proximity_field(tiab). % Title/Abstract
search_term_proximity_field(ad).   % affiliation ? 

% very basic. for now we are just translating space to %20
% we should use some SWI http internals here
%
quote_curl_atom( In, Out ) :-
     atom_codes( In, InCs ),
     maplist( quote_curl_code, InCs, NestOutCs ),
     flatten( NestOutCs, OutCs ),
     atom_codes( Out, OutCs ).

quote_curl_code( 0' , [0'%,0'2,0'0] ) :- !.
quote_curl_code( 0'", [0'%,0'2,0'2] ) :- !.
% quote_curl_code( 0' , [0'+] ) :- !.
quote_curl_code( Code, Code ).
     
%% memberchk_optional( Elem, List ).
%
%  Unifies Elem with the first matching term in List if one exists.
%  The predicate always succeeds exactly once.
%
memberchk_optional( Elem, List ) :-
     memberchk( Elem, List ),
     !.
memberchk_optional( _Elem, _List ).
     
true_atom_keeps_file( Keep, _File ) :-
     Keep == true,
     !.
true_atom_keeps_file( _Keep, File ) :-
     delete_file( File ).

/*
to_list( Either, List ) :-
    ( (var(Either);(Either\=[_H|_T],Either\==[]) ) ->
        List = [Either]
        ;
        List = Either
    ).
    */

true_writes( true, Report ) :-
     !,
     write( Report ), nl.
true_writes( _Opts, _Report ).

%% get_url_in_file(+URL, +Verbose, -In).
%
% From SWI-Prolog's doc files (July 2012). tmp_file_stream/3.
%
get_url_in_file(Url, _Verb, File) :-
    ( var(File) -> tmp_file_stream(text, File, Stream), close(Stream) ; true),
    debug( pub_grapsh, 'Downloading URL: ~p, onto file: ~p', [Url,File] ),
    setup_call_cleanup(
        http_open(Url, In,
              [ % cert_verify_hook(ssl_verify) % checkme:
              ]),
        setup_call_cleanup(
        open(File, write, Out, [type(binary)]),
        copy_stream_data(In, Out),
        close(Out)),
        close(In)
    ).

% this was the old version: currently not called from anywhere 18.09.22
get_url_in_file(curl, URL, Verb, File) :-
        ( var(File) -> tmp_file_stream(text, File, Stream), close(Stream) ; true),
        ( Verb==true -> Args = ['-o',File,URL] ; Args = ['-s','-o',File,URL] ),
        % true_writes( Verb, process_create(path(curl),Args,[]) ),
       debug( pub_graph, 'Getting url via curl with args:~w', [Args] ),
        % fixme: use url_file/2
        process_create( path(curl), Args, [] ),
        exists_file( File ).

/*
http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=science[journal]+AND+breast+cancer+AND+2008[pdat]
        
        */

all_subs_in_xml_single( Xml, Single, SubSel, Subs ) :-
     once( search_element_in_list( Xml, Single, [], element(_,_,Nest) ) ),
     findall( Sub, 
               search_element_in_list(Nest,SubSel,[],element(_,_,Sub)), 
              Subs ).

%% search_element_in_list(+Term, +Name, +Attrs, -Elem ).
% 
%  Find an element in an sgml file that have specific Attrs.
%  Got from hostip.pl .
%
search_element_in_list([Content|MoreContent], Name, ListAttributes, Element) :-
    (   search_element(Content, Name, ListAttributes, Element)
    ;   search_element_in_list(MoreContent, Name, ListAttributes, Element)
    ).

search_element(HTML, Name, ListAttributes, HTML) :-
    compound(HTML),
    arg(1, HTML, Name),
    arg(2, HTML, HTML_Attributi),
    forall(member(Attribute, ListAttributes),
           memberchk(Attribute, HTML_Attributi)).
search_element(HTML, Name, ListAttributes, Element) :-
    compound(HTML),
    arg(3, HTML, Contents),
    search_element_in_list(Contents, Name, ListAttributes, Element).

/*
to_atom_ids( In, Ids ) :-
     to_list( In, List ),
     maplist( to_atom_id, List, Ids ).

to_atom_id( Either, Atom ) :-
     ( number(Either) -> number_codes( Either, Codes ), atom_codes( Atom, Codes )
                       ; Atom = Either ).
                       */

pub_graph_treadmill_file_name( true, File, _Di, _Dl, File ).
pub_graph_treadmill_file_name( false, File, Di, Dl, Full ) :-
     number_codes( Dl, DLcs ),
     number_codes( Di, DIcs ),
     reverse( DLcs, DLcsR ),
     reverse( DIcs, DIcsR ),
     pad_codes( DLcsR, DIcsR, 0'0, DFcsR ),
     reverse( DFcsR, DFcs ),
     atom_codes( DF, DFcs ),
     atomic_list_concat( [File,DF], '_d', Full ).
     
/*
options_append( Opts, Defs, All ) :-
     to_list( Opts, OptsList ),
     append( OptsList, Defs, All ).
    */

ext_file( FileIn, Ext, File ) :-
     ( file_name_extension(_,Ext,FileIn) ->
          File = FileIn
          ;
          file_name_extension(FileIn,Ext,File)
     ).
     
pad_codes( [], [], _Code, [] ) :- !.
pad_codes( [_H|T], [], Code, [Code|M] ) :- !,
     pad_codes( T, [], Code, M ).
pad_codes( [_H|T], [F|R], Code, [F|M] ) :- !,
     pad_codes( T, R, Code, M ).

kvs_to_unique_k_v_as_list( [], K, PrvVs, [K-V] ) :-
     ( PrvVs = [V] -> true ; reverse(PrvVs,V) ).
kvs_to_unique_k_v_as_list( [K1-V1|T], K, Vs, KVs ) :-
     ( K == K1 -> 
          Vs2 = [V1|Vs],
          TKVs = KVs
          ;
          Vs2 = [V1],
          ( Vs=[VofK] -> true; reverse(Vs,VofK) ),
          KVs = [K-VofK|TKVs]
     ),
     kvs_to_unique_k_v_as_list( T, K1, Vs2, TKVs ).
     
ord_facts_aggregate_arg( [H|T], N, Agr ) :-
     functor( H, Pname, Arity ),
     findall( On, (between(1,Arity,On),On =\= N), Ons ),
     maplist( term_n_arg(H), Ons, Hons ),
     ord_facts_aggregate_arg( T, Ons, N, Pname/Arity, Hons, [], Agr ).

ord_facts_aggregate_arg( [], Ons, N, Pn/Ar, Hons, HAgrs, [Fact] ) :-
     de_singleton( HAgrs, Agr ),
     functor( Fact, Pn, Ar ),
     maplist( csv_atom_pl_term, Hons, Cons),
     maplist( term_n_arg(Fact), [N|Ons], [Agr|Cons] ).
ord_facts_aggregate_arg( [H1|T], Ons, N, Funct, Hons, HAgrs, Facts ) :-
     term_ons( Ons, H1, Hons1 ),
     arg( N, H1, Nth ),
     ( Hons = Hons1 -> 
          HAgrs2 = [Nth|HAgrs],
          Facts = TFacts 
          ;
          HAgrs2 = [Nth],
          Funct = Pname/Arity,
          functor( Fact, Pname, Arity ),
          de_singleton( HAgrs, Agr ),
          maplist( csv_atom_pl_term, Hons, Cons ),
          maplist( term_n_arg(Fact), [N|Ons], [Agr|Cons] ),
          Facts = [Fact|TFacts]
     ),
     ord_facts_aggregate_arg( T, Ons, N, Funct, Hons1, HAgrs2, TFacts ).

term_n_arg( Term, N, Arg ) :- arg( N, Term, Arg ).

pl_term_csv_atom( date(Yn,Mn,Dn), Csv ) :-
     !,
     maplist( atom_number, [Y,M,D], [Yn,Mn,Dn] ),
     atomic_list_concat( [Y,M,D], '/', Csv ).
pl_term_csv_atom( Term, Csv ) :-
     ( atomic(Term) -> 
          ( Term == [] -> Csv = 0; Csv = Term )
          ;
          ( is_list(Term) ->
               atomic_list_concat( Term, ';', Csv )
               ;
               term_to_atom(Term,Csv)
          )
     ).

csv_atom_pl_term( 0, [] ) :- !.
csv_atom_pl_term( CsvDate, Date ) :-
     \+ CsvDate = [_|_],
     atomic_list_concat( [Y,D,M], '/', CsvDate ),
     !,
     Date = date(Y,D,M).
csv_atom_pl_term( Other, Other ).

csv_row_to_nest_row( Csv, Fact ) :-
     functor( Csv, Pname, Functor ),
     functor( Fact, Pname, Functor ),
     arg( 1, Csv, First ),
     arg( 1, Fact, First ),
     arg( 2, Csv, Second ),
     csv_atom_pl_term( Second, Deutepo ),
     arg( 2, Fact, Deutepo ),
     ( Functor =:= 3 -> 
          arg( 3, Csv, Third ),
          ( (Third=:=0;Third=='0') -> 
               Tpith = []
               ;
               atomic_list_concat( Atoms, ';', Third ),
               maplist( to_number, Atoms, Tpith )
          ),
          arg( 3, Fact, Tpith )
     ).

assert_at( Module, Fact ) :-
     % mapargs( csv_atom_pl_term, Row, Fact ), 
     assert( Module:Fact ).

get_url( Url ) :-
     file_base_name(Url, Base),
     directory_file_path(_Dir, Base, File),
     file_mime_type(File, Mime ),
     mime_file_type( Mime, FType ),
     get_url( Url, File, FType ).

get_url( Url, File, Type ) :-
     setup_call_cleanup(
        http_open(Url, In, []),
        setup_call_cleanup(
        open(File, write, Out, [type(Type)]),
        copy_stream_data(In, Out),
        close(Out)),
        close(In) ).

mime_file_type(text/_, text) :- !.
mime_file_type(_, binary).
% mime_file_type(text/_, binary). % was this changed it above ?

a_month_ago( date(Y1,M1,D1) ) :-
     % date( date(Y,M,D) ),
	get_time( T ),
     stamp_date_time( T, Date, local ), 
	Date = date(Y,M,D,_H,_N,_S,_,_,_),
     ( M =:= 1 -> Y1 is Y - 1,
                  M1 is 12,
                  D is D1   % Dec. has at least as many days as Jan.
                  ;
                  Y1 is Y,
                  M1 is M - 1,
                  % actually for this application D1 is D   should suffice...
                  month_days( M, Ds ),
                  D1 is min(Ds,D)
     ).

month_days(  1, 31 ).
month_days(  2, 28 ).  % ok ok 
month_days(  3, 31 ).
month_days(  4, 30 ).
month_days(  5, 31 ).
month_days(  6, 30 ).
month_days(  7, 31 ).
month_days(  8, 31 ).
month_days(  9, 30 ).
month_days( 10, 31 ).
month_days( 11, 30 ).
month_days( 12, 31 ).

non_var_list( IdS, Ids ) :-
    \+ var(IdS),
    pg_en_list( IdS, Ids ).

de_kv_list_on( [_K-Single], On, Single ) :-
    \+ is_list( On ),
    !.
de_kv_list_on( List, _On, List ).

mapargs( Partial, Goal1, Goal2 ) :-
     functor( Goal1, Gname, Garity ),
     functor( Goal2, Gname, Garity ),
     mapargs_1( Garity, Partial, Goal1, Goal2 ).

mapargs_1( 0, _Partial, _Goal1, _Goal2 ) :- !.
mapargs_1( I, Partial, Goal1, Goal2 ) :-
     % functor( Call, Pname, 2 ),
     arg( I, Goal1, Arg1 ),
     % arg( 1, Call, Arg1 ),
     % arg( 2, Call, Arg2 ),
     call( Partial, Arg1, Arg2 ),
     arg( I, Goal2, Arg2 ),
     K is I - 1,
     mapargs_1( K, Partial, Goal1, Goal2 ).

to_number( Atom, Num ) :-
     atom( Atom ),
     !,
     atom_number( Atom, Num ).
to_number( Num, Num ).

nest_pair_flatten_removes( [], [] ).
nest_pair_flatten_removes( [K-(_-V)|T], [K-V|M] ) :-
     nest_pair_flatten_removes( T, M ).

kv_decompose_vs( [], [] ).
kv_decompose_vs( [_K-V|T], [V|Tv] ) :-
    kv_decompose_vs( T, Tv ).

pg_en_list( List, TheList ) :-
    is_list(List),
    !,
    TheList = List.
pg_en_list( Elem, [Elem] ).

semscholar_id_json( Id, Json ) :-
    url_semscholar( SemScholar ),
    Incl = '?include_unknown_references=true',
    atomic_list_concat( [SemScholar,Id,Incl], '', Url ),
    http_open( Url, In, [] ),
    % json_read_dict( In, Dict ),
    json_read( In, JsonT ),
    close( In ),
    JsonT = json(Json).

json_value( Json, Atom ) :-
    atomic( Json ),
    !,
    Atom = Json.
json_value( Json, Names ) :-
    findall( Name, ( member(json(Sub),Json), 
                     member(name=Name,Sub)
                     ),
                Names ).              
