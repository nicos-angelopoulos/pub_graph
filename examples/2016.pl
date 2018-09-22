?- pub_graph_search( author='Giamas', Ids, true ), assert( giamas(Ids) ).
?- giamas(Gs), pub_graph_summary_display( Gs, _, display(['Author','Title','PubDate','Source']) ).

1
...
45

[debug]  ?- pub_graph_summary_info( 27264793, Results, [] ).
Results = ['Author'-['Marsousi N', 'Samer CF', 'Fontana P', 'Reny JL', 'Rudaz S', 'Desmeules JA', 'Daali Y'], 'Title'-'Co-administration of ticagrelor and ritonavir: Towards prospective dose adjustment to maintain an optimal platelet inhibition using PBPK approach.', 'Source'-'Clin Pharmacol Ther', 'Pages'-[], 'PubDate'-'2016 Jun 6', 'Volume'-[], 'Issue'-[], 'ISSN'-'0009-9236', ... - ...|...].

[debug]  ?- pub_graph_summary_info( 27264794, Results, [] ).
Results = ['Author'-[], 'Title'-[], 'Source'-[], 'Pages'-[], 'PubDate'-[], 'Volume'-[], 'Issue'-[], 'ISSN'-[], ... - ...|...].

[debug]  ?- date.
date(2016, 6, 13).
