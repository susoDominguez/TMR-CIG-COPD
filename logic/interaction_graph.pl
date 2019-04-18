% :- use_rendering(graphviz).

:- include(recommendations).
:- include(transition).
:- include(interactions).

%%	interaction_graph(?Guideline, -Graph)
%
%	Graph is a graphical representation of all recommendations in Guideline and their
%	interactions.

interaction_graph(Guideline, Graph) :-
    guideline_recommendations(Guideline, Recommendations),
    maplist(recommendation_term, Recommendations, Terms),
    findall(interaction(Interaction,Label,Elems,External),
            interaction(Recommendations, Interaction, Label, Elems, External),
            Interactions),
    maplist(arg(4), Interactions, Externals),
    external_transitions(Externals, Transitions),
    append([Terms, Transitions, Interactions], GraphTerm),
    once(graph(Graph, GraphTerm)).

interaction_graph_SWAT4LScaseStudy(Guideline, Graph) :-
    rdf_global_id(data:'CIG-OA-HT-DB',Guideline),
    %guideline_recommendations(Guideline, Recommendations),
    rdf_global_id(data:'RecOA-HT-DB-AvoidGIB_assertion', R1),
    rdf_global_id(data:'RecOA-HT-DB-HT-DB-AntiThrombotic1_assertion', R2),
    rdf_global_id(data:'RecOA-HT-DB-Diuretic_assertion', R3),
    %rdf_global_id(data:'RecOA-HT-DB-ReduceLBS_assertion', R4),
    rdf_global_id(data:'RecOA-HT-DB-Painkiller_assertion', R5),
        Recommendations = [R1,R2,R3,R5],
    maplist(recommendation_term, Recommendations, Terms),
    findall(interaction(Interaction,Label,Elems,External),
            interaction(Recommendations, Interaction, Label, Elems, External),
            Interactions),
    %maplist(arg(4), Interactions, Externals),
    Externals = [ ['http://anonymous.org/data/drugbankIBActAdministerAspirinActAdministerIbuprofen_assertion'],
                  %['http://anonymous.org/data/drugbankCBActAdministerEpoprostenolTrDecreaseBCalways_assertion'],
                  %['http://anonymous.org/data/liddiIBActAdministerClopidogrelActAdministerDipyridamole_assertion'],
                  ['http://anonymous.org/data/siderCBActAdministerIbuprofenTrNoneSitHighBPalways_assertion']
                  %['http://anonymous.org/data/siderCBActAdministerBendroflumethiazideTrNoneSitHighLBSalways_assertion']
                  %['http://anonymous.org/data/aersIB714768_assertion']
                ],
    external_transitions(Externals, Transitions),
    append([Terms, Transitions, Interactions], GraphTerm),
    once(graph(Graph, GraphTerm)).

interaction_graph_caseStudySpringer(Guideline, Graph) :-
    rdf_global_id(data:'CIG-OA-HT-DB',Guideline),
    guideline_recommendations(Guideline, Recommendations),
    maplist(recommendation_term, Recommendations, Terms),
    findall(interaction(Interaction,Label,Elems,External),
            interaction(Recommendations, Interaction, Label, Elems, External),
            Interactions),
    %maplist(arg(4), Interactions, Externals),
    Externals = [ ['http://anonymous.org/data/drugbankIBActAdministerAspirinActAdministerIbuprofen_assertion'],
                  ['http://anonymous.org/data/drugbankCBActAdministerEpoprostenolTrDecreaseBCalways_assertion'],
                  %['http://anonymous.org/data/liddiIBActAdministerClopidogrelActAdministerDipyridamole_assertion'],
                  ['http://anonymous.org/data/siderCBActAdministerIbuprofenTrNoneSitHighBPalways_assertion'],
                  ['http://anonymous.org/data/siderCBActAdministerBendroflumethiazideTrNoneSitHighLBSalways_assertion'],
                  ['http://anonymous.org/data/aersIB714768_assertion']
                ],
    external_transitions(Externals, Transitions),
    append([Terms, Transitions, Interactions], GraphTerm),
    once(graph(Graph, GraphTerm)).

interaction_graph_caseStudy_internal(Guideline, Graph) :-
    rdf_global_id(data:'CIG-OA-HT-DB',Guideline),
    guideline_recommendations(Guideline, Recommendations),
    maplist(recommendation_term, Recommendations, Terms),
    findall(interaction(Interaction,Label,Elems,External),
            interaction(Recommendations, Interaction, Label, Elems, External),
            Interactions),
    Transitions = [ ], %External ones
    append([Terms, Transitions, Interactions], GraphTerm),
    once(graph(Graph, GraphTerm)).

guideline_graph(Guideline, Graph) :-
    guideline_recommendations(Guideline, Recommendations),
    maplist(recommendation_term, Recommendations, Terms),
    once(graph(Graph, Terms)).

recommendation_term(R, recommendation(R, Label, Strength,
                                      transition(TransitionR, TropeType, BeforeValue, ActionL, AfterValue))) :-
    recommendation(R, Label, Strength, TransitionR),
    transition(TransitionR, TropeType, BeforeValue, ActionL, AfterValue).

interaction(Recommendations, Interaction, Label, Elements, External) :-
    sort(Recommendations, RecommendationsSorted),
    interaction(Interaction, Label, Elements),
    ord_intersection(RecommendationsSorted, Elements, Elems),
    Elems = [_|_],
    ord_subtract(Elements, RecommendationsSorted, External).

guideline_recommendations(Guideline, Recommendations) :-
    guideline(Guideline),
    findall(R, rdf(R, vocab:partOf, Guideline), Recommendations).

guideline(GuideLine) :-
    rdf(GuideLine, rdf:type, vocab:'ClinicalGuideline').

external_transitions(Externals, Transitions) :-
    append(Externals, FlatExternals0),
    sort(FlatExternals0, FlatExternals),
    maplist(ext_transition, FlatExternals, Transitions).

ext_transition(TransitionR, transition(TransitionR, TropeType, BeforeValue, ActionL, AfterValue)) :-
    transition(TransitionR, TropeType, BeforeValue, ActionL, AfterValue), !.
ext_transition(TransitionR, transition(TransitionR, ?, ?, ?, ?)).

%%	graph(-Graph, +Data)

graph(digraph([ rankdir='LR',
                ranksep=0.2,
                compound=true
              | Graph
              ]), Data) :-
    phrase(graph(Data), Graph).

graph(Data) -->
    graph(Data, [], Map),
    { maplist(rank_id, Map, RankPairs),
      keysort(RankPairs, Sorted),
      group_pairs_by_key(Sorted, Grouped)
    },
    same_ranks(Grouped).

rank_id(_-node(NodeID), recommendation-NodeID).
rank_id(_-cluster(ClusterID,_NodeID), cluster-ClusterID).

same_ranks([]) --> [].
same_ranks([Type-Members|T]) --> same_rank(Type, Members), same_ranks(T).

same_rank(cluster, _Members) -->
    [].
same_rank(_, Members) -->
    [ group([rank=same|Members]) ].

graph([], Map, Map) --> [].
graph([H|T], Map0, Map) -->
    recommendation_node(H, Map0, Map1), !,
    graph(T, Map1, Map).
graph([H|T], Map0, Map) -->
    ext_transition_node(H, Map0, Map1), !,
    graph(T, Map1, Map).
graph([H|T], Map0, Map) -->
    interaction_node(H, Map0, Map1), !,
    graph(T, Map1, Map).

recommendation_node(recommendation(RR, Target, Strength,
                                   transition(TR, Quantity,Before,Action,After)),
          	Map0, [RR-node(TargetID), TR-cluster(TransitionID, BeforeID)|Map0]) -->
    { cliopatria_href(TR, THREF),
      transition(Quantity, Before, Action, After, TransitionID, BeforeID, Cluster,
                 [href=THREF, target='cliopatria-localview']),
      gensym(t, TargetID),
      strength(Strength, EdgeAttr),
      cliopatria_href(RR, HREF)
    },
    [ node(TargetID, [ shape(box), style(rounded), label(Target), width(2),
                       href(HREF), target('cliopatria-localview')]),
      Cluster,
      edge(TargetID -> BeforeID, [lhead=TransitionID|EdgeAttr])
    ].

strength("do",         [penwidth=8, label="do", color="#7DC5F2"]).
strength("do not",     [penwidth=8, label="do not", color="#FF2E05"]).
strength('should',     [penwidth=8, label="should", color="#7DC5F2"]).
strength('should not', [penwidth=8, label="should not", color="#FF2E05"]).
strength('should-not', [penwidth=8, label="should not", color="#FF2E05"]).

ext_transition_node(transition(TR, Quantity,Before,Action,After),
                    Map0, [TR-node(NodeID)|Map0]) -->
    { transition(Quantity, Before, Action, After, TransitionID, BeforeID, Cluster,
                 [ style=filled, fillcolor="#eeeeee", href=HREF, target='cliopatria-localview' ]),
      gensym(v, NodeID),
      cliopatria_href(TR, HREF),
      ext_label(TR, ExtLabel, ExtHREF)
    },
    [ node(NodeID, [shape(box3d), fillcolor("#eeeeee"), style(filled), label(ExtLabel),
                   href(ExtHREF), target('cliopatria-localview')]),
      Cluster,
      edge(NodeID -> BeforeID, [lhead=TransitionID,arrowhead="none"])
    ].

ext_label(TR, Label, HREF) :-
    rdf(TR, prov:'wasDerivedFrom', Source),
	rdf(Source, rdfs:label, literal(Label)),
    rdf(TR, prov:'wasGeneratedBy', ActivityExtResources),
    cliopatria_href(ActivityExtResources, HREF).
ext_label(_TR, "External", '').

interaction_node(interaction(IR, Label, Elems, _Externals), Map, Map) -->
    { maplist(handle(Map), Elems, [H1|Handles]),
      interaction_color(Label, Color),
      labelBreak(Label, NewLabel),
      gensym(i, ID),
      cliopatria_href(IR, HREF)
    },
    [ node(ID, [ shape(none), fillcolor(Color), style(filled),
                 width=0,height=0,margin=0.02, label(NewLabel),
                 href=HREF, target='cliopatria-localview'
               ])
    ],
    interaction_link(H1, ID, []),
    interaction_links(Handles, ID).
interaction_node(_Rel, Map, Map) -->
    [].
%   { format('Could not link ~p~n~p~n', [Rel, Map]) }.

labelBreak('External-Incompatible Effects', 'External-\nIncompatible\nEffects').
labelBreak('External-Incompatible Actions', 'External-\nIncompatible\nActions').
labelBreak('External-Alternative Action', 'External-\nAlternative\nAction').
labelBreak('Contradictory Norms', 'Contradictory\nNorms').
labelBreak('Alternative Actions', 'Alternative\nActions').
labelBreak('Repeated Action', 'Repeated\nAction').
labelBreak('Reparable Transition', 'Reparable\nTransition').
labelBreak(Label, Label).

handle(Map, R, Node) :-
    memberchk(R-Node, Map).

interaction_links([], _) --> [].
interaction_links([H|T], ID) -->
    interaction_link(H, ID, [constraint=false]),
    interaction_links(T, ID).

interaction_link(node(NodeID), ID, Attrs) -->
    [ edge(ID->NodeID, Attrs) ].
interaction_link(cluster(ClusterID, NodeID), ID, Attrs) -->
    [ edge(ID->NodeID, [lhead=ClusterID|Attrs]) ].

interaction_color(contradiction, "#dc79ff") :- !.
interaction_color('External-Incompatible Effects', "#dc79ff") :- !.
interaction_color('External-Incompatible Actions', "#dc79ff") :- !.
interaction_color('Contradictory Norms', "#dc79ff") :- !.
interaction_color(alternative, "#D4FB79") :- !.
interaction_color('Alternative Actions', "#D4FB79") :- !.
interaction_color('External-Alternative Action', "#D4FB79") :- !.
interaction_color('Reparable Transition', "#D4FB79") :- !.
interaction_color(_, yellow).

transition(Quantity, Before, Action, After, ClusterID, BeforeID,
           subgraph(ClusterID, [ BeforeID->ActionID->AfterID, label=Quantity,
                                 node(BeforeID, [shape(box), label=Before, width=W]),
                                 node(AfterID,  [shape(box), label=After,  width=W]),
                                 node(ActionID, [shape(ellipse), label=ActionL, style=dashed])
                               | Attrs
                               ]),
           Attrs) :-
    wrap(Action, ActionL),
    W = 1,
    gensym(n, BeforeID),
    gensym(n, ActionID),
    gensym(n, AfterID),
    gensym(cluster, ClusterID).

wrap(Administer, Label) :-
    atom_concat('Administer ', What, Administer), !,
    format(atom(Label), 'Administer\n~w', [What]).
wrap(Label, Label).

%%	cliopatria_href(+URI, -HREF)
%
%	HREF opens the ClioPatria localview for URI.

cliopatria_href(URI, HREF) :-
    uri_encoded(query_value, URI, Enc),
    format(atom(HREF), '/browse/list_resource?r=~w', [Enc]).

/** <examples>
?- interaction_graph(Guideline, Graph).
?- interaction_graph('http://anonymous.org/data/CIG-DB', Graph).
?- interaction_graph(Guideline, Graph, Exts), member(Ext, Exts).
?- interaction_graph_caseSudy(Graph).
*/
