xquery version "3.1";

(: ####++++----  

    Functions for extracting node indices (sal:index) from TEI works; also includes functionality for making 
    citeIDs, labels, and crumbtrails.
   
   ----++++#### :)

module namespace index    = "https://www.salamanca.school/factory/works/index";

declare namespace tei     = "http://www.tei-c.org/ns/1.0";
declare namespace sal     = "http://salamanca.adwmainz.de";
declare namespace admin   = "http://www.salamanca.school/xquery/admin";

declare namespace exist   = "http://exist.sourceforge.net/NS/exist";
declare namespace map     = "http://www.w3.org/2005/xpath-functions/map";
declare namespace range   = "http://exist-db.org/xquery/range";
declare namespace util    = "http://exist-db.org/xquery/util";
declare namespace xi      = "http://www.w3.org/2001/XInclude";
declare namespace xmldb   = "http://exist-db.org/xquery/xmldb";

import module namespace console = "http://exist-db.org/xquery/console";
import module namespace functx  = "http://www.functx.com";

import module namespace config = "http://www.salamanca.school/xquery/config"      at "xmldb:exist:///db/apps/salamanca/modules/config.xqm";
import module namespace sutil  = "http://www.salamanca.school/xquery/sutil"       at "xmldb:exist:///db/apps/salamanca/modules/sutil.xqm";
import module namespace txt    = "https://www.salamanca.school/factory/works/txt" at "xmldb:exist:///db/apps/salamanca/modules/factory/works/txt.xqm";

declare option exist:timeout "86400000"; (: 24 h :)
declare option exist:output-size-limit "5000000"; (: max number of nodes in memory :)

(: CONFIG :)

declare variable $index:citeIDConnector := '.';
declare variable $index:labelConnector := ' ';
declare variable $index:crumbtrailConnector := ' » ';


(: NODE INDEX functions :)
(: See /docs/Technical.md for some information on what happens here :)

(:
~ Controller function for creating (and reporting about) node indexes.
~ Here's what it does:
~ - It determines at which depth to segment a work
~ - It resolves all XIncludes
~ - It calls index:getFragmentNodes() in order to build the set of nodes constituting html fragments (the "target-set")
~ - It collects all nodes that we should be keeping track of (defined in index:isIndexNode())
~ - For all these nodes, it registers in which fragment they end up
~   (this is the nearest ((ancestor-or-self or descendant that has no preceding siblings) that is also contained in the target-set))
:)
declare function index:makeNodeIndex($tei as element(tei:TEI)) as map(*) {
    let $wid := xs:string($tei/@xml:id)

    let $debug := if ($config:debug = ("trace", "info")) then console:log("[INDEX] Indexing " || $wid || " ...") else ()

    (: First, get fragmentation level requirement :)
    let $debug := if ($config:debug = ("trace")) then console:log("[INDEX] Node indexing: Find out required fragmentation depth ...") else ()
    let $fragmentationDepth := index:determineFragmentationDepth($tei)
    let $debug := if ($config:debug = ("trace", "info")) then console:log("[INDEX] Node indexing: fragmentation level " || $fragmentationDepth || ".") else ()

    let $xincludes := $tei//tei:text//xi:include/@href
    let $work      := if (count($xincludes) gt 0) then
                            util:expand($tei)
                      else
                            $tei
    let $texts := $work//tei:text[(@type eq 'work_volume' and sutil:WRKisPublished($wid || '_' || @xml:id)) or @type eq 'work_monograph']

    (: Next, get all relevant nodes :)
    let $debug := if ($config:debug = ("trace", "info")) then console:log("[INDEX] Node indexing: Get various sets of nodes ...") else ()
    let $nodeCandidates := $texts/descendant-or-self::tei:*
    let $specialNodes := index:gatherSpecialNodes($nodeCandidates)

    (: Next, get future fragment nodes :)
    let $targetSet := index:getFragmentNodes($work, $fragmentationDepth, $specialNodes)
    let $targetSetNodeTypes := distinct-values($targetSet ! (local-name(.)))
    let $debug := if ($config:debug = ("trace", "info")) then console:log("[INDEX] Target set contains " || count($targetSet) || " nodes (to become html fragments) of these types: " || string-join($targetSetNodeTypes, ", ") || ". Acquiring all nodes that have to be indexed ...") else ()

    let $debug := if ($config:debug = ("trace", "info")) then
                    console:log("[INDEX] Node indexing: " || count($specialNodes('indexNodes')) || " nodes "
                                || " (" || count($specialNodes('pageNodes')) || " unique pages) to process. Identifying fragment Ids for all to-be-indexed nodes...")
                  else ()

    (: Create the fragment id for each node beforehand, so that recursive crumbtrail creation has it readily available :)
    let $fragmentIds :=
        map:merge(
            for $node at $pos in $specialNodes('indexNodes')
                let $debug :=   if (($config:debug = "trace") and ($pos mod 1000 eq 0)) then
                                    console:log("[INDEX] Node indexing: processing node no. " || string($pos)  || " ...")
                                else ()
                let $n := $node/@xml:id/string()
(:
                let $debug := if ($config:debug = "trace") then
                                 console:log("[INDEX] node " || $n || " (at pos " || $pos || ")...")
                              else ()
                let $debug := if ($config:debug = "trace") then
                                 console:log("[INDEX] count($node/ancestor-or-self::tei:*)=" || count($node/ancestor-or-self::tei:*))
                              else ()
                let $debug := if ($config:debug = "trace") then
                                 console:log("[INDEX] count($node//tei:*)=" || count($node//tei:*))
                              else ()
                let $debug := if ($config:debug = "trace") then
                                 console:log("[INDEX] some $n in ($node/ancestor-or-self::tei:* | $node//tei:*) satisfies $n = $targetSet: " || xs:boolean(some $n in ($node/ancestor-or-self::tei:* | $node//tei:*) satisfies $n = $targetSet))
                              else ()
:)
                let $frag := (($node/ancestor-or-self::tei:* | $node//tei:*) intersect $targetSet)[1]
(:                let $frag := (($node/ancestor-or-self::tei:* | $node//*[not(preceding-sibling::tei:*)]) intersect $targetSet)[1]:)
(:  
              let $debug := if ($config:debug = "trace") then
                                 console:log("[INDEX] ... gives $frag with @xml:id " || $frag/@xml:id)
                              else ()
:)
                let $err  := if (not($frag)) then
                                let $debug := if ($config:debug = ("trace", "info")) then
                                                 console:log("[INDEX] Node indexing: Could not find $frag for $node " || $n || ". Aborting.")
                                              else ()
                                return error(QName('http://salamanca.school/err', 'FragmentationProblem'),
                                             'Could not find $frag for ' || $n || '.')
                             else ()
                let $err2  := if (not($frag/@xml:id)) then
                                let $debug := if ($config:debug = ("trace", "info")) then
                                                 console:log("[INDEX] Node indexing: Could not find $frag/@xml:id for $node " || $n || ". Aborting.")
                                              else ()
                                return error(QName('http://salamanca.school/err', 'FragmentationProblem'),
                                             'Could not find $frag/@xml:id for ' || $n || '.')
                             else ()
                let $err3 := if (xs:string($frag/@xml:id) eq "") then
                                let $debug := if ($config:debug = ("trace", "info")) then
                                                 console:log("[INDEX] Node indexing: $frag/@xml:id is empty string for $node " || $n || ". Aborting.")
                                             else ()
                                return error(QName('http://salamanca.school/err', 'FragmentationProblem'),
                                             '$frag/@xml:id is empty string for $node ' || $n || '.')
                             else ()
(:
                let $debug := if ($config:debug = "trace") then
                                 console:log("[INDEX] sanity tests passed. Create FragmentId...")
                              else ()
:)
                (: make fragment id with fragment number (with leading zeroes) and xml:id :)
                let $fragId := index:makeFragmentId(functx:index-of-node($targetSet, $frag), $frag/@xml:id/string())
  
(:
                let $debug :=   if ($config:debug = "trace") then
                                    console:log("[INDEX] Node indexing: node " || $n || " ends up in frag " || $fragId)
                                else ()
:)
                return map:entry($n, $fragId)
        )
    let $debug := if ($config:debug = ("trace")) then console:log("[INDEX] Node indexing: fragment ids extracted. Creating index file (stage 1) ...") else ()

    (: node indexing has 2 stages: :)
    (: 1.) extract nested sal:nodes with rudimentary information :)
    let $debug := if ($config:debug = ("trace")) then console:log("[INDEX] extractNodeStructure for these text nodes: " || string-join($texts/@xml:id, ", ") || "...") else ()
    let $indexTree := 
(:            {index:extractNodeStructure($wid, ($work//tei:text[not(ancestor::tei:text)] intersect $nodeCandidates), $xincludes, $fragmentIds, $specialNodes)}:)
        <sal:index>
            {index:extractNodeStructure($wid, $texts, $xincludes, $fragmentIds, $specialNodes)}
        </sal:index>

    (: 2.) flatten the index from 1.) and enrich sal:nodes with full-blown citeIDs, etc. :)
    let $debug := if ($config:debug = ("trace")) then console:log("[INDEX] Node indexing: stage 1 finished. " ||
                                                                    string(count($indexTree//sal:node[@type eq "pb"])) || " page and " ||
                                                                    string(count($indexTree//sal:node[@type eq "milestone"])) || " milestone nodes. " ||
                                                                    "Cont'ing with stage 2 ...") else ()
    let $index := 
        <sal:index work="{$wid}" xml:space="preserve">
            {index:constructIndexNodes($indexTree)}
        </sal:index>
    let $debug := if ($config:debug = ("trace")) then console:log("[INDEX] Node indexing: stage 2 finished. " ||
                                                                    string(count($index//sal:node[@type eq "pb"])) || " page and " ||
                                                                    string(count($index//sal:node[@type eq "milestone"])) || " milestone nodes. " ||
                                                                    "Cont'ing with quality check ...") else ()
        
    let $check := index:qualityCheck($index, $work, $targetSet, $fragmentationDepth, $specialNodes)
    let $debug := if ($config:debug = ("trace")) then console:log("[INDEX] Node indexing: Quality control finished, node indexing finished.") else ()
        
    return 
        map {
            'index': $index,
            'missed_elements': $check('missed_elements'),
            'unidentified_elements': $check('unidentified_elements'),
            'fragmentation_depth': $fragmentationDepth,
            'target_set_count': count($targetSet)
        }
};

declare function index:gatherSpecialNodes($nodeCandidates as element(*)*) as map(*) {
    (: ~ QC: Anchor nodes occur within main nodes only. Anchor nodes are informative, as opposed to technical or decorative milestones. :)
    let $anchorNodes       := filter($nodeCandidates, function($a) {$a[self::tei:milestone[@unit ne 'other'][@xml:id]]})
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($anchorNodes) || " $anchorNodes.") else ()

    (: ~ QC: Marginal nodes occur within main nodes. :)
    let $marginalNodes     := filter($nodeCandidates, function($a) {$a[self::tei:note | self::tei:label][@place eq 'margin'][@xml:id]})
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($marginalNodes) || " $marginalNodes.") else ()

    (: ~ QC: Page nodes occur within main nodes and have no content. :)
    let $pageNodes         := filter($nodeCandidates, function($a) {$a[self::tei:pb[not(@sameAs or @corresp)][@xml:id]]})
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($pageNodes) || " $pageNodes.") else ()

    (: ~ QC: Structural nodes contain only other structural or main nodes. Structural nodes are are high-level nodes in the document hierarchy. :)
    let $structuralNodes   := filter($nodeCandidates, function($a) {$a[self::tei:text[@type eq 'work_volume'][@xml:id]  |
                                                                       self::tei:div[@type ne "work_part"][@xml:id]     |
                                                                       self::tei:back[@xml:id]                          |
                                                                       self::tei:body[@xml:id]                          |
                                                                       self::tei:front[@xml:id]                         |
                                                                       self::tei:titlePage[@xml:id] 
                                                                      ]}
                                )
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($structuralNodes) || " $structuralNodes.") else ()

    (: ~ Main nodes are more low-level mixed-content elements such as tei:p, which may contain marginal or anchor nodes. :)
    let $mainNodes         := filter($nodeCandidates, function($a) {$a[self::tei:p[@xml:id]                            |
                                                                       self::tei:head[@xml:id]                         |
                                                                       self::tei:signed[@xml:id]                       |
                                                                       self::tei:byline[@xml:id]                       |
                                                                       self::tei:titlePart[@xml:id]                    |
                                                                       self::tei:docTitle[@xml:id]                     |
                                                                       self::tei:docDate[@xml:id]                      |
                                                                       self::tei:docEdition[@xml:id]                   |
                                                                       self::tei:docImprint[@xml:id]                   |
                                                                       self::tei:imprimatur[@xml:id]                   |
                                                                       self::tei:lg[@xml:id]                           |
                                                                       self::tei:label[@place ne 'margin'][@xml:id]    |
                                                                       self::tei:argument[@xml:id]                     |
                                                                       self::tei:table[@xml:id]
                                                                      ][not( ancestor::tei:list                             |
                                                                             ancestor::tei:note[@place eq 'margin']         |
                                                                             ancestor::tei:label[@place eq 'margin']
                                                                            )
                                                                      ]}
                                      )
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($mainNodes) || " $mainNodes.") else ()

    (: ~ List nodes are lists and certain nodes within lists (head, argument) that would otherwise be main nodes. :)
    let $listNodes          := filter($nodeCandidates, function($a) {$a[self::tei:list[@xml:id]                         |
                                                                        self::tei:item[@xml:id]                         |
                                                                        self::tei:head[ancestor::tei:list][@xml:id]     |
                                                                        self::tei:argument[ancestor::tei:list][@xml:id]
                                                                       ](: [not ( ancestor::tei:* intersect ( $marginalNodes | $mainNodes ) ) ] :)
                                                                     }
                                     )
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($listNodes) || " $listNodes.") else ()

    (: ~ Index nodes are all nodes that are indexed. :)
    let $indexNodes         := $anchorNodes | $marginalNodes | $pageNodes | $structuralNodes | $mainNodes | $listNodes
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($indexNodes) || " $indexNodes.") else ()

    (: Citable nodes are all nodes that will appear in citeIDs. For body, some main nodes and second-level list nodes, we prefer an ancestor's citeID :)
    let $nonCiteableMainNodes   := filter($mainNodes, function($a) {$a[self::tei:docTitle      |
                                                                       self::tei:docDate       |
                                                                       self::tei:docEdition    |
                                                                       self::tei:docImprint    |
                                                                       self::tei:imprimatur    |
                                                                       self::tei:titlePart     |
                                                                       self::tei:head          |
                                                                       self::tei:label         |
                                                                       self::tei:byline        |
                                                                       self::tei:signed
                                                                      ]}
                                            )
    let $nonCiteableStructNodes := filter($structuralNodes, function($a) {$a[self::tei:body]})
    let $nonCiteableListNodes   := filter($listNodes, function($a) {$a[self::tei:list     |
                                                                       self::tei:head
                                                                      ][ancestor::tei:list]
                                                                    }
                                         )
    let $nonCiteableNodes       := $nonCiteableMainNodes | $nonCiteableStructNodes | $nonCiteableListNodes
    let $citableNodes           := $indexNodes except $nonCiteableNodes
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($citableNodes) || " $citableNodes.") else ()

    (: Eligible fragment nodes are nodes that can constitute an html fragment: Structural nodes, titlePage and first- and second-level lists :)
    let $eligibleFragmentNodes  := $structuralNodes |
                                    filter($nodeCandidates, function($a) {$a[self::tei:titlePage]}) |
                                    filter($nodeCandidates, function($a) {$a[self::tei:list][count(ancestor::tei:list) le 1]})
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($eligibleFragmentNodes) || " $eligibleFragmentNodes.") else ()

    (: ~ Determines which nodes have labels. :)
    let $labelNodes         := filter($indexNodes, function($a) {$a[self::tei:text[@type eq 'work_volume']                             |
                                                                    self::tei:div[$config:citationLabels(@type)?('isCiteRef')]         |
                                                                    self::tei:milestone[$config:citationLabels(@unit)?('isCiteRef')]   |
                                                                    self::tei:pb[not(@sameAs or @corresp)]                             |
                                                                    self::tei:*[$config:citationLabels(local-name(.))?('isCiteRef')]
                                                                   ][not(ancestor::tei:note)]
                                                                 }
                                     )
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($labelNodes) || " $labelNodes.") else ()

    (: ~ Determines whether a node is a specific citeID element, i.e. one that is specially prefixed in citeIDs. Ex.: pages (p), volumes (vol), notes (n) :)
    let $namedCiteIDNodes   := $anchorNodes | $marginalNodes | $pageNodes   |
                                ( $indexNodes ! (.[self::tei:text[@type eq 'work_volume']    | 
                                                   self::tei:back                            | 
                                                   self::tei:front] ) )     |
                                ( $mainNodes  ! (.[self::tei:titlePage] ) ) |
                                ( $listNodes  ! (.[self::tei:list[@type = ('dict', 'index')] |
                                                   self::tei:item[ancestor::tei:list[@type eq 'dict']]] ) ) 
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($namedCiteIDNodes) || " $namedCiteIDNodes.") else ()
    (: ~ Determines whether a node is a 'generic' citeID element, i.e. one that isn't specially prefixed in citeIDs. complement of namedCiteIDNodes :)
    let $unnamedCiteIDNodes := $indexNodes except $namedCiteIDNodes
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($unnamedCiteIDNodes) || " $unnamedCiteIDNodes.") else ()

    (: ~ Basic nodes represent *all* container nodes at the bottom of the index tree,
         i.e. mixed-content elements that comprise all text nodes in a sequential, non-overlapping manner. 
         To be used for Sphinx snippets, for checking consistency etc. :)
    let $basicNodes         := $mainNodes | $marginalNodes |
                                ( $listNodes  ! (.[self::tei:list][not(descendant::tei:list)] |
                                                 .[self::tei:item | self::tei:argument][not(descendant::tei:list)][following-sibling::tei:item[./tei:list[. intersect $listNodes]]]
                                                )
                                )
    let $debug := if ($config:debug = "trace") then console:log("[INDEX] " ||
                                                                    count($basicNodes) || " $basicNodes.") else ()

    return
        map{
            'anchorNodes'           : $anchorNodes,
            'marginalNodes'         : $marginalNodes,
            'pageNodes'             : $pageNodes,
            'structuralNodes'       : $structuralNodes,
            'mainNodes'             : $mainNodes,
            'listNodes'             : $listNodes,
            'indexNodes'            : $indexNodes,
            'citableNodes'          : $citableNodes,
            'eligibleFragmentNodes' : $eligibleFragmentNodes,
            'labelNodes'            : $labelNodes,
            'namedCiteIDNodes'      : $namedCiteIDNodes,
            'unnamedCiteIDNodes'    : $unnamedCiteIDNodes,
            'basicNodes'            : $basicNodes
        }
};

(:
~ Determines the fragmentation depth of a work, i.e. the hierarchical level of nodes within a TEI dataset which serve
~ as root nodes for spltting the dataset into HTML fragments.
~ We are setting it as Processing Instructions in the TEI files: <?svsal htmlFragmentationDepth="4"?>
:)
declare function index:determineFragmentationDepth($work as element(tei:TEI)) as xs:integer {
    if ($work//processing-instruction('svsal')[matches(., 'htmlFragmentationDepth="\d{1,2}"')]) then
        xs:integer($work//processing-instruction('svsal')[matches(., 'htmlFragmentationDepth="\d{1,2}"')][1]/replace(., 'htmlFragmentationDepth="(\d{1,2})"', '$1'))
    else $config:fragmentationDepthDefault
};


(: 
~ A rule picking those elements that should become the fragments for HTML-rendering a work.
~ Requires an expanded(!) TEI work's dataset.
~ Here's what it does:
~ - It finds all tei nodes that have n ancestors up to the root node,
~   where n is a configurable value that is extracted (via index:determineFragmentationDepth) from the TEI file itself.
~ - Then, if a node is member of our set of "structural nodes" (div, front etc., defined in index:isStructuralNode),
~   it goes into the target set; otherwise its nearest ancestor that is a "structural node" does.
~ - Finally, we return the distinct nodes, i.e. no duplicates
~   (two non-structural nodes of the desired level may have the same ancestor)
~ - (Obsolete: In front and back, fragmentation must not go below the child level, since we don't expect child fragments be too large here.)
:)
declare function index:getFragmentNodes($work as element(tei:TEI), $fragmentationDepth as xs:integer, $specialNodes as map(*)) as node()* {
    for $text in $work//tei:text[@type eq 'work_monograph' or
                                (@type eq 'work_volume' and sutil:WRKisPublished($work/@xml:id/string() || "_" || @xml:id/string()))] return
            let $debug  :=  if ($config:debug = "trace") then
                                console:log("[INDEX] Build set of nodes at level " || $fragmentationDepth || " ...")
                            else ()
            let $candidates := $text//tei:*[count(./ancestor-or-self::tei:*) eq $fragmentationDepth]
            let $debug  :=   if ($config:debug = "trace") then
                                console:log("[INDEX] Found " || count($candidates) || " candidate nodes for " || string($text/@xml:id) || ".")
                             else ()
            let $result := if ($candidates) then
                                functx:distinct-nodes(
                                    for $node in $candidates return
                                        if ($node intersect $specialNodes('eligibleFragmentNodes')) then
                                            $node
                                         else
                                            ($node/ancestor::tei:* intersect $specialNodes('eligibleFragmentNodes'))[last()]
                                )
                           else
                                $text
            return $result
};


(:
~ Creates a tree of index nodes (sal:node), where nodes are hierarchically nested
~   according to the hierarchy of nodes in the original TEI tree.
~ Supplies nodes with basic information (@title, @type etc.),
~   while temporary elements/attributes provide information that can be used
~   for the production of citeID, crumbtrails etc. in the 
~   final index creation through index:constructIndexNodes().
:)
declare function index:extractNodeStructure($wid as xs:string,
                                            $input as node()*,
                                            $xincludes as attribute()*,
                                            $fragmentIds as map()?,
                                            $specialNodes as map(*)) as element(sal:node)* {
    for $node in $input return
        typeswitch($node)
            case element() return
                let $children := $node/*
                let $dbg := if ($node/self::tei:pb and functx:is-a-number($node/@n)) then
                                let $pag := number($node/@n)
                                return if ($pag mod 100 eq 0 and $config:debug = "trace") then
                                          let $log := util:log('warn', '[INDEX] Processing tei:pb node ' || $node/@n)
                                          return console:log('[INDEX] Processing tei:pb ' || $node/@n || ' ...')
                                       else ()
                            else ()
                return
                    if ($node/@xml:id and $fragmentIds($node/@xml:id/string())) then
                        let $subtype := 
                            if ($node[self::tei:milestone]/@n) then (: TODO: where is this used? Should this be @unit? :)
                                string($node/@n)
                            else if ($node/@type) then
                                string($node/@type)
                            else ()
                        let $isNamedCiteIDNode  := boolean($node intersect $specialNodes('namedCiteIDNodes'))
                        let $class              := index:dispatch($node, 'class', $specialNodes)
                        let $title              := index:dispatch($node, 'title', $specialNodes)
                        let $citableParent      := index:getCitableParent($node, $specialNodes)/@xml:id/string()
                        let $fragmentId         := $fragmentIds(xs:string($node/@xml:id))
                        let $crumb              := index:makeCrumb($wid, $node, $fragmentIds, $specialNodes)
                        let $childnodes         := index:extractNodeStructure($wid, $children, $xincludes, $fragmentIds, $specialNodes)
                        let $citAttribute       := if ( $node intersect $specialNodes('citableNodes') ) then attribute cit   {index:dispatch($node, 'citeID', $specialNodes)} else ()
                        let $labelAttribute     := if ( $node intersect $specialNodes('labelNodes')   ) then attribute label {index:dispatch($node, 'label', $specialNodes)}  else ()
                        let $xincAttribute      := if ($node/@xml:id eq 'completeWork' and $xincludes)  then attribute xinc  {$xincludes} else ()
                        let $salNode :=
                            element sal:node {
                                attribute type              {local-name($node)}, 
                                attribute subtype           {$subtype}, 
                                attribute xml:id            {xs:string($node/@xml:id)},
                                attribute class             {$class},
                                attribute title             {$title},
                                attribute citableParent     {$citableParent},
                                attribute fragment          {$fragmentId},
                                $xincAttribute,
                                $citAttribute,
                                $labelAttribute,
                                element sal:crumb           {$crumb},
                                element sal:children        {$childnodes}
                            }
                        return $salNode
                    else
                        index:extractNodeStructure($wid, $children, $xincludes, $fragmentIds, $specialNodes)
            default
                return ()
};

(:
~ Creates a flat structure of index nodes (sal:node) from a hierarchically structured preliminary index (see index:extractNodeStructure()),
~ while enriching those nodes with final citeIDs, crumbtrails, etc.
:)
declare function index:constructIndexNodes($input as element(sal:index)) as element(sal:node)* {
    for $node in $input//sal:node return
        let $dbg := if ($node[@type eq "pb"]) then
                        let $pos := count($node/preceding::sal:node[@type eq "pb"]) + 1
                        return  if ($pos mod 100 eq 0 and $config:debug = "trace") then
                                      let $log := util:log('warn', '[INDEX] Processing tei:pb node ' || $node/@citeID)
                                      return console:log('[INDEX] Processing tei:pb ' || $node/@citeID/string() || ' ...')
                                else ()
                    else ()

        let $citeAttribute := if ($node/@cit) then attribute citeID {index:constructCiteID($node)} else ()
        let $label         := index:constructLabel($node)
        let $crumbtrail    := index:constructCrumbtrail($node)
        return
            element sal:node {
                (: copy all attributes from the previous node, except for those we compute anew :)
                $node/@* except ( (: TODO: re-insert this $node/@cit, :) $node/@xml:id, $node/@label),
                attribute n {xs:string($node/@xml:id)},
                $citeAttribute,
                attribute label {$label},
                element sal:crumbtrail {$crumbtrail}
            }
};


(: Conducts some basic quality checks with regards to consistency, uniqueness of citeIDs, etc. within an sal:index :)
(: For some reason, this stresses only one cpu core mostly... :) 
declare function index:qualityCheck($index as element(sal:index), 
                                    $work as element(tei:TEI), 
                                    $targetNodes as element()*, 
                                    $fragmentationDepth as xs:integer,
                                    $specialNodes as map(*)) {
                                    
    let $wid := $work/@xml:id

    let $removeTempfile :=  if (doc-available($config:temp-root || "/" || $wid || "_nodeIndex.xml")) then
                                xmldb:remove($config:temp-root, $wid || "_nodeIndex.xml")
                            else ()
    let $saveTempfile := xmldb:store($config:temp-root, $wid || "_nodeIndex.xml", $index)

    (: #### Basic quality / consistency check #### :)
    let $resultNodes := $index//sal:node[not(@n = ('completeWork', 'work_Volume'))]
    let $testNodes := 
        if (count($resultNodes) eq 0) then 
            error(QName('http://salamanca.school/err', 'CreateNodeIndex'), 'Node indexing did not produce any results.') 
        else ()
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 1 passed.") else ()

    (: every ordinary sal:node should have all of the required fields and values: :)
    let $testAttributes := 
        if ($testNodes[not(@class and @type and @n)]) then 
            error(QName('http://salamanca.school/err', 'CreateNodeIndex'), 'Essential attributes are missing in at least one index node (in ' || $wid || ')') 
        else ()
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 2 passed.") else ()

    let $testChildren := if ($testNodes[not(@title and @fragment and @citableParent and @citeID and @label and sal:crumbtrail/*)]) then error() else ()
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 3 passed.") else ()

    (: there should be as many distinctive citeIDs and crumbtrails as there are ordinary sal:node elements: :)
    let $testAmbiguousCiteIDs := 
        if (count($resultNodes[@citeID]) ne count(distinct-values($resultNodes/@citeID))) then
            let $debug := console:log('[WARN] count($resultNodes)=' || string(count($resultNodes)) || ' ne count(distinct-values($resultNodes/@citeID)) = ' || count(distinct-values($resultNodes/@citeID)) || '.')
            let $debug := console:log('[WARN] count($resultNodes/@citeID)=' || string(count($resultNodes/@citeID)) || '.')
            let $allIDs := distinct-values($resultNodes/@citeID/string())
            let $unambiguousIDs := for $i in $allIDs return if (count($resultNodes[@citeID eq $i]) eq 1) then $i else () (: $allIDs ! (.[count($resultNodes[@citeID = .]) eq 1]) :)
            let $ambiguousIDs   := for $i in $allIDs return if (count($resultNodes[@citeID eq $i]) gt 1) then $i else () (: $allIDs ! (.[count($resultNodes[@citeID = .]) gt 1]) :)
            let $ambiguousNodes := $resultNodes[@citeID = $ambiguousIDs]

            (: let $problematicNodes := $resultNodes[@citeID eq preceding::sal:node/@citeID] :)
            let $problematicNodes := $resultNodes[index-of($resultNodes/@citeID, @citeID)[2]]
            let $debug := console:log('[WARN] ' || string(count($problematicNodes)) || ' $problematicNodes: ' || string-join($problematicNodes/@n/string(), ', ') || '.')
            let $debug := console:log('[WARN] ' || string(count($problematicNodes)) || ' citeIDs: ' || string-join($problematicNodes/@citeID/string(), ', ') || '.')
            let $debug := console:log('[WARN] ' || string(count($ambiguousIDs)) || ' ambiguous IDs: ' || string-join($ambiguousIDs, ', ') || '.')
            let $debug := console:log('[WARN] ' || string(count($ambiguousNodes)) || ' ambiguous nodes: ' || serialize($ambiguousNodes) || '.')
            let $log := if ($config:debug = ("trace", "info")) then
                            let $line1 := util:log('error', '[WARN] Could not produce a unique citeID for each sal:node (in ' || $wid || '). Problematic nodes: '
                                    || string-join($problematicNodes/@n, '; '))
                            let $line2 := util:log('error', serialize(//sal:node[@citeID = ./following::sal:node/@citeID]))
                            return $line2
                        else ()
            let $dbg := if ($config:debug = ("trace", "info")) then
                            console:log('[WARN] Could not produce a unique citeID for each sal:node (in ' || $wid || '). Problematic nodes: '
                                         || string-join($problematicNodes/@n, '; ')
                                         || ", having citeID "
                                         || string-join($problematicNodes/@citeID, "; ")
                                         || ".")
        else ()
            return
                error(QName('http://salamanca.school/err', 'CreateNodeIndex'), 
                  'Could not produce a unique citeID for each sal:node (in ' || $wid || '). Problematic nodes: '
                  || string-join($problematicNodes/@n, '; '))
        else ()
    (: search these cases using: " //sal:node/@citeID[./string() = following::sal:node/@citeID/string()] :)
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 4 passed.") else ()

    let $testEmptyCiteIDs :=
        let $elementTypes := distinct-values(for $n in $specialNodes('citableNodes') return local-name($n))
        let $emptyCiteIDsTypes := distinct-values(for $n in $resultNodes[not(@citeID)] return local-name($n))
        return if ($elementTypes = $emptyCiteIDsTypes) then
            error(QName('http://salamanca.school/err', 'CreateNodeIndex'), 
                  'Could not produce a citeID for one or more sal:node (in ' || $wid || '). Problematic nodes: '
                  || string-join(($resultNodes[not(@citeID)][local-name(.) = $elementTypes]/@n), '; '))
        else ()
    (: search for " //*[not(./@citeID)] ":)
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 5 passed.") else ()

    (: not checking crumbtrails here ATM for not slowing down index creation too much... :)
    
    (: check whether all text is being captured through basic index nodes (that is, whether every single passage is citable) :)
    let $checkBasicNodes := 
        for $t in $work//tei:text[@type eq 'work_monograph' or
                                     (@type eq 'work_volume' and
                                      sutil:WRKisPublished($wid || '_' || @xml:id))
                                 ]//text()[not(ancestor::tei:figDesc)][normalize-space() ne ''] return
            if ($t[not(./ancestor::tei:* intersect $specialNodes('basicNodes'))]) then         (: if ($t[not(ancestor::tei:*[index:isBasicNode(.)]) and not(ancestor::tei:figDesc)]) then :) 
                let $debug := util:log('error', 'Encountered text node without ancestor::tei:*[index:isBasicNode(.)], in line ' || $t/preceding::tei:lb[1]/@xml:id/string() || ' – this might indicate a structural anomaly in the TEI data.')
                return error(QName('http://salamanca.school/err', 'CreateNodeIndex'), 'Encountered text node without ancestor::tei:*[index:isBasicNode(.)], in line ' || $t/preceding::tei:lb[1]/@xml:id/string()) 
            else ()
    (: if no xml:id is put out, try to search these cases like so:
        //text//text()[not(normalize-space() eq '')][not(ancestor::tei:*[@xml:id and (self::p or self::signed or self::head or self::titlePage or self::lg or self::item or self::label or self::argument or self::table)])]
    :)
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 6 passed.") else ()

    (: See if there are any leaf elements in our text that are not matched by our rule :)
    let $missed-elements := $work//(tei:front|tei:body|tei:back)//tei:*[count(ancestor-or-self::tei:*) < $fragmentationDepth][not(*)]         (: TODO: use index :)
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] " || count($missed-elements) || " missed elements collected.") else ()

    (: See if any of the elements we did get is lacking an xml:id attribute :)
    let $unidentified-elements := $targetNodes[not(@xml:id)]
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] " || count($unidentified-elements) || " unidentified elements collected.") else ()

    (: Keep track of how long this index did take :)

    let $removeTempfile :=  if (doc-available($config:temp-root || "/" || $wid || "_nodeIndex.xml")) then
                                xmldb:remove($config:temp-root, $wid || "_nodeIndex.xml")
                            else ()
    return
        (: return information that we want to inform about rather than throw hard errors :)
        map {
            'missed_elements': $missed-elements,
            'unidentified_elements': $unidentified-elements
        }
};


(: LABELS, CITEIDS, CRUMBTRAILS (-- deep recursion) :)

declare function index:constructCiteID($node as element(sal:node)) as xs:string {
    let $prefix :=
        if ($node/ancestor::sal:node[@xml:id eq $node/@citableParent]) then
            index:constructCiteID($node/ancestor::sal:node[@xml:id eq $node/@citableParent])
        else ()
    let $this :=
        if ($node/@cit) then $node/@cit/string() 
        (: if @cit doesn't already exist, we are dealing with a numeric/unnamed citeID node and create the citeID part here: :)
        else string(count($node/preceding-sibling::sal:node[@isNamedCit eq 'false']) + 1)
    return
        if ($prefix and $this) then $prefix || $index:citeIDConnector || $this else $this
};

declare function index:constructCrumbtrail($node as element(sal:node)) as item()+ {
    let $prefix := 
        if ($node/ancestor::sal:node[@xml:id eq $node/@citableParent]) then
            index:constructCrumbtrail($node/ancestor::sal:node[@xml:id eq $node/@citableParent])
        else ()
    let $this := $node/sal:crumb/*
    return
        if ($prefix and $this) then ($prefix, $index:crumbtrailConnector, $this) else $this
};

declare function index:constructLabel($node as element(sal:node)) as xs:string? {
    let $prefix := 
        if ($node/ancestor::sal:node[@xml:id eq $node/@citableParent]) then
            index:constructLabel($node/ancestor::sal:node[@xml:id eq $node/@citableParent])
        else ()
    (: not every sal:node has a distinctive passage: :)
    let $this := if ($node/@passage) then xs:string($node/@passage) else ''
    return
        if ($prefix and $this) then 
            $prefix || $index:labelConnector || $this 
        else $prefix || $this (: this will only return one of both, if any at all :)
};

declare function index:makeCrumb($wid as xs:string, $node as node(), $fragmentIds as map()?, $specialNodes as map(*)) as element(a)? {
    let $class := index:dispatch($node, 'class', $specialNodes)
    return
        if ($class) then
            <a class="{$class}" href="{index:makeUrl($wid, $node, $fragmentIds)}">{index:dispatch($node, 'title', $specialNodes)}</a>
        else 
            <a href="{index:makeUrl($wid, $node, $fragmentIds)}">{index:dispatch($node, 'title', $specialNodes)}</a>
};


(: Gets the citable crumbtrail/citeID (not label!) parent :)
declare function index:getCitableParent($node as node(), $specialNodes as map(*)) as node()? {

    (: Notes, milestones etc. are counted from div etc, they must not have p as their citableParent :)
    if ($node/self::tei:milestone[@unit ne 'other'] | $node[@place eq 'margin'] ) then
        ($node/ancestor::tei:*[not(self::tei:p)] intersect $specialNodes('citableNodes'))[last()]

    (: pages are counted from volume - special prefixes for front/backmatter prevent naming collisions :)
    else if ($node/self::tei:pb[not(@sameAs or @corresp)]) then
        if ($node/ancestor::tei:front|$node/ancestor::tei:back|$node/ancestor::tei:text[@xml:id eq 'work_volume']) then
            ($node/ancestor::tei:front|$node/ancestor::tei:back|$node/ancestor::tei:text[@xml:id eq 'work_volume'])[last()]
        else
            () (: TODO: this makes "ordinary" pb appear outside of any structural hierarchy - is this correct?
                  - yeeees...(?) :)

    else
        let $result := ($node/ancestor::tei:* intersect ( $specialNodes('citableNodes') ))[last()]
        return $result
};


(: Marginal citeIDs: "nX" where X is the anchor used (if it is alphanumeric)
                and "nXY" where Y is the number of times that "X" occurs inside the current div
    (important: nodes are citeID children of div (not of p) and are counted as such) :)
declare function index:makeMarginalCiteID($node as element(), $specialNodes as map(*)) as xs:string {

    (:  For performance, we will be working only on a copy the current section.
        However, if we want to intersect this with specialNodes, we have to take precautions,
        because these nodes are back in the actual document :)
    let $currentSection := sutil:copy(index:getCitableParent($node, $specialNodes))
    let $currentNode := $currentSection//*[@xml:id eq $node/@xml:id]

    let $marginalNodeIDs := for $i in $specialNodes('marginalNodes') return $i/@xml:id/string()

    let $label :=

        (: This note has an @n attribute that may make for a good citeID :)
        if (range:matches($currentNode/@n, '^[A-Za-z0-9\[\]]+$')) then

            (: Count how often the "X" of the current note appears as a note's @n attribute in this section ... :)
            let $numberOfX := count(  $currentSection//tei:note[upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))] )
            return
            if ($numberOfX gt 1) then
                concat(
                    upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', '')),
                    string(
                        count(  $currentSection//tei:note[upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))]
                                intersect
                                $currentNode/preceding::tei:note[upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))]
                             ) + 1
                          )
                )
            else
                upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))

        (: otherwise, just count how many notes precede the current note in this section :)
        else
            string(
                    count( $currentNode/preceding::tei:note intersect $currentSection//* ) + 1 
                  )
    return 'n' || $label
};

declare function index:makeUrl($targetWorkId as xs:string, $targetNode as node(), $fragmentIds as map()) as xs:string {
    let $targetNodeId := string($targetNode/@xml:id)
    let $viewerPage   :=      
        if (substring($targetWorkId, 1, 2) eq 'W0') then
            'work.html?wid='
        else if (substring($targetWorkId, 1, 2) eq 'L0') then
            'lemma.html?lid='
        else if (substring($targetWorkId, 1, 2) eq 'A0') then
            'author.html?aid='
        else if (substring($targetWorkId, 1, 2) eq 'WP') then
            'workingPaper.html?wpid='
        else
            'index.html?wid='
    let $targetNodeHTMLAnchor :=    
        if (contains($targetNodeId, '-pb-')) then
            concat('pageNo_', $targetNodeId)
        else $targetNodeId
    let $frag := $fragmentIds($targetNodeId)
    return concat($viewerPage, $targetWorkId, (if ($frag) then concat('&amp;frag=', $frag) else ()), '#', $targetNodeHTMLAnchor)
};

(:
~  Creates a teaser string of limited length (defined in $config:chars_summary) from a given node.
~  @param mode: must be one of 'orig', 'edit' (default)
:)
declare function index:makeTeaserString($node as element(), $mode as xs:string?, $specialNodes as map(*)) as xs:string {
    let $thisMode := if ($mode eq 'orig') then $mode else 'edit'
    let $string := string-join(txt:dispatch($node, $thisMode), '') (: replace(replace(string-join(txt:dispatch($node, $thisMode)), '\[.*?\]', ''), '\{.*?\}', '') :)
    return
        if (string-length($string) gt $config:chars_summary) then
            concat('&#34;', normalize-space(substring($string, 1, $config:chars_summary)), '…', '&#34;')
        else
            concat('&#34;', normalize-space($string), '&#34;')
};

declare function index:makeFragmentId($index as xs:integer, $xmlId as xs:string) as xs:string {
    format-number($index, '00000') || '_' || $xmlId
};



(: BOOLEAN FUNCTIONS for defining different classes of nodes :)

(: 
!!! IMPORTANT: before changing any of these functions, make sure to have read and understood
the section on node indexing in the docs/technical.md documentation file.

!!! However, according to https://exist-db.org/exist/apps/doc/tuning, using function calls in xpath
    is likely to force the engine into item-by-item mode, so there may be room for performance
    improvement here.
:)

(:
~ Determines which nodes serve for node label production.
:)
(: NOTE: the tei:text[@type eq 'completeWork'] node is NOT part of the index itself :)
declare function index:isLabelNode($node as element()) as xs:boolean {
    index:isIndexNode($node) and
    not($node/ancestor::tei:note) and
    (
        $node/self::tei:text[@type eq 'work_volume'] or
        $node/self::tei:div[$config:citationLabels(@type)?('isCiteRef')] or
        $node/self::tei:milestone[$config:citationLabels(@unit)?('isCiteRef')] or
        $node/self::tei:pb[not(@sameAs or @corresp)] or
        $node[$config:citationLabels(local-name(.))?('isCiteRef') and not(ancestor::tei:note)]
    )
};

(:
~ Determines the set of nodes that are generally citable (and indexed).
:)
declare function index:isIndexNode($node as node()) as xs:boolean {
    typeswitch($node)
        case element() return
            (: any element type relevant for nodetrail creation must be included in one of the following functions: :)
            index:isStructuralNode($node) or
            index:isMainNode($node) or
            index:isMarginalNode($node) or
            index:isAnchorNode($node) or
            index:isPageNode($node) or
            index:isListNode($node)
(:
                $node[@xml:id][
                    $node[self::tei:back                         |
                          self::tei:div[@type ne "work_part"]    | 
                          self::tei:front                        |
                          self::tei:label[@place eq 'margin']    |
                          self::tei:milestone[@unit ne 'other']  |
                          self::tei:note[@place eq 'margin']     |
                          self::tei:pb[not(@sameAs or @corresp)] |
                          self::tei:text[@type eq 'work_volume']
                         ] |
                    (
                        $node[self::tei:argument                  |
                              self::tei:head                      |
                              self::tei:item                      |
                              self::tei:label[@place ne 'margin'] |
                              self::tei:lg                        |
                              self::tei:list                      |
                              self::tei:p                         |
                              self::tei:signed                    |
                              self::tei:table                     |
                              self::tei:titlePage
                            ][not(
                                  ancestor::tei:argument or
                                  ancestor::tei:head or
                                  ancestor::tei:label[@place ne 'margin'] or
                                  ancestor::tei:lg or
                                  ancestor::tei:p or
                                  ancestor::tei:signed or
                                  ancestor::tei:table or
                                  ancestor::tei:titlePage
                                 )
                            ]
                    )
                ]
:)
        default return 
            false()
};

(:
~ Determines whether a node is a specific citeID element, i.e. one that is specially prefixed in citeIDs.
:)
declare function index:isNamedCiteIDNode($node as element()) as xs:boolean {
    index:isAnchorNode($node) or
    index:isPageNode($node) or
    index:isMarginalNode($node) or
    (index:isStructuralNode($node) 
        and $node[self::tei:text[@type eq 'work_volume'] | 
                  self::tei:back                          | 
                  self::tei:front]) or (: TODO: include div here? :)
    (index:isMainNode($node) 
        and $node[self::tei:head | self::tei:titlePage]) or
    (index:isListNode($node) 
        and $node[self::tei:list[@type = ('dict', 'index')] or
                  self::tei:item[ancestor::tei:list[@type eq 'dict']]])
};

(:
~ Determines whether a node is a 'generic' citeID element, i.e. one that isn't specially prefixed in citeIDs.
~ --> complement of index:isNamedCiteIDNode()
:)
declare function index:isUnnamedCiteIDNode($node as element()) as xs:boolean {
    index:isIndexNode($node) and not(index:isNamedCiteIDNode($node))
};

(:
~ Basically, we can determine several types of elements in a TEI/text tree:
:)

(:
~ Anchor and page nodes occur within main nodes, marginal nodes, or structural nodes, and have no content.
~ (NOTE: should work with on-the-fly copying of sections. )
:)
declare function index:isAnchorNode($node as node()) as xs:boolean {
    boolean(
        $node[self::tei:milestone][@unit ne 'other'][@xml:id]
    )
(:
    boolean(
        $node/@xml:id and
        $node/self::tei:milestone[@unit ne 'other']
    )
:)
};

(:
~ Page nodes are regular page breaks.
~ (NOTE: should work with on-the-fly copying of sections. )
:)
declare function index:isPageNode($node as node()) as xs:boolean {
    boolean(
        $node[self::tei:pb][not(@sameAs or @corresp)][@xml:id]
    )
(:
        $node/@xml:id and
        $node/self::tei:pb[not(@sameAs or @corresp)]
    )
:)
};

(:
~ Marginal nodes occur within structural or main nodes.
~ (NOTE: should work with on-the-fly copying of sections. )
:)
(: TODO: if this is changed, we also need to change txt:isMarginalNode() :)
declare function index:isMarginalNode($node as node()) as xs:boolean {
    boolean(
        $node[@place eq 'margin'][self::tei:note | self::tei:label][@xml:id]
    )
(:
        $node/@xml:id and
        (
            $node/self::tei:note[@place eq 'margin'] or
            $node/self::tei:label[@place eq 'margin']
        )
:)
        (:and not($node/ancestor::tei:*[index:isMarginalNode(.)]):) (: that shouldn't be possible :)
};

(:
~ Main nodes are mixed-content elements such as tei:p, which may contain marginal or anchor nodes.
~ Note: all main nodes should be citable in the reading view.
:)
declare function index:isMainNode($node as node()) as xs:boolean {
    not($node/ancestor::tei:*[index:isMainNode(.) or index:isMarginalNode(.) or self::tei:list]) and
    (
        $node/self::tei:p[@xml:id]                          |
        $node/self::tei:signed[@xml:id]                     |
        $node/self::tei:head[@xml:id]                       |
        $node/self::tei:titlePage[@xml:id]                  |
        $node/self::tei:lg[@xml:id]                         |
        $node/self::tei:label[@place ne 'margin'][@xml:id]  |
        $node/self::tei:argument[@xml:id]                   |
        $node/self::tei:table[@xml:id]
    )
};

(:
~ List nodes are certain nodes within lists (list, item, head) that occur outside of main nodes and marginal nodes.
:)
declare function index:isListNode($node as node()) as xs:boolean {
    not($node/ancestor::tei:*[index:isMainNode(.) or index:isMarginalNode(.)]) and
    (
        $node/self::tei:list[@xml:id]                           |
        $node/self::tei:item[@xml:id]                           |
        $node/self::tei:head[ancestor::tei:list][@xml:id]       |
        $node/self::tei:argument[ancestor::tei:list][@xml:id]
    ) 
};


(:
~ Structural nodes are high-level nodes containing any of the other types of nodes (main, marginal, anchor nodes).
:)
declare function index:isStructuralNode($node as node()) as xs:boolean {
(:
    boolean(
        $node[self::tei:div[@type ne "work_part"] | self::tei:back | self::tei:front | self::tei:text[@type eq 'work_volume'] ]
            [@xml:id]
    )
:)
    boolean(
       $node/self::tei:div[@type ne "work_part"][@xml:id]      | (: TODO: comment out for div label experiment :)
       $node/self::tei:back[@xml:id]                           |
       $node/self::tei:front[@xml:id]                          |
       $node/self::tei:text[@type eq 'work_volume'][@xml:id]
    )
};


(:
~ Basic nodes represent *all* container nodes at the bottom of the index tree, i.e. mixed-content elements 
    that comprise all text nodes in a sequential, non-overlapping manner. 
    To be used for Sphinx snippets, for checking consistency etc.
:)
declare function index:isBasicNode($node as node()) as xs:boolean {
    index:isMainNode($node) or
    index:isMarginalNode($node) or
    (:(index:isListNode($node) and not($node/descendant::tei:*[index:isListNode(.)])):)
    (index:isListNode($node) and (($node/self::tei:list and not($node/descendant::tei:list))
                                   or ($node[(self::tei:item | self::tei:head | self::tei:argument) 
                                             and not(descendant::tei:list) 
                                             and following-sibling::tei:item[./tei:list[index:isListNode(.)]]])
                                  )
    (: read as: 'lists that do not contain lists (=lists at the lowest level), or siblings thereof' :)
    (: (this is quite a complicated XPath, but I don't know how to simplify it without breaking things...) :)
    )
};


declare function index:getNodeCategory($node as element()) as xs:string {
         if (index:isMainNode($node))       then 'main'
    else if (index:isMarginalNode($node))   then 'marginal'
    else if (index:isStructuralNode($node)) then 'structural'
    else if (index:isListNode($node))       then 'list'
    else if (index:isPageNode($node))       then 'page'
    else if (index:isAnchorNode($node))     then 'anchor'
    else                                    error()
};




(: NODE TYPESWITCH FUNCTIONS :)

(:  MODES: 
~   - 'title': title of a node/section (only for nodes that represent sections)
~   - 'label': label of a node (only for nodes that represent labelled sections)
~   - 'citeID': citeID of a node (only for nodes that are index:isNamedCiteIDNode() - all other are created at index time)
~   - 'class': i18n class of a node, usually to be used by HTML-/RDF-related functionalities for generating verbose labels when displaying section titles 
:)

(:
~ @param $node : the node to be dispatched
~ @param $mode : the mode for which the function shall generate results
:)
declare function index:dispatch($node as node(), $mode as xs:string, $specialNodes as map(*)) {
    typeswitch($node)
    (: Try to sort the following nodes based (approx.) on frequency of occurences, so fewer checks are needed. :)
        case element(tei:pb)            return index:pb($node, $mode)
        case element(tei:head)          return index:head($node, $mode, $specialNodes)
        case element(tei:p)             return index:p($node, $mode, $specialNodes)
        case element(tei:signed)        return index:signed($node, $mode, $specialNodes)
(:        case element(tei:byline)        return index:byline($node, $mode, $specialNodes):)
        case element(tei:note)          return index:note($node, $mode, $specialNodes)
        case element(tei:div)           return index:div($node, $mode, $specialNodes)
        case element(tei:milestone)     return index:milestone($node, $mode, $specialNodes)
        
        case element(tei:list)          return index:list($node, $mode, $specialNodes)
        case element(tei:item)          return index:item($node, $mode, $specialNodes)

        case element(tei:lg)            return index:lg($node, $mode, $specialNodes)
        
        case element(tei:table)         return index:table($node, $mode, $specialNodes)
        
        case element(tei:label)         return index:label($node, $mode, $specialNodes)
        case element(tei:argument)      return index:argument($node, $mode, $specialNodes)
        
        case element(tei:titlePage)     return index:titlePage($node, $mode)
        case element(tei:titlePart)     return index:titlePart($node, $mode, $specialNodes)
(:        case element(tei:docImprint)    return index:docImprint($node, $mode, $specialNodes)        :)

        case element(tei:front)         return index:front($node, $mode) 
        case element(tei:body)          return index:body($node, $mode)
        case element(tei:back)          return index:back($node, $mode)
        case element(tei:text)          return index:text($node, $mode, $specialNodes)
        
        case element(tei:figDesc)       return ()
        case element(tei:teiHeader)     return ()
        case element(tei:fw)            return ()
        case element(tei:byline)        return ()
        case element(tei:imprimatur)    return ()
        case element(tei:docTitle)      return ()
        case element(tei:docDate)       return ()
        case element(tei:docEdition)    return ()
        case element(tei:docImprint)    return ()
        case element()                  return error(QName('http://salamanca.school/err', 'indexDispatch'), 'Unknown element: ' || local-name($node) || ' (in mode: "' || $mode || '")')
        case comment()                  return ()
        case processing-instruction()   return ()

        default return ()
};


(: ####++++ Element functions (ordered alphabetically) ++++#### :)


declare function index:argument($node as element(tei:argument), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'class' return 
            'tei-' || local-name($node)

        case 'citeID' return
                (: TODO: maybe we use one abbreviation for different div types, then we have to include them when counting... :)
                let $abbr := if ($node/@type) then $config:citationLabels($node/@type)?('abbr') else ()
                let $prefix := if ($abbr) then lower-case(if (contains($abbr, '.')) then substring-before($abbr, '.') else $abbr) else 'arg'
                let $citeParent := ($node/ancestor::tei:* intersect $specialNodes('citableNodes'))[last()]
                let $position :=
                    if ( count( $citeParent//tei:argument ) gt 1 ) then
                        string(count($citeParent//tei:argument intersect $node/preceding::*) + 1 )
                    else ()
                return $prefix || $position
        default return
            ()
};

declare function index:back($node as element(tei:back), $mode as xs:string) {
    switch($mode)
        case 'title' return
            ()
        case 'class' return
            'tei-' || local-name($node)
        case 'citeID' return
            'backmatter'
        case 'label' return
            $config:citationLabels(local-name($node))?('abbr')
        default return
            ()
};

declare function index:body($node as element(tei:body), $mode as xs:string) {
    switch($mode)
        case 'class' return
            'tei-' || local-name($node)
        default return
            ()
};

(: declare function index:byline($node as element(tei:byline), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            index:makeTeaserString($node, 'edit', $specialNodes)
        
        case 'class' return
            'tei-' || local-name($node)
            
        default return
            ()
};
:)

declare function index:div($node as element(tei:div), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            (: purely numeric section titles: :)
            if ($node/@n and range:matches($node/@n, '^[0-9\[\]]+$')) then
                normalize-space($node/@n)
            (: non-numeric n attribute :)
            else if ($node/@n) then
                '"' || normalize-space($node/@n) || '"'
            else if ($node/(tei:head|tei:label)) then
                index:makeTeaserString(($node/(tei:head|tei:label))[1], 'edit', $specialNodes)
            (: otherwise, try to derive a title from potential references to the current node :)
            else if (//tei:ref[@target eq concat('#', $node/@xml:id)]) then
                index:makeTeaserString(//tei:ref[@target eq concat('#', $node/@xml:id)][1], 'edit', $specialNodes)
            (: if there is a list/head and nothing else works, we may use that :)
            else if ($node/tei:list/(tei:head|tei:label)) then
                index:makeTeaserString(($node/tei:list/(tei:head|tei:label))[1], 'edit', $specialNodes)
            else ()
            
        case 'class' return
            'tei-div-' || $node/@type
        
        case 'citeID' return
                (: TODO: maybe we use one abbreviation for different div types, then we have to include them when counting... :)
                let $abbr       :=  $config:citationLabels($node/@type)?('abbr')
                let $prefix     :=  if ($abbr) then lower-case(if (contains($abbr, '.')) then substring-before($abbr, '.') else $abbr)
                                    else 'div'
                let $position   :=  if (        count( $node/parent::tei:*/tei:div[@type eq $node/@type] ) gt 1 ) then
                                        string( count( $node/parent::tei:*/tei:div[@type eq $node/@type] intersect $node/preceding::* ) + 1 )
                                    else ()
                let $result     :=  $prefix || $position
                return $result

        case 'label' return
            if (boolean($node intersect $specialNodes('labelNodes'))) then     (: index:isLabelNode($node)) then :)
                let $prefix := lower-case($config:citationLabels($node/@type)?('abbr')) (: TODO: upper-casing with first element of label ? :)
                return 
                    if ($node/@type = ('lecture', 'gloss')) then (: TODO: 'lemma'? :)
                        (: special cases: with these types, we provide a short teaser string instead of a numeric value :)
                        let $teaser := '"' || normalize-space(substring(substring-after(index:div($node, 'title', $specialNodes), '"'),1,15)) || '…"'
                        return $prefix || ' ' || $teaser
                    else
                        let $position := 
                            if ($node/@n[range:matches(., '^[0-9\[\]]+$')]) then $node/@n (:replace($node/@n, '[\[\]]', '') ? :)
                            else if (boolean($node/ancestor::tei:* intersect $specialNodes('labelNodes')))  then     (: [index:isLabelNode(.)]) then :)
                                (: using the none-copy version here for sparing memory: :)
                                if (count(($node/ancestor::tei:* intersect $specialNodes('labelNodes'))//tei:div[@type eq $node/@type][. intersect $specialNodes('labelNodes')]
                                         ) gt 1 ) then
                                    string(
                                        count(
                                             ($node/ancestor::tei:* intersect $specialNodes('labelNodes'))[1]//tei:div[@type eq $node/@type][. intersect $specialNodes('labelNodes')]
                                             intersect
                                             $node/preceding::tei:div[@type eq $node/@type][. intersect $specialNodes('labelNodes')]
                                             ) + 1)
                                else ()
                            else if (count($node/parent::tei:*/tei:div[@type eq $node/@type]) gt 1) then 
                                string(count($node/preceding-sibling::tei:div[@type eq $node/@type]) + 1)
                            else ()
                        return
                            $prefix || (if ($position) then ' ' || $position else ())
            else ()
        
        default return
            ()
};

declare function index:front($node as element(tei:front), $mode as xs:string) {
    switch ($mode)
        case 'title' return
            ()
            
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            'frontmatter'
            
        case 'label' return
            $config:citationLabels(local-name($node))?('abbr')
            
        default return
            ()
};

(: FIXME: In the following, the #anchor does not take account of html partitioning of works. Change this to use semantic section id's. :)
declare function index:head($node as element(tei:head), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            index:makeTeaserString($node, 'edit', $specialNodes)
        
        case 'class' return
            'tei-' || local-name($node)
        
        default return 
            ()
};

declare function index:item($node as element(tei:item), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
                if ($node/parent::tei:list/@type='dict' and $node//tei:term[1][@key]) then
                    (: TODO: collision with div/@type='lemma'? :)
                    let $positionStr := 
                        if (count($node/parent::tei:list/tei:item[.//tei:term[1]/@key eq $node//tei:term[1]/@key]) gt 1) then
                             ' - ' || 
                             normalize-space(string(count($node/preceding::tei:item[tei:term[1]/@key eq $node//tei:term[1]/@key] 
                                          intersect $node/ancestor::tei:div[1]//tei:item[tei:term[1]/@key eq $node//tei:term[1]/@key]) + 1))
                        else ()
                    return
                        '"' || normalize-space($node//tei:term[1]/@key || $positionStr) || '"'
                else if ($node/@n and not(range:matches($node/@n, '^[0-9\[\]]+$'))) then
                    '"' || normalize-space(string($node/@n)) || '"'
                else if ($node/(tei:head|tei:label)) then
                    index:makeTeaserString(($node/(tei:head|tei:label))[1], 'edit', $specialNodes)
                (: purely numeric section titles: :)
                else if ($node/@n and (range:matches($node/@n, '^[0-9\[\]]+$'))) then
                    normalize-space($node/@n)
                (: otherwise, try to derive a title from potential references to the current node :)
(:                else if ($node/ancestor::tei:TEI//tei:ref[@target eq concat('#', $node/@xml:id/string())]) then:)
                else if (//tei:ref[@target eq concat('#', $node/@xml:id/string())]) then
(:                    index:makeTeaserString($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)][1], 'edit', $specialNodes):)
                    index:makeTeaserString(//tei:ref[@target eq concat('#', $node/@xml:id/string())][1], 'edit', $specialNodes)
                else ()
        
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            (: "XY" where X is the section title (index:item($node, 'title')) in lower-case, use only for items in indexes and dictionary :)
            let $title := lower-case(replace(index:item($node, 'title', $specialNodes), '[^a-zA-Z0-9]', ''))
            let $position :=
                if ($title) then
                    let $siblings := $node/parent::tei:list/tei:item[lower-case(replace(index:item(., 'title', $specialNodes), '[^a-zA-Z0-9]', '')) eq $title]
                    return
                        if (count($siblings) gt 1) then 
                            string(count($siblings intersect $node/preceding::*) + 1)
                        else ()
                else if (count($node/parent::tei:list/tei:item) gt 1) then 
                    string(count($node/parent::tei:list/tei:item intersect $node/preceding::*) + 1)
                else ()
            return $title || $position

        default return
            ()
};

declare function index:label($node as element(tei:label), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            index:makeTeaserString($node, 'edit', $specialNodes)
          
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            index:makeMarginalCiteID($node, $specialNodes)

        default return
            ()
};

declare function index:list($node as element(tei:list), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            (: purely numeric n attribute :)
            if ($node/@n and range:matches($node/@n, '^[0-9\[\]]+$')) then
                normalize-space($node/@n)
            (: non-numeric n attribute :)
            else if ($node/@n) then
                '"' || normalize-space($node/@n) || '"'
            (: from head or label :)
            else if ($node/(tei:head|tei:label)) then
                index:makeTeaserString(($node/(tei:head|tei:label))[1], 'edit', $specialNodes)
            (: otherwise, try to derive a title from potential references to the current node :)
            else if (//tei:ref[@target eq concat('#', $node/@xml:id/string())]) then
                index:makeTeaserString(//tei:ref[@target eq concat('#', $node/@xml:id/string())][1], 'edit', $specialNodes)
            else ()

        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            (: dictionaries, indices and summaries get their type prepended to their number :)
            let $currentSection := sutil:copy(($node/ancestor::tei:div|$node/ancestor::tei:body|$node/ancestor::tei:front|$node/ancestor::tei:back)[last()])
            let $currentNode := $currentSection//tei:list[@xml:id eq $node/@xml:id]
            return
              concat(
                  $node/@type/string(), 
                  if (count($currentNode/parent::*/tei:list[@type eq $currentNode/@type]) gt 1) then string(
                                 count($currentNode/preceding::tei:list[@type eq $currentNode/@type]
                                       intersect $currentSection//tei:list[@type eq $currentNode/@type]
                                 ) + 1)
                  else ()
              )

        default return
            ()
};

declare function index:lg($node as element(tei:lg), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            index:makeTeaserString($node, 'edit', $specialNodes)
           
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            if ($node intersect $specialNodes('citableNodes')) then            
                string(count(preceding-sibling::* intersect $specialNodes('citableNodes')) + 1)
            else ()
            
        default return
            ()
};

declare function index:milestone($node as element(tei:milestone), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            if ($node/@n and not(range:matches($node/@n, '^[0-9\[\]]+$'))) then
                '"' || normalize-space($node/@n) || '"'
            (: purely numeric section titles: :)
            (:else if (matches($node/@n, '^[0-9\[\]]+$') and $node/@unit eq 'number') then
                normalize-space($node/@n)   :)
            (: use @unit to derive a title: :)
            (:else if (matches($node/@n, '^\[?[0-9]+\]?$') and $node/@unit[. ne 'number']) then
                $config:citationLabels($node/@unit)?('abbr') || ' ' || $node/@n:)
            (: if milestone has numerical information, just state the number, regardless of @unit and other attributes: :)
            else if (range:matches($node/@n, '^[0-9\[\]]+$')) then
                normalize-space($node/@n)
            (: otherwise, try to derive a title from potential references to the current node :)
            else if ($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)]) then
                index:makeTeaserString($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)][1], 'edit', $specialNodes)
            else ()
            
        case 'class' return
            'tei-ms-' || $node/@unit
            
        case 'citeID' return
            (: "XY" or "XYNZ" where X is the unit and Y is the anchor or the number of milestones where this occurs
                and Z is the count of preceding milestones with the same name :)
            let $currentSection := sutil:copy(index:getCitableParent($node, $specialNodes))
            let $currentNode := $currentSection//tei:milestone[@xml:id eq $node/@xml:id]
            return
                if ($node/@n[range:matches(., '[a-zA-Z0-9]')]) then
                    let $similarMs :=
                        $currentSection//tei:milestone[@unit eq $currentNode/@unit 
                                                       and upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))]
                    let $position :=
                        if (count($similarMs) gt 1) then
                            (: put 'N' between @n and position, so as to avoid collisions :)
                            'N' || string(count($currentNode/preceding::tei:milestone intersect $similarMs) + 1)
                        else ()
                    return $currentNode/@unit/string() || upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', '')) || $position
                else
                    $currentNode/@unit/string() || string(count($currentNode/preceding::tei:milestone[@unit eq $node/@unit] intersect $currentSection//tei:milestone[@unit eq $currentNode/@unit]) + 1)
        
        case 'label' return
            if (boolean ($node intersect $specialNodes('labelNodes'))) then    (: index:isLabelNode($node)) then :)
                (: TODO: ATM milestone/@unit = ('article', 'section') resolves to the same abbrs as div/@type = ('article', 'section') :)
                (: TODO: if @n is numeric, always resolve to 'num.' ? :)
                let $prefix := lower-case($config:citationLabels($node/@unit)?('abbr'))
                let $num := 
                    if ($node/@n[range:matches(., '^[0-9\[\]]+$')]) then $node/@n (:replace($node/@n, '[\[\]]', '') ? :)
                    else 
                        let $currentSection := sutil:copy( ( $node/ancestor::tei:*[not(self::tei:p)] intersect $specialNodes('labelNodes') )[1] )
(:                        let $currentSection := sutil:copy($node/ancestor::tei:*[index:isLabelNode(.) and not(self::tei:p)][1]):)
                        let $currentNode := $currentSection//tei:milestone[@xml:id eq $node/@xml:id]

                        let $position := count($currentSection//tei:milestone[@unit eq $currentNode/@unit][. intersect $specialNodes('labelNodes')]
                                               intersect $currentNode/preceding::tei:milestone[@unit eq $currentNode/@unit][. intersect $specialNodes('labelNodes')]
                                               ) + 1

(:                        let $position := count($currentSection//tei:milestone[@unit eq $currentNode/@unit and index:isLabelNode(.)]
                                               intersect $currentNode/preceding::tei:milestone[@unit eq $currentNode/@unit and index:isLabelNode(.)]) + 1
:) 
                       return string($position)
                return
                    $prefix || ' ' || $num
            else ()
        
        default return () (: also for snippets-orig, snippets-edit :)
};

declare function index:note($node as element(tei:note), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            let $currentSection := sutil:copy(index:getCitableParent($node, $specialNodes))
            let $currentNode := $currentSection//tei:note[@xml:id eq $node/@xml:id]
            return
                if ($node/@n) then
                    let $noteNumber :=
                        if (count($currentSection//tei:note[upper-case(normalize-space(@n)) eq upper-case(normalize-space($currentNode/@n))]) gt 1) then
                            ' (' || 
                            string(count(   $currentNode/preceding::tei:note[upper-case(normalize-space(@n)) eq upper-case(normalize-space($currentNode/@n))] 
                                         intersect $currentSection//tei:note[upper-case(normalize-space(@n)) eq upper-case(normalize-space($currentNode/@n))])
                                   + 1) 
                            || ')'
                        else ()
                    return '"' || normalize-space($currentNode/@n) || '"' || $noteNumber
                else string(count($currentNode/preceding::tei:note intersect $currentSection//tei:note) + 1)

        case 'class' return
            'tei-' || local-name($node)
        
        case 'citeID' return
            index:makeMarginalCiteID($node, $specialNodes)
        
        case 'label' return
            if (boolean($node intersect $specialNodes('labelNodes')) ) then    (: index:isLabelNode($node)) then :)
                (: labelled parents of note are div, not p :)
                let $currentSection := sutil:copy( ($node/ancestor::tei:*[not(self::tei:p)] intersect $specialNodes('labelNodes'))[last()])
                let $currentNode := $currentSection//tei:note[@xml:id eq $node/@xml:id]
                let $prefix := $config:citationLabels(local-name($node))?('abbr')
                let $label := 
                    if ($node/@n) then '"' || normalize-space($node/@n) || '"' (: TODO: what if there are several notes with the same @n in a div :)
                    else string(count($currentSection//tei:note
                                      intersect $currentNode/preceding::tei:note) + 1)
                return $prefix || ' ' || $label
            else ()
        
        default return
            ()
};

declare function index:p($node as element(tei:p), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            index:makeTeaserString($node, 'edit', $specialNodes)
        
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citeID' return
            if ($node intersect $specialNodes('citableNodes')) then
                let $result := string(count($node/preceding-sibling::* intersect $specialNodes('citableNodes')) + 1)
                return $result
            else
                let $debug := console:log("[INDEX] Warning: index:p finds no intersection for " || $node/@xml:id/string() || ".")
                return ()
        
        case 'label' return
            if (boolean($node intersect $specialNodes('labelNodes')) ) then    (: index:isLabelNode($node)) then :)
                let $prefix := $config:citationLabels(local-name($node))?('abbr')
                let $teaser := '"' || normalize-space(substring(substring-after(index:p($node, 'title', $specialNodes), '"'),1,15)) || '…"'(: short teaser :)
                return $prefix || ' ' || $teaser
            else ()
        
        default return
            ()
};

declare function index:pb($node as element(tei:pb), $mode as xs:string) {
    switch($mode)
        case 'title' return
            (: any pb with @sameAs and @corresp probably won't even get reached, since they typically have note ancestors :)
            if ($node/@sameAs) then
                concat('[pb_sameAs_', normalize-space($node/@sameAs), ']')
            else if ($node/@corresp) then
                concat('[pb_corresp_', normalize-space($node/@corresp), ']')
            else
                (: not prepending 'Vol. ' prefix here :)
                if (contains($node/@n, 'fol.')) then 
                    normalize-space($node/@n)
                else
                    'p. ' || normalize-space($node/@n)
        
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citeID' return
            (: "pX" where X is page number :)
            concat('p',
                if (range:matches($node/@n, '[A-Za-z0-9\[\]]')
                    and not($node/preceding::tei:pb[ancestor::tei:text[1] intersect $node/ancestor::tei:text[1]
                                                    and upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($node/@n, '[^a-zA-Z0-9]', ''))]
                            )
                   ) then
                    upper-case(replace($node/@n, '[^a-zA-Z0-9]', ''))
                else substring($node/@facs, 6)
            )
            (: TODO: are collisions possible, esp. if pb's crumb does not inherit from the specific section (titlePage|div)? 
               -> for example, with repetitive page numbers in the appendix 
                (ideally, such collisions should be resolved in TEI markup, but one never knows...) :)
        
        case 'label' return
            if (contains($node/@n, 'fol.')) then normalize-space($node/@n)
            else 'p. ' || normalize-space($node/@n)
        
        (: pb nodes are good candidates for tracing the speed/performance of document processing, 
            since they are equally distributed throughout a document :)
        case 'debug' return
            let $log := util:log('warn', '[INDEX] Processing tei:pb node ' || $node/@xml:id)
            let $dbg := console:log('[INDEX] Processing tei:pb ' || $node/@xml:id/string() || ' ...')
            return ()
        default return ()
};

declare function index:signed($node as element(tei:signed), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            index:makeTeaserString($node, 'edit', $specialNodes)
        
        case 'class' return
            'tei-' || local-name($node)
            
        default return
            ()
};

declare function index:table($node as element(tei:table), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            if ($node/tei:head) then
                index:makeTeaserString($node/tei:head, 'edit', $specialNodes)
            else ()
            
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            if ($node intersect $specialNodes('citableNodes')) then
                let $citeParent := ($node/ancestor::tei:* intersect $specialNodes('citableNodes'))[last()]
                return
                    "tbl" || string(count($node/preceding::tei:table intersect $citeParent//tei:table) + 1)
            else ()
            
        default return
            ()
};

declare function index:text($node as element(tei:text), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            normalize-space(
                if ($node/@type eq 'work_volume') then
                    $node/@n/string()
                (: tei:text with solely technical information: :)
                else if ($node/@xml:id eq 'completeWork') then
                    '[complete work]'
                else if (starts-with($node/@xml:id, 'work_part_')) then
                    '[process-technical part: ' || substring(string($node/@xml:id), 11, 1) || ']'
                else ()
            )
        
        case 'class' return
            if ($node/@type eq 'work_volume') then 'tei-text-' || $node/@type
            else if ($node/@xml:id eq 'completeWork') then 'tei-text-' || $node/@xml:id
            else if (starts-with($node/@xml:id, 'work_part_')) then 'elem-text-' || $node/@xml:id
            else 'tei-text'
        
        case 'citeID' return
            (: "volX" where X is the current volume number, don't use it at all for monographs :)
            if ($node/@type eq 'work_volume') then
               concat('vol', count($node/preceding::tei:text[@type eq 'work_volume']) + 1)
            else ()
        
        case 'label' return
            if (boolean($node intersect $specialNodes('labelNodes')) ) then    (: index:isLabelNode($node)) then :)
                'vol. ' || $node/@n
            else ()
        
        default return
            ()
};

declare function index:titlePage($node as element(tei:titlePage), $mode as xs:string) {
    switch($mode)
        case 'title' return
            (:normalize-space(
                let $volumeString := 
                    if ($node/ancestor::tei:text[@type='work_volume']) then 
                        concat('Vol. ', $node/ancestor::tei:text[@type='work_volume']/@n, ', ') 
                    else ()
                let $volumeCount :=
                    if (count($node/ancestor::tei:text[@type='work_volume']//tei:titlePage) gt 1) then 
                        string(count($node/preceding-sibling::tei:titlePage)+1) || ', '
                    else ()
                return $volumeCount || $volumeString
            ):)
            ()
        
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citeID' return
            if (count($node/ancestor::tei:front//tei:titlePage) gt 1) then
                'titlepage' || string(count($node/preceding-sibling::tei:titlePage) + 1)
            else 'titlepage'
        
        case 'label' return
            $config:citationLabels(local-name($node))?('abbr')
        
        default return
            ()
};

declare function index:titlePart($node as element(tei:titlePart), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            index:makeTeaserString($node, 'edit', $specialNodes)
            
        case 'class' return
            'tei-' || local-name($node)

        case 'citeID' return
            error()
        
        default return 
            ()
};

(: declare function index:docImprint($node as element(tei:docImprint), $mode as xs:string, $specialNodes as map(*)) {
    switch($mode)
        case 'title' return
            index:makeTeaserString($node, 'edit', $specialNodes)
            
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            (/: "imprint.X" where X is the number of parts where this occurs :/)
            concat('imprint.', string(count($node/preceding-sibling::tei:docImprint) + 1))
        
        default return 
            ()
};
:)
