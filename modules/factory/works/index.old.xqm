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

(:
~ Controller function for creating (and reporting about) node indexes.
~ Here's what it does:
~ - It determines at which depth to segment a work
~ - It resolves all XIncludes
~ - It calls index:getFragmentNodes() in order to build the set of nodes constituting html fragments (the "target-set")
~ - It collects all nodes that we should be keeping track of (defined in index:isIndexNode())
~ - For all these nodes, it registers in which fragment they end up
~   (the nearest ((ancestor-or-self or descendant that has no preceding siblings) that's also contained in the target-set))
:)
declare function index:makeNodeIndex($tei as element(tei:TEI)) as map(*) {
    let $wid := xs:string($tei/@xml:id)
    let $fragmentationDepth := index:determineFragmentationDepth($tei)
    let $debug := if ($config:debug = ("trace", "info")) then console:log("[INDEX] Indexing " || $wid || " at fragmentation level " || $fragmentationDepth || ".") else ()

    let $xincludes := $tei//tei:text//xi:include/@href/string()
    let $work      := if (count($xincludes) gt 0) then
                        util:expand($tei)
                      else
                        $tei

    let $targetSet := index:getFragmentNodes($work, $fragmentationDepth)
    let $targetSetNodeTypes := distinct-values($targetSet ! (local-name(.)))
    let $debug := if ($config:debug = ("trace", "info")) then console:log("[INDEX] Target set contains " || count($targetSet) || " nodes (to become html fragments) of these types: " || string-join($targetSetNodeTypes, ", ") || ". Acquiring all nodes that have to be indexed ...") else ()

    (: First, get all relevant nodes :)
    let $nodes := 
        for $text in $work//tei:text return 
            (: make sure that we only grasp nodes that are within a published volume :)
            if (($text/@type eq 'work_volume' and sutil:WRKisPublished($wid || '_' || $text/@xml:id))
                or $text/@type eq 'work_monograph') then 
                $text/descendant-or-self::*[index:isIndexNode(.)]
            else ()

    let $pages := $nodes ! (.[self::tei:pb])                
    (: Create the fragment id for each node beforehand, so that recursive crumbtrail creation has it readily available :)
    let $debug := if ($config:debug = ("trace", "info")) then console:log("[INDEX] Node indexing: " || count($nodes) || " nodes (" || count($pages) || " pages) to process. Identifying fragment Ids ...") else ()
    let $fragmentIds :=
        map:merge(
            for $node at $pos in $nodes
                let $debug :=   if (($config:debug = "trace") and ($pos mod 1000 eq 0)) then
                                    console:log("[INDEX] Node indexing: processing node no. " || string($pos)  || " ...")
                                else ()
                let $n := xs:string($node/@xml:id)
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
(:                let $frag := (($node/ancestor-or-self::tei:* | $node//*[not(preceding-sibling::*)]) intersect $targetSet)[1]:)
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
                let $fragId := index:makeFragmentId(functx:index-of-node($targetSet, $frag), xs:string($frag/@xml:id))
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
    let $indexTree := 
        <sal:index>
            {index:extractNodeStructure($wid, $work//tei:text[not(ancestor::tei:text)], $xincludes, $fragmentIds)}
        </sal:index>

    (: 2.) flatten the index from 1.) and enrich sal:nodes with full-blown citeIDs, etc. :)
    let $debug := if ($config:debug = ("trace")) then console:log("[INDEX] Node indexing: stage 1 finished, cont'ing with stage 2 ...") else ()
    let $index := 
        <sal:index work="{$wid}" xml:space="preserve">
            {index:createIndexNodes($indexTree)}
        </sal:index>
    let $debug := if ($config:debug = ("trace")) then console:log("[INDEX] Node indexing: stage 2 finished, cont'ing with quality check ...") else ()
        
    let $check := index:qualityCheck($index, $work, $targetSet, $fragmentationDepth)
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
~   it goes into the target set; otherwise is nearest ancestor that is a "structural node" does.
~ - Finally, we return the distinct nodes, i.e. no duplicates
~   (two non-structural nodes of the desired level may have the same ancestor)
~ - (Obsolete: In front and back, fragmentation must not go below the child level, since we don't expect child fragments be too large here.)
:)
declare function index:getFragmentNodes($work as element(tei:TEI), $fragmentationDepth as xs:integer) as node()* {
    for $text in $work//tei:text[@type eq 'work_monograph'
                                  or (@type eq 'work_volume' and sutil:WRKisPublished($work/@xml:id || '_' || @xml:id))] return
            let $debug  :=  if ($config:debug = "trace") then
                                console:log("[INDEX] Build set of nodes at level " || $fragmentationDepth || " ...")
                            else ()
            let $candidates := $text//tei:*[count(./ancestor-or-self::tei:*) eq $fragmentationDepth]
            let $debug  :=   if ($config:debug = "trace") then
                                console:log("[INDEX] Done. Found " || count($candidates) || " nodes.")
                             else ()
            let $result := if ($candidates) then
                                functx:distinct-nodes(
                                    for $node at $pos in $candidates
                                        let $debug :=   if (($config:debug = "trace") and ($pos eq 1 or $pos mod 50 eq 0)) then
                                                            console:log("[INDEX] Fragment node inventory: get node no. " || string($pos)  || " ...")
                                                        else ()
                                        return
                                            (if ($node/self::tei:div[@type ne "work_part"] or
                                                 $node/self::tei:back or
                                                 $node/self::tei:front or
                                                 $node/self::tei:text[@type eq 'work_volume']) then
                                                $node
                                             else
                                                $node/ancestor::tei:*[self::tei:div[@type ne "work_part"] or
                                                                      self::tei:back or
                                                                      self::tei:front or
                                                                      self::tei:text[@type eq 'work_volume']
                                                                     ][1]
                                            )
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
~   final index creation through index:createIndexNodes().
:)
declare function index:extractNodeStructure($wid as xs:string, $input as node()*, $xincludes as attribute()*, $fragmentIds as map(*)?) as element(sal:node)* {
    for $node in $input return
        typeswitch($node)
            case element() return
                (: index:isIndexNode($node) has already been called in admin:createIndex, so we can use that run here: :)
                if ($node/@xml:id and $fragmentIds($node/@xml:id)) then
                    let $dbg := if ($node/self::tei:pb and functx:is-a-number($node/@n)) then
                                    let $pag := number($node/@n)
                                    return if ($pag mod 100 eq 0 and $config:debug = "trace") then
                                              let $log := util:log('warn', '[INDEX] Processing tei:pb node ' || $node/@n)
                                              return console:log('[INDEX] Processing tei:pb ' || $node/@n/string() || ' ...')
                                           else ()
                                else ()
                    let $subtype := 
                        if ($node[self::tei:milestone]/@n) then (: TODO: where is this used? :)
                            string($node/@n)
                        else if ($node/@type) then
                            string($node/@type)
                        else ()
                    let $isNamedCiteIDNode := if (index:isNamedCiteIDNode($node)) then 'true' else 'false'
(:                    let $isBasicNode := if (index:isBasicNode($node)) then 'true' else 'false':)
(:                    let $category := index:getNodeCategory($node):)
(:                    let $isPassageNode := if (index:isLabelNode($node)) then 'true' else 'false':)
                    return
                        element sal:node {
                            attribute type              {local-name($node)}, 
                            attribute subtype           {$subtype}, 
                            attribute xml:id            {xs:string($node/@xml:id)},
                            if ($node/@xml:id eq 'completeWork' and $xincludes) then
                                attribute xinc          {$xincludes}
                            else (), 
                            attribute class             {index:dispatch($node, 'class')},
(:                            attribute category          {$category},:)
(:                            attribute isBasic           {$isBasicNode},:)
(:                            attribute isPassage         {$isPassageNode},:)
                            attribute title             {index:dispatch($node, 'title')},
                            attribute citableParent     {xs:string(index:getCitableParent($node)/@xml:id)},
                            attribute fragment          {$fragmentIds(xs:string($node/@xml:id))},
                            attribute isNamedCit        {$isNamedCiteIDNode},
                            if ($isNamedCiteIDNode eq 'true') then 
                                attribute cit           {index:dispatch($node, 'citeID')} 
                            else (),
                            if (index:isLabelNode($node)) then 
                                attribute label         {index:dispatch($node, 'label')}
                            else (),
                            element sal:crumb           {index:makeCrumb($wid, $node, $fragmentIds)},
                            (: if the node is a named citeID node, we include its citeID part here already 
                               - unnamed citeIDs can be done much faster in phase 2 :)
                            element sal:children        {index:extractNodeStructure($wid, $node/node(), $xincludes, $fragmentIds)}
                        }
                else index:extractNodeStructure($wid, $node/node(), $xincludes, $fragmentIds)
            default return ()
};

(:
~ Creates a flat structure of index nodes (sal:node) from a hierarchically structured preliminary index (see index:extractNodeStructure()),
~ while enriching those nodes with final citeIDs, crumbtrails, etc.
:)
declare function index:createIndexNodes($input as element(sal:index)) as element(sal:node)* {
    for $node in $input//sal:node return
        let $dbg := if ($node/@type/string() = "pb") then
                        let $pos := count($node/preceding::sal:node[@type/string() = "pb"]) + 1
                        return  if ($pos mod 100 eq 0 and $config:debug = "trace") then
                                      let $log := util:log('warn', '[INDEX] Processing tei:pb node ' || $node/@citeID)
                                      return console:log('[INDEX] Processing tei:pb ' || $node/@citeID/string() || ' ...')
                                else ()
                    else ()

        let $citeID       := index:constructCiteID($node)
        let $label        := index:constructLabel($node)
        let $crumbtrail   := index:constructCrumbtrail($node)
        return
            element sal:node {
                attribute n {xs:string($node/@xml:id)},
                (: copy all attributes from the previous node, except for those we compute anew :)
                $node/@* except ($node/@xml:id, $node/@cit, $node/@label),
                attribute citeID {$citeID},
                attribute label {$label},
                element sal:crumbtrail {$crumbtrail}
            }
};


(: Conducts some basic quality checks with regards to consistency, uniqueness of citeIDs, etc. within an sal:index :)
(: For some reason, this stresses only once cpu core mostly... :) 
declare function index:qualityCheck($index as element(sal:index), 
                                    $work as element(tei:TEI), 
                                    $targetNodes as element()*, 
                                    $fragmentationDepth as xs:integer) {
                                    
    let $wid := $work/@xml:id

    let $removeTempfile :=  if (doc-available($config:temp-root || "/" || $wid || "_nodeIndex.xml")) then
                                xmldb:remove($config:temp-root, $wid || "_nodeIndex.xml")
                            else ()
    let $saveTempfile := xmldb:store($config:temp-root, $wid || "_nodeIndex.xml", $index)

    (: #### Basic quality / consistency check #### :)
    let $resultNodes := $index//sal:node[not(@n eq 'completeWork')]
    let $testNodes := 
        if (count($resultNodes) eq 0) then 
            error(xs:QName('admin:createNodeIndex'), 'Node indexing did not produce any results.') 
        else ()
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 1 passed.") else ()

    (: every ordinary sal:node should have all of the required fields and values: :)
    let $testAttributes := 
        if ($testNodes[not(@class and @type and @n)]) then 
            error(xs:QName('admin:createNodeIndex'), 'Essential attributes are missing in at least one index node (in ' || $wid || ')') 
        else ()
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 2 passed.") else ()

    let $testChildren := if ($testNodes[not(@title and @fragment and @citableParent and @citeID and @label and sal:crumbtrail/*)]) then error() else ()
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 3 passed.") else ()

    (: there should be as many distinctive citeIDs and crumbtrails as there are ordinary sal:node elements: :)
    let $testAmbiguousCiteIDs := 
        if (count($resultNodes) ne count(distinct-values($resultNodes/@citeID))) then 
            let $log := if ($config:debug = ("trace", "info")) then
                            let $line1 := util:log('error', '[WARN] Could not produce a unique citeID for each sal:node (in ' || $wid || '). Problematic nodes: '
                                    || string-join(($resultNodes[@citeID/string() = preceding::sal:node/@citeID/string()]/@n), '; '))
                            let $line2 := util:log('error', serialize(//sal:node[@citeID = ./following::sal:node/@citeID]))
                            return util:log('error', serialize(//sal:node[@citeID = ./preceding::sal:node/@citeID]))
                        else ()
            let $dbg := if ($config:debug = ("trace", "info")) then
                            console:log('[WARN] Could not produce a unique citeID for each sal:node (in '
                                    || $wid || '). Problematic nodes: '
                                    || string-join(($resultNodes[@citeID/string() = preceding::sal:node/@citeID/string()]/@n), '; ')
                                    || ", having citeID"
                                    || string-join(($resultNodes[@citeID/string() = preceding::sal:node/@citeID/string()]/@citeID), "; ") || ".")
                        else ()
            return
                error(xs:QName('admin:createNodeIndex'), 
                  'Could not produce a unique citeID for each sal:node (in ' || $wid || '). Problematic nodes: '
                  || string-join(($resultNodes[@citeID/string() = preceding::sal:node/@citeID/string()]/@n), '; ')) 
        else ()
    (: search these cases using: " //sal:node/@citeID[./string() = following::sal:node/@citeID/string()] :)
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 4 passed.") else ()

    let $testEmptyCiteIDs :=
        if (count($resultNodes[not(./@citeID)]) gt 0) then
            error(xs:QName('admin:createNodeIndex'), 
                  'Could not produce a citeID for one or more sal:node (in' || $wid || '). Problematic nodes: '
                  || string-join(($resultNodes[not(./@citeID)]/@n), '; '))
        else ()
    (: search for " //*[not(./@citeID)] ":)
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 5 passed.") else ()

    (: not checking crumbtrails here ATM for not slowing down index creation too much... :)
    
    (: check whether all text is being captured through basic index nodes (that is, whether every single passage is citable) :)
    let $checkBasicNodes := 
        for $t in $work//tei:text[@type eq 'work_monograph' 
                                  or (@type eq 'work_volume' and sutil:WRKisPublished($wid || '_' || @xml:id))]
                                  //text()[normalize-space() ne ''] return
            if ($t[not(ancestor::*[index:isBasicNode(.)]) and not(ancestor::tei:figDesc)]) then 
                let $debug := util:log('error', 'Encountered text node without ancestor::*[index:isBasicNode(.)], in line ' || $t/preceding::tei:lb[1]/@xml:id/string() || ' – this might indicate a structural anomaly in the TEI data.')
                return error(xs:QName('admin:createNodeIndex'), 'Encountered text node without ancestor::*[index:isBasicNode(.)], in line ' || $t/preceding::tei:lb[1]/@xml:id/string()) 
            else ()
    (: if no xml:id is put out, try to search these cases like so:
        //text//text()[not(normalize-space() eq '')][not(ancestor::*[@xml:id and (self::p or self::signed or self::head or self::titlePage or self::lg or self::item or self::label or self::argument or self::table)])]
    :)
    let $dbg := if ($config:debug = "trace") then console:log("[INDEX] test 6 passed.") else ()

    (: See if there are any leaf elements in our text that are not matched by our rule :)
    let $missed-elements := $work//(tei:front|tei:body|tei:back)//tei:*[count(./ancestor-or-self::tei:*) < $fragmentationDepth][not(*)]
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
        if ($node/@cit) then xs:string($node/@cit) 
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

declare function index:makeCrumb($wid as xs:string, $node as node(), $fragmentIds as map(*)?) as element(a)? {
    let $class := index:dispatch($node, 'class')
    return
        if ($class) then
            <a class="{$class}" href="{index:makeUrl($wid, $node, $fragmentIds)}">{index:dispatch($node, 'title')}</a>
        else 
            <a href="{index:makeUrl($wid, $node, $fragmentIds)}">{index:dispatch($node, 'title')}</a>
};


(: Gets the citable crumbtrail/citeID (not label!) parent :)
declare function index:getCitableParent($node as node()) as node()? {
    if (index:isMarginalNode($node) or index:isAnchorNode($node)) then
        (: notes, milestones etc. must not have p as their citableParent :)
        $node/ancestor::*[index:isStructuralNode(.)][1]
    else if (index:isPageNode($node)) then
        if ($node/ancestor::tei:front|$node/ancestor::tei:back|$node/ancestor::tei:text[1][not(@xml:id = 'completeWork' or @type eq 'work_part')]) then
            (: within front, back, and single volumes, citable parent resolves to one of those elements for avoiding collisions with identically named pb in other parts :)
            ($node/ancestor::tei:front|$node/ancestor::tei:back|$node/ancestor::tei:text[1][not(@xml:id = 'completeWork' or @type eq 'work_part')])[last()]
        else () (: TODO: this makes "ordinary" pb appear outside of any structural hierarchy - is this correct? :)
    else $node/ancestor::*[index:isIndexNode(.)][1]
};


(: Marginal citeIDs: "nX" where X is the anchor used (if it is alphanumeric) and "nXY" where Y is the number of times that X occurs inside the current div
    (important: nodes are citeID children of div (not of p) and are counted as such) :)
declare function index:makeMarginalCiteID($node as element()) as xs:string {
    let $currentSection := sutil:copy(index:getCitableParent($node))
    let $currentNode := $currentSection//*[@xml:id eq $node/@xml:id]
    let $label :=
        if (matches($currentNode/@n, '^[A-Za-z0-9\[\]]+$')) then
            if (count($currentSection//*[index:isMarginalNode(.) and upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))]) gt 1) then
                concat(
                    upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', '')),
                    string(
                        count($currentSection//*[index:isMarginalNode(.) and upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))]
                              intersect $currentNode/preceding::*[index:isMarginalNode(.) and upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))])
                        + 1)
                )
            else upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))
        else string(count($currentNode/preceding::*[index:isMarginalNode(.)] intersect $currentSection//*[index:isMarginalNode(.)]) + 1)
    return 'n' || $label
};

declare function index:makeUrl($targetWorkId as xs:string, $targetNode as node(), $fragmentIds as map()) {
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
declare function index:makeTeaserString($node as element(), $mode as xs:string?) as xs:string {
    let $thisMode := if ($mode = ('orig', 'edit')) then $mode else 'edit'
    let $string := normalize-space(replace(replace(string-join(txt:dispatch($node, $thisMode)), '\[.*?\]', ''), '\{.*?\}', ''))
    return 
        if (string-length($string) gt $config:chars_summary) then
            concat('&#34;', normalize-space(substring($string, 1, $config:chars_summary)), '…', '&#34;')
        else
            concat('&#34;', $string, '&#34;')
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
    xs:boolean(
        index:isIndexNode($node) and
        not($node/ancestor::tei:note) and
        (
            $node/self::tei:text[@type eq 'work_volume'] or
            $node/self::tei:div[$config:citationLabels(@type)?('isCiteRef')] or
            $node/self::tei:milestone[$config:citationLabels(@unit)?('isCiteRef')] or
            $node/self::tei:pb[not(@sameAs or @corresp)] or
            $node[$config:citationLabels(local-name(.))?('isCiteRef') and not(ancestor::tei:note)]
        )
    )
};

(:
~ Determines the set of nodes that are generally citable (and indexed).
:)
declare function index:isIndexNode($node as node()) as xs:boolean {
    typeswitch($node)
        case element() return
            (: any element type relevant for nodetrail creation must be included in one of the following functions: :)
            boolean(
                index:isStructuralNode($node) or
                index:isMainNode($node) or
                index:isMarginalNode($node) or
                index:isAnchorNode($node) or
                index:isPageNode($node) or
                index:isListNode($node)
            )
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
    boolean(
        index:isAnchorNode($node) or
        index:isPageNode($node) or
        index:isMarginalNode($node) or
        (index:isStructuralNode($node) 
            and $node[self::tei:text[@type eq 'work_volume'] or 
                      self::tei:back or 
                      self::tei:front]) or (: TODO: include div here? :)
        (index:isMainNode($node) 
            and $node[self::tei:head or 
                      self::tei:titlePage]) or
        (index:isListNode($node) 
            and $node[self::tei:list[@type = ('dict', 'index')] or
                      self::tei:item[ancestor::tei:list[@type = ('dict')]]])
    )
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
~ Anchor and page nodes occur within main nodes, marginal nodes, or structural nodes,
~ and have no content.
~ More concretely, milestone[@unit ne 'other'] elements
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
        (:and not($node/ancestor::*[index:isMarginalNode(.)]):) (: that shouldn't be possible :)
};

(:
~ Main nodes are mixed-content elements such as tei:p, which may contain marginal or anchor nodes.
~ Note: all main nodes should be citable in the reading view.
:)
declare function index:isMainNode($node as node()) as xs:boolean {
    boolean(
        (
            $node/self::tei:p[@xml:id] or
 $node/self::tei:signed[@xml:id] or
 $node/self::tei:head[@xml:id] or
             $node/self::tei:titlePage[@xml:id] or
              $node/self::tei:lg[@xml:id] or
              $node/self::tei:label[@place ne 'margin'][@xml:id] or
             $node/self::tei:argument[@xml:id]    or
 $node/self::tei:table[@xml:id]
 ) and 
    not($node/ancestor::*[index:isMainNode(.) or index:isMarginalNode(.)             or self::tei:list])
    )
};

(:
~ List nodes are certain nodes within lists (list, item, head) that occur outside of main nodes and marginal nodes.
:)
declare function index:isListNode($node as node()) as xs:boolean {
    boolean(
        (
            $node/self::tei:list[@xml:id] or
            $node/self::tei:item[@xml:id] or
            $node/self::tei:head[ancestor::tei:list][@xml:id] or
            $node/self::tei:argument[ancestor::tei:list][@xml:id]
        ) and 
        not($node/ancestor::*[index:isMainNode(.) or index:isMarginalNode(.)])
    )
};


(:
~ Structural nodes are high-level nodes containing any of the other types of nodes
~ (main, marginal, anchor nodes).
~ More concretely, div, front, back, or text[@type='work_volume'] nodes.
:)
declare function index:isStructuralNode($node as node()) as xs:boolean {
(:
    boolean(
        $node[self::tei:div[@type ne "work_part"] | self::tei:back | self::tei:front | self::tei:text[@type eq 'work_volume'] ]
            [@xml:id]
    )
:)
    boolean(
        $node/self::tei:div[@type ne "work_part"][@xml:id] or (: TODO: comment out for div label experiment :)
        $node/self::tei:back[@xml:id] or
        $node/self::tei:front[@xml:id] or
        $node/self::tei:text[@type eq 'work_volume'][@xml:id]
    )
};


(:
~ Basic nodes represent *all* container nodes at the bottom of the index tree, i.e. mixed-content elements 
    that comprise all text nodes in a sequential, non-overlapping manner. 
    To be used for Sphinx snippets, for checking consistency etc.
:)
declare function index:isBasicNode($node as node()) as xs:boolean {
    boolean(
        index:isMainNode($node) or
        index:isMarginalNode($node) or
        (:(index:isListNode($node) and not($node/descendant::*[index:isListNode(.)])):)
        (index:isListNode($node) and (($node/self::tei:list and not($node/descendant::tei:list))
                 or ($node[(self::tei:item or                  self::tei:head or self::tei:argument) 
                  and not(descendant::tei:list) 
                  and following-sibling::tei:item[./tei:list[index:isListNode(.)]]])
                 )
                 (: read as: 'lists that do not contain lists (=lists at the lowest level), or siblings thereof' :)
        (: (this is quite a complicated XPath, but I don't know how to simplify it without breaking things...) :)
        )
    )
};


declare function index:getNodeCategory($node as element()) as xs:string {
    if (index:isMainNode($node)) then 'main'
    else if (index:isMarginalNode($node)) then 'marginal'
    else if (index:isStructuralNode($node)) then 'structural'
    else if (index:isListNode($node)) then 'list'
    else if (index:isPageNode($node)) then 'page'
    else if (index:isAnchorNode($node)) then 'anchor'
    else error()
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
declare function index:dispatch($node as node(), $mode as xs:string) {
    typeswitch($node)
    (: Try to sort the following nodes based (approx.) on frequency of occurences, so fewer checks are needed. :)
        case element(tei:pb)            return index:pb($node, $mode)
        case element(tei:head)          return index:head($node, $mode)
        case element(tei:p)             return index:p($node, $mode)
        case element(tei:signed)        return index:signed($node, $mode)
        case element(tei:note)          return index:note($node, $mode)
        case element(tei:div)           return index:div($node, $mode)
        case element(tei:milestone)     return index:milestone($node, $mode)
        
        case element(tei:list)          return index:list($node, $mode)
        case element(tei:item)          return index:item($node, $mode)

        case element(tei:lg)            return index:lg($node, $mode)
        
        case element(tei:table)         return index:table($node, $mode)
        
        case element(tei:label)         return index:label($node, $mode)
        case element(tei:argument)      return index:argument($node, $mode)
        
        case element(tei:titlePage)     return index:titlePage($node, $mode)
        case element(tei:titlePart)     return index:titlePart($node, $mode)
        
        case element(tei:front)         return index:front($node, $mode) 
        case element(tei:body)          return index:body($node, $mode)
        case element(tei:back)          return index:back($node, $mode)
        case element(tei:text)          return index:text($node, $mode)
        
        case element(tei:figDesc)       return ()
        case element(tei:teiHeader)     return ()
        case element(tei:fw)            return ()
        case element()                  return error(QName('index:dispatch', 'Unknown element: ' || local-name($node) || ' (in mode: "' || $mode || '")'))
        case comment()                  return ()
        case processing-instruction()   return ()

        default return ()
};


(: ####++++ Element functions (ordered alphabetically) ++++#### :)


declare function index:argument($node as element(tei:argument), $mode as xs:string) {
    switch($mode)
        case 'class' return 
            'tei-' || local-name($node)
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


declare function index:div($node as element(tei:div), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                    '"' || string($node/@n) || '"'
                else if ($node/(tei:head|tei:label)) then
                    index:makeTeaserString(($node/(tei:head|tei:label))[1], 'edit')
                (: purely numeric section titles: :)
                else if ($node/@n and (matches($node/@n, '^[0-9\[\]]+$')) and ($node/@type)) then
                    string($node/@n)
                (: otherwise, try to derive a title from potential references to the current node :)
                else if ($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)]) then
                    index:makeTeaserString($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)][1], 'edit')
                (: if there is a list/head and nothing else works, we may use that :)
                else if ($node/tei:list/(tei:head|tei:label)) then
                    index:makeTeaserString(($node/tei:list/(tei:head|tei:label))[1], 'edit')
                else ()
            )
            
        case 'class' return
            'tei-div-' || $node/@type
        
        case 'citeID' return
            if (index:isNamedCiteIDNode($node)) then
                (: use abbreviated form of @type (without dot), possibly followed by position :)
                (: TODO: div label experiment (delete the following block if this isn't deemed plausible) :)
                let $abbr := $config:citationLabels($node/@type)?('abbr')
                let $prefix :=
                    if ($abbr) then 
                        lower-case(if (contains($abbr, '.')) then substring-before($config:citationLabels($node/@type)?('abbr'), '.') else $abbr)
                    else 'div' (: divs for which we haven't defined an abbr. :)
                let $position :=
                    if (count($node/parent::*[self::tei:body or index:isIndexNode(.)]/tei:div[$config:citationLabels(@type)?('abbr') eq $config:citationLabels($node/@type)?('abbr')]) gt 1) then
                        string(count($node/preceding-sibling::tei:div[$config:citationLabels(@type)?('abbr') eq $config:citationLabels($node/@type)?('abbr')]) + 1)
                    else ()
                return $prefix || $position
            else error()
        
        case 'label' return
            if (index:isLabelNode($node)) then
                let $prefix := lower-case($config:citationLabels($node/@type)?('abbr')) (: TODO: upper-casing with first element of label ? :)
                return 
                    if ($node/@type = ('lecture', 'gloss')) then (: TODO: 'lemma'? :)
                        (: special cases: with these types, we provide a short teaser string instead of a numeric value :)
                        let $teaser := '"' || normalize-space(substring(substring-after(index:div($node, 'title'), '"'),1,15)) || '…"'
                        return $prefix || ' ' || $teaser
                    else
                        let $position := 
                            if ($node/@n[matches(., '^[0-9\[\]]+$')]) then $node/@n (:replace($node/@n, '[\[\]]', '') ? :)
                            else if ($node/ancestor::*[index:isLabelNode(.)]) then
                                (: using the none-copy version here for sparing memory: :)
                                if (count($node/ancestor::*[index:isLabelNode(.)][1]//tei:div[@type eq $node/@type and index:isLabelNode(.)]) gt 1) then 
                                    string(count($node/ancestor::*[index:isLabelNode(.)][1]//tei:div[@type eq $node/@type and index:isLabelNode(.)]
                                                 intersect $node/preceding::tei:div[@type eq $node/@type and index:isLabelNode(.)]) + 1)
                                else ()
                            else if (count($node/parent::*/tei:div[@type eq $node/@type]) gt 1) then 
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
declare function index:head($node as element(tei:head), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                index:makeTeaserString($node, 'edit')
            )
        
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citeID' return
            'heading' ||
            (if (count($node/parent::*/tei:head) gt 1) then          
                (: we have several headings on this level of the document ... :)
                string(count($node/preceding-sibling::tei:head) + 1)
             else ())
        
        default return 
            ()
};

declare function index:item($node as element(tei:item), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                if ($node/parent::tei:list/@type='dict' and $node//tei:term[1][@key]) then
                    (: TODO: collision with div/@type='lemma'? :)
                    let $positionStr := 
                        if (count($node/parent::tei:list/tei:item[.//tei:term[1]/@key eq $node//tei:term[1]/@key]) gt 1) then
                             ' - ' || 
                             string(count($node/preceding::tei:item[tei:term[1]/@key eq $node//tei:term[1]/@key] 
                                          intersect $node/ancestor::tei:div[1]//tei:item[tei:term[1]/@key eq $node//tei:term[1]/@key]) + 1)
                        else ()
                    return
                        '"' || $node//tei:term[1]/@key || $positionStr || '"'
                else if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                    '"' || string($node/@n) || '"'
                else if ($node/(tei:head|tei:label)) then
                    index:makeTeaserString(($node/(tei:head|tei:label))[1], 'edit')
                (: purely numeric section titles: :)
                else if ($node/@n and (matches($node/@n, '^[0-9\[\]]+$'))) then
                    $node/@n/string()
                (: otherwise, try to derive a title from potential references to the current node :)
                else if ($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)]) then
                    index:makeTeaserString($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)][1], 'edit')
                else ()
            )
        
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            (: "entryX" where X is the section title (index:item($node, 'title')) in capitals, use only for items in indexes and dictionary :)
            if(index:isNamedCiteIDNode($node)) then
                let $title := upper-case(replace(index:item($node, 'title'), '[^a-zA-Z0-9]', ''))
                let $position :=
                    if ($title) then
                        let $siblings := $node/parent::tei:list/tei:item[upper-case(replace(index:item(., 'title'), '[^a-zA-Z0-9]', '')) eq $title]
                        return
                            if (count($siblings) gt 0) then 
                                string(count($node/preceding-sibling::tei:item intersect $siblings) + 1)
                            else ()
                    else if (count($node/parent::tei:list/tei:item) gt 0) then 
                        string(count($node/preceding-sibling::tei:item) + 1)
                    else ()
                return 'entry' || $title || $position
            else error() 
        
        case 'label' return
            ()
        
        default return
            ()
};


declare function index:label($node as element(tei:label), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                index:makeTeaserString($node, 'edit')
            )
          
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            if (index:isNamedCiteIDNode($node)) then
                index:makeMarginalCiteID($node)
            else error()
            
        default return
            ()
};


declare function index:list($node as element(tei:list), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                    '"' || string($node/@n) || '"'
                else if ($node/(tei:head|tei:label)) then
                    index:makeTeaserString(($node/(tei:head|tei:label))[1], 'edit')
                (: purely numeric section titles: :)
                else if ($node/@n and (matches($node/@n, '^[0-9\[\]]+$')) and ($node/@type)) then
                    $node/@n/string()
                (: otherwise, try to derive a title from potential references to the current node :)
                else if ($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)]) then
                    index:makeTeaserString($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)][1], 'edit')
                else ()
            )
        
        case 'class' return
            'tei-' || local-name($node)
            
        case 'label' return
            ()
        
        case 'citeID' return
            (: dictionaries, indices and summaries get their type prepended to their number :)
            if(index:isNamedCiteIDNode($node)) then
(:                let $currentSection := sutil:copy($node/(ancestor::tei:div|ancestor::tei:body|ancestor::tei:front|ancestor::tei:back)[last()]):)
                let $currentSection := $node/(ancestor::tei:div|ancestor::tei:body|ancestor::tei:front|ancestor::tei:back)[last()]
(:                let $currentNode := $currentSection//tei:list[@xml:id eq $node/@xml:id]:)
                return
                  concat(
                      xs:string($node/@type), 
                      string(
                          count($node/preceding::tei:list[@type eq $node/@type]
                                intersect $currentSection//tei:list[@type eq $node/@type]
                          ) + 1)
                  )
            else error()
            
        default return
            ()
};


declare function index:lg($node as element(tei:lg), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                index:makeTeaserString($node, 'edit')
            )
            
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            error()
            
        default return
            ()
};

declare function index:milestone($node as element(tei:milestone), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                    '"' || string($node/@n) || '"'
                (: purely numeric section titles: :)
                (:else if (matches($node/@n, '^[0-9\[\]]+$') and $node/@unit eq 'number') then
                    $node/@n/string():)
                (: use @unit to derive a title: :)
                (:else if (matches($node/@n, '^\[?[0-9]+\]?$') and $node/@unit[. ne 'number']) then
                    $config:citationLabels($node/@unit)?('abbr') || ' ' || $node/@n:)
                (: if milestone has numerical information, just state the number, regardless of @unit and other attributes: :)
                else if (matches($node/@n, '^[0-9\[\]]+$')) then
                    $node/@n/string()
                (: otherwise, try to derive a title from potential references to the current node :)
                else if ($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)]) then
                    index:makeTeaserString($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)][1], 'edit')
                else ()
            )
            
        case 'class' return
            'tei-ms-' || $node/@unit
            
        case 'citeID' return
            (: "XY" where X is the unit and Y is the anchor or the number of milestones where this occurs :)
            let $currentSection := sutil:copy(index:getCitableParent($node))
            let $currentNode := $currentSection//tei:milestone[@xml:id eq $node/@xml:id]
            return
                if ($node/@n[matches(., '[a-zA-Z0-9]')]) then 
                    let $similarMs :=
                        $currentSection//tei:milestone[@unit eq $currentNode/@unit 
                                                       and upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))]
                    let $position :=
                        if (count($similarMs) gt 1) then
                            (: put 'N' between @n and position, so as to avoid collisions :)
                            'N' || string(count($currentNode/preceding::tei:milestone intersect $similarMs) + 1)
                        else ()
                    return $currentNode/@unit || upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', '')) || $position
                else $currentNode/@unit || string(count($currentNode/preceding::tei:milestone[@unit eq $node/@unit] intersect $currentSection//tei:milestone[@unit eq $currentNode/@unit]) + 1)
        
        case 'label' return
            if (index:isLabelNode($node)) then
                (: TODO: ATM milestone/@unit = ('article', 'section') resolves to the same abbrs as div/@type = ('article', 'section') :)
                (: TODO: if @n is numeric, always resolve to 'num.' ? :)
                let $prefix := lower-case($config:citationLabels($node/@unit)?('abbr'))
                let $num := 
                    if ($node/@n[matches(., '^[0-9\[\]]+$')]) then $node/@n (:replace($node/@n, '[\[\]]', '') ? :)
                    else 
                        let $currentSection := sutil:copy($node/ancestor::*[index:isLabelNode(.) and not(self::tei:p)][1])
                        let $currentNode := $currentSection//tei:milestone[@xml:id eq $node/@xml:id]
                        let $position := count($currentSection//tei:milestone[@unit eq $currentNode/@unit and index:isLabelNode(.)]
                                               intersect $currentNode/preceding::tei:milestone[@unit eq $currentNode/@unit and index:isLabelNode(.)]) + 1
                        return string($position)
                return
                    $prefix || ' ' || $num
            else ()
        
        default return () (: also for snippets-orig, snippets-edit :)
};


declare function index:note($node as element(tei:note), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                let $currentSection := sutil:copy(index:getCitableParent($node))
                let $currentNode := $currentSection//tei:note[@xml:id eq $node/@xml:id]
                return
                    if ($node/@n) then
                        let $noteNumber :=
                            if (count($currentSection//tei:note[upper-case(normalize-space(@n)) eq upper-case(normalize-space($currentNode/@n))]) gt 1) then
                                ' (' || 
                                string(count($currentNode/preceding::tei:note[upper-case(normalize-space(@n)) eq upper-case(normalize-space($currentNode/@n))] 
                                             intersect $currentSection//tei:note[upper-case(normalize-space(@n)) eq upper-case(normalize-space($currentNode/@n))])
                                       + 1) 
                                || ')'
                            else ()
                        return '"' || normalize-space($currentNode/@n) || '"' || $noteNumber
                    else string(count($currentNode/preceding::tei:note intersect $currentSection//tei:note) + 1)
            )
        
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citeID' return
            index:makeMarginalCiteID($node)
        
        case 'label' return
            if (index:isLabelNode($node)) then
                (: labelled parents of note are div, not p :)
                let $currentSection := sutil:copy($node/ancestor::*[index:isLabelNode(.) and not(self::tei:p)][1])
                let $currentNode := $currentSection//tei:note[@xml:id eq $node/@xml:id]
                let $prefix := $config:citationLabels(local-name($node))?('abbr')
                let $label := 
                    if ($node/@n) then '"' || $node/@n || '"' (: TODO: what if there are several notes with the same @n in a div :)
                    else string(count($currentSection//tei:note
                                      intersect $currentNode/preceding::tei:note) + 1)
                return $prefix || ' ' || $label
            else ()
        
        default return
            ()
};


declare function index:p($node as element(tei:p), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                index:makeTeaserString($node, 'edit')
            )
        
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citeID' return
            error()
        
        case 'label' return
            if (index:isLabelNode($node)) then
                let $prefix := $config:citationLabels(local-name($node))?('abbr')
                let $teaser := '"' || normalize-space(substring(substring-after(index:p($node, 'title'), '"'),1,15)) || '…"'(: short teaser :)
                return $prefix || ' ' || $teaser
            else ()
        
        default return
            ()
};


(:declare function index:passthru($nodes as node()*, $mode as xs:string) as item()* {
    for $node in $nodes/node() return index:dispatch($node, $mode)
};:)


declare function index:pb($node as element(tei:pb), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                (: any pb with @sameAs and @corresp probably won't even get reached, since they typically have note ancestors :)
                if ($node/@sameAs) then
                    concat('[pb_sameAs_', $node/@sameAs, ']')
                else if ($node/@corresp) then
                    concat('[pb_corresp_', $node/@corresp, ']')
                else
                    (: not prepending 'Vol. ' prefix here :)
                    if (contains($node/@n, 'fol.')) then 
                        $node/@n
                    else
                        'p. ' || $node/@n
            )
        
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citeID' return
            (: "pX" where X is page number :)
            concat('p',
                if (matches($node/@n, '[\[\]A-Za-z0-9]') 
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
            if (contains($node/@n, 'fol.')) then $node/@n/string()
            else 'p. ' || $node/@n/string()
        
        (: pb nodes are good candidates for tracing the speed/performance of document processing, 
            since they are equally distributed throughout a document :)
        case 'debug' return
            let $log := util:log('warn', '[INDEX] Processing tei:pb node ' || $node/@xml:id)
            let $dbg := console:log('[INDEX] Processing tei:pb ' || $node/@xml:id/string() || ' ...')
            return ()
        default return ()
};

declare function index:signed($node as element(tei:signed), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                index:makeTeaserString($node, 'edit')
            )
        
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            error()
            
        default return
            ()
};


declare function index:table($node as element(tei:table), $mode as xs:string) {
    switch($mode)
        case 'title' return
            if ($node/tei:head) then
                normalize-space(
                    index:makeTeaserString($node/tei:head, 'edit')
                )
            else ()
            
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            error()
            
        default return
            ()
};


declare function index:text($node as element(tei:text), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                if ($node/@type eq 'work_volume') then
                    $node/@n/string()
                (: tei:text with solely technical information: :)
                else if ($node/@xml:id eq 'completeWork') then
                    '[complete work]'
                else if (matches($node/@xml:id, 'work_part_[a-z]')) then
                    '[process-technical part: ' || substring(string($node/@xml:id), 11, 1) || ']'
                else ()
            )
        
        case 'class' return
            if ($node/@type eq 'work_volume') then 'tei-text-' || $node/@type
            else if ($node/@xml:id eq 'completeWork') then 'tei-text-' || $node/@xml:id
            else if (matches($node/@xml:id, 'work_part_[a-z]')) then 'elem-text-' || $node/@xml:id
            else 'tei-text'
        
        case 'citeID' return
            (: "volX" where X is the current volume number, don't use it at all for monographs :)
            if ($node/@type eq 'work_volume') then
               concat('vol', count($node/preceding::tei:text[@type eq 'work_volume']) + 1)
            else ()
        
        case 'label' return
            if (index:isLabelNode($node)) then
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

declare function index:titlePart($node as element(tei:titlePart), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                index:makeTeaserString($node, 'edit')
            )
            
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citeID' return
            (: "titlePage.X" where X is the number of parts where this occurs :)
            concat('titlepage.', string(count($node/preceding-sibling::tei:titlePart) + 1))
        
        default return 
            ()
};
