xquery version "3.1";

module namespace render            = "http://salamanca/render";
declare namespace exist            = "http://exist.sourceforge.net/NS/exist";
declare namespace output           = "http://www.w3.org/2010/xslt-xquery-serialization";
declare namespace tei              = "http://www.tei-c.org/ns/1.0";
declare namespace sal              = "http://salamanca.adwmainz.de";
declare namespace i18n             = 'http://exist-db.org/xquery/i18n';
import module namespace util       = "http://exist-db.org/xquery/util";
import module namespace console    = "http://exist-db.org/xquery/console";
import module namespace config     = "http://salamanca/config" at "../../modules/config.xqm";
import module namespace app        = "http://salamanca/app"    at "../../modules/app.xql";
import module namespace sal-util    = "http://salamanca/sal-util" at "../../modules/sal-util.xql";
(:import module namespace i18n      = "http://exist-db.org/xquery/i18n"        at "i18n.xql";:)

(:declare option exist:serialize       "method=html5 media-type=text/html indent=no";:)

(: *Work* rendering functions and settings :)

(: TODO: some of the functions here are also used by non-work entity rendering procedures (such as WP snippet rendering)
         - these should eventually have there own rendering functions/modules at some point :)

(: SETTINGS :)

(: the max. amount of characters to be shown in a note teaser :)
declare variable $render:noteTruncLimit := 33;
(: the max. amount of characters to be shown in a title teaser :)
declare variable $render:titleTruncLimit := 15;

(:declare variable $render:teaserTruncLimit := 45;:)

declare variable $render:basicElemNames := ('p', 'head', 'note', 'item', 'cell', 'label', 'signed', 'lg', 'titlePage');

declare variable $render:citetrailConnector := '.';
declare variable $render:passagetrailConnector := ' ';
declare variable $render:crumbtrailConnector := ' » ';



(: ####++++---- NODE INDEX functions ----++++#### :)


(:
~ Creates a tree of index nodes (sal:node), where nodes are hierarchically nested according to the hierarchy of nodes in the original TEI tree.
~ Supplies nodes with basic information (sal:title, sal:passage, etc.), while temporary elements/attributes provide 
~ information that can be used for the production of citetrails, crumbtrails etc. in the 
~ final index creation through render:createIndexNodes().
:)
declare function render:extractNodeStructure($wid as xs:string, $input as node()*, $xincludes as attribute()*, $fragmentIds as map()?) as element(sal:node)* {
    for $node in $input return
        typeswitch($node)
            case element() return
                if (:(render:isIndexNode($node)):) ($node/@xml:id and $fragmentIds($node/@xml:id)) then
                    let $debug := if ($config:debug = ("trace") and $node/self::tei:pb) then render:pb($node, 'debug') else ()
                    let $subtype := 
                        (: 'sameAs', 'corresp' and 'work_part' are excluded through render:isIndexNode(): :)
                        (:if ($node/@sameAs) then
                            "sameAs"
                        else if ($node/@corresp) then
                            "corresp"
                        else if ($node/@type eq "work_part") then
                            "work_part"
                        else:) 
                        if ($node[self::tei:milestone]/@n) then (: TODO: where is this used? :)
                            string($node/@n)
                        else if ($node/@type) then
                            string($node/@type)
                        else ()
(:                    let $isBasicNode := if (render:isBasicNode($node)) then 'true' else 'false':)
                    let $isNamedCitetrailNode := if (render:isNamedCitetrailNode($node)) then 'true' else 'false'
(:                    let $category := render:getNodeCategory($node):)
(:                    let $isPassageNode := if (render:isPassagetrailNode($node)) then 'true' else 'false':)
                    return
                        element sal:node {
                            attribute type              {local-name($node)}, 
                            attribute subtype           {$subtype}, 
                            attribute xml:id                 {$node/@xml:id/string()},
                            if ($node/@xml:id eq 'completeWork' and $xincludes) then
                                attribute xinc          {$xincludes}
                            else (), 
                            attribute class             {render:dispatch($node, 'class')},
(:                            attribute category          {$category},:)
(:                            attribute isBasic           {$isBasicNode},:)
                            attribute isNamedCit        {$isNamedCitetrailNode},
(:                            attribute isPassage         {$isPassageNode},:)
                            element sal:title           {render:dispatch($node, 'title')},
                            element sal:fragment        {$fragmentIds($node/@xml:id/string())},
                            element sal:crumb           {render:makeCrumb($wid, $node, $fragmentIds)},
                            if (render:isPassagetrailNode($node)) then 
                                element sal:passage {render:dispatch($node, 'passagetrail')}
                            else (),
                            element sal:citableParent   {render:getCitableParent($node)/@xml:id/string()},
                            (: if the node is a named citetrail node, we include its citetrail part here already 
                               - unnamed citetrails can be done much faster in phase 2 :)
                            if ($isNamedCitetrailNode eq 'true') then 
                                element sal:cit {render:dispatch($node, 'citetrail')} 
                            else (),
                            element sal:children        {render:extractNodeStructure($wid, $node/node(), $xincludes, $fragmentIds)}
                        }
                else render:extractNodeStructure($wid, $node/node(), $xincludes, $fragmentIds)
            default return ()
};

(:
~ Creates a flat structure of index nodes (sal:node) from a hierarchically structured preliminary index (see render:extractNodeStructure()),
~ while enriching those nodes with final citetrails, crumbtrails, etc.
:)
declare function render:createIndexNodes($input as element(sal:index)) as element(sal:node)* {
    for $node in $input//sal:node return
        let $citetrail := render:constructCitetrail($node)
        let $crumbtrail := render:constructCrumbtrail($node)
        let $passagetrail := render:constructPassagetrail($node)
        return
            element sal:node {
                (: copy some elements and attributes from the previous node :)
                attribute n {$node/@xml:id/string()},
                $node/@* except ($node/@category, $node/@isBasicNode, $node/@isNamedCit, $node/@isPassage, $node/@xml:id),
                $node/sal:title, $node/sal:fragment, $node/sal:citableParent,
                element sal:citetrail {$citetrail},
                element sal:crumbtrail {$crumbtrail},
                element sal:passagetrail {$passagetrail}
            }
};

(: Construct passagetrail, citetrail, crumbtrail -- DEEP RECURSION :)
declare function render:constructCitetrail($node as element(sal:node)) as xs:string {
    let $prefix := 
        if ($node/sal:citableParent/text() and $node/ancestor::sal:node[@xml:id eq $node/sal:citableParent/text()]) then
            render:constructCitetrail($node/ancestor::sal:node[@xml:id eq $node/sal:citableParent/text()])
        else ()
    let $this := 
        if ($node/sal:cit) then $node/sal:cit/text() 
        (: if sal:cit doesn't already exist, we are dealing with a numeric/unnamed citetrail node and create the citetrail part here: :)
        else string(count($node/preceding-sibling::sal:node[@isNamedCit eq 'false']) + 1)
    return
        if ($prefix and $this) then $prefix || $render:citetrailConnector || $this else $this
};

declare function render:constructCrumbtrail($node as element(sal:node)) as item()+ {
    let $prefix := 
        if ($node/sal:citableParent/text() and $node/ancestor::sal:node[@xml:id eq $node/sal:citableParent/text()]) then
            render:constructCrumbtrail($node/ancestor::sal:node[@xml:id eq $node/sal:citableParent/text()])
        else ()
    let $this := $node/sal:crumb/*
    return
        if ($prefix and $this) then ($prefix, $render:crumbtrailConnector, $this) else $this
};

declare function render:constructPassagetrail($node as element(sal:node)) as xs:string? {
    let $prefix := 
        if ($node/sal:citableParent/text() and $node/ancestor::sal:node[@xml:id eq $node/sal:citableParent/text()]) then
            render:constructPassagetrail($node/ancestor::sal:node[@xml:id eq $node/sal:citableParent/text()])
        else ()
    (: not every sal:node has a distinctive passage: :)
    let $this := if ($node/sal:passage/text()) then $node/sal:passage/text() else ''
    return
        if ($prefix and $this) then 
            $prefix || $render:passagetrailConnector || $this 
        else $prefix || $this (: this will only return one of both, if any at all :)
};

declare function render:makeCrumb($wid as xs:string, $node as node(), $fragmentIds as map()?) as element(a)? {
    let $class := render:dispatch($node, 'class')
    return
        if ($class) then
            <a class="{$class}" href="{render:mkUrlWhileRendering($wid, $node, $fragmentIds)}">{render:dispatch($node, 'title')}</a>
        else 
            <a href="{render:mkUrlWhileRendering($wid, $node, $fragmentIds)}">{render:dispatch($node, 'title')}</a>
};


(: Gets the citable crumbtrail/citetrail (not passagetrail!) parent :)
declare function render:getCitableParent($node as node()) as node()? {
    if (render:isMarginalNode($node) or render:isAnchorNode($node)) then
        (: notes, milestones etc. must not have p as their citableParent :)
        $node/ancestor::*[render:isStructuralNode(.)][1]
    else if (render:isPageNode($node)) then
        if ($node/ancestor::tei:front|$node/ancestor::tei:back|$node/ancestor::tei:text[1][not(@xml:id = 'completeWork' or @type eq 'work_part')]) then
            (: within front, back, and single volumes, citable parent resolves to one of those elements for avoiding collisions with identically named pb in other parts :)
            ($node/ancestor::tei:front|$node/ancestor::tei:back|$node/ancestor::tei:text[1][not(@xml:id = 'completeWork' or @type eq 'work_part')])[last()]
        else () (: TODO: this makes "ordinary" pb appear outside of any structural hierarchy - is this correct? :)
    else $node/ancestor::*[render:isIndexNode(.)][1]
};


(: Marginal citetrails: "nX" where X is the anchor used (if it is alphanumeric) and "nXY" where Y is the number of times that X occurs inside the current div
    (important: nodes are citetrail children of div (not of p) and are counted as such) :)
declare function render:makeMarginalCitetrail($node as element()) as xs:string {
    let $currentSection := sal-util:copy(render:getCitableParent($node))
    let $currentNode := $currentSection//*[@xml:id eq $node/@xml:id]
    let $label :=
        if (matches($currentNode/@n, '^[A-Za-z0-9\[\]]+$')) then
            if (count($currentSection//*[render:isMarginalNode(.) and upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))]) gt 1) then
                concat(
                    upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', '')),
                    string(
                        count($currentSection//*[render:isMarginalNode(.) and upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))]
                              intersect $currentNode/preceding::*[render:isMarginalNode(.) and upper-case(replace(@n, '[^a-zA-Z0-9]', '')) eq upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))])
                        + 1)
                )
            else upper-case(replace($currentNode/@n, '[^a-zA-Z0-9]', ''))
        else string(count($currentNode/preceding::*[render:isMarginalNode(.)] intersect $currentSection//*[render:isMarginalNode(.)]) + 1)
    return 'n' || $label
};



(: DEPRECATED - unnamed citetrail nodes are now processed during index time
~ Get the number of preceding nodes with purely numeric citetrails, in the same section, but (potentially) on different tree layers.
:)
(:declare function render:determineUnnamedCitetrailNodePosition($node as element()) as xs:integer {
    let $citableParent := render:getCitableParent($node)
    return
        if ($citableParent) then
            let $currentSection := sal-util:copy($citableParent) (\: make copy on-the-fly for not going into performance trouble with large trees... :\)
            let $allUnnamed := $currentSection//*[render:isUnnamedCitetrailNode(.)]
            let $sameLevelUnnamed := $allUnnamed[render:getCitableParent(.) is $currentSection] (\: count(render:getCitableParent(.) | $thisCitableParent) eq 1 :\)
            let $precedingUnnamed := $sameLevelUnnamed[following::*[@xml:id eq $node/@xml:id]]
            return
                count($precedingUnnamed) + 1
        else 
            count($node/preceding-sibling::*[render:isUnnamedCitetrailNode(.)]) + 1
};:)


(: ####++++---- BOOLEAN FUNCTIONS for defining different classes of nodes ----++++####  :)

(:
~ Determines which nodes serve for "passagetrail" production.
:)
declare function render:isPassagetrailNode($node as element()) as xs:boolean {
    boolean(
        render:isIndexNode($node) and
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
~ Determines which nodes to make HTML teasers for (subset of //*[render:isIndexNode(.)] excluding low-level elements). 
    Should mostly be used together with render:makeHTMLSummaryTitle()
:)
declare function render:isCitableWithTeaserHTML($node as node()) as xs:boolean {
    boolean(
        not(render:isBasicNode($node)) and
        (
            (render:isStructuralNode($node) and $node[self::tei:div or self::tei:text]) or
            render:isAnchorNode($node)
            (: TODO: lists here? :)
        ) and
        render:dispatch($node, 'html-title') (: if there is no title, we won't make a teaser :)
    )
};

(:
~ Other nodes that are citable, but without summary title / teaser.
:)
(:declare function render:isCitableWithoutTeaserHTML($node as node()) as xs:boolean {
    boolean(...)
};:)

(:
~ Determines whether a node should have a citation anchor, without an additional teaser.
:)
(:declare function render:isBasicCitableHTML($node as node()) as xs:boolean {
    render:isMainNode($node) or render:isMarginalNode($node)
};:)

(:
~ Determines the set of nodes that are generally citable (and indexed).
:)
declare function render:isIndexNode($node as node()) as xs:boolean {
    typeswitch($node)
        case element() return
            (: any element type relevant for nodetrail creation must be included in one of the following functions: :)
            boolean(
                render:isStructuralNode($node) or
                render:isMainNode($node) or
                render:isMarginalNode($node) or
                render:isAnchorNode($node) or
                render:isPageNode($node) or
                render:isListNode($node)
            )
        default return 
            false()
};

(:
~ Determines whether a node is a specific citetrail element, i.e. one that is specially prefixed in citetrails.
:)
declare function render:isNamedCitetrailNode($node as element()) as xs:boolean {
    boolean(
        render:isAnchorNode($node) or
        render:isPageNode($node) or
        render:isMarginalNode($node) or
        (render:isStructuralNode($node) 
            and $node[self::tei:text[@type eq 'work_volume'] or 
                      self::tei:back or 
                      self::tei:front]) or (: TODO: include div here? :)
        (render:isMainNode($node) 
            and $node[self::tei:head or 
                      self::tei:titlePage]) or
        (render:isListNode($node) 
            and $node[self::tei:list[@type = ('dict', 'index')] or
                      self::tei:item[ancestor::tei:list[@type = ('dict')]]])
    )
};

(:
~ Determines whether a node is a 'generic' citetrail element, i.e. one that isn't specially prefixed in citetrails.
~ --> complement of render:isNamedCitetrailNode()
:)
declare function render:isUnnamedCitetrailNode($node as element()) as xs:boolean {
    render:isIndexNode($node) and not(render:isNamedCitetrailNode($node))
};

(:
~ Basically, we can determine several types of elements in a TEI/text tree:
:)

(:
~ Anchor and page nodes occur within main nodes, marginal nodes, or structural nodes, and have no content.
~ (NOTE: should work with on-the-fly copying of sections. )
:)
declare function render:isAnchorNode($node as node()) as xs:boolean {
    boolean(
        $node/@xml:id and
        $node/self::tei:milestone[@unit ne 'other']
    )
};

(:
~ Page nodes are regular page breaks.
~ (NOTE: should work with on-the-fly copying of sections. )
:)
declare function render:isPageNode($node as node()) as xs:boolean {
    boolean(
        $node/@xml:id and
        $node/self::tei:pb[not(@sameAs or @corresp)]
    )
};

(:
~ Marginal nodes occur within structural or main nodes.
~ (NOTE: should work with on-the-fly copying of sections. )
:)
declare function render:isMarginalNode($node as node()) as xs:boolean {
    boolean(
        $node/@xml:id and
        (
            $node/self::tei:note[@place eq 'margin'] or
            $node/self::tei:label[@place eq 'margin']
        )
        (:and not($node/ancestor::*[render:isMarginalNode(.)]):) (: that shouldn't be possible :)
    )
};

(:
~ Main nodes are mixed-content elements such as tei:p, which may contain marginal or anchor nodes.
~ Note: all main nodes should be citable in the reading view.
:)
declare function render:isMainNode($node as node()) as xs:boolean {
    boolean(
        $node/@xml:id and
        (
            $node/self::tei:p or
            $node/self::tei:signed or
            $node/self::tei:head[not(ancestor::tei:list)] or
            $node/self::tei:titlePage or
            $node/self::tei:lg or
            $node/self::tei:label[@place ne 'margin'] or
            $node/self::tei:argument[not(ancestor::tei:list)] or
            $node/self::tei:table
        ) and 
        not($node/ancestor::*[render:isMainNode(.) or render:isMarginalNode(.) or self::tei:list])
    )
};

(:
~ List nodes are certain nodes within lists (list, item, head) that occur outside of main nodes and marginal nodes.
:)
declare function render:isListNode($node as node()) as xs:boolean {
    boolean(
        $node/@xml:id and
        (
            $node/self::tei:list or
            $node/self::tei:item or
            $node/self::tei:head[ancestor::tei:list] or
            $node/self::tei:argument[ancestor::tei:list]
        ) and 
        not($node/ancestor::*[render:isMainNode(.) or render:isMarginalNode(.)])
    )
};


(:
~ Structural nodes are high-level nodes containing any of the other types of nodes (main, marginal, anchor nodes).
:)
declare function render:isStructuralNode($node as node()) as xs:boolean {
    boolean(
        $node/@xml:id and
        (
            $node/self::tei:div[@type ne "work_part"] or (: TODO: comment out for div label experiment :)
            $node/self::tei:back or
            $node/self::tei:front or
            $node/self::tei:text[@type eq 'work_volume']
        )
    )
};


(:
~ Basic nodes represent *all* elements at the bottom of the tree, i.e. all mixed-content elements 
    that, in total, comprise all text nodes. To be used for Sphinx snippets, for checking consistency etc.
:)
declare function render:isBasicNode($node as node()) as xs:boolean {
    boolean(
        render:isMainNode($node) or
        render:isMarginalNode($node) or
        (:(render:isListNode($node) and not($node/descendant::*[render:isListNode(.)])):)
        (render:isListNode($node) and (($node/self::tei:list and not($node/descendant::tei:list))
                                       or ($node[(self::tei:item or self::tei:head or self::tei:argument) 
                                                 and not(descendant::tei:list) 
                                                 and following-sibling::tei:item[./tei:list[render:isListNode(.)]]])
                                      )
        (: read as: 'lists that do not contain lists (=lists at the lowest level), or siblings thereof' :)
        (:(($node/self::tei:list and not($node/descendant::tei:list))
                                       or ($node/self::tei:head and following-sibling::tei:item[./tei:list[not($node/descendant::tei:list)]])):) (: head may occur outside of lowest-level lists... :)
        )
    )
};


declare function render:getNodeCategory($node as element()) as xs:string {
    if (render:isMainNode($node)) then 'main'
    else if (render:isMarginalNode($node)) then 'marginal'
    else if (render:isStructuralNode($node)) then 'structural'
    else if (render:isListNode($node)) then 'list'
    else if (render:isPageNode($node)) then 'page'
    else if (render:isAnchorNode($node)) then 'anchor'
    else error()
};



(: ####++++---- HTML helper functions ----++++#### :)

(: debug: :)
declare function render:preparePaginationHTML($work as element(tei:TEI), $lang as xs:string?, $fragmentIds as map()?) as element(ul) {
    let $workId := $work/@xml:id
    return 
        <ul id="later" class="dropdown-menu scrollable-menu" role="menu" aria-labelledby="dropdownMenu1">{
            for $pb in $work//tei:text//tei:pb[render:isIndexNode(.) and not(@sameAs or @corresp)] return
                let $fragment := $fragmentIds($pb/@xml:id/string()) (:$pb/sal:fragment:)
                let $url      := 'work.html?wid=' || $workId || '&amp;frag=' || $fragment || '#' || concat('pageNo_', $pb/@n)
                return 
                    <li role="presentation"><a role="menuitem" tabindex="-1" href="{$url}">{normalize-space($pb/sal:title)}</a></li>
        }</ul>
};


declare function render:mkUrlWhileRendering($targetWorkId as xs:string, $targetNode as node(), $fragmentIds as map()) {
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
declare function render:teaserString($node as element(), $mode as xs:string?) as xs:string {
    let $thisMode := if ($mode = ('orig', 'edit')) then $mode else 'edit'
    let $string := normalize-space(replace(replace(string-join(render:dispatch($node, $thisMode)), '\[.*?\]', ''), '\{.*?\}', ''))
    return 
        if (string-length($string) gt $config:chars_summary) then
            concat('&#34;', normalize-space(substring($string, 1, $config:chars_summary)), '…', '&#34;')
        else
            concat('&#34;', $string, '&#34;')
};

(: ####++++ HTML Helper Functions ++++####:)


(:
~ Recursively creates a TOC list (of lists...) for a sequence of nodes.
:)
declare function render:HTMLgenerateTocFromDiv($nodes as element()*, $wid as xs:string) as element(ul)* {
    for $node in $nodes/(tei:div[@type="work_part"]/tei:div[render:isIndexNode(.)]
                         |tei:div[not(@type="work_part")][render:isIndexNode(.)]
                         |*/tei:milestone[@unit ne 'other'][render:isIndexNode(.)]) return
        let $fragTrail := sal-util:getNodetrail($wid, $node, 'citetrail')        
        let $fragId := $config:idserver || '/texts/' || $wid || ':' || $fragTrail || '?format=html'
        let $section := $node/@xml:id/string()
        let $i18nKey := 
            if (render:dispatch($node, 'class')) then render:dispatch($node, 'class')
            else 'tei-generic'
        let $label := ('[', <i18n:text key="{$i18nKey}"/>, ']')
        let $titleString := render:dispatch($node, 'title')
        let $titleAtt := '[i18n(' || $i18nKey || ')] ' || $titleString
(:        let $titleElems := render:HTMLmakeTOCTitle($node):)
        (: title="{$title}" :)
        return 
            <ul>
                <li>
                    <a class="hideMe" href="{$fragId}" title="{$titleAtt}">
                        {($label, ' ', $titleString)}
                        <span class="jstree-anchor hideMe pull-right">{render:HTMLgetPagesFromDiv($node)}</span>
                    </a>
                    {render:HTMLgenerateTocFromDiv($node, $wid)}
                </li>
            </ul>
};

declare function render:HTMLmakeTOCTitle($node as node()) as item()* {
    let $i18nKey := 
        (: every div or milestone type with a citation label should have an entry in i18n files: :)
        if ($node/self::tei:div) then
            if ($config:citationLabels($node/@type/string())?('full')) then 'tei-div-' || $node/@type 
            else 'tei-generic'
        else if ($node/self::tei:milestone) then
            if ($config:citationLabels($node/@unit/string())?('full')) then 'tei-ms-' || $node/@unit
            else 'tei-generic'
        else ()
    let $divLabel := ('[', <i18n:text key="{$i18nKey}"/>, ']')
    let $titleString := render:dispatch($node, 'title')
    return
        ($divLabel, ' ', $titleString)
};

declare function render:HTMLgetPagesFromDiv($div) {
    let $firstpage :=   
        if ($div[@type='work_volume'] | $div[@type = 'work_monograph']) then ($div//tei:pb[not(@sameAs or @corresp)])[1]/@n/string() 
        else ($div/preceding::tei:pb[not(@sameAs or @corresp)])[last()]/@n/string()
    let $lastpage := if ($div//tei:pb[not(@sameAs or @corresp)]) then ($div//tei:pb[not(@sameAs or @corresp)])[last()]/@n/string() else ()
    return
        if ($firstpage ne '' or $lastpage ne '') then 
            concat(' ', string-join(($firstpage, $lastpage), ' - '))
        else ()
};


(:
~ From a given fragment root, searches for ancestors that occur above the fragmentation level and renders them (non-recursively)
    such that they can be re-included into a fragment's HTML.
:)
(: TODO: currently, this merely creates a "Vol. X" teaser at the beginning of volumes - this means that fragmentation depth cannot go below (front|body|back)/* ! :)
declare function render:excludedAncestorHTML($fragmentRoot as element()) {
    (: determine whether fragment is first structural element of volume :)
    if ($fragmentRoot[ancestor-or-self::tei:text[@type eq 'work_volume'] 
                      and not(preceding::*[self::tei:div or self::tei:titlePage] 
                              intersect ancestor-or-self::tei:text[@type eq 'work_volume']//*[self::tei:div or self::tei:titlePage])]) then
        let $delimiter := 
            if ($fragmentRoot/ancestor-or-self::tei:text[@type='work_volume']/preceding::tei:text[@type='work_volume']) then 
                <hr/> 
            else ()
        let $sumTitle := render:makeHTMLSummaryTitle($fragmentRoot/ancestor-or-self::tei:text[@type eq 'work_volume'])
        return ($delimiter, $sumTitle)
    else ()        
    
    (: functionality for generic ancestor teaser creation - needs debugging; what about ancestor headings (tei:head?) (TODO) :)
    (:if (not($fragmentRoot/preceding-sibling::*)) then
        let $ancestorTeasers :=
            for $a in $fragmentRoot/ancestor::*[render:isCitableWithTeaserHTML(.) and not(preceding-sibling::*
                                                                                          or self::tei:text[@type eq 'work_volume'])] return
                render:makeHTMLSummaryTitle($a) (\: TODO: wrong order? :\)
        let $volTeaser :=
            (\: if fragment is first structural element of volume, also make a "Vol." teaser :\)
            if ($fragmentRoot[ancestor-or-self::tei:text[@type eq 'work_volume'] 
                              and not(preceding::*[self::tei:div or self::tei:titlePage] 
                                      intersect ancestor-or-self::tei:text[@type eq 'work_volume']//*[self::tei:div or self::tei:titlePage])]) then
                let $delimiter := 
                    if ($fragmentRoot/ancestor-or-self::tei:text[@type='work_volume']/preceding::tei:text[@type='work_volume']) then 
                        <hr/> 
                    else ()
                let $sumTitle := render:makeHTMLSummaryTitle($fragmentRoot/ancestor-or-self::tei:text[@type eq 'work_volume'])
                return ($delimiter, $sumTitle)
            else ()
        return ($volTeaser, $ancestorTeasers)
    else ():)
};

(:
~ Creates a section title, which appears to the left of the main area.
:)
declare function render:makeHTMLSummaryTitle($node as element()) as element(div) {
    let $toolbox := render:HTMLSectionToolbox($node)
(:    let $teaser := render:HTMLSectionTeaser($node):)
    let $fullTitle := 
        <span class="section-title-text">{
            if ($node/self::tei:text[@type='work_volume']) then <b>{render:dispatch($node, 'html-title')}</b>
            else render:dispatch($node, 'html-title')
        }</span>
    (: make anchors according to the amount of structural ancestors so that JS knows what to highlight: :)
    let $levels := count($node/ancestor::*[render:isStructuralNode(.)])
    let $levelAnchors := for $l in (1 to $levels) return <a style="display:none;" class="div-l-{$l}"/>
    return
        <div class="section-title container" id="{$node/@xml:id}">
            {$toolbox}
            <div class="section-title-body">{
                if (string-length(string($fullTitle)) gt $render:titleTruncLimit) then
                    let $id := 'collapse-' || $node/@xml:id
                    return
                        <a role="button" class="collapsed title-teaser" data-toggle="collapse" href="{('#' || $id)}" 
                           aria-expanded="false" aria-controls="{$id}">    
                            <p class="collapse" id="{$id}" aria-expanded="false">
                                {$fullTitle}
                            </p>
                        </a>
                else 
                    $fullTitle
            }</div>
            {$levelAnchors}
        </div>
};


(:
~ Renders a marginal element (currently all tei:note as well as label[@place eq 'margin']; head[@place eq 'margin'] are treated as ordinary head)
:)
declare function render:makeMarginalHTML($node as element()) as element(div) {
    let $label := if ($node/@n) then <span class="note-label">{$node/@n || ' '}</span> else ()
    let $content :=
        if ($node/tei:p) then 
            render:passthru($node, 'html')
        else
            <span class="note-paragraph">{render:passthru($node, 'html')}</span>
    (: determine string-length of complete note text, so as to see whether note needs to be truncated: :)
    let $noteLength := 
        string-length((if ($label) then $node/@n || ' ' else ()) || normalize-space(replace(string-join(render:dispatch($node, 'edit'), ''), '\[.*?\]', '')))
    let $toolbox := render:HTMLSectionToolbox($node)
    return
        <div class="marginal container" id="{$node/@xml:id}">
            {$toolbox}
            <div class="marginal-body">{
                if ($noteLength gt $render:noteTruncLimit) then
                    let $id := 'collapse-' || $node/@xml:id
                    return
                        <a role="button" class="collapsed note-teaser" data-toggle="collapse" href="{('#' || $id)}" 
                           aria-expanded="false" aria-controls="{$id}">    
                            <p class="collapse" id="{$id}" aria-expanded="false">
                                {$label}
                                {' '}
                                {$content}
                            </p>
                        </a>
                else 
                    $content
            }</div>
        </div>
};


(:
~ Creates a teaser for a certain type of structural element (i.e., certain div, milestone, list[@type='dict'], and item[parent::list/@type='dict'])
:)
(:declare function render:HTMLSectionTeaser($node as element()) {
    let $identifier := $node/@xml:id/string()
    let $fullTitle := render:dispatch($node, 'html-title')
    return 
        <span class="sal-section-teaser">{
            if (string-length($fullTitle) gt $render:teaserTruncLimit) then
                (<a data-toggle="collapse" data-target="{('#restOfString' || $identifier)}">
                    {substring($fullTitle,1,$render:teaserTruncLimit) || '…'} 
                    <i class="fa fa-angle-double-down"/>
                </a>,
                <span class="collapse" id="{('restOfString' || $identifier)}">
                    {$fullTitle}
                </span>)
            else $fullTitle
            
        }</span>
};:)

(:
~ Creates a toolbox including export and link buttons. Should be placed as preceding sibling of the element that
~ it refers to for JS (highlighting etc.) to work correctly. 
:)
declare function render:HTMLSectionToolbox($node as element()) as element(div) {
    let $id := $node/@xml:id/string()
    let $wid := $node/ancestor::tei:TEI/@xml:id
    let $fileDesc := $node/ancestor::tei:TEI/tei:teiHeader/tei:fileDesc
    let $class := 
        if (render:isMarginalNode($node)) then 
            'sal-toolbox-marginal' 
        else if (render:isCitableWithTeaserHTML($node)) then
            'sal-toolbox-title'
        else 'sal-toolbox'
    let $i18nSuffix := (: Suffix for determining what kind of description to display :) 
        if ($class eq 'sal-toolbox-title' or $node/self::tei:titlePage) then 'Sect' 
        else if ($class eq 'sal-toolbox') then 'Para' 
        else 'Note'
    let $citetrailBaseUrl := render:makeCitetrailURI($node)
    return
        <div class="{$class}">
            <a id="{$id}" href="#" data-rel="popover" class="sal-tb-a"><!-- href="{('#' || $id)}" -->
                <i class="fas fa-hand-point-right messengers" title="{concat('i18n(openToolbox', $i18nSuffix, ')')}"/>
            </a>
            <div class="sal-toolbox-body">
                <div class="sal-tb-btn" title="{concat('i18n(link', $i18nSuffix, ')')}">
                    <button onclick="copyLink(this); return false;" class="messengers">
                        <i class="fas fa-link"/>{' '}<i18n:text key="copyLink"/>
                    </button>
                    <span class="cite-link" style="display:none;">{$citetrailBaseUrl || '?format=html'}</span>
                </div>
                <div class="sal-tb-btn" title="{concat('i18n(cite', $i18nSuffix, ')')}">
                    <button onclick="copyCitRef(this); return false;" class="messengers">
                        <i class="fas fa-feather-alt"/>{' '}<i18n:text key="copyCit"/>
                    </button>
                    <span class="sal-cite-rec" style="display:none">
                        {app:HTMLmakeCitationReference($wid, $fileDesc, 'reading-passage', $node)}
                    </span>
                </div>
                <div class="sal-tb-btn dropdown" title="{concat('i18n(txtExp', $i18nSuffix, ')')}">
                    <button class="dropdown-toggle messengers" data-toggle="dropdown">
                        <i class="fas fa-align-left" title="i18n(txtExpPass)"/>{' '}<i18n:text key="txtExpShort"/>
                    </button>
                    <ul class="dropdown-menu" role="menu">
                        <li><a href="{$citetrailBaseUrl || '?format=txt&amp;mode=edit'}"><i class="messengers fas fa-align-left" title="i18n(downloadTXTEdit)"/>{' '}<i18n:text key="constitutedLower">constituted</i18n:text></a></li>
                        <li><a href="{$citetrailBaseUrl || '?format=txt&amp;mode=orig'}"><i class="messengers fas fa-align-left" title="i18n(downloadTXTOrig)"/>{' '}<i18n:text key="diplomaticLower">diplomatic</i18n:text></a></li>
                    </ul>
                </div>
                <div class="sal-tb-btn" title="{concat('i18n(teiExp', $i18nSuffix, ')')}">
                    <button class="messengers" onclick="window.location.href = '{$citetrailBaseUrl || '?format=tei'}'">
                        <i class="fas fa-file-code" />{' '}<i18n:text key="teiExpShort"/>
                    </button>
                </div>
                <div class="sal-tb-btn" style="display:none;">
                    <a class="updateHiliteBox" href="#"> 
                        <i class="glyphicon glyphicon-refresh"/>
                    </a>
                </div>
            </div>
        </div>
};

declare function render:WRKpreparePagination($node as node()?, $model as map(*)?, $wid as xs:string?, $lang as xs:string?) {
    let $workId :=  
        if ($wid) then 
            if (contains($wid, '_')) then substring-before(sal-util:normalizeId($wid), '_') 
            else sal-util:normalizeId($wid)
        else substring-before($model('currentWorkId'), '_')
    return 
        <ul id="later" class="dropdown-menu scrollable-menu" role="menu" aria-labelledby="dropdownMenu1">{
            for $pb in doc($config:index-root || '/' || $workId || '_nodeIndex.xml')//sal:node[@type='pb'][not(starts-with(sal:title, 'sameAs') or starts-with(sal:title, 'corresp'))]
                let $fragment := $pb/sal:fragment
                let $url      := $config:idserver || '/texts/' || $workId || ':' || $pb/sal:citetrail/text() 
                (:'work.html?wid=' || $workId || '&amp;frag=' || $fragment || '#' || concat('pageNo_', $pb/@n):)
                return 
                    <li role="presentation"><a role="menuitem" tabindex="-1" href="{$url}">{normalize-space($pb/sal:title)}</a></li>
        }</ul>
};


(: ####++++---- OTHER helper functions ----++++#### :)

(:
~ For a node, make a full-blown URI including the citetrail of the node
:)
declare function render:makeCitetrailURI($node as element()) as xs:string? {
    let $citetrail := sal-util:getNodetrail($node/ancestor::tei:TEI/@xml:id, $node, 'citetrail')
    let $workId := $node/ancestor::tei:TEI/@xml:id
    return
        if ($citetrail) then $config:idserver || '/texts/' || $workId || ':' || $citetrail
        else ()
};


declare function render:classableString($str as xs:string) as xs:string? {
    replace($str, '[,: ]', '')
};

(: ####====---- End Helper Functions ----====#### :)




(: ####====---- RENDERING FUNCTIONS ----====#### :)

(: #### HTML Util Functions ####:)

declare function render:createHTMLFragment($workId as xs:string, $fragmentRoot as element(), $fragmentIndex as xs:integer, $prevId as xs:string?, $nextId as xs:string?) as element(div) {
    (:let $serializationParams :=
        <output:serialization-parameters>
            <output:method value="html"/>
            <output:indent value="no"/>
        </output:serialization-parameters>
    
    let $fragment :=:)
        (: SvSalPage: main area (id/class page in order to identify page-able content :)
        <div class="row" xml:space="preserve">
            <div class="col-md-12">
                <div id="SvSalPages">
                    <div class="SvSalPage">                
                        {
                        if ($fragmentRoot[not(preceding-sibling::*) and not((ancestor::body|ancestor::back) and preceding::front/*)]) then
                            render:excludedAncestorHTML($fragmentRoot)
                        else ()    
                        }
                        {render:dispatch($fragmentRoot, 'html')}
                    </div>
                </div>
            </div>
            {render:createPaginationLinks($workId, $fragmentIndex, $prevId, $nextId) (: finally, add pagination links :)}
        </div>
        (: the rest (to the right, in col-md-12) is filled by _spans_ of class marginal, possessing
             a negative right margin (this happens in eXist's work.html template) :)
    (:return 
        serialize($fragment, $serializationParams):)
};

declare function render:makeFragmentId($index as xs:integer, $xmlId as xs:string) as xs:string {
    format-number($index, '0000') || '_' || $xmlId
};

declare function render:createPaginationLinks($workId as xs:string, $fragmentIndex as xs:integer, $prevId as xs:string?, $nextId as xs:string?) {
    let $prevLink :=
        if ($prevId) then
            let $link := 'work.html?wid=' || $workId || '&amp;frag=' || render:makeFragmentId($fragmentIndex - 1, $prevId)
            return
                (<a class="previous" href="{$link}">prev</a>, ' | ')
        else ()
    let $top := <a class="top" href="work.html?wid={$workId}">top</a>
    let $nextLink :=
        if ($nextId) then
            let $link := 'work.html?wid=' || $workId || '&amp;frag=' || render:makeFragmentId($fragmentIndex + 1, $nextId)
            return 
                (' | ', <a class="next" href="{$link}">next</a>)
        else ()
    return
        <div id="SvSalPagination">
            {($prevLink, $top, $nextLink)}
        </div>
};

(:
~ Determines the type of list in which an element (item, list, head, ...) occurs.
:)
declare function render:determineListType($node as element()) as xs:string? {
    if ($node[self::tei:list and @type]) then $node/@type
    else if ($node/ancestor::tei:list[@type]) then $node/ancestor::tei:list[@type][1]/@type
    else () (: fall back to simple? :)
};


declare function render:resolveCanvasID($pb as element(tei:pb)) as xs:string {
    let $facs := normalize-space($pb/@facs/string())
    return
        if (matches($facs, '^facs:W[0-9]{4}-[A-z]-[0-9]{4}$')) then 
            let $index := string(count($pb/preceding::tei:pb[not(@sameAs) and substring(@facs, 1, 12) eq substring($facs, 1, 12)]) + 1)
            return $config:imageserver || '/iiif/presentation/' || sal-util:convertVolumeID(substring($facs,6,7)) || '/canvas/p' || $index
        else if (matches($facs, '^facs:W[0-9]{4}-[0-9]{4}$')) then
            let $index := string(count($pb/preceding::tei:pb[not(@sameAs)]) + 1)
            return $config:imageserver || '/iiif/presentation/' || substring($facs,6,5) || '/canvas/p' || $index
        else error(xs:QName('render:resolveCanvasID'), 'Unknown pb/@facs value')
};


declare function render:resolveFacsURI($facsTargets as xs:string) as xs:string {
    let $facs := (tokenize($facsTargets, ' '))[1]
    let $iiifRenderParams := '/full/full/0/default.jpg'
    let $singleVolRegex := 'facs:(W[0-9]{4})\-([0-9]{4})'
    let $multiVolRegex := 'facs:(W[0-9]{4})\-([A-z])\-([0-9]{4})'
    return
        if (matches($facs, $singleVolRegex)) then (: single-volume work, e.g.: facs:W0017-0005 :)
            let $workId := replace($facs, $singleVolRegex, '$1')
            let $facsId := replace($facs, $singleVolRegex, '$2')
            return 
                $config:imageserver || '/iiif/image/' || $workId || '!' || $workId || '-' || $facsId || $iiifRenderParams
        else if (matches($facs, $multiVolRegex)) then (: volume of a multi-volume work, e.g.: facs:W0013-A-0007 :)
            let $workId := replace($facs, $multiVolRegex, '$1')
            let $volId := replace($facs, $multiVolRegex, '$2')
            let $facsId := replace($facs, $multiVolRegex, '$3')
            return $config:imageserver || '/iiif/image/' || $workId || '!' || $volId || '!' || $workId 
                        || '-' || $volId || '-' || $facsId || $iiifRenderParams
        else error(xs:QName('render:pb'), 'Illegal facs ID (pb/@facs): ' || $facs)
};


(:
~ Transforms a $node into an HTML link anchor (a[@href]), dispatching all its content ((render:dispatch())) and, if required,
    preventing tei:pb from occurring within the link.
:)
declare function render:transformToHTMLLink($node as element(), $uri as xs:string) {
    if (not($node/tei:pb)) then
        <a href="{$uri}" target="_blank">{render:passthru($node, 'html')}</a>
    else
        (: make an anchor for the preceding part, then render the pb, then "continue" the anchor :)
        (: TODO: ATM, this works only if pb occurs at the child level, and only with the first pb :)
        let $before :=
            <a href="{$uri}" target="_blank">
                {for $n in $node/tei:pb[1]/preceding-sibling::node() return render:dispatch($n, 'html')}
            </a>
        let $break := render:dispatch($node/tei:pb[1], 'html')
        let $after :=
            <a href="{$uri}" target="_blank">
                {for $n in $node/tei:pb[1]/following-sibling::node() return render:dispatch($n, 'html')}
            </a>
        return
            ($before, $break, $after)
};

(: TODO: debugging with references to extratextual entities :)
declare function render:resolveURI($node as element(), $targets as xs:string) {
    let $currentWork := $node/ancestor-or-self::tei:TEI
    let $target := (tokenize($targets, ' '))[1]
    let $prefixDef := $currentWork//tei:prefixDef
    let $workScheme := '(work:(W[A-z0-9.:_\-]+))?#(.*)'
    let $facsScheme := 'facs:((W[0-9]+)[A-z0-9.:#_\-]+)'
    let $genericScheme := '(\S+):([A-z0-9.:#_\-]+)'
    return
        if (starts-with($target, '#') and $currentWork//*[@xml:id eq substring($target, 2)]) then
            (: target is some node within the current work :)
            render:makeCitetrailURI($currentWork//*[@xml:id eq substring($target, 2)])
        else if (matches($target, $workScheme)) then
            (: target is something like "work:W...#..." :)
            let $targetWorkId :=
                if (replace($target, $workScheme, '$2')) then (: Target is a link containing a work id :)
                    replace($target, $workScheme, '$2')
                else $currentWork/@xml:id/string() (: Target is just a link to a fragment anchor, so targetWorkId = currentWork :)
            let $anchorId := replace($target, $workScheme, '$3')
            return 
                if ($anchorId) then render:makeCitetrailURI($node) else ()
        else if (matches($target, $facsScheme)) then (: Target is a facs string :)
            (: Target does not contain "#", or is not a "work:..." url: :)
            let $targetWorkId :=
                if (replace($target, $facsScheme, '$2')) then (: extract work id from facs string :)
                    replace($target, $facsScheme, '$2')
                else $currentWork/@xml:id/string()
            let $anchorId := replace($target, $facsScheme, '$1') (: extract facs string :)
            return
                render:makeCitetrailURI($node)
        else if (matches($target, $genericScheme)) then 
            (: Use the general replacement mechanism as defined by the prefixDef in works-general.xml: :)
            let $prefix := replace($target, $genericScheme, '$1')
            let $value := replace($target, $genericScheme, '$2')
            return 
                if ($prefixDef[@ident eq $prefix]) then
                    for $p in $prefixDef[@ident eq $prefix][matches($value, @matchPattern)] return
                        replace($value, $p/@matchPattern, $p/@replacementPattern)
                else replace($target, $genericScheme, '$0') (: regex-group(0) :)
        else $target    
};    


(: ####====---- TEI Node Rendering Typeswitch Functions ----====#### :)

(:  MODES: 
~   - 'orig', 'edit': plain text
~   - 'snippets-orig', 'snippets-edit': plain text for Sphinx snippets
~   - 'title': title of a node (only for nodes that represent sections)
~   - 'passagetrail': passagetrail ID of a node (only for nodes that represent passagetrail sections)
~   - 'citetrail': citetrail ID of a node (only for nodes that are render:isNamedCitetrailNode() - all other are created at index time)
~   - 'class': i18n class of a node, usually to be used by HTML-/RDF-related functionalities for generating verbose labels when displaying section titles 
~   - 'html': HTML snippet for the reading view
~   - 'html-title': a full version of the title, for toggling of teasers in the reading view (often simply falls back to 'title', see above)
:)

(:
~ @param $node : the node to be dispatched
~ @param $mode : the mode for which the function shall generate results
:)
declare function render:dispatch($node as node(), $mode as xs:string) {
    let $rendering :=
        typeswitch($node)
        (: Try to sort the following nodes based (approx.) on frequency of occurences, so fewer checks are needed. :)
            case text()                     return render:textNode($node, $mode)
            case element(tei:g)             return render:g($node, $mode)
            case element(tei:lb)            return render:lb($node, $mode)
            case element(tei:pb)            return render:pb($node, $mode)
            case element(tei:cb)            return render:cb($node, $mode)
    
            case element(tei:head)          return render:head($node, $mode) (: snippets: passthru :)
            case element(tei:p)             return render:p($node, $mode)
            case element(tei:note)          return render:note($node, $mode)
            case element(tei:div)           return render:div($node, $mode)
            case element(tei:milestone)     return render:milestone($node, $mode)
            
            case element(tei:choice)        return render:choice($node, $mode)
            case element(tei:abbr)          return render:abbr($node, $mode)
            case element(tei:orig)          return render:orig($node, $mode)
            case element(tei:sic)           return render:sic($node, $mode)
            case element(tei:expan)         return render:expan($node, $mode)
            case element(tei:reg)           return render:reg($node, $mode)
            case element(tei:corr)          return render:corr($node, $mode)
            
            case element(tei:persName)      return render:persName($node, $mode)
            case element(tei:placeName)     return render:placeName($node, $mode)
            case element(tei:docAuthor)     return render:docAuthor($node, $mode)
            case element(tei:orgName)       return render:orgName($node, $mode)
            case element(tei:pubPlace)      return render:pubPlace($node, $mode)
            case element(tei:publisher)     return render:publisher($node, $mode)
            case element(tei:title)         return render:title($node, $mode)
            case element(tei:term)          return render:term($node, $mode)
            case element(tei:bibl)          return render:bibl($node, $mode)
    
            case element(tei:hi)            return render:hi($node, $mode) 
            case element(tei:emph)          return render:emph($node, $mode)
            case element(tei:ref)           return render:ref($node, $mode) 
            case element(tei:quote)         return render:quote($node, $mode)
            case element(tei:soCalled)      return render:soCalled($node, $mode)
    
            case element(tei:list)          return render:list($node, $mode)
            case element(tei:item)          return render:item($node, $mode)
            case element(tei:gloss)         return render:gloss($node, $mode)
            case element(tei:eg)            return render:eg($node, $mode)
    
            case element(tei:birth)         return render:birth($node, $mode) 
            case element(tei:death)         return render:death($node, $mode)
    
            case element(tei:lg)            return render:lg($node, $mode)
            case element(tei:l)             return render:l($node, $mode)
            
            case element(tei:signed)        return render:signed($node, $mode) 
            
            case element(tei:titlePage)     return render:titlePage($node, $mode)
            case element(tei:titlePart)     return render:titlePart($node, $mode)
            case element(tei:docTitle)      return render:docTitle($node, $mode)
            case element(tei:docDate)       return render:docDate($node, $mode)
            case element(tei:byline)        return render:byline($node, $mode)
            case element(tei:imprimatur)    return render:imprimatur($node, $mode)
            case element(tei:docImprint)    return render:docImprint($node, $mode)
            
            case element(tei:label)         return render:label($node, $mode)
            case element(tei:argument)      return render:argument($node, $mode)
            
            case element(tei:damage)        return render:damage($node, $mode)
            case element(tei:gap)           return render:gap($node, $mode)
            case element(tei:supplied)      return render:supplied($node, $mode)
            case element(tei:unclear)       return render:unclear($node, $mode)
            case element(tei:del)           return render:del($node, $mode)
            case element(tei:space)         return render:space($node, $mode)
            
            case element(tei:figure)        return render:figure($node, $mode)
            
            case element(tei:text)          return render:text($node, $mode) 
            case element(tei:front)         return render:front($node, $mode) 
            case element(tei:body)          return render:body($node, $mode)
            case element(tei:back)          return render:back($node, $mode)
    
            case element(tei:table)         return render:table($node, $mode)
            case element(tei:row)           return render:row($node, $mode)
            case element(tei:cell)          return render:cell($node, $mode)
            
            case element(tei:foreign)       return render:foreign($node, $mode)
            case element(tei:date)          return render:date($node, $mode)
            case element(tei:cit)           return render:cit($node, $mode)
            case element(tei:author)        return render:author($node, $mode)
            case element(tei:docEdition)    return render:docEdition($node, $mode)
            
            case element(tei:TEI)           return render:passthru($node, $mode)
            case element(tei:group)         return render:passthru($node, $mode)
            
            case element(tei:figDesc)       return ()
            case element(tei:teiHeader)     return ()
            case element(tei:fw)            return ()
            case element()                  return error(xs:QName('render:dispatch'), 'Unkown element: ' || local-name($node) || '.')
            case comment()                  return ()
            case processing-instruction()   return ()
    
            default return render:passthru($node, $mode)
    (: for fine-grained debugging: :)
    (:let $debug := 
        if (render:isIndexNode($node)) then 
            util:log('warn', '[RENDER] Processing node tei:' || local-name($node) || ', with @xml:id=' || $node/@xml:id) 
        else ():)
    return
        if ($mode eq 'html') then
            if (render:isCitableWithTeaserHTML($node)) then
                let $citationAnchor := render:makeHTMLSummaryTitle($node)
                return ($citationAnchor, $rendering)
            else if (render:isBasicNode($node)) then 
                (: toolboxes need to be on the sibling axis with the text body they refer to... :)
                if (render:isMarginalNode($node) 
                    or $node/self::tei:head 
                    or $node/self::tei:argument (: no toolboxes for 'heading' elements such as head and argument :)
                    or $node/self::tei:titlePage (: toolbox is produced in render:titlePage :)
                    or $node/self::tei:p[ancestor::tei:list] (: we do not make toolboxes for p within list :)
                    or $node[ancestor::tei:list and ancestor::tei:div[@type eq 'contents']] (: TOC list elements do not need to be citable :)
                    ) then 
                    (: for these elements, $toolboxes are created right in their render: function if required :)
                    $rendering
                else 
                    let $toolbox := render:HTMLSectionToolbox($node)
                    return
                        <div class="hauptText">
                            {$toolbox}
                            <div class="hauptText-body">{$rendering}</div>
                        </div>
            else $rendering
        else 
            $rendering
};


(: ####++++ Element functions (ordered alphabetically) ++++#### :)


declare function render:abbr($node as element(tei:abbr), $mode) {
    switch($mode)
        case 'snippets-orig' return
            render:passthru($node, $mode)
            
        case 'snippets-edit' return
            if (not($node/preceding-sibling::tei:expan|$node/following-sibling::tei:expan)) then
                render:passthru($node, $mode)
            else ()
            
        case 'class' return ()
        
        default return
            render:origElem($node, $mode)
};


declare function render:argument($node as element(tei:argument), $mode as xs:string) {
    switch($mode)
        case 'class' return 
            'tei-' || local-name($node)
        
        case 'citetrail' return 
            error()
            (:if (render:isUnnamedCitetrailNode($node)) then 
                string(render:determineUnnamedCitetrailNodePosition($node))
            else ():)
        
        case 'html' return
            if (render:isBasicNode($node)) then
                <div class="hauptText">
                    <div class="argument">
                        {render:passthru($node, $mode)}
                    </div>
                </div>
            else
                <div class="argument">
                    {render:passthru($node, $mode)}
                </div>
        
        default return
            render:passthru($node, $mode)
};


declare function render:author($node as element(tei:author), $mode as xs:string) {
    switch($mode)
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};


declare function render:back($node as element(tei:back), $mode as xs:string) {
    switch($mode)
        case 'title' return
            ()
        
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citetrail' return
            'backmatter'
            
        case 'passagetrail' return
            $config:citationLabels(local-name($node))?('abbr')
        
        default return
            render:passthru($node, $mode)
};

declare function render:bibl($node as element(tei:bibl), $mode as xs:string) {
    switch($mode)
        case 'orig'
        case 'snippets-orig' return
            render:passthru($node, $mode)
            
        case 'edit' return
            if ($node/@sortKey) then
                (render:passthru($node, $mode), ' [', replace(string($node/@sortKey), '_', ', '), ']')
            else
                render:passthru($node, $mode)
        
        case 'snippets-edit' return
            if ($node/@sortKey) then
                replace(string($node/@sortKey), '_', ', ')
            else
                render:passthru($node, $mode)
        
        case 'html' return
            if ($node/@sortKey) then 
                <span class="{local-name($node) || ' hi_', render:classableString($node/@sortKey)}">{render:passthru($node, $mode)}</span>
            else <span>{render:passthru($node, $mode)}</span>
        
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};


declare function render:birth($node as element(tei:birth), $mode as xs:string) {
    if ($mode = ("orig", "edit")) then
        render:passthru($node, $mode)
    else if ($mode = ('snippets-edit', 'snippets-orig')) then
        render:passthru($node, $mode)
    else ()
};


declare function render:body($node as element(tei:body), $mode as xs:string) {
    switch($mode)
        case 'class' return
            'tei-' || local-name($node)
        
        default return
            render:passthru($node, $mode) 
};

declare function render:byline($node as element(tei:byline), $mode as xs:string) {
    switch($mode)
        case 'html' return
            <span class="tp-paragraph">
                {render:passthru($node, $mode)}
            </span>
        
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};

declare function render:cb($node as element(tei:cb), $mode as xs:string) {
    switch($mode)
        case 'orig' 
        case 'edit'
        case 'snippets-orig'
        case 'snippets-edit' 
        case 'html' return
            if (not($node/@break = 'no')) then
                ' '
            else ()
        
        case 'class' return ()
        
        default return () (: some sophisticated function to insert a pipe and a pagenumber div in the margin :)
};

declare function render:cell($node as element(tei:cell), $mode) {
    switch($mode)
        case 'html' return 
            if ($node/@role eq 'label') then 
                <td class="table-label">{render:passthru($node, $mode)}</td>
            else <td>{render:passthru($node, $mode)}</td>
        
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};


declare function render:choice($node as element(tei:choice), $mode as xs:string) {
    switch($mode)
        case 'html' return
            (: HTML: Editorial interventions: Don't hide original stuff where we have no modern alternative, otherwise
             put it in an "orignal" class span which we make invisible by default.
             Put our own edits in spans of class "edited" and add another class to indicate what type of edit has happened :)
            render:passthru($node, $mode)
        
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};


declare function render:cit($node as element(tei:cit), $mode as xs:string) {
    switch($mode)
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};


declare function render:corr($node as element(tei:corr), $mode) {
    switch($mode)
        case 'snippets-orig' return 
            ()
            
        case 'snippets-edit' return
            render:passthru($node, $mode)
        
        default return
            render:editElem($node, $mode)
};


declare function render:damage($node as element(tei:damage), $mode as xs:string) {
    switch($mode)
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};


declare function render:date($node as element(tei:date), $mode as xs:string) {
    switch($mode)
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};


declare function render:death($node as element(tei:death), $mode as xs:string) {
    if ($mode = ("orig", "edit")) then
        render:passthru($node, $mode)
    else if ($mode = ('snippets-edit', 'snippets-orig')) then
        render:passthru($node, $mode)
    else ()
};


declare function render:del($node as element(tei:del), $mode as xs:string) {
    switch($mode)
        case 'html' return
            if ($node/tei:supplied) then
                render:passthru($node, $mode)
            else error(xs:QName('render:del'), 'Unexpected content in tei:del')
        
        default return 
            render:passthru($node, $mode)
};


declare function render:div($node as element(tei:div), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                    '"' || string($node/@n) || '"'
                else if ($node/(tei:head|tei:label)) then
                    render:teaserString(($node/(tei:head|tei:label))[1], 'edit')
                (: purely numeric section titles: :)
                else if ($node/@n and (matches($node/@n, '^[0-9\[\]]+$')) and ($node/@type)) then
                    string($node/@n)
                (: otherwise, try to derive a title from potential references to the current node :)
                else if ($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)]) then
                    render:teaserString($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)][1], 'edit')
                (: if there is a list/head and nothing else works, we may use that :)
                else if ($node/tei:list/(tei:head|tei:label)) then
                    render:teaserString(($node/tei:list/(tei:head|tei:label))[1], 'edit')
                else ()
            )
        
        case 'html-title' return
            if (not($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) and $node/(tei:head|tei:label)) then
                (: for expanded titles, we need the full version, not just the teaser :)
                normalize-space(replace(string-join(render:dispatch(($node/(tei:head|tei:label))[1], 'edit'), ''), '\[.*?\]', ''))
            else if (render:div($node, 'title')) then replace(render:div($node, 'title'), '"', '')
            else <i18n:text key="{render:div($node, 'class')}"></i18n:text> (: if everything fails, simply use the label (such as 'Preface') :)
        
        case 'html' return
            render:passthru($node, $mode)
        
        case 'class' return
            'tei-div-' || $node/@type
        
        case 'citetrail' return
            if (render:isNamedCitetrailNode($node)) then
                (: use abbreviated form of @type (without dot), possibly followed by position :)
                (: TODO: div label experiment (delete the following block if this isn't deemed plausible) :)
                let $abbr := $config:citationLabels($node/@type)?('abbr')
                let $prefix :=
                    if ($abbr) then 
                        lower-case(if (contains($abbr, '.')) then substring-before($config:citationLabels($node/@type)?('abbr'), '.') else $abbr)
                    else 'div' (: divs for which we haven't defined an abbr. :)
                let $position :=
                    if (count($node/parent::*[self::tei:body or render:isIndexNode(.)]/tei:div[$config:citationLabels(@type)?('abbr') eq $config:citationLabels($node/@type)?('abbr')]) gt 1) then
                        string(count($node/preceding-sibling::tei:div[$config:citationLabels(@type)?('abbr') eq $config:citationLabels($node/@type)?('abbr')]) + 1)
                    else ()
                return $prefix || $position
            (:else if (render:isUnnamedCitetrailNode($node)) then 
                string(render:determineUnnamedCitetrailNodePosition($node))
            else ():)
            else error()
        
        case 'passagetrail' return
            if (render:isPassagetrailNode($node)) then
                let $prefix := lower-case($config:citationLabels($node/@type)?('abbr')) (: TODO: upper-casing with first element of passagetrail ? :)
                return 
                    if ($node/@type = ('lecture', 'gloss')) then (: TODO: 'lemma'? :)
                        (: special cases: with these types, we provide a short teaser string instead of a numeric value :)
                        let $teaser := '"' || normalize-space(substring(substring-after(render:div($node, 'title'), '"'),1,15)) || '…"'
                        return $prefix || ' ' || $teaser
                    else
                        let $position := 
                            if ($node/@n[matches(., '^[0-9\[\]]+$')]) then $node/@n (:replace($node/@n, '[\[\]]', '') ? :)
                            else if ($node/ancestor::*[render:isPassagetrailNode(.)]) then
                                (: using the none-copy version here for sparing memory: :)
                                if (count($node/ancestor::*[render:isPassagetrailNode(.)][1]//tei:div[@type eq $node/@type and render:isPassagetrailNode(.)]) gt 1) then 
                                    string(count($node/ancestor::*[render:isPassagetrailNode(.)][1]//tei:div[@type eq $node/@type and render:isPassagetrailNode(.)]
                                                 intersect $node/preceding::tei:div[@type eq $node/@type and render:isPassagetrailNode(.)]) + 1)
                                else ()
                            else if (count($node/parent::*/tei:div[@type eq $node/@type]) gt 1) then 
                                string(count($node/preceding-sibling::tei:div[@type eq $node/@type]) + 1)
                            else ()
                        return
                            $prefix || (if ($position) then ' ' || $position else ())
            else ()
        
        case 'orig' return
             ($config:nl, render:passthru($node, $mode), $config:nl)
        
        case 'edit' return
            if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                (concat($config:nl, '[ *', string($node/@n), '* ]'), $config:nl, render:passthru($node, $mode), $config:nl)
                (: oder das hier?:   <xsl:value-of select="key('targeting-refs', concat('#',@xml:id))[1]"/> :)
            else
                ($config:nl, render:passthru($node, $mode), $config:nl)
        
        case 'snippets-orig' return 
            render:passthru($node, $mode)
            
        case 'snippets-edit' return
            if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                concat(' ', string($node/@n), ' ', render:passthru($node, $mode))
                (: or this?:   <xsl:value-of select="key('targeting-refs', concat('#',@xml:id))[1]"/> :)
            else render:passthru($node, $mode)
        
        default return
            render:passthru($node, $mode)
};

declare function render:docAuthor($node as element(tei:docAuthor), $mode as xs:string) {
    switch($mode)
        case 'html' return
            render:name($node, $mode)
        default return 
            render:passthru($node, $mode)
};

declare function render:docDate($node as element(tei:docDate), $mode as xs:string) {
    switch($mode)
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};

declare function render:docEdition($node as element(tei:docEdition), $mode as xs:string) {
    switch($mode)
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};

declare function render:docImprint($node as element(tei:docImprint), $mode as xs:string) {
    switch($mode)
        case 'html' return
            <span class="tp-paragraph">
                {render:passthru($node, $mode)}
            </span>
        default return
            render:passthru($node, $mode)
};

declare function render:docTitle($node as element(tei:docTitle), $mode as xs:string) {
    switch($mode)
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};

declare function render:editElem($node as element(), $mode as xs:string) {
    switch($mode)
        case "orig" return ()
        case "edit" return
            render:passthru($node, $mode)
            
        case 'html' return
            if ($node/parent::tei:choice) then
                let $origString := string-join(render:dispatch($node/parent::tei:choice/(tei:abbr|tei:orig|tei:sic), 'orig'), '')
                return
                    <span class="messengers edited {local-name($node)}" title="{$origString}">
                        {string-join(render:passthru($node, $mode), '')}
                    </span>
            else render:passthru($node, $mode)
        default return
            render:passthru($node, $mode)
};

declare function render:eg($node as element(tei:eg), $mode as xs:string) {
    if ($mode = ("orig", "edit")) then
        render:passthru($node, $mode)
    else 
        render:passthru($node, $mode)
};


declare function render:emph($node as element(tei:emph), $mode as xs:string) {
    if ($mode = ("orig", "edit")) then
        render:passthru($node, $mode)
    
    else
        render:passthru($node, $mode)
};


declare function render:expan($node as element(tei:expan), $mode) {
    switch($mode)
        case 'snippets-orig' return 
            ()
        case 'snippets-edit' return
            render:passthru($node, $mode)
        default return
            render:editElem($node, $mode)
};


declare function render:figure($node as element(tei:figure), $mode as xs:string) {
    switch($mode)
        case 'html' return
            if ($node/@type eq 'ornament') then
                <hr class="ornament"/>
            else ()
            
        default return ()
};


declare function render:foreign($node as element(tei:foreign), $mode as xs:string) {
    switch($mode)
        case 'html' return
            <span class="foreign-lang">{render:passthru($node, $mode)}</span>
            
        default return 
            render:passthru($node, $mode)
};


declare function render:front($node as element(tei:front), $mode as xs:string) {
    switch ($mode)
        case 'title' return
            ()
            
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citetrail' return
            'frontmatter'
            
        case 'passagetrail' return
            $config:citationLabels(local-name($node))?('abbr')
            
        default return
            render:passthru($node, $mode)
};


declare function render:g($node as element(tei:g), $mode as xs:string) {
    switch($mode)
        case 'orig'
        case 'snippets-orig' return
            let $glyph := $node/ancestor::tei:TEI/tei:teiHeader/tei:encodingDesc/tei:charDecl/tei:char[@xml:id = substring(string($node/@ref), 2)] (: remove leading '#' :)
            return if ($glyph/tei:mapping[@type = 'precomposed']) then
                    string($glyph/tei:mapping[@type = 'precomposed'])
                else if ($glyph/tei:mapping[@type = 'composed']) then
                    string($glyph/tei:mapping[@type = 'composed'])
                else if ($glyph/tei:mapping[@type = 'standardized']) then
                    string($glyph/tei:mapping[@type = 'standardized'])
                else
                    render:passthru($node, $mode)
        
        case 'edit' 
        case 'snippets-edit' return
            if (substring($node/@ref,2) = ('char017f', 'char0292')) then
                let $char := $node/ancestor::tei:TEI/tei:teiHeader/tei:encodingDesc/tei:charDecl/tei:char[@xml:id = substring(string($node/@ref), 2)]
                return
                    if ($node/text() = ($char/tei:mapping[@type = 'composed']/text(),$char/tei:mapping[@type = 'precomposed']/text())) then
                        $char/tei:mapping[@type = 'standardized']/text()
                    else render:passthru($node, $mode)
            else if (render:passthru($node, $mode)) then
                render:passthru($node, $mode)
            else
                error(xs:QName('render:g'), 'Found tei:g without text content')
        
        case 'html' return
            let $thisString := 
                if ($node/text()) then 
                    xs:string($node/text())
                else error(xs:QName('render:g'), 'Found tei:g without text content') (: ensure correct character markup :)
            let $charCode := substring($node/@ref,2)
            let $char := $node/ancestor::tei:TEI/tei:teiHeader/tei:encodingDesc/tei:charDecl/tei:char[@xml:id eq $charCode]
            let $test := (: make sure that the char reference is correct :)
                if (not($char)) then 
                    error(xs:QName('render:g'), 'g/@ref is invalid, the char code does not exist): ', $charCode)
                else ()
            let $precomposedString := 
                if ($char/tei:mapping[@type='precomposed']/text()) then 
                    string($char/tei:mapping[@type='precomposed']/text())
                else ()
            let $composedString := 
                if ($char/tei:mapping[@type='composed']/text()) then
                    string($char/tei:mapping[@type='composed']/text())
                else ()
            let $originalGlyph := if ($composedString) then $composedString else $precomposedString
                (: composed strings are preferable since some precomposed chars are displayed oddly in certain contexts 
                    (e.g. chare0303 in bold headings) :)
            return 
                (: Depending on the context or content of the g element, there are several possible cases: :)
                (: 1. if g occurs within choice, we can simply take an original character since any expansion should be handled through the choice mechanism :)
                if ($node/ancestor::tei:choice) then
                    $originalGlyph
                (: 2. g occurs outside of choice: :)
                else
                    let $test := 
                        if (string-length($originalGlyph) eq 0) then 
                            error(xs:QName('render:g'), 'No correct mapping available for char: ', $node/@ref)
                        else ()
                    return
                        (: a) g has been used for resolving abbreviations (in early texts W0004, W0013 and W0015) -> treat it like choice elements :)
                        if (not($thisString = ($precomposedString, $composedString)) and not($charCode = ('char017f', 'char0292'))) then
                            (<span class="original glyph unsichtbar" title="{$thisString}">{$originalGlyph}</span>,
                            <span class="edited glyph" title="{$originalGlyph}">{$thisString}</span>)
                        (: b) most common case: g simply marks a special character -> pass it through (except for the very frequent "long s" and "long z", 
                                which are to be normalized :)
                        else if ($charCode = ('char017f', 'char0292')) then
                            (: long s and z shall be switchable in constituted mode to their standardized versions, but due to their high frequency 
                            we refrain from colourful highlighting (.simple-char). In case colour highlighting is desirable, simply remove .simple-char :)
                            let $standardizedGlyph := string($char/tei:mapping[@type='standardized']/text())
                            return 
                                (<span class="original glyph unsichtbar simple-char" title="{$standardizedGlyph}">{$originalGlyph}</span>,
                                <span class="edited glyph simple-char" title="{$originalGlyph}">{$standardizedGlyph}</span>)
                        else 
                            (: all other simple characters :)
                            render:passthru($node, $mode)
                        
        default return
            render:passthru($node, $mode)
};


declare function render:gap($node as element(tei:gap), $mode as xs:string) {
    switch($mode)
        case 'html' return
            if ($node/ancestor::tei:damage) then
                <span title="?" class="gap"/>
            else ()
        default return ()
};


declare function render:gloss($node as element(tei:gloss), $mode as xs:string) {
    switch($mode)
        case 'class' return ()
        
        default return
            render:passthru($node, $mode)
};

(: FIXME: In the following, the #anchor does not take account of html partitioning of works. Change this to use semantic section id's. :)
declare function render:head($node as element(tei:head), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                render:teaserString($node, 'edit')
            )
        case 'html-title' return
            normalize-space(replace(string-join(render:dispatch($node, 'edit')), '\[.*?\]', ''))
        
        case 'html' return
            (: list[not(@type eq 'dict')]/head are handled in render:list() :)
            if ($node/parent::tei:list[not(@type eq 'dict')]) then 
                () 
            (: within notes: :)
            else if ($node/parent::tei:lg) then 
                <h5 class="poem-head">{render:passthru($node, $mode)}</h5>
            (: usual headings: :)
            else 
(:                let $toolbox := render:HTMLSectionToolbox($node)
                return:)
                <h3>
                    <span class="heading-text">{render:passthru($node, $mode)}</span>
                </h3>
            
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citetrail' return
            'heading' ||
            (if (count($node/parent::*/tei:head) gt 1) then          
                (: we have several headings on this level of the document ... :)
                string(count($node/preceding-sibling::tei:head) + 1)
             else ())
        
        case 'orig'
        case 'edit' return
            (render:passthru($node, $mode), $config:nl)
        
        default return 
            render:passthru($node, $mode)
};

declare function render:hi($node as element(tei:hi), $mode as xs:string) {
    switch($mode)
        case 'orig'
        case 'edit' return
            render:passthru($node, $mode)
            
        case 'html' return
            let $styles := distinct-values(tokenize($node/@rendition, ' '))
            (: names of elements that have their own, specific text alignment 
                (where hi/@rendition alignment is to be omitted) :)
            let $specificAlignElems := ('head', 'signed', 'titlePage', 'argument') (: TODO: add more element names here when necessary :)
            let $cssStyles := 
                for $s in $styles return
                    if ($s eq '#b') then 'font-weight:bold;'
                    else if ($s eq '#it') then 'font-style:italic;'
                    else if ($s eq '#rt') then 'font-style: normal;'
                    else if ($s eq '#l-indent') then 'display:block;margin-left:4em;'
                    (: centering and right-alignment apply only in certain contexts :)
                    else if ($s eq '#r-center'
                             and not($node/ancestor::*[local-name(.) = $specificAlignElems])
                             and not($node/ancestor::*[local-name(.) = $render:basicElemNames][1]//text()[not(ancestor::tei:hi[contains(@rendition, '#r-center')])])
                         ) then
                             (: workaround for suppressing trailing centerings at the end of paragraphs :)
                         'display:block;text-align:center;'
                    else if ($s eq '#right' 
                             and not($node/ancestor::*[local-name(.) = $specificAlignElems])
                             and not($node/ancestor::tei:item)) then 
                        'display:block;text-align: right;'
                    else if ($s eq '#sc') then 'font-variant:small-caps;'
                    else if ($s eq '#spc') then 'letter-spacing:2px;'
                    else if ($s eq '#sub') then 'vertical-align:sub;font-size:.83em;'
                    else if ($s eq '#sup') then 'vertical-align:super;font-size: .83em;'
                    else ()
            let $classnames := if ('#initCaps' = $styles) then 'initialCaps' else ()
            return
                element {'span'} {
                    if (string-join($cssStyles, ' ')) then attribute {'style'} {string-join($cssStyles, ' ')} else (),
                    if (string-join($classnames, ' ')) then attribute {'class'} {string-join($classnames, ' ')} else (),
                    render:passthru($node, $mode)
                }
            
        default return 
            render:passthru($node, $mode)
};

declare function render:imprimatur($node as element(tei:imprimatur), $mode as xs:string) {
    switch($mode)
        case 'html' return
            <span class="tp-paragraph">
                {render:passthru($node, $mode)}
            </span>
        default return
            render:passthru($node, $mode)
};

declare function render:item($node as element(tei:item), $mode as xs:string) {
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
                    render:teaserString(($node/(tei:head|tei:label))[1], 'edit')
                (: purely numeric section titles: :)
                else if ($node/@n and (matches($node/@n, '^[0-9\[\]]+$'))) then
                    $node/@n/string()
                (: otherwise, try to derive a title from potential references to the current node :)
                else if ($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)]) then
                    render:teaserString($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)][1], 'edit')
                else ()
            )
        
        case 'html-title' return
            if (not($node/parent::tei:list/@type='dict' and $node//tei:term[1][@key])
                and not($node/@n and not(matches($node/@n, '^[0-9\[\]]+$')))
                and $node/(tei:head|tei:label)) 
                then normalize-space(replace(string-join(render:dispatch(($node/(tei:head|tei:label))[1], 'edit'), ''),'\[.*?\]', ''))
            else replace(render:dispatch($node, 'title'), '"', '')
                
        case 'html' return
            (: tei:item should be handled exclusively in render:list :)
            error()
                
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citetrail' return
            (: "entryX" where X is the section title (render:item($node, 'title')) in capitals, use only for items in indexes and dictionary :)
            if(render:isNamedCitetrailNode($node)) then
                let $title := upper-case(replace(render:item($node, 'title'), '[^a-zA-Z0-9]', ''))
                let $position :=
                    if ($title) then
                        let $siblings := $node/parent::tei:list/tei:item[upper-case(replace(render:item(., 'title'), '[^a-zA-Z0-9]', '')) eq $title]
                        return
                            if (count($siblings) gt 0) then 
                                string(count($node/preceding-sibling::tei:item intersect $siblings) + 1)
                            else ()
                    else if (count($node/parent::tei:list/tei:item) gt 0) then 
                        string(count($node/preceding-sibling::tei:item) + 1)
                    else ()
                return 'entry' || $title || $position
            else error() (:string(render:determineUnnamedCitetrailNodePosition($node)):)
        
        case 'passagetrail' return
            ()
        
        case 'orig'
        case 'edit' return
            let $leader :=  if ($node/parent::tei:list/@type = "numbered") then
                                '#' || $config:nbsp
                            else if ($node/parent::tei:list/@type = "simple") then
                                $config:nbsp
                            else
                                '-' || $config:nbsp
            return ($leader, render:passthru($node, $mode), $config:nl)
       
        default return
            render:passthru($node, $mode)
};


declare function render:l($node as element(tei:l), $mode as xs:string) {
    switch($mode)
        case 'html' return
            (render:passthru($node, $mode),<br/>)
        
        default return render:passthru($node, $mode)
};


declare function render:label($node as element(tei:label), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                render:teaserString($node, 'edit')
            )
            
        case 'html-title' return
            normalize-space(replace(string-join(render:dispatch($node, 'edit')), '\[.*?\]', ''))
            
        case 'html' return
            switch($node/@place)
                case 'margin' return
                    render:makeMarginalHTML($node)
                case 'inline' return
                    <span class="label-inline">
                        {render:passthru($node, $mode)}
                    </span>
                default return render:passthru($node, $mode)
            
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citetrail' return
            if (render:isNamedCitetrailNode($node)) then
                render:makeMarginalCitetrail($node)
            else error()
            (:if (render:isUnnamedCitetrailNode($node)) then 
                string(render:determineUnnamedCitetrailNodePosition($node))
            else render:makeMarginalCitetrail($node):)
            
        case 'edit'
        case 'orig' return
            if ($node/@place eq 'margin') then (: or render:isMarginalNode($node) :)
                (:($config:nl, '        {', render:passthru($node, $mode), '}', $config:nl):)
                ('{', $config:nl, '        ', render:passthru($node, $mode), '        ', $config:nl, '}') 
            else render:passthru($node, $mode) (: TODO: more fine-grained processing? (simple vs. important/heading-like labels) :)
        
        default return
            render:passthru($node, $mode)
};


declare function render:lb($node as element(tei:lb), $mode as xs:string) {
    switch($mode)
        case 'orig'
        case 'edit'
        case 'snippets-orig'
        case 'snippets-edit' 
        case 'html' return
            if (not($node/@break = 'no')) then
                ' '
            else ()
        
        default return () 
};

declare function render:list($node as element(tei:list), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                    '"' || string($node/@n) || '"'
                else if ($node/(tei:head|tei:label)) then
                    render:teaserString(($node/(tei:head|tei:label))[1], 'edit')
                (: purely numeric section titles: :)
                else if ($node/@n and (matches($node/@n, '^[0-9\[\]]+$')) and ($node/@type)) then
                    $node/@n/string()
                (: otherwise, try to derive a title from potential references to the current node :)
                else if ($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)]) then
                    render:teaserString($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)][1], 'edit')
                else ()
            )
        
        case 'html-title' return
            if (not($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) and $node/(tei:head|tei:label)) then
                normalize-space(replace(string-join(render:dispatch(($node/(tei:head|tei:label))[1], 'edit')), '\[.*?\]', ''))
            else replace(render:dispatch($node, 'title'), '"', '')
        
        case 'html' return
            (: available list types: "dict", "ordered", "simple", "bulleted", "gloss", "index", or "summaries" :)
            (: In html, lists must contain nothing but <li>s, so we have to move headings (and arguments) before the list 
               and nest everything else (sub-lists) in <li>s. :)
            switch(render:determineListType($node))
                (: tei:item are actually handled here, not in render:item, due to the tight coupling of their layout to tei:list :)
                case 'ordered' return (: enumerated/ordered list :)
                    <div id="{$node/@xml:id}">
                        {for $head in $node/tei:head return <h4>{render:passthru($head, $mode)}</h4>}
                        {
                        (: in ordered lists, we outsource non-item elements before items (such as argument, p, ...) to a non-ordered, non-bulleted list :)
                        if ($node/*[not(self::tei:head or self::tei:item) and not(preceding-sibling::tei:item)]) then
                            <ul style="list-style: none;">
                                {
                                for $child in $node/*[not(self::tei:head or self::tei:item) and not(preceding-sibling::tei:item)] return
                                    <li>{render:passthru($child, $mode)}</li>
                                }    
                            </ul>
                        else ()
                        }
                        <ol>
                            {for $child in $node/*[self::tei:item or preceding-sibling::tei:item] return 
                                <li>{render:passthru($child, $mode)}</li>
                            }
                        </ol>
                    </div>
                case 'simple' return (: make no list in html terms at all :)
                    <div id="{$node/@xml:id}">
                        {for $head in $node/tei:head return <h4 class="inlist-head">{render:passthru($head, $mode)}</h4>}
                        {for $child in $node/*[not(self::tei:head)] return
                            if ($child//list) then render:passthru($child, $mode)
                            else if (not($child/self::tei:item)) then (: argument, p, etc. :)
                                <div>{render:passthru($child, $mode)}</div>
                            else (' ', <span class="inline-item">{render:passthru($child, $mode)}</span>, ' ')}
                    </div>
                case 'index'
                case 'summaries' return (: unordered list :)
                    let $content := 
                        <div class="list-index" id="{$node/@xml:id}">
                            {for $head in $node/tei:head return <h4 class="list-index-head">{render:passthru($head, $mode)}</h4>}
                            <ul style="list-style-type:circle;">
                                {for $child in $node/*[not(self::tei:head)] return 
                                    if (not($child/self::tei:item)) then (: argument, p, etc. :)
                                        <li class="list-paragraph">{render:passthru($child, $mode)}</li>
                                    else
                                        <li class="list-index-item">{render:passthru($child, $mode)}</li>}
                            </ul>
                        </div>
                    return
                        (:if (not($node/ancestor::tei:list)) then
                            <section>{$content}</section>
                        else :)
                        $content
                default return (: e.g., 'bulleted' :)
                    (: put an unordered list (and captions) in a figure environment (why?) of class @type :)
                    <div class="list-default" id="{$node/@xml:id}">
                        {for $head in $node/tei:head return <h4 class="list-default-head">{render:passthru($head, $mode)}</h4>}
                        <ul style="list-style-type:circle;">
                             {for $child in $node/*[not(self::tei:head)] return 
                                  if (not($child/self::tei:item)) then (: argument, p, etc. :)
                                      <li class="list-paragraph">{render:passthru($child, $mode)}</li>
                                  else
                                      <li class="list-default-item">{render:passthru($child, $mode)}</li>}
                        </ul>
                    </div>
        
        case 'class' return
            'tei-' || local-name($node)
            
        case 'passagetrail' return
            ()
        
        case 'citetrail' return
            (: dictionaries, indices and summaries get their type prepended to their number :)
            if(render:isNamedCitetrailNode($node)) then
                let $currentSection := sal-util:copy($node/(ancestor::tei:div|ancestor::tei:body|ancestor::tei:front|ancestor::tei:back)[last()])
                let $currentNode := $currentSection//tei:list[@xml:id eq $node/@xml:id]
                return
                  concat(
                      $currentNode/@type, 
                      string(
                          count($currentNode/preceding::tei:list[@type eq $currentNode/@type]
                                intersect $currentSection//tei:list[@type eq $currentNode/@type]
                          ) + 1)
                  )
            (: other types of lists are simply counted :)
            (:else if (render:isUnnamedCitetrailNode($node)) then 
                string(render:determineUnnamedCitetrailNodePosition($node))
            else ():)
            else error()
                
        case 'orig' return
            ($config:nl, render:passthru($node, $mode), $config:nl)
        
        case 'edit' return
            if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                (concat($config:nl, ' [*', string($node/@n), '*]', $config:nl), render:passthru($node, $mode), $config:nl)
                (: or this?:   <xsl:value-of select="key('targeting-refs', concat('#',@xml:id))[1]"/> :)
            else
                ($config:nl, render:passthru($node, $mode), $config:nl)
        
        case 'snippets-edit'
        case 'snippets-orig' return
            render:passthru($node, $mode)
        
        default return
            ($config:nl, render:passthru($node, $mode), $config:nl)
};


declare function render:lg($node as element(tei:lg), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                render:teaserString($node, 'edit')
            )
            
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citetrail' return
            (:if (render:isUnnamedCitetrailNode($node)) then 
                string(render:determineUnnamedCitetrailNodePosition($node))
            else ():)
            error()
        
        case 'html' return
            <span class="poem">{render:passthru($node, $mode)}</span>
            
        default return
            render:passthru($node, $mode)
};

declare function render:milestone($node as element(tei:milestone), $mode as xs:string) {
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
                    render:teaserString($node/ancestor::tei:TEI//tei:ref[@target = concat('#', $node/@xml:id)][1], 'edit')
                else ()
            )
            (: TODO: bring i18n labels somehow into html-title... :)
        case 'html-title' return
            replace(render:milestone($node, 'title'), '"', '')
            
        case 'html' return
            let $inlineText := if ($node/@rendition eq '#dagger') then <sup>†</sup> else '*'
            return
                $inlineText
        
        case 'class' return
            'tei-ms-' || $node/@unit
            
        case 'citetrail' return
            (: "XY" where X is the unit and Y is the anchor or the number of milestones where this occurs :)
            let $currentSection := sal-util:copy(render:getCitableParent($node))
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
        
        case 'passagetrail' return
            if (render:isPassagetrailNode($node)) then
                (: TODO: ATM milestone/@unit = ('article', 'section') resolves to the same abbrs as div/@type = ('article', 'section') :)
                (: TODO: if @n is numeric, always resolve to 'num.' ? :)
                let $prefix := lower-case($config:citationLabels($node/@unit)?('abbr'))
                let $num := 
                    if ($node/@n[matches(., '^[0-9\[\]]+$')]) then $node/@n (:replace($node/@n, '[\[\]]', '') ? :)
                    else 
                        let $currentSection := sal-util:copy($node/ancestor::*[render:isPassagetrailNode(.) and not(self::tei:p)][1])
                        let $currentNode := $currentSection//tei:milestone[@xml:id eq $node/@xml:id]
                        let $position := count($currentSection//tei:milestone[@unit eq $currentNode/@unit and render:isPassagetrailNode(.)]
                                               intersect $currentNode/preceding::tei:milestone[@unit eq $currentNode/@unit and render:isPassagetrailNode(.)]) + 1
                        return string($position)
                return
                    $prefix || ' ' || $num
            else ()
        
        case 'orig' return
            if ($node/@rendition = '#dagger') then '†'
            else if ($node/@rendition = '#asterisk') then '*'
            else '[*]'
        
        case 'edit' return
            if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                concat('[', string($node/@n), ']')
            else if ($node/@n and matches($node/@n, '^[0-9\[\]]+$')) then
                concat('[',  $config:citationLabels($node/@unit)?('abbr'), ' ', string($node/@n), ']')
                (: TODO: remove normalization parentheses '[', ']' here (and elsewhere?) :)
            else '[*]'
            
        default return () (: also for snippets-orig, snippets-edit :)
};

declare function render:name($node as element(*), $mode as xs:string) {
    switch($mode)
        case 'orig' return
            render:passthru($node, $mode)
        
        case 'edit' return
            if ($node/(@key|@ref)) then
                (render:passthru($node, $mode), ' [', string-join(($node/@key, $node/@ref), '/'), ']')
            else
                render:passthru($node, $mode)
        
        case 'html' return
            let $hiliteName := if ($node/@ref) then 'hi_' || render:classableString((tokenize($node/@ref, ' '))[1]) else ()
            let $dictLemma := 
                if ($node[self::tei:term and ancestor::tei:list[@type='dict'] and not(preceding-sibling::tei:term)]) then
                    'dictLemma'
                else ()
            return 
                (: as long as any link would lead nowhere, omit linking and simply grasp the content: :)
                <span class="{normalize-space(string-join((local-name($node),$hiliteName,$dictLemma), ' '))}">
                    {render:passthru($node, $mode)}
                </span>
                (: as soon as links have actual targets, execute something like the following: :)
                (:let $resolvedURI := render:resolveURI($node, @ref)
                return
                    if ($node/@ref and substring($resolvedURI,1,5) = ('http:', '/exis')) then
                        render:transformToHTMLLink($node, $resolvedURI)
                    else 
                        {render:passthru($node, $mode)}:)
                (: 
                <xsl:choose>
                    <xsl:when test="@ref and substring(sal:resolveURI(current(), @ref)[1],1, 5) = ('http:', '/exis') ">
                        <xsl:choose>
                            <xsl:when test="not(./pb)"> <!-\- The entity does not contain a pagebreak intervention - no problem then -\->
                                <xsl:element name="a">
                                    <xsl:attribute name="href" select="sal:resolveURI(current(), @ref)"/>
                                    <xsl:attribute name="target">_blank</xsl:attribute>
                                    <xsl:apply-templates/>
                                </xsl:element>
                            </xsl:when>
                            <xsl:otherwise>             <!-\- Otherwise, make an anchor for the preceding part, then render the pb, then "continue" the anchor -\->
                                <xsl:element name="a">
                                    <xsl:attribute name="href" select="sal:resolveURI(current(), @ref)"/>
                                    <xsl:attribute name="target">_blank</xsl:attribute>
                                    <xsl:apply-templates select="./pb/preceding-sibling::node()"/>
                                </xsl:element>
                                <xsl:apply-templates select="./pb"/>
                                <xsl:element name="a">
                                    <xsl:attribute name="href" select="sal:resolveURI(current(), @ref)"/>
                                    <xsl:attribute name="target">_blank</xsl:attribute>
                                    <xsl:apply-templates select="./pb/following-sibling::node()"/>
                                </xsl:element>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:apply-templates/>
                    </xsl:otherwise>
                </xsl:choose>
                :)
        
        default return
            render:passthru($node, $mode)
};


declare function render:note($node as element(tei:note), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                let $currentSection := sal-util:copy(render:getCitableParent($node))
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
        
        case 'html-title' return ()
        
        case 'html' return
            render:makeMarginalHTML($node)
        
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citetrail' return
            render:makeMarginalCitetrail($node)
        
        case 'passagetrail' return
            if (render:isPassagetrailNode($node)) then
                (: passagetrail parents of note are div, not p :)
                let $currentSection := sal-util:copy($node/ancestor::*[render:isPassagetrailNode(.) and not(self::tei:p)][1])
                let $currentNode := $currentSection//tei:note[@xml:id eq $node/@xml:id]
                let $prefix := $config:citationLabels(local-name($node))?('abbr')
                let $label := 
                    if ($node/@n) then '"' || $node/@n || '"' (: TODO: what if there are several notes with the same @n in a div :)
                    else string(count($currentSection//tei:note
                                      intersect $currentNode/preceding::tei:note) + 1)
                return $prefix || ' ' || $label
            else ()
            
        case 'orig'
        case 'edit' return (: TODO: distinguish using render:isMarginalNode($node) :)
            (:($config:nl, '        {', render:passthru($node, $mode), '}', $config:nl):)
            ('{', $config:nl, '        ', render:passthru($node, $mode), '        ', $config:nl, '}') 
        
        default return
            render:passthru($node, $mode)
};


declare function render:orgName($node as element(tei:orgName), $mode as xs:string) {
    switch($mode)
        case 'snippets-orig'
        case 'snippets-edit' return
            render:passthru($node, $mode)
        default return
            render:name($node, $mode)
};

declare function render:orig($node as element(tei:orig), $mode) {
    switch($mode)
        case 'snippets-orig' return
            render:passthru($node, $mode)
        case 'snippets-edit' return
            if (not($node/preceding-sibling::tei:reg|$node/following-sibling::tei:reg)) then
                render:passthru($node, $mode)
            else ()
        default return
            render:origElem($node, $mode)
};


declare function render:origElem($node as element(), $mode as xs:string) {
    switch($mode)
        case 'orig' return
            render:passthru($node, $mode)
        
        case 'edit' return
            if (not($node/(preceding-sibling::tei:expan|preceding-sibling::tei:reg|preceding-sibling::tei:corr|following-sibling::tei:expan|following-sibling::tei:reg|following-sibling::tei:corr))) then
                render:passthru($node, $mode)
            else ()
            
        case 'html' return 
            if ($node/parent::tei:choice) then
                let $editString := string-join(render:dispatch($node/parent::tei:choice/(tei:expan|tei:reg|tei:corr), 'edit'), '')
                return
                    <span class="original {local-name($node)} unsichtbar" title="{$editString}">
                        {string-join(render:passthru($node, $mode), '')}
                    </span>
            else 
                render:passthru($node, $mode)
        
        default return
            render:passthru($node, $mode)
};


declare function render:p($node as element(tei:p), $mode as xs:string) {
    switch($mode)
        case 'title' 
        case 'html-title' return
            normalize-space(
                render:teaserString($node, 'edit')
            )
        
        case 'class' return
            'tei-' || local-name($node)
        
        case 'citetrail' return
            (:if (render:isUnnamedCitetrailNode($node)) then 
                string(render:determineUnnamedCitetrailNodePosition($node))
            else ():)
            error()
        
        case 'passagetrail' return
            if (render:isPassagetrailNode($node)) then
                let $prefix := $config:citationLabels(local-name($node))?('abbr')
                let $teaser := '"' || normalize-space(substring(substring-after(render:p($node, 'title'), '"'),1,15)) || '…"'(: short teaser :)
                return $prefix || ' ' || $teaser
            else ()
        
        case 'orig'
        case 'edit' return
            if ($node/ancestor::tei:note) then
                if ($node/following-sibling::tei:p) then
                    (render:passthru($node, $mode), $config:nl)
                else
                    render:passthru($node, $mode)
            else
                ($config:nl, render:passthru($node, $mode), $config:nl)
        
        case 'html' return
            (: special cases :)
            if ($node/ancestor::tei:note) then
                <span class="note-paragraph">
                    {render:passthru($node, $mode)}
                </span>
            else if ($node/ancestor::tei:item) then
                <span class="item-paragraph">
                    {render:passthru($node, $mode)}
                </span>
            else if ($node/ancestor::tei:titlePage) then
                <span class="tp-paragraph">
                    {render:passthru($node, $mode)}
                </span>
            (: main text: :)
            else if ($node/ancestor::item[not(ancestor::list/@type = ('dict', 'index'))]) then
                <p id="{$node/@xml:id}">
                    {render:passthru($node, $mode)}
                </p>
            else
                render:passthru($node, $mode)
        
        case 'snippets-orig'
        case 'snippets-edit' return
            render:passthru($node, $mode)
(:            for $subnode in $node/node() where (local-name($subnode) ne 'note') return render:dispatch($subnode, $mode):)
        
        default return
            render:passthru($node, $mode)
};


declare function render:passthru($nodes as node()*, $mode as xs:string) as item()* {
    for $node in $nodes/node() return 
        if ($mode = ('snippets-orig', 'snippets-edit') and render:isMarginalNode($node)) then 
            (: basic separator for main and marginal nodes in snippet creation :)
            ()
        else render:dispatch($node, $mode)
};


declare function render:pb($node as element(tei:pb), $mode as xs:string) {
    switch($mode)
        case 'title'
        case 'html-title' return
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
        
        case 'citetrail' return
            (: "pagX" where X is page number :)
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
        
        case 'html' return
            if (render:isIndexNode($node)) then 
                let $inlineBreak :=
                    if ($node[@type eq 'blank']) then (: blank pages - make a typographic line break :)
                        <br/>
                    else if ($node[preceding::tei:pb 
                                   and preceding-sibling::node()[descendant-or-self::text()[not(normalize-space() eq '')]]                                                                                
                                   and following-sibling::node()[descendant-or-self::text()[not(normalize-space() eq '')]]]) then
                        (: mark page break by means of '|', but not at the beginning or end of structural sections :)
                        if ($node/@break eq 'no') then '|' else ' | '
                    else ()
                let $link :=
                    if ($node[@n]) then
                        let $pageAnchor := 'pageNo_' || (if ($node/@xml:id) then $node/@xml:id/string() else generate-id($node))
                        let $title := if (contains($node/@n, 'fol.')) then 'View image of ' || $node/@n else 'View image of p. ' || $node/@n
                        return
                            <div class="pageNumbers">
                                <a href="{render:resolveFacsURI($node/@facs)}" data-canvas="{render:resolveCanvasID($node)}"
                                   data-sal-id="{render:makeCitetrailURI($node)}" id="{$pageAnchor}" title="{$title}"
                                   class="pageNo messengers">
                                    <i class="fas fa-book-open facs-icon"/>
                                    {' '}
                                        {render:pb($node, 'html-title')}
                                </a>
                            </div>
                    else ()
                return ($inlineBreak, $link)
            else ()
                    
        case 'passagetrail' return
            if (contains($node/@n, 'fol.')) then $node/@n/string()
            else 'p. ' || $node/@n/string()
        
        case 'orig'
        case 'edit' return
            if (not($node/@break = 'no')) then
                ' '
            else ()
        
        case 'snippets-orig'
        case 'snippets-edit' return
            if (not($node/@break = 'no')) then
                ' '
            else ()
        
        (: pb nodes are excellent candidates for tracing the speed/performance of document processing, 
            since they are equally distributed throughout a document :)
        case 'debug' return
            util:log('warn', '[RENDER] Processing tei:pb node ' || $node/@xml:id)
        
        default return () (: some sophisticated function to insert a pipe and a pagenumber div in the margin :)
};


declare function render:persName($node as element(tei:persName), $mode as xs:string) {
    switch($mode)
        case 'snippets-orig' return
            render:passthru($node, $mode)
        
        case 'snippets-edit' return
            (: make persons searchable by their normalized names or IDs :)
            if ($node/@key and $node/@ref) then
                string($node/@key) || ' [' || string($node/@ref) || ']'
            else if ($node/@key) then
                string($node/@key)
            else if ($node/@ref) then
                '[' || string($node/@ref) || ']'
            else
                render:passthru($node, $mode)
        
        case 'html' return
            render:name($node, $mode)
        
        case 'class' return ()
        
        default return
            render:name($node, $mode)
};

declare function render:placeName($node as element(tei:placeName), $mode as xs:string) {
    switch($mode)
        case 'snippets-orig' return
            render:passthru($node, $mode)
        case 'snippets-edit' return
            (: make persons searchable by their normalized names :)
            if ($node/@key) then
                string($node/@key)
            else
                render:passthru($node, $mode)
        case 'html' return
            render:name($node, $mode)
        case 'class' return ()
        default return
            render:name($node, $mode)
};

(: Same as render:persName() :)
declare function render:publisher($node as element(tei:publisher), $mode as xs:string) {
    switch($mode)
        case 'snippets-orig' return
            render:passthru($node, $mode)
        
        case 'snippets-edit' return
            if ($node/@key and $node/@ref) then
                string($node/@key) || ' [' || string($node/@ref) || ']'
            else if ($node/@key) then
                string($node/@key)
            else if ($node/@ref) then
                '[' || string($node/@ref) || ']'
            else
                render:passthru($node, $mode)
        
        case 'html' return
            render:name($node, $mode)
        
        default return
            render:name($node, $mode)
};

(: Same as render:placeName() :)
declare function render:pubPlace($node as element(tei:pubPlace), $mode as xs:string) {
    switch($mode)
        case 'snippets-orig' return
            render:passthru($node, $mode)
        case 'snippets-edit' return
            if ($node/@key) then
                string($node/@key)
            else
                render:passthru($node, $mode)
        case 'html' return
            render:name($node, $mode)
        default return
            render:name($node, $mode)
};

declare function render:quote($node as element(tei:quote), $mode as xs:string) {
    switch($mode)
        case 'orig'
        case 'edit' return
            ('"', render:passthru($node, $mode), '"')
        
        case 'snippets-edit'
        case 'snippets-orig' return
            render:passthru($node, $mode)
            
        case 'html' return
            (:<span class="quote">
                {:)render:passthru($node, $mode)(:}
            </span>:)
            (: how to deal with longer quotes, spanning several paragraphs or even divs? (possible solution: anchors) :)
        
        default return
            ('"', render:passthru($node, $mode), '"')
};

declare function render:ref($node as element(tei:ref), $mode as xs:string) {
    switch($mode)
        case 'html' return
            if ($node/@type eq 'note-anchor') then
                () (: omit note references :)
            else if ($node/@target) then
                let $resolvedUri := render:resolveURI($node, $node/@target) (: TODO: verify that this works :)
                return render:transformToHTMLLink($node, $resolvedUri)
            else render:passthru($node, $mode)
        
        default return
            render:passthru($node, $mode)
};


declare function render:reg($node as element(tei:reg), $mode) {
    switch($mode)
        case 'snippets-orig' return 
            ()
        case 'snippets-edit' return
            render:passthru($node, $mode)
        default return
            render:editElem($node, $mode)
};

declare function render:row($node as element(tei:row), $mode) {
    switch($mode)
        case 'html' return 
            <tr>{render:passthru($node, $mode)}</tr>
        
        default return
            render:passthru($node, $mode)
};

declare function render:sic($node as element(tei:sic), $mode) {
    switch($mode)
        case 'snippets-orig' return
            render:passthru($node, $mode)
        case 'snippets-edit' return
            if (not($node/preceding-sibling::tei:corr|$node/following-sibling::tei:corr)) then
                render:passthru($node, $mode)
            else ()
        default return
            render:origElem($node, $mode)
};

declare function render:signed($node as element(tei:signed), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                render:teaserString($node, 'edit')
            )
        
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citetrail' return
            (:if (render:isUnnamedCitetrailNode($node)) then 
                string(render:determineUnnamedCitetrailNodePosition($node))
            else ():)
            error()
            
        case 'snippets-orig'
        case 'snippets-edit' return
            render:passthru($node, $mode)
(:            for $subnode in $node/node() where (local-name($subnode) ne 'note') return render:dispatch($subnode, $mode):)
            
        case 'html' return
            <div class="signed">
                {render:passthru($node, $mode)}
            </div>
            
        default return
            render:passthru($node, $mode)
};


declare function render:soCalled($node as element(tei:soCalled), $mode as xs:string) {
    if ($mode=("orig", "edit")) then
        ("'", render:passthru($node, $mode), "'")
    else if ($mode = ('snippets-edit', 'snippets-orig')) then
        render:passthru($node, $mode)
    else
        ("'", render:passthru($node, $mode), "'")
};

declare function render:space($node as element(tei:space), $mode as xs:string) {
    if ($node/@dim eq 'horizontal' or @rendition eq '#h-gap') then ' ' else ()
};


declare function render:supplied($node as element(tei:supplied), $mode as xs:string) {
    switch($mode)
        case 'html' return
            (<span class="original unsichtbar" title="{string($node)}">{'[' || string-join(render:passthru($node,$mode)) || ']'}</span>,
            <span class="edited" title="{concat('[', string($node), ']')}">{render:passthru($node,$mode)}</span>)
            
        default return
            render:passthru($node, $mode)
};


declare function render:table($node as element(tei:table), $mode as xs:string) {
    switch($mode)
        case 'html' return
            <table>{render:passthru($node, $mode)}</table>
            
        case 'citetrail' return
            (:if (render:isUnnamedCitetrailNode($node)) then 
                string(render:determineUnnamedCitetrailNodePosition($node))
            else ():)
            error()
            
        default return render:passthru($node, $mode)
};

(: FIXME: In the following, work mode functionality has to be added - also paying attention to intervening pagebreak marginal divs :)
declare function render:term($node as element(tei:term), $mode as xs:string) {
    switch($mode)
        case 'orig' 
        case 'snippets-orig' return
            render:passthru($node, $mode)
        
        case 'edit' return
            if ($node/@key) then
                (render:passthru($node, $mode), ' [', string($node/@key), ']')
            else
                render:passthru($node, $mode)
        
        case 'snippets-edit' return
            if ($node/@key) then
                string($node/@key)
            else
                render:passthru($node, $mode)
        
        case 'html' return
            render:name($node, $mode)
        
        default return
            render:passthru($node, $mode)
};

declare function render:text($node as element(tei:text), $mode as xs:string) {
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
        case 'html-title' return
            if ($node/@type eq 'work_volume') then
                'Vol. ' || $node/@n/string()
            else ()
        
        case 'html' return
            if (render:isCitableWithTeaserHTML($node)) then
                let $delimiter := 
                    if ($node/@type eq 'work_volume' and $node/preceding::tei:text[@type eq 'work_volume']) 
                        then <hr/> 
                    else ()
                return ($delimiter, render:passthru($node, $mode))
            else render:passthru($node, $mode)
        
        case 'class' return
            if ($node/@type eq 'work_volume') then 'tei-text-' || $node/@type
            else if ($node/@xml:id eq 'completeWork') then 'tei-text-' || $node/@xml:id
            else if (matches($node/@xml:id, 'work_part_[a-z]')) then 'elem-text-' || $node/@xml:id
            else 'tei-text'
        
        case 'citetrail' return
            (: "volX" where X is the current volume number, don't use it at all for monographs :)
            if ($node/@type eq 'work_volume') then
               concat('vol', count($node/preceding::tei:text[@type eq 'work_volume']) + 1)
            else ()
        
        case 'passagetrail' return
            if (render:isPassagetrailNode($node)) then
                'vol. ' || $node/@n
            else ()
        
        default return
            render:passthru($node, $mode)
};

declare function render:textNode($node as node(), $mode as xs:string) {
    switch($mode)
        case "orig"
        case "edit" return
            let $leadingSpace   := if (matches($node, '^\s+')) then ' ' else ()
            let $trailingSpace  := if (matches($node, '\s+$')) then ' ' else ()
            return concat($leadingSpace, 
                          normalize-space(replace($node, '&#x0a;', ' ')),
                          $trailingSpace)
        
        case 'html'
        case 'snippets-orig' 
        case 'snippets-edit' return 
(:            let $debug := util:log('warn', 'Processing textNode: ' || $node) return:)
            $node
        
        default return 
            $node
};

declare function render:title($node as element(tei:title), $mode as xs:string) {
    switch($mode)
        case 'snippets-orig' return
            render:passthru($node, $mode)
        
        case 'snippets-edit' return
            if ($node/@key) then
                string($node/@key)
            else
                render:passthru($node, $mode)
        
        case 'html' return
            render:name($node, $mode)
        
        default return
            render:name($node, $mode)
};

declare function render:titlePage($node as element(tei:titlePage), $mode as xs:string) {
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
        
        case 'citetrail' return
            if (count($node/ancestor::tei:front//tei:titlePage) gt 1) then
                'titlepage' || string(count($node/preceding-sibling::tei:titlePage) + 1)
            else 'titlepage'
        
        case 'passagetrail' return
            $config:citationLabels(local-name($node))?('abbr')
        
        case 'html' return
            let $toolbox := render:HTMLSectionToolbox($node)
            (: distinguishing first and subsequent titlePage(s) for rendering them differently :)
            let $class := if ($node[not(preceding-sibling::tei:titlePage)]) then 'titlePage' else 'sec-titlePage'
            return
                <div class="{$class}">
                    {$toolbox}
                    <div class="titlePage-body">
                        {render:passthru($node, $mode)}
                    </div>
                </div>
        
        default return
            render:passthru($node, $mode)
};

declare function render:titlePart($node as element(tei:titlePart), $mode as xs:string) {
    switch($mode)
        case 'title' return
            normalize-space(
                render:teaserString($node, 'edit')
            )
            
        case 'class' return
            'tei-' || local-name($node)
            
        case 'citetrail' return
            (: "titlePage.X" where X is the number of parts where this occurs :)
            concat('titlepage.', string(count($node/preceding-sibling::tei:titlePart) + 1))
        
        case 'html' return
            if ($node/@type eq 'main') then
                <h1>{render:passthru($node, $mode)}</h1>
            else render:passthru($node, $mode)
            
        default return 
            render:passthru($node, $mode)
};


declare function render:unclear($node as element(tei:unclear), $mode as xs:string) {
    switch($mode)
        case 'html' return
            (: TODO i18n title :)
            if ($node//text()) then
                <span title="unclear" class="sal-unclear-text">{render:passthru($node, $mode)}</span>
            else <span title="unclear" class="sal-unclear"/>
            
        default return 
            render:passthru($node, $mode)
};

(: TODO - Html:
    * add line- and column breaks in diplomatic view? (problem: infinite scrolling has to comply with the current viewmode as well!)
    * make bibls, ref span across (page-)breaks (like persName/placeName/... already do)
    * teasers: break text at word boundaries
:)
