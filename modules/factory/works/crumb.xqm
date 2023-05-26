xquery version "3.1";

module namespace crumb         = "https://www.salamanca.school/factory/works/crumb";
declare namespace exist            = "http://exist.sourceforge.net/NS/exist";
declare namespace tei              = "http://www.tei-c.org/ns/1.0";
declare namespace sal              = "http://salamanca.adwmainz.de";
declare namespace xi                = "http://www.w3.org/2001/XInclude";
declare namespace admin              = "http://www.salamanca.school/xquery/admin";
 

import module namespace functx      = "http://www.functx.com";
import module namespace util       = "http://exist-db.org/xquery/util";
import module namespace console    = "http://exist-db.org/xquery/console";
import module namespace config     = "http://www.salamanca.school/xquery/config" at "xmldb:exist:///db/apps/salamanca/modules/config.xqm";
import module namespace sutil    = "http://www.salamanca.school/xquery/sutil" at "xmldb:exist:///db/apps/salamanca/modules/sutil.xqm";
import module namespace txt        = "https://www.salamanca.school/factory/works/txt" at "xmldb:exist:///db/apps/salamanca/modules/factory/works/txt.xqm";
import module namespace index = "https://www.salamanca.school/factory/works/index" at "xmldb:exist:///db/apps/salamanca/modules/factory/works/index.xqm";


declare variable $crumb:crumbtrailConnector := ' » ';

(: 
Marie-Astrid Hugel, 25.08.2022

Specific file to create the Crumbtrails. 

This file is responsible for creating the crumbtrails since August 2022. 
The QualityCheck has been improved inside the admin:createCrumbtrails function. 

:)





declare function crumb:createCrumbNode($tei as element(tei:TEI)) as map(*) {
 let $wid := $tei/@xml:id
let $work := util:expand($tei)
  let $xincludes := $tei//tei:text//xi:include/@href
    let $fragmentationDepth := index:determineFragmentationDepth($tei)
 let $target-set := index:getFragmentNodes($work, $fragmentationDepth)
 (: First, get all relevant nodes :)
    let $nodes := 
        for $text in $work//tei:text[@type = ('work_volume', 'work_monograph')] return 
            (: make sure that we only grasp nodes that are within a published volume :)
            if (($text/@type eq 'work_volume' and sutil:WRKisPublished($wid || '_' || $text/@xml:id))
                or $text/@type eq 'work_monograph') then 
                $text/descendant-or-self::*[index:isIndexNode(.)]
            else ()
(: Create the fragment id for each node beforehand, so that recursive crumbtrail creation has it readily available :)
 let $fragmentIds :=
        map:merge(
            for $node in $nodes
                let $n := $node/@xml:id/string()
                let $frag := (($node/ancestor-or-self::tei:* | $node//tei:*) intersect $target-set)[1]
                let $fragId := index:makeFragmentId(functx:index-of-node($target-set, $frag), $frag/@xml:id)
                return map:entry($n, $fragId)
        )
let $crumbTree := 
<sal:crumb> {crumb:extractStructure($wid, $work//tei:text[not(ancestor::tei:text)], $xincludes, $fragmentIds)}</sal:crumb>

let $crumb := 
<sal:crumb  work="{$wid}" xml:space="preserve">{crumb:createSalNode($crumbTree)} </sal:crumb>


 let $check := crumb:qualityCheck($crumb, $work, $target-set, $fragmentationDepth) 

  
return
 map{
  'crumbtrails': $crumb,
 
            'fragmentation_depth': $fragmentationDepth,
             'missed_elements': $check('missed_elements'),
            'unidentified_elements': $check('unidentified_elements'),
            'target_set_count': count($target-set)
}
};



declare function crumb:extractStructure($wid as xs:string, $input as node()*, $xincludes as attribute()*, $fragmentIds as map()?) as element(sal:nodecrumb)* {
   for $node in $input return
       (: sans$input//sal:nodecrumb = renvoie 25 résultats et non pas 1 seul. :)
        typeswitch($node)
            case element() return
  
     if (:(index:isIndexNode($node)):) ($node/@xml:id and $fragmentIds($node/@xml:id)) then
                    let $debug := if ($config:debug = ("trace") and $node/self::tei:pb) then index:pb($node, 'debug') else ()
                    let $subtype := 
                        if ($node[self::tei:milestone]/@n) then (: TODO: where is this used? :)
                            string($node/@n)
                        else if ($node/@type) then
                            string($node/@type)
                        else ()
(:                    let $isBasicNode := if (index:isBasicNode($node)) then 'true' else 'false':)
                  (:  let $isNamedCitetrailNode := if (index:isNamedCitetrailNode($node)) then 'true' else 'false' :)
(:                    let $category := index:getNodeCategory($node):)
(:                    let $isPassageNode := if (index:isPassagetrailNode($node)) then 'true' else 'false':)
                    return
                        element sal:nodecrumb {
                           attribute type              {local-name($node)}, 
                           attribute xml:id                 {$node/@xml:id/string()},
                            if ($node/@xml:id eq 'completeWork' and $xincludes) then
                                attribute xinc          {$xincludes}
                            else (), 
                            attribute class             {index:dispatch($node, 'class')},
(:                            attribute category          {$category},:)
(:                            attribute isBasic           {$isBasicNode},:)
                           
                            element sal:crumbtrail           {crumb:makeCrumb($wid, $node, $fragmentIds)},
                           
                            element sal:citableParent   {index:getCitableParent($node)/@xml:id/string()},
                          
                            element sal:children        {crumb:extractStructure($wid, $node/node(), $xincludes, $fragmentIds)}
                        }
                else crumb:extractStructure($wid, $node/node(), $xincludes, $fragmentIds)
            default return ()
};       


declare function crumb:createSalNode($input as element(sal:crumb)) as element(sal:nodecrumb)* {
   
for $node in $input//sal:nodecrumb return

   
    
    let $crumbtrail := crumb:constructCrumbtrail($node)
 return

    element sal:nodecrumb{
$node/@*,
attribute n {$node/@xml:id/string()},
attribute citableParent {$node/sal:citableParent/string()},
        element sal:crumbtrail {$crumbtrail}
        }

};


(: ici problème: le node/citableParent n'est pas dans le même fichier, mais dans le fichier index. Faut-il faire un appel vers le fichier index ?  :)

declare function crumb:constructCrumbtrail($node as element(sal:nodecrumb)) as item()+ {

    let $prefix := 
        if ($node/sal:citableParent/text() and $node/ancestor::sal:nodecrumb[@xml:id eq $node/sal:citableParent/text()]) then
            crumb:constructCrumbtrail( $node/ancestor::sal:nodecrumb[@xml:id eq $node/sal:citableParent/text()])
        else ()
    let $this := $node/sal:crumbtrail/*
    return
        if ($prefix and $this) then ($prefix, $crumb:crumbtrailConnector, $this) else $this
};

(: creation of the <a> element :)

declare function crumb:makeCrumb($wid as xs:string, $node as node(), $fragmentIds as map()?) as element(a)? {
    let $class := index:dispatch($node, 'class')
    return
        if ($class) then
            <a class="{$class}" href="{index:makeUrl($wid, $node, $fragmentIds)}">{index:dispatch($node, 'title')}</a>
        else 
            <a href="{index:makeUrl($wid, $node, $fragmentIds)}">{index:dispatch($node, 'title')}</a>
};



(: corriger le qualityCheck pour faire apparaître les éléments :) 


declare function crumb:qualityCheck($crumb as element(sal:crumb), 
                                    $work as element(tei:TEI), 
                                    $targetNodes as element()*, 
                                    $fragmentationDepth as xs:integer) {
                                    
    let $wid := $work/@xml:id
    
    (: #### Basic quality / consistency check #### :)
    let $resultNodes := $crumb//sal:nodecrumb
    let $testNodes := 
        if (count($resultNodes) eq 0) then 
            error(xs:QName('admin:createNodeCrumb'), 'Node crumbing did not produce any results.') 
        else ()
    (: every ordinary sal:node should have all of the required fields and values: :)
    
    let $testChildren := if ($testNodes[not(sal:citableParent/text() and sal:crumbtrail/* )]) then error() else ()

let $testAttributes := if ($testNodes[not(@type and @xml:id and @href and @class)]) then error() else ()
    (: there should be as many distinctive citetrails and crumbtrails as there are ordinary sal:node elements: :)
    
    (: search these cases using: " //sal:citetrail[./text() = following::sal:citetrail/text()] --> à garder ? Car pas de citretrails ici... :)
  (:  let $testEmptyCrumbrails :=
        if (count($resultNodes/sal:crumbtrail[not(./text())]) gt 0) then
            error(xs:QName('admin:createNodeIndex'), 
                  'Could not produce a citetrail for one or more sal:node (in' || $wid || '). Problematic nodes: '
                  || string-join(($resultNodes[not(sal:crumbtrail/text())]/@n), '; '))
        else () :)
    (: search for " //sal:citetrail[not(./text())] ":)
    (: not checking crumbtrails here ATM for not slowing down index creation too much... :)
    
    (: check whether all text is being captured through basic index nodes (that is, whether every single passage is citable) :)
    let $checkBasicNodes_crumb := 
        for $t in $work//tei:text[@type eq 'work_monograph' 
                                  or (@type eq 'work_volume' and sutil:WRKisPublished($wid || '_' || @xml:id))]
                                  //text()[normalize-space() ne ''] return
            if ($t[not(ancestor::*[index:isBasicNode(.)]) and not(ancestor::tei:figDesc)]) then 
                let $debug := util:log('error', 'Encountered text node without ancestor::*[index:isBasicNode(.)], in line ' || $t/preceding::tei:lb[1]/@xml:id/string() || ' – this might indicate a structural anomaly in the TEI data.')
                return error(xs:QName('admin:createCrumbNode'), 'Encountered text node without ancestor::*[index:isBasicNode(.)], in line ' || $t/preceding::tei:lb[1]/@xml:id/string()) 
            else ()
    (: if no xml:id is put out, try to search these cases like so:
        //text//text()[not(normalize-space() eq '')][not(ancestor::*[@xml:id and (self::p or self::signed or self::head or self::titlePage or self::lg or self::item or self::label or self::argument or self::table)])]
    :)

                       
    
    (: See if there are any leaf elements in our text that are not matched by our rule :)
    let $missed-elements := $work//(tei:front|tei:body|tei:back)//tei:*[count(./ancestor-or-self::tei:*) < $fragmentationDepth][not(*)]
    (: See if any of the elements we did get is lacking an xml:id attribute :)
    let $unidentified-elements := $targetNodes[not(@xml:id)]
    (: Keep track of how long this index did take :)
    
    return 
        (: return information that we want to inform about rather than throw hard errors :)
        map {
            'missed_elements': $missed-elements,
            'unidentified_elements': $unidentified-elements
        }
};


