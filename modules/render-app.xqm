xquery version "3.1";

(: ####++++---- 

    A collection of rendering mechanisms for different,
    not necessarily related parts of the app, such as
    dictinary lemmata, search help, participants pages, etc.
    In essence, for everything that is not a work.

 ----++++#### :)

module namespace render-app        = "http://www.salamanca.school/xquery/render-app";

declare namespace exist            = "http://exist.sourceforge.net/NS/exist";
declare namespace output           = "http://www.w3.org/2010/xslt-xquery-serialization";
declare namespace tei              = "http://www.tei-c.org/ns/1.0";
declare namespace sal              = "http://salamanca.adwmainz.de";

import module namespace console    = "http://exist-db.org/xquery/console";
import module namespace request    = "http://exist-db.org/xquery/request";

import module namespace config     = "http://www.salamanca.school/xquery/config" at "xmldb:exist:///db/apps/salamanca/modules/config.xqm";
import module namespace i18n       = "http://exist-db.org/xquery/i18n"           at "xmldb:exist:///db/apps/salamanca/modules/i18n.xqm";
import module namespace sutil      = "http://www.salamanca.school/xquery/sutil"  at "xmldb:exist:///db/apps/salamanca/modules/sutil.xqm";

(: Helper functions :)

declare function render-app:makeLink($node as element(*), $mode as xs:string, $lang as xs:string?, $linkAddress as xs:string?, $linkName as xs:string?) {
    let $workPsgId  := tokenize(tokenize($linkAddress, 'work:'  )[2], ' ')[1]
    let $workId     := tokenize($workPsgId, ':')[1]
    let $lemmaId    := tokenize(tokenize(tokenize($linkAddress, 'lemma:'  )[2], ' ')[1], ':')[1]
    let $valWorkId  := sutil:WRKvalidateId($workId)
    let $valLemmaId := sutil:LEMvalidateId($lemmaId)
    let $available  :=  if (($workId and $valWorkId < 1) or ($lemmaId and $valLemmaId < 1)) then
                            'unavailable'
                        else ()
    let $classnames :=  if (substring($linkAddress, 1, 4) = "http") then
                            "external_link"
                        else
                            let $resClass   :=  if ($workId) then
                                                    'hi_work_'   || $workId
                                                else if ($lemmaId) then
                                                    'hi_lemma_'   || $lemmaId
                                                else ()
                            let $debug      :=  if ($config:debug = ("trace") and (($workId and $valWorkId < 0) or ($lemmaId and $valLemmaId < 0))) then
                                                    if ($workId) then
                                                        console:log("[Render] invalid $workId: " || $workId || " (validate: " || $valWorkId || ").")
                                                    else if ($lemmaId) then
                                                        console:log("[Render] invalid $lemmaId: " || $lemmaId || " (validate: " || $valLemmaId || ").")
                                                    else
                                                        console:log("[Render] ?? invalid ?? $workId: " || $workId || " (validate: " || $valWorkId || "), $lemmaId: " || $lemmaId || " (validate: " || $valLemmaId || ").")
                                                else ()
                            return string-join(($node/local-name(.), $resClass, $available), " ")
    let $targetURL :=   if ($available = "unavailable") then
                            "javascript: void(0)"
                        else if ($workId) then
                            concat($config:idserver, '/texts/', $workPsgId)
                        else if ($lemmaId) then
                            concat($config:idserver, '/texts/', $lemmaId)
                        else
                            $linkAddress
    let $key       := if ($linkName) then $linkName else ""
    let $title     := if ($available = "unavailable") then "not available yet" else $key
    let $newTab    := boolean($classnames = "external_link")
    return  if ($newTab) then
                <a class="{$classnames}" href="{$targetURL}" title="{$title}" target="_blank">{render-app:passthru($node, $mode, $lang)}</a>
            else
                <a class="{$classnames}" href="{$targetURL}" title="{$title}">{render-app:passthru($node, $mode, $lang)}</a>
};


(:
~ Modes:
~  - 'participants': HTML rendering for the project team's page (projectTeam.html)
:)
declare function render-app:dispatch($node as node(), $mode as xs:string, $lang as xs:string?) {
    typeswitch($node)
        case text()                     return render-app:textNode($node, $mode, $lang)
        case comment()                  return ()
        case processing-instruction()   return ()

        case element(tei:abbr)          return render-app:abbr($node, $mode, $lang)
        case element(tei:bibl)          return render-app:bibl($node, $mode, $lang)
        case element(tei:birth)         return render-app:birth($node, $mode, $lang) 
        case element(tei:cb)            return render-app:cb($node, $mode, $lang)
        case element(tei:corr)          return render-app:corr($node, $mode, $lang)
        case element(tei:death)         return render-app:death($node, $mode, $lang)
        case element(tei:div)           return render-app:div($node, $mode, $lang)
        case element(tei:eg)            return render-app:eg($node, $mode, $lang)
        case element(tei:email)         return render-app:email($node, $mode, $lang)
        case element(tei:emph)          return render-app:emph($node, $mode, $lang)
        case element(tei:event)         return render-app:event($node, $mode, $lang)
        case element(tei:expan)         return render-app:expan($node, $mode, $lang)
        case element(tei:foreign)       return render-app:foreign($node, $mode, $lang)
        case element(tei:fw)            return render-app:fw($node, $mode, $lang)
        case element(tei:g)             return render-app:g($node, $mode, $lang)
        case element(tei:gloss)         return render-app:gloss($node, $mode, $lang)
        case element(tei:head)          return render-app:head($node, $mode, $lang) (: snippets: passthru :)
        case element(tei:hi)            return render-app:hi($node, $mode, $lang) 
        case element(tei:item)          return render-app:item($node, $mode, $lang)
        case element(tei:keywords)      return render-app:keywords($node, $mode, $lang)
        case element(tei:lb)            return render-app:lb($node, $mode, $lang)
        case element(tei:list)          return render-app:list($node, $mode, $lang)
        case element(tei:listBibl)      return render-app:listBibl($node, $mode, $lang)
        case element(tei:milestone)     return render-app:milestone($node, $mode, $lang)
        case element(tei:name)          return render-app:nameNode($node, $mode, $lang)
        case element(tei:note)          return render-app:note($node, $mode, $lang)
        case element(tei:num)           return render-app:num($node, $mode, $lang)
        case element(tei:orgName)       return render-app:orgName($node, $mode, $lang)
        case element(tei:orig)          return render-app:orig($node, $mode, $lang)
        case element(tei:p)             return render-app:p($node, $mode, $lang)
        case element(tei:pb)            return render-app:pb($node, $mode, $lang)
        case element(tei:quote)         return render-app:quote($node, $mode, $lang)
        case element(tei:persName)      return render-app:persName($node, $mode, $lang)
        case element(tei:placeName)     return render-app:placeName($node, $mode, $lang)
        case element(tei:ref)           return render-app:ref($node, $mode, $lang) 
        case element(tei:reg)           return render-app:reg($node, $mode, $lang)
        case element(tei:sic)           return render-app:sic($node, $mode, $lang)
        case element(tei:soCalled)      return render-app:soCalled($node, $mode, $lang)
        case element(tei:term)          return render-app:term($node, $mode, $lang)
        case element(tei:title)         return render-app:name($node, $mode, $lang)

        case element(tei:figDesc)       return ()
        case element(tei:teiHeader)     return ()

        default return render-app:passthru($node, $mode, $lang)
};


declare function render-app:passthru($nodes as node()*, $mode as xs:string, $lang as xs:string?) as item()* {
    switch($mode)
        case 'participants' return
            for $node in $nodes/node() return 
                if (not($node/@xml:lang) or $node/@xml:lang eq $lang) then
                    render-app:dispatch($node, $mode, $lang)
                else ()
        default return
            for $node in $nodes/node() return render-app:dispatch($node, $mode, $lang)
};

declare function render-app:textNode($node as node(), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case "html"
        case "work" return
            let $leadingSpace   := if (matches($node, '^\s+')) then ' ' else ()
            let $trailingSpace  := if (matches($node, '\s+$')) then ' ' else ()
            return concat($leadingSpace, 
                          normalize-space(replace($node, '&#x0a;', ' ')),
                          $trailingSpace)
        
        case 'snippets-orig' 
        case 'snippets-edit'
        case 'participants' return
            (:if (not(normalize-space($node) eq '')) then:) $node (:else ' ':)
        
        default return ()
};


(: FIXME: In the following, the #anchor does not take account of html partitioning of works. Change this to use semantic section id's. :)
declare function render-app:head($node as element(tei:head), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'html'
        case 'work' return
            let $lang   := request:get-attribute('lang')
            let $page   :=      if ($node/ancestor::tei:text/@type="author_article") then
                                    "author.html?aid="
                           else if ($node/ancestor::tei:text/@type="lemma_article") then
                                    "lemma.html?lid="
                           else
                                    "work.html?wid="
            return    
                <h3 id="{$node/@xml:id}">
                    <a class="anchorjs-link" id="{$node/parent::tei:div/@xml:id}" href="{session:encode-url(xs:anyURI($page || $node/ancestor::tei:TEI/@xml:id || '#' || $node/parent::tei:div/@xml:id))}">
                        <span class="anchorjs-icon"></span>
                    </a>
                    {render-app:passthru($node, $mode, $lang)}
                </h3>
        
        case 'participants' return
            <h4>{render-app:passthru($node, $mode, $lang)}</h4>
        
        default return 
            render-app:passthru($node, $mode, $lang)
};

(: FIXME: In the following, work mode functionality has to be added - also paying attention to intervening pagebreak marginal divs :)
declare function render-app:term($node as element(tei:term), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'html'
        case 'work' return
            let $elementName    := "term"
            let $key            := $node/@key
            let $getLemmaId     := tokenize(tokenize($node/@ref, 'lemma:')[2], ' ')[1]
            let $highlightName  :=  
                if ($node/@ref) then
                    concat('hi_', translate(translate(translate(tokenize($node/@ref, ' ')[1], ',', ''), ' ', ''), ':', '_'))
                else if ($node/@key) then
                    concat('hi_', translate(translate(translate(tokenize($node/@key, ' ')[1], ',', ''), ' ', ''), ':', '_'))
                else ()
            let $dictLemmaName :=  
                if ($node/ancestor::tei:list[@type="dict"] and not($node/preceding-sibling::tei:term)) then
                    'dictLemma'
                else ()
            let $lemmaClass       := if ($getLemmaId) then "lemma_link hi_lemma_" || $getLemmaId else ()
            let $unavailableClass := if (sutil:LEMvalidateId($getLemmaId) < 1) then "unavailable" else ()
            let $title            := if ($unavailableClass = "unavailable") then "not available yet" else $key
            let $targetURL        := if ($unavailableClass = "unavailable") then "javascript: void(0)" else session:encode-url(xs:anyURI('lemma.html?lid=' || $getLemmaId))
            let $classes := normalize-space(string-join(($elementName, $lemmaClass, $unavailableClass, $highlightName, $dictLemmaName), ' '))
            return                
                if ($getLemmaId) then
                    <a class="{$classes}" title="{$title}" href="{$targetURL}">{render-app:passthru($node, $mode, $lang)}</a>
                 else
                    <span class="{$classes}" title="{$title}">{render-app:passthru($node, $mode, $lang)}</span>
        case 'snippets-orig' return
            render-app:passthru($node, $mode, $lang)
        case 'snippets-edit' return
            if ($node/@key) then
                string($node/@key)
            else
                render-app:passthru($node, $mode, $lang)
        case 'participants' return ()
        default return
            render-app:passthru($node, $mode, $lang)
};



declare function render-app:abbr($node as element(tei:abbr), $mode as xs:string, $lang as xs:string?) {
    render-app:origElem($node, $mode, $lang)
};

declare function render-app:bibl($node as element(tei:bibl), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'work' return
            let $getBiblId :=  $node/@sortKey
            return if ($getBiblId) then
                        <span class="{('bibl hi_' || $getBiblId)}">{render-app:passthru($node, $mode, $lang)}</span>
                    else
                        render-app:passthru($node, $mode, $lang)
        
        case 'participants' return
            <li>{render-app:passthru($node, $mode, $lang)}</li>
        
        default return
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:birth($node as element(tei:birth), $mode as xs:string, $lang as xs:string?) {
    if ($mode = ("html", "work")) then
        <span>*&#xA0;{render-app:name($node/tei:placeName[1], $mode, $lang) || ': ' || $node/tei:date[1]}</span>
    else if ($mode eq 'participants') then render-app:passthru($node, $mode, $lang)
    else ()
};

declare function render-app:cb($node as element(tei:cb), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'html'
        case 'work' 
        case 'snippets-orig' 
        case 'snippets-edit' return
            if (not($node/@break = 'no')) then
                ' '
            else ()
        
        case 'participants' return ()
        
        default return () (: some sophisticated function to insert a pipe and a pagenumber div in the margin :)
};

declare function render-app:corr($node as element(tei:corr), $mode as xs:string, $lang as xs:string?) {
    render-app:editElem($node, $mode, $lang)
};

declare function render-app:death($node as element(tei:death), $mode as xs:string, $lang as xs:string?) {
    if ($mode = ("html", "work")) then
        <span>†&#xA0;{render-app:name($node/tei:placeName[1], $mode, $lang) || ': ' || $node/tei:date[1]}</span>
    else if ($mode eq 'participants') then render-app:passthru($node, $mode, $lang)
    else ()
};

declare function render-app:div($node as element(tei:div), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'html' return
            if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                (<h4 id="{$node/@xml:id}">{string($node/@n)}</h4>,<p id="p_{$node/@xml:id}">{render-app:passthru($node, $mode, $lang)}</p>)
                (: oder das hier?:   <xsl:value-of select="key('targeting-refs', concat('#',@xml:id))[1]"/> :)
            else
                <div id="{$node/@xml:id}">{render-app:passthru($node, $mode, $lang)}</div>
        
        case 'work' return (: basically, the same except for eventually adding a <div class="summary_title"/> the data for which is complicated to retrieve :)
            render-app:passthru($node, $mode, $lang)
        
        case 'participants' return ()
        
        default return
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:editElem($node as element(), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case "html"
        case "work" return
            let $originalString := render-app:dispatch($node/parent::tei:choice/(tei:abbr|tei:orig|tei:sic), "orig", $lang)
            return  
                <span class="edited {local-name($node)}" title="{string-join($originalString, '')}">
                    {render-app:passthru($node, $mode, $lang)}
                </span>
        
        case 'snippets-orig' return ()
        case 'snippets-edit' return
            render-app:passthru($node, $mode, $lang)
        case 'participants' return ()
        default return
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:eg($node as element(tei:eg), $mode as xs:string, $lang as xs:string?) {
    if ($mode = ("html", "work")) then
        <pre>{render-app:passthru($node, $mode, $lang)}</pre>
    else if ($mode eq 'participants') then ()
    else 
        render-app:passthru($node, $mode, $lang)
};

declare function render-app:email($node as element(tei:email), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'participants' return 
            <span><i18n:text key="email"/>{(': ', render-app:passthru($node, $mode, $lang))}</span>
        default return 
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:emph($node as element(tei:emph), $mode as xs:string, $lang as xs:string?) {
    if ($mode = "work") then
        <span class="emph">{render-app:passthru($node, $mode, $lang)}</span>
    else if ($mode = "html") then
        <em>{render-app:passthru($node, $mode, $lang)}</em>
    else
        render-app:passthru($node, $mode, $lang)
};

declare function render-app:event($node as element(tei:event), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'participants' return 
            switch($node/@type/string())
                case 'person' return
                    <div>
                        <h4><i18n:text key="cv"/></h4>
                        {render-app:passthru($node, $mode, $lang)}
                    </div>
                case 'research_interest' return
                    <div>
                        <h4><i18n:text key="researchInterests"/></h4>
                        {render-app:passthru($node, $mode, $lang)}
                    </div>
                default return 
                    <div>{render-app:passthru($node, $mode, $lang)}</div>
        default return 
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:expan($node as element(tei:expan), $mode as xs:string, $lang as xs:string?) {
    render-app:editElem($node, $mode, $lang)
};

declare function render-app:foreign($node as element(tei:foreign), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case "html"
        case "work" return
            if ($node[./@xml:lang eq "gr"]) then
                render-app:passthru($node, $mode, $lang)
            else    
                render-app:italics($node, $mode, $lang)
        default return
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:fw($node as element(tei:fw), $mode as xs:string, $lang as xs:string?) {
    ()
};

declare function render-app:g($node as element(tei:g), $mode as xs:string, $lang as xs:string?) {
    switch ($mode)
        case "work" return
            let $originalGlyph := render-app:g($node, "orig", $lang)
            return
                (<span class="original glyph unsichtbar" title="{$node/text()}">
                    {$originalGlyph}
                </span>,
                <span class="edited glyph" title="{$originalGlyph}">
                    {$node/text()}
                </span>)
        case 'participants' return ()
        default return (: also 'snippets-edit' :)
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:gloss($node as element(tei:gloss), $mode as xs:string, $lang as xs:string?) {
    if ($mode = ("html", "work")) then
        render-app:passthru($node, $mode, $lang)
    else if ($mode eq 'participants') then ()
    else
        render-app:passthru($node, $mode, $lang)
};

declare function render-app:hi($node as element(tei:hi), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'html'
        case 'work'
        case 'participants' return
            switch($node/@rendition/string())
                case "#b" return
            <b>{render-app:passthru($node, $mode, $lang)}</b>
                case "#initCaps" return
                    <span class="initialCaps">{render-app:passthru($node, $mode, $lang)}</span>
                case "#it" return
                    render-app:italics($node, $mode, $lang)
                case "#l-indent" return
                    <span style="display:block;margin-left:4em;">
                        {render-app:passthru($node, $mode, $lang)}
                    </span>
                case "#r-center" return
                    <span style="display:block;text-align:center;">
                        {render-app:passthru($node, $mode, $lang)}
                    </span>
                case "#sc" return
                    <span class="smallcaps">
                        {render-app:passthru($node, $mode, $lang)}
                    </span>
                case "#spc" return
                    <span class="spaced">
                        {render-app:passthru($node, $mode, $lang)}
                    </span>
                case "#sub" return
                    <sub>
                        {render-app:passthru($node, $mode, $lang)}
                    </sub>
                case "#sup" return
                    <sup>
                        {render-app:passthru($node, $mode, $lang)}
                    </sup>
                default return
                    render-app:italics($node, $mode, $lang)
        default return 
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:italics($node as element(*), $mode as xs:string, $lang as xs:string?) {
    <it>{render-app:passthru($node, $mode, $lang)}</it>
};

declare function render-app:item($node as element(tei:item), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case "html"
        case "work" return
            if ($node/parent::tei:list/@type="simple") then
                render-app:passthru($node, $mode, $lang)
            else
                <li>{render-app:passthru($node, $mode, $lang)}</li>
        
        case 'participants' return
            <li>{render-app:passthru($node, $mode, $lang)}</li>
        
        case 'snippets-orig' 
        case 'snippets-edit' return
            ($config:nl, render-app:passthru($node, $mode, $lang), $config:nl)
        
        default return
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:keywords($node as element(tei:keywords), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'participants' return ()
        default return render-app:passthru($node, $mode, $lang)
};

declare function render-app:lb($node as element(tei:lb), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'work' 
        case 'snippets-orig' 
        case 'snippets-edit' return
            if (not($node/@break = 'no')) then
                ' '
            else ()
    
        case 'html'
        case 'participants' return
            <br/>
        default return () 
};

declare function render-app:list($node as element(tei:list), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
         case 'html'
         case 'work' return
             if ($node/@type = "ordered") then
                 <section>
                     {if ($node/child::tei:head) then
                         for $head in $node/tei:head
                             return
                                 <h4>
                                     {render-app:passthru($head, $mode, $lang)}
                                 </h4>
                      else ()
                     }
                     <ol>
                         {for $item in $node/tei:*[not(local-name() = "head")]
                                 return
                                     render-app:dispatch($item, $mode, $lang)
                         }
                     </ol>
                 </section>
             else if ($node/@type = "simple") then
                 <section>
                     {if ($node/tei:head) then
                         for $head in $node/tei:head
                             return
                                 <h4>{render-app:passthru($head, $mode, $lang)}</h4>
                      else ()
                     }
                     {for $item in $node/tei:*[not(local-name() = "head")]
                             return
                                     render-app:dispatch($item, $mode, $lang)
                     }
                 </section>
             else
                 <figure class="{$node/@type}">
                     {if ($node/child::tei:head) then
                         for $head in $node/tei:head
                             return
                                 <h4>{render-app:passthru($head, $mode, $lang)}</h4>
                      else ()
                     }
                     <ul>
                         {for $item in $node/tei:*[not(local-name() = "head")]
                                 return
                                     render-app:dispatch($item, $mode, $lang)
                         }
                     </ul>
                 </figure>
         
         case 'participants' return
             <ul>{render-app:passthru($node, $mode, $lang)}</ul>
         
         default return
             ($config:nl, render-app:passthru($node, $mode, $lang), $config:nl)
};

declare function render-app:listBibl($node as element(tei:listBibl), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'participants' return
            if ($node/@type eq 'publications') then
                <div>
                    <h4><i18n:text key="selectedPublications"/></h4>
                    <ul>{render-app:passthru($node, $mode, $lang)}</ul>
                </div>
            else
                <ul>{render-app:passthru($node, $mode, $lang)}</ul>
        
        default return
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:milestone($node as element(tei:milestone), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case "html" return
            let $anchor :=  if ($node/@rendition = '#dagger') then
                                '†'
                            else if ($node/@rendition = '#asterisk') then
                                '*'
                            else ()
            let $summary := if ($node/@n and not(matches($node/@n, '^[0-9\[\]]+$'))) then
                                <div class="summary_title" id="{string($node/@xml:id)}">{string($node/@n)}</div>
                            else if ($node/@n and matches($node/@n, '^[0-9\[\]]+$')) then
                                <div class="summary_title" id="{string($node/@xml:id)}">{concat($config:citationLabels($node/@unit)?('abbr'), ' ', string($node/@n))}</div>
                            (: oder das hier?:   <xsl:value-of select="key('targeting-refs', concat('#',@xml:id))[1]"/> :)
                            else ()
            return ($anchor, $summary)
        
        case 'work'
        case 'participants' return ()
        
        default return () 
};

declare function render-app:name($node as element(*), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'html'
        case 'work' return
            let $nodeType       := local-name($node)
            let $lang           := request:get-attribute('lang')
            let $getWorkId      := tokenize(tokenize($node/@ref, 'work:'  )[2], ' ')[1]
            let $getAutId       := tokenize(tokenize($node/@ref, 'author:')[2], ' ')[1]
            let $getCerlId      := tokenize(tokenize($node/@ref, 'cerl:'  )[2], ' ')[1]
            let $getGndId       := tokenize(tokenize($node/@ref, 'gnd:'   )[2], ' ')[1]
            let $getGettyId     := tokenize(tokenize($node/@ref, 'getty:' )[2], ' ')[1]
            let $key            := $node/@key
            let $unavailable    := ( let $valWorkId := sutil:WRKvalidateId($getWorkId)
                                     let $valAutId  := sutil:AUTvalidateId($getAutId)
                                     return if (max(($valWorkId, $valAutId)) < 1) then
                                         "unavailable"
                                     else
                                         ""
                                    )
            let $title          := if ($unavailable = "unavailable") then "not available yet" else $key
    
            return
               if ($getWorkId) then
                    <a class="{($nodeType || ' hi_work_'   || $getWorkId  || ' ' || $unavailable )}" href="{if ($unavailable = "unavailable") then "javascript: void(0)" else concat($config:idserver, '/texts/', $getWorkId)}"  title="{$title}">{render-app:passthru($node, $mode, $lang)}</a>
               else if ($getAutId) then
                    <a class="{($nodeType || ' hi_author_' || $getAutId   || ' ' || $unavailable)}"  href="{if ($unavailable = "unavailable") then "javascript: void(0)" else concat($config:idserver, '/persons/', $getAutId)}" title="{$title}">{render-app:passthru($node, $mode, $lang)}</a>
                else if ($getCerlId) then 
                    <a class="{($nodeType || ' hi_cerl_'   || $getCerlId  || ' external_link')}"     href="{('http://thesaurus.cerl.org/cgi-bin/record.pl?rid=' || $getCerlId)}" title="{$title}" target="_blank" >{render-app:passthru($node, $mode, $lang)}</a>
                else if ($getGndId) then 
                    <a class="{($nodeType || ' hi_gnd_'    || $getGndId   || ' external_link')}"     href="{('http://d-nb.info/' || $getGndId)}" title="{$title}" target="_blank" >{render-app:passthru($node, $mode, $lang)}</a>
                else if ($getGettyId) then 
                    <a class="{($nodeType || ' hi_getty_'  || $getGettyId || ' external_link')}"     href="{('http://www.getty.edu/vow/TGNFullDisplay?find=&amp;place=&amp;nation=&amp;english=Y&amp;subjectid=' || $getGettyId)}" title="{$title}" target="_blank" >{render-app:passthru($node, $mode, $lang)}</a>
                else
                    <span class="{($nodeType || ' hi_' || replace($key, " ", "_") )}">{render-app:passthru($node, $mode, $lang)}</span>
        
        case 'participants' return 
            <h4>{render-app:passthru($node, $mode, $lang)}</h4>
        
        default return
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:nameNode($node as element(tei:name), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'participants' return 
            if ($node/@type = ('org', 'place')) then
                <span>{render-app:passthru($node, $mode, $lang)}</span>
            else ()
        default return 
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:note($node as element(tei:note), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'html'
        case 'work' return
            let $normalizedString := normalize-space(string-join(render-app:passthru($node, $mode, $lang), ' '))
            let $noteLength := string-length($normalizedString)

            let $identifier       := $node/@xml:id
            let $anchor           := if (not($node/@n)) then
                                        string(count($node/preceding::tei:note[not(@n)][ancestor::tei:body])+1)
                                     else
                                        $node/@n/string()
            return
                (<sup>{$anchor}</sup>,
                <span class="marginal note" id="note_{$identifier}">
                    {if ($noteLength gt $config:chars_summary) then
                        (<a class="{string-join(distinct-values(for $key in $node//@sortKey | $node//@ref return concat('hi_', translate(translate(tokenize($key, ' ')[1], ':', '_'), ' ', '_'))), ' ')}" data-toggle="collapse" data-target="#subdiv_{$identifier}">{concat($anchor, ' ', substring($normalizedString, 1, $config:chars_summary), '…')}<i class="fa fa-angle-double-down"/></a>,<br/>,
                         <span class="collapse" id="subdiv_{$identifier}">{render-app:passthru($node, $mode, $lang)}</span>)
                     else
                        <span><sup>{$anchor} </sup>{render-app:passthru($node, $mode, $lang)}</span>
                    }
                </span>)
        
        case 'participants' return 
            if ($node/@type eq 'desc') then
                <div>{render-app:passthru($node, $mode, $lang)}</div>
            else <span>{render-app:passthru($node, $mode, $lang)}</span>
        
        default return
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:num($node as element(tei:num), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'participants' return 
            switch($node/@type/string())
                case 'phone' return
                    <span><i18n:text key="phoneAbbr"/>{(': ', render-app:passthru($node, $mode, $lang))}</span>
                case 'fax' return
                    <span><i18n:text key="faxAbbr"/>{(': ', render-app:passthru($node, $mode, $lang))}</span>
                default return ()
        default return 
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:orgName($node as element(tei:orgName), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'participants' return
            render-app:passthru($node, $mode, $lang)
        default return
            render-app:name($node, $mode, $lang)
};

declare function render-app:orig($node as element(tei:orig), $mode as xs:string, $lang as xs:string?) {
    render-app:origElem($node, $mode, $lang)
};

declare function render-app:origElem($node as element(), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'html'
        case 'work' return
            let $editedString := render-app:dispatch($node/parent::tei:choice/(tei:expan|tei:reg|tei:corr), "edit", $lang)
            return  if ($node/parent::tei:choice) then
                        <span class="original {local-name($node)} unsichtbar" title="{string-join($editedString, '')}">
                            {render-app:passthru($node, $mode, $lang)}
                        </span>
                    else
                        render-app:passthru($node, $mode, $lang)
        
        case 'snippets-orig' return
            render-app:passthru($node, $mode, $lang)
        
        case 'snippets-edit' return
            if (not($node/(preceding-sibling::tei:expan|preceding-sibling::tei:reg|preceding-sibling::tei:corr|following-sibling::tei:expan|following-sibling::tei:reg|following-sibling::tei:corr))) then
                render-app:passthru($node, $mode, $lang)
            else ()
        case 'participants' return ()
        default return
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:p($node as element(tei:p), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        
        case 'participants' return 
            <p>{render-app:passthru($node, $mode, $lang)}</p>
        
        case 'html' return
            if ($node/ancestor::tei:note) then
                render-app:passthru($node, $mode, $lang)
            else
                <p class="hauptText" id="{$node/@xml:id}">
                    {render-app:passthru($node, $mode, $lang)}
                </p>
        
        case 'work' return   (: the same as in html mode except for distinguishing between paragraphs in notes and in the main text. In the latter case, make them a div, not a p and add a tool menu. :)
            if ($node/parent::tei:note) then
                render-app:passthru($node, $mode, $lang)
            else
                <p class="hauptText" id="{$node/@xml:id}">
                    {render-app:passthru($node, $mode, $lang)}
                </p>
                
        default return
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:pb($node as element(tei:pb), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'html'
        case 'work' 
        case 'snippets-orig' 
        case 'snippets-edit' return
            if (not($node/@break = 'no')) then
                ' '
            else ()
        
        case 'participants' return ()
        
        default return () (: some sophisticated function to insert a pipe and a pagenumber div in the margin :)
};

declare function render-app:persName($node as element(tei:persName), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'participants' return
            render-app:passthru($node, $mode, $lang)
        default return
            render-app:name($node, $mode, $lang)
};

declare function render-app:placeName($node as element(tei:placeName), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case 'participants' return
            render-app:passthru($node, $mode, $lang)
        default return
            render-app:name($node, $mode, $lang)
};

declare function render-app:quote($node as element(tei:quote), $mode as xs:string, $lang as xs:string?) {
    if ($mode = ("html", "work", "participants")) then
        <span class="quote">{render-app:passthru($node, $mode, $lang)}</span>
    else
        ('"', render-app:passthru($node, $mode, $lang), '"')
};

declare function render-app:ref($node as element(tei:ref), $mode as xs:string, $lang as xs:string?) {
    let $workPsgId  := tokenize(tokenize($node/@target, 'work:'  )[2], ' ')[1]
    let $workId     := tokenize($workPsgId, ':')[1]
    let $lemmaId    := tokenize(tokenize(tokenize($node/@target, 'lemma:'  )[2], ' ')[1], ':')[1]
    let $valWorkId  := sutil:WRKvalidateId($workId)
    let $valLemmaId := sutil:LEMvalidateId($lemmaId)
    let $available  :=  if (($workId and $valWorkId < 1) or ($lemmaId and $valLemmaId < 1)) then
                            'unavailable'
                        else ()
    let $classnames :=  if (substring($node/@target, 1, 4) = "http") then
                            "external_link"
                        else
                            let $resClass   :=  if ($workId) then
                                                    'hi_work_'   || $workId
                                                else if ($lemmaId) then
                                                    'hi_lemma_'   || $lemmaId
                                                else ()
                            let $debug      :=  if ($config:debug = ("trace") and (($workId and $valWorkId < 0) or ($lemmaId and $valLemmaId < 0))) then
                                                    if ($workId) then
                                                        console:log("[Render] invalid $workId: " || $workId || " (validate: " || $valWorkId || ").")
                                                    else if ($lemmaId) then
                                                        console:log("[Render] invalid $lemmaId: " || $lemmaId || " (validate: " || $valLemmaId || ").")
                                                    else
                                                        console:log("[Render] ?? invalid ?? $workId: " || $workId || " (validate: " || $valWorkId || "), $lemmaId: " || $lemmaId || " (validate: " || $valLemmaId || ").")
                                                else ()
                            return string-join(($resClass, $available), " ")
    let $targetURL :=   if ($available = "unavailable") then
                            "javascript: void(0)"
                        else if ($workId) then
                            concat($config:idserver, '/texts/', $workPsgId)
                        else if ($lemmaId) then
                            concat($config:idserver, '/texts/', $lemmaId)
                        else
                            $node/@target
    let $key        := if ($node/@key) then $node/@key/string() else ""
    let $title      := if ($available = "unavailable") then "not available yet" else $key
    let $newTab     := boolean($classnames = "external_link")

    return

        if (($mode = ("html", "participants") and $node/@type = "url") or $mode = "work") then
            if ($newTab) then
                <a class="{$classnames}" href="{$targetURL}" title="{$title}" target="_blank">{render-app:passthru($node, $mode, $lang)}</a>
            else
                <a class="{$classnames}" href="{$targetURL}" title="{$title}">{render-app:passthru($node, $mode, $lang)}</a>
        else
            render-app:passthru($node, $mode, $lang)
};

declare function render-app:reg($node as element(tei:reg), $mode as xs:string, $lang as xs:string?) {
    render-app:editElem($node, $mode, $lang)
};

declare function render-app:sic($node as element(tei:sic), $mode as xs:string, $lang as xs:string?) {
    render-app:origElem($node, $mode, $lang)
};

declare function render-app:soCalled($node as element(tei:soCalled), $mode as xs:string, $lang as xs:string?) {
    if ($mode = ("html", "work")) then
        <span class="soCalled">{render-app:passthru($node, $mode, $lang)}</span>
    else if ($mode eq 'participants') then render-app:passthru($node, $mode, $lang)
    else
        ("'", render-app:passthru($node, $mode, $lang), "'")
};

declare function render-app:title($node as element(tei:title), $mode as xs:string, $lang as xs:string?) {
    switch($mode)
        case "html"
        case "work" return
            if ($node/@ref) then
                render-app:makeLink($node, $mode, $lang, string($node/@ref), string($node/@n))
            else
                 <span class="title">{render-app:passthru($node, $mode, $lang)}</span>
        case 'participants' return ()
        default return
            render-app:passthru($node, $mode, $lang)
};
