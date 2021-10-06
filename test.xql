xquery version "3.1";

declare namespace output  = "http://www.w3.org/2010/xslt-xquery-serialization";
declare namespace request = "http://exist-db.org/xquery/request";
declare namespace sal     = "http://salamanca.adwmainz.de";
declare namespace tei     = "http://www.tei-c.org/ns/1.0";
declare namespace util    = "http://exist-db.org/xquery/util";

import module namespace console    = "http://exist-db.org/xquery/console";

import module namespace config = "http://www.salamanca.school/xquery/config" at "xmldb:exist:///db/apps/salamanca/modules/config.xqm";

declare option output:media-type "text/html";
declare option output:method "xhtml";

let $rid    := request:get-parameter('rid',     'W0002')

let $path        := $config:tei-works-root || '/' || $rid || ".xml"
let $dok         := if (doc-available($path)) then
                        doc($path)
                    else
                        error(xs:QName("sal:PerfTest"), "Document not found for " || $rid || ".")

let $debug       := console:log("[PerfTest]: Document " || util:document-name($dok) || ", containing " || count($dok//@xml:id) || " nodes with @xml:id. (Expanding...)")
let $document    := $dok
let $debug       := console:log("[PerfTest]: Document containing " || count($document//@xml:id) || " nodes with @xml:id.")
let $start-time0 := util:system-time()

let $debug       := console:log("[PerfTest]: Approach 1...")
let $start-time1 := util:system-time()
let $results1    := for $i in (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    let $count1      := count($document//*[substring(@xml:id, 8) eq "W0002-00"])
    return $count1
let $runtime-ms1 := ((util:system-time() - $start-time1) div xs:dayTimeDuration('PT1S'))  * 1000

let $debug       := console:log("[PerfTest]: Approach 2...")
let $start-time2 := util:system-time()
let $results2    := for $i in (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    let $count2      := count($document//*[@xml:id/contains(., "W0002-00")])
    return $count2
let $runtime-ms2 := ((util:system-time() - $start-time2) div xs:dayTimeDuration('PT1S'))  * 1000

let $debug       := console:log("[PerfTest]: Approach 3...")
let $start-time3 := util:system-time()
let $results3    := for $i in (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    let $count3      := count($document//*[contains(@xml:id, "W0002-00")])
    return $count3
let $runtime-ms3 := ((util:system-time() - $start-time3) div xs:dayTimeDuration('PT1S'))  * 1000

let $debug       := console:log("[PerfTest]: Approach 4...")
let $start-time4 := util:system-time()
let $results4    := for $i in (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    let $count4      := count($document//@xml:id[contains(., "W0002-00")]/parent::node())
    return $count4
let $runtime-ms4 := ((util:system-time() - $start-time4) div xs:dayTimeDuration('PT1S'))  * 1000

let $debug       := console:log("[PerfTest]: Approach 5...")
let $start-time5 := util:system-time()
let $results5    := for $i in (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    let $count5      := count($document//*[starts-with(@xml:id, "W0002-00")])
    return $count5
let $runtime-ms5 := ((util:system-time() - $start-time5) div xs:dayTimeDuration('PT1S'))  * 1000

let $debug       := console:log("[PerfTest]: Approach 6...")
let $start-time6 := util:system-time()
let $results6    := for $i in (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    let $count6      := count($document//*[@xml:id/starts-with(., "W0002-00")])
    return $count6
let $runtime-ms6 := ((util:system-time() - $start-time6) div xs:dayTimeDuration('PT1S'))  * 1000

let $debug       := console:log("[PerfTest]: Approach 7...")
let $start-time7 := util:system-time()
let $results7    := for $i in (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    let $count7      := count($document//*[matches(@xml:id, "W0002-00")])
    return $count7
let $runtime-ms7 := ((util:system-time() - $start-time7) div xs:dayTimeDuration('PT1S'))  * 1000

let $debug       := console:log("[PerfTest]: Approach 8...")
let $start-time8 := util:system-time()
let $results8    := for $i in (1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    let $count8      := count($document//*[@xml:id/matches(., "W0002-00")])
    return $count8
let $runtime-ms8 := ((util:system-time() - $start-time8) div xs:dayTimeDuration('PT1S'))  * 1000


let $debug       := console:log("[PerfTest]: Done. Reporting...")
let $runtime-ms0 := ((util:system-time() - $start-time0) div xs:dayTimeDuration('PT1S'))  * 1000
return
<div>
<h1>Performance of String comparisons</h1>

<p>Document {util:document-name($document)}, containing {count($document//@xml:id)} nodes with @xml:id.</p>

<p>Run 1 (*[substring(@xml:id, 8) eq "W0002-00"]):<br/>
$results: ({$results1})<br/>
$runtime-1: {
  if ($runtime-ms1 < (1000 * 60)) then format-number($runtime-ms1 div 1000, "#.##") || " Sek."
  else if ($runtime-ms1 < (1000 * 60 * 60)) then format-number($runtime-ms1 div (1000 * 60), "#.##") || " Min."
  else format-number($runtime-ms1 div (1000 * 60 * 60), "#.##") || " Std."
}
</p>

<p>Run 2 (*[@xml:id/contains(., "W0002-00")]):<br/>
$results: ({$results2})<br/>
$runtime-2: {
  if ($runtime-ms2 < (1000 * 60)) then format-number($runtime-ms2 div 1000, "#.##") || " Sek."
  else if ($runtime-ms2 < (1000 * 60 * 60)) then format-number($runtime-ms2 div (1000 * 60), "#.##") || " Min."
  else format-number($runtime-ms2 div (1000 * 60 * 60), "#.##") || " Std."
}
</p>

<p>Run 3 (*[contains(@xml:id, "W0002-00")]):<br/>
$results: ({$results3})<br/>
$runtime-3: {
  if ($runtime-ms3 < (1000 * 60)) then format-number($runtime-ms3 div 1000, "#.##") || " Sek."
  else if ($runtime-ms3 < (1000 * 60 * 60)) then format-number($runtime-ms3 div (1000 * 60), "#.##") || " Min."
  else format-number($runtime-ms3 div (1000 * 60 * 60), "#.##") || " Std."
}
</p>

<p>Run 4 (@xml:id[contains(., "W0002-00")]/parent::node()):<br/>
$results: ({$results4})<br/>
$runtime-4: {
  if ($runtime-ms4 < (1000 * 60)) then format-number($runtime-ms4 div 1000, "#.##") || " Sek."
  else if ($runtime-ms4 < (1000 * 60 * 60)) then format-number($runtime-ms4 div (1000 * 60), "#.##") || " Min."
  else format-number($runtime-ms4 div (1000 * 60 * 60), "#.##") || " Std."
}
</p>

<p>Run 5 (*[starts-with(@xml:id, "W0002-00")]):<br/>
$results: ({$results5})<br/>
$runtime-5: {
  if ($runtime-ms5 < (1000 * 60)) then format-number($runtime-ms5 div 1000, "#.##") || " Sek."
  else if ($runtime-ms5 < (1000 * 60 * 60)) then format-number($runtime-ms5 div (1000 * 60), "#.##") || " Min."
  else format-number($runtime-ms5 div (1000 * 60 * 60), "#.##") || " Std."
}
</p>

<p>Run 6 (*[@xml:id/starts-with(., "W0002-00")]):<br/>
$results: ({$results6})<br/>
$runtime-6: {
  if ($runtime-ms6 < (1000 * 60)) then format-number($runtime-ms6 div 1000, "#.##") || " Sek."
  else if ($runtime-ms6 < (1000 * 60 * 60)) then format-number($runtime-ms6 div (1000 * 60), "#.##") || " Min."
  else format-number($runtime-ms6 div (1000 * 60 * 60), "#.##") || " Std."
}
</p>

<p>Run 7 (*[matches(@xml:id, "W0002-00")]):<br/>
$results: ({$results7})<br/>
$runtime-7: {
  if ($runtime-ms7 < (1000 * 60)) then format-number($runtime-ms7 div 1000, "#.##") || " Sek."
  else if ($runtime-ms7 < (1000 * 60 * 60)) then format-number($runtime-ms7 div (1000 * 60), "#.##") || " Min."
  else format-number($runtime-ms7 div (1000 * 60 * 60), "#.##") || " Std."
}
</p>

<p>Run 8 (*[@xml:id/matches(., "W0002-00")]):<br/>
$results: ({$results8})<br/>
$runtime-8: {
  if ($runtime-ms8 < (1000 * 60)) then format-number($runtime-ms8 div 1000, "#.##") || " Sek."
  else if ($runtime-ms8 < (1000 * 60 * 60)) then format-number($runtime-ms8 div (1000 * 60), "#.##") || " Min."
  else format-number($runtime-ms8 div (1000 * 60 * 60), "#.##") || " Std."
}
</p>


<p>Total computing time: {      
  if ($runtime-ms0 < (1000 * 60)) then format-number($runtime-ms0 div 1000, "#.##") || " Sek."
  else if ($runtime-ms0 < (1000 * 60 * 60)) then format-number($runtime-ms0 div (1000 * 60), "#.##") || " Min."
  else format-number($runtime-ms0 div (1000 * 60 * 60), "#.##") || " Std."
}
</p>
</div>