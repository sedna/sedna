(:
   List the names of items registered in Australia along with 
   their descriptions.
:)

for $i in document("auction")/site/regions/australia/item
return element item {
        attribute name {$i/name/text()},
        $i/description
       }
