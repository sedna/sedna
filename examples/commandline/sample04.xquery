(:
   How many items are listed on all continents?
:)

for    $b in document("auction")/site/regions
return count ($b//item)
