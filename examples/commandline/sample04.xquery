(:
   How many items are listed on all continents?
:)

for    $b in doc("auction")/site/regions
return count ($b//item)
