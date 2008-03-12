(:
   How many pieces of prose are in our database?
:)

for $p in document("auction")/site
return count($p//description) + count($p//annotation) + count($p//email)
