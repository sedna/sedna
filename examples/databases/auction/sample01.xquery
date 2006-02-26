(:
   Return the name of the person with ID `person0'
   registered in North America.
:)

for    $b in document("auction")/site/people/person[@id="person0"]
return $b/name/text()
