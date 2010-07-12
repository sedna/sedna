(:
  Deletes John Smith from auction description
:)

UPDATE
delete doc("auction")/site/people/person[name/text()="John Smith"]
