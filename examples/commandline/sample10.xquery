(:
  Deletes John Smith from auction description
:)

UPDATE
delete document("auction")/site/people/person[name/text()="John Smith"]
