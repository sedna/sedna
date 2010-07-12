(:
  Inserts a new person description into the auction
:)

UPDATE
insert <person id="person25">
        <name>John Smith</name>
        <phone>223-322-223-322</phone>
        <creditcard>3454 3656 2344 6767</creditcard>
       </person>
into doc("auction")/site/people
