(:
   Group customers by their
   income and output the cardinality of each group.
:)

<result>
 <preferred>{
   count(doc("auction")/site/people/person/profile[@income >= 100000])
 }</preferred>
 <standard>{
   count(doc("auction")/site/people/person/profile[@income < 100000
                                                   and @income >= 30000])
 }</standard>
 <challenge>{
   count(doc("auction")/site/people/person/profile[@income < 30000])
 }</challenge>
 <na>{
   count(for $p in doc("auction")/site/people/person
         where  empty($p/@income)
         return $p)
 }</na>
</result>
