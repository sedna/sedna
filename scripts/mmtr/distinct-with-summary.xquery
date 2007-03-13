fn:distinct-values(
for $i in fn:doc("mt")//block
let $i_file := $i/file
let $i_line := $i/line
let $k := for $j in fn:doc("mt")//block
	  	  where $j/file eq $i_file and $j/line eq $i_line
		  return $j/size
let $new_block := 
	<block>{
		$i_file,
		"-",
		$i_line,
		"-",
		<sum>{fn:sum($k)}</sum>,
		"-",
		<count>{count($k)}</count>,
		"-",
		$i/flag
	}</block>
order by xs:integer($new_block/sum) descending, xs:integer($new_block/line)
return $new_block)
