#!/usr/bin/perl 

sub do_current_dir_rec($);

do_current_dir_rec("");

sub mk_html($$$)
{
    my ($gcovout, $gcov, $out) = @_;

    open(GCOVOUT, "$gcovout") or die;
    open(GCOV, "$gcov") or die;
    open(OUT, ">$out") or die;

    print OUT <<EOF;
<!-- This is a generated file, do not edit -->
<html>
  <head>
    <title>Test Coverage</title>
  </head>
  <body bgcolor="#ffffff" text="#000000" link="#525D76">
  <a href="index.html">dir index</a>
  <table border="1" width="100%" cellspacing="0">
    <thead>
      <tr bgcolor=#f0f0f0>
        <td>percentage</td>
        <td>function</td>
      </tr>
    </thead>
    <tbody>
EOF


    while (<GCOVOUT>) {
        my ($percentage, $func) = m/(.*)% of [0-9]* lines executed in function (.*)/;
        last if ($percentage eq "");
        my $bgcolor;
        if ($percentage < 33) {
            $bgcolor = "#ffaaaa";
        }
        elsif ($percentage < 66) {
            $bgcolor = "#ffff77";
        }
        else {
            $bgcolor = "#aaffaa";
        }
        print OUT "      <tr bgcolor=$bgcolor><td>$percentage%</td>\n";
        print OUT "      <td>$func</td></tr>\n";
    }

    print OUT <<EOF;
    </tbody>
  </table>

  <br />

  <table border="1" width="100%" cellspacing="0" rules="groups">
    <thead>
      <tr bgcolor=#f0f0f0>
        <td>line #</td>
        <td>count</td>
        <td>source</td>
      </tr>
    </thead>
    <tbody>
EOF

    my ($lines, $covered) = (0, 0);
    while (<GCOV>) {
        my ($count, $line, $src) = m/\s+(.+):\s+(\d+):(.*)/;

        next if ($line == 0);
    
        my $bgcolor;
        if ($count eq "#####") {
	    $bgcolor = "#ffaaaa";
	    $cnt = 0;
	    $lines++;
	}
	elsif (not $count eq "-") {
	    $bgcolor = "#aaffaa";
	    $cnt = $count;
	    $lines++;
	    $covered++;
	}
	else 
	{
	    $bgcolor = "#ffffff";
	    $cnt = "";
	}
	print OUT "      <tr bgcolor=$bgcolor><td>$line</td>\n";
	print OUT "      <td>$cnt</td>";
	$src =~ s/ /&nbsp;/g;
	$src =~ s/\t/&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;/g;
	print OUT "      <td>$src</td></tr>\n";
    }

    print OUT <<EOF;
    </tbody>
  </table>

</td></tr>
</table>
 </body>
</html>
EOF

    return {
	'lines'		=> $lines,
	'covered'	=> $covered
    };
}

sub do_current_dir()
{
    my $first = 1;

    system("rm -rf coverage");

    open(FIND, "find . -name \"*.gcno\" -maxdepth 1 | sort |");
    
    @flist = ();
    while (<FIND>)
    {
        if ($first)
        {
	    $first = 0;
	    system("mkdir coverage");
        }
    
	/^(.*)\.gcno$/ or die;

        $j = "$1";
	print "Processing file '$j.gcno'...\n";
	system("gcov -f \"$j.gcno\" > \"coverage/$j.gcno.gcovout\"");
        for $ext qw(h cpp c)
	{
	    if ( -f "$j.$ext.gcov" )
	    {
		my $info = mk_html("coverage/$j.gcno.gcovout", "$j.$ext.gcov", "coverage/$j.$ext.html");
		$info->{'fn'} = "$j.$ext";
		push @flist, $info;
	    }
	}
    }
    if (! $first)
    {
	system("mv *.gcov coverage");
    }
    
    return @flist;
}

sub create_idx($)
{
    my $reldir = @_[0];
    mkdir "coverage";
    open(IDX, ">coverage/index.html") or die;
    print IDX <<EOF;
<html>
<body>
EOF
    if ($reldir)
    {
	my @dd = split /\//, $reldir;
	my $i;
	
	print IDX "<a href=\"" . "../" x ((scalar @dd) + 1) ."coverage/index.html\">root</a>\n";
	for ($i = 0; $i < scalar @dd; $i++)
	{
	    $d = $dd[$i];
	    print IDX "/<a href=\"" . "../" x ((scalar @dd) - $i) ."coverage/index.html\">$d</a>\n";
	}
    }
    print IDX <<EOF;
 <table>
EOF

    if ($reldir)
    {
	print IDX "<tr> <td> <a href=\"../../coverage/index.html\">..</a> </td></tr>";
    }
}

sub do_current_dir_rec($)
{
    my $reldir = @_[0];
    my @lst = do_current_dir();
    
    my $first = 1;
    
    my $DIRS;
    
    open($DIRS, "find . -maxdepth 1 -type d |");
    
    my @dirs = ();
    while (<$DIRS>)
    {
	chomp;
	my $dir = "$_";
	next if ($dir eq ".");
	
	$dir =~ s/^\.\///;
	
	print "entering dir $reldir/$dir\n";
	chdir "$dir" or die;
	
	if (do_current_dir_rec($reldir ? "$reldir/$dir" : $dir))
	{
	    push @dirs, $dir;
	}
	
	chdir ".." or die;
    }
    
    if ((scalar @dirs) > 0)
    {
	if ($first)
	{
	    create_idx($reldir);
	    $first = 0;
	}
    
	for $j (@dirs)
	{
	    print IDX "<tr> <td> <a href=\"../$j/coverage/index.html\">$j</a> </td></tr>";
	}
    }

    if ((scalar @lst) > 0)
    {
	if ($first)
	{
	    create_idx($reldir);
	    $first = 0;
	}
	for $inf (@lst)
	{
	    $j = $inf->{'fn'};
	    
	    $ln = $inf->{'lines'};
	    $cov = $inf->{'covered'};
	    
	    $jj = $j;
	    $jj =~ s/^\.\///;
	    
	    
	    if ($ln < 1)
	    {
		$perc = 0;
	    }
	    else
	    {
		$perc = (100*$cov)/$ln;
	    }
	    if ($perc > 70)
	    {
		$bg = "#aaffaa";
	    }
	    elsif ($perc > 30)
	    {
		$bg = "#ffffaa";
	    }
	    else
	    {
		$bg = "#ffaaaa";
	    }
	    
	    $perc = sprintf "%.02f%%", $perc;
	    
	    print IDX "<tr bgcolor=\"$bg\"> <td> <a href=\"$jj.html\">$jj</a> </td> <td>$perc covered ($cov/$ln) </td></tr>";
	}
    }

    if (! $first)
    {
	print IDX <<EOF;
 </table>
</body>
</html>
EOF
	close IDX;
    }
    
    return (! $first);
}
