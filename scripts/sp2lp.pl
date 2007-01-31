
use File::Find;
use File::Copy;


# SETTINGS
$modif = 1;

#**********************************************************
$find_file_name = '';
$find_file_path = '';
%find_file_map =
(
"sp_defs.h" => "sp_defs.h",
"error_codes.h" => "kernel/common/errdbg/error_codes.h",
"expat.h" => "expat/expat.h",
"chicken.h" => "chicken/chicken.h",
"pcre.h" => "pcre/pcre.h",
"pcre_matcher.h" => "pcre/pcre_matcher.h",
"pcre_matcher_base.h" => "pcre/pcre_matcher_base.h",
"pcre_pattern.h" => "pcre/pcre_pattern.h",
"ucp.h" => "pcre/ucp.h",
"AParser.h" => "pg/h/AParser.h",
"ASTBase.h" => "pg/h/ASTBase.h",
"AToken.h" => "pg/h/AToken.h",
"ATokenBuffer.h" => "pg/h/ATokenBuffer.h",
"ATokenStream.h" => "pg/h/ATokenStream.h",
"ATokPtr.h" => "pg/h/ATokPtr.h",
"DLexer.h" => "pg/h/DLexer.h",
"DLexerBase.h" => "pg/h/DLexerBase.h",
"pccts_setjmp.h" => "pg/h/pccts_setjmp.h",
"pccts_stdarg.h" => "pg/h/pccts_stdarg.h",
"pccts_stdio.h" => "pg/h/pccts_stdio.h",
"pccts_stdlib.h" => "pg/h/pccts_stdlib.h",
"pccts_string.h" => "pg/h/pccts_string.h",
"PCCTSAST.h" => "pg/h/PCCTSAST.h",
"pcctscfg.h" => "pg/h/pcctscfg.h",
"SASTBase.h" => "pg/h/SASTBase.h",
"STreeParser.h" => "pg/h/STreeParser.h",
"PPCreateTrigger.h" => "kernel/tr/executor/root/PPCreateTrigger.h",
"PPDropTrigger.h" => "kernel/tr/executor/root/PPDropTrigger.h",
"numeric_operations.h" => "kernel/tr/executor/fo/numeric_operations.h",
"sym_engine.h" => "sym_engine.h",
"XQuerytokens.h" => "kernel/tr/xqp/XQuerytokens.h",
"XQueryTreeParser.h" => "kernel/tr/xqp/XQueryTreeParser.h",
"XQueryDLGLexer.h" => "kernel/tr/xqp/XQueryDLGLexer.h",
"XQueryParser.h" => "kernel/tr/xqp/XQueryParser.h",
"dtsearch.h" => "dtsearch/include/dtsearch.h"
);

# Bugs in source code
# btree/test_helpers.cpp
#   time.h
# btree/test_overall.cpp
#   stdlib.h
#   time.h
# nid/test_nbm.cpp
#   stdlib.h
#   time.h
# pstr/test.cpp
#   stdlib.h
#   time.h

sub find_file_sub
{
  if (-f && $_ eq $find_file_name)
  {
    $find_file_path = $File::Find::name;
  }

}

# two params: file to find, where to find
# returns: path to file
sub find_file
{
  $find_file_name=$_[0];
  $find_file_path='';
  $find_file_f = $find_file_map{$find_file_name};
  return $find_file_f if $find_file_f;

  find(\&find_file_sub, ($_[1]));
  if ($find_file_path eq '')
  {
    #return $find_file_map{$find_file_name} if $find_file_map{$find_file_name};
    die "File $_[0] was not found";
    #print "!!! $_[0]\n";
  }
  else
  {
    $find_file_map{$find_file_name} = $find_file_path;
  }
  return $find_file_path;
}
#**********************************************************
#print find_file('PPBase.h', 'sedna/kernel');


#**********************************************************
# two params: 
#   file (to which one we should find path)
#   where to find
# return: relative file path from folder passed as second param
sub get_path
{
  #print "0: $_[0]\n1: $_[1]\n";
  @get_path_base_list = split(/\//, $_[1]);
  $found_file = find_file($_[0], $_[1]);
  @get_path_list = split(/\//, $found_file);
  return join('/', @get_path_list[$#get_path_base_list+1..$#get_path_list]);
}
#**********************************************************
#print get_path('PPBase.h', 'sedna/kernel');

#**********************************************************
$dir_path = '';

sub process_file
{
  if (-f)
  {
    if (?^.+\.old$?)
    {
      reset;
    }
    elsif (?^.+\.new$?)
    {
      reset;
    }
    else
    {
      print "Processing $File::Find::name...\n";
      print "           $File::Find::dir...\n";
      $filename = $_;
      open($INP, "< $_") || die "File not found";
      open($OUTP, "> $_.new") || die "Could not open file for writing" if $modif;
      while (<$INP>)
      {
        if (?^#include \"(.+)\"(.*)$?)
        {
          $res = get_path($1, $dir_path);
          print "    ", $1, " -> ", $res, "\n";
          print { $OUTP } "#include \"", $res, "\"$2\n" if $modif;
          reset;
        }
        else
        {
          print { $OUTP } $_ if $modif;
        }
      }
      close($INP);
      close($OUTP) if $modif;

      copy("$filename", "$filename.old") || die "Could not copy file: $!" if $modif;
      move("$filename.new", "$filename") || die "Could not move file: $!" if $modif;
    }
  }
}
#**********************************************************

$dir_path = '.';
find({"wanted" => \&process_file, "no_chdir" => 1}, ($dir_path));

