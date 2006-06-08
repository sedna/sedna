#!/usr/bin/perl -w

use Cwd;

my $gc_cmd = getcwd() . "/gc.exe";

#open(FILES, "c:/cygwin/bin/find ../{kernel,driver,include,examples,term} -regex '.*\.\(c\|cpp\|h\)' |");
open(FILES, "c:/cygwin/bin/find ../{kernel,driver,include,examples,term,export} -regex '.*\\.\\(c\\|cpp\\|h\\)' |");

while (<FILES>)
{
    chomp;
    my $fn = $_;
    print "indenting $fn\n";
    
    system("dos2unix \"$fn\"");
    system("unix2dos \"$fn\"");
#    my $opts = "-space_if- -no-cmt_add_gc_tag- -no-tab_out- -cmt_dont_modify- -cmt_sep_char_split-¨ -cmt_keep_cpp- -no-cmt_add_fct_def- -no-cmt_add_fct_def_class- -no-cmt_decl- -pp_align_breakline-";
	my $opts = "-no-cmt_add_class_access- -code_align_max_blanks-1 -space_if- -no-cmt_add_gc_tag- -no-tab_out- -cmt_dont_modify- -cmt_sep_char_split-¨ -cmt_keep_cpp- -no-cmt_add_fct_def- -no-cmt_add_fct_def_class- -no-cmt_decl- -pp_align_breakline-";
    system("$gc_cmd $opts -file-$fn");
}

close FILES;
