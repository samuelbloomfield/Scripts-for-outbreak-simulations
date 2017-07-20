use warnings;
use strict;

#Pathway to nexus file
my $fileA='/Pathway/to/Output.nexus_altered_extended_root';

open FILE1, $fileA or die "FILE $fileA NOT FOUND - $!\n";

my $outfileA = ($fileA . '_altered');
my $line;

my @altered_linesA;

foreach $_ (<FILE1>) {
	if ($_ =~ /tree\s+TREE\d+/) {
		$line = $_;
		$line =~ s/tree\s+TREE\d+\s+=\s+\[&R\]//g;
		$line =~ s/\[&type="//g;
		$line =~ s/"\]//g;
		push @altered_linesA, $line;
	}
}

close FILE1;

open(OUT, ">$outfileA");
print OUT join("", @altered_linesA);
close OUT;
