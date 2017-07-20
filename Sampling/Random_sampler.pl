use warnings;
use strict;
use POSIX;

#Pathway to nexus file
my $wkdir='/Pathway/to/directory/containing/nexus/file/';

#Name of nexus and newick file
my $fileA=($wkdir . 'Outbreak_simulations.nexus');
my $newick = ($wkdir . 'Outbreak_simulations.newick');

#Pathway to Rscript.exe
my $Rscript = '/Pathway/to/Rscript.exe';

#Pathway to Simplify_newick_tree_for_samples_terminal.R
my $newick_simplify = '/Pathway/to/Simplify_newick_tree_for_samples_terminal.R';

#Pathway to directory containing beast.jar
my $BEAST2 = ('/Pathway/to/beast.jar');

#Number of SNPs to simulate
my $SNP_number = 800;

#Number of extra animals
my $animal_extra = 0;

#Number of extra humans
my $human_extra = 0; 

#Total number of samples
my $total_number = 100;

my $outfileB = ($wkdir . 'Samples.txt');
my $outfileA=($wkdir . 'Outbreak_isolates.txt'); 
my $final_fasta = ($wkdir . "Output.fasta");

my $newick_altered = ($newick . '_sampled');

my $sequence_simulation = ($wkdir . 'Sequence_simulation.xml');
my $sequence_simulation_altered = ($wkdir . 'Sequence_simulation_altered.xml');


my $line;
my $line_A;

open FILE1, $fileA or die "FILE $fileA NOT FOUND - $!\n";


my @isolate = ();
my @source = ();
my @time = ();

my @animal_numbers = ();
my @human_numbers = ();

my @chosen_isolate = ();
my @chosen_source = ();
my @chosen_time = ();

my @isolate_animal=();
my @source_animal=();
my @time_animal=();

my @isolate_human=();
my @source_human=();
my @time_human=();

my $animal_number;
my $human_number;

#Subroutine that returns the unique strings in an array 
sub uniq {
	my %seen;
	grep !$seen{$_}++, @_;
}

#Loop that extracts isolate information from nexus file 
foreach $_ (<FILE1>) {
	if ($_ =~ /tree\s\w+\s=\s/) {
		$line = $_;
		while ($line =~ m/(\d+)\[&type="([A-Za-z]+)_I",reaction="\w+",time=(\d+\.\d+)\]/g) {
			push @isolate, $1;
			push @source, $2;
			push @time, $3;
		}
	}
}

#Loop that splits isolate information by source
for (my $i=0; $i < (scalar @source); $i++) {
	if($source[$i] eq "Animal"){
		push @isolate_animal, $isolate[$i];
		push @source_animal, $source[$i];
		push @time_animal, $time[$i];
	} elsif ($source[$i] eq "Human") {
		push @isolate_human, $isolate[$i];
		push @source_human, $source[$i];
		push @time_human, $time[$i];
	}
}

#Section of script that calculates the amount of animal and human isolates to be sampled
$animal_number = sprintf "%.0f", ((scalar(@source_animal)/scalar(@source))*$total_number);
$animal_number = $animal_number + $animal_extra - $human_extra;

if ($animal_number > $total_number){
	$animal_number = $total_number;
}

if ($animal_number < 0){
	$animal_number = 0;
}

$human_number = $total_number - $animal_number;

close FILE1;


print "\n";

#Loop that prints out all isolates information
open(OUT, ">$outfileA");
for (my $i=0; $i < (scalar @isolate); $i++) {
	print OUT $isolate[$i];
	print OUT "\t";
	print OUT $source[$i];
	print OUT "\t";
	print OUT $time[$i];
	print OUT "\n";
}

#Loop that sample the outbreak after stratifying by source
until((scalar(@animal_numbers))> ($animal_number-1)) {
	push @animal_numbers, (int(rand(scalar(@isolate_animal))));
	@animal_numbers = uniq(@animal_numbers);
}

until((scalar(@human_numbers))> ($human_number-1)) {
	push @human_numbers, (int(rand(scalar(@isolate_human))));
	@human_numbers = uniq(@human_numbers);
}

for (my $i=0; $i < (scalar @animal_numbers); $i++) {
	push @chosen_isolate, $isolate_animal[$animal_numbers[$i]];
	push @chosen_source, $source_animal[$animal_numbers[$i]];
	push @chosen_time, $time_animal[$animal_numbers[$i]];
}

for (my $i=0; $i < (scalar @human_numbers); $i++) {
	push @chosen_isolate, $isolate_human[$human_numbers[$i]];
	push @chosen_source, $source_human[$human_numbers[$i]];
	push @chosen_time, $time_human[$human_numbers[$i]];
}

print OUT "\n\n";

#Section of script that sorts isolates
my @index2 = sort { $chosen_isolate[$a] <=> $chosen_isolate[$b] } 0 .. $#chosen_isolate;
@chosen_isolate = @chosen_isolate[@index2];
@chosen_source = @chosen_source[@index2];
@chosen_time = @chosen_time[@index2];

#Loop that prints out sampled isolates information
for (my $i=0; $i < (scalar @chosen_isolate); $i++) {
	print OUT $chosen_isolate[$i];
	print OUT "\t";
	print OUT $chosen_source[$i];
	print OUT "\t";
	print OUT $chosen_time[$i];
	print OUT "\n";
}

close OUT;

#Section of script that prints sampled isolate numbers to a separate file
open(OUTB, ">$outfileB");
print OUTB join("\n", @chosen_isolate);
close OUTB;

#Command that calls Simplify_newick_tree_for_samples_terminal.R to simplify newick tree for the sampled isolates
system "$Rscript $newick_simplify $newick $outfileB";

#Loop that modifies sampled newick for sequence simulations
open FILE2, $newick_altered or die "FILE $newick_altered NOT FOUND - $!\n";
foreach $_ (<FILE2>) {
	if ($_ =~ /^(.*?\d+.*?)$/) {
		$line = $1;
	}
}
close FILE2;

#Section of script that writes an xml file to simulate sequences
open(OUTC, ">$sequence_simulation");
print OUTC '<?xml version="1.0"?>';
print OUTC "\n";
print OUTC '<beast namespace="beast.core :beast.core.parameter :beast.evolution.sitemodel :beast.evolution.substitutionmodel :beast.evolution.alignment :beast.app.seqgen" version="2.0">';
print OUTC "\n";
print OUTC '<run outputFileName="';
print OUTC "$sequence_simulation_altered";
print OUTC '" sequencelength="5000" spec="SequenceSimulator">';
print OUTC "\n";
print OUTC '<data spec="Alignment" dataType="nucleotide" id="input_alignment">';
print OUTC "\n";
for (my $i=0; $i < (scalar @chosen_isolate); $i++) {
	print OUTC '<sequence spec="Sequence" value="?" taxon="';
	print OUTC $chosen_isolate[$i];
	print OUTC '"/>';
	print OUTC "\n";
}
print OUTC '</data>';
print OUTC "\n";
print OUTC '<tree spec="beast.util.TreeParser" adjustTipHeights="false" IsLabelledNewick="true"  newick="';
print OUTC $line;
print OUTC '"/>';
print OUTC "\n";
print OUTC '<siteModel spec="SiteModel" gammaCategoryCount="4">';
print OUTC "\n";
print OUTC '<mutationRate spec="RealParameter" value="0.005"/>';
print OUTC "\n";
print OUTC '<shape spec="RealParameter" value="14.5"/>';
print OUTC "\n";
print OUTC '<proportionInvariant spec="RealParameter" value="0.0014"/>';
print OUTC "\n";
print OUTC '<substModel spec="GTR">';
print OUTC "\n";
print OUTC '<rateAC spec="RealParameter" value="0.338"/>';
print OUTC "\n";
print OUTC '<rateAG spec="RealParameter" value="0.909"/>';
print OUTC "\n";
print OUTC '<rateAT spec="RealParameter" value="0.21"/>';
print OUTC "\n";
print OUTC '<rateCG spec="RealParameter" value="0.18"/>';
print OUTC "\n";
print OUTC '<rateCT spec="RealParameter" value="1.0"/>';
print OUTC "\n";
print OUTC '<rateGT spec="RealParameter" value="0.408"/>';
print OUTC "\n";
print OUTC '<frequencies spec="Frequencies">';
print OUTC "\n";
print OUTC '<frequencies spec="RealParameter" value="0.25 0.25 0.25 0.25"/>';
print OUTC "\n";
print OUTC '</frequencies>';
print OUTC "\n";
print OUTC '</substModel>';
print OUTC "\n";
print OUTC '</siteModel>';
print OUTC "\n";
print OUTC '</run>';
print OUTC "\n";
print OUTC '</beast>';
print OUTC "\n";
close OUTC;

#Command that runs Sequence simulation xml script in BEAST2
system "java -jar $BEAST2 $sequence_simulation";


#Section of script that reformats the simulated sequence file
open FILE4, $sequence_simulation or die "FILE $sequence_simulation NOT FOUND - $!\n";

my @taxon=();
my $taxon_count = 0;
my $line2;

foreach $_ (<FILE4>) {
	$line2 = $_;
	if ($line2 =~ /sequence\sspec="Sequence"\svalue="\?"\staxon="(\d+)"/) {
		push @taxon, $1;
		$taxon_count++;
	}
}

close FILE4;

open FILE3, $sequence_simulation_altered or die "FILE $sequence_simulation_altered NOT FOUND - $!\n";

my $sequence_simulation_altered_altered = ($sequence_simulation_altered . '_altered');

my @altered_lines;
my $count=0;

foreach $_ (<FILE3>) {
	if ($_ =~ /sequence\staxon/) {
		$line = $_;
		$line =~ s/\s+<sequence.*?value='/>$taxon[$count]\t/g;
		$line =~ s/'\/>//g;
		push @altered_lines, $line;
		$count++;
	}
}

close FILE3;

open(OUT5, ">$sequence_simulation_altered_altered");
print OUT5 join("", @altered_lines);
close OUT5;

my @sequence_isolates=();
my @extended_sequences=();
my @locations=();

open FILE6, $sequence_simulation_altered_altered or die "FILE $sequence_simulation_altered_altered NOT FOUND - $!\n";
foreach $_ (<FILE6>) {
	if ($_ =~ />(\d+)\t([GATC]+)/) {
		push @sequence_isolates, $1;
		push @extended_sequences, $2;
	}
}
close FILE6;

#Section of script that extracts the intended number of SNPs from the simulated genetic data
my @index = sort { $sequence_isolates[$a] <=> $sequence_isolates[$b] } 0 .. $#sequence_isolates;
@sequence_isolates = @sequence_isolates[@index];
@extended_sequences = @extended_sequences[@index];

my $SNP_count = 0;
my $character_count=0;

my $A_count;
my $C_count;
my $G_count;
my $T_count;

my $line3;
my $check;

until($SNP_count > ($SNP_number-1)){
	$A_count=0;
	$C_count=0;
	$G_count=0;
	$T_count=0;
	
	foreach(@extended_sequences){
		$line3 = $_;
	
		$check =substr($line3, $character_count, 1);
		if($check=~ /A/){
			$A_count++;
		}
		if($check=~ /C/){
			$C_count++;
		}
		if($check=~ /G/){
			$G_count++;
		}
		if($check=~ /T/){
			$T_count++;
		}
	}
	
	if($A_count > 1) {
		$A_count = 1;
	}
	if($C_count > 1) {
		$C_count = 1;
	}
	if($G_count > 1) {
		$G_count = 1;
	}
	if($T_count > 1) {
		$T_count = 1;
	}
	
	if(($A_count + $C_count + $G_count + $T_count) > 1) {
		push @locations, $character_count;
		$SNP_count++;
	}
	
	$character_count++;
}

my $location_number;

open(OUTD, ">$final_fasta");

for (my $j=0; $j < (scalar @chosen_isolate); $j++) {
	print OUTD ">";
	print OUTD $chosen_isolate[$j];
	print OUTD "_";
	print OUTD $chosen_source[$j];
	print OUTD "_";
	print OUTD $chosen_time[$j];
	print OUTD "\n";
	foreach(@locations){
		$location_number = $_;
		print OUTD substr($extended_sequences[$j], $location_number, 1);
	}
	print OUTD "\n";
}

close OUTD;


#Section of script that simplified the nexus file to only include nodes common to the sampled isolates
my @r_tips;
my @k_tips;


for (my $k=0; $k < (scalar @isolate); $k++) {
	push @r_tips, ($isolate[$k] . '_' . $source[$k]);
}


for (my $l=0; $l < (scalar @chosen_isolate); $l++) {
	push @k_tips, ($chosen_isolate[$l] . '_' . $chosen_source[$l]);
}

my @r_nodes = ("Animal", "Human");

my $sample_no = scalar(@k_tips);

my %h;

@h{@k_tips} = undef;

@r_tips = grep {not exists $h{$_}} @r_tips;

open FILE5, $fileA or die "FILE $fileA NOT FOUND - $!\n";

$outfileA = ($fileA . '_altered');
my $outfileD = ($fileA . '_full');


my @altered_linesA;
my @altered_linesC;

foreach $_ (<FILE5>) {
	if ($_ =~ /tree\s\w+\s=\s/) {
		$line = $_;
		$line =~ s/tree\s\w+\s=\s//g;
		$line =~ s/(\d+)\[&type="/$1_/g;
		$line =~ s/\[&type="//g;
		$line =~ s/_\w".*?\]//g;		
		$line =~ s/\[.*?\]//g;
		$line =~ s/"\]//g;
		push @altered_linesA, $line;
	}
}

close FILE5;


open(OUT, ">$outfileA");
print OUT join("", @altered_linesA);
close OUT;

open FILE7, $fileA or die "FILE $fileA NOT FOUND - $!\n";

foreach $_ (<FILE7>) {
	if ($_ =~ /tree\s\w+\s=\s/) {
		$line = $_;
		$line =~ s/tree\s\w+\s=\s//g;
		$line =~ s/\d+\[&type="//g;
		$line =~ s/\[&type="//g;
		$line =~ s/_\w".*?\]//g;		
		$line =~ s/\[.*?\]//g;
		$line =~ s/"\]//g;
		push @altered_linesC, $line;
	}
}

close FILE7;

open(OUT8, ">$outfileD");
print OUT8 join("", @altered_linesC);
close OUT8;

my $char_1 = ',';
my $char_2 = ':';
my $char_3 = '\)+';

my @rooted;
my $final_with_root;
my $line_1;
my $line_2;
my $line_3;
my $s_length_1;
my $index_1;
my $s_length_2;
my $index_2;
my $s_length_3;
my $index_3;
my $sstring_1;
my $sstring_2;
my $sstring_3;
my $sstring_4;
my $sstring_5;
my $sstring_6;
my @indexes;
my $index;
my @indexes_A;
my $index_A;
my $index_number;
my $indexes_number;

my $b_reduced;
my $b_end;
my $b_start;
my $b_number;
my $b_length;
my $b_left;
my $b_right;
my @b_equal;
my $b_check;
my $sstring_sub;
my @b_id;
my $start_length;
my $end_length;

my $final;
my $final_adjusted;
my $final_sampled;

my @brackets;

my @altered_lines_sampled;


open FILE9, $outfileA or die "FILE $outfileA NOT FOUND - $!\n";

my $outfile = ($outfileA . '_sampled');
$outfileB = ($outfileA . '_sourced');
my $outfileC = ($outfileA . '_extended_root');

foreach $_ (<FILE9>) {
	if ($_ =~ /:/) {
		$line_1 = $_;
		foreach (@r_tips){
			$line_2 = $_;
			
			for ($line_1) {
				s/\($line_2:\d+\.\d+(E[+-]\d+)?,/\(/g;
				s/,$line_2:\d+\.\d+(E[+-]\d+)?\)/\)/g;
				s/\($line_2:\d+\.\d+(E[+-]\d+)?\)//g;
			}
			
		}
		my $condition_1 = 1;
		until ($condition_1 < 1){
			my $test1 = () = $line_1 =~ /\(\w+:\d+\.\d+E[+-]\d+,/gi;
			my $test2 = () = $line_1 =~ /,\w+:\d+\.\d+E[+-]\d+\)/gi;
			my $test3 = () = $line_1 =~ /\(\w+:\d+\.\d+E[+-]\d+\)/gi;
			my $test4 = () = $line_1 =~ /\(\w+:\d+\.\d+,/gi;
			my $test5 = () = $line_1 =~ /,\w+:\d+\.\d+\)/gi;
			my $test6 = () = $line_1 =~ /\(\w+:\d+\.\d+\)/gi;						
			my $final_test = $test1 + $test2 + $test3 + $test4 + $test5 + $test6;
			
			if ($final_test < ($sample_no + 1)) {
				$condition_1--;
			}
			
			foreach (@r_nodes){
				$line_3 = $_;

				for ($line_1) {
					s/\($line_3:\d+\.\d+(E[+-]\d+)?,/\(/g;
					s/,$line_3:\d+\.\d+(E[+-]\d+)?\)/\)/g;
					s/\($line_3:\d+\.\d+(E[+-]\d+)?\)//g;
				}
			}
		}
		$final = $line_1;
		
		$final_with_root = $final;
		
		$final_with_root =~ s/\d+_(\w+)/$1/g;
		$final_with_root =~ s/(\w+):/\[&type="$1"\]:/g;
		
		push @rooted, $final_with_root;
		
		open(OUT2, ">$outfileC");
		print OUT2 '#NEXUS';
		print OUT2 "\nBegin trees;\n";
		print OUT2 'tree TREE1 = [&R]';
		print OUT2 join("", @rooted);
		print OUT2 "\nEnd;";
		close OUT2;
	}
}

close FILE9;
