# Scripts for genomic analysis

The scripts contained in this repository were used for analysing, rearranging and compiling Genomic data. 


### Prerequisites

The scripts contained in this repository consist of Perl and Python scripts. One script also also utilises the Velvet assembler:

```
Perl v5.18 or higher
BEAST v2.4 or higher
	MASTER package v5.0 or higher
R v3.2 or higher
	Cairo package v1.5 or higher
	coda package v0.19 or higher
	foreign package v0.8 or higher
	MASS package v7.3 or higher
	phytools package v0.5 or higher
```

### Installing

The scripts in this depository can be downloaded directly from GitHub:


```
git clone https://github.com/samuelbloomfield/Scripts-for-outbreak-simulations
```


## Running the scripts

Most of the scripts in this depository will have paths and arrays that will need to be re-written before they can be run:


### Outbreak simulations

The Outbreak_simulations.xml script was used to simulate outbreaks stochastically.
The Outbreak_simulations.xml script will need to be re-written to include the intended rates, susceptible population size and initially infected host population parameters.
The script will return a .json, .newick and .nexus file of the simulated outbreaks. 
The script can be run in BEAST2 with the MASTER package installed.
From the terminal the script can be run as:

```
java -jar /Pathway/to/BEAST/lib/beast.jar /Pathway/to/Outbreak_simulations.xml
```



### Sample newick trees

The Simplify_newick_tree_for_samples_terminal.R script is called by the Constant_time_random sampler.pl and Random_sampler.pl scripts to simplify a newick tree such that it only includes nodes common to sampled isolates.
The Simplify_newick_tree_for_samples_terminal.R script will need to be rewritten to include the directory containing the R packages.
The script requires a newick file and a .txt file containing a list of isolates to be sampled, and will return a sampled newick tree.
From the terminal the script can be run as:

```
/Pathway/to/Rscript.exe Simplify_newick_tree_for_samples_terminal.R /Pathway/to/newick_file /Pathway/to/text_file_containing_list_of_isolates_to_sample
```

### Random sampling

The Random_sampler.pl script extracts isolate information from the simulated .newick and .nexus files, randomly samples isolates from these simulations after stratifying for source, simplifies the .newick and .nexus trees to only include nodes common to the sampled isolates, and simulates sequence data based on the samples trees.
The Random_sampler.pl script will need to be re-written to include the directory of the .nexus and .newick files, the pathway to Rscript.exe and Simplify_newick_tree_for_samples_terminal.R, the total number of samples required, if any extra human and animal isolates should be sampled, and the total number of SNPs required.
The script will return a .fasta file containing the simulated SNP data, a Outbreak_isolates.txt file containing all information on the isolates followed by all the information on the sampled isolates, along with a large number of temporary files.
From the terminal the file can be run as:

```
perl Random_sampler.pl
```


### Constant time sampling

The Constant_time_random_sampler.pl script extracts isolate information from the simulated .newick and .nexus files, randomly samples isolates equally over time from these simulations after stratifying for source, simplifies the .newick and .nexus trees to only include nodes common to the sampled isolates, and simulates sequence data based on the samples trees.
The Constant_time_random_sampler.pl script will need to be re-written to include the directory of the .nexus and .newick files, the pathway to Rscript.exe and Simplify_newick_tree_for_samples_terminal.R, the total number of samples required, if any extra human and animal isolates should be sampled, the number of years at the beginning and end of the outbreak that should be excluded from sampling, and the total number of SNPs required.
The script will return a .fasta file containing the simulated SNP data, a Outbreak_isolates.txt file containing all information on the isolates followed by all the information on the sampled isolates, along with a large number of temporary files.
From the terminal the file can be run as:

```
perl Constant_time_random_sampler.pl 
```



### Alter sampled nexus tree for R analysis

The Convert_sampled_tree_for_R.pl script reformats the sampled nexus tree for R analysis. However, before using this script the extra nodes that make up the root may need to be removed (this will need to be performed manually).
The Convert_sampled_tree_for_R.pl script will need to be re-written to include the location sampled nexus file.
The script will return a sampled newick file that can be read by R.
From the terminal the file can be run as:

```
perl Convert_sampled_tree_for_R.pl
```

### Model comparison

The Model_comparison.R script compares the structured coalescent and discrete trait analysis models' estimates with the known parameters calculated from the samples nexus tree.
The Model_comparison.R script will need to be re-written to include the pathways to the required R packages, an array of the locations of the simulated outbreak files, the pathways to the structured coalescent and discrete trait analysis model log files within these directories, and the names of the transmission and population parameter estimates within these log files. It is best to keep the formatting of files similar between simulated outbreaks or else a lot of modifications will need to be made to this R script.
The script will form a series of tables and figures that compare the models' estimates with the known parameters and proportion of isolates from each population. 
