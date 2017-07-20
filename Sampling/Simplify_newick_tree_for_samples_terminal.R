#requires phytools package
library("phytools", lib.loc="~/pathway/to/package/directory")

args=(commandArgs(TRUE))

# test if there is at least one argument: if not, return an error
if (length(args)<2) {
  stop("At least two arguments must be supplied (input file).n", call.=FALSE)
}




#Pathway to newick file
tree_location <- args[1]
  
tree <- read.tree(tree_location)

#List of sampled tips
sample_tips_file <- args[2]

sample_tips <- read.table(sample_tips_file, header = FALSE)

tips <- c(1:length(tree$tip.label))

tips <- tips [! tips %in% sample_tips[[1]]]

tree_output <- paste(tree_location, "_sampled",  sep = "")


tree <- drop.tip(tree, tips)

write.tree(tree, file = tree_output)