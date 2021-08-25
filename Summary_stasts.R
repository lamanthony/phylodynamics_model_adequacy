#Summary statistics
#ours

mean_internal_node_age <- mean(node_times_sorted) / max(node_times_sorted)

node_times_sorted <- sort(intnode.times(tree), decreasing = T)
first_branching <- node_times_sorted[2] / node_times_sorted[1]

ext_int_ratio <- length(get.external.branch.length(tree))/length(get.internal.branch.length(tree))

length_numtip <- sum(tree$edge.length)/length(tree$tip.label)

mean_internal_node_age <- mean(node_times_sorted) / max(node_times_sorted)

#summary stat based on branch length
max_H <- sum(tree$edge.length)
min_H <- 
mean_branch <- mean(tree$edge.length)
median_branch <- median(tree$edge.length)
variance <- var(tree$edge.length)
e_mean_length <- mean(get.external.branch.length(tree))
e_median_length <- median(get.external.branch.length(tree))
e_var <- var(get.external.branch.length(tree))
i_mean_branch <- mean(get.internal.branch.length(tree))
i_median_branch <- median(get.internal.branch.length(tree))
i_var <- var(get.external.branch.length(tree))
ie_mean_ratio <- i_mean_branch / e_mean_length
ie_median_ratio <- i_median_branch /e_median_length
ie_var_ratio <- i_var / e_var

#summary stat based on topology
colless_index <- colless(as.treeshape(tree))
#sum for each internal node of the absolute difference between 
  #the number of leaves on the left side / num of leaves right
sackin <- sackin(as.treeshape(tree), norm = NULL)
#sum for each leaf of the number of internal nodes between the leaf and the root
WD_ratio <-function(tree){
    node_depth <- node.depth(tree)
    max_depth <- sort(node_depth, decreasing = T)[2]
    node_width <- table(node_depth[!(node_depth == 1 | node_depth == length(tree$tip.label))])
    return(max(node_width) / max_depth)
} # Ratio of the maximal width (W) over the maximal depth (D), where 
  #the depth of a node characterizes the number of branchs that lies between it
#and the root, and the width of a tree at a depth level d is the number of nodes
#that have the same depth d.
delta_w <- function(tree){
    tree <- tr
    node_depth <- node.depth(tree)
    node_depth <- node_depth[!(node_depth == 1 | node_depth == length(tree$tip.label))]
    node_width <- table(node_depth)
    return(diff(range(node_width)))
} #maximal difference in width
max_ladder <-  max_ladder <- max(node.depth(tree,method =2)) / length(tree$tip.label)
#maximal number of internal nodes in a ladder which is a chain of
  #connected internal nodes each linked to a single leaf, divided by the number of leaves

ladderScores3<-function(tree)
{
  LT<-NULL
  tr<- ladderize(tree)
  b <- balance(tr)
  count <- 0
  for (i in 1:length(b[,1]))
  {
    left<-as.numeric(b[i,1])
    right <-as.numeric(b[i,2])
    if (left == 1){
      count = count+1
    } else if (right == 1){
        count = count +1
      }
  }
  LT <- count / (length(b[,1]))
  return (LT)
IL_nodes <- ladderScores3(tree)
  #proportion of internal nodes in Ladders
  
  ladderScores<-function(tree)
{
  LT<-NULL
  tr<- ladderize(tree)
  b <- balance(tr)
  for (i in 1:length(b[,1]))
  {
    left<-as.numeric(b[i,1])
    right <-as.numeric(b[i,2])
    ratio <- right/left
    LT <- c(LT,ratio)
  }
  LT=sort(LT)
  return (LT)
}
LT=ladderScores(tree)
Staircaseness_1 <- length(which(LT<1))/length(LT)
  #Proportion of imbalanced internal nodes that have different 
  #numbers of leaves between the left and the right side
  
 ladderScores2<-function(tree)
{
  LT<-NULL
  tr<- ladderize(tree)
  b <- balance(tr)
  for (i in 1:length(b[,1]))
  {
    left<-as.numeric(b[i,1])
    right <-as.numeric(b[i,2])
    combine <- c(left , right) 
    ratio <- min(combine)/max(combine)
    LT <- c(LT,ratio)
  }
  LT=sort(LT)
  return (LT)}
Staircaseness_2 <- sum(unlist(ladderScores2(tree)))/length(ladderScores2(tree))
  #Mean ratio of the minimal number of leaves on a side over 
  #the maximal number of leaves on a side, for each internal node.
  
#summary stat based on LTT plot
max_L <- max(unlist(count_lineages_through_time(tree, Ntimes = length(tree$edge.length))[3]))
time_max <- get.ltt.summary(tree)[1]
slope_1 <- get.ltt.summary(tree)[2]
slope_2 <- get.ltt.summary(tree)[3]
ltt_ratio <-get.ltt.summary(tree)[4]
mean_s_time <- #mean time between two consecutive down steps (mean sampling time)
mean_b_time <- #Piecewise mean times between two consecutive up steps (mean branching times)

