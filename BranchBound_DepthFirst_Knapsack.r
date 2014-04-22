################################################################
# R script using only the base package
################################################################
# depth first branch and bound designed for the knapsack problem
#
# knapsack problem ::
# decision variables: include item x (1) or not (0)
# linear constraints: total set of items implies a cost lower than the capacity xw<=C
# objective function: maximise total value v = xv=V

# bounding > optimistic relaxation -> prune
# branching > split problem in subproblems
#
# with thanks to the coursera course on discrete optimisation
# of professor Pascal Van Hentenryck

# manual specification of a tiny dataset
# in agreement with course notes

# maximum weight
maxw <- 31181
# the combinations of value - weight for each of 20 items
tdta <- scan(sep="")
19 31181
1945 4990
321 1142
2945 7390
4136 10372
1107 3114
1022 2744
1101 3102
2890 7280
962 2624
1060 3020
805 2310
689 2078
1513 3926
3878 9656
13504 32708
1865 4830
667 2034
1833 4766
16553 40006

dta <- matrix(tdta,nrow=20,byrow=T)
# my data import
# dtaName <- "data/ks_19_0"
# dta <- read.table(dtaName,sep=" ")
# nri <- dta[1,1]
# maxw <- dta[1,2]
# dta <- dta[-1,]
dimnames(dta)[[1]] <- paste0("i",1:nrow(dta))
dimnames(dta)[[2]] <- c("v","w")

# reorder the items according to their weight to get near the maximum as soon as possible
dta <- dta[rev(order(dta[,2])),]

# remove combinations that are invalid from the start
# only consider items with a weight that is less than the capacity
dta <- dta[dta[,'w']<=maxw,]

# global variable to keep track of the best solution so far
opt <- 0

# determine one single item to add
nextNode <- function(nodes,dta){
	# consider only items that are still open [invalid items are closed for further exploration]
	stillOpen <- (unlist(lapply(nodes,function(.x) .x$open)))
	# determine index for the first node that is open for further exploration
	select <- (1:length(nodes))[stillOpen & !duplicated(stillOpen)]
	# return id of next item
	return(select)	
}
# nextNode(nodes,dta)
# 1

# determine next item to evaluate [to add / or not to add]
nextItem <- function(node,dta){
	tnode <- node[[1]]
	# return name of the first item in the dta table that is not yet included nor refuted for current node
	return(row.names(dta)[!row.names(dta)%in%c(tnode$include,tnode$out)][1])
}
# nextItem(nodes[nextNode(nodes,dta)],dta)
# "i1"

# evaluate if the proposed node is valid for exploration for a given item
okAdd <- function(tnode,dta,id){
	# requirement: adding the item does not exceed the remaining capacity
	ok1 <- (tnode$capacity - dta[id,'w']) >= 0
	# requirement: the maximum possible value in this branch is not less than an already determined best solution
	ok2 <- (tnode$maxvalue >= opt)
	# return True only if all requirements are met, or False otherwise
	return(ok1 & ok2)
}

# remove nodes from the list to avoid large lists of nodes that are not explored further
pruneNodes <- function(nodes){
	# remove if the maximum value is less then the optimal value
	nodes <- nodes[unlist(lapply(nodes,function(.x) .x$maxvalue >= opt))]
	return(nodes)
}

# select the proposed item (alternative is to refute it)
itemSelect <- function(node,dta){
	tnode <- node[[1]]
	# identify an item to add to the path
	id <- nextItem(node,dta)
	# determine wether it could be added before proceeding, otherwise ignore this step
	if(okAdd(tnode,dta,id)){
		# increase the total value for the node so far
		tnode$value <- tnode$value + dta[id,'v']
		# decrease the remaining capacity for the node so far
		tnode$capacity <- tnode$capacity - dta[id,'w']
		# add the item to the list of included items
		tnode$include <- c(tnode$include,id)
		# if all items are attempted (included + refuted) avoid further exploration of this node (open = False)
		if(length(c(tnode$out,tnode$include)) == nrow(dta)) tnode$open <- F
		# if the total value so far for this node is equal to the maximum value avoid further processing of this node (open = False)
		if(tnode$value == tnode$maxvalue){ 
			tnode$open <- F
			# if a value can not be increased, and if it is bigger than the best solution so far
			# then change it to be the best solution so far
			opt <<- max(opt,tnode$value)
			cat("change global value for best solution : ",opt,"\n")
		}
		# if the maximum value is less than the best solution so far, avoid further processing of this node (open = False)
		if(tnode$maxvalue < opt){ 
			tnode$open <- F
		}
		# return the node with changed attributes
		return(list(tnode))
	}
}

# refute the item (alternative is to selecting it)
itemRefute <- function(node,dta){
	tnode <- node[[1]]
	# identify an item to add to the path
	id <- nextItem(node,dta)
	# decrease the maximum value with the value of the item that is refuted
	tnode$maxvalue <- tnode$maxvalue - dta[id,'v']
	# add the item to the list of not included items (out)
	tnode$out <- c(tnode$out,id)
	# if all items are attempted (included + refuted) avoid further exploration of this node (open = False)
	if(length(c(tnode$out,tnode$include)) == nrow(dta)) tnode$open <- F
	# return the node with changed attributes
	return(list(tnode))
}

# set up an empty list with the appropriate attributes to start from
nodes <- list(list(value=0,maxvalue=sum(dta[,'v']),capacity=maxw,include=vector(mode="character"),out=vector(mode="character"),open=T))
# execute the branch and bound untill all nodes are closed for further exploration
while(any(unlist(lapply(nodes,function(.x) .x$open)))){
	# combine the following three parts into a new list:
	# - the modified node when selecting the proposed item
	# - the modified node when refuting the proposed item
	# - the nodes that were not considered in current step
	nodes <- c(itemSelect(nodes[nextNode(nodes,dta)],dta),itemRefute(nodes[nextNode(nodes,dta)],dta),nodes[-nextNode(nodes,dta)])
	# prune the proposed nodes that after modifications are not valid anymore
	nodes <- pruneNodes(nodes)
}
# the structure of the resulting nodes is :
str(nodes)

# the maximum value is :
(MaximumValue <- max(unlist(lapply(nodes,function(.x) .x$value))))
# the node that returns the maximum value is :
BestNode <- nodes[MaximumValue==unlist(lapply(nodes,function(.x) .x$value))][[1]]
# the items that are included are :
(BestNode$include)
# the items imply the following value and weight combinations
(dta[BestNode$include,])
# the final weight for this set of items is :
maxw - BestNode$capacity
# the capacity still remaining is :
BestNode$capacity
