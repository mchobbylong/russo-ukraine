# Build igraph instance from edge list
library("igraph")
edgelist <- read.table("edges_list.tsv", sep="\t", header=FALSE, col.names=c("hashtagA", "hashtagB", "weight"))
G.igraph <- graph.data.frame(edgelist, directed=FALSE)

# Load up node attributes
attrs.raw <- read.table("nodes_attr.tsv", sep="\t", header=FALSE, col.names=c("hashtag", "category", "frequency"))
attrs.ordered <- attrs.raw[match(V(G.igraph)$name, attrs.raw$hashtag), ]
V(G.igraph)$category <- attrs.ordered$category
V(G.igraph)$frequency <- attrs.ordered$frequency

detach("package:igraph")
library("statnet")

# Convert igraph instance to statnet instance
G.statnet <- intergraph::asNetwork(G.igraph)

# Prepare for reproducible results
ergm.control <- control.ergm(seed=12315, MCMLE.maxit=1000)

# Category homophily
model.homophily <- ergm(G.statnet~edges + nodematch("category"), control=ergm.control)
print(summary(model.homophily))

# Frequency
model.frequency <- ergm(G.statnet~edges + nodecov("frequency"), control=ergm.control)
print(summary(model.frequency))

# Category-specific differential homophily (use with other categories)
model.category <- ergm(G.statnet~edges + nodematch("category", diff=TRUE), control=ergm.control)
print(summary(model.category))

# Summarize all terms
model.all <- ergm(G.statnet~edges + nodematch("category") + nodecov("frequency") + nodematch("category", diff=TRUE), control=ergm.control)
# print(summary(model.all))


# ==== Below are models for valued ERGM (edge weights as counts) ====
# !!!! Not done yet !!!!


# # Type homophily (valued)
# model.homophily.valued <- ergm(G.statnet~edges + nodematch("category"), response="weight", reference=~Poisson, control=ergm.control)
# print(summary(model.homophily.valued))
# 
# # Frequency (valued)
# model.frequency.valued <- ergm(G.statnet~edges + nodecov("frequency"), response="weight", reference=~Poisson)
# print(summary(model.frequency.valued))
# 
# # Category-specific (valued)
# model.category.valued <- ergm(G.statnet~edges + nodefactor("category"), response="weight", reference=~Poisson)
# print(summary(model.category.valued))
