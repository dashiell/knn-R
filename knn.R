# Dashiell Gough, 2015.  k-Nearest neighbours test
library(ggplot2)
library(grid)
# set up a data frame for 6 points belonging to two known categories  
df.known = data.frame(category=c('a','a','a','b','b','b'), x=c(1,2,3,8,10,9), y=c(10,8,10,6,4,8), stringsAsFactors = FALSE)
# a second data frame for 2 points with unknown categories
df.unknown = data.frame(category=c('NA','NA'), x=c(5,6),y=c(7,8), stringsAsFactors = FALSE)
# merge the two data frames for convenience in plotting
df.all = merge(df.known, df.unknown, all=TRUE)

# returns the plot object
get.plot = function(df) {
  ggplot(df, aes(x=x,y=y,color=category)) + 
    geom_point(size=5) + 
    scale_x_continuous(breaks=0:10,limits=c(0,10)) + 
    scale_y_continuous(breaks=0:10, limits=c(0,10))
}
unsolved.plot = get.plot(df.all)
#print(unsolved.plot)

euclidean.distance <- function(x1, x2) sqrt(sum((x1 - x2)^2))

# loop through all the unknown row indexes
for(idx.unknown in 1:nrow(df.unknown)) {
  # get the euclidean distance between each x, y coordinate of the unknown table and the current known table index
  # example expected output: 4.472136 3.000000 2.828427 3.605551 6.403124 4.000000
  df.known$dist = sapply(1:nrow(df.known), function(idx.known) { 
    euclidean.distance(df.known[idx.known,2:3], df.unknown[idx.unknown,2:3])
    #dist( rbind(df.known[idx.known,2:3], df.unknown[idx.unknown,2:3]), method="euclidean") 
  })
  # get the nearest three row index numbers. Example output: 3 2 4
  nearest.three = order(df.known$dist)[1:3]
  # get the categories for each row. Output: "a" "a" "b"
  nearest.three.categories = df.known[nearest.three,]$category
  # output: "a"
  most.common.category = names(sort(table(nearest.three.categories),decreasing=TRUE)[1])
  # update the value for the category in the unkown table to "a"
  df.unknown[idx.unknown,]$category = most.common.category
}
df.known$dist = NULL

df.solved = merge(df.known,df.unknown, all=TRUE)

solved.plot = get.plot(df.solved)

# plot the unsolved and solved scatterplots side by side
pushViewport(viewport(layout = grid.layout(1,2)))
print(unsolved.plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(solved.plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

## Alternatively, using library
library(class)
k_train = rbind(df.known[2:3])
k_test = rbind(df.unknown[2:3])
k_cl = df.known[,1]
k_result = knn(k_train, k_test, k_cl, k = 3)

