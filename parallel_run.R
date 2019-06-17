parallel_run=function(n)
{
library(foreach)
library(doParallel)


nodes <-max(detectCores()-n,1)
cl <- makeCluster(nodes)
registerDoParallel(cl)

return(cl)
}



