





#calculate_statistic-part-2-----------------------------------------------------------
#-------------------------------------------------------------------------------------
base_extended_f=function(base_H0,sample_test,break_stat)
{
#-------------------------------------------------------------------------------------
  library(magrittr) #pipe
  matrix_kernels=gaussian_kernel(matrix1=base_H0$sample_H0,
                                 matrix2=sample_test,
                                 scale_k=base_H0$scale_k)

  #matrix_kernels[which(matrix_kernels<1e-10)]=1e-10
  
  sum_kernels=colSums(matrix_kernels)
  
  sum_kernels[sum_kernels<1e-8]=1e-8
  
  base_extended=list(n_H0=nrow(base_H0$sample_H0),
                     row_sums_kernels_H0=base_H0$row_sums_kernels_H0,
                     col_sums_kernels_mixed=sum_kernels,
                    eigenfunctions=         
                       (t(matrix_kernels) %*% 
                       base_H0$eigenfunctions[,1:(break_stat+1)]) %>% 
                          sweep(.,1,sum_kernels,"/",F) %>%
                       sweep(.,2,base_H0$eigenvalues[1:(break_stat+1)],"/",F)

                      
                    )
  
  class(base_extended)="statistic_test"

  return(base_extended)
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------









#calculate_statistic-part-3-----------------------------------------------------------
#-------------------------------------------------------------------------------------
statistic_test_f=function(base_extended,elements)
{
#-------------------------------------------------------------------------------------
  library(magrittr) #pipe
  measure_s=base_extended$col_sums_kernels_mixed/sum(base_extended$row_sums_kernels_H0)
  
  theta=sweep(base_extended$eigenfunctions,1,measure_s,"*",F) %>% 
  colMeans() %>% "*" (base_extended$n_H0)
  
  if(elements)
  {

    return(cumsum(theta[-1]^2))
  
  } else {

    statistic=sum(theta[-1]^2)
  
    return(statistic)

  }

}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------










#calculate_statistic-part-1-----------------------------------------------------------
#-------------------------------------------------------------------------------------
calculate_statistic=function(sample_test,base_H0,break_stat,elements=F)
{
#-------------------------------------------------------------------------------------
  library(magrittr) #pipe
  base_extended=base_extended_f(base_H0,sample_test,break_stat)

  statistic_test=statistic_test_f(base_extended,elements)
  
  return(statistic_test)
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
