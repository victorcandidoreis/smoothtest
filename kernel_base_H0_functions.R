 #pipe




#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
gaussian_kernel=function(matrix1,matrix2=NULL,scale_k,partial=F)
{
  library(magrittr)
 if (!partial)
  {
    fields::rdist(x1=matrix1,x2=matrix2) %>%  .^(2) %>% "*" (-1) %>% "/" (scale_k) %>% 
  exp() %>%  return()
  } else 
  {
   matrix1 %>% "*" (-1) %>% "/" (scale_k) %>% 
  exp() %>%  return()
  }

  
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------



smooth_kernel=function(amostra_original,matrix_kernels,type_groups,number_groups)
{
  n=nrow(matrix_kernels)
  if(type_groups=="divided")
  {
    index=seq(1,n,n/number_groups)
    number=number_groups
    size=n/number_groups
  } else if(type_groups=="size"){
    index=seq(1,n,number_groups)
    number=n/number_groups
    size=number_groups
  } else {
    return(NA)
  }
  smooth_sample=rep(NA,number)
  for (i in 1:number) {
    smooth_sample[i]=mean(amostra_original[index[i]:(size*i),1])
  }
  
  
  smooth_matrix=matrix(0,number,number)
  for (i in 1:(number-1))
  {
    for (j in (i+1):number)
    {
      lines=index[i]:(size*i)
      columns=index[j]:(size*j)
      smooth_matrix[i,j]=mean(matrix_kernels[lines,columns])
      smooth_matrix[j,i]=smooth_matrix[i,j]
    }
  }
  return(list(sample=as.matrix(smooth_sample),matrix=smooth_matrix+diag(nrow=number)))
}




#-------------------------------------------------------------------------------------
#arguments=list(sample_H0,scale_k,type_scale)-----------------------------------------
calculate_base_H0=function(arguments)
{
  library(magrittr)
  if(arguments$type_scale=="quantile")
  {
    auxiliary=1

    if (arguments$scale_k>=1)
    {
      auxiliary=arguments$scale_k
      arguments$ scale_k=0.99
    }


    squared_distances=fields::rdist(x1=arguments$sample_H0) %>% .^(2)

    arguments$scale_k= squared_distances %>% 
    quantile(.,arguments$scale_k) %>% "*" (auxiliary)
  }
  matrix_kernels=gaussian_kernel(matrix1=squared_distances,
                                 scale_k=arguments$scale_k,partial=T)
  # new=smooth_kernel(arguments$sample_H0,matrix_kernels,
  #                   type_groups=arguments$type_groups,number_groups=arguments$number_groups)
  # 
  # arguments$sample_H0=new$sample
  # 
  # matrix_kernels=new$matrix
  
  sum_kernels=rowSums(matrix_kernels)
  
  measure_s=sum_kernels/sum(sum_kernels)
  
  symmetric_matrix=sweep(matrix_kernels,1,sqrt(sum_kernels),"/",F) %>% 
    sweep(.,2,sqrt(sum_kernels),"/",F)
  
  eigen_decomposition=eigen(symmetric_matrix,symmetric = T)
  
  
  eigenfunctions=eigen_decomposition$vectors %>% 
    sweep(.,1,sqrt(measure_s),"/",F)
  
  base_H0=list(sample_H0=arguments$sample_H0,
    row_sums_kernels_H0=sum_kernels,
    eigenvalues=eigen_decomposition$values,eigenfunctions=eigenfunctions,
    scale_k=arguments$scale_k)

  class(base_H0)="base_extended"
  
  return(base_H0)
}
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------





