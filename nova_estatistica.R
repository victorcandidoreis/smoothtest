source("~/funcoes_genericas.R")
caminhos=define_diretorio()
setwd(caminhos$pasta)

source("kernel_base_H0_functions.R")
source("statistic_functions2.R")
source("parallel_run.R")

library(foreach)
library(doParallel)
library(magrittr)


#---------------------------------------------------------------------


n=c(20,50,100,500,1000)[2]
mi=seq(0,1.5,length.out = 12)[3]
escala=c(seq(0.01,0.99,length.out = 5),5,100,2e3,5e3,1e4)
corte=8

#---------------------------------------------------------------------
#bases


# amostra_bases=matrix(rnorm(1e4),1e4,1)
# 
# #cl=parallel_run(3)
# Sys.time()
# hora=Sys.time()
# bases=list()
# 
# for(q in 1:length(escala)) {
# 
# 
#     bases[[q]]=calculate_base_H0(list(sample_H0=amostra_bases,scale_k=escala[q],
#                                       type_scale="quantile",type_groups="size",
#                                       number_groups=100))
# 
# 
# print(q/length(escala))
# }    # 2.376611 mins #1.252406 mins
# Sys.time()-hora
# #stopCluster(cl)



#saveRDS(bases,"bases_n50_2.RDS")
bases=readRDS("bases_n50_2.RDS")



#---------------------------------------------------------------------
#empiricas

# 
#  n_empiricas=1e4
# 
# cl=parallel_run(3)
# 
# Sys.time()
# hora=Sys.time()
# empiricas=foreach(q=1:length(escala)) %dopar%{
# 
#   matriz=matrix(NA,corte,n_empiricas)
# 
#   for (i in 1:corte)
#   {
#     amostra_empiricas=matrix(rnorm(n_empiricas*n),n_empiricas,n)
# 
#     matriz[i,]=apply(amostra_empiricas,1, function(amostra)
#       (calculate_statistic(sample_test = amostra,base_H0 = bases[[q]],
#                            break_stat = i,elements = F)))
#   }
# 
#   matriz
# }
# Sys.time()-hora #4.807784 mins #3.736318 mins
# stopCluster(cl)





#saveRDS(empiricas,"empiricas_n50_2.RDS")
empiricas=readRDS("empiricas_n50_2.RDS")


medidas=lapply(empiricas, function(x) (apply(x, 1, function(a) c(mean(a),sd(a)) )))



# n_empiricas=5e4
# 
# 
# cl=parallel_run(3)
# 
# Sys.time()
# hora=Sys.time()
# 
# amostra_empiricas=matrix(rnorm(n_empiricas*n),n_empiricas,n)
# 
# empiricas2=foreach(q=1:length(escala)) %dopar%{
# 
#   estatisticas=apply(amostra_empiricas,1, function(amostra)
#     (calculate_statistic(sample_test = amostra,base_H0 = bases[[q]],
#                          break_stat = corte,elements = T))
#   )
# }
# 
# Sys.time()-hora #19.45988 mins #3.780216 mins
# 
#  stopCluster(cl)



#saveRDS(empiricas2,"empirica_nova_parcial_n50_2.RDS")
 
 

#  empirica_nova_parcial=readRDS("empirica_nova_parcial_n50_2.RDS")
# 
#  Sys.time()
#  hora=Sys.time()
# n_empiricas=ncol(empirica_nova_parcial[[1]])
# 
#   matriz=matrix(NA,n_empiricas,length(escala)*corte)
#    matriz2=matrix(NA,n_empiricas,length(escala)*corte)
#   
#   for(a in 1:n_empiricas)
#   {
#     for(b in 1:length(escala))
#     {
#        matriz[a,((b-1)*corte+1):(b*corte)]=(empirica_nova_parcial[[b]][,a]-medidas[[b]][1,])/
#          medidas[[b]][2,]
#        matriz2[a,((b-1)*corte+1):(b*corte)]=sapply(1:corte,function(x)
#          (mean(empiricas[[b]][x,]>=empirica_nova_parcial[[b]][x,a])) )
#     }
#   print(a/n_empiricas)
#   } #6.859426 mins
# Sys.time()-hora 



#empirica_nova=apply((1-matriz2)*abs(matriz),1,sum)











 #  empirica_nova=apply((1-matriz2[,1:70])*abs(matriz[,1:70]),1,sum) 
  # empirica_nova=apply(matriz[,1:70], 1, function(x) (1/sum(abs(x)))*sum(x^2) )
#matrizes=list(matriz,matriz2)
 #matriz2[ matriz2==1e-100]=0
    #/apply((1-(matriz2[1:135e3,])),1,sum)
   #(((0.3)>(matriz2[1:135e3,])))*
  # empirica_nova=apply((1/matriz2)*(matriz^2), 1,sum)/
  #   ((apply(1/matriz2,1,sum))*(apply(abs(matriz2),1,sum)))
  
   # empirica_nova=apply((1/matriz2[1:135e3,])*(matriz[1:135e3,]^2), 1,sum)/
   #   ((apply(1/matriz2[1:135e3,],1,sum))*(apply(abs(matriz2[1:135e3,]),1,sum)))
  
   # empirica_nova=apply(((abs(matriz[1:135e3,])-apply(abs(matriz[1:135e3,]),1,mean))^2)*
   #                       (matriz[1:135e3,]^2), 1,sum)/
   #   (apply(((abs(matriz[1:135e3,])-apply(abs(matriz[1:135e3,]),1,mean))^2),1,sum)*
   #      (apply(abs(matriz[1:135e3,]),1,sum)))
   
#empirica_nova=apply(abs(matriz[1:135e3,])*matriz2[1:135e3,], 1,sum)/apply(abs(matriz[1:135e3,]), 1, sum)
  
#empirica_nova=apply((abs(matriz[1:135e3,])-apply(matriz[1:135e3,],1,mean))^2*(1-matriz2[1:135e3,])*
#                      abs(matriz[1:135e3,]), 1, sum )
# empirica_nova=apply(matriz, 1, function(x) (1/sum(abs(x)))*sum(x^2) )
#empirica_nova=apply(matriz, 1, function(x) sum((1/( 1 - abs(x)/sum(abs(x)) )) *
 #                                                  abs(x)) )


# empirica_nova=apply(matriz, 1, function(x) sum((1/( (length(x)+1)/length(x) - abs(x)/sum(abs(x)) )) *
#                                                    abs(x)) ) 
 
#(length(x)+1)/length(x)
#empirica_nova=apply(matriz, 1, function(x) sum((1/( sum(abs(x)) - abs(x) )) *
 #                                                            x^2) )

 # empirica_nova=apply(matriz, 1, function(x) max(x))
  








#saveRDS(empirica_nova,"empirica_nova_n50_2.RDS")

empirica_nova=readRDS("empirica_nova_n50_2.RDS")



grid_mi=seq(0,0.95,length.out = 20)



po_rep=3e2


poderes_so=rep(NA,length(grid_mi))
poderes_ks=rep(NA,length(grid_mi))
poderes_z=rep(NA,length(grid_mi))
#poderes_ad=rep(NA,length(grid_mi))
#poderes_cvm=rep(NA,length(grid_mi))


Sys.time()
hora=Sys.time()
for(mu in 1:length(grid_mi))
{

p_valores=rep(NA,po_rep)

amostras_po=matrix(NA,n,po_rep)





for(po in 1:po_rep){
 
  amostra=rnorm(n,mean=grid_mi[mu],sd=1)
  #amostra=rnorm(n)
  amostras_po[,po]=amostra
  
  #matriz3=matrix(NA,length(escala),corte)
  #matriz4=matrix(NA,length(escala),corte)
  
  matriz3=matrix(NA,length(escala),corte)
  matriz4=matrix(NA,length(escala),corte)
  for(q in 1:length(escala))  #length(escala))
  {
      estat_parcial=(calculate_statistic(sample_test = amostra,base_H0 = bases[[q]],
                                         break_stat = corte,elements = T))
        matriz3[q,]=( estat_parcial-medidas[[q]][1,])/
           medidas[[q]][2,]
        matriz4[q,]=sapply(1:corte,function(x)
          (mean(empiricas[[q]][x,]>= estat_parcial[x])) )
  }
  #matriz4[matriz4==0]=1e-100
  
  
 # estatistica=sum((1-matriz4)*abs(matriz3))#/sum(1-(matriz4))

  #estatistica=(1/sum(abs(matriz3)))*sum(matriz3^2)
  estatistica=sum((1-matriz4)*abs(matriz3))
  
  #((0.3)>(matriz4))*
   # estatistica=sum((1/matriz4)*(matriz3^2))/
   #   (sum(1/matriz4)*(sum(abs(matriz3))))
 # matriz4[matriz4==0]=1e-8
  #estatistica=sum(((abs(matriz3)-mean(abs(matriz3)))^2)*(matriz3^2))/
   # (sum((abs(matriz3)-mean(abs(matriz3)))^2)*(sum(abs(matriz3))))
  
 # estatistica=sum(abs(matriz3)*matriz4)/sum(abs(matriz3))
  
  #estatistica=sum((1-matriz4)*abs(matriz3)*(abs(matriz3)-mean(abs(matriz3)))^2)
  
  #estatistica=(1/sum(abs(matriz3)))*sum(matriz3^2)
  #estatistica=sum((1/( 1 - abs(matriz)/sum(abs(matriz)) )) *
   #                 abs(matriz)) 
  
  
  #estatistica=sum((1/( (length(matriz)+1)/length(matriz) - abs(matriz)/sum(abs(matriz)) )) *abs(matriz))
  
  #estatistica=sum((1/( sum(abs(x)) - abs(x) )) *
   #                 x^2)
 # estatistica=max(matriz)
  p_valores[po]=mean(estatistica<=empirica_nova)
  
  print(paste(Sys.time()-hora,"poder",po/po_rep,"valor",mean(p_valores[1:po]<0.05)))
  
}



poderes_so[mu]=mean(p_valores<0.05)


poderes_ks[mu]=mean(apply(amostras_po, 2,function(x) ks.test(x,"pnorm")$p.value)<0.05)


poderes_z[mu]=mean(apply(amostras_po, 2,function(x) BSDA::z.test(x,mu =0,sigma.x = 1)$p.value)<0.05)


#poderes_cvm[mu]=mean(apply(amostras_po, 2,function(x) goftest::cvm.test(x,"pnorm",0,1)$p.value)<0.05)


#poderes_ad[mu]=mean(apply(amostras_po, 2,function(x) goftest::ad.test(x,"pnorm",0,1)$p.value)<0.05)

print(mu/length(grid_mi))
}
Sys.time()-hora #35.9min #2.255371 mins


library(ggplot2)
library(RColorBrewer)

# dados_grafico1=data.frame(media=grid_mi,ks=poderes_ks,z=poderes_z,smooth=poderes_so,
#                          anderson_darling=poderes_ad,cramer_von_mises=poderes_cvm)
dados_grafico1=data.frame(media=grid_mi,ks=poderes_ks,z=poderes_z,smooth=poderes_so)
dados_grafico2=reshape2::melt(dados_grafico1,id="media")
names(dados_grafico2)=c("media","teste","poder")


# dados_grafico_temp=dados_grafico2[as.logical((dados_grafico2$teste=="smooth")+
#                                                (dados_grafico2$teste=="anderson_darling")),]
p1=ggplot(dados_grafico2,aes(x=media,y=poder,colour=teste))+geom_line()+theme_minimal()+xlim(c(0,1))+
  theme(text = element_text(size=20),legend.position = "top",legend.title = element_blank())+
  scale_color_manual(values=c("red", "blue","brown"))

ggsave("poder_simulacao1_n50_2.pdf")


# 
# 
# dados_grafico_temp=dados_grafico2[as.logical((dados_grafico2$teste=="smooth")+
#                                                (dados_grafico2$teste=="cramer_von_mises")),]
# p2=ggplot(dados_grafico_temp,aes(x=media,y=poder,colour=teste))+geom_line()+theme_minimal()+xlim(c(0,1))+
#   theme(text = element_text(size=20),legend.position = "top",legend.title = element_blank())+
#   scale_color_manual(values=c("red", "steelblue"))
# 
# 
# 
# 
# dados_grafico_temp=dados_grafico2[as.logical((dados_grafico2$teste=="smooth")+
#                                                (dados_grafico2$teste=="ks")),]
# p3=ggplot(dados_grafico_temp,aes(x=media,y=poder,colour=teste))+geom_line()+theme_minimal()+xlim(c(0,1))+
#   theme(text = element_text(size=20),legend.position = "top",legend.title = element_blank())+
#   scale_color_manual(values=c("steelblue","red"))
# 
# 
# dados_grafico_temp=dados_grafico2[as.logical((dados_grafico2$teste=="smooth")+
#                                                (dados_grafico2$teste=="z")),]
# p4=ggplot(dados_grafico_temp,aes(x=media,y=poder,colour=teste))+geom_line()+theme_minimal()+xlim(c(0,1))+
#   theme(text = element_text(size=20),legend.position = "top",legend.title = element_blank())+
#   scale_color_manual(values=c("steelblue","red"))
# 
# plot=gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)
















grid_sigma=seq(1,3,length.out = 20)



po_rep=5e3


poderes_so=rep(NA,length(grid_sigma))
poderes_ks=rep(NA,length(grid_sigma))
poderes_z=rep(NA,length(grid_sigma))
poderes_ad=rep(NA,length(grid_sigma))
poderes_cvm=rep(NA,length(grid_sigma))


Sys.time()
hora=Sys.time()
for(si in 1:length(grid_sigma))
{
  
  p_valores=rep(NA,po_rep)
  
  amostras_po=matrix(NA,n,po_rep)
  
  
  
  
  
  for(po in 1:po_rep){
    
    amostra=rnorm(n,mean=0,sd=2)
    #amostra=rnorm(n)
    amostras_po[,po]=amostra
    
    matriz=matrix(NA,length(escala),corte)
    
    for(q in 1:length(escala))
    {
      matriz[q,]=((calculate_statistic(sample_test = amostra,base_H0 = bases[[q]],
                                       break_stat = corte,elements = T))-medidas[[q]][1,])/
        medidas[[q]][2,]
    }
    
    #estatistica=(1/sum(abs(matriz)))*sum(matriz^2)
   # estatistica=sum((1/( 1 - abs(matriz)/sum(abs(matriz)) )) *
    #                  abs(matriz)) 
    
    
    estatistica=sum((1/( (length(matriz)+1)/length(matriz) - abs(matriz)/sum(abs(matriz)) )) *abs(matriz))
    
    
    #(length(matriz)+1)/length(matriz)
    #estatistica=sum((1/( sum(abs(matriz)) - abs(matriz) )) *
     #                 matriz^2)
    
    p_valores[po]=mean(estatistica<=empirica_nova)
    
    print(paste(Sys.time()-hora,"poder",po/po_rep,"valor",mean(p_valores[1:po]<0.05)))
    
  }
  
  
  
  poderes_so[si]=mean(p_valores<0.05)
  
  
  poderes_ks[si]=mean(apply(amostras_po, 2,function(x) ks.test(x,"pnorm")$p.value)<0.05)
  
  
  poderes_z[si]=mean(apply(amostras_po, 2,function(x) BSDA::z.test(x,mu =0,sigma.x = 1)$p.value)<0.05)
  
  
  poderes_cvm[si]=mean(apply(amostras_po, 2,function(x) goftest::cvm.test(x,"pnorm",0,1)$p.value)<0.05)
  
  
  poderes_ad[si]=mean(apply(amostras_po, 2,function(x) goftest::ad.test(x,"pnorm",0,1)$p.value)<0.05)
  
  
}
Sys.time()-hora #35.9min


library(ggplot2)
library(RColorBrewer)

dados_grafico3=data.frame(desvio_padrao=grid_sigma,ks=poderes_ks,z=poderes_z,smooth=poderes_so,
                          anderson_darling=poderes_ad,cramer_von_mises=poderes_cvm)
dados_grafico4=reshape2::melt(dados_grafico1,id="desvio_padrao")
names(dados_grafico2)=c("desvio_padrao","teste","poder")


dados_grafico_temp=dados_grafico2[as.logical((dados_grafico2$teste=="smooth")+
                                               (dados_grafico2$teste=="anderson_darling")),]
p1=ggplot(dados_grafico_temp,aes(x=desvio_padrao,y=poder,colour=teste))+geom_line()+theme_minimal()+xlim(c(0,1.15))+
  theme(text = element_text(size=20),legend.position = "top",legend.title = element_blank())+
  scale_color_manual(values=c("red", "steelblue"))




dados_grafico_temp=dados_grafico2[as.logical((dados_grafico2$teste=="smooth")+
                                               (dados_grafico2$teste=="cramer_von_mises")),]
p2=ggplot(dados_grafico_temp,aes(x=desvio_padrao,y=poder,colour=teste))+geom_line()+theme_minimal()+xlim(c(0,1.15))+
  theme(text = element_text(size=20),legend.position = "top",legend.title = element_blank())+
  scale_color_manual(values=c("red", "steelblue"))




dados_grafico_temp=dados_grafico2[as.logical((dados_grafico2$teste=="smooth")+
                                               (dados_grafico2$teste=="ks")),]
p3=ggplot(dados_grafico_temp,aes(x=desvio_padrao,y=poder,colour=teste))+geom_line()+theme_minimal()+xlim(c(0,1.15))+
  theme(text = element_text(size=20),legend.position = "top",legend.title = element_blank())+
  scale_color_manual(values=c("steelblue","red"))


dados_grafico_temp=dados_grafico2[as.logical((dados_grafico2$teste=="smooth")+
                                               (dados_grafico2$teste=="z")),]
p4=ggplot(dados_grafico_temp,aes(x=desvio_padrao,y=poder,colour=teste))+geom_line()+theme_minimal()+xlim(c(0,1.15))+
  theme(text = element_text(size=20),legend.position = "top",legend.title = element_blank())+
  scale_color_manual(values=c("steelblue","red"))

plot=gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

ggsave("poder3_n50.png",plot,width = 14,height = 7)














sapply(apply(amostras_po,2, function(x) calculate_statistic(x,bases[[17]],2,F)),function (a)
  
  mean(empiricas[[17]][2,]>=a)

  ) %>% (function(b) mean(b<0.05))





# repeticoes=500
# 
# matriz_poderes=matrix(NA,length(grid_mi),3+length(escala)*corte)
# matriz_poderes=as.data.frame(matriz_poderes)
# matriz_poderes[,1]=grid_mi
# 
# coluna=4
# for(media in 1:length(grid_mi))
# {
#   matriz_amostras=matrix(rnorm(repeticoes*50,mean=grid_mi[media],sd=1),50,repeticoes)
# 
#   matriz_poderes[media,2]=mean(apply( matriz_amostras, 2,function(x)
#     BSDA::z.test(x,mu=0,sigma.x =1)$p.value)<0.05)
#   matriz_poderes[media,3]=mean(apply( matriz_amostras, 2,function(x)
#     ks.test(x,"pnorm",0,1)$p.value)<0.05)
#   for(q in 1:length(escala)){
# 
#     parciais=apply(matriz_amostras,2, function(x) calculate_statistic(x,bases[[q]],corte,T))
# 
#     for (i in 1:corte)
#     {
#       matriz_poderes[media,coluna]=sapply(parciais[i,],function (a)
# 
#         mean(empiricas[[q]][i,]>=a)) %>% (function(b) mean(b<0.05))
#       coluna=coluna+1
#       }
#   }
# coluna=4
# }  # 9min
# 
# 
# lista_plots=list()
# ref_plot=1
# for(q in 1:length(escala))
# {
# for(i in 1:corte)
# {
#     ref_i=i
#     ref_escala=q
#     matriz_poderes_e=matriz_poderes[,c(1,2,3,(ref_escala-1)*corte+ref_i+3)]
#     names(matriz_poderes_e)=c("media","z","ks","smooth")
#     matriz_poderes_e=reshape2::melt(matriz_poderes_e,id="media")
#     names(matriz_poderes_e)=c("media","teste","poder")
# 
# 
#     lista_plots[[ref_plot]]=ggplot(matriz_poderes_e,aes(x=media,y=poder,colour=teste))+geom_line()+theme_minimal()+
#       xlim(c(0,1))+theme(text = element_text(size=20),legend.position = "top",
#                          legend.title = element_blank())+scale_color_manual(values=c("red", "blue",'black'))+
#       ggtitle(paste("I:",ref_i,"e","escala:",ref_escala))
#     ref_plot=ref_plot+1
#     }
# }
# 
# 
# posicao=function(i,q,corte)
# {
# ref_i=i
# ref_escala=q
# 
# return((ref_escala-1)*corte+ref_i)
# }
# 
# 
# 
#  saveRDS(list(corte=corte,escala=length(escala),poderes=matriz_poderes,desc="primeira coluna medias segunda z terceira ks e demais smooth",
#          graficos=lista_plots,posicao=posicao),"poderes_cada_lambda_media.RDS")


graficos_media=readRDS("poderes_cada_lambda_media.RDS")

# repeticoes=500
# 
# matriz_poderes2=matrix(NA,length(grid_sigma),2+length(escala)*corte)
# matriz_poderes2=as.data.frame(matriz_poderes2)
# matriz_poderes2[,1]=grid_sigma
# 
# coluna=3
# for(desv in 1:length(grid_sigma))
# {
#   matriz_amostras=matrix(rnorm(repeticoes*50,sd=grid_sigma[desv],mean=0),50,repeticoes)
#   
#   matriz_poderes2[desv,2]=mean(apply( matriz_amostras, 2,function(x) 
#     ks.test(x,"pnorm",0,1)$p.value)<0.05)
#   for(q in 1:length(escala)){
#     
#     parciais=apply(matriz_amostras,2, function(x) calculate_statistic(x,bases[[q]],corte,T))
#     
#     for (i in 1:corte)
#     {
#       matriz_poderes2[desv,coluna]=sapply(parciais[i,],function (a)
#         
#         mean(empiricas[[q]][i,]>=a)) %>% (function(b) mean(b<0.05))
#       coluna=coluna+1
#     }
#   }
#   coluna=3  
# } # 9min
# 
# 
# lista2_plots=list()
# ref_plot=1
# for(q in 1:length(escala))
# {
#   for(i in 1:corte)
#   {
#     ref_i=i
#     ref_escala=q
#     matriz_poderes2_e=matriz_poderes2[,c(1,2,(ref_escala-1)*corte+ref_i+2)]
#     names(matriz_poderes2_e)=c("desv","ks","smooth")
#     matriz_poderes2_e=reshape2::melt(matriz_poderes2_e,id="desv")
#     names(matriz_poderes2_e)=c("desv","teste","poder")
#     
#     matriz_poderes2_e=as.data.frame( matriz_poderes2_e)
#     lista2_plots[[ref_plot]]=ggplot(matriz_poderes2_e,aes(x=desv,y=poder,colour=teste))+geom_line()+theme_minimal()+
#      theme(text = element_text(size=20),legend.position = "top",
#                          legend.title = element_blank())+scale_color_manual(values=c("red", "steelblue"))+
#       ggtitle(paste("I:",ref_i,"e","escala:",ref_escala))+xlab("desvio padrão")
#     ref_plot=ref_plot+1
#   }
# }

# saveRDS(list(corte=corte,escala=length(escala),poderes=matriz_poderes2,desc="primeira coluna sigma segunda ks e demais smooth",
#         graficos=lista2_plots,posicao=posicao),"poderes_cada_lambda_desv.RDS")


graficos_desv=readRDS("poderes_cada_lambda_desv.RDS")
