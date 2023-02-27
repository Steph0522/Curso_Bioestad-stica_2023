## install.packages("hillR")
## # o instala la versión en desarrollo del github
## devtools::install_github("daijiang/hillR")

## install.packages("FD")

set.seed(123)
library(FD)
dummy_data <- dummy
comunidades<-  dummy_data$abun
funciones <- dummy_data$trait
arbol <- ape::rtree(n = ncol(comunidades), tip.label = paste0("sp", 1:ncol(comunidades)))

class(comunidades)
class(funciones)
class(arbol)

head(comunidades)

head(funciones)

 head(arbol)

library(hillR)

hill_taxa(comunidades, q = 0)

hill_func(comunidades, funciones, q = 0)

hill_phylo(comunidades, arbol, q = 0) 

hill_taxa(comunidades, q = 1)
hill_taxa(comunidades, q = 2)

hill_taxa_parti(comunidades, q = 0)

hill_func_parti(comunidades, funciones, q = 0)

hill_phylo_parti(comunidades, arbol, q = 0)

hill_taxa_parti_pairwise(comunidades, q = 0, .progress = FALSE)

hill_func_parti_pairwise(comunidades, funciones, q = 0, show_warning = FALSE, .progress =FALSE)

hill_phylo_parti_pairwise(comunidades, arbol, q = 0, show_warning = FALSE, .progress = FALSE) 

beta_q0_mat<-hill_taxa_parti_pairwise(
  comunidades, q = 0, .progress = FALSE,
  output = "matrix", pairs = "full")
class(beta_q0_mat)

## library(iNEXT)
## data("spider")

## install.packages("betapart")
## 

library(betapart)
comm<- data.frame(comm =1:6,
                         sp1=c(2,2,3,0,0,1),
                         sp2=c(2,2,0,1,1,2),
                         sp3=c(1,0,1,2,3,2),
                         sp4=c(1,0,1,0,2,0),
                         sp5=c(1,2,0,0,0,1),
                         sp6=c(2,2,1,0,0,0),
                         sp7=c(0,0,0,1,0,1),
                         sp8=c(1,0,1,0,1,0), row.names = 1)


groups <- factor(c(rep(1,3), rep(2,3)), 
                 labels = c("noperturbado","perturbado"))
head(comm);groups

 presabs<-ifelse(comm>0,1,0)

 dist_comp<-beta.pair(presabs, index.family="sorensen")

head(dist_comp)

library(vegan)
bd<-betadisper(dist_comp$beta.sor,groups)
bd.sim<-betadisper(dist_comp$beta.sim,groups)
bd.sne<-betadisper(dist_comp$beta.sne,groups)



par(mfrow=c(1,3))
plot(bd);plot(bd.sim);plot(bd.sne)

dist.multi<-beta.multi(presabs,index.family ="sorensen" )
head(dist.multi)

data(BCI, BCI.env)

shannon <- diversity(BCI)
simpson <- diversity(BCI, "simpson")
inverso_simp <- diversity(BCI, "inv")
fisher <- fisher.alpha(BCI)
especies_num <- specnumber(BCI) 

## La diversidad beta definida como gamma/alpha - 1:
## teniendo en cuenta el número total de especies
(alpha <- with(BCI.env, tapply(specnumber(BCI), Habitat, mean)))
(gamma <- with(BCI.env, specnumber(BCI, Habitat)))
gamma/alpha - 1
## de manela similar pero con la diversidad de Shannon
(alpha <- with(BCI.env, tapply(diversity(BCI), Habitat, mean))) # promedio
(gamma <- with(BCI.env, diversity(BCI, groups=Habitat))) # junta
## aditiva con la diversidad de Shannon
gamma-alpha

library(tidyverse)
distancias<- vegdist(BCI, method = "jaccard")
distancias %>% as.matrix() %>% as.data.frame() %>% dplyr::select(1:4) %>% dplyr::slice(1:4)


## ## instalando iNEXT del CRAN
## install.packages("iNEXT")
## 
## ## instalando la versión de desarrollo
## install.packages('devtools')
## library(devtools)
## install_github('AnneChao/iNEXT')

## cargando el paquete
library(iNEXT)
library(ggplot2)

data("spider")
str(spider)
out <- iNEXT(spider, q=c(0, 1, 2), datatype="abundance")

out

ggiNEXT(out, type=1, facet.var="site")

ggiNEXT(out, type=2)

ggiNEXT(out, type=3, facet.var="site")

ggiNEXT(out, type=3, facet.var="order")

estimateD(spider$Logged, datatype="abundance", base="size",  conf=0.95)

estimateD(spider$Girdled, datatype="abundance", base="size",  conf=0.95)

## data(ciliates)
## #str(ciliates)

ChaoRichness(spider)
ChaoShannon(spider)

library(vegan)
data("BCI")
head(BCI)[1:4,1:4]

#por sitios
sac <- specaccum(BCI)
plot(sac, ci.type="polygon") #ver vegan para opciones

#por individuos
sac <- specaccum(BCI, method = "rarefaction")
plot(sac, xvar = "individual", ci.type="polygon") 
