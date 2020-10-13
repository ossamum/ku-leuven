rm(list = ls())

library(MASS)
library(dplyr)
library(ggplot2)
library(maptools)
library(factoextra)

load('Assignment/wvs.Rdata')

wvs[,1:10] <- wvs[,1:10] %>% scale(center = TRUE, scale = TRUE)

wvs.mean <- wvs %>%
  group_by(country) %>% 
  summarise(mean_creative = mean(V_creative),
            mean_rich = mean(V_rich),
            mean_secure = mean(V_secure),
            mean_spoil_oneself = mean(V_spoil_oneself),
            mean_do_good = mean(V_do_good),
            mean_be_successful = mean(V_be_successful),
            mean_exciting_life = mean(V_exciting_life),
            mean_behave_properly = mean(V_behave_properly),
            mean_protect_environment = mean(V_protect_environment),
            mean_tradition = mean(V_tradition))

wvs.pca <- wvs[,1:10] %>% prcomp()

screeplot(wvs.pca,type="lines")
wvs.pca %>% fviz_eig(addlabels = TRUE, ylim = c(0, 33))

# eigenvalues
round(wvs.pca$sdev^2,2)

# proportion of explained variance per component
round(wvs.pca$sdev^2/10,2)

# compute component loadings
A <- wvs.pca$rotation%*%diag(wvs.pca$sdev)
plot(A[,1:2],xlim=c(-0.7, -0.35),ylim=c(-0.5,0.6),xlab="PC1",ylab="PC2")
pointLabel(A[,1],A[,2],names(wvs[,1:10]),cex=0.5)

# wvs.pca %>% biplot(pc.biplot=TRUE, xlim=c(-4,0),ylim=c(-2,2), cex = 0.7)
wvs.pca %>%  fviz_pca_var(col.var="steelblue")
wvs.pca %>%  fviz_pca_biplot(label ="var")


# references
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# http://www.sthda.com/english/wiki/fviz-pca-quick-principal-component-analysis-data-visualization-r-software-and-data-mining


# CONCLUSIONS
# PCA - The first two components explain 48% of the variance
# using the Kaiser's rule, we should retain 2 components

# Biplot - Respondents are similar with secure, good, behave properly,
# and protect environment. It makes sense, since they seem to be
# more conservative
# On the other side, people more oudacious seem to share common values like
# being rich, exciting life, spoil one self, creative and be successful


