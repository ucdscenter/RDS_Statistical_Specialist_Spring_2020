##Data Cleaning

tavr= read.csv("C:/Users/prava/Documents/PART TIME/Don/tavr.csv")
names(tavr)[1]="pre_wbc"
newtavr<-sapply(c("pre", "post"), function(x) tavr[startsWith(names(tavr),x)], simplify = FALSE)
newtavr$pre$lb = 'pre'
newtavr$pre$number <- 1:nrow(newtavr$pre)
colnames(newtavr$pre)= gsub(pattern = "pre_",replacement = "", x  = colnames(newtavr$pre))
newtavr$post$lb = 'post'
newtavr$post$number <- 1:nrow(newtavr$post)
colnames(newtavr$post)= gsub(pattern = "post_",replacement = "", x  = colnames(newtavr$post))
names(newtavr$post)[3]="hgb"
newtavr=rbind(newtavr$pre,newtavr$post)

#write.csv(newtavr,file = "newtavr.csv")

## Density Plots

#colnames(newtavr)
library(reshape2)
library(ggplot2)

# newtavr.plot <- melt(newtavr,id.vars= c("lb","number"))
# ggplot(newtavr.plot, aes (x=value,colour = lb)) +
#  geom_density() +
#  scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue")) +
# facet_wrap(~variable)

ggplot(newtavr, aes(x = wbc, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = rbc, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = hgb, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = hct, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = mcv, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = mch, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = mchc, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = chcm, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = ch, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = rdw, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = hdw, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = plt, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = mpv, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = retic, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = retic_2, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = chr, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = chm, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = pdw, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = pct, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = mpc, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = mpm, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = large_plt, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = neut, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = neut_2, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = lymph, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = lymph_2, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = mono, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = mono_2, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = eos, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = eos2, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = baso, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = baso_2, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = luc, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = luc_2, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = nrbc, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = li, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = mpxi, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))
ggplot(newtavr, aes(x = wbcp, colour = lb)) + geom_density() + scale_color_manual(breaks = c("pre", "post"), values=c("red", "blue"))

## Correlation

pre <- newtavr[newtavr$lb=="pre",] 
post <- newtavr[newtavr$lb=="post",]
pre$lymph_2=as.numeric(pre$lymph_2)
post$lymph_2=as.numeric(post$lymph_2)

## Correlation between pre and post values
library(ggcorrplot)

corr_pre_post <- round(cor(pre[,-c(39,40)],post[,-c(39,40)]))
ggcorrplot(corr_pre_post, type = "lower",
           outline.col = "grey",colors = c("#00AFBB", "white", "#FC4E07"),tl.cex=8,tl.srt=90)

## Correlation between all variables taking change into consideration
change <- post[,-c(39,40)]-pre[,-c(39,40)]
corr <- round(cor(change), 2)

ggcorrplot(corr,  type = "lower",
           outline.col = "grey",colors = c("#00AFBB", "white", "#FC4E07"),tl.cex=8,tl.srt=90)


## Clustering Analysis

summary(newtavr)

#Clustering doesnot work if there are NA(missing values). 
#Looking at the summary of data shows that the varibles lymph_2 and nrbc have missing values
#removing these columns for cluster analysis
colnames(newtavr)
newtavr_clus <- newtavr[,-c(26,35)]

library(fpc)
fit <- kmeans(newtavr_clus[,-c(37,38)], 2)
plotcluster(newtavr_clus[,-c(37,38)], fit$cluster)

#K-means clustering was not able to differentiate between pre and post treament records

#Heirarchial clustering

hc_result <- hclust(dist(newtavr_clus[,-c(37,38)]))
plot(hc_result)
#Cut Dendrogram into 3 Clusters
rect.hclust(hc_result, k=2)

#heirarchial clustering was also not able to differentiate between pre and post treatment

