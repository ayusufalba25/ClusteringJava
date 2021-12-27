data.pemetaan<-as.matrix(DATA_PENDIDIKAN[,seq(25,38,2)])
cluster.kmeans<-kmeans(data.pemetaan,iter.max=1000,5)
cek_clust<-cluster.kmeans$cluster
hasil.cluster<-cbind(data.pemetaan,cek_clust)
write.table(hasil.cluster,file="D:/KULIAH/LOMBA/SATRIA DATA/SIC/PROGRAM/OUTPUT/HASIL KMEAN.csv",sep=",")


n<-nrow(data.pemetaan)
col<-ncol(data.pemetaan)
clust<-max(cek_clust)
n.clust<-rep(0,clust)
for(i in 1:clust)
{
  n.clust[i]=length(cek_clust[cek_clust==i])
}
avg.variable<-matrix(0,clust,col)
for(i in 1:clust)
{
  for(j in 1:col)
  {
    jml.var=0
    for(k in 1:n)
    {
      if(cek_clust[k]==i)
        jml.var=jml.var+hasil.cluster[k,j]
    }
    avg.variable[i,j]=jml.var/n.clust[i]
  }
}

library(cluster)
win.graph()
clusplot(data.pemetaan,cek_clust,main="CLUSTER",color=TRUE,shade=TRUE,labels=2,lines=0)

x <- rbind(cbind(rnorm(10,0,0.5), rnorm(10,0,0.5)),
           cbind(rnorm(15,5,0.5), rnorm(15,5,0.5)))
clusplot(pam(x, 2))

wssplot<-function(data,nc=15,seed=1234)
{
  wss<-(nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc)
  {
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum of squares")
}
wssplot(data.pemetaan,nc=6)