
### Projet Mesim_3

####Van der corput
convertBase<-function(n,base){
  res<-c()
  while(n>0){
    res<-c(n%%base,res)
    n<-trunc(n/base)
  }
  res
}
convertBase(19,2)

help(trunc)
mirror<-function(res,base){
  s<-0
  n<-length(res)
  for(i in 1:n){
    s<-s+res[i]/base^(n-i+1)
  }
  s
}
mirror(6,2)

VDC<-function(base,N){
  res<-c()
  for(i in 1:N){
    a<-mirror(convertBase(i,base),base)
    res<-c(res,a)
  }
  res
}

vdc<-(VDC(11,1000))
plotvdc1<-qplot(y=vdc,xlab=" ")

plotvdc2<-ggplot(NULL,aes(x=vdc,y=..density..), xlab=" ")+geom_density(adjust=0.1)
grid.arrange(plotvdc1,plotvdc2,ncol=2)
plotvdc1
require(gridExtra)
library(randtoolbox)
library(ggplot2)
par(mfrow=c(1,2))
n=10000 
plot1=qplot(y=runif(n),main="Tracé de la Loi Uniforme",ylab="Runif",xlab=" ")

sob<-sobol(100,dim=1)

plot2=qplot(y=sob,,main="Tracé de Sobol",ylab="Suite de Sobol",xlab=" ")

grid.arrange(plot2, ncol=1)


#########"
#Halton

install.packages("matlab")
library(matlab)

###Méthode de Halton

Halton<-function(N,s){
  n<-1:1000
  base<-n[as.logical(isprime(n))]
  res1<-c()
  for(j in 1:s){
    res1<-c(res1,VDC(base[j],N))
  }
  res1
}

halton<-Halton(50,10)
plothal2<-qplot(y=hal,xlab="halton",main="",ylab="")

plothal2

haltonR<-halton(100,dim=1,scrambling=2)

qplot(y=halton)

ggplot(NULL,aes(x=halton,y=..density..), xlab=" ")+geom_density(adjust=1/10)

ggplot(NULL,aes(x=haltonR,y=..density..), xlab=" ")+geom_density()
hal2<-ggplot(NULL,aes(x=halton,y=..density..), xlab=" ")+geom_density(adjust=0.1)
hal2
grid.arrange(plothal2,hal2,ncol=1)





###Méthode de Sobol
sobol<-sobol(10000,1)
sob2<-qplot(y=sobol,xlab="sobol ")
splot2<-ggplot(NULL,aes(x=sobol,y=..density..),xlab=" ")+geom_density(adjust=1/10,alpha=1)
grid.arrange(sob2,splot2,ncol=2)







#####INFLUENCE DE LA BASE HALTON pour 100 points 
halton<-Halton(100,10)
plothal2<-qplot(y=halton,xlab="halton",main="",ylab="")
hal2<-ggplot(NULL,aes(x=halton,y=..density..), xlab=" ")+geom_density(adjust=0.1)
grid.arrange(plothal2,hal2,ncol=1)

n=1000
sobol10<-sobol(n,1)
halton10<-halton(n)
vander10<- VDC(2,n)
unif10<-runif(n)
all_val<-data.frame(halton10,sobol10,vander10,unif10)
names(all_val)<-c("Halton","Sobol","Vander","Uniforme")
df.m<-melt(all_val)
names(all_val)

p<-ggplot(df.m) + geom_freqpoly(aes(x = value,y = ..density.., colour = variable),binwidth=1/30)
p+theme(legend.text = element_text(colour="black", size = 13, face = "bold"))
p           
