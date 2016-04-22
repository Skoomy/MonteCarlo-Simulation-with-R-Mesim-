
#Projet Mesim_1 étude des simulations de variables aléatoires

#Les serires de nombres aléatoires a tester simulated with C++ program 
random<-read.csv("~/ensiie_S4/MESIM/RANDOM.csv")
fibonacci<-read.csv("~/ensiie_S4/MESIM/FIBONNACI.csv")
Jenssens <-read.csv("~/ensiie_S4/MESIM/Jenssens.csv") 

StdMinValues$V1
runif<-runif(n=10000)

#data frame récapitulant toutes les series 
all_val<-data.frame(fibonacci$V1,StdMinValues$V1,runif,random,Jenssens)
SFL<-data.frame(fibonacci$V1,StdMinValues$V1,Jenssens$V1)

names(all_val)<-c("Fibonnacci Series", "StdMinimal Series","Runif","Random","Jenssens Series")
names(SFL)<-c("Fibonacci Series", "StdMinimal Series","Jenssens Series")
# Tracé de leurs densité sur un meme graphe

library(ggplot2)
library(reshape)
library(RDieHarder)
df.m<-melt(all_val)
sfl.PLOT<-melt(SFL)
p<-ggplot(df.m) + geom_freqpoly(aes(x = value,y = ..density.., colour = variable),binwidth=1/30)
plot.s<-ggplot(sfl.PLOT) + geom_freqpoly(aes(x = value,y = ..density.., colour = variable),binwidth=1/30)
plot.s+ theme(legend.text = element_text(colour="black", size = 13, face = "bold"))
p+theme(legend.text = element_text(colour="black", size = 13, face = "bold"))

#Mise en Place des test de validité de ces différents générateur de nombres 
dieharder(rng="randu",psamples=100)
dieharder(5)
fibonacci
attach(fib)
help(dieharder)
dh<-dieharder(41, 15, seed=12345)
plot(dh)

attach(fibonacci)
typeof(fib)
fib<-as.list(fibonacci)
fib
d<-fibonacci$V1

install.packages("dgof")


fib<-ks.test(fibonacci$V1,"punif")
std<-ks.test(StdMinValues$V1,"punif")
jens<-ks.test(Jenssens$V1,"punif")
ru<-ks.test(runif,"punif")
ran<-ks.test(random$V1,"punif")
ran
library(gap)
par(mfrow=c(2,2),pty="m")

qqunif(fibonacci$V1,type="unif",alpha=0.05,main="Fibonacci")
qqunif(Jenssens$V1,type="unif",alpha=0.05,main="Jenssens&Lavaux")
qqunif(StdMinValues$V1,type="unif",alpha=0.05,main="STDminimal")
qqunif(random$V1,type="unif",alpha=0.05,main="random")
title("Tracé des QQplot",outer=T)



#test du poker
library(randtoolbox)
poker.test(fibonacci$V1)
poker.test(Jenssens$V1)
poker.test(StdMinValues$V1)
poker.test(random$V1)
poker.test(runif)
help(RNG)
randu<-dieharder(rng="Randu", seed=1000) #Randu 
mt_R<-dieharder(rng="R_mersenne_twister", seed=1000) #Mersenne Twister de R
Cplus_mt<-dieharder(rng= "mt19937",seed = 1000) #mt19937 C++ Mersenne Twister
dieharderGenerators()
dieharderTests()
x11()
par(mfrow=c(2,2))
plot(randu)
plot(Cplus_mt)
plot(mt_R)
names(mt_R)

plot(density(fibonacci$V1))
ggplot(all_values,aes(x=all_values$fibonacci.V1))+
  geom_density()
bbs2<-bbs$V1/(87566873*15485143)
bbs2<-bbs$V1/133501
poker.test(bbs$V1)ks.test(bbs$V1,"punif")
plot(density(bbs$V1))

binaire<-data.frame(bbsbinaire$V1)
ggplot(binaire,aes(x=binaire$bbsbinaire.V1))+
  geom_density()

plot(bbsbinaire$V1)
plot(density(bbsbinaire$V1))
all_val<-data.frame(bbs$V1)
ggplot(all_val,aes(x=all_val$bbs.V1))+
  geom_density()
bbsbinaire$V1

#######################################
fibonacci
StdMinValues$V1

all_val<-data.frame(fibonacci$V1,StdMinValues$V1)
length(all_values$StdMinValues.V1)
dty.fib=density(fibonacci$V1)
dty.std=density(StdMinValues$V1)
plot(dty.fib)
plot(dty.std)
install.packages(c("ggplot2", "gcookbook"))
library(ggplot2)
qplot(c(0,1),fun=dty.std,stat="function",geom="line")

#density plots


ggplot(all_values,aes(x=all_values$fibonacci.V1))+
  geom_density()

ggplot(all_val,aes(x=all_val$StdMinValues.V1))+
  geom_density(fill="blue",colour=NA,alpha=.2)
geom_line(stat="density")

all_values$fibonacci.V1<-factor(all_values$fibonacci.V1)
all_values$StdMinValues.V1<-factor(all_values$StdMinValues.V1)
ggplot(all_values,aes(x=all_values$fibonacci.V1,colour=all_values$StdMinValues.V1))+
  geom_density()
geom_vline(data=StdMinValues, aes(x=StdMinValues$V1),linetype="dashed", size=1)
all_values$StdMinValues.V1

df.m<-melt(all_val)
ggplot(df.m) + geom_freqpoly(aes(x = value,
                                 y = ..density.., colour = variable))

install.packages("reshape")
library(reshape)

x=rnorm(1000,2,1.5)



Plotdensity=density(StdMinValues$V1
          all_val<-data.frame(fibonacci$V1,a)
          summary(StdMinValues$V1)
          install.packages("RDieHarder")
          library(RDieHarder)
          install.packages("gsl")
          library(RDieHarder)
          help("dieharder")
          dieharder(rng=StdMinValues$V1)
          
          
          p+scale_fill_discrete(name="Densité des Générateurs\n=10000",breaks=c("fibonacci.V1","StdMinValues.V1","runif"),labels=c("Fibonnaci","STDMinimal","Runif"))
          p+scale_shape_discrete(name="Player")
          
