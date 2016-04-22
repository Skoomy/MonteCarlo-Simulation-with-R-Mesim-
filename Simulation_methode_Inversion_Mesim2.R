###Simulation par la méthode d'inversion 
library(ggplot2)
library(reshape)
##Loi exponentielle
n=2000
x<-runif(n)
y<-(-log(x)/1.5)
exp<-rexp(n)
#grpahe général
par(mfrow=c(1,2))
p<-ggplot(NULL,aes(x=y,y=..density..))+geom_histogram(binwidth=0.8,fill="grey60",coulour="black",size=0.2)+geom_density()
p + annotate("text", x=5, y=0.3, parse=TRUE,
             label="Y==-frac(log(U),lambda)")

p<-ggplot(NULL,aes(x=exp,y=..density..))+geom_histogram(binwidth=0.8,fill="pink",coulour="black",size=0.2)+geom_density()
p + annotate("text", x=5, y=0.3, parse=TRUE,
             label=" Y==lambda*e^{-lambda*x}")

#Loi de Weidbull

#Loi de Poisson 
n=10^4
lambda=100
spread=3*sqrt(lambda)
t=round(seq(max(0,lambda-spread),lambda+spread,1))
prob=ppois(t, lambda)
Y=rep(0,n)
for (i in 1:n){
  u=runif(1)
  Y[i]=t[1]+sum(prob<u) }

poisson<-rpois(n,lambda)


hist(poiss,col="pink",breaks=20)

help(hist)
poisson_r<-ggplot(prob)
poisson_R<-ggplot(NULL,aes(x=poisson,y=..density..))+
  geom_histogram(binwidth=0.8,fill="pink",coulour="black",size=0.1)+geom_density()
poisson_R + annotate("text", x=125, y=0.04, parse=TRUE,
             label="Y==frac(lambda^(k)*e^{-lambda},factorial(k))")
poisson_R 
poisson_inv<-ggplot(NULL,aes(x=Y,y=..density..))+geom_histogram(binwidth=0.8,fill="grey60",coulour="black",size=0.2)+geom_density()
poisson_inv+annotate("text", x=125, y=0.04, parse=TRUE,
                    label="Y==t+sum(prob<u) ")
poisson_inv


#centrale limite
n<-10000
ytcl<-rep(0,n)
for(i in 1:n){
  x=runif(n)
  ytcl[i] =(sum(x)-n/2)/sqrt(n/12) 
  }

tcl<-ggplot(NULL,aes(x=ytcl,y=..density..))+
  geom_histogram(binwidth=0.3,fill="grey60",coulour="black",size=0.2)+geom_density()
tcl+annotate("text", x=-3.5, y=0.3, parse=TRUE,
             label="Y==frac(sum(X[i],i=1,n)-n/2,sqrt(n/12))")

normal<-rnorm(n)
normalp<-ggplot(NULL,aes(x=normal,y=..density..))+
  geom_histogram(binwidth=0.8,fill="pink",coulour="black",size=0.1)+geom_density()
normalp +annotate("text", x=2.5, y=0.3, parse=TRUE,
                     label="Y==frac(1,sqrt(2*pi))*e^{frac(-x^2,2)}")

sqrt(2)
sd(ytcl)
mean(ytcl)
hist(ytcl)

#Box Muller 


size = 15000
u = runif(size) 
v = runif(size)

y1=rep(0,size)
y2=rep(0,size)

for (i in 1:size){
  y1[i] = sqrt(-2*log(u[i]))*cos(2*pi*v[i])
  y2[i] = sqrt(-2*log(u[i]))*sin(2*pi*v[i])
}


box_muller<-c(y1,y2)

pbox<-ggplot(NULL,aes(x=box_muller,y=..density..))+
  geom_histogram(binwidth=0.3,fill="grey60",coulour="black",size=0.2)+geom_density()
pbox+annotate("text", x=2, y=0.3, parse=TRUE,
             label="Y ==f(Y1,Y2)")

normal<-rnorm(size)
normalp<-ggplot(NULL,aes(x=normal,y=..density..))+
  geom_histogram(binwidth=10,fill="pink",coulour="black",size=0.1)+geom_density()
normalp +annotate("text", x=2.5, y=0.3, parse=TRUE,
                  label="Y==frac(1,sqrt(2*pi))*e^{frac(-x^2,2)}")

system.time()

#loi Binomial

Uni_binom<-function(s0,n0,p0){
  cp=pbinom(c(0:n0),n0,p0)
  X=array(0,c(s0,1))
  for (i in 1:s0){
    u=runif(1)
    X[i]=sum(cp<u)
  }
  return(X)
}
uni_Bin<-Uni_binom(8000,30,0.5)

pUni_binom<-ggplot(NULL,aes(x=uni_Bin,y=..density..))+
  geom_histogram(binwidth=1,fill="grey60",coulour="black",size=0.2)+geom_density()
pUni_binom+annotate("text", x=20, y=0.15, parse=TRUE,
              label="Y==sum(P(U<p))")


help("rbinom")
Rbinom<-rbinom(8000,30,0.5)
hist(Rbinom)

pRbinom<-ggplot(NULL,aes(x=Rbinom,y=..density..))+
  geom_histogram(binwidth=1,fill="pink",coulour="black",size=0.1)+geom_density()
pRbinom+annotate("text", x=20, y=0.15, parse=TRUE,
                  label=" Y %~% B(8000,0.5)" )




#Loi géométriqque 
help(rgeom)
Dgeom<-rpareto(8000,1/2)
pgeom<-ggplot(NULL,aes(x=Dgeom,y=..density..))+
  geom_histogram(binwidth=0.8,fill="pink",coulour="black",size=0.1)+geom_density()
pgeom+annotate("text", x=10, y=0.75, parse=TRUE,
                 label=" Y %~% G(8000,0.5)" )



Uni_geom<-function(n,p){
  U =runif(n)
  X<-rep(0,n)
  for (i in 1:n){

    X[i]=1+log(U[i]/log(1-p))
  }
  return(X)
}

Uni_geom(8000,1/2)
rpareto(n, m, s)


n=8000
x<-runif(n)


##loi de weibull
uni_weib<-(log(x)^(1/6))
puni_weib<-ggplot(NULL,aes(x=uni_weib,y=..density..))+
  geom_histogram(binwidth=0.8,fill="grey",coulour="black",size=0.1)+geom_density()+scale_x_sqrt()
puni_weib+annotate("text", x=20, y=0.4, parse=TRUE,
               label=" Y==-(log(u))^k" )
help("geom_histogram")

plot(df1)

help("rweibull")
library(stats)
weib<-rweibull(8000,1)
pweib<-ggplot(NULL,aes(x=weib,y=..density..))+
  geom_histogram(binwidth=0.8,fill="pink",coulour="black",size=0.1)+geom_density()
pweib
+annotate("text", x=20, y=0.4, parse=TRUE,
              label=" Y==1-e^(-x^(k))" )

system.time
