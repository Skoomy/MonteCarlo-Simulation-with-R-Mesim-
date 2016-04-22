
###Recuit simulé In R
f<-function(x){
  return (5/49*(x-10)^2+4)
}
p=ggplot(data.frame(x=c(0, 2)), aes(x)) + stat_function(fun=f)




recuit<-function(f,x0,Ti,Tf,ampli,alpha,Max_etat,Iter)

  { 
  l=vector(,Iter)
  x=xopt=x0;
  fx=fxopt=f(x0);
  Tmp=Ti;
  ess= 0;
  
     while(Tmp>Tf&&xopt<3){
    etat=0;
    for(i in 1:Iter){
      y=x+ampli*runif(min=0,max=1,n=1)-0.5;
      fy=f(y);
          if(fy-fx<0){
          x=y;
          fx=fy;
        if(fx<fxopt){
          xopt=x;
          fxopt=f(xopt);
        }
        etat=etat+1;
      }
      else{
        if(runif(min=0,max=0.1,n = 1)<=exp(-(fy-fx)/Tmp)){
          x=y;
          fx=fy;
          etat=etat+1;
        }
      }
      ess=ess+1;
      if(etat==Max_etat)
      {break;}
      
      l[i]=Tmp
    }
    
    Tmp=Tmp*alpha
    
  } #end while
  plot(l)
     return (list(Xopt=xopt,Fxopt=f(xopt)))}


recuit(f,2,100,1,1,0.001,200,2000)

