library(ggmap)
library(maptools)
library(maps)

visited <- c("Juneau","Ottawa","Jakarta","Stockholm","Beirut","Tokyo","Tripoli","Dublin","Addis-Abeba","New York","Havane","Nairobi","Brasilia","Téhéran","Madrid","New Delhi","Moscou","Paris", "SF", "Abidjan", "Melbourne", "Caracas","Buenos Aires","Harare","Mexico")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
ll.visited$Name<-visited
cities=ll.visited
n=nrow(ll.visited)

  

distance <-function(cities)
{
d = 0;
for (n in 1 : nrow(cities) ) {
if ( n == nrow(cities) ) {
d = d + sqrt((cities$lon[n]-cities$lon[1])^2 +(cities$lat[n]-cities$lat[1] )^2)} 
else
  d = d + sqrt((cities$lon[n]-cities$lon[n+1])^2 +(cities$lat[n]-cities$lat[n+1] )^2 );
}
return(d)
}


swapcities<- function(cities,n)
{
s = cities; 
for (i in 1 : n)
city_1 = ceiling(nrow(cities)*runif(max=1,n=1 ,min=0));
if (city_1 < 1)
{city_1 = 1;}

city_2 = ceiling(length(cities)*runif(max=1,n=1,min=0));
if (city_2 < 1)
city_2 = 1;
temp = s[city_1,]
s[city_1,] = s[city_2,];
s[city_2,] = temp;
return (s) }

###############################################"
Recrut_Voyage<-function(cities,Init_Temp,Tf,alpha,threshold,numberofcitiestoswap)
{temperature=Init_Temp
  iterations=1
  complete_temperature_iterations=0
  while (iterations < threshold)
  {
    
    previous_distance = distance(cities);
    temp_cities = swapcities(cities,numberofcitiestoswap)
    current_distance = distance(temp_cities);
    diff = abs(current_distance - previous_distance);
    if (current_distance < previous_distance) {
    cities = temp_cities;
    #plotcities(cities);
    if (complete_temperature_iterations >= 10)
    {temperature = alpha*temperature;
    complete_temperature_iterations = 0;
    }
    numberofcitiestoswap = ceiling(numberofcitiestoswap*exp(-diff/(iterations*temperature)));
    if (numberofcitiestoswap == 0)
    {numberofcitiestoswap = 1;
    }
    iterations = iterations + 1;
    complete_temperature_iterations = complete_temperature_iterations*alpha;}

    else
      if (runif(max=1,n=1,min=0) < exp(-diff/(temperature)) )
    {cities = temp_cities;
    
    numberofcitiestoswap = ceiling(numberofcitiestoswap*exp(-diff/(iterations*temperature)));
    if(numberofcitiestoswap == 0)
    {numberofcitiestoswap = 1;
      }
    complete_temperature_iterations = complete_temperature_iterations* alpha;
    iterations = iterations + 1;
      }
    }
  mapWorld <- borders("world", colour="gray50", fill="gray80") # create a layer of borders
  mp <- ggplot() +mapWorld+geom_point(dat=ll.visited,aes(x=lon, y=lat,label=Name) ,color="blue", size=0.8)
  
  a=mp+geom_segment(aes(x = cities$lon[1], y = cities$lat[1], xend = cities$lon[2], yend = cities$lat[2], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[2], y = cities$lat[2], xend = cities$lon[3], yend = cities$lat[3], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[3], y = cities$lat[3], xend = cities$lon[4], yend = cities$lat[4], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[4], y = cities$lat[4], xend = cities$lon[5], yend = cities$lat[5], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[5], y = cities$lat[5], xend = cities$lon[6], yend = cities$lat[6], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[6], y = cities$lat[6], xend = cities$lon[7], yend = cities$lat[7], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[7], y = cities$lat[7], xend = cities$lon[8], yend = cities$lat[8], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[8], y = cities$lat[8], xend = cities$lon[9], yend = cities$lat[9], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[9], y = cities$lat[9], xend = cities$lon[10], yend = cities$lat[10], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[10], y = cities$lat[10], xend = cities$lon[11], yend = cities$lat[11], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[11], y = cities$lat[11], xend = cities$lon[12], yend = cities$lat[12], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[12], y = cities$lat[12], xend = cities$lon[13], yend = cities$lat[13], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[13], y = cities$lat[13], xend = cities$lon[14], yend = cities$lat[14], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))+
    geom_segment(aes(x = cities$lon[14], y = cities$lat[14], xend = cities$lon[15], yend = cities$lat[15], colour = "segment"),show.legend=FALSE,size=0.08, data = cities, arrow = arrow(length = unit(0.3, "cm")))
  
    return(list(a,cities$Name,previous_distance,iterations,temperature,complete_temperature_iterations))
  }

Recrut_Voyage(cities,10000,1,0.99,200,2)

