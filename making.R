con4<-dbConnect(RMySQL::MySQL(),dbname="guidme",host="localhost" , port=3306 , user="root" , password="")

p_type<-"select * from place_and_t_type where is_used=0"
type<-dbGetQuery(con4,p_type)

x<-1
count<-nrow(type)
pname<-"none"
#Htotal_time<-0
Stime<-8
################################################################
check_tour<-function(p_city,ptime,tour_type){
  num<-1
  f<-0
  city<-dbGetQuery(con4,paste0("select time_left,place_name5,tourism_type_ID,ID from tour_plans where City_ID='",p_city,"'"))
  thecoun<-nrow(city)
  while (thecoun>0) {
    if(!is.null(city[num,3])==FALSE && city[num,3]==tour_type&& city[num,1]>=ptime&& !is.null(city[num,2])){
      f<-(city[num,4])
      return(f)}
    else{thecoun<-thecoun-1
    num<-num+1}}
  return(f<-0)
}
################################################################
update_time<-function(ID,ptime,n,p_city){
  time_up<-paste0("update tour_plans set total_time=total_time +'",ptime,"', time_left=time_left -'",ptime,"' where ID='",ID,"'")
  update<-dbSendQuery(con4,time_up)
  if(n>0){
    switch(n,"one"={check_time<-dbGetQuery(con4,paste0("select time2 from tour_plans where ID='",ID,"'"))
    uptime<-dbSendQuery(con4,paste0("update tour_plans set time3='",check_time+ptime,"'where ID='",ID,"'"))},
    "Two"={check_time<-dbGetQuery(con4,paste0("select time3 from tour_plans where ID='",ID,"'"))
    uptime<-dbSendQuery(con4,paste0("update tour_plans set time4='",check_time+ptime,"'where ID='",ID,"'"))},
    "three"={check_time<-dbGetQuery(con4,paste0("select time4 from tour_plans where ID='",ID,"'"))
    uptime<-dbSendQuery(con4,paste0("update tour_plans set time5='",check_time+ptime,"'where ID='",ID,"'"))})
  }}


################################################################
historic_tour_plans<-function(pname){
  p_in<-paste0("select time_take, CityID from place_info where place_name='",pname,"'")
  p_info<-dbGetQuery(con4,p_in)
  if(is.null(p_info[1,1])&& p_info[1,1]== 0){
    ptime<-dbGetQuery(con4,paste0("select avg_time from place_natural where ID=1"))}
  else{ptime<-p_info[1,1]}
  p_city<-p_info[1,2]
  timeleft<-12-ptime
  test1<-paste0("SELECT * from tour_plans where City_ID='",p_city,"'")
  if(is.null(dbGetQuery(con4,test1))==TRUE){tour1<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(1,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
  tour_p1<-dbSendQuery(con4,tour1)}
  else{ID<-check_tour(p_city,ptime,1)
  print(ID)
  if(ID==0){
    tour<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(1,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
    tour_p<-dbSendQuery(con4,tour)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name2 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name2='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour) 
    update_time(ID,ptime,1,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name3 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name3='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,2,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name4 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name4='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,3,p_city)}
  else {tour<-paste0("update tour_plans set place_name5='",pname,"'where ID='",ID,"'")
  tour_p<-dbSendQuery(con4,tour)
   }}
  use<-paste0("update place_and_t_type set is_used=1 where p_name='",pname,"'")
  used<-dbSendQuery(con4,use)
}

###############################################################
shopping_tour_plans<-function(pname){
  p_in<-paste0("select time_take, CityID from place_info where place_name='",pname,"'")
  p_info<-dbGetQuery(con4,p_in)
  if(is.null(p_info[1,1])&& p_info[1,1]== 0){
    ptime<-dbGetQuery(con4,paste0("select avg_time from place_natural where ID=2"))}
  else{ptime<-p_info[1,1]}
  p_city<-p_info[1,2]
  timeleft<-12-ptime
  test1<-paste0("SELECT * from tour_plans where City_ID='",p_city,"'")
  if(is.null(dbGetQuery(con4,test1))==TRUE){tour1<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(2,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
  tour_p1<-dbSendQuery(con4,tour1)}
  else{ID<-check_tour(p_city,ptime,2)
  print(ID)
  if(ID==0){
    tour<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(2,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
    tour_p<-dbSendQuery(con4,tour)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name2 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name2='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour) 
    update_time(ID,ptime,1,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name3 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name3='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,2,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name4 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name4='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,3,p_city)}
  else {tour<-paste0("update tour_plans set place_name5='",pname,"'where ID='",ID,"'")
  tour_p<-dbSendQuery(con4,tour)
   }}
  use<-paste0("update place_and_t_type set is_used=1 where p_name='",pname,"'")
  used<-dbSendQuery(con4,use)
}
###############################################################
natural_tour_plans<-function(pname){
  p_in<-paste0("select time_take, CityID from place_info where place_name='",pname,"'")
  p_info<-dbGetQuery(con4,p_in)
  if(is.null(p_info[1,1])&& p_info[1,1]== 0){
    ptime<-dbGetQuery(con4,paste0("select avg_time from place_natural where ID=3"))}
  else{ptime<-p_info[1,1]}
  p_city<-p_info[1,2]
  timeleft<-12-ptime
  test1<-paste0("SELECT * from tour_plans where City_ID='",p_city,"'")
  if(is.null(dbGetQuery(con4,test1))==TRUE){tour1<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(3,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
  tour_p1<-dbSendQuery(con4,tour1)}
  else{ID<-check_tour(p_city,ptime,3)
  print(ID)
  if(ID==0){
    tour<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(3,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
    tour_p<-dbSendQuery(con4,tour)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name2 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name2='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour) 
    update_time(ID,ptime,1,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name3 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name3='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,2,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name4 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name4='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,3,p_city)}
  else {tour<-paste0("update tour_plans set place_name5='",pname,"'where ID='",ID,"'")
  tour_p<-dbSendQuery(con4,tour)
   }}
  use<-paste0("update place_and_t_type set is_used=1 where p_name='",pname,"'")
  used<-dbSendQuery(con4,use)
}
###############################################################
exhibitions_tour_plans<-function(pname){
  p_in<-paste0("select time_take, CityID from place_info where place_name='",pname,"'")
  p_info<-dbGetQuery(con4,p_in)
  if(is.null(p_info[1,1])&& p_info[1,1]== 0){
    ptime<-dbGetQuery(con4,paste0("select avg_time from place_natural where ID=4"))}
  else{ptime<-p_info[1,1]}
  p_city<-p_info[1,2]
  timeleft<-12-ptime
  test1<-paste0("SELECT * from tour_plans where City_ID='",p_city,"'")
  if(is.null(dbGetQuery(con4,test1))==TRUE){tour1<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(4,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
  tour_p1<-dbSendQuery(con4,tour1)}
  else{ID<-check_tour(p_city,ptime,4)
  print(ID)
  if(ID==0){
    tour<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(4,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
    tour_p<-dbSendQuery(con4,tour)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name2 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name2='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour) 
    update_time(ID,ptime,1,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name3 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name3='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,2,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name4 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name4='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,3,p_city)}
  else {tour<-paste0("update tour_plans set place_name5='",pname,"'where ID='",ID,"'")
  tour_p<-dbSendQuery(con4,tour)
   }}
  use<-paste0("update place_and_t_type set is_used=1 where p_name='",pname,"'")
  used<-dbSendQuery(con4,use)
}
###############################################################
Festivals_tour_plans<-function(pname){
  p_in<-paste0("select time_take, CityID from place_info where place_name='",pname,"'")
  p_info<-dbGetQuery(con4,p_in)
  if(is.null(p_info[1,1])&& p_info[1,1]== 0){
    ptime<-dbGetQuery(con4,paste0("select avg_time from place_natural where ID=5"))}
  else{ptime<-p_info[1,1]}
  p_city<-p_info[1,2]
  timeleft<-12-ptime
  test1<-paste0("SELECT * from tour_plans where City_ID='",p_city,"'")
  if(is.null(dbGetQuery(con4,test1))==TRUE){tour1<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(5,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
  tour_p1<-dbSendQuery(con4,tour1)}
  else{ID<-check_tour(p_city,ptime,5)
  print(ID)
  if(ID==0){
    tour<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(5,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
    tour_p<-dbSendQuery(con4,tour)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name2 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name2='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour) 
    update_time(ID,ptime,1,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name3 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name3='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,2,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name4 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name4='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,3,p_city)}
  else {tour<-paste0("update tour_plans set place_name5='",pname,"'where ID='",ID,"'")
  tour_p<-dbSendQuery(con4,tour)
   }}
  use<-paste0("update place_and_t_type set is_used=1 where p_name='",pname,"'")
  used<-dbSendQuery(con4,use)
}
###############################################################
Hiking_tour_plans<-function(pname){
  p_in<-paste0("select time_take, CityID from place_info where place_name='",pname,"'")
  p_info<-dbGetQuery(con4,p_in)
  if(is.null(p_info[1,1])&& p_info[1,1]== 0){
    ptime<-dbGetQuery(con4,paste0("select avg_time from place_natural where ID=8"))}
  else{ptime<-p_info[1,1]}
  p_city<-p_info[1,2]
  timeleft<-12-ptime
  test1<-paste0("SELECT * from tour_plans where City_ID='",p_city,"'")
  if(is.null(dbGetQuery(con4,test1))==TRUE){tour1<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(8,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
  tour_p1<-dbSendQuery(con4,tour1)}
  else{ID<-check_tour(p_city,ptime,8)
  print(ID)
  if(ID==0){
    tour<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(8,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
    tour_p<-dbSendQuery(con4,tour)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name2 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name2='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour) 
    update_time(ID,ptime,1,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name3 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name3='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,2,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name4 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name4='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,3,p_city)}
  else {tour<-paste0("update tour_plans set place_name5='",pname,"'where ID='",ID,"'")
  tour_p<-dbSendQuery(con4,tour)
   }}
  use<-paste0("update place_and_t_type set is_used=1 where p_name='",pname,"'")
  used<-dbSendQuery(con4,use)
}
###############################################################
Youth_tour_plans<-function(pname){
  p_in<-paste0("select time_take, CityID from place_info where place_name='",pname,"'")
  p_info<-dbGetQuery(con4,p_in)
  if(is.null(p_info[1,1])&& p_info[1,1]== 0){
    ptime<-dbGetQuery(con4,paste0("select avg_time from place_natural where ID=9"))}
  else{ptime<-p_info[1,1]}
  p_city<-p_info[1,2]
  timeleft<-12-ptime
  test1<-paste0("SELECT * from tour_plans where City_ID='",p_city,"'")
  if(is.null(dbGetQuery(con4,test1))==TRUE){tour1<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(9,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
  tour_p1<-dbSendQuery(con4,tour1)}
  else{ID<-check_tour(p_city,ptime,9)
  print(ID)
  if(ID==0){
    tour<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(9,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
    tour_p<-dbSendQuery(con4,tour)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name2 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name2='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour) 
    update_time(ID,ptime,1,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name3 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name3='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,2,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name4 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name4='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,3,p_city)}
  else {tour<-paste0("update tour_plans set place_name5='",pname,"'where ID='",ID,"'")
  tour_p<-dbSendQuery(con4,tour)
   }}
  use<-paste0("update place_and_t_type set is_used=1 where p_name='",pname,"'")
  used<-dbSendQuery(con4,use)
}
###############################################################
Entertainment_tour_plans<-function(pname){
  p_in<-paste0("select time_take, CityID from place_info where place_name='",pname,"'")
  p_info<-dbGetQuery(con4,p_in)
  if(is.null(p_info[1,1])&& p_info[1,1]== 0){
    ptime<-dbGetQuery(con4,paste0("select avg_time from place_natural where ID=10"))}
  else{ptime<-p_info[1,1]}
  p_city<-p_info[1,2]
  timeleft<-12-ptime
  test1<-paste0("SELECT * from tour_plans where City_ID='",p_city,"'")
  if(is.null(dbGetQuery(con4,test1))==TRUE){tour1<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(10,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
  tour_p1<-dbSendQuery(con4,tour1)}
  else{ID<-check_tour(p_city,ptime,10)
  print(ID)
  if(ID==0){
    tour<-paste0("insert into tour_plans (tourism_type_ID,place_name1,time1,total_time,City_ID,time_left,time2)values(10,'",pname,"','",Stime,"','",ptime,"','",p_city,"','",timeleft,"','",Stime+ptime,"')")
    tour_p<-dbSendQuery(con4,tour)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name2 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name2='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour) 
    update_time(ID,ptime,1,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name3 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name3='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,2,p_city)}
  else if(!is.null(dbGetQuery(con4,paste0("select place_name4 from tour_plans where ID='",ID,"'")))){
    tour<-paste0("update tour_plans set place_name4='",pname,"'where ID='",ID,"'")
    tour_p<-dbSendQuery(con4,tour)
    update_time(ID,ptime,3,p_city)}
  else {tour<-paste0("update tour_plans set place_name5='",pname,"'where ID='",ID,"'")
  tour_p<-dbSendQuery(con4,tour)
   }}
  use<-paste0("update place_and_t_type set is_used=1 where p_name='",pname,"'")
  used<-dbSendQuery(con4,use)
}
###############################################################
while (count>0) {
  if (type[x,1]==1){
    pname<-type[x,2]
    historic_tour_plans(pname)} 
else if (type[x,1]==2){
  pname<-type[x,2]
  shopping_tour_plans(pname)}
else if (type[x,1]==3){
  pname<-type[x,2]
  natural_tour_plans(pname)}
else if (type[x,1]==4){
  pname<-type[x,2]
  exhibitions_tour_plans(pname)}
else if (type[x,1]==5){
    pname<-type[x,2]
    Festivals_tour_plans(pname)} 
else if (type[x,1]==8){
    pname<-type[x,2]
    Hiking_tour_plans(pname)} 
else if (type[x,1]==9){
    pname<-type[x,2]
    Youth_tour_plans(pname)}
else{pname<-type[x,2]
    Entertainment_tour_plans(pname)}
 count<-count-1
  x<-x+1
}

dbDisconnect(con4)
