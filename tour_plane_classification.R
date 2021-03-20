con<-dbConnect(RMySQL::MySQL(),dbname="guidme",host="localhost" , port=3306 , user="root" , password="")
query<-"select place_name,Fplace_naturalID,Splace_naturalID,Fplace_descriptionsID,Splace_descriptionsID,Tplace_descriptionsID,Four_place_descriptionsID from place_info where statusID=2"
place<-dbGetQuery(con,query)
x<-1
count<-count2<-nrow(place)
discription<- function(x, y){
  
  switch(place[x,y],
         "one"={heal<-paste0("update place_info set Fhealth_constrainID=2 ,Shealth_constrainID=3 where place_name='",name,"'")
         health<-dbSendQuery(con,heal)},
         "Two"={heal<-paste0("update place_info set Fhealth_constrainID=1 ,Shealth_constrainID=7 where place_name='",name,"'")
         health<-dbSendQuery(con,heal)},
         "Three"={heal<-paste0("update place_info set Fhealth_constrainID=1 ,Shealth_constrainID=5 where place_name='",name,"'")
         health<-dbSendQuery(con,heal)},
         "Four"={heal<-paste0("update place_info set Fhealth_constrainID=1 ,Shealth_constrainID=6 where place_name='",name,"'")
         health<-dbSendQuery(con,heal)},
         "Five"={heal<-paste0("update place_info set Fhealth_constrainID=1 ,Shealth_constrainID=4 where place_name='",name,"'")
         health<-dbSendQuery(con,heal)})
}

while(count>0){
  name<-place[x,1]
  if(place[x,2]==1){
    histo<-paste0("insert into place_and_t_type(t_ID,p_name) values(1,'",place[x,1],"')")
    historic<-dbSendQuery(con,histo)
    if(place[x,3]==2){
      shop<-paste0("insert into place_and_t_type(t_ID,p_name) values(2,'",place[x,1],"')")
      shopping<-dbSendQuery(con,shop)
      count<-count-1 
      x<-x+1 }
    else {count<-count-1 
    x<-x+1}}
  else if(place[x,2]==2) {
    shop<-paste0("insert into place_and_t_type(t_ID,p_name) values(2,'",place[x,1],"')")
    shopping<-dbSendQuery(con,shop) 
    if(!!is.null(place[x,3])==FALSE & place[x,3]==12){
      enter<-paste0("insert into place_and_t_type(t_ID,p_name) values(10,'",place[x,1],"')")
      entertament<-dbSendQuery(con,enter)
      count<-count-1 
      x<-x+1}else{count<-count-1 
      x<-x+1} }
  else if(any(place[x,2]==3:8)){
    natu<-paste0("insert into place_and_t_type(t_ID,p_name) values(3,'",place[x,1],"')")
    natural<-dbSendQuery(con,natu)
    if(!is.null(place[x,3])==FALSE & place[x,3]==14){
      adven<-paste0("insert into place_and_t_type(t_ID,p_name) values(8,'",place[x,1],"')")
      adventure<-dbSendQuery(con,adven)
      count<-count-1 
      x<-x+1 }
    else {count<-count-1 
    x<-x+1}}
  else if(place[x,2]==9){
    exhab<-paste0("insert into place_and_t_type(t_ID,p_name) values(4,'",place[x,1],"')")
    exhabit<-dbSendQuery(con,exhab)
    if(!is.null(place[x,3])==FALSE & place[x,3]==1){
      histo<-paste0("insert into place_and_t_type(t_ID,p_name) values(1,'",place[x,1],"')")
      historic<-dbSendQuery(con,histo)
      count<-count-1 
      x<-x+1}
    else{ count<-count-1 
    x<-x+1} }
  else if(any (place[x,2]==10:11) ){
    you<-paste0("insert into place_and_t_type(t_ID,p_name) values(9,'",place[x,1],"')")
    youth<-dbSendQuery(con,you)
    if(!is.null(place[x,3])==FALSE & place[x,3]==14){
      adven<-paste0("insert into place_and_t_type(t_ID,p_name) values(8,'",place[x,1],"')")
      adventure<-dbSendQuery(con,adven)
      count<-count-1 
      x<-x+1}else {count<-count-1 
      x<-x+1}}
  else if(place[x,2]==12){
    enter<-paste0("insert into place_and_t_type(t_ID,p_name) values(10,'",place[x,1],"')")
    entertament<-dbSendQuery(con,enter)
    if(!is.null(place[x,3])==FALSE & place[x,3]==2){
      shop<-paste0("insert into place_and_t_type(t_ID,p_name) values(2,'",place[x,1],"')")
      shopping<-dbSendQuery(con,shop)
      count<-count-1 
      x<-x+1 }
    else{count<-count-1 
    x<-x+1}}
  else if(place[x,2]==13){
    fast<-paste0("insert into place_and_t_type(t_ID,p_name) values(5,'",place[x,1],"')")
    fastival<-dbSendQuery(con,fast)
    shop<-paste0("insert into place_and_t_type(t_ID,p_name) values(2,'",place[x,1],"')")
    shopping<-dbSendQuery(con,shop)
    count<-count-1 
    x<-x+1}
  else {
    adven<-paste0("insert into place_and_t_type(t_ID,p_name) values(8,'",place[x,1],"')")
    adven
    adventure<-dbSendQuery(con,adven)
    count<-count-1 
    x<-x+1}
}#while place natural
x<-1
while(count2>0){
  discription(x,4)
  
  if(!is.null(place[x,5])==FALSE){
    discription(x,5)
    if(!is.null(place[x,6])==FALSE){
      discription(x,6)
      if(!is.null(place[x,7])==FALSE){
        discription(x,7)}
      else {stat<-paste0("update place_info set statusID=1  where place_name='",name,"'")
      status<-dbSendQuery(con,stat)
      count2<-count2-1
      x<-x+1}
    } else {stat<-paste0("update place_info set statusID=1  where place_name='",name,"'")
    status<-dbSendQuery(con,stat)
    count2<-count2-1
    x<-x+1} }
  else {stat<-paste0("update place_info set statusID=1  where place_name='",name,"'")
  status<-dbSendQuery(con,stat)
  count2<-count2-1
  x<-x+1}
  
  stat<-paste0("update place_info set statusID=1  where place_name='",name,"'")
  status<-dbSendQuery(con,stat)
  count2<-count2-1
  x<-x+1
  
}#while place description


dbDisconnect(con)




