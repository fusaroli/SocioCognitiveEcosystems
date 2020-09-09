## Functions

CreateKey <- function(Network_Data_intro, Network_Data_followup){
  Names <- data.frame(key = sort(unique(tolower(c(
    Network_Data_intro$survey.1.player.pid,
    Network_Data_intro$network.1.player.friend1,
    Network_Data_intro$network.1.player.friend2,
    Network_Data_intro$network.1.player.friend3,
    Network_Data_intro$network.1.player.friend4,
    Network_Data_intro$network.1.player.friend5,
    Network_Data_intro$network.1.player.friend6,
    Network_Data_intro$network.1.player.friend7,
    Network_Data_intro$network.1.player.friend8,
    Network_Data_intro$network.1.player.friend9,
    Network_Data_intro$network.1.player.friend10,
    Network_Data_followup$survey_followup.1.player.pid,
    Network_Data_followup$network_followup.1.player.friend1,
    Network_Data_followup$network_followup.1.player.friend2,
    Network_Data_followup$network_followup.1.player.friend3,
    Network_Data_followup$network_followup.1.player.friend4,
    Network_Data_followup$network_followup.1.player.friend5,
    Network_Data_followup$network_followup.1.player.friend6,
    Network_Data_followup$network_followup.1.player.friend7,
    Network_Data_followup$network_followup.1.player.friend8,
    Network_Data_followup$network_followup.1.player.friend9,
    Network_Data_followup$network_followup.1.player.friend10)
  ))))
  return(Names)
}

CreateEdges_intro <- function(Network_Data, key, uniqueKey){
  for (i in seq(nrow(key))){
    Network_Data$survey.1.player.pid[tolower(Network_Data$survey.1.player.pid)==tolower(key$key[i])] =  key$label[i]
    Network_Data$network.1.player.friend1[tolower(Network_Data$network.1.player.friend1)==tolower(key$key[i])] =  key$label[i]
    Network_Data$network.1.player.friend2[tolower(Network_Data$network.1.player.friend2)==tolower(key$key[i])] =  key$label[i]
    Network_Data$network.1.player.friend3[tolower(Network_Data$network.1.player.friend3)==tolower(key$key[i])] =  key$label[i]
    Network_Data$network.1.player.friend4[tolower(Network_Data$network.1.player.friend4)==tolower(key$key[i])] =  key$label[i]
    Network_Data$network.1.player.friend5[tolower(Network_Data$network.1.player.friend5)==tolower(key$key[i])] =  key$label[i]
    Network_Data$network.1.player.friend6[tolower(Network_Data$network.1.player.friend6)==tolower(key$key[i])] =  key$label[i]
    Network_Data$network.1.player.friend7[tolower(Network_Data$network.1.player.friend7)==tolower(key$key[i])] =  key$label[i]
    Network_Data$network.1.player.friend8[tolower(Network_Data$network.1.player.friend8)==tolower(key$key[i])] =  key$label[i]
    Network_Data$network.1.player.friend9[tolower(Network_Data$network.1.player.friend9)==tolower(key$key[i])] =  key$label[i]
    Network_Data$network.1.player.friend10[tolower(Network_Data$network.1.player.friend10)==tolower(key$key[i])] =  key$label[i]
  }
  
  d <- data.frame(from=rep(NA,nrow(Network_Data)*10),to=rep(NA,nrow(Network_Data)*10),weight=rep(NA,nrow(Network_Data)*10))
  n=1
  for (i in seq(nrow(Network_Data))){
    d$from[n] <- Network_Data$survey.1.player.pid[i]
    d$to[n] <- Network_Data$network.1.player.friend1[i]
    d$weight[n] <- Network_Data$network.1.player.scale1[i]
    n<-n+1
    d$from[n] <- Network_Data$survey.1.player.pid[i]
    d$to[n] <- Network_Data$network.1.player.friend2[i]
    d$weight[n] <- Network_Data$network.1.player.scale2[i]
    n<-n+1
    d$from[n] <- Network_Data$survey.1.player.pid[i]
    d$to[n] <- Network_Data$network.1.player.friend3[i]
    d$weight[n] <- Network_Data$network.1.player.scale3[i]
    n<-n+1
    d$from[n] <- Network_Data$survey.1.player.pid[i]
    d$to[n] <- Network_Data$network.1.player.friend4[i]
    d$weight[n] <- Network_Data$network.1.player.scale4[i]
    n<-n+1
    d$from[n] <- Network_Data$survey.1.player.pid[i]
    d$to[n] <- Network_Data$network.1.player.friend5[i]
    d$weight[n] <- Network_Data$network.1.player.scale5[i]
    n<-n+1
    d$from[n] <- Network_Data$survey.1.player.pid[i]
    d$to[n] <- Network_Data$network.1.player.friend6[i]
    d$weight[n] <- Network_Data$network.1.player.scale6[i]
    n<-n+1
    d$from[n] <- Network_Data$survey.1.player.pid[i]
    d$to[n] <- Network_Data$network.1.player.friend7[i]
    d$weight[n] <- Network_Data$network.1.player.scale7[i]
    n<-n+1
    d$from[n] <- Network_Data$survey.1.player.pid[i]
    d$to[n] <- Network_Data$network.1.player.friend8[i]
    d$weight[n] <- Network_Data$network.1.player.scale8[i]
    n<-n+1
    d$from[n] <- Network_Data$survey.1.player.pid[i]
    d$to[n] <- Network_Data$network.1.player.friend9[i]
    d$weight[n] <- Network_Data$network.1.player.scale9[i]
    n<-n+1
    d$from[n] <- Network_Data$survey.1.player.pid[i]
    d$to[n] <- Network_Data$network.1.player.friend10[i]
    d$weight[n] <- Network_Data$network.1.player.scale10[i]
    n<-n+1
  }
  
  edges <- subset(d,complete.cases(d))
  edges <- subset(edges, !(grepl("fake",edges$from)|grepl("fake",edges$to)))
  
  
  return(edges)
}

CreateEdges_followup <- function(Network_Data, key, uniqueKey){
  for (i in seq(nrow(key))){
    Network_Data$survey_followup.1.player.pid[tolower(Network_Data$survey_followup.1.player.pid)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend1[tolower(Network_Data$network_followup.1.player.friend1)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend2[tolower(Network_Data$network_followup.1.player.friend2)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend3[tolower(Network_Data$network_followup.1.player.friend3)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend4[tolower(Network_Data$network_followup.1.player.friend4)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend5[tolower(Network_Data$network_followup.1.player.friend5)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend6[tolower(Network_Data$network_followup.1.player.friend6)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend7[tolower(Network_Data$network_followup.1.player.friend7)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend8[tolower(Network_Data$network_followup.1.player.friend8)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend9[tolower(Network_Data$network_followup.1.player.friend9)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend10[tolower(Network_Data$network_followup.1.player.friend10)==key$key[i]] =  key$label[i]
  }
  
  d <- data.frame(from=rep(NA,nrow(Network_Data)*10),to=rep(NA,nrow(Network_Data)*10),weight=rep(NA,nrow(Network_Data)*10))
  n=1
  for (i in seq(nrow(Network_Data))){
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_followup.1.player.friend1[i]
    d$weight[n] <- Network_Data$network_followup.1.player.scale1[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_followup.1.player.friend2[i]
    d$weight[n] <- Network_Data$network_followup.1.player.scale2[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_followup.1.player.friend3[i]
    d$weight[n] <- Network_Data$network_followup.1.player.scale3[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_followup.1.player.friend4[i]
    d$weight[n] <- Network_Data$network_followup.1.player.scale4[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_followup.1.player.friend5[i]
    d$weight[n] <- Network_Data$network_followup.1.player.scale5[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_followup.1.player.friend6[i]
    d$weight[n] <- Network_Data$network_followup.1.player.scale6[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_followup.1.player.friend7[i]
    d$weight[n] <- Network_Data$network_followup.1.player.scale7[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_followup.1.player.friend8[i]
    d$weight[n] <- Network_Data$network_followup.1.player.scale8[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_followup.1.player.friend9[i]
    d$weight[n] <- Network_Data$network_followup.1.player.scale9[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_followup.1.player.friend10[i]
    d$weight[n] <- Network_Data$network_followup.1.player.scale10[i]
    n<-n+1
    
  }
  
  edges <- subset(d,complete.cases(d))
  nodes <- data.frame(ID = stri_trans_general(str = unique(Network_Data$survey_followup.1.player.pid), id = "Latin-ASCII"))
  
  edges <- subset(edges, !(grepl("fake",edges$from)|grepl("fake",edges$to)))
  nodes <- subset(nodes, !(grepl("fake",nodes$ID)))
  
  return(edges)
}

CreateEdges_cogsci19n1 <- function(Network_Data, key, uniqueKey){
  for (i in seq(nrow(key))){
    Network_Data$survey_followup.1.player.pid[tolower(Network_Data$survey_followup.1.player.pid)==key$key[i]] =  key$label[i]
    Network_Data$network_cogsci.1.player.friend1[tolower(Network_Data$network_cogsci.1.player.friend1)==key$key[i]] =  key$label[i]
    Network_Data$network_cogsci.1.player.friend2[tolower(Network_Data$network_cogsci.1.player.friend2)==key$key[i]] =  key$label[i]
    Network_Data$network_cogsci.1.player.friend3[tolower(Network_Data$network_cogsci.1.player.friend3)==key$key[i]] =  key$label[i]
    Network_Data$network_cogsci.1.player.friend4[tolower(Network_Data$network_cogsci.1.player.friend4)==key$key[i]] =  key$label[i]
    Network_Data$network_cogsci.1.player.friend5[tolower(Network_Data$network_cogsci.1.player.friend5)==key$key[i]] =  key$label[i]
    Network_Data$network_cogsci.1.player.friend6[tolower(Network_Data$network_cogsci.1.player.friend6)==key$key[i]] =  key$label[i]
    Network_Data$network_cogsci.1.player.friend7[tolower(Network_Data$network_cogsci.1.player.friend7)==key$key[i]] =  key$label[i]
    Network_Data$network_cogsci.1.player.friend8[tolower(Network_Data$network_cogsci.1.player.friend8)==key$key[i]] =  key$label[i]
    Network_Data$network_cogsci.1.player.friend9[tolower(Network_Data$network_cogsci.1.player.friend9)==key$key[i]] =  key$label[i]
    Network_Data$network_cogsci.1.player.friend10[tolower(Network_Data$network_cogsci.1.player.friend10)==key$key[i]] =  key$label[i]
  }
  
  d <- data.frame(from=rep(NA,nrow(Network_Data)*10),to=rep(NA,nrow(Network_Data)*10),weight=rep(NA,nrow(Network_Data)*10))
  n=1
  for (i in seq(nrow(Network_Data))){
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_cogsci.1.player.friend1[i]
    d$weight[n] <- Network_Data$network_cogsci.1.player.scale1[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_cogsci.1.player.friend2[i]
    d$weight[n] <- Network_Data$network_cogsci.1.player.scale2[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_cogsci.1.player.friend3[i]
    d$weight[n] <- Network_Data$network_cogsci.1.player.scale3[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_cogsci.1.player.friend4[i]
    d$weight[n] <- Network_Data$network_cogsci.1.player.scale4[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_cogsci.1.player.friend5[i]
    d$weight[n] <- Network_Data$network_cogsci.1.player.scale5[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_cogsci.1.player.friend6[i]
    d$weight[n] <- Network_Data$network_cogsci.1.player.scale6[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_cogsci.1.player.friend7[i]
    d$weight[n] <- Network_Data$network_cogsci.1.player.scale7[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_cogsci.1.player.friend8[i]
    d$weight[n] <- Network_Data$network_cogsci.1.player.scale8[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_cogsci.1.player.friend9[i]
    d$weight[n] <- Network_Data$network_cogsci.1.player.scale9[i]
    n<-n+1
    d$from[n] <- Network_Data$survey_followup.1.player.pid[i]
    d$to[n] <- Network_Data$network_cogsci.1.player.friend10[i]
    d$weight[n] <- Network_Data$network_cogsci.1.player.scale10[i]
    n<-n+1
    
  }
  
  edges <- subset(d,complete.cases(d))
  nodes <- data.frame(ID = stri_trans_general(str = unique(Network_Data$survey_followup.1.player.pid), id = "Latin-ASCII"))
  
  edges <- subset(edges, !(grepl("fake",edges$from)|grepl("fake",edges$to)))
  nodes <- subset(nodes, !(grepl("fake",nodes$ID)))
  
  return(edges)
}

CreateEdges_cogsci19_label <- function(Network_Data, key, uniqueKey, label="Studious"){
  key$label <- tolower(key$label)
  
  for (i in seq(nrow(key))){
    Network_Data$survey_followup.1.player.pid[tolower(Network_Data$survey_followup.1.player.pid)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.studymate1[tolower(Network_Data$network_followup.1.player.studymate1)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.studymate2[tolower(Network_Data$network_followup.1.player.studymate2)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.studymate3[tolower(Network_Data$network_followup.1.player.studymate3)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.studymate4[tolower(Network_Data$network_followup.1.player.studymate4)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.studymate5[tolower(Network_Data$network_followup.1.player.studymate5)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student1[tolower(Network_Data$network_followup.1.player.student1)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student2[tolower(Network_Data$network_followup.1.player.student2)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student3[tolower(Network_Data$network_followup.1.player.student3)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student4[tolower(Network_Data$network_followup.1.player.student4)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student5[tolower(Network_Data$network_followup.1.player.student5)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student6[tolower(Network_Data$network_followup.1.player.student6)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student7[tolower(Network_Data$network_followup.1.player.student7)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student8[tolower(Network_Data$network_followup.1.player.student8)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student9[tolower(Network_Data$network_followup.1.player.student9)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student10[tolower(Network_Data$network_followup.1.player.student10)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student11[tolower(Network_Data$network_followup.1.player.student11)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student12[tolower(Network_Data$network_followup.1.player.student12)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student13[tolower(Network_Data$network_followup.1.player.student13)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student14[tolower(Network_Data$network_followup.1.player.student14)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student15[tolower(Network_Data$network_followup.1.player.student15)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student16[tolower(Network_Data$network_followup.1.player.student16)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student17[tolower(Network_Data$network_followup.1.player.student17)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student18[tolower(Network_Data$network_followup.1.player.student18)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student19[tolower(Network_Data$network_followup.1.player.student19)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student20[tolower(Network_Data$network_followup.1.player.student20)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student21[tolower(Network_Data$network_followup.1.player.student21)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student22[tolower(Network_Data$network_followup.1.player.student22)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student23[tolower(Network_Data$network_followup.1.player.student23)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student24[tolower(Network_Data$network_followup.1.player.student24)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student25[tolower(Network_Data$network_followup.1.player.student25)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student26[tolower(Network_Data$network_followup.1.player.student26)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student27[tolower(Network_Data$network_followup.1.player.student27)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student28[tolower(Network_Data$network_followup.1.player.student28)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student29[tolower(Network_Data$network_followup.1.player.student29)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student30[tolower(Network_Data$network_followup.1.player.student30)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student31[tolower(Network_Data$network_followup.1.player.student31)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student32[tolower(Network_Data$network_followup.1.player.student32)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student33[tolower(Network_Data$network_followup.1.player.student33)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student34[tolower(Network_Data$network_followup.1.player.student34)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student35[tolower(Network_Data$network_followup.1.player.student35)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student36[tolower(Network_Data$network_followup.1.player.student36)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student37[tolower(Network_Data$network_followup.1.player.student37)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student38[tolower(Network_Data$network_followup.1.player.student38)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student39[tolower(Network_Data$network_followup.1.player.student39)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student40[tolower(Network_Data$network_followup.1.player.student40)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student41[tolower(Network_Data$network_followup.1.player.student41)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student42[tolower(Network_Data$network_followup.1.player.student42)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student43[tolower(Network_Data$network_followup.1.player.student43)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student44[tolower(Network_Data$network_followup.1.player.student44)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student45[tolower(Network_Data$network_followup.1.player.student45)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student46[tolower(Network_Data$network_followup.1.player.student46)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student47[tolower(Network_Data$network_followup.1.player.student47)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student48[tolower(Network_Data$network_followup.1.player.student48)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student49[tolower(Network_Data$network_followup.1.player.student49)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student50[tolower(Network_Data$network_followup.1.player.student50)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student51[tolower(Network_Data$network_followup.1.player.student51)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student52[tolower(Network_Data$network_followup.1.player.student52)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student53[tolower(Network_Data$network_followup.1.player.student53)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student54[tolower(Network_Data$network_followup.1.player.student54)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student55[tolower(Network_Data$network_followup.1.player.student55)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student56[tolower(Network_Data$network_followup.1.player.student56)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student57[tolower(Network_Data$network_followup.1.player.student57)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student58[tolower(Network_Data$network_followup.1.player.student58)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student59[tolower(Network_Data$network_followup.1.player.student59)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student60[tolower(Network_Data$network_followup.1.player.student60)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student61[tolower(Network_Data$network_followup.1.player.student61)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.student62[tolower(Network_Data$network_followup.1.player.student62)==key$key[i]] =  key$label[i]
  }
  
  NetworkGroup <- gather(Network_Data, Key, to,
                        colnames(Network_Data[,grepl("network_followup.1.player.studymate",colnames(Network_Data))]))
  
  NetworkStud <- gather(Network_Data, Key, to,
                        colnames(Network_Data[,grepl("network_followup.1.player.student",colnames(Network_Data))]))
  NetworkStud <- NetworkStud[, c("survey_followup.1.player.pid", "Key", "to")]
  
  NetworkFun <- gather(Network_Data, Key, weight,
                        colnames(Network_Data[,grepl("network_followup.1.player.fun",colnames(Network_Data))]))
  NetworkFun <- NetworkFun[, c("survey_followup.1.player.pid", "Key", "weight")]
  NetworkStudy <- gather(Network_Data, Key, weight,
                         colnames(Network_Data[,grepl("network_followup.1.player.studious",colnames(Network_Data))]))
  NetworkStudy <- NetworkStudy[, c("survey_followup.1.player.pid", "Key", "weight")]
  
  NetworkStudy <- data.frame(
    from = NetworkStud$survey_followup.1.player.pid,
    to = NetworkStud$to,
    weight = NetworkStudy$weight
  )
  
  if (label == "Studious"){
  
  d <- data.frame(
    from = NetworkStud$survey_followup.1.player.pid,
    to = NetworkStud$to,
    weight = NetworkFun$weight
  )
  
  } else if (label == "Fun"){
    d <- data.frame(
      from = NetworkStud$survey_followup.1.player.pid,
      to = NetworkStud$to,
      weight = NetworkStudy$weight
    )
  } else if (label == "StudyGroup"){
    d <- data.frame(
      from = NetworkStudy$survey_followup.1.player.pid,
      to = NetworkStudy$to,
      weight = 1)
  }
  edges <- subset(d,complete.cases(d))
  nodes <- data.frame(ID = stri_trans_general(str = unique(Network_Data$survey_followup.1.player.pid), id = "Latin-ASCII"))
  
  edges <- subset(edges, !(grepl("fake",edges$from)|grepl("fake",edges$to)))
  nodes <- subset(nodes, !(grepl("fake",nodes$ID)))
  
  return(edges)
}

CreateEdges_label <- function(Network_Data, key, uniqueKey, label="Studious"){
  key$label <- tolower(key$label)
  
  for (i in seq(nrow(key))){
    Network_Data$survey_followup.1.player.pid[tolower(Network_Data$survey_followup.1.player.pid)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.studymate1[tolower(Network_Data$network_followup.1.player.studymate1)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.studymate2[tolower(Network_Data$network_followup.1.player.studymate2)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.studymate3[tolower(Network_Data$network_followup.1.player.studymate3)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.studymate4[tolower(Network_Data$network_followup.1.player.studymate4)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.studymate5[tolower(Network_Data$network_followup.1.player.studymate5)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend1[tolower(Network_Data$network_followup.1.player.friend1)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend2[tolower(Network_Data$network_followup.1.player.friend2)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend3[tolower(Network_Data$network_followup.1.player.friend3)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend4[tolower(Network_Data$network_followup.1.player.friend4)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend5[tolower(Network_Data$network_followup.1.player.friend5)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend6[tolower(Network_Data$network_followup.1.player.friend6)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend7[tolower(Network_Data$network_followup.1.player.friend7)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend8[tolower(Network_Data$network_followup.1.player.friend8)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend9[tolower(Network_Data$network_followup.1.player.friend9)==key$key[i]] =  key$label[i]
    Network_Data$network_followup.1.player.friend10[tolower(Network_Data$network_followup.1.player.friend10)==key$key[i]] =  key$label[i]
    }
  
  NetworkGroup <- gather(Network_Data, Key, to,
                         colnames(Network_Data[,grepl("network_followup.1.player.studymate",colnames(Network_Data))]))
  
  NetworkStud <- gather(Network_Data, Key, to,
                        colnames(Network_Data[,grepl("network_followup.1.player.friend",colnames(Network_Data))]))
  NetworkStud <- NetworkStud[, c("survey_followup.1.player.pid", "Key", "to")]
  
  NetworkFun <- gather(Network_Data, Key, weight,
                       colnames(Network_Data[,grepl("network_followup.1.player.fun",colnames(Network_Data))]))
  NetworkFun <- NetworkFun[, c("survey_followup.1.player.pid", "Key", "weight")]
  NetworkStudy <- gather(Network_Data, Key, weight,
                         colnames(Network_Data[,grepl("network_followup.1.player.studious",colnames(Network_Data))]))
  NetworkStudy <- NetworkStudy[, c("survey_followup.1.player.pid", "Key", "weight")]
  
  NetworkStudy <- data.frame(
    from = NetworkStud$survey_followup.1.player.pid,
    to = NetworkStud$to,
    weight = NetworkStudy$weight
  )
  
  if (label == "Studious"){
    
    d <- data.frame(
      from = NetworkStud$survey_followup.1.player.pid,
      to = NetworkStud$to,
      weight = NetworkFun$weight
    )
    
  } else if (label == "Fun"){
    d <- data.frame(
      from = NetworkStud$survey_followup.1.player.pid,
      to = NetworkStud$to,
      weight = NetworkStudy$weight
    )
  } else if (label == "StudyGroup"){
    d <- data.frame(
      from = NetworkStudy$from,
      to = NetworkStudy$to,
      weight = 1)
  }
  edges <- subset(d,complete.cases(d))
  nodes <- data.frame(ID = stri_trans_general(str = unique(Network_Data$survey_followup.1.player.pid), id = "Latin-ASCII"))
  
  edges <- subset(edges, !(grepl("fake",edges$from)|grepl("fake",edges$to)))
  nodes <- subset(nodes, !(grepl("fake",nodes$ID)))
  
  return(edges)
}

