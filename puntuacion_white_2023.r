rm(list=ls())
library(openxlsx)
compe <- c("Participante_1",
           "Participante_2",
           "Participante_3",
           "Participante_4",
           "Participante_5",
           "Participante_6",
           "Participante_7",
           "Participante_8",
           "Participante_9")

acad <- c("Escuela_1",
          "Escuela_2",
          "Escuela_3",
          "Escuela_4",
          "Escuela_1",
          "Escuela_3",
          "Escuela_5",
          "Escuela_6",
          "Escuela_6")

cate <- "NIVEL_1_TOTS_G1"
fecha <- "2023-11-11"

J1A <- c(55,53,48,44,57,50,47,52,50)
J2A <- c(55,52,46,44,58,48,47,53,50)
J3A <- c(55,51,46,44,57,48,47,53,50)
J1B <- c(53,51,46,42,56,48,46,51,49)
J2B <- c(53,50,44,42,56,46,45,51,48)
J3B <- c(54,50,45,43,56,47,46,52,49)

nombre <- LETTERS[1:length(J1A)]

d <- data.frame(nombre,J1A,J2A,J3A,J1B,J2B,J3B)
d$SJ1 <- d$J1A+d$J1B
d$SJ2 <- d$J2A+d$J2B
d$SJ3 <- d$J3A+d$J3B
d$SJB <- d$J1B+d$J2B+d$J3B
d$ST <- (d$SJ1+d$SJ2+d$SJ3)/10

mm <- d$nombre
gg <- c()
ggv <- c()

for(i in 1:dim(d)[1]){
  nn <- mm[i]
  d$id <- 1
  d$id[d$nombre==nn] <- 0
  d <- d[order(d$id),]
  g1 <- rep(0,dim(d)[1]-1)
  for(j in 2:dim(d)[1]){
    g1[j-1] <- ifelse(d$SJ1[1] > d$SJ1[j],1,ifelse(d$SJ1[1]==d$SJ1[j],ifelse(d$SJB[1] > d$SJB[j],1,ifelse(d$SJB[1] == d$SJB[j],0.5,0)),0))+
               ifelse(d$SJ2[1] > d$SJ2[j],1,ifelse(d$SJ2[1]==d$SJ2[j],ifelse(d$SJB[1] > d$SJB[j],1,ifelse(d$SJB[1] == d$SJB[j],0.5,0)),0))+
               ifelse(d$SJ3[1] > d$SJ3[j],1,ifelse(d$SJ3[1]==d$SJ3[j],ifelse(d$SJB[1] > d$SJB[j],1,ifelse(d$SJB[1] == d$SJB[j],0.5,0)),0))
    gg[i] <- sum(g1 >= 2) + 0.5*sum(g1 == 1.5) 
    ggv[i] <- sum(g1)
  }
}

dg <- data.frame(nombre = mm, MV = gg, TOTAL_V = ggv)

df <- merge(d,dg,by = "nombre")
df$id <- NULL

df <- df[order(df$MV,decreasing = T),]
df$POS <- rank(df$MV,ties.method = "max")


df$empate <- duplicated(df$POS)
emp11 <- df[df$MV %in% unique(df[df$empate,"MV"]),]
dg1 <- data.frame(nombre = NA,MV1 = NA)
for(kk in unique(df[df$empate,"MV"])){
  emp1 <- emp11[emp11$MV == kk,]
  mm1 <- emp1$nombre
  ggemp1 <- c()
print(head(emp1))
for(i in 1:dim(emp1)[1]){
  nn <- mm1[i]
  emp1$id <- 1
  emp1$id[emp1$nombre==nn] <- 0
  emp1 <- emp1[order(emp1$id),]
  g1 <- rep(0,dim(emp1)[1]-1)
  for(j in 2:dim(emp1)[1]){
    g1[j-1] <- ifelse(emp1$SJ1[1] > emp1$SJ1[j],1,ifelse(emp1$SJ1[1]==emp1$SJ1[j],ifelse(emp1$SJB[1] > emp1$SJB[j],1,ifelse(emp1$SJB[1] == emp1$SJB[j],0.5,0)),0))+
      ifelse(emp1$SJ2[1] > emp1$SJ2[j],1,ifelse(emp1$SJ2[1]==emp1$SJ2[j],ifelse(emp1$SJB[1] > emp1$SJB[j],1,ifelse(emp1$SJB[1] == emp1$SJB[j],0.5,0)),0))+
      ifelse(emp1$SJ3[1] > emp1$SJ3[j],1,ifelse(emp1$SJ3[1]==emp1$SJ3[j],ifelse(emp1$SJB[1] > emp1$SJB[j],1,ifelse(emp1$SJB[1] == emp1$SJB[j],0.5,0)),0))
    ggemp1[i] <- sum(g1)
    print(g1)
    print(ggemp1)
  }
}
dg1 <- rbind(dg1,data.frame(nombre = mm1,MV1 = ggemp1))
}


dg1 <- dg1[!is.na(dg1$nombre),]

df1 <- merge(emp11,dg1,by = "nombre")
df1$empate <- NULL

df1$POS1 <- rank(df1$MV1,ties.method = "max")
df1 <- df1[order(df1$POS1,decreasing = T),]
df1$empate <- duplicated(df1$POS1)
sd1 <- df1[!(df1$POS1 %in% unique(df1[df1$empate,"POS1"])),c("nombre","MV1","POS1")]


df <- merge(df,sd1,by = "nombre",all.x = T)


df2 <- df1[(df1$POS1 %in% unique(df1[df1$empate,"POS1"])),]
df2$empate <- NULL
df2$MV2 <- df2$SJB
df2$POS2 <- rank(df2$SJB,ties.method = "max")

df2$empate <- duplicated(df2$POS2)
sd2 <- df2[!(df2$POS2 %in% unique(df2[df2$empate,"POS2"])),c("nombre","MV2","POS2")]

df <- merge(df,sd2,by = "nombre",all.x = T)


df2 <- df2[(df2$POS2 %in% unique(df2[df2$empate,"POS2"])),]
df2$empate <- duplicated(df2$MV2)


df2$MV3 <- df2$TOTAL_V
df2$POS3 <- rank(df2$TOTAL_V,ties.method = "max")

df2$empate <- duplicated(df2$POS3)

sd3 <- df2[!(df2$POS3 %in% unique(df2[df2$empate,"POS3"])),c("nombre","MV3","POS3")]

df <- merge(df,sd3,by = "nombre",all.x = T)

df2 <- df2[(df2$POS3 %in% unique(df2[df2$empate,"POS3"])),]
df2$empate <- duplicated(df2$MV3)


df2$MV4 <- df2$ST
df2$POS4 <- rank(df2$MV4,ties.method = "max")

 
df <- merge(df,df2[c("nombre","MV4","POS4")],by = "nombre",all.x = T)

df <- df[order(df$POS,df$POS1,df$POS2,df$POS3,df$POS4,decreasing = T),]

final <- merge(
  data.frame(nombre = LETTERS[1:length(compe)],Competidoras = compe,Academias = acad),
  df, by = "nombre")[,-c(1)]

final <- final[order(final$POS,final$POS1,final$POS2,final$POS3,final$POS4,decreasing = T),]
final$POS_F <- 1:dim(final)[1]
if(length(compe) != dim(final)[1]){"Hay algún participantes ausente"}
final[,c(1,2,13,14,26)]

write.xlsx(
  final,paste0(cate,"_",fecha,".xlsx")
)
