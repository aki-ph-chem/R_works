
#######メタン分子の取り込み####

metane=read.csv("metane.csv")



library(dplyr)

AN=5 #原子数


n=which(metane[,]=="Standard orientation:") %>% max()


atm2=metane[(n+5):(n+5+AN-1),]



atm_n2=atm2[] %>% strsplit(" ") %>% type.convert() ##　文字列を分割して数値に変換



A=matrix(0,AN,6)


f=A

for (i in 1:AN) {for (j in 1:6)
{
  f[i,j]=atm_n2[[i]][complete.cases(atm_n2[[i]])][j]
}
  
}


##----------------out put-----------------------------



n
AN
f

########メタン表示##########

fcol=c(8,7,6,5,4,1)


f3=data.frame(f)


plot3d(f3$X4,f3$X5,f3$X6,col = fcol[f3[,2]],type = "s")　#分子全体





