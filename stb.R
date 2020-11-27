

####### 別のデータで試してみる#########

library(dplyr)

AN=26 #原子数

stb3=read.csv("stb3.csv")

n=which(stb3[,]=="Standard orientation:") %>% max()


atm2=stb3[(n+5):(n+5+AN-1),]



atm_n2=atm2[] %>% strsplit(" ") %>% type.convert() ##　文字列を分割して数値に変換



A=matrix(0,26,6)


f=A

for (i in 1:26) {for (j in 1:6)
{
  f[i,j]=atm_n2[[i]][complete.cases(atm_n2[[i]])][j]
}
  
}


##----------------out put-----------------------------
  
  

n
AN
f


#####ロドリゲス回転公式を実装してみる#######




I=diag(1,3)

n1=c(1,2,3)

n2=rbind(c(0,-n1[3],n1[2]),c(0,0,-n1[1]),c(0,0,0))

n22=-t(n2)




Rot=function(x)
{
  A=I+sin(x)*(n2+n22)+(n2+n22)%*%(n2+n22)*(1-cos(x))
  
  return(A)
}

#####実際に使ってみる#####

n1=c(0,0,1)

y=c(1,1,0)

Rot(pi/2)%*%y


######ガウシアンのデータでやってみる####


f2=f

n1=f2[2,4:6]-f2[1,4:6]

r_ni=sqrt(sum(n1*n1))

n1=n1/r_ni

for (k in c(2:26)) {
  
  
  f2[k,4:6]=Rot(pi/2)%*%f2[k,4:6]
  
}

f2



