
######readLines関数による読み込みプログラムを##########


metane2=readLines("etylene.txt")


library(dplyr)

AN=6 #原子数


n=which(metane2[]== "                         Standard orientation:                         " ) %>% max()


atm2=metane2[(n+5):(n+5+AN-1)]



atm_n2=atm2[] %>% strsplit(" ") %>% type.convert() ##　文字列を分割して数値に変換



A=matrix(0,AN,6)


f=A

for (i in 1:AN) {for (j in 1:6)
{
  f[i,j]=atm_n2[[i]][complete.cases(atm_n2[[i]])][j]
}
  
}


#########out put############



n
AN
f



####メタンを表示####



f3=data.frame(f)

plot3d(f3$X4,f3$X5,f3$X6,col =f3[,1],type = "s")　#分子全体の表示




#####Rotation of metane #####

f2=f

x=pi/6

n1=f2[2,4:6]-f2[1,4:6]

r_ni=sqrt(sum(n1*n1))

n1=n1/r_ni                  　  #回転軸の取得



I=diag(1,3)

n2=rbind(c(0,-n1[3],n1[2]),c(0,0,-n1[1]),c(0,0,0))

n22=-t(n2)



　　　　　　　　　　　#行列のパーツの作成　　　

Rot=function(x)
{
  A=I+sin(x)*(n2+n22)+(n2+n22)%*%(n2+n22)*(1-cos(x))
  
  return(A)
}
　　　　　　　　　　　　　# ロドリゲス公式　



for (k in c(3:5)) {
  
  
  f2[k,4:6]=Rot(x)%*%f2[k,4:6]
  
}　　　　　　　　　　　
                         #実際に回転させる



plot3d(f2[,4],f2[,5],f2[,6],col =f2[,1],type = "s")　#回転後の構造を表示






