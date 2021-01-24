
library(dplyr)
library(rgl)

###########データの取得#############
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

##############ロドリゲス回転公式###################



n1=c(1,2,3) #回転軸の定義


Rot=function(x) 
{
  x=pi*x/180
  
  I=diag(1,3)
  
  n2=rbind(c(0,-n1[3],n1[2]),c(0,0,-n1[1]),c(0,0,0))
  
  n22=-t(n2)
  
  A=I+sin(x)*(n2+n22)+(n2+n22)%*%(n2+n22)*(1-cos(x))
  
  return(A)
}

#####フェニル基2の回転#####


f_ph2=f

n1=f_ph2[13,4:6]-f_ph2[1,4:6]

r_ni=sqrt(sum(n1*n1))

n1=n1/r_ni


z=c(13:23)


for (k in z) {
  
  
  f_ph2[k,4:6]=Rot(90)%*%f_ph2[k,4:6]
  
}


f2c=f_ph2

x=rep(1,26)



plot3d(f2c[,4],f2c[,5],f2c[,6],col=x,type = "s") #炭素骨格のみ90度回転した





#####慣性テンソルを計算してみる#####

m=c(1.008,4.003,6.941,9.012,10.81,12.01) #原子量vs原子番号

mm=m[f[,2]]

G=c(sum(f[,4]*mm),sum(f[,5]*mm),sum(f[,6]*mm))


G ##まあ当然0ですよねえ...


f[,4]=f[,4]-G[1]

f[,5]=f[,5]-G[2]

f[,6]=f[,6]-G[3]　　# 重心系に変換


Ind=function(i,j)
  
  
{(1/(6.02*10^26))*sum(f[,i]*f[,j]*mm)}


Id=function(i)
  
{  a=data.frame(x=c(4,5,6))

g=function(y){
  
  a %>% filter(a[]!=y)
}

k=g(i)[1,]
l=g(i)[2,]

y=(1/(6.02*10^26))*(sum(f[,k]^2*mm)+sum(f[,l]^2*mm))

return(y)

}

I0=diag(0,3)

diag(I0)=c(Id(4),Id(5),Id(6)) 

#対角成分
I1=rbind(c(0,-Ind(4,5),-Ind(4,6)),c(0,0,-Ind(5,6)),c(0,0,0))  


I=I0+I1+t(I1)



##==========回転定数を計算してみる####==========================

A=eigen(I)    #慣性テンソルの対角化

AA=A$values    #固有値
AA


AAA=AA*(10^-20)
B=sort((2.799275e-46)/AAA) #　単位換算

BB=data.frame(A=c(B[3]),B=c(B[2]),C=c(B[1]))















