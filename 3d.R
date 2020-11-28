f
##########分子の回転########
f2=f

n1=f2[2,4:6]-f2[1,4:6]

r_ni=sqrt(sum(n1*n1))

n1=n1/r_ni

for (k in c(10:26)) {
  
  
  f2[k,4:6]=Rot(0)%*%f2[k,4:6]
  
}


f3=data.frame(f2)


##########分子の表示########

f3=data.frame(f)



carbon=f3 %>% filter(f3[,2]==6)

Hydrogen=f3 %>% filter(f3[,2]==1)

fcol=c(8,7,6,5,4,1)



plot3d(carbon$X4,carbon$X5,carbon$X6,type = "s") #炭素骨格のみ


plot3d(f3$X4,f3$X5,f3$X6,col = fcol[f3[,2]],type = "s")　#分子全体





######エチレンでやってみる######

ety=read.csv("ety.csv")

##########エチレンを回転してみる#######

f2=f

n1=f2[4,4:6]-f2[1,4:6]

r_ni=sqrt(sum(n1*n1))

n1=n1/r_ni

for (k in c(2,3,5,6)) {
  
  
  f2[k,4:6]=Rot(4*pi/6)%*%f2[k,4:6]
  
}


f3=data.frame(f2)

plot3d(f3$X4,f3$X5,f3$X6,col = fcol[f3[,2]],type = "s")



