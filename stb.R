setwd("c:/Users/simiz/Desktop/e-data")

stb=read.csv("stb.csv")
stb
View(stb)

which(stb[,]== " Center     Atomic                   Forces (Hartrees/Bohr)")


stb[397,]
which(stb[,]=="                         Standard orientation:                         ") %>% max()

which(stb[,]=="Standard orientation")

stb[700,]


#### Excelでの前処理ではスペース区切りにすること########

stb2=read.csv("stb2.csv")
stb2
stb2[259,]
stb2[257,]
stb2[258,]


which(stb2[,]=="Standard orientation:") %>% max()　## 最適化構造の直前のStandard orientation:の探索

atm=stb2[(5383+5):(5383+5+26-1),]  ##最適化構造の取得
atm

r_const=stb2[(5383+5+26-1+2),] ## 最適化回転定数の取得

r_const 


####### 別のデータで試してみる#########

AN=26 #原子数

stb3=read.csv("stb3.csv")

n=which(stb3[,]=="Standard orientation:") %>% max()
n

atm2=stb3[(n+5):(n+5+AN-1),]

atm2

r_const2=stb3[(n+5+AN-1+2),]
r_const2

atm_n2=atm2[] %>% strsplit(" ") %>% type.convert() ##　文字列を分割して数値に変換

atm_n2

A=matrix(0,26,6)
A

f=A
f
for (i in 1:26) {for (j in 1:6)
{
  f[i,j]=atm_n2[[i]][complete.cases(atm_n2[[i]])][j]
}
  
}
f





