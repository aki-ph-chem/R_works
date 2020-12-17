#####program to get atomic coordinates #######



library(dplyr)

library(stringr)


##### plase insert the data ########

AN=26 #原子数


y=readLines("stb.txt")  #データの入力


######process########


n=str_detect(y,"Standard orientation") %>% which() %>% max()


atm2=y[(n+5):(n+5+AN-1)]


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


