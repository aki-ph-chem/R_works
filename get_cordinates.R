
#####program to get atomic coordinates #######


metane2=readLines("metane2.txt")


library(dplyr)

library(stringr)

AN=5 #原子数


n=str_detect(metane2,"Standard orientation") %>% which() %>% max()


atm2=metane2[(n+5):(n+5+AN-1)]


atm2 

atm2 %>% str_extract_all(pattern = "\\d+")




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


