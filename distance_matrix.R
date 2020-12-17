
m=y %>% str_detect("Distance matrix") %>% which() %>% min()

m


#Distance mtrixの直前の添え字


yy=y[(m+2):(m+1+AN)]%>% str_split(" ") 



mm=matrix(0,AN,AN+2)


for (k in 1:AN) {

  

n=which(yy[[k]][]!="")

nn=length(n)

nnn=yy[[k]][n]

mm[k,1:nn]=nnn

}


A=mm[,3:7] %>% type.convert() %>% round()


A
