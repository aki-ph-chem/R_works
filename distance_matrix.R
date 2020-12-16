
m=y %>% str_detect("Distance matrix") %>% which() %>% min()

m


#Distance mtrixの直前の添え字


yy=y[(m+1):(m+1+AN)]

yy
