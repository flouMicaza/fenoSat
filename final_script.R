#calculo de las regresiones: 


print("promedios:")
mean_by_season("/home/florencia/Documentos/UC/NDVI_ZAA",17,TRUE)
print("calculo de las regresiones:")
tval<-tvalue_regression("/home/florencia/Documentos/UC/NDVI_ZAA/stats",17)
pval <-pvalue_regression("/home/florencia/Documentos/UC/NDVI_ZAA/stats",17)

