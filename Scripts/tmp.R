#######
#
# Pruebas para ir incorporando en el script de Exp2_Boston
#
###########


names(df)


summary(df)




tmp<-df[,c(paste0("E2TC_",2:8), "E2TC_10", "E2TC_11", paste0("E2T1a_", 2:5), paste0("E2T1b_", 2:5), paste0("E2T1c_", 2:4),
           "E2T1b_6", paste0("E2T1d_", 2:4), "E2T1d_6",  paste0("E2T2a_",2:5), paste0("E2T2b_", 2:5), paste0("E2T2b_", 8:10),  paste0("E2T2c_",2:5))]


tmp <- mutate_all(tmp, function(x) as.numeric(as.character(x)))

tmp2<-ifelse(tmp==1, 1, 0)

tmp[is.na(tmp)] <- 0
tmp$SumFalse<- rowSums(tmp)

df$SumFalse<-tmp$SumFalse

rm(tmp)
