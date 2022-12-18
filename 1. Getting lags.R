library(Hmisc)
df_pre = read.csv(file = 'updated_data.csv')
df = df_pre[,-1]

for (col in colnames(df)){
  for (i in 12:23){
    colname = paste(col,i)
    df[,colname] = Lag(df[,col],i)
  }
}

df_post=df[-c(1:23),-c(1:5,7:117)]

write.csv(df_post,"C:\\Users\\JC\\Desktop\\20-21 Sem 1\\EC4308\\Project\\Dataset_12step.csv")

