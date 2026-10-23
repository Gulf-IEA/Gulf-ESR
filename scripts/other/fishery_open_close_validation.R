
library(here)

setwd(here('data/intermediate/fishery_open_close/'))

tseries <- read.csv('annual_average_open_species.csv') |>
  subset(year>1989)

plot(tseries$year, tseries$commercial, typ='o')
plot(tseries$year, tseries$recreational, typ='o')


testcom90 <- read.csv('commercial_matrix_1990.csv')
testrec90 <- read.csv('recreational_matrix_1990.csv')

cbind(colSums(testcom90[,-1]) |> mean(),
      colSums(testrec90[,-1]) |> mean())
tseries[1,]

rec_comps <- data.frame(year = 1990:2025)
for(i in 1990:2025){
  tmp <- read.csv(paste0('recreational_matrix_',i,'.csv'))
  rec_comps$cols[i-1989] <- mean(colSums(tmp[,-1]))
  rec_comps$rows[i-1989] <- mean(rowSums(tmp[,-1]))
}

com_comps <- data.frame(year = 1990:2025)
for(i in 1990:2025){
  tmp <- read.csv(paste0('commercial_matrix_',i,'.csv'))
  com_comps$cols[i-1989] <- mean(colSums(tmp[,-1]))
  com_comps$rows[i-1989] <- mean(rowSums(tmp[,-1]))
}

par(mfrow = c(2,2))
# plot(tseries$year, tseries$recreational, typ='o')
plot(rec_comps$year,rec_comps$cols,typ='o')
plot(rec_comps$year,rec_comps$rows,typ='o')
# abline(h=365)

par(mfrow = c(2,2))
# plot(tseries$year, tseries$commercial, typ='o')
plot(rec_comps$year,rec_comps$cols,typ='o')
plot(rec_comps$year,rec_comps$rows,typ='o')
plot(com_comps$year,com_comps$cols,typ='o')
plot(com_comps$year,com_comps$rows,typ='o')

