
library(here)

setwd(here('data/formatted/formatted_csvs/fishery_open_close/'))

tseries <- read.csv('annual_average_open_species.csv') |>
  subset(year>1989)

plot(tseries$year, tseries$commercial, typ='o')
plot(tseries$year, tseries$recreational, typ='o')


testcom90 <- read.csv('commercial_matrix_1990.csv')
testrec90 <- read.csv('recreational_matrix_1990.csv')

colSums(testcom90[,-1]) |> mean()
colSums(testrec90[,-1]) |> mean()
tseries

comps <- data.frame(year = 1990:2025)
for(i in 1990:2025){
  tmp <- read.csv(paste0('recreational_matrix_',i,'.csv'))
  comps$cols[i-1989] <- mean(colSums(tmp[,-1]))
  comps$rows[i-1989] <- mean(rowSums(tmp[,-1]))
}

plot(tseries$year, tseries$recreational, typ='o')
plot(comps$year,comps$cols,typ='o')
plot(comps$year,comps$rows,typ='o')
abline(h=365)
