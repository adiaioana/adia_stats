date=read.csv("life_expect.csv", header=T, sep=',')
tari=date[['country']]
barbati=date[['male']]
femei=date[['female']]

mean(femei)
median(femei)
mean(barbati)
median(barbati)

hist(femei, breaks=8)
hist(barbati, breaks=8)