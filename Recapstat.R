dat = read.csv("Example data.csv", header = TRUE, fileEncoding = 'CP950')
head(dat)

mean(dat$eGFR, na.rm = TRUE)

sd(dat$eGFR, na.rm = TRUE)

var(dat$eGFR, na.rm = TRUE)

median(dat$eGFR, na.rm = TRUE)


quantile(dat$eGFR, na.rm = TRUE)

min(dat$eGFR, na.rm = TRUE)

max(dat$eGFR, na.rm = TRUE)

table(dat$Cancer)

table(dat$Cancer, dat$Diabetes)

tab1 = table(dat$Cancer)
prop.table(tab1)

tab2 = table(dat$Cancer, dat$Diabetes)
prop.table(tab2)

prop.table(tab2, 1)

prop.table(tab2, 2)
