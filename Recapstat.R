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

func.1 = function (x, y, dig = 3) {
  
  lvl.x = levels(factor(x))
  result = rep(NA, length(lvl.x))
  
  for (i in 1:length(lvl.x)) {
    m = mean(y[x==lvl.x[i]], na.rm = TRUE)
    s = sd(y[x==lvl.x[i]], na.rm = TRUE)
    result[i] = paste0(formatC(m, digits = dig, format = "f"), "±", formatC(s, digits = dig, format = "f"))
  }
  
  result
  
}

func.1(x = dat[,"Disease"], y = dat[,"eGFR"])

func.1(x = dat[,"Education"], y = dat[,"SBP"], dig = 1)

func.2 = function (x, y, dig = 3) {
  
  lvl.x = levels(factor(x))
  result = rep(NA, length(lvl.x))
  
  for (i in 1:length(lvl.x)) {
    m = median(y[x==lvl.x[i]], na.rm = TRUE)
    q.25 = quantile(y[x==lvl.x[i]], 0.25, na.rm = TRUE)
    q.75 = quantile(y[x==lvl.x[i]], 0.75, na.rm = TRUE)
    result[i] = paste0(formatC(m, digits = dig, format = "f"), "(", formatC(q.25, digits = dig, format = "f"), "-", formatC(q.75, digits = dig, format = "f"), ")")
  }
  
  result
  
}

func.2(x = dat[,"Disease"], y = dat[,"eGFR"])

func.2(x = dat[,"Education"], y = dat[,"SBP"], dig = 1)

pred_func = function (SBP = NA, DBP = NA) {
  
  if (is.na(SBP) & is.na(DBP)) {stop('需要至少輸入一個數值')} else {
    
    var_name = c('SBP', 'DBP')
    var_vec = c(1, SBP, DBP)
    
    var_name = var_name[!is.na(var_vec)[-1]]
    var_vec = var_vec[!is.na(var_vec)]
    
    model = lm(dat[,"eGFR"] ~ ., data = dat[,var_name,drop=FALSE])
    
    result = model$coefficients %*% var_vec
    result
    
  }
  
}

pred_func(SBP = 100)

pred_func(DBP = 100)

pred_func(SBP = 100, DBP = 100)
