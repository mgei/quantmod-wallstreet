library(quantmod)

# mkt data SPI
SPI_Symbols <- c("ABBN.VX", "ADXN.SW", "ADENE.SW", "ADVN.SW", "AEVS.SW", "AIRE.SW", "AIRN.SW", 
  "ALLN.SW", "ALSN.SW", "AMS.SW", "APGN.SW", "ARBN.SW", "ARON.SW", "ARYN.VX", "ASCN.SW", "AUTN.SW", "BANB.SW", "BALNE.SW", 
  "BPDG.SW", "BARN.SW", "BLKB.SW", "BSLN.SW", "BSKP.SW", "BCGE.SW", "BCJ.SW", "BCVN.SW", "BEKN.SW", "BEAN.SW", "BELL.SW", 
  "BBN.SW", "BLIN.SW", "BC.SW", "LINN.SW", "BKW.SW", "BOBNN.SW", "BOSN.SW", "BUCN.SW", "BCHN.SW", "BRKN.SW", "BVZN.SW", 
  "CALN.SW", "SKIN.SW", "CMBN.SW", "CPGN.SW", "CIE.SW", "CICN.SW", "CFT.SW", "CLN.VX", "CLTN.SW", "COTN.SW","CON.SW", 
  "COPN.SW", "CPHN.SW", "CLXN.SW", "CSGN.VX", "DAE.SW", "DKSH.SW", "DOKA.SW", "DUFN.VX", "ESUN.SW", "EFGN.SW", "ELMN.SW", 
  "EMMN.SW", "EMSN.SW", "EVE.SW", "FTON.SW", "FI.SW", "FHZN.SW", "FORN.SW", "GALE.SW", "GAM.SW", "GAV.SW", "GEBN.VX", 
  "GIVN.VX", "GLKBN.SW", "GMI.SW", "GBMN.SW", "GRKP.SW", "GUR.SW", "HELN.SW", "HIAG.SW", "HLEE.SW", "HOCN.SW", "HUBN.SW", 
  "HUE.SW", "HBLN.SW", "IDIA.SW", "IMPN.SW", "IFCN.SW", "INRN.SW", "ISN.SW", "IREN.SW", "VBSN.SW", "BAER.VX", "JFN.SW", 
  "KARN.SW", "KOMN.SW", "KTMI.SW", "KUD.SW", "KNIN.VX", "KURN.SW", "LHN.VX", "LAND.SW", "LMN.SW", "LECN.SW", "LEHN.SW", 
  "LEON.SW", "LLBN.SW", "LISN.SW", "LISP.SW", "LOGN.VX", "LONN.VX", "LUMX.SW", "LUKN.SW", "MCHN.SW", "METN.SW", "MBTN.SW", 
  "MIKN.SW", "MOZN.SW", "MOBN.SW", "MOLN.SW", "MYRN.SW", "NESN.VX", "NWRN.SW", "NOVN.VX", "OFN.SW", "OERL.SW", "ODHN.SW", 
  "ORON.SW", "PWTN.SW", "PARG.SW", "PGHN.VX", "PEAN.SW", "PRFN.SW", "PEDU.SW", "PM.SW", "PLAN.SW", "PNHO.SW", "PEHN.SW", 
  "PSPN.SW", "RLF.SW", "CFR.VX", "RIEN.SW", "ROG.VX", "HREN.SW", "SANN.SW", "SAHN.SW", "SCHN.SW", "SCHP.VX", "STRN.SW", 
  "STLN.SW", "SWTQ.SW", "SFSN.SW", "SGSN.VX", "SFZN.SW", "SIK.VX", "SNBN.SW", "SOON.SW", "SPCE.SW", "SGKN.SW", "STGN.SW", 
  "STMN.SW", "SUN.SW", "SRCG.SW", "UHR.VX", "UHRN.SW", "SFPN.SW", "SLHN.SW", "SPSN.SW", "SREN.VX", "SCMN.VX", "SQN.SW", 
  "TAMN.SW", "TECN.SW", "TEMN.SW", "TKBP.SW", "TIBN.SW", "TOHN.SW", "UBXN.SW", "UBSG.VX", "VLRT.SW", "VATN.SW", "VALN.SW", 
  "VARN.SW", "VACN.SW", "VAHN.SW", "VET.SW", "VIFN.VX", "VILN.SW", "ROL.SW", "VONN.SW", "VPBN.SW", "VZN.SW", "WKBN.SW", 
  "WMN.SW", "WARN.SW", "WIHN.SW", "YPSN.SW", "ZEHN.SW", "ZUBN.SW", "ZUGN.SW", "ZG.SW", "ROSE.SW", "ZURN.VX", "ZWM.SW")
  
SPI_test <- c("ABBN.VX", "ADXN.SW", "ADENE.SW")

tickers <- SPI_test
from = "2000-01-01"
ntic <- length(tickers)
for (i in 1:ntic) {
  temp <- getSymbols(tickers[i], auto.assign = FALSE, from = from)
  if (i == 1) {
    data <- temp
  } else {
    data <- merge(data,temp)
  }
  rm(temp)
  Sys.sleep(1)
}
data_df <- data.frame(data)

# set outcome variable
outcomeSymbol <- 'ABBN.VX.Volume'

# shift outcome value to be on same line as predictors
data_e <- merge(data, lm1 = lag(data[,outcomeSymbol],-1))
data_e$outcome <- ifelse(data_e[,paste0(outcomeSymbol,".1")] > data_e[,paste0(outcomeSymbol)], 1, 0)

data_e <- data_e[,-(ntic*6+1)]

returns <- ROC(data_e)[,-ncol(data_e)]
data_e <- merge(data_e, returns)

# data_e$date <- as.Date(index(data_e))

data_e <- na.locf(data_e)

data_e <- data_e["2015/"]

data_r <- data.frame(data_e)
data_r <- data_r[sample(nrow(data_r)),]
 
# let's model
library(xgboost)
predictorNames <- names(data_r)[names(data_r) != 'outcome']
 
set.seed(1234)
split <- sample(nrow(data_r), floor(0.7*nrow(data_r)))
train <-data_r[split,]
test <- data_r[-split,]
 
bst <- xgboost(data = as.matrix(train[,predictorNames]),
               label = train$outcome,
               verbose=0,
               eta = 0.1,
               gamma = 50, 
               nround = 50,
               colsample_bytree = 0.1,
               subsample = 1, #8.6
               objective="binary:logistic")
 
predictions <- predict(bst, as.matrix(test[,predictorNames]), outputmargin=TRUE)
 
library(pROC)
auc <- roc(test$outcome, predictions)
print(paste('AUC score:', auc$auc))