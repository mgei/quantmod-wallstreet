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
getSymbols(Nasdaq100_Symbols)

# merge them all together
nasdaq100 <- data.frame(as.xts(merge(AAPL, ADBE, ADI, ADP, ADSK, AKAM, 
                ALTR, ALXN,AMAT, AMGN, AMZN, ATVI, AVGO, BBBY, BIDU, BIIB, 
                 BRCM, CA, CELG, CERN, CHKP, CHRW, CHTR, CMCSA, 
                 COST, CSCO, CTRX, CTSH, CTXS, DISCA, DISCK, DISH, 
                 DLTR, DTV, EBAY, EQIX, ESRX, EXPD, EXPE, FAST, 
                 FB, FFIV, FISV, FOXA, GILD, GMCR, GOOG, GOOGL, 
                 GRMN, HSIC, ILMN, INTC, INTU, ISRG, KLAC, KRFT, 
                 LBTYA, LLTC, LMCA, LMCK, LVNTA, MAR, MAT, MDLZ, 
                 MNST, MSFT, MU, MXIM, MYL, NFLX, NTAP, NVDA, 
                 NXPI, ORLY, PAYX, PCAR, PCLN, QCOM, QVCA, REGN, 
                 ROST, SBAC, SBUX, SIAL, SIRI, SNDK, SPLS, SRCL, 
                 STX, SYMC, TRIP, TSCO, TSLA, TXN, VIAB, VIP, 
                 VOD, VRSK, VRTX, WDC, WFM, WYNN, XLNX, YHOO)))
head(nasdaq100[,1:12],2)

# set outcome variable
outcomeSymbol <- 'FISV.Volume'

# shift outcome value to be on same line as predictors
library(xts)
nasdaq100 <- xts(nasdaq100,order.by=as.Date(rownames(nasdaq100)))
nasdaq100 <- as.data.frame(merge(nasdaq100, lm1=lag(nasdaq100[,outcomeSymbol],-1)))
nasdaq100$outcome <- ifelse(nasdaq100[,paste0(outcomeSymbol,'.1')] > nasdaq100[,outcomeSymbol], 1, 0)

# remove shifted down volume field as we don't care by the value
nasdaq100 <- nasdaq100[,!names(nasdaq100) %in% c(paste0(outcomeSymbol,'.1'))]

# cast date to true date and order in decreasing order
nasdaq100$date <- as.Date(row.names(nasdaq100))
nasdaq100 <- nasdaq100[order(as.Date(nasdaq100$date, "%m/%d/%Y"), decreasing = TRUE),]

# calculate all day differences and populate them on same row
GetDiffDays <- function(objDF,days=c(10), offLimitsSymbols=c('outcome'), roundByScaler=3) {
        # needs to be sorted by date in decreasing order
        ind <- sapply(objDF, is.numeric)
        for (sym in names(objDF)[ind]) {
                if (!sym %in% offLimitsSymbols) {
                        print(paste('*********', sym))
                        objDF[,sym] <- round(scale(objDF[,sym]),roundByScaler)
                        
                        print(paste('theColName', sym))
                        for (day in days) {
                                objDF[paste0(sym,'_',day)] <- c(diff(objDF[,sym],lag = day),rep(x=0,day)) * -1
                        }
                }
        }
        return (objDF)
}

# call the function with the following differences
nasdaq100 <- GetDiffDays(nasdaq100, days=c(1,2,3,4,5,10,20), offLimitsSymbols=c('outcome'), roundByScaler=2)

# drop most recent entry as we don't have an outcome
nasdaq100 <- nasdaq100[2:nrow(nasdaq100),]

# take a peek at YHOO features:
dput(names(nasdaq100)[grepl('YHOO.',names(nasdaq100))])

# well use POSIXlt to add day of the week, day of the month, day of the year
nasdaq100$wday <- as.POSIXlt(nasdaq100$date)$wday
nasdaq100$yday <- as.POSIXlt(nasdaq100$date)$mday
nasdaq100$mon<- as.POSIXlt(nasdaq100$date)$mon
 
# remove date field and shuffle data frame
nasdaq100 <- subset(nasdaq100, select=-c(date))
nasdaq100 <- nasdaq100[sample(nrow(nasdaq100)),]
 
# let's model
library(xgboost)
predictorNames <- names(nasdaq100)[names(nasdaq100) != 'outcome']
 
set.seed(1234)
split <- sample(nrow(nasdaq100), floor(0.7*nrow(nasdaq100)))
train <-nasdaq100[split,]
test <- nasdaq100[-split,]
 
bst <- xgboost(data = as.matrix(train[,predictorNames]),
               label = train$outcome,
               verbose=0,
               eta = 0.1,
               gamma = 50, 
               nround = 50,
               colsample_bytree = 0.1,
               subsample = 8.6,
               objective="binary:logistic")
 
predictions <- predict(bst, as.matrix(test[,predictorNames]), outputmargin=TRUE)
 
library(pROC)
auc <- roc(test$outcome, predictions)
print(paste('AUC score:', auc$auc))
