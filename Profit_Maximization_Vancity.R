####  ASSIGNMENT 1  ######################## 
# BUS 445 D100
# Pair 2
############################################

# R packages + Source BCA
  library("dplyr")
  library("car")
  library("forcats")
  library("gplots")
  library("effects")
  library("gmodels")
  library("corrplot")
  source("BCA_functions_source_file.R")

# Load Data 
  vc<-read.csv(file="VC_data.csv",stringsAsFactors = TRUE)
  nrow(vc)
  
# View Data
  View(vc)
  glimpse(vc)
  variable.summary(vc)

############################################

# Q3 - CORRELATIONS 
  
  corVC<-cor(select_if(vc, is.numeric))
  
  View(corVC)
  
  corrplot(corVC, method="number", type="lower", 
           diag=FALSE, number.cex=0.7)   
  
# Variables with multicollinearity issues are listed below along with their correlation values
  cor(vc$BALMRGG,vc$DUMNOMRGG)
  cor(vc$NINDINC1,vc$numrr1)
  cor(vc$gendm,vc$gendf)
  
  
############################################
 
# Q4 - LOGIT REGRESSION 
     
# Create Sample
  vc$Sample <- create.samples(vc, 
                              est = 0.60, 
                              val = 0.40, 
                              rand.seed = 100)  
  
# AIC: 3580.5 #allvariables
  lm1 <- glm(formula = APURCH ~ age + gendm + atmcrd + paydep + DUMNOCHQ +  
              BALCHQ + DUMNOSAV + BALSAV + TOTDEP + DUMNOLOAN + BALLOAN +  
              DUMNOLOC + BALLOC + BALMRGG + NEWLOC + NEWMRGG + TXBRAN + TXATM +  
              TXPOS + TXCHQ + TXWEB + TXTEL + TOTSERV + CHNMSERV + CHNMPRD +  
              valsegm + numrr1 + avginc1 + avginv1,
                   data = filter(vc, Sample == "Estimation"),
                   family = binomial(logit)) 
  summary(lm1)

  
# AIC: 3570.9 -- lm1 but remove variables w/ z>0.5
  lm2 <- glm(formula = APURCH ~ age + gendm + atmcrd + paydep + DUMNOCHQ +  
              BALCHQ + DUMNOSAV + BALSAV + TOTDEP + DUMNOLOAN + DUMNOLOC + 
              BALLOC + BALMRGG + NEWLOC + TXBRAN + TXPOS + TXCHQ +  TXWEB + 
              TXTEL + TOTSERV + CHNMSERV + valsegm + numrr1 + avginc1,
            data = filter(vc, Sample == "Estimation"),
            family = binomial(logit)) 
  summary(lm2)
 
   
# AIC: 3563.8 -- lm2 but remove variables w/ z>0.2
  lm3 <- glm(formula = APURCH ~ age + gendm + atmcrd + paydep + DUMNOCHQ +  
              BALCHQ + BALSAV + DUMNOLOAN + BALLOC + BALMRGG + NEWLOC + 
              TXBRAN + TOTSERV + CHNMSERV + valsegm + numrr1 + avginc1,
            data = filter(vc, Sample == "Estimation"),
            family = binomial(logit)) 
  summary(lm3)
  
  
# AIC: 3563.3 -- lm3 w/o gendm
  lm4 <- glm(formula = APURCH ~ age + atmcrd + paydep + DUMNOCHQ +  
              BALCHQ + BALSAV + DUMNOLOAN + BALLOC + BALMRGG + NEWLOC + 
              TXBRAN + TOTSERV + CHNMSERV + valsegm + numrr1 + avginc1,
            data = filter(vc, Sample == "Estimation"),
            family = binomial(logit)) 
  summary(lm4)
  
  
# AIC: 3563.1 -- lm4 w/o DUMNOCHQ
  lm5 <- glm(formula = APURCH ~ age + atmcrd + paydep + BALCHQ + BALSAV + 
              DUMNOLOAN + BALLOC + BALMRGG + NEWLOC + TXBRAN + TOTSERV + 
              CHNMSERV + valsegm + numrr1 + avginc1,
            data = filter(vc, Sample == "Estimation"),
            family = binomial(logit)) 
  summary(lm5)

  
# AIC: 3563.2 -- lm5 w/o numrr1
  lm6 <- glm(formula = APURCH ~ age + atmcrd + paydep +  BALCHQ + BALSAV + 
              DUMNOLOAN + BALLOC + BALMRGG + NEWLOC + TXBRAN + TOTSERV + 
              CHNMSERV + valsegm + avginc1,
            data = filter(vc, Sample == "Estimation"),
            family = binomial(logit)) 
  summary(lm6)

  
# AIC: 3564.6 -- lm6 w/o NEWLOC
  lm7 <- glm(formula = APURCH ~ age + atmcrd + paydep + BALCHQ + BALSAV + 
              DUMNOLOAN + BALLOC + BALMRGG + TXBRAN + TOTSERV + CHNMSERV + 
              valsegm + avginc1,
            data = filter(vc, Sample == "Estimation"),
            family = binomial(logit)) 
  summary(lm7)
  


  
  
############################################

# Q4 - LIFT CHARTS
    
# All Models
  lift.chart(modelList = c("lm1", "lm2", "lm3", "lm4", "lm5", "lm6","lm7"),
             data = filter(vc, Sample == "Validation"),
             targLevel = "Y", 
             trueResp = 0.021, 
             type = "cumulative", 
             sub = "Validation")
  
# Just lm4, lm5, & lm7
  lift.chart(modelList = c("lm4", "lm5","lm7"),
             data = filter(vc, Sample == "Validation"),
             targLevel = "Y", 
             trueResp = 0.021, 
             type = "cumulative", 
             sub = "Validation")

# Just lm5 & lm7
  lift.chart(modelList = c("lm5","lm7"),
             data = filter(vc, Sample == "Validation"),
             targLevel = "Y", 
             trueResp = 0.021, 
             type = "cumulative", 
             sub = "Validation")
  
 ############################################
  
  #### Plot of Means ####
  
  vc$APURCH.Num <- if_else(vc$APURCH == "Y",1,0)

  
  # BALCHQ - good
  # BALSAV - unevendistribution
  # BALLOC - unevendistribution
  # BALMRGG - unevendistribution
  # TXBRAN - good
  # TOTSERV - unevendistribution
  # avginc1 - good

# Histogram of 3 variables below show that the data is not evenly distributed. 
  # There is large skew e.g (A large number of customers have a low value, while 
  # a small number have a high value) in the predictor of interest)
  
  hist(vc$BALSAV, col = "lightblue", breaks = 30)
  hist(vc$BALLOC, col = "lightblue", breaks = 30)
  hist(vc$BALMRGG, col = "lightblue", breaks = 30)

#plotofmeans for BALSAV
  vc$BALSAV.Cat <- binVariable(vc$BALSAV, bins =10,method = "intervals",labels = NULL)
  plotmeans(APURCH.Num ~ BALSAV.Cat, data = vc)

#Plotofmeans for BALLOC
  vc$BALLOC.Cat <- binVariable(vc$BALLOC, bins =10,method = "intervals",labels = NULL)
  plotmeans(APURCH.Num ~ BALLOC.Cat, data = vc)

#Plotofmeans for TOTSERV
  vc$TOTSERV.Cat <- binVariable(vc$TOTSERV, bins =15,method = "intervals",labels = NULL)
  plotmeans(APURCH.Num ~ TOTSERV.Cat, data = vc)

#Plotofmeans for BALMRGG
  vc$BALMRGG.Cat <- binVariable(vc$BALMRGG, bins =8,method = "intervals",labels = NULL)
  plotmeans(APURCH.Num ~ BALMRGG.Cat, data = vc)


#Plot of means for BALCHQ which is evenly distributed continous predictor variables   
  vc$BALCHQ.Cat <- binVariable(vc$BALCHQ, bins =4,method = "proportions",labels = NULL)
  plotmeans(APURCH.Num ~ BALCHQ.Cat, data = vc)
#nonlinear #concaveup #increasing

#plot of means for TXBRAN
  vc$TXBRAN.Cat <- binVariable(vc$TXBRAN, bins = 4,method = "proportions",labels = NULL)
  plotmeans(APURCH.Num ~ TXBRAN.Cat, data = vc)
#nonlinear #concavedown #increasing
 
#plot of means for avginc1
  vc$avginc1.Cat <- binVariable(vc$avginc1, bins =8,method = "proportions",labels = NULL)
  plotmeans(APURCH.Num ~ avginc1.Cat, data = vc)
#nonlinear #zigzag #ushaped



######################## Transformations #############################


vc$Log.avginc1=log(vc$avginc1)
vc$Log.TXBRAN=log(vc$TXBRAN+1)
vc$Log.BALCHQ=log(vc$BALCHQ+1)


#LogTransformation
lm5_log <- glm(formula = APURCH ~ age + atmcrd + paydep + Log.BALCHQ + BALSAV + 
             DUMNOLOAN + BALLOC + BALMRGG + NEWLOC + Log.TXBRAN + TOTSERV + CHNMSERV + 
             valsegm + numrr1 + Log.avginc1,
           data = filter(vc, Sample == "Estimation"),
           family = binomial(logit)) 
summary(lm5_log)

vc$sqrt.avginc1=sqrt(vc$avginc1)
vc$sqrt.TXBRAN=sqrt(vc$TXBRAN)
vc$sqrt.BALCHQ=sqrt(vc$BALCHQ)
vc$sqrt.BALSAV=sqrt(vc$BALSAV)
vc$sqrt.BALLOC=sqrt(vc$BALLOC)


#SquarerootTransformation
lm5_sqrt <- glm(formula = APURCH ~ age + atmcrd + paydep + sqrt.BALCHQ + sqrt.BALSAV + 
                 DUMNOLOAN + sqrt.BALLOC + BALMRGG + NEWLOC+ sqrt.TXBRAN + TOTSERV + CHNMSERV + 
                 valsegm + numrr1 + sqrt.avginc1,
               data = filter(vc, Sample == "Estimation"),
               family = binomial(logit)) 
summary(lm5_sqrt)

#squareTransformation
lm5_sqr <- glm(formula = APURCH ~ age + atmcrd + paydep + I(BALCHQ^2) + BALSAV + 
                  DUMNOLOAN + BALLOC + BALMRGG + NEWLOC + I(TXBRAN^2) + TOTSERV + CHNMSERV + 
                  valsegm + numrr1 + I(avginc1^2),
                data = filter(vc, Sample == "Estimation"),
                family = binomial(logit)) 
summary(lm5_sqr)

#ReciprocalTransformation
lm5_rec <- glm(formula = APURCH ~ age + atmcrd + paydep + 1/BALCHQ + BALSAV + 
                 DUMNOLOAN + BALLOC + BALMRGG + NEWLOC+ 1/TXBRAN + TOTSERV + CHNMSERV + 
                 valsegm + numrr1+ 1/avginc1,
               data = filter(vc, Sample == "Estimation"),
               family = binomial(logit)) 
summary(lm5_rec)


#LIFTCHART ON VALIDATION SAMPLE for deciding the final model
lift.chart(modelList = c("lm5","lm5_sqrt","lm5_log","lm5_sqr","lm5_rec"),
           data = filter(vc, Sample == "Validation"),
           targLevel = "Y", 
           trueResp = 0.021, 
           type = "cumulative", 
           sub = "Validation")

#final model lift chart
lift.chart(modelList = c("lm5_sqrt"),
           data = filter(vc, Sample == "Validation"),
           targLevel = "Y", 
           trueResp = 0.021, 
           type = "cumulative", 
           sub = "Validation")

lift.chart(modelList = c("lm5_sqrt"),
           data = filter(vc, Sample == "Validation"),
           targLevel = "Y", 
           trueResp = 0.021, 
           type = "incremental", 
           sub = "Validation")



  
  
  
  
  
  