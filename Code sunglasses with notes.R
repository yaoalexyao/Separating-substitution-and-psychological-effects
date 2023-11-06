################Code sunglasses

################This code estimate sunglasses conjoint model

########################################################################################################
############################################### Import  ################################################
########################################################################################################


n = 1020  #the number of respondents

###########Product and consumer information in the conjoint
brand <- read.csv("brand.csv")
color = read.csv("color.csv")
shape = read.csv("shape.csv")
price = read.csv("price.csv")
budget = read.csv("budget.csv")
outside = budget-price
Demo <- read.csv("Demo.csv")

############Dummy coding
brand1 = (brand==1)*1
brand2 = (brand==2)*1

colorlight = (color==2)*1
colordark = (color==1)*1

shaperound = (shape==1)*1
shapesq = (shape==2)*1
shapeother = (shape==3)*1



####choices of general and driver DCE tasks
choicema <- read.csv("choice.csv")
sigma <- read.csv("identity.csv")
selfma <- read.csv("self reward.csv")
qualityma <- read.csv("quality.csv")




########################################################################################################
############################################### Estimation##############################################
########################################################################################################


#############Step 1: to establish the existence of the psychological effect

likelihoodforeachpersontype1=rep(0,n)
likelihoodforeachpersontype2=rep(0,n)
price = price/100
budget = budget/100
###########likelihood function for a latent class
fchoice<-function(x){
  inter=x[1:2]
  betaprice=x[3]
  alpha12=x[4]
  cocolor=x[5]
  coshape=x[6:7]
  
  inter2=x[8:9]
  betaprice2=x[10]
  alpha122=x[11]
  cocolor2=x[12]
  coshape2=x[13:14]
  
  segmentco=x[15:20]  #parameters for latent class
  
  prAll1=exp(inter[1]*brand1 + inter[2]*brand2 + cocolor*colorlight + 
               coshape[1]*shaperound  + coshape[2]*shapesq + betaprice*log(price+1) + alpha12*log(outside+1)  )
  
  prAll2=exp(inter2[1]*brand1 + inter2[2]*brand2 + cocolor2*colorlight + 
               coshape2[1]*shaperound + coshape2[2]*shapesq + betaprice2*log(price+1) + alpha122*log(outside+1)  )
  
  
  prAll1=rowSums(choicema*prAll1/rowSums(prAll1))
  prAll2=rowSums(choicema*prAll2/rowSums(prAll2))
  
  for (i in 1:n) {
    likelihoodforeachpersontype1[i]=prod(prAll1[((i-1)*8+1):(i*8)])
    likelihoodforeachpersontype2[i]=prod(prAll2[((i-1)*8+1):(i*8)])
  }
  
  prob10=exp(segmentco[1]+Demo[,1]*segmentco[2]+Demo[,2]*segmentco[3]+Demo[,3]*segmentco[4]+Demo[,4]*segmentco[5]+
               Demo[,5]*segmentco[6])
  
  -(sum(log(prob10*likelihoodforeachpersontype1+likelihoodforeachpersontype2))-sum(log(prob10+1)))
  
}


set.seed(6117)
init = rnorm(20)

conj1choice=optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice

#Writes out coefficients to calculate the segment size required for next steps. 
#write.csv(conj1choice$par, file = "Step 1 coefficients.csv", row.names = F) 






##########Obtain segment size at the aggregate level and membership for each respondent
######Note: here I assume segment 1 has the positive psychological effect (consistent with the results in Step 1)

likelihoodforeachpersontype1 = rep(0,n)
likelihoodforeachpersontype2 = rep(0,n)

x <- read.csv("Step 1 coefficients.csv")
x = x[,1]
segmentco = x[15:20]

prob10=exp(segmentco[1]+Demo[,1]*segmentco[2]+Demo[,2]*segmentco[3]+Demo[,3]*segmentco[4]+Demo[,4]*segmentco[5]+
             Demo[,5]*segmentco[6])


size1 = prob10/(prob10+1)
size2 = 1/(prob10+1)


inter=x[1:2]
betaprice=x[3]
alpha12=x[4]
cocolor=x[5]
coshape=x[6:7]

inter2=x[8:9]
betaprice2=x[10]
alpha122=x[11]
cocolor2=x[12]
coshape2=x[13:14]

segmentco=x[15:20]

prAll1=exp(inter[1]*brand1 + inter[2]*brand2 + cocolor*colorlight + 
             coshape[1]*shaperound  + coshape[2]*shapesq + betaprice*log(price+1) + alpha12*log(outside+1)  )

prAll2=exp(inter2[1]*brand1 + inter2[2]*brand2 + cocolor2*colorlight + 
             coshape2[1]*shaperound + coshape2[2]*shapesq + betaprice2*log(price+1) + alpha122*log(outside+1)  )


prAll1=rowSums(choicema*prAll1/rowSums(prAll1))
prAll2=rowSums(choicema*prAll2/rowSums(prAll2))

for (i in 1:n) {
  likelihoodforeachpersontype1[i]=prod(prAll1[((i-1)*8+1):(i*8)])
  likelihoodforeachpersontype2[i]=prod(prAll2[((i-1)*8+1):(i*8)])
}

probseg1 =  likelihoodforeachpersontype1*size1 / (likelihoodforeachpersontype1*size1 + likelihoodforeachpersontype2*size2)
probseg2 = 1-probseg1

sizepanel = cbind(probseg1,probseg2)
sizema = t(apply(sizepanel, 1, function(x) x==max(x) ) )



#segment membership for segment 1 and 2
sizema = sizema * 1
summary(sizema)


















###################Step 2 model driver indicators' impact on choice for Segment 1 (with positive psychological effect)


#replicate 8 times because each respondent answers 8 conjoint questions in each task. 
sizema1by8 = rep(sizema[,1] , rep(8,n) )
likelihoodforeachpersontype1=rep(0,colSums(sizema)[1])
likelihoodforeachpersontype2=rep(0,colSums(sizema)[1])


###########likelihood function to estimate only respondents in Segment 1
fchoice<-function(x){
  inter=x[1:2]
  betaprice=x[3]
  alpha12=x[4]
  cocolor=x[5]
  coshape=x[6:7]
  cosig=x[8]
  coself=x[9]
  coqual=x[10]  
  
  
  inter2=x[11:12]
  betaprice2=x[13]
  alpha122=x[14]
  cocolor2=x[15]
  coshape2=x[16:17]
  cosig2=x[18]
  coself2=x[19]
  coqual2=x[20]
  
  segmentco=x[21:26]
  
  
  prAll1=exp(inter[1]*brand1[sizema1by8==1,] + inter[2]*brand2[sizema1by8==1,] + cocolor*colorlight[sizema1by8==1,] + 
               coshape[1]*shaperound[sizema1by8==1,]  + coshape[2]*shapesq[sizema1by8==1,] + betaprice*log(price[sizema1by8==1,]+1) + alpha12*log(outside[sizema1by8==1,]+1) +
               cosig*cbind(sigma[sizema1by8==1,],0) + coself*cbind(selfma[sizema1by8==1,],0) + coqual*cbind(qualityma[sizema1by8==1,],0) )
  
  prAll2=exp(inter2[1]*brand1[sizema1by8==1,] + inter2[2]*brand2[sizema1by8==1,] + cocolor2*colorlight[sizema1by8==1,] + 
               coshape2[1]*shaperound[sizema1by8==1,]  + coshape2[2]*shapesq[sizema1by8==1,] + betaprice2*log(price[sizema1by8==1,]+1) + alpha122*log(outside[sizema1by8==1,]+1) +
               cosig2*cbind(sigma[sizema1by8==1,],0) + coself2*cbind(selfma[sizema1by8==1,],0) + coqual2*cbind(qualityma[sizema1by8==1,],0) )
  
  prAll1=rowSums(choicema[sizema1by8==1,]*prAll1/rowSums(prAll1))
  prAll2=rowSums(choicema[sizema1by8==1,]*prAll2/rowSums(prAll2))
  
  for (i in 1:colSums(sizema)[1]) {
    likelihoodforeachpersontype1[i]=prod(prAll1[((i-1)*8+1):(i*8)])
    likelihoodforeachpersontype2[i]=prod(prAll2[((i-1)*8+1):(i*8)])
  }
  
  prob10=exp(segmentco[1]+Demo[sizema[,1]==1,1]*segmentco[2]+Demo[sizema[,1]==1,2]*segmentco[3]+Demo[sizema[,1]==1,3]*segmentco[4]+Demo[sizema[,1]==1,4]*segmentco[5]+
               Demo[sizema[,1]==1,5]*segmentco[6])
  
  -(sum(log(prob10*likelihoodforeachpersontype1+likelihoodforeachpersontype2))-sum(log(prob10+1)))
  
}




set.seed(51326)
init=runif(26) 

conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice
#Writes out coefficients to calculate the segment size required for the next step. 
#write.csv(conj1choice$par, file = "Step 2 coefficients.csv", row.names = F) 




##################Get segment size for segment 1a and 1b
##########Obtain segment size at the aggregate level and membership for each respondent
######Note: here I assume segment 1a is the prestige seeker segment (consistent with results in the last step)


Step2coe <- read.csv("Step 2 coefficients.csv")

likelihoodforeachpersontype1=rep(0,colSums(sizema)[1])
likelihoodforeachpersontype2=rep(0,colSums(sizema)[1])

segmentco = Step2coe[21:26,]

prob10=exp(segmentco[1]+Demo[sizema[,1]==1,1]*segmentco[2]+Demo[sizema[,1]==1,2]*segmentco[3]+Demo[sizema[,1]==1,3]*segmentco[4]+Demo[sizema[,1]==1,4]*segmentco[5]+
             Demo[sizema[,1]==1,5]*segmentco[6])


size1a = prob10/(prob10+1)
size1b = 1/(prob10+1)

x = Step2coe[,1]
inter=x[1:2]
betaprice=x[3]
alpha12=x[4]
cocolor=x[5]
coshape=x[6:7]
cosig=x[8]
coself=x[9]
coqual=x[10]  


inter2=x[11:12]
betaprice2=x[13]
alpha122=x[14]
cocolor2=x[15]
coshape2=x[16:17]
cosig2=x[18]
coself2=x[19]
coqual2=x[20]

prAll1=exp(inter[1]*brand1[sizema1by8==1,] + inter[2]*brand2[sizema1by8==1,] + cocolor*colorlight[sizema1by8==1,] + 
             coshape[1]*shaperound[sizema1by8==1,]  + coshape[2]*shapesq[sizema1by8==1,] + betaprice*log(price[sizema1by8==1,]+1) + alpha12*log(outside[sizema1by8==1,]+1) +
             cosig*cbind(sigma[sizema1by8==1,],0) + coself*cbind(selfma[sizema1by8==1,],0) + coqual*cbind(qualityma[sizema1by8==1,],0) )

prAll2=exp(inter2[1]*brand1[sizema1by8==1,] + inter2[2]*brand2[sizema1by8==1,] + cocolor2*colorlight[sizema1by8==1,] + 
             coshape2[1]*shaperound[sizema1by8==1,]  + coshape2[2]*shapesq[sizema1by8==1,] + betaprice2*log(price[sizema1by8==1,]+1) + alpha122*log(outside[sizema1by8==1,]+1) +
             cosig2*cbind(sigma[sizema1by8==1,],0) + coself2*cbind(selfma[sizema1by8==1,],0) + coqual2*cbind(qualityma[sizema1by8==1,],0) )


prAll1=rowSums(choicema[sizema1by8==1,]*prAll1/rowSums(prAll1))
prAll2=rowSums(choicema[sizema1by8==1,]*prAll2/rowSums(prAll2))

for (i in 1:colSums(sizema)[1]) {
  likelihoodforeachpersontype1[i]=prod(prAll1[((i-1)*8+1):(i*8)])
  likelihoodforeachpersontype2[i]=prod(prAll2[((i-1)*8+1):(i*8)])
}

probseg1a =  likelihoodforeachpersontype1*size1a / (likelihoodforeachpersontype1*size1a + likelihoodforeachpersontype2*size1b)
probseg1b = 1-probseg1a


sizepanel = cbind(probseg1a,probseg1b)
sizeseg1ma = t(apply(sizepanel, 1, function(x) x==max(x) ) )
summary((sizeseg1ma*1))












###################Step 2 model driver indicators' impact on choice for Segment 2


###########likelihood function
fchoice<-function(x){
  inter=x[1:2]
  betaprice=x[3]
  alpha12=x[4]
  cocolor=x[5]
  coshape=x[6:7]
  cosig=x[8]
  coself=x[9]
  coqual=x[10]  
  
  prAll1=exp(inter[1]*brand1[sizema1by8==0,] + inter[2]*brand2[sizema1by8==0,] + cocolor*colorlight[sizema1by8==0,] + 
               coshape[1]*shaperound[sizema1by8==0,]  + coshape[2]*shapesq[sizema1by8==0,] + betaprice*log(price[sizema1by8==0,]+1) + alpha12*log(outside[sizema1by8==0,]+1) +
               cosig*cbind(sigma[sizema1by8==0,],0) + coself*cbind(selfma[sizema1by8==0,],0) + coqual*cbind(qualityma[sizema1by8==0,],0) )
  
  prAll1=rowSums(choicema[sizema1by8==0,]*prAll1/rowSums(prAll1))
  
  -sum(log(prAll1)) 
}



set.seed(1041)
init=runif(10) 
conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice




###############Here summarize the segment membership for segment 1a, segment 1b, and segment 2

segment1amember = rep(0,n)
segment1amember[sizema[,1]==1] = sizeseg1ma[,1]

segment1bmember = rep(0,n)
segment1bmember[sizema[,1]==1] = sizeseg1ma[,2]


segmentall = cbind(segment1amember,segment1bmember,sizema[,2])
colnames(segmentall) = c("segment1a" , "segment1b", "segment2")
summary(segmentall)





















#####################Step 3: model price effect on driver indicators

##### replicate the membership by 8 because each respondent answers 8 conjoint questions in each task
segmentallby8 = apply( segmentall, 2, function(x) {rep(x , rep(8,n) )} ) 





##################Part 3-1: identity match model for each segment 
# segment 1a
fchoice<-function(x){
  inter=x[1]
  betaprice=x[2]
  cocolor=x[3]
  coshape=x[4:5]
  
  prAll1=exp(inter[1]*brand1[segmentallby8[,1]==1,1:4] + cocolor*colorlight[segmentallby8[,1]==1,1:4] + 
               coshape[1]*shaperound[segmentallby8[,1]==1,1:4]  + coshape[2]*shapesq[segmentallby8[,1]==1,1:4] +  betaprice*log(price[segmentallby8[,1]==1,1:4]+1) )
  
  prAll1=rowSums(sigma[segmentallby8[,1]==1,]*prAll1/rowSums(prAll1))
  
  -sum(log(prAll1)) 
}

set.seed(1123)
init=runif(5)
conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice


#segment 1b
fchoice<-function(x){
  inter=x[1]
  betaprice=x[2]
  cocolor=x[3]
  coshape=x[4:5]
  
  prAll1=exp(inter[1]*brand1[segmentallby8[,2]==1,1:4] + cocolor*colorlight[segmentallby8[,2]==1,1:4] + 
               coshape[1]*shaperound[segmentallby8[,2]==1,1:4]  + coshape[2]*shapesq[segmentallby8[,2]==1,1:4] +  betaprice*log(price[segmentallby8[,2]==1,1:4]+1) )
  
  
  prAll1=rowSums(sigma[segmentallby8[,2]==1,]*prAll1/rowSums(prAll1))
  
  -sum(log(prAll1)) 
}


set.seed(1007)
init=runif(5)

conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice



#segment 2
fchoice<-function(x){
  inter=x[1]
  betaprice=x[2]
  cocolor=x[3]
  coshape=x[4:5]
  
  prAll1=exp(inter[1]*brand1[segmentallby8[,3]==1,1:4] + cocolor*colorlight[segmentallby8[,3]==1,1:4] + 
               coshape[1]*shaperound[segmentallby8[,3]==1,1:4]  + coshape[2]*shapesq[segmentallby8[,3]==1,1:4] +  betaprice*log(price[segmentallby8[,3]==1,1:4]+1) )
  
  prAll1=rowSums(sigma[segmentallby8[,3]==1,]*prAll1/rowSums(prAll1))
  
  -sum(log(prAll1)) 
}



set.seed(1007)
init=runif(5)

conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice










##################Part 3-2: self-rewarding model for each segment 
# segment 1a
fchoice<-function(x){
  inter=x[1]
  betaprice=x[2]
  cocolor=x[3]
  coshape=x[4:5]
  
  prAll1=exp(inter[1]*brand1[segmentallby8[,1]==1,1:4] + cocolor*colorlight[segmentallby8[,1]==1,1:4] + 
               coshape[1]*shaperound[segmentallby8[,1]==1,1:4]  + coshape[2]*shapesq[segmentallby8[,1]==1,1:4] +  betaprice*log(price[segmentallby8[,1]==1,1:4]+1) )
  
  prAll1=rowSums(selfma[segmentallby8[,1]==1,]*prAll1/rowSums(prAll1))
  
  -sum(log(prAll1)) 
}
set.seed(1007)
init=runif(5)
conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice






#segment 1b
fchoice<-function(x){
  inter=x[1]
  betaprice=x[2]
  cocolor=x[3]
  coshape=x[4:5]
  
  prAll1=exp(inter[1]*brand1[segmentallby8[,2]==1,1:4] + cocolor*colorlight[segmentallby8[,2]==1,1:4] + 
               coshape[1]*shaperound[segmentallby8[,2]==1,1:4]  + coshape[2]*shapesq[segmentallby8[,2]==1,1:4] +  betaprice*log(price[segmentallby8[,2]==1,1:4]+1) )
  
  prAll1=rowSums(selfma[segmentallby8[,2]==1,]*prAll1/rowSums(prAll1))
  
  -sum(log(prAll1)) 
}

set.seed(1007)
init=runif(5)
conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice



#segment 2
fchoice<-function(x){
  inter=x[1]
  betaprice=x[2]
  cocolor=x[3]
  coshape=x[4:5]
  
  prAll1=exp(inter[1]*brand1[segmentallby8[,3]==1,1:4] + cocolor*colorlight[segmentallby8[,3]==1,1:4] + 
               coshape[1]*shaperound[segmentallby8[,3]==1,1:4]  + coshape[2]*shapesq[segmentallby8[,3]==1,1:4] +  betaprice*log(price[segmentallby8[,3]==1,1:4]+1) )
  
  prAll1=rowSums(selfma[segmentallby8[,3]==1,]*prAll1/rowSums(prAll1))
  
  -sum(log(prAll1)) 
}
set.seed(1007)
init=runif(5)
conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice






















##################Part 3-3:  quality model for each segment 
#segment 1a
fchoice<-function(x){
  inter=x[1]
  betaprice=x[2]
  cocolor=x[3]
  coshape=x[4:5]
  
  prAll1=exp(inter[1]*brand1[segmentallby8[,1]==1,1:4] + cocolor*colorlight[segmentallby8[,1]==1,1:4] + 
               coshape[1]*shaperound[segmentallby8[,1]==1,1:4]  + coshape[2]*shapesq[segmentallby8[,1]==1,1:4] +  betaprice*log(price[segmentallby8[,1]==1,1:4]+1) )
  
  prAll1=rowSums(qualityma[segmentallby8[,1]==1,]*prAll1/rowSums(prAll1))
  
  -sum(log(prAll1)) 
}
set.seed(1007)
init=runif(5)
conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice









#segment 1b
fchoice<-function(x){
  inter=x[1]
  betaprice=x[2]
  cocolor=x[3]
  coshape=x[4:5]
  
  prAll1=exp(inter[1]*brand1[segmentallby8[,2]==1,1:4] + cocolor*colorlight[segmentallby8[,2]==1,1:4] + 
               coshape[1]*shaperound[segmentallby8[,2]==1,1:4]  + coshape[2]*shapesq[segmentallby8[,2]==1,1:4] +  betaprice*log(price[segmentallby8[,2]==1,1:4]+1) )
  
  prAll1=rowSums(qualityma[segmentallby8[,2]==1,]*prAll1/rowSums(prAll1))
  
  -sum(log(prAll1)) 
}
set.seed(1007)
init=runif(5)
conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice










#segment 2
fchoice<-function(x){
  inter=x[1]
  betaprice=x[2]
  cocolor=x[3]
  coshape=x[4:5]
  
  prAll1=exp(inter[1]*brand1[segmentallby8[,3]==1,1:4] + cocolor*colorlight[segmentallby8[,3]==1,1:4] + 
               coshape[1]*shaperound[segmentallby8[,3]==1,1:4]  + coshape[2]*shapesq[segmentallby8[,3]==1,1:4] +  betaprice*log(price[segmentallby8[,3]==1,1:4]+1) )
  
  prAll1=rowSums(qualityma[segmentallby8[,3]==1,]*prAll1/rowSums(prAll1))
  
  -sum(log(prAll1)) 
}
set.seed(1007)
init=runif(5)
conj1choice = optim(init,fchoice,method="BFGS",hessian=T,control=list(maxit=1000))
conj1choice$par   #
stderrconj1choice=sqrt(abs(diag(solve(conj1choice$hessian) ) ) )    #get standard error
stderrconj1choice






