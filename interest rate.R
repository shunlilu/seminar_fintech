rate_id=0.079273
r=0.016
bid=50
term=24
dt=0.695
#rate
#p=0.98
mr_id=as.integer(bid*rate_id/12*(1+rate_id/12)^term*100/((1+rate_id/12)^term-1))/100
v_id=mr_id*(1-1/(1+r/12)^term)/(r/12)
#mr_no=as.integer(bid*rate/12*(1+rate/12)^term*100/((1+rate/12)^term-1))/100
#v_no=mr_no*(1-1/(1+r)^term)/r
#v_d1=mr_no*(1-1/(1+r)^(as.integer(dt*term-1)))/r
#v_d=v_d1+(bid-v_d1)*(1+r)^(-(as.integer(dt*term)))
#vd*p+v_no*(1-p)=v_id
library(rootSolve)
fun<-function(rate){
  mr_no=as.integer(bid*rate/12*(1+rate/12)^term*100/((1+rate/12)^term-1))/100
  v_no=mr_no*(1-1/(1+r/12)^term)/(r/12)
  v_d1=mr_no*(1-1/(1+r/12)^(as.integer(dt*term-1)))/(r/12)
  v_d=v_d1+(bid-v_d1)*(1+r/12)^(-(as.integer(dt*term)))
  return (v_id-v_d1*p-v_no*(1-p))
}
uniroot(fun,c(0.00001,5))$root


data=read.csv("Desktop/prosper_default_rate_1.csv")
for (i in 1:28049){
  if (data$predict_default_rate[i]>0.9){
    data$predict_default_rate[i]=0.9
  }
}
data$predict_default_rate
rate_list=c()
for (i in 1:28049){
  p=data$predict_default_rate[i]
  bid=data$LoanOriginalAmount[i]
  term=data$Term[i]
  rate_id=0.079273
  r=0.016
  dt=0.695
  mr_id=as.integer(bid*rate_id/12*(1+rate_id/12)^term*100/((1+rate_id/12)^term-1))/100
  v_id=mr_id*(1-1/(1+r/12)^term)/(r/12)
  rate_list=c(rate_list,uniroot(fun,c(0.00001,5))$root)
}

data$predict_rate=rate_list
mean(data$predict_rate[data$Occupation.Computer.Programmer==1])
mean(data$predict_rate[data$Occupation.Executive==1])
mean(data$predict_rate[data$Occupation.Professional==1])
mean(data$predict_rate[data$Occupation.Food.Service==1])
mean(data$predict_rate[data$Occupation.Laborer==1])
mean(data$predict_rate[data$Occupation.Fireman==1])
mean(data$predict_rate[data$Occupation.Analyst==1])
mean(data$predict_rate[data$Occupation.Architect==1])
mean(data$predict_rate[data$Occupation.Teacher==1])
mean(data$predict_rate[data$Occupation.Waiter.Waitress==1])

mean(data$predict_rate[data$BorrowerState.IL==1])
mean(data$predict_rate[data$BorrowerState.NY==1])
mean(data$predict_rate[data$BorrowerState.OH==1])
mean(data$predict_rate[data$BorrowerState.TX==1])
mean(data$predict_rate[data$BorrowerState.CO==1])
mean(data$predict_rate[data$BorrowerState.MO==1])
mean(data$predict_rate[data$BorrowerState.LA==1])
mean(data$predict_rate[data$BorrowerState.CA==1])
mean(data$predict_rate[data$BorrowerState.MD==1])

write.csv(data,'Desktop/final predict.csv')
