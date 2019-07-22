
x=data$GNP+data$Unemployed+data$Armed.Forces+data$Year
x0=data$GNP
x1=data$Unemployed
x2=data$Armed.Forces
x3=data$Year
y=data$Employed
X=as.matrix(cbind(1,x0,x1,x2,x3))
alpha=0.01

error=function(beta){
  sum((X%*%beta-y)^2)
  }

del.b0=function(b0,b1,b2,b3,b4){
sum(-y+(b1*x0+b2*x1+b3*x2+b4*x3+b0))*(2/length(x0))}
del.b1=function(b0,b1,b2,b3,b4){
  sum(-y+(b1*x0+b2*x1+b3*x2+b4*x3+b0))*(2/length(x0))}
del.b1=function(b0,b1,b2,b3,b4){
  sum(-y+(b1*x0+b2*x1+b3*x2+b4*x3+b0))*(2/length(x0))}
del.b2=function(b0,b1,b2,b3,b4){
  sum(-y+(b1*x0+b2*x1+b3*x2+b4*x3+b0))*(2/length(x0))}
del.b3=function(b0,b1,b2,b3,b4){
  sum(-y+(b1*x0+b2*x1+b3*x2+b4*x3+b0))*(2/length(x0))}
del.b4=function(b0,b1,b2,b3,b4){
  + sum(-y+(b1*x0+b2*x1+b3*x2+b4*x3+b0))*(2/length(x0))}
b0=0
b1=0
 b2=0
 b3=0
 b4=0
error.history=numeric(length(x))
 b0.history=numeric(length(x))
 b1.history=numeric(length(x))
 b2.history=numeric(length(x))
 b3.history=numeric(length(x))
 b3.history=numeric(length(x))
 b4.history=numeric(length(x))
for(i in 1:length(x0)){
     b0.g=del.b0(b0,b1,b2,b3,b4)
     b1.g=del.b1(b0,b1,b2,b3,b4)
     b2.g=del.b2(b0,b1,b2,b3,b4)
     b3.g=del.b3(b0,b1,b2,b3,b4)
     b4.g=del.b4(b1,b1,b2,b3,b4)
     b0=b0-alpha*b0.g
     b1=b1-alpha*b1.g
     b2=b2-alpha*b2.g
     b3=b3-alpha*b3.g
     b4=b4-alpha*b4.g
     beta=as.matrix(c(b0,b1,b2,b3,b4))
     error.history[i]=error(beta)
     b0.history[i]=b0
     b1.history[i]=b1
     b2.history[i]=b2
     b3.history[i]=b3
     b4.history[i]=b4
   }
