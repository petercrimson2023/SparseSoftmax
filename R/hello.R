library(Rcpp)
library(nnet)
#sourceCpp("multi_logistic_L1.cpp")

sourceCpp("fista.cpp")



adata_nn = read.csv("adata_nn.csv",header=T)

y_raw = adata_nn[,"celltype"]
y_num = as.numeric(factor(y_raw))

p = ncol(adata_nn)-2
X_raw = adata_nn[,1:p]
X = matrix(unlist(X_raw),nrow=nrow(X_raw),ncol=p)
X = cbind(rep(1,nrow(X_raw)),X)
p = p+1
y = ifelse(y_num %in% c(2,7, 8, 10, 11, 12, 13),15,y_num)

### recoding y into 0-6

for (i in 1:length(y))
{
  if(y[i] == 1){
    y[i] = 0
  }else if(y[i] %in% c(3, 4, 5, 6)){
    y[i] = y[i]-2
  }else if(y[i] == 9){
    y[i] = 5
  }else
  {
    y[i] = 6
  }
}

k = length(table(y))
n = nrow(X)

beta0 = matrix(0.1,k*p,1)
lambda = 0.045
L_init = 1

y_one_hot=class.ind(y)


result = fista(lambda,L_init,beta0,X,y_one_hot,n=n,p=p,k=k)

source("evaluation.R")

plot_function(result)

accuracy_function(result,X,y,p,k)


