library(ggplot2)
library(patchwork)
library(caret)


softmax <- function(x)
{
  exp_x <- exp(x - max(x))
  return(exp_x / sum(exp_x))
}

softmax_predict=function(beta,X,y,p,k)
{
  beta_matrix <- matrix(beta, nrow = p, ncol = k)
  scores <- X %*% beta_matrix
  softmax_scores <- t(apply(scores, 1, softmax))
  y_predict <- apply(softmax_scores,1,which.max)
  return(y_predict-1)
}

plot_function=function(result)
{
  p = length(result$theta[,1])

  g1 = ggplot(data.frame(x=1:p,y=result$theta[,1]),aes(x=x,y=y))+
    geom_point()+
    ggtitle("Class 1")

  g2 = ggplot(data.frame(x=1:p,y=result$theta[,2]),aes(x=x,y=y))+
    geom_point()+
    ggtitle("Class 3")

  g3 = ggplot(data.frame(x=1:p,y=result$theta[,3]),aes(x=x,y=y))+
    geom_point()+
    ggtitle("Class 4")

  g4 = ggplot(data.frame(x=1:p,y=result$theta[,4]),aes(x=x,y=y))+
    geom_point()+
    ggtitle("Class 5")

  g5 = ggplot(data.frame(x=1:p,y=result$theta[,5]),aes(x=x,y=y))+
    geom_point()+
    ggtitle("Class 6")

  g6 = ggplot(data.frame(x=1:p,y=result$theta[,6]),aes(x=x,y=y))+
    geom_point()+
    ggtitle("Class 9")

  g7 = ggplot(data.frame(x=1:p,y=result$theta[,7]),aes(x=x,y=y))+
    geom_point()+
    ggtitle("Class combined")

  g8 = ggplot(data.frame(x=2:length(result$loss),loss=result$loss[2:length(result$loss)]),
              aes(x=x,y=loss))+
    geom_line()+
    ggtitle("Loss")

  combined_plot=g1+g2+g3+g4+g5+g6+g7+g8+plot_layout(ncol=3)

  #ggsave(file.path(dir_name,"combined_plot.png"), plot = combined_plot, width = 10, height = 8, units = "in")

  print(combined_plot)

}

accuracy_function = function(result,x_matrix,y,p,k)
{
  class_list = c("Class 1","Class 3","Class 4","Class 5","Class 6","Class 9","Class Combined")

  y_predict = softmax_predict(result$theta,x_matrix,y,p,k)

  confusion_matrix = confusionMatrix(factor(y_predict),factor(y))$byClass[,c("Sensitivity","Specificity","F1","Balanced Accuracy")]

  rownames(confusion_matrix) = class_list

  #write.csv(confusion_matrix*100,file=file.path(dir_name,"confusion_matrix.csv"))

  return(confusion_matrix*100)
}
