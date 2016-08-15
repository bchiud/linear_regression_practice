#======================================================================
# Title: Regression Practice
# Author: Brady Chiu
# Date: August 12, 2016
#======================================================================
## @knitr notes

## @knitr setup
work_dir="/Users/bradychiu/Dropbox/R/regression_practice"
data_dir="data/"
deliverable_dir="deliverables/"
# setwd(work_dir)

package_loader<-function(package_names){
  for(i in package_names){
    if(!i %in% rownames(installed.packages())) install.packages(i, repos="http://cran.r-project.org")
    library(i, character.only=T)
  }
}
package_names<-c("car", "corrplot", "datasets", "data.table", "dplyr", "ggplot2", "knitr", "lubridate", "psych", "stringr", "tidyr")
package_loader(package_names)

## @knitr functions
get_data<-function(data_file_paths){
  bind_rows(lapply(data_file_paths, fread, na.strings="", stringsAsFactors=F)) %>%
    data.frame()
}

get_table<-function(df,dig=2){
  kable(df, align='r', digits=dig, format.args=list(big.mark=","))
}

## @knitr parameters

## @knitr data
data(swiss)

swiss %>%
  gather(variable, value, Fertility:Infant.Mortality) %>%
  ggplot(aes(x=variable, y=value, fill=variable))+
  geom_boxplot()+
  ggtitle("Swiss")+
  scale_x_discrete(name="Variable")+
  scale_y_continuous(name="Value")+
  theme(legend.position="none")

swiss %>%
  head() %>%
  get_table()

swiss %>%
  describe() %>%
  get_table()

## @knitr multicolinearity_corrplot


corrplot(cor(swiss %>%
               dplyr::select(-Fertility)))

## @knitr multicolinearity_pca

swiss.pca<-prcomp(swiss %>%
                    dplyr::select(-Fertility))
swiss.pca
summary(swiss.pca)

swiss.eig<-data.frame(eigenvalue=(swiss.pca$sdev)^2) %>%
  mutate(percent_variance=eigenvalue*100/sum(eigenvalue),
         cumulative_percent_variance=cumsum(percent_variance))
swiss.eig %>%
  get_table()

swiss.eig %>%
  ggplot(aes(x=row.names(.), y=percent_variance, fill=row.names(.)))+
  geom_bar(stat="identity")+
  scale_x_discrete("Principal Components")+
  scale_y_continuous("Percent of Variance",
                     breaks=seq(0,100,20),
                     labels=paste0(seq(0,100,20), "%"))+
  theme(legend.position="none")

biplot(swiss.pca, cex=0.6)

## @knitr multicolinearity_vif

swiss.fit<-glm(Fertility~., data=swiss)
summary(swiss.fit)
vif(swiss.fit)

## @knitr linear_model

swiss.fit<-glm(Fertility~., data=swiss)
summary(swiss.fit)


swiss.fit1<-glm(Fertility~Agriculture, data=swiss)
swiss.fit2<-update(swiss.fit1, .~.+Examination)
swiss.fit3<-update(swiss.fit2, .~.+Education)
swiss.fit4<-update(swiss.fit3, .~.+Catholic)
swiss.fit5<-update(swiss.fit4, .~.+Infant.Mortality)

anova(swiss.fit1, swiss.fit2, swiss.fit3, swiss.fit4, swiss.fit5, test="F")

## sequential table
fit <- lm(sr ~ ., data = LifeCycleSavings)
anova(fit)

## same effect via separate models
fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
fit1 <- update(fit0, . ~ . + pop15)
fit2 <- update(fit1, . ~ . + pop75)
fit3 <- update(fit2, . ~ . + dpi)
fit4 <- update(fit3, . ~ . + ddpi)
anova(fit0, fit1, fit2, fit3, fit4, test = "F")


