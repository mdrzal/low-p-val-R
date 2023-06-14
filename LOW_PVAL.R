set.seed(42)
library(dplyr)
library(stringr)

sequence1 <- 1:1000
sequence2 <- sequence1 + rnorm(length(sequence1), mean = 0, sd = 0.1)

A<-cor.test(sequence1,sequence2)

#P-value is equal to 0
A$p.value

#This function says that p value is < 2.2e-16
A

#calculare proper pvalue


corr2<-function(x,y){
  # correlation coefficient
  correlation_coefficient <- cor(x,y)
  # degrees of freedom
  n <- length(DATA$MAF)
  df <- n - 2
  #t-value
  t_value <- correlation_coefficient * sqrt(df) / sqrt(1 - correlation_coefficient^2)
  
  #p-value is saved as log
  p_value <- 2 * pt(abs(t_value), df = df, lower.tail = FALSE,  log.p = T)
  #transformed to a log of 10
  p_value<-p_value/log(10)
  #it converts the number to scientific notation based on this principle
  #10^(-123.0234) = 10^(-123)*10^(-0.0234) = 0.234*10^(-123) = 2.34E-124
  #NUM2 is E-124
  #NUM1 is 2.34
  NUM2<-p_value %>% as.character() %>% str_extract("\\..+") %>% str_remove("\\.")
  NUM2_<-as.numeric(NUM2)
  NUM2_<-NUM2_/paste(1,paste( rep(0,str_length(NUM2)+1), collapse = ""), collapse = "", sep = "")  %>% as.numeric()
  
  NUM1<-(p_value %>% as.character() %>% str_extract(".+\\.") %>% str_remove("\\.") %>% as.numeric)-1
  PVAL<-paste((NUM2_ %>% round(2)), "E-", -NUM1, sep = "")
  NUM1
  
  OUT<-data.frame(correlation_coefficient=correlation_coefficient, pval= PVAL)
  return(OUT)
  
  
  
}

corr2(sequence1,sequence2)
