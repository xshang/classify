blood_transfusion <- read.csv("transfusion.data")
names(blood_transfusion) <- c("recency", "frequency", "monetary", "time", "donated")
blood_transfusion <- list(
  x = blood_transfusion[,-5],
  y = factor(blood_transfusion[,5])
)
save(blood_transfusion, file="blood_transfusion.RData")
