library(caret);
library(kernlab);
data(spam);

mytrain <- read.csv("pml-training.csv");
feature.names <- names(mytrain)[2:ncol(mytrain)-1]
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
used.columns<-c("num_window","roll_belt","pitch_belt","yaw_belt","total_accel_belt",
                "gyros_belt_x","gyros_belt_y", "gyros_belt_z", "accel_belt_x", "accel_belt_y",
                "accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z","roll_arm",
                "pitch_arm","yaw_arm","total_accel_arm","gyros_arm_x",
                "gyros_arm_y","gyros_arm_z","accel_arm_x", "accel_arm_y","accel_arm_z","magnet_arm_x",
                "magnet_arm_y", "magnet_arm_z","roll_dumbbell","pitch_dumbbell","yaw_dumbbell",
                "total_accel_dumbbell","gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z",
                "accel_dumbbell_x","accel_dumbbell_y","accel_dumbbell_z", "magnet_dumbbell_x", 
                "magnet_dumbbell_y", "magnet_dumbbell_z","roll_forearm",
                "pitch_forearm","yaw_forearm","total_accel_forearm",
                "gyros_forearm_x","gyros_forearm_y","gyros_forearm_z","accel_forearm_x",
                "accel_forearm_y", "accel_forearm_z","magnet_forearm_x", "magnet_forearm_y", 
                "magnet_forearm_z","classe");
mytrain <-mytrain[,used.columns];

mytest <- read.csv("pml-testing.csv");
used.columns.test<-c("num_window","roll_belt","pitch_belt","yaw_belt","total_accel_belt",
                "gyros_belt_x","gyros_belt_y", "gyros_belt_z", "accel_belt_x", "accel_belt_y",
                "accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z","roll_arm",
                "pitch_arm","yaw_arm","total_accel_arm","gyros_arm_x",
                "gyros_arm_y","gyros_arm_z","accel_arm_x", "accel_arm_y","accel_arm_z","magnet_arm_x",
                "magnet_arm_y", "magnet_arm_z","roll_dumbbell","pitch_dumbbell","yaw_dumbbell",
                "total_accel_dumbbell","gyros_dumbbell_x", "gyros_dumbbell_y", "gyros_dumbbell_z",
                "accel_dumbbell_x","accel_dumbbell_y","accel_dumbbell_z", "magnet_dumbbell_x", 
                "magnet_dumbbell_y", "magnet_dumbbell_z","roll_forearm",
                "pitch_forearm","yaw_forearm","total_accel_forearm",
                "gyros_forearm_x","gyros_forearm_y","gyros_forearm_z","accel_forearm_x",
                "accel_forearm_y", "accel_forearm_z","magnet_forearm_x", "magnet_forearm_y", 
                "magnet_forearm_z");
mytest <- mytest[,used.columns.test];

####BEGIN: Data cleaning########
feature.names <- names(mytrain)[2:ncol(mytrain)-1]
cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(mytrain[[f]])=="character") {
    levels <- unique(c(mytrain[[f]], mytest[[f]]))
    mytrain[[f]] <- as.integer(factor(mytrain[[f]], levels=levels))
    mytest[[f]]  <- as.integer(factor(mytest[[f]],  levels=levels))
  }
}
cat("replacing missing values with -1\n")
mytrain[is.na(mytrain)] <- -1
mytest[is.na(mytest)]   <- -1
####END: Data clearning#######

inTrain <- createDataPartition(y=mytrain$classe, p=0.75,list=FALSE);
training <- mytrain[inTrain,];
testing <- mytrain[-inTrain,];
modelFit<- train(classe ~.,data=training, method = "rf", tuneGrid = data.frame(mtry = 3));
# cross validation
predictions <-predict(modelFit,newdata=testing);
confusionMatrix(predictions,testing$type);
# real prediction
answers <-predict(modelFit,newdata=mytest);
pml_write_files(answers);
# inTrain <- createDataPartition(y=spam$type, p=0.75,list=FALSE);
# training <- spam[inTrain,];
# testing <- spam[-inTrain,];
# set.seed(32343);
# modelFit<- train(type ~.,data=training,method="glm");
# predictions <-predict(modelFit,newdata=testing);
# confusionMatrix(predictions,testing$type);