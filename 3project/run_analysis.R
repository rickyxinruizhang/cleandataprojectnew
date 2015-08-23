analyse<-function(){
  library("dplyr")
  library("data.table")
  trainfile<-file("./UCI HAR Dataset/train/X_train.txt")
  testfile<-file("./UCI HAR Dataset/test/X_test.txt")
  trainac<-file("./UCI HAR Dataset/train/y_train.txt")
  testac<-file("./UCI HAR Dataset/test/y_test.txt")
  trainsub<-file("./UCI HAR Dataset/train/subject_train.txt")
  testsub<-file("./UCI HAR Dataset/test/subject_test.txt")
  labelfile<-file("./UCI HAR Dataset/activity_labels.txt")
  desfile<-file("./UCI HAR Dataset/features.txt")
  
  trainset<-read.csv(trainfile,colClasses="numeric",nrows=7352,sep="",header=FALSE,row.names=NULL)
  testset<-read.csv(testfile,colClasses="numeric",nrows=2947,sep="",header=FALSE,row.names=NULL)
  trainsub<-read.csv(trainsub,colClasses="integer",nrows=7352,sep="",header=FALSE,row.names=NULL)
  testsub<-read.csv(testsub,colClasses="integer",nrows=2947,sep="",header=FALSE,row.names=NULL)
  trainac<-read.csv(trainac,colClasses="integer",nrows=7352,sep="",header=FALSE,row.names=NULL)
  testac<-read.csv(testac,colClasses="integer",nrows=2947,sep="",header=FALSE,row.names=NULL)
  vades<-read.csv(desfile,colClasses="character", nrows=561, sep="",header=FALSE, row.names=NULL)
  label<-read.csv(labelfile,colClasses="character", nrows=6, sep="",header=FALSE, row.names=NULL)
  
  hhh<<-trainsub
  names(trainset)<-as.character(1:ncol(trainset))
  trainset1<-data.frame(subject=unlist(trainsub),activity=unlist(trainac),trainset)
  
  names(testset)<-as.character(1:ncol(testset))
  testset1<-data.frame(subject=unlist(testsub),activity=unlist(testac),testset)

  #step 1
  newset<-merge(trainset1,testset1,all=TRUE)
  
  #step 4
  names(newset)[3:563]<-as.character(vades[[2]])
  
  #step 2
  newset<-newset[,sort(c(1:2,unique(c(grep("mean",names(newset)),grep("std",names(newset)),grep("Mean",names(newset))))))]
  
  #step 3
  newset[,2]<-label[as.numeric(newset[[2]]),2]
  
  #step 5
  newset<-newset %>% group_by(subject,activity)%>% summarise_each(funs(mean(.,na.rm=TRUE)))

  #write file
  file<-file("newset.txt","w")
  write.table(newset,file=file,row.names=FALSE,col.names=TRUE, sep="\t", quote=FALSE)
  #   newset<-aggregate(.~c(subject,activity),newset,mean)  
#  close(trainfile)
#  close(testfile)
#  close(trainac)
#  close(testac)
#  close(trainsub)
#  close(testsub)
#  close(labelfile)
#  extract=vector("logical",ncol(newset))
#  extract[1:2]<-TRUE
#  for (i in 3:ncol(newset)) {
#    if (pmatch("mean",names(newset)[i],nomatch=2)==1||pmatch("std",names(newset)[i],nomatch=2)==1){
#      extract[i]<-TRUE            
#    }
#    else{
#      extract[i]<-FALSE
#    }
#  }
#  pp<<-newset
  
#  dim(newset)
}