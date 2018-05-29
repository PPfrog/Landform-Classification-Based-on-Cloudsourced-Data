library(outliers)
#read.csv("C:\\Users\\zzc981026\\Desktop\\ERG2050\\training.csv",header=T)
data.all=read.csv("training.csv", head = T)

#classify the dataset by the classes

forest.index = which(data.all$class=='forest')
forest.data = data.all[forest.index,]

farm.index = which(data.all$class=='farm')
farm.data = data.all[farm.index,]

grass.index = which(data.all$class=='grass')
grass.data = data.all[grass.index,]

impervious.index = which(data.all$class=='impervious')
impervious.data = data.all[impervious.index,]

orchard.index = which(data.all$class=='orchard')
orchard.data = data.all[orchard.index,]

water.index = which(data.all$class=='water')
water.data = data.all[water.index,]

#Change the type of the data from character to numeric.

for (i in 1:28){
  for (k in 1:nrow(forest.data)){
    forest.data[k,i+1] = as.numeric(forest.data[k,i+1])  
  }
}

for (i in 1:28){
  for (k in 1:nrow(farm.data)){
    farm.data[k,i+1] = as.numeric(farm.data[k,i+1])  
  }
}

for (i in 1:28){
  for (k in 1:nrow(grass.data)){
    grass.data[k,i+1] = as.numeric(grass.data[k,i+1])  
  }
}

for (i in 1:28){
  for (k in 1:nrow(impervious.data)){
    impervious.data[k,i+1] = as.numeric(impervious.data[k,i+1])  
  }
}

for (i in 1:28){
  for (k in 1:nrow(orchard.data)){
    orchard.data[k,i+1] = as.numeric(orchard.data[k,i+1])  
  }
}

for (i in 1:28){
  for (k in 1:nrow(water.data)){
    water.data[k,i+1] = as.numeric(water.data[k,i+1])  
  }
}

data.classified=rbind(farm.data,forest.data,grass.data,impervious.data,orchard.data,water.data)

#chi-suqare test to find the outliers:
#forest  with p.value=0.005 K=3
for (m in 1:28){
  for (n in 1:nrow(forest.data)){
    chisq.out.test(forest.data[,m+1])
    x=chisq.out.test(forest.data[,m+1])
    if (x$p.value<0.005) 
      forest.data[which.min(forest.data[,m+1]),m+1]=NA 
    else break
  }
  for (n in 1:nrow(forest.data)){
    chisq.out.test(forest.data[,m+1],opposite = TRUE)
    x=chisq.out.test(forest.data[,m+1],opposite = TRUE)
    if (x$p.value<0.005) 
      forest.data[which.max(forest.data[,m+1]),m+1]=NA
    else break
  }
}
number.outliers.forest=sum(is.na(forest.data)) #the number of outliers in forest
#test if there is any outlier class
for (i in 1:nrow(forest.data)){
  if (sum(is.na(forest.data[i,]))>2)
    forest.data[i,1]=NA
  else next
}
number.outliers.class.forest=sum(is.na(forest.data[,1])) # the number of outliers of class in forest
#delete the wrong class
index.outlier.forest=which(is.na(forest.data$class))
forest.data.prune=forest.data[-index.outlier.forest,] #the pruned data
forest.prune.left=sum(is.na(forest.data.prune))
#convert the outlier to the normal values
#converted by mean`
for (m in 1:27){
  for (n in 1:nrow(forest.data.prune)){
    if (is.na(forest.data.prune[n,m+2]))
      forest.data.prune[n,m+2]=mean(forest.data.prune[,m+2],na.rm= T) 
    else next
  }
} 

#converted by median
#for (m in 1:28){
  #for (n in 1:nrow(forest.data.prune)){
    #if (is.na(forest.data.prune[n,m+1]==TRUE)
        #forest.data.prune[n,m+1]==median(forest.data.prune[,m+1],na.rm = FALSE)
        #else next
  #}
#}

for (n in 1:nrow(forest.data.prune)){
  if (is.na(forest.data.prune[n,2]))
    forest.data.prune[n,2]=max(forest.data.prune[n,-1],na.rm= T) 
  else next
}

#farm p.value=0.0005 K=4
for (m in 1:28){
  for (n in 1:nrow(farm.data)){
    chisq.out.test(farm.data[,m+1])
    x=chisq.out.test(farm.data[,m+1])
    if (x$p.value<0.0005) 
      farm.data[which.min(farm.data[,m+1]),m+1]=NA 
    else break
  }
  for (n in 1:nrow(farm.data)){
    chisq.out.test(farm.data[,m+1],opposite = TRUE)
    x=chisq.out.test(farm.data[,m+1],opposite = TRUE)
    if (x$p.value<0.0005) 
      farm.data[which.max(farm.data[,m+1]),m+1]=NA
    else break
  }
}
number.outliers.farm=sum(is.na(farm.data)) #the number of outliers in farm
#test if there is any outlier class
for (i in 1:nrow(farm.data)){
  if (sum(is.na(farm.data[i,]))>3)
    farm.data[i,1]=NA
  else next
}
number.outliers.class.farm=sum(is.na(farm.data[,1])) # the number of outliers of class in farm
#delete the wrong class
index.outlier.farm=which(is.na(farm.data$class))
farm.data.prune=farm.data[-index.outlier.farm,] #the pruned data
farm.prune.left=sum(is.na(farm.data.prune))
#convert the outlier to the normal values
#converted by mean`
for (m in 1:27){
  for (n in 1:nrow(farm.data.prune)){
    if (is.na(farm.data.prune[n,m+2]))
      farm.data.prune[n,m+2]=mean(farm.data.prune[,m+2],na.rm= T)
    else next
  }
} 

#converted by median
#for (m in 1:27){
  #for (n in 1:nrow(farm.data.prune)){
    #if (is.na(farm.data.prune[n,m+1]==TRUE)
      #farm.data.prune[n,m+1]==median(farm.data.prune[,m+2],na.rm = FALSE)
    #else next
    #}
  #}

for (n in 1:nrow(farm.data.prune)){
  if (is.na(farm.data.prune[n,2]))
    farm.data.prune[n,2]=max(farm.data.prune[n,-1],na.rm= T) 
  else next
}

#grass p.value=0.0005 K=4
for (m in 1:28){
  for (n in 1:nrow(grass.data)){
    chisq.out.test(grass.data[,m+1])
    x=chisq.out.test(grass.data[,m+1])
    if (x$p.value<0.0005) 
      grass.data[which.min(grass.data[,m+1]),m+1]=NA 
    else break
  }
  for (n in 1:nrow(grass.data)){
    chisq.out.test(grass.data[,m+1],opposite = TRUE)
    x=chisq.out.test(grass.data[,m+1],opposite = TRUE)
    if (x$p.value<0.0005) 
      grass.data[which.max(grass.data[,m+1]),m+1]=NA
    else break
  }
}
number.outliers.grass=sum(is.na(grass.data)) #the number of outliers in grass
#test if there is any outlier class
for (i in 1:nrow(grass.data)){
  if (sum(is.na(grass.data[i,]))>3)
    grass.data[i,1]=NA
  else next
}
number.outliers.class.grass=sum(is.na(grass.data[,1])) # the number of outliers of class in grass
#delete the wrong class
index.outlier.grass=which(is.na(grass.data$class))
grass.data.prune=grass.data[-index.outlier.grass,] #the pruned data
grass.prune.left=sum(is.na(grass.data.prune))
#convert the outlier to the normal values
#converted by mean`
for (m in 1:27){
  for (n in 1:nrow(grass.data.prune)){
    if (is.na(grass.data.prune[n,m+2]))
      grass.data.prune[n,m+2]=mean(grass.data.prune[,m+2],na.rm= T)
    else next
  }
} 
#converted by median
#for (m in 1:28){
  #for (n in 1:nrow(grass.data.prune)){
    #if (is.na(grass.data.prune[n,m+1]==TRUE)
      #grass.data.prune[n,m+1]==median(grass.data.prune[,m+1],na.rm = FALSE)
    #else next
  #}
#}

for (n in 1:nrow(grass.data.prune)){
  if (is.na(grass.data.prune[n,2]))
    grass.data.prune[n,2]=max(grass.data.prune[n,-1],na.rm= T) 
  else next
}

#impervious p.value=0.000001 K=6
for (m in 1:28){
  for (n in 1:nrow(impervious.data)){
    chisq.out.test(impervious.data[,m+1])
    x=chisq.out.test(impervious.data[,m+1])
    if (x$p.value<0.000001) 
      impervious.data[which.min(impervious.data[,m+1]),m+1]=NA 
    else break
  }
  for (n in 1:nrow(impervious.data)){
    chisq.out.test(impervious.data[,m+1],opposite = TRUE)
    x=chisq.out.test(impervious.data[,m+1],opposite = TRUE)
    if (x$p.value<0.000001) 
      impervious.data[which.max(impervious.data[,m+1]),m+1]=NA
    else break
  }
}
number.outliers.impervious=sum(is.na(impervious.data)) #the number of outliers in impervious
#test if there is any outlier class
for (i in 1:nrow(impervious.data)){
  if (sum(is.na(impervious.data[i,]))>5)
    impervious.data[i,1]=NA
  else next
}
number.outliers.class.impervious=sum(is.na(impervious.data[,1])) # the number of outliers of class in impervious
#delete the wrong class
index.outlier.impervious=which(is.na(impervious.data$class))
impervious.data.prune=impervious.data[-index.outlier.impervious,] #the pruned data

impervious.prune.left=sum(is.na(impervious.data.prune))
#convert the outlier to the normal values
#converted by mean`
for (m in 1:27){
  for (n in 1:nrow(impervious.data.prune)){
    if (is.na(impervious.data.prune[n,m+2]))
      impervious.data.prune[n,m+2]=mean(impervious.data.prune[,m+2],na.rm= T)
    else next
  }
} 
#converted by median
#for (m in 1:28){
  #for (n in 1:nrow(impervious.data.prune)){
    #if (is.na(impervious.data.prune[n,m+1]==TRUE)
      #impervious.data.prune[n,m+1]==median(impervious.data.prune[,m+1],na.rm = FALSE)
    #else next
  #}
#}

for (n in 1:nrow(impervious.data.prune)){
  if (is.na(impervious.data[n,2]))
    impervious.data.prune[n,2]=max(impervious.data.prune[n,-1],na.rm= T) 
  else next
}

#orchard p.value=0.005 K=4
for (m in 1:28){
  for (n in 1:nrow(orchard.data)){
    chisq.out.test(orchard.data[,m+1])
    x=chisq.out.test(orchard.data[,m+1])
    if (x$p.value<0.005) 
      orchard.data[which.min(orchard.data[,m+1]),m+1]=NA 
    else break
  }
  for (n in 1:nrow(orchard.data)){
    chisq.out.test(orchard.data[,m+1],opposite = TRUE)
    x=chisq.out.test(orchard.data[,m+1],opposite = TRUE)
    if (x$p.value<0.005) 
      orchard.data[which.max(orchard.data[,m+1]),m+1]=NA
    else break
  }
}
number.outliers.orchard=sum(is.na(orchard.data)) #the number of outliers in orchard
#test if there is any outlier class
for (i in 1:nrow(orchard.data)){
  if (sum(is.na(orchard.data[i,]))>3)
    orchard.data[i,1]=NA
  else next
}
number.outliers.class.orchard=sum(is.na(orchard.data[,1])) # the number of outliers of class in orchard
#delete the wrong class
index.outlier.orchard=which(is.na(orchard.data$class))
orchard.data.prune=orchard.data[-index.outlier.orchard,] #the pruned data
orchard.prune.left=sum(is.na(orchard.data.prune))
#convert the outlier to the normal values
#converted by mean`
for (m in 1:27){
  for (n in 1:nrow(orchard.data.prune)){
    if (is.na(orchard.data.prune[n,m+2]))
      orchard.data.prune[n,m+2]=mean(orchard.data.prune[,m+2],na.rm= T)
    else next
  }
} 
#converted by median

#for (m in 1:28){
  #for (n in 1:nrow(orchard.data.prune)){
    #if (is.na(orchard.data.prune[n,m+1]==TRUE)
      #orchard.data.prune[n,m+1]==median(orchard.data.prune[,m+1],na.rm = FALSE)
    #else next
  #}
#}

for (n in 1:nrow(orchard.data.prune)){
  if (is.na(orchard.data.prune[n,2]))
    orchard.data.prune[n,2]=max(orchard.data.prune[n,-1],na.rm= T) 
  else next
}

#water p.value=0.00001 K=5
for (m in 1:28){
  for (n in 1:nrow(water.data)){
    chisq.out.test(water.data[,m+1])
    x=chisq.out.test(water.data[,m+1])
    if (x$p.value<0.00001) 
      water.data[which.min(water.data[,m+1]),m+1]=NA 
    else break
  }
  for (n in 1:nrow(water.data)){
    chisq.out.test(water.data[,m+1],opposite = TRUE)
    x=chisq.out.test(water.data[,m+1],opposite = TRUE)
    if (x$p.value<0.00001) 
      water.data[which.max(water.data[,m+1]),m+1]=NA
    else break
  }
}
number.outliers.water=sum(is.na(water.data)) #the number of outliers in water
#test if there is any outlier class
for (i in 1:nrow(water.data)){
  if (sum(is.na(water.data[i,]))>4)
    water.data[i,1]=NA
  else next
}
number.outliers.class.water=sum(is.na(water.data[,1])) # the number of outliers of class in water
#delete the wrong class
index.outlier.water=which(is.na(water.data$class))
water.data.prune=water.data[-index.outlier.water,] #the pruned data
water.prune.left=sum(is.na(water.data.prune))
#convert the outlier to the normal values
#converted by mean`
for (m in 1:27){
  for (n in 1:nrow(water.data.prune)){
    if (is.na(water.data.prune[n,m+2]))
      water.data.prune[n,m+2]=mean(water.data.prune[,m+2],na.rm= T)
    else next
  }
} 
#converted by median

#for (m in 1:28){
  #for (n in 1:nrow(water.data.prune)){
    #if (is.na(water.data.prune[n,m+1]==TRUE)
      #water.data.prune[n,m+1]==median(water.data.prune[,m+1],na.rm = FALSE)
    #else next
    #}
  #}

for (n in 1:nrow(water.data.prune)){
  if (is.na(water.data.prune[n,2]))
    water.data.prune[n,2]=max(water.data.prune[n,-1],na.rm= T)
  else next
}

data.denoising=rbind(farm.data.prune,forest.data.prune,grass.data.prune,impervious.data.prune,orchard.data.prune,water.data.prune)



