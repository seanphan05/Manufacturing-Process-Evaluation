
factorMaterialA <- function(data) {
  labeled_data = factor(data, levels = c(
    "75a5f96063fbc3290b07b0e81c3249d0",
    "42a6854ae47630b3a32e84823d147e0b",
    "f5c2479be5048388c45cb2e81edfbd3f",
    "59a06f37e2f0262919e5aab9e083ccbd",
    "b27d68f669d9758e698a35c20fa2bab3"
  ),
  labels = c(
    "A1", "A2", "A3", "A4", "A5"
  ))
  return(labeled_data)
}

factorMaterialB <- function(data) {
  labeled_data = factor(data, levels = c(
    "4610065df728e0bd399446ef5fd3ea74",
    "6fc672934f2f6e2a64898efd18c24111",
    "0e05731278221a3ac6ebaa1d795b6177",
    "fe68fc884ca9961c9e8ec90e7fc5ec9d",
    "e3d6228b4bb426d6d7004613eb04e124",
    "5437f1e2e5cf83c784e5dba00ee42e7a",
    "9e617be991132586229e9964b8bf4463",
    "129d425bfebdb61df18ad3940118751b",
    "dfac0f954eb95d09d014305d0c5dbe3f",
    "2e54a9875f82b6686db826c4ab45cb8a",
    "5a9af4e32acd65b9baed049e037d86d3"
  ),
  labels = c(
    "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11"
  ))
  return (labeled_data)
}

factorBrandName <- function(data) {
  labeled_data = factor(data, levels = c(
    "29f4d775d7fd37f40a72f66f39b7453f",
    "d69b8ab522882db30043159ad9918216"
  ),
  labels = c(
    "BrandName1", "BrandName2"
  ))
  return (labeled_data)
}

factorMaterialSize <- function(data) {
  labeled_data = factor(data, levels = c(
    "0.115*600",
    "0.115*720",
    "0.115*580",
    "0.115*620",
    "0.115*300"
  ),
  labels = c(
    "0.115*600",
    "0.115*720",
    "0.115*580",
    "0.115*620",
    "0.115*300"
  ))
  return (labeled_data)
}

processNominalVars <- function(data) {
  processed.data <- data
  # convert Label column into factor
  if (length(processed.data)==12) {processed.data$Label <- as.factor(processed.data$Label)}
  # 4 dummy variables for 5 type of materialsA
  processed.data$A1 <- ifelse(processed.data$MaterialA=="A1",1,0)
  processed.data$A2 <- ifelse(processed.data$MaterialA=="A2",1,0)
  processed.data$A3 <- ifelse(processed.data$MaterialA=="A3",1,0)
  processed.data$A4 <- ifelse(processed.data$MaterialA=="A4",1,0)
  # 10 dummies variable for 11 type of materialsB
  processed.data$B1 <- ifelse(processed.data$MaterialB=="B1",1,0)
  processed.data$B2 <- ifelse(processed.data$MaterialB=="B2",1,0)
  processed.data$B3 <- ifelse(processed.data$MaterialB=="B3",1,0)
  processed.data$B4 <- ifelse(processed.data$MaterialB=="B4",1,0)
  processed.data$B5 <- ifelse(processed.data$MaterialB=="B5",1,0)
  processed.data$B6 <- ifelse(processed.data$MaterialB=="B6",1,0)
  processed.data$B7 <- ifelse(processed.data$MaterialB=="B7",1,0)
  processed.data$B8 <- ifelse(processed.data$MaterialB=="B8",1,0)
  processed.data$B9 <- ifelse(processed.data$MaterialB=="B9",1,0)
  processed.data$B10 <- ifelse(processed.data$MaterialB=="B10",1,0)
  # 1 dummy variable for 2 type of BrandName
  processed.data$BN1 <- ifelse(processed.data$BrandName=="BrandName1",1,0)
  # 1 dummy variable for 2 type of MixProduction
  processed.data$Mix50.50 <- ifelse(processed.data$MixProportion=="50-50",1,0)
  # 4 dummy variables for 5 type of MaterialSize
  processed.data$s600 <- ifelse(processed.data$MaterialSize=="0.115*600",1,0)
  processed.data$s720 <- ifelse(processed.data$MaterialSize=="0.115*720",1,0)
  processed.data$s580 <- ifelse(processed.data$MaterialSize=="0.115*580",1,0)
  processed.data$s620 <- ifelse(processed.data$MaterialSize=="0.115*620",1,0)
  
  summary(processed.data)
  return (processed.data)
}

ggplotMissingData <- function(x){
  x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey (name = '', labels = c('Present', 'Missing')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = 'Variables on Dataset', y = 'Rows / Observations')
}

imputeMissingValues <- function(data) {
  #install.packages("missForest")
  library(missForest)
  # exclude column ProductNo (product code is not a concerned variable)
  data.imp <- missForest(data[,!"ProductNo", with=FALSE])
  summary(data.imp)
  data.imp$ximp
  new.data <- data.frame(data$ProductNo, data.imp$ximp)
  return (new.data)
}

featureMatch <- function(data)
{ # start featureMatch
  
  # Match MaterialA Feature:
{
  if (length(data)==12) {
    index.A5 <- which(data$MaterialA=="A4")
    data$MaterialA[index.A5[1:144]] <- "A5"}
  else {
    index.A4 <- which(data$MaterialA=="A5")
    data$MaterialA[index.A4[1:32]] <- "A4" }
}

# Match MaterialB Feature:
{
  if (length(data)==12) {
    index.B <- which(data$MaterialB=="B8")
    data$MaterialB[index.B[133:264]] <- "B9"
    data$MaterialB[index.B[265:396]] <- "B10"
    data$MaterialB[index.B[397:528]] <- "B11" }
}
# Match MixProduction Feature:
{  
  if (length(data)==12) {
    data$MixProportion <- as.character(data$MixProportion)
    data$MixProportion[which(data$MixProportion=="41-59")] <- "40-60" 
    data$MixProportion <- as.factor(data$MixProportion) 
    }
  else {
    data$MixProportion <- as.character(data$MixProportion)
    data$MixProportion[which(data$MixProportion=="45-55")] <- "40-60" 
    data$MixProportion <- as.factor(data$MixProportion) }
}
# Match MaterialSize Feature:
{
  if (length(data)==11) {
    index.s620 <- which(data$MaterialSize=="0.115*300")
    data$MaterialSize[index.s620[1:236]] <- "0.115*620"
  }
}
  
  return(data)
} # End featureMatch

normalizeData <- function(data) {
  # Nomalization
  normalize <- function(x) {return((x - min(x)) / (max(x) - min(x)))}
  if (length(data)==32) {
    nom.train <- lapply(data[c(6,8:11)],normalize)
    scaled.data <- data.frame(data[2],nom.train,data[,13:32])
    summary(scaled.data)
    return (scaled.data) }
  else {
    nom.test <- lapply(data[c(5,7:10)],normalize)
    scaled.data <- data.frame(nom.test,data[,12:31])
    summary(scaled.data)
    return (scaled.data) }
}