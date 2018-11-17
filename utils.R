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
    0.115*600,
    0.115*720,
    0.115*580,
    0.115*620,
    0.115*300
  ))
  return (labeled_data)
}

normalizeData <- function(data) {
  # Nomalization
  normalize <- function(x) {return((x - min(x)) / (max(x) - min(x)))}
  nom.train <- lapply(data[,6:11],normalize)
  scaled.data <- data.frame(data[2],nom.train,data[,13:25])
  summary(scaled.data)
  
  return (scaled.data)
}

processNominalVars <- function(data) {
  processed.data <- data
  #convert Label column into factor
  processed.data$Label <- as.factor(processed.data$Label)
  # 3 dummies variable for 4 type of materialsA
  processed.data$A1 <- ifelse(processed.data$MaterialA=="A1",1,0)
  processed.data$A2 <- ifelse(processed.data$MaterialA=="A2",1,0)
  processed.data$A3 <- ifelse(processed.data$MaterialA=="A3",1,0)
  # 7 dummies variable for 8 type of materialsB
  processed.data$B1 <- ifelse(processed.data$MaterialB=="B1",1,0)
  processed.data$B2 <- ifelse(processed.data$MaterialB=="B2",1,0)
  processed.data$B3 <- ifelse(processed.data$MaterialB=="B3",1,0)
  processed.data$B4 <- ifelse(processed.data$MaterialB=="B4",1,0)
  processed.data$B5 <- ifelse(processed.data$MaterialB=="B5",1,0)
  processed.data$B6 <- ifelse(processed.data$MaterialB=="B6",1,0)
  processed.data$B7 <- ifelse(processed.data$MaterialB=="B7",1,0)
  # 1 dummy variable for 2 type of BrandName
  processed.data$BN1 <- ifelse(processed.data$BrandName=="BrandName1",1,0)
  # 2 dummies variable for 3 type of MixProduction
  processed.data$Mix40.60 <- ifelse(processed.data$MixProportion=="40-60",1,0)
  processed.data$Mix50.50 <- ifelse(processed.data$MixProportion=="50-50",1,0)
  
  summary(processed.data)
  summary(processed.data[,6:11])
  
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