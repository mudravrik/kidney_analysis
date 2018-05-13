# Ivan Tsarev, mudravr@gmail.com
# Reshaping script for some access-producted .xlsx tables



library(xlsx)
library(reshape)

access_import_reshape = function(filename){
  
  import_data = read.xlsx(
    file = filename,
    sheetIndex = 1,
    encoding = "UTF-8",
    header = TRUE,
    stringsAsFactors = FALSE
  )
  
  import_data$visit_code = as.numeric(import_data$Код.визита)
  import_data$patient_code = as.integer(import_data$Код.пациента)
  
  import_data = import_data[order(import_data$patient_code, import_data$Дата.визита), ]
  
  import_data$visit_index = NA
  
  
  for (i in 1:max(import_data$patient_code)) {
    import_data$visit_index[which(import_data$patient_code == i)] = c(1:length(import_data$visit_code[which(import_data$patient_code == i)]))
  }
  
  melted = melt(import_data, id = c("patient_code", "visit_index"))
  casted = cast(melted, patient_code ~ variable + visit_index)
  
  return(casted)
}