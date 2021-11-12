#######################################################
# Mark Gosink, Ph.D.
# Global Pathology & Investigative Toxicology
# Pfizer Inc.
# 
# Function: 
#######################################################

# Modify the path for your environment
setwd("~/Projects/review_ml_ibd")

library(data.table)
library(plotly)
library(openxlsx)
library(stringi)
library(htmlwidgets)

# Load the appropriate excel file ####
phvig_data <- as.data.table(read.xlsx("20210817_systematicreview_table_nonotes.xlsx", check.names = TRUE))

# Change the column names to something short ####
setnames(phvig_data, "ML.Method.s.", "Method")
setnames(phvig_data, "Prediction.Classification.Task", "Task")
setnames(phvig_data, "Type.of.Data", "Data.type")

# Remove any row where there is missing data ####
phvig_data <- phvig_data[!is.na(phvig_data$Method)]
phvig_data <- phvig_data[!is.na(phvig_data$Task)]
phvig_data <- phvig_data[!is.na(phvig_data$Data.type)]
phvig_data <- phvig_data[!grepl("no access", Task)]


#################################################################################.
# Because multiple ML methods can be used in a single paper, make duplicate entries with
#   one ML method per entry (row) ####
phvig_data$KEEP <- TRUE
for (row in 1:nrow(phvig_data)) {
  entry <- phvig_data[row,]
  method_list = stri_split_regex(entry$Method, '\\s*,\\s*')
  if (length(method_list[[1]]) > 1) {
    for (mtd in method_list[[1]]) {
      new_entry <- entry
      new_entry$Method <- mtd
      phvig_data[row,]$KEEP <- FALSE
      phvig_data <- rbind(phvig_data, new_entry)
    }
  }
}
phvig_data <- phvig_data[KEEP==TRUE]
phvig_data$Method <- trimws(phvig_data$Method, which = c("both"))
# Roll-up the different ML methods into a common term ####
#   First let's handle a few specific cases which might otherwise be mis-classified
phvig_data[grepl("CatBoost \\(tree based\\)", Method, ignore.case = TRUE), Method:= "Boosting"]
phvig_data[grepl("Gradient boosted trees", Method, ignore.case = TRUE), Method:= "Boosting"]
phvig_data[grepl("Guassian Mixture Model", Method, ignore.case = TRUE), Method:= "Clustering"]
phvig_data[grepl("Nearest Shrunken Centroids", Method, ignore.case = TRUE), Method:= "Clustering"]
phvig_data[grepl("Support Vecotr machine", Method, ignore.case = TRUE), Method:= "Support Vector Machine"]
#   Next we handle the more general cases
phvig_data[grepl("bayes", Method, ignore.case = TRUE), Method:= "Bayes Network"]
phvig_data[grepl("forest", Method, ignore.case = TRUE), Method:= "Random Forest"]
phvig_data[grepl("tree", Method, ignore.case = TRUE), Method:= "Decision Tree"]
phvig_data[grepl("regression", Method, ignore.case = TRUE), Method:= "Regression"]
phvig_data[grepl("boost", Method, ignore.case = TRUE), Method:= "Boosting"]
phvig_data[grepl("cluster", Method, ignore.case = TRUE), Method:= "Clustering"]
phvig_data[grepl("k-nearest", Method, ignore.case = TRUE), Method:= "Clustering"]
phvig_data[grepl("knn", Method, ignore.case = TRUE), Method:= "Clustering"]
phvig_data[grepl("k-nn", Method, ignore.case = TRUE), Method:= "Clustering"]
phvig_data[grepl("vector machine", Method, ignore.case = TRUE), Method:= "Support Vector Machine"]
phvig_data[grepl("linear kernel", Method, ignore.case = TRUE), Method:= "Support Vector Machine"]
phvig_data[grepl("LightGBM", Method, ignore.case = TRUE), Method:= "Boosting"]
phvig_data[grepl("DeepCNN", Method, ignore.case = TRUE), Method:= "Neural Network"]
phvig_data[grepl("Neural Net", Method, ignore.case = TRUE), Method:= "Neural Network"]
phvig_data[grepl("Sparse neural encoder-decoder network", Method, ignore.case = TRUE), Method:= "Neural Network"]
phvig_data[grepl("Elastic net", Method, ignore.case = TRUE), Method:= "Regression"]
phvig_data[grepl("Partial Least Squares Discriminant Analysis", Method, ignore.case = TRUE), Method:= "PLS Analysis"]


All_Methods <- unique(phvig_data[,get("Method")])
for (mtd in All_Methods) {
  paper_cnt <- length(unique(phvig_data[get("Method") == mtd]$Title))
  if (paper_cnt == 1) { phvig_data[get("Method") == mtd, Method:= "Other"] }
}


#################################################################################.
# Because multiple task can be described in a single paper, make duplicate entries with
#   one task per entry (row) ####
for (row in 1:nrow(phvig_data)) {
  entry <- phvig_data[row,]
  task_list = stri_split_regex(entry$Task, '\\s*,\\s*')
  if (length(task_list[[1]]) > 1) {
    for (task in task_list[[1]]) {
      new_entry <- entry
      new_entry$Task <- task
      phvig_data[row,]$KEEP <- FALSE
      phvig_data <- rbind(phvig_data, new_entry)
    }
  }
}
phvig_data <- phvig_data[KEEP==TRUE]
phvig_data$Task <- trimws(phvig_data$Task, which = c("both"))
# Roll-up the different Tasks into a common term ####
phvig_data[grepl("Disease Course", Task, ignore.case = TRUE), Task:= "Disease Course"]
phvig_data[grepl("Disease Severity", Task, ignore.case = TRUE), Task:= "Disease Severity"]
phvig_data[grepl("Disease prediction", Task, ignore.case = TRUE), Task:= "Disease Prediction"]
phvig_data[grepl("Subtype Diagnosis", Task, ignore.case = TRUE), Task:= "Disease Subtypes"]
phvig_data[grepl("Disease subtype", Task, ignore.case = TRUE), Task:= "Disease Subtypes"]
phvig_data[grepl("Subphenotypes of CD", Task, ignore.case = TRUE), Task:= "Disease Subtypes"]
phvig_data[grepl("Predict metabolite abundance from microbiome", Task, ignore.case = TRUE), Task:= "Metabolite Abundance"]

#################################################################################.
# Because data types can be used in a single paper, make duplicate entries with
#   one task per entry (row) ####
for (row in 1:nrow(phvig_data)) {
  entry <- phvig_data[row,]
  type_list = stri_split_regex(entry$Data.type, '\\s*(,|\\+|\\s+and\\s+)\\s*')
  if (length(type_list[[1]]) > 1) {
    for (type in type_list[[1]]) {
      new_entry <- entry
      new_entry$Data.type <- type
      phvig_data[row,]$KEEP <- FALSE
      phvig_data <- rbind(phvig_data, new_entry)
    }
  }
}
phvig_data <- phvig_data[KEEP==TRUE]
phvig_data$Data.type <- trimws(phvig_data$Data.type, which = c("both"))
# Roll-up the different data types into a common term ####
phvig_data[grepl("16s", Data.type, ignore.case = TRUE), Data.type:= "Microbiome"]
phvig_data[grepl("Microbiota", Data.type, ignore.case = TRUE), Data.type:= "Microbiome"]
phvig_data[grepl("Microbial", Data.type, ignore.case = TRUE), Data.type:= "Microbiome"]
phvig_data[grepl("metabolomics", Data.type, ignore.case = TRUE), Data.type:= "Metabolome"]
phvig_data[grepl("Metatranscriptomic data", Data.type, ignore.case = TRUE), Data.type:= "Microbiome"]
phvig_data[grepl("metagenom", Data.type, ignore.case = TRUE), Data.type:= "Microbiome"]
phvig_data[grepl("Microbiome", Data.type, ignore.case = TRUE), Data.type:= "Microbiome"]
phvig_data[grepl("expression", Data.type, ignore.case = TRUE), Data.type:= "Expression"]
phvig_data[grepl("image", Data.type, ignore.case = TRUE), Data.type:= "Image"]
phvig_data[grepl("MRI Data", Data.type, ignore.case = TRUE), Data.type:= "Image"]
phvig_data[grepl("NMR spectra", Data.type, ignore.case = TRUE), Data.type:= "Metabolome"]
phvig_data[grepl("Imaging", Data.type, ignore.case = TRUE), Data.type:= "Image"]
phvig_data[grepl("endoscop", Data.type, ignore.case = TRUE), Data.type:= "Image"]
phvig_data[grepl("Hist data", Data.type, ignore.case = TRUE), Data.type:= "Image"]
phvig_data[grepl("clinical", Data.type, ignore.case = TRUE), Data.type:= "Clinical"]
phvig_data[grepl("clincial", Data.type, ignore.case = TRUE), Data.type:= "Clinical"]
phvig_data[grepl("demographic", Data.type, ignore.case = TRUE), Data.type:= "Demographic"]
phvig_data[grepl("genomic", Data.type, ignore.case = TRUE), Data.type:= "Genetic Data"]
phvig_data[grepl("snp", Data.type, ignore.case = TRUE), Data.type:= "Genetic Data"]
phvig_data[grepl("Exome Sequencing", Data.type, ignore.case = TRUE), Data.type:= "Genetic Data"]
phvig_data[grepl("WGS", Data.type, ignore.case = TRUE), Data.type:= "Genetic Data"]
phvig_data[grepl("genotyping", Data.type, ignore.case = TRUE), Data.type:= "Genetic Data"]
phvig_data[grepl("Exome Data", Data.type, ignore.case = TRUE), Data.type:= "Genetic Data"]
phvig_data[grepl("Genetic Data", Data.type, ignore.case = TRUE), Data.type:= "Genetic Data"]
phvig_data[grepl("GWAS Data", Data.type, ignore.case = TRUE), Data.type:= "Genetic Data"]
phvig_data[grepl("Exome-sequencing data", Data.type, ignore.case = TRUE), Data.type:= "Genetic Data"]
phvig_data[grepl("EHR Data", Data.type, ignore.case = TRUE), Data.type:= "EMR Data"]
phvig_data[grepl("Health Records", Data.type, ignore.case = TRUE), Data.type:= "EMR Data"]
phvig_data[grepl("EMR Databases", Data.type, ignore.case = TRUE), Data.type:= "EMR Data"]
phvig_data[grepl("Electronic Medical Records Data", Data.type, ignore.case = TRUE), Data.type:= "EMR Data"]

Arrangements <- data.table(file_prefix=character(), Ring_Order=list())
# Set the order of the concentric rings from outer to inner
Arrangements <- rbind(Arrangements,
                      data.table(
                        file_prefix = 'MTD',
                        Ring_Order = list(c("Method", "Task", "Data.type"))))
Arrangements <- rbind(Arrangements,
                      data.table(
                        file_prefix = 'MDT',
                        Ring_Order = list(c("Method", "Data.type", "Task"))))
Arrangements <- rbind(Arrangements,
                      data.table(
                        file_prefix = 'DTM',
                        Ring_Order = list(c("Data.type", "Task", "Method"))))
Arrangements <- rbind(Arrangements,
                      data.table(
                        file_prefix = 'DMT',
                        Ring_Order = list(c("Data.type", "Method", "Task"))))
Arrangements <- rbind(Arrangements,
                      data.table(
                        file_prefix = 'TMD',
                        Ring_Order = list(c("Task", "Method", "Data.type"))))
Arrangements <- rbind(Arrangements,
                      data.table(
                        file_prefix = 'TDM',
                        Ring_Order = list(c("Task", "Data.type", "Method"))))
#for(i in 1:1) {
for(i in 1:nrow(Arrangements)) {
  file_prefix <- Arrangements[i]$file_prefix
  Ring_Order <- unlist(Arrangements[i]$Ring_Order)

  Tier_1 <- unique(phvig_data[,get(Ring_Order[1])])
  Tier_2 <- unique(phvig_data[,get(Ring_Order[2])])
  Tier_3 <- unique(phvig_data[,get(Ring_Order[3])])
  # Title should always be the outermost ring
  Tier_4 <- unique(phvig_data$Title)
  
  Sunburst_Data <- data.table( id=character(),
                               label=character(),
                               parent=character(),
                               hovertemplate=character(),
                               texttemplate=character(),
                               values=numeric()
                               )
  curr_id <- 0
  total_paper_cnt <- length(unique(phvig_data$Title))
  hover <- "<B>%{label}</B><br> (%{percentParent:.2%} of '%{parent}' papers)"
  text_temp <- "<B>%{label}</B><br>%{value} papers"
  text_temp2 <- ""
  
  for (t1 in Tier_1) {
    values <- length(unique(phvig_data[get(Ring_Order[1]) == t1]$Title))
    t1_paper_cnt <- values
    t1_paper_pct <- round((t1_paper_cnt/total_paper_cnt)*100, 2)
  cat(values, " papers\tTier ", t1, "\t", t1_paper_cnt, "\t", total_paper_cnt, "\n")
    if (values == 0) { next }
    curr_id <- curr_id + 1
    id <- paste0("T1.", curr_id)
    parent <- "All"
    label <- t1
    hover_val <- paste0("<B>%{label}</B><br> (", t1_paper_pct, "% of '%{parent}' papers)")
    Sunburst_Data <- rbind(Sunburst_Data, 
                              data.table(id = id,
                                         label = label,
                                         parent = parent,
                                         hovertemplate = hover_val,
                                         texttemplate = text_temp,
                                         values = values))
  
    for (t2 in Tier_2) {
      values <- length(unique(phvig_data[get(Ring_Order[1]) == t1 & get(Ring_Order[2]) == t2]$Title))
      t2_paper_cnt <- values
      t2_paper_pct <- round((t2_paper_cnt/t1_paper_cnt)*100, 2)
  #cat("\t", values, " papers\tTier ", t2, "\t", t2_paper_cnt, "\t", t1_paper_cnt, "\t", t2_paper_pct, "\n")
      if (values == 0) { next }
      curr_id <- curr_id + 1
      t2s_parent <- id
      t2_id <- paste0(t2s_parent, "-T2.", curr_id)
      label <- t2
      hover_val <- paste0("<B>%{label}</B><br> (", t2_paper_pct, "% of '%{parent}' papers)")
      Sunburst_Data <- rbind(Sunburst_Data,
                                data.table(id = t2_id,
                                           label = label,
                                           parent = t2s_parent,
                                           hovertemplate = hover_val,
                                           texttemplate = text_temp,
#                                           texttemplate = ifelse(t2_paper_cnt>1, text_temp, text_temp2),
                                           values = values))
  
      for (t3 in Tier_3) {
        values <- length(unique(phvig_data[get(Ring_Order[1]) == t1 & get(Ring_Order[2]) == t2 & get(Ring_Order[3]) == t3]$Title))
        t3_paper_cnt <- values
        t3_paper_pct <- round((t3_paper_cnt/t2_paper_cnt)*100, 2)
        if (values == 0) { next }
        curr_id <- curr_id + 1
        t3s_parent <- t2_id
        t3_id <- paste0(t3s_parent, "-T3.", curr_id)
        label <- t3
        hover_val <- paste0("<B>%{label}</B><br> (", t3_paper_pct, "% of '%{parent}' papers)")
        Sunburst_Data <- rbind(Sunburst_Data,
                                  data.table(id = t3_id,
                                             label = label,
                                             parent = t3s_parent,
                                             hovertemplate = hover_val,
                                             texttemplate = text_temp,
#                                             texttemplate = ifelse(t3_paper_cnt>1, text_temp, text_temp2),
                                             values = values))
  
        for (t4 in Tier_4) {
          values <- length(unique(phvig_data[get(Ring_Order[1]) == t1 & get(Ring_Order[2]) == t2 & get(Ring_Order[3]) == t3 & Title == t4]$Title))
          if (values == 0) { next }
          curr_id <- curr_id + 1
          t4s_parent <- t3_id
          t4_id <- paste0(t4s_parent, "-T4.", curr_id)
          title <- t4
          if (nchar(title) > 20) {
            title <- paste0(substr(title,1,17), '...')
          }
          label <- title
  
          paper_link <- ""
          papers <- unique(phvig_data[get(Ring_Order[1]) == t1 & get(Ring_Order[2]) == t2 & get(Ring_Order[3]) == t3 & Title == t4]$Title)
          url <- unique(phvig_data[get(Ring_Order[1]) == t1 & get(Ring_Order[2]) == t2 & get(Ring_Order[3]) == t3 & Title == t4]$DOI)
          paper_cnt <- 1
          for (paper in papers) {
            if (nchar(paper) > 80) {
              paper <- paste0(substr(paper,1,77), '...')
            }
            paper_link <- paste0(paper_link, paper, "<br>")
            paper_cnt <- paper_cnt + 1
          }
          hover_and_papers <- paper_link
          Sunburst_Data <- rbind(Sunburst_Data,
                                    data.table(id = t4_id,
                                               label = label,
                                               parent = t4s_parent,
                                               hovertemplate = hover_and_papers,
                                               texttemplate = paste0("<B>%{label}</B><br><a href='", url, "'>LINK</a>"),
                                               values = values))
        }
      }
    }
  }
  
  
  fig <- plot_ly(Sunburst_Data,
                    ids = ~id,
                    labels = ~label,
                    name = '',
                    parents = ~parent,
                    values = ~values,
                    type = 'sunburst',
                    maxdepth = 3,
                    insidetextorientation='radial',
                    hovertemplate = ~hovertemplate,
                    texttemplate = ~texttemplate, textfont=list(size=20)
      )
  fig <- config(fig, displaylogo = FALSE)
  fig <- layout(fig, title = "IBD ML Articles")
  
  print(fig)
  
  # save the html file
  filename <- paste0(file_prefix, '.html')
  saveWidget(fig, file = filename)
}