
install.packages("dplyr")
install.packages('stringr')
install.packages("irr")
install.packages("psych")
library("psych")
library("irr")
library("stringr")
library("dplyr")


setwd("C:/0_RData/1")
dir()

# for Cohen's Kappa: create a column of annotation results for each rater
rater1 <- c()
rater2 <- c()


##calculate agreement (1 or 0) for each element in all essays  
Nonannotated_all <- c()
Final_Claim_all <- c()
Primary_Claim_all <- c()
Data_all <- c()
Counterclaim_all <- c()
Rebuttal_all <- c()
Concluding_Summary_all <- c()

# to caculate overlap percentage 
AreaCount <- 0
OverlapCount <- 0


Allfiles <- list.files (path = "C:/0_RData/1", pattern = "\\.csv", full.names = TRUE,recursive = FALSE)
filesN <- length(Allfiles)
  for (Num in 1:filesN){

  afile <- read.csv(Allfiles[Num], header = TRUE)

  ##calculate agreement (1 or 0) for each element in each essay  
  Nonannotated_essay <- c()
  Final_Claim_essay <- c()
  Primary_Claim_essay <- c()
  Data_essay <- c()
  Counterclaim_essay <- c()
  Rebuttal_essay <- c()
  Concluding_Summary_essay <- c()
  
  # segment the file according to paragraphs
  paralevel <- levels(factor(afile$discourse_part))

  paraN <- length(paralevel)
  for (p in 1:paraN){
    
    afile_segment <- filter(afile, discourse_part == paralevel[p])

    # Step 1: Group the annotations according to the argument labels for both annotators (A and B) for each essay segment. 
    
    # analyze annotations for rater1
    rater1_annotation_element <- c()
    rater1_annotation_start <- c()
    rater1_annotation_end <- c()
    
    rater1_results <- factor(afile_segment$jialiwang_discourse_type_boundary)
    rater1_items <- levels(rater1_results)
    
    
    m <- length(rater1_items) # get the number of elements annotated
    if (m > 0){
      for (x in 1:m){
        element_raw <- str_replace_all(rater1_items[x], "[(]", "")
        element_cleaned <- str_replace_all(element_raw, "[)]", "")
        annotation_element <- str_replace_all(element_cleaned, "([A-z]+)(\\s+)(\\d+)([,])(\\s+)(\\d+)", '\\1')
        annotation_start <- as.integer(str_replace_all(element_cleaned, "([A-z]+)(\\s+)(\\d+)([,])(\\s+)(\\d+)", '\\3'))
        annotation_end <- as.integer(str_replace_all(element_cleaned, "([A-z]+)(\\s+)(\\d+)([,])(\\s+)(\\d+)", '\\6'))
        rater1_annotation_element <- append(rater1_annotation_element, annotation_element)
        rater1_annotation_start <- append(rater1_annotation_start, annotation_start)
        rater1_annotation_end <- append(rater1_annotation_end, annotation_end)
      }
    }
    
    ## combine the adjacent two elements that have the same label. This will make the results more accurate. 
    n_element <- length(rater1_annotation_element)
    
    v <- 0 
    if (n_element > 1){
      for (i in 1:n_element-1){
        if (i - v > 0){
          if (rater1_annotation_element[i-v] == rater1_annotation_element[i + 1 -v] & rater1_annotation_start[i + 1 - v] - rater1_annotation_end[i - v] <= 4 ){ # adjacent
            rater1_annotation_element <- rater1_annotation_element[-(i-v)]
            rater1_annotation_start <- rater1_annotation_start[-(i+1-v)]
            rater1_annotation_end <- rater1_annotation_end[-(i-v)]
            v = v + 1
          }
        }
      }
    }
    
    rater1_annotation_start <- sort(rater1_annotation_start)
    rater1_annotation_end <- sort(rater1_annotation_end)
    
    # analyze annotations for rater2
    rater2_annotation_element <- c()
    rater2_annotation_start <- c()
    rater2_annotation_end <- c()
    
    rater2_results <- factor(afile_segment$mthomas145_discourse_type_boundary)
    rater2_items <- levels(rater2_results)
    
    n <- length(rater2_items) # get the number of elements annotated
    if (n > 0){
      for (y in 1:n){
        element_raw <- str_replace_all(rater2_items[y], "[(]", "")
        element_cleaned <- str_replace_all(element_raw, "[)]", "")
        annotation_element <- str_replace_all(element_cleaned, "([A-z]+)(\\s+)(\\d+)([,])(\\s+)(\\d+)", '\\1')
        annotation_start <- as.integer(str_replace_all(element_cleaned, "([A-z]+)(\\s+)(\\d+)([,])(\\s+)(\\d+)", '\\3'))
        annotation_end <- as.integer(str_replace_all(element_cleaned, "([A-z]+)(\\s+)(\\d+)([,])(\\s+)(\\d+)", '\\6'))
        rater2_annotation_element <- append(rater2_annotation_element, annotation_element)
        rater2_annotation_start <- append(rater2_annotation_start, annotation_start)
        rater2_annotation_end <- append(rater2_annotation_end, annotation_end)
      }
    }
    
    ## combine the adjacent two elements that have the same label. This will make the results more accurate. 
    n_element <- length(rater2_annotation_element)
    
    v <- 0 
    if (n_element > 1){
      for (i in 1:n_element-1){
        if (i - v > 0){
          if (rater2_annotation_element[i-v] == rater2_annotation_element[i + 1 -v] & rater2_annotation_start[i + 1 - v] - rater2_annotation_end[i - v] <= 4 ){ # adjacent
            rater2_annotation_element <- rater2_annotation_element[-(i-v)]
            rater2_annotation_start <- rater2_annotation_start[-(i+1-v)]
            rater2_annotation_end <- rater2_annotation_end[-(i-v)]
            v = v + 1
          }
        }
      }
    }

    rater2_annotation_start <- sort(rater2_annotation_start)
    rater2_annotation_end <- sort(rater2_annotation_end)

    
    
    ####calculate all the text areas annotated by Rater1 and Rater2 combined. 
    combined_start <- rater2_annotation_start
    combined_end <- rater2_annotation_end
    rater1_N <- length(rater1_annotation_element)
    rater2_N <- length(rater2_annotation_element)

    for (p in 1: rater1_N){
      for (q in 1: rater2_N){
        if(rater1_annotation_start[p] < rater2_annotation_start[q] &  rater1_annotation_end[p] <=  rater2_annotation_end[q] & rater1_annotation_end[p] > rater2_annotation_start[q]){
          combined_start[q] <- rater1_annotation_start[p]
          break
        }else if(rater1_annotation_start[p] >= rater2_annotation_start[q] & rater1_annotation_end[p] >  rater2_annotation_end[q] &  rater1_annotation_start[p] < rater2_annotation_end[q]) {
          combined_end[q] <- rater1_annotation_end[p]
          break
        }else if(q < rater2_N & rater1_annotation_start[p] >= rater2_annotation_end[q] & rater1_annotation_end[p] <= rater2_annotation_start[q+1]){
          combined_start <- append(combined_start, rater1_annotation_start[p])
          combined_end <- append(combined_end, rater1_annotation_end[p])
          break
        }else if(q == rater2_N & rater1_annotation_start[p] >= rater2_annotation_end[q] & rater1_annotation_end[p] > rater2_annotation_end[q]){
          combined_start <- append(combined_start, rater1_annotation_start[p])
          combined_end <- append(combined_end, rater1_annotation_end[p])
          break
        }else{
          combined_start <- combined_start
          combined_end <- combined_end
        }
        
      }

    }

    # make adjustments to remove repeatedly counted areas 
    combinedN <- length(combined_start)

    if(combinedN > 1){
      num <- combinedN - 1
      for (h in 1: num){
        if(as.integer(combined_end[h]) - as.integer(combined_start[h+1]) > 0){
          combined_end[h] <- combined_start[h+1]
        }
      }
    }
   

    # calculate combined annotated areas in this paragraph
    
    for (e in 1: combinedN){
      AreaCount <- AreaCount + (combined_end[e] - combined_start[e])
    }
 
    
    ##calculate agreement (1 or 0) for each element  
    Nonannotated <- c()
    Final_Claim <- c()
    Primary_Claim <- c()
    Data <- c()
    Counterclaim <- c()
    Rebuttal <- c()
    Concluding_Summary <- c()
    
    element <- c("Nonannotated", "Final_Claim", "Primary_Claim", "Data", "Counterclaim", "Rebuttal","Concluding_Summary")
    
    #Nonannotated 
    m <- str_which(rater1_annotation_element, element[1])
    n <- str_which(rater2_annotation_element, element[1])
    if (element[1] %in% rater1_annotation_element & element[1] %in% rater2_annotation_element){  # the element is contained in both annotators' annotations
      for (x in m){
        for (y in n){
          if ((rater1_annotation_start[x]-rater2_annotation_start[y] <= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] >= 0)){ # rater2's element area is completely inlcuded in rater1's element area
            Nonannotated <- append(Nonannotated, 1)
            rater1 <- append(rater1, 'Nonannotated')
            rater2 <- append(rater2, 'Nonannotated')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y]- rater2_annotation_start[y])  
          } else if ((rater1_annotation_start[x]-rater2_annotation_start[y] >= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] <= 0)){ # rater1's element area is completely inlcuded in rater2's element area
            Nonannotated <- append(Nonannotated, 1)
            rater1 <- append(rater1, 'Nonannotated')
            rater2 <- append(rater2, 'Nonannotated')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x]- rater1_annotation_start[x])  
          }else if((rater1_annotation_start[x]-rater2_annotation_start[y] == 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] == 0)){
            Nonannotated <- Nonannotated[-1] # remove duplicated calculations 
            rater1 <- rater1[-length(rater1)]
            rater2 <- rater2[-length(rater2)]
            OverlapCount <- OverlapCount - (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          } else if ((rater2_annotation_end[y] > rater1_annotation_end[x]) &
                     (rater1_annotation_end[x] > rater2_annotation_start[y]) &
                     (rater2_annotation_start[y] > rater1_annotation_start[x]) & 
                     ((rater1_annotation_end[x] - rater2_annotation_start[y]) / (rater2_annotation_end[y] - rater1_annotation_start[x]) >= 0.5)){ # overlap >= 66%
            Nonannotated <- append(Nonannotated, 1)
            rater1 <- append(rater1, 'Nonannotated')
            rater2 <- append(rater2, 'Nonannotated')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x] - rater2_annotation_start[y])
            
            
          } else if((rater1_annotation_end[x] > rater2_annotation_end[y]) &
                    (rater2_annotation_end[y] > rater1_annotation_start[x]) &
                    (rater1_annotation_start[x] > rater2_annotation_start[y]) & 
                    ((rater2_annotation_end[y] - rater1_annotation_start[x])/ (rater1_annotation_end[x] - rater2_annotation_start[y]) >= 0.5)){ # overlap >= 66%
            Nonannotated <- append(Nonannotated, 1)
            rater1 <- append(rater1, 'Nonannotated')
            rater2 <- append(rater2, 'Nonannotated')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y] - rater1_annotation_start[x])
          } else {
            Nonannotated <- Nonannotated
            rater1 <- rater1
            rater2 <- rater2
          }  
        }
      }
    }else{
      Nonannotated <- c()
      rater1 <- rater1
      rater2 <- rater2
    }
    

    
    #Final_Claim 
    m <- str_which(rater1_annotation_element, element[2])
    n <- str_which(rater2_annotation_element, element[2])
    if (element[2] %in% rater1_annotation_element & element[2] %in% rater2_annotation_element){
      for (x in m){
        for (y in n){
          if ((rater1_annotation_start[x]-rater2_annotation_start[y] <= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] >= 0)){ # rater2's element area is completely inlcuded in rater1's element area
            Final_Claim <- append(Final_Claim, 1)
            rater1 <- append(rater1, 'Final_Claim')
            rater2 <- append(rater2, 'Final_Claim')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y]- rater2_annotation_start[y])  
          } else if ((rater1_annotation_start[x]-rater2_annotation_start[y] >= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] <= 0)){ # rater1's element area is completely inlcuded in rater2's element area
            Final_Claim <- append(Final_Claim, 1)
            rater1 <- append(rater1, 'Final_Claim')
            rater2 <- append(rater2, 'Final_Claim')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          }else if((rater1_annotation_start[x]-rater2_annotation_start[y] == 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] == 0)){
            Final_Claim <- Final_Claim[-1] # remove duplicated calculations 
            rater1 <- rater1[-length(rater1)]
            rater2 <- rater2[-length(rater2)]
            OverlapCount <- OverlapCount - (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          } else if ((rater2_annotation_end[y] > rater1_annotation_end[x]) &
                     (rater1_annotation_end[x] > rater2_annotation_start[y]) &
                     (rater2_annotation_start[y] > rater1_annotation_start[x]) & 
                     ((rater1_annotation_end[x] - rater2_annotation_start[y]) / (rater2_annotation_end[y] - rater1_annotation_start[x]) >= 0.5)){ # overlap >= 66%
            Final_Claim <- append(Final_Claim, 1)
            rater1 <- append(rater1, 'Final_Claim')
            rater2 <- append(rater2, 'Final_Claim')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x] - rater2_annotation_start[y])
          } else if((rater1_annotation_end[x] > rater2_annotation_end[y]) &
                    (rater2_annotation_end[y] > rater1_annotation_start[x]) &
                    (rater1_annotation_start[x] > rater2_annotation_start[y]) & 
                    ((rater2_annotation_end[y] - rater1_annotation_start[x])/ (rater1_annotation_end[x] - rater2_annotation_start[y]) >= 0.5)){ # overlap >= 66%
            Final_Claim <- append(Final_Claim, 1)
            rater1 <- append(rater1, 'Final_Claim')
            rater2 <- append(rater2, 'Final_Claim')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y] - rater1_annotation_start[x])
          } else {
            Final_Claim <- Final_Claim
            rater1 <- rater1
            rater2 <- rater2
          }  
        }
      }
     
      
    }else{
      Final_Claim <- c()
      rater1 <- rater1
      rater2 <- rater2
    }
    
    
    #Primary_Claim
    
    m <- str_which(rater1_annotation_element, element[3])
    n <- str_which(rater2_annotation_element, element[3])
    if (element[3] %in% rater1_annotation_element & element[3] %in% rater2_annotation_element){
      for (x in m){
        for (y in n){
          if ((rater1_annotation_start[x]-rater2_annotation_start[y] <= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] >= 0)){ # rater2's element area is completely inlcuded in rater1's element area
            Primary_Claim <- append(Primary_Claim, 1)
            rater1 <- append(rater1, 'Primary_Claim')
            rater2 <- append(rater2, 'Primary_Claim')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y]- rater2_annotation_start[y])  
          } else if ((rater1_annotation_start[x]-rater2_annotation_start[y] >= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] <= 0)){ # rater1's element area is completely inlcuded in rater2's element area
            Primary_Claim <- append(Primary_Claim, 1)
            rater1 <- append(rater1, 'Primary_Claim')
            rater2 <- append(rater2, 'Primary_Claim')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          }else if((rater1_annotation_start[x]-rater2_annotation_start[y] == 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] == 0)){
            Primary_Claim <- Primary_Claim[-1] # remove duplicated calculations 
            rater1 <- rater1[-length(rater1)]
            rater2 <- rater2[-length(rater2)]
            OverlapCount <- OverlapCount - (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          } else if ((rater2_annotation_end[y] > rater1_annotation_end[x]) &
                     (rater1_annotation_end[x] > rater2_annotation_start[y]) &
                     (rater2_annotation_start[y] > rater1_annotation_start[x]) & 
                     ((rater1_annotation_end[x] - rater2_annotation_start[y]) / (rater2_annotation_end[y] - rater1_annotation_start[x]) >= 0.5)){ # overlap >= 66%
            Primary_Claim <- append(Primary_Claim, 1)
            rater1 <- append(rater1, 'Primary_Claim')
            rater2 <- append(rater2, 'Primary_Claim')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x] - rater2_annotation_start[y])
          } else if((rater1_annotation_end[x] > rater2_annotation_end[y]) &
                    (rater2_annotation_end[y] > rater1_annotation_start[x]) &
                    (rater1_annotation_start[x] > rater2_annotation_start[y]) & 
                    ((rater2_annotation_end[y] - rater1_annotation_start[x])/ (rater1_annotation_end[x] - rater2_annotation_start[y]) >= 0.5)){ # overlap >= 66%
            Primary_Claim <- append(Primary_Claim, 1)
            rater1 <- append(rater1, 'Primary_Claim')
            rater2 <- append(rater2, 'Primary_Claim')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y] - rater1_annotation_start[x])
          } else {
            Primary_Claim <- Primary_Claim
            rater1 <- rater1
            rater2 <- rater2
          }  
        }
      }
     
    }else{
      Primary_Claim <- c()
      rater1 <- rater1
      rater2 <- rater2
    }
    
    
    #Data
    m <- str_which(rater1_annotation_element, element[4])
    n <- str_which(rater2_annotation_element, element[4])
    if (element[4] %in% rater1_annotation_element & element[4] %in% rater2_annotation_element){
      for (x in m){
        for (y in n){
          if ((rater1_annotation_start[x]-rater2_annotation_start[y] <= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] >= 0)){ # rater2's element area is completely inlcuded in rater1's element area
            Data <- append(Data, 1)
            rater1 <- append(rater1, 'Data')
            rater2 <- append(rater2, 'Data')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y]- rater2_annotation_start[y])  
          } else if ((rater1_annotation_start[x]-rater2_annotation_start[y] >= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] <= 0)){ # rater1's element area is completely inlcuded in rater2's element area
            Data <- append(Data, 1)
            rater1 <- append(rater1, 'Data')
            rater2 <- append(rater2, 'Data')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          }else if((rater1_annotation_start[x]-rater2_annotation_start[y] == 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] == 0)){
            Data <- Data[-1] # remove duplicated calculations 
            rater1 <- rater1[-length(rater1)]
            rater2 <- rater2[-length(rater2)]
            OverlapCount <- OverlapCount - (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          } else if ((rater2_annotation_end[y] > rater1_annotation_end[x]) &
                     (rater1_annotation_end[x] > rater2_annotation_start[y]) &
                     (rater2_annotation_start[y] > rater1_annotation_start[x]) & 
                     ((rater1_annotation_end[x] - rater2_annotation_start[y]) / (rater2_annotation_end[y] - rater1_annotation_start[x]) >= 0.5)){ # overlap >= 66%
            Data <- append(Data, 1)
            rater1 <- append(rater1, 'Data')
            rater2 <- append(rater2, 'Data')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x] - rater2_annotation_start[y])
          } else if((rater1_annotation_end[x] > rater2_annotation_end[y]) &
                    (rater2_annotation_end[y] > rater1_annotation_start[x]) &
                    (rater1_annotation_start[x] > rater2_annotation_start[y]) & 
                    ((rater2_annotation_end[y] - rater1_annotation_start[x])/ (rater1_annotation_end[x] - rater2_annotation_start[y]) >= 0.5)){ # overlap >= 66%
            Data <- append(Data, 1)
            rater1 <- append(rater1, 'Data')
            rater2 <- append(rater2, 'Data')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y] - rater1_annotation_start[x])
          } else {
            Data <- Data
            rater1 <- rater1
            rater2 <- rater2
          }  
        }
      }
      
    }else{
      Data <- c()
      rater1 <- rater1
      rater2 <- rater2
    }
    
    
    #Counterclaim
    m <- str_which(rater1_annotation_element, element[5])
    n <- str_which(rater2_annotation_element, element[5])
    if (element[5] %in% rater1_annotation_element & element[5] %in% rater2_annotation_element){
      for (x in m){
        for (y in n){
          if ((rater1_annotation_start[x]-rater2_annotation_start[y] <= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] >= 0)){ # rater2's element area is completely inlcuded in rater1's element area
            Counterclaim <- append(Counterclaim, 1)
            rater1 <- append(rater1, 'Counterclaim')
            rater2 <- append(rater2, 'Counterclaim')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y]- rater2_annotation_start[y])  
          } else if ((rater1_annotation_start[x]-rater2_annotation_start[y] >= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] <= 0)){ # rater1's element area is completely inlcuded in rater2's element area
            Counterclaim <- append(Counterclaim, 1)
            rater1 <- append(rater1, 'Counterclaim')
            rater2 <- append(rater2, 'Counterclaim')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          }else if((rater1_annotation_start[x]-rater2_annotation_start[y] == 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] == 0)){
            Counterclaim <- Counterclaim[-1] # remove duplicated calculations 
            rater1 <- rater1[-length(rater1)]
            rater2 <- rater2[-length(rater2)]
            OverlapCount <- OverlapCount - (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          } else if ((rater2_annotation_end[y] > rater1_annotation_end[x]) &
                     (rater1_annotation_end[x] > rater2_annotation_start[y]) &
                     (rater2_annotation_start[y] > rater1_annotation_start[x]) & 
                     ((rater1_annotation_end[x] - rater2_annotation_start[y]) / (rater2_annotation_end[y] - rater1_annotation_start[x]) >= 0.5)){ # overlap >= 66%
            Counterclaim <- append(Counterclaim, 1)
            rater1 <- append(rater1, 'Counterclaim')
            rater2 <- append(rater2, 'Counterclaim')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x] - rater2_annotation_start[y])
          } else if((rater1_annotation_end[x] > rater2_annotation_end[y]) &
                    (rater2_annotation_end[y] > rater1_annotation_start[x]) &
                    (rater1_annotation_start[x] > rater2_annotation_start[y]) & 
                    ((rater2_annotation_end[y] - rater1_annotation_start[x])/ (rater1_annotation_end[x] - rater2_annotation_start[y]) >= 0.5)){ # overlap >= 66%
            Counterclaim <- append(Counterclaim, 1)
            rater1 <- append(rater1, 'Counterclaim')
            rater2 <- append(rater2, 'Counterclaim')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y] - rater1_annotation_start[x])
          } else {
            Counterclaim <- Counterclaim
            rater1 <- rater1
            rater2 <- rater2
          }  
        }
      }
      
    }else{
      Counterclaim <- c()
      rater1 <- rater1
      rater2 <- rater2
    }
    
    
    # Rebuttal 
    m <- str_which(rater1_annotation_element, element[6])
    n <- str_which(rater2_annotation_element, element[6])
    if (element[6] %in% rater1_annotation_element & element[6] %in% rater2_annotation_element){
      for (x in m){
        for (y in n){
          if ((rater1_annotation_start[x]-rater2_annotation_start[y] <= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] >= 0)){ # rater2's element area is completely inlcuded in rater1's element area
            Rebuttal <- append(Rebuttal, 1)
            rater1 <- append(rater1, 'Rebuttal')
            rater2 <- append(rater2, 'Rebuttal')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y]- rater2_annotation_start[y])  
          } else if ((rater1_annotation_start[x]-rater2_annotation_start[y] >= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] <= 0)){ # rater1's element area is completely inlcuded in rater2's element area
            Rebuttal <- append(Rebuttal, 1)
            rater1 <- append(rater1, 'Rebuttal')
            rater2 <- append(rater2, 'Rebuttal')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          }else if((rater1_annotation_start[x]-rater2_annotation_start[y] == 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] == 0)){
            Rebuttal <- Rebuttal[-1] # remove duplicated calculations 
            rater1 <- rater1[-length(rater1)]
            rater2 <- rater2[-length(rater2)]
            OverlapCount <- OverlapCount - (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          } else if ((rater2_annotation_end[y] > rater1_annotation_end[x]) &
                     (rater1_annotation_end[x] > rater2_annotation_start[y]) &
                     (rater2_annotation_start[y] > rater1_annotation_start[x]) & 
                     ((rater1_annotation_end[x] - rater2_annotation_start[y]) / (rater2_annotation_end[y] - rater1_annotation_start[x]) >= 0.5)){ # overlap >= 66%
            Rebuttal <- append(Rebuttal, 1)
            rater1 <- append(rater1, 'Rebuttal')
            rater2 <- append(rater2, 'Rebuttal')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x] - rater2_annotation_start[y])
          } else if((rater1_annotation_end[x] > rater2_annotation_end[y]) &
                    (rater2_annotation_end[y] > rater1_annotation_start[x]) &
                    (rater1_annotation_start[x] > rater2_annotation_start[y]) & 
                    ((rater2_annotation_end[y] - rater1_annotation_start[x])/ (rater1_annotation_end[x] - rater2_annotation_start[y]) >= 0.5)){ # overlap >= 66%
            Rebuttal <- append(Rebuttal, 1)
            rater1 <- append(rater1, 'Rebuttal')
            rater2 <- append(rater2, 'Rebuttal')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y] - rater1_annotation_start[x])
            
          } else {
            Rebuttal <- Rebuttal
            rater1 <- rater1
            rater2 <- rater2
            
          }  
        }
      }
    
    }else{
      Rebuttal <- c()
      rater1 <- rater1
      rater2 <- rater2
    }
    
    # Concluding_Summary
    m <- str_which(rater1_annotation_element, element[7])
    n <- str_which(rater2_annotation_element, element[7])
    if (element[7] %in% rater1_annotation_element & element[7] %in% rater2_annotation_element){
      for (x in m){
        for (y in n){
          if ((rater1_annotation_start[x]-rater2_annotation_start[y] <= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] >= 0)){ # rater2's element area is completely inlcuded in rater1's element area
            Concluding_Summary <- append(Concluding_Summary, 1)
            rater1 <- append(rater1, 'Concluding_Summary')
            rater2 <- append(rater2, 'Concluding_Summary')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y]- rater2_annotation_start[y])  
          } else if ((rater1_annotation_start[x]-rater2_annotation_start[y] >= 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] <= 0)){ # rater1's element area is completely inlcuded in rater2's element area
            Concluding_Summary <- append(Concluding_Summary, 1)
            rater1 <- append(rater1, 'Concluding_Summary')
            rater2 <- append(rater2, 'Concluding_Summary')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          }else if((rater1_annotation_start[x]-rater2_annotation_start[y] == 0) & (rater1_annotation_end[x]-rater2_annotation_end[y] == 0)){
            Concluding_Summary <- Concluding_Summary[-1] # remove duplicated calculations 
            rater1 <- rater1[-length(rater1)]
            rater2 <- rater2[-length(rater2)]
            OverlapCount <- OverlapCount - (rater1_annotation_end[x]- rater1_annotation_start[x]) 
          } else if ((rater2_annotation_end[y] > rater1_annotation_end[x]) &
                     (rater1_annotation_end[x] > rater2_annotation_start[y]) &
                     (rater2_annotation_start[y] > rater1_annotation_start[x]) & 
                     ((rater1_annotation_end[x] - rater2_annotation_start[y]) / (rater2_annotation_end[y] - rater1_annotation_start[x]) >= 0.5)){ # overlap >= 50%
            Concluding_Summary <- append(Concluding_Summary, 1)
            rater1 <- append(rater1, 'Concluding_Summary')
            rater2 <- append(rater2, 'Concluding_Summary')
            OverlapCount <- OverlapCount + (rater1_annotation_end[x] - rater2_annotation_start[y])
          } else if((rater1_annotation_end[x] > rater2_annotation_end[y]) &
                    (rater2_annotation_end[y] > rater1_annotation_start[x]) &
                    (rater1_annotation_start[x] > rater2_annotation_start[y]) & 
                    ((rater2_annotation_end[y] - rater1_annotation_start[x])/ (rater1_annotation_end[x] - rater2_annotation_start[y]) >= 0.5)){ # overlap >= 50%
            Concluding_Summary <- append(Concluding_Summary, 1)
            rater1 <- append(rater1, 'Concluding_Summary')
            rater2 <- append(rater2, 'Concluding_Summary')
            OverlapCount <- OverlapCount + (rater2_annotation_end[y] - rater1_annotation_start[x])
          } else {
            Concluding_Summary <- Concluding_Summary
            rater1 <- rater1
            rater2 <- rater2
          }  
        }
      }
      
      
    }else{
      Concluding_Summary <- c()
      rater1 <- rater1
      rater2 <- rater2
    }
    
    
    # calculate the total number of elements and add disagreements for rater1 and rater2 results  
    Element_num <- max(length(rater1_annotation_element), length(rater2_annotation_element))
    Element_num2 <- min(length(rater1_annotation_element), length(rater2_annotation_element))
    
    agreement_num <- length(Nonannotated)+length(Final_Claim)+length(Primary_Claim)+length(Data)+length(Counterclaim)+length(Rebuttal)+length(Concluding_Summary)
    element_gap <- Element_num - agreement_num
    if (element_gap > 0){
      if (agreement_num == 0){
        rater1 <- append(rater1, rep('Primary_Claim', Element_num2))   
        rater2 <- append(rater2, rep('Data', Element_num2))
      }else if(agreement_num > 0 & agreement_num == Element_num2){
        rater1 <- append(rater1, 'Primary_Claim')
        rater2 <- append(rater2, 'Data')
      }else if(agreement_num > 0 & agreement_num < Element_num2){
        rater1 <- append(rater1, rep('Primary_Claim', Element_num2 - agreement_num))   
        rater2 <- append(rater2, rep('Data', Element_num2 - agreement_num))
      }
      
    }
    

    ## add disagreement 0s for each element 
    Nonannotated_total <- max(length(str_which(rater1_annotation_element, element[1])), length(str_which(rater2_annotation_element,element[1])))
    Nonannotated_agreement <- length(Nonannotated)
    Nonannotated_gap <- Nonannotated_total - Nonannotated_agreement
    if (Nonannotated_gap > 0 ){
      Nonannotated <- append(Nonannotated, rep(0, Nonannotated_gap))
    }
    
    Final_Claim_total <- max(length(str_which(rater1_annotation_element, element[2])), length(str_which(rater2_annotation_element,element[2])))
    Final_Claim_agreement <- length(Final_Claim)
    Final_Claim_gap <- Final_Claim_total - Final_Claim_agreement
    if (Final_Claim_gap > 0 ){
      Final_Claim <- append(Final_Claim, rep(0, Final_Claim_gap))
    }
    
    Primary_Claim_total <- max(length(str_which(rater1_annotation_element, element[3])), length(str_which(rater2_annotation_element,element[3])))
    Primary_Claim_agreement <- length(Primary_Claim)
    Primary_Claim_gap <- Primary_Claim_total - Primary_Claim_agreement
    if (Primary_Claim_gap > 0 ){
      Primary_Claim <- append(Primary_Claim, rep(0, Primary_Claim_gap))
    }
    
    Data_total <- max(length(str_which(rater1_annotation_element, element[4])), length(str_which(rater2_annotation_element,element[4])))
    Data_agreement <- length(Data)
    Data_gap <- Data_total - Data_agreement
    if (Data_gap > 0 ){
      Data <- append(Data, rep(0, Data_gap))
    }
    
  
    Counterclaim_total <- max(length(str_which(rater1_annotation_element, element[5])), length(str_which(rater2_annotation_element,element[5])))
    Counterclaim_agreement <- length(Counterclaim)
    Counterclaim_gap <- Counterclaim_total - Counterclaim_agreement
    if (Counterclaim_gap > 0 ){
      Counterclaim <- append(Counterclaim, rep(0, Counterclaim_gap))
    }
    
    Rebuttal_total <- max(length(str_which(rater1_annotation_element, element[6])), length(str_which(rater2_annotation_element,element[6])))
    Rebuttal_agreement <- length(Rebuttal)
    Rebuttal_gap <- Rebuttal_total - Rebuttal_agreement
    if (Rebuttal_gap > 0 ){
      Rebuttal <- append(Rebuttal, rep(0, Rebuttal_gap))
    }
    
    Concluding_Summary_total <- max(length(str_which(rater1_annotation_element, element[7])), length(str_which(rater2_annotation_element,element[7])))
    Concluding_Summary_agreement <- length(Concluding_Summary)
    Concluding_Summary_gap <- Concluding_Summary_total - Concluding_Summary_agreement
    if (Concluding_Summary_gap > 0 ){
      Concluding_Summary <- append(Concluding_Summary, rep(0, Concluding_Summary_gap))
    }
    
  
    ## combine the results for the current essay 
    Nonannotated_essay <- append(Nonannotated_essay, Nonannotated)
    Final_Claim_essay <- append(Final_Claim_essay, Final_Claim)
    Primary_Claim_essay <- append(Primary_Claim_essay, Primary_Claim)
    Data_essay <- append(Data_essay, Data)
    Counterclaim_essay <- append(Counterclaim_essay, Counterclaim)
    Rebuttal_essay <- append(Rebuttal_essay, Rebuttal)
    Concluding_Summary_essay <- append(Concluding_Summary_essay, Concluding_Summary)
    
  }
  
  ##combine the results for all the essays
  
  Nonannotated_all <- append(Nonannotated_all, Nonannotated_essay)
  Final_Claim_all <- append(Final_Claim_all, Final_Claim_essay)
  Primary_Claim_all <- append(Primary_Claim_all, Primary_Claim_essay)
  Data_all <- append(Data_all, Data_essay)
  Counterclaim_all <- append(Counterclaim_all, Counterclaim_essay)
  Rebuttal_all <- append(Rebuttal_all, Rebuttal_essay)
  Concluding_Summary_all <- append(Concluding_Summary_all, Concluding_Summary_essay)
  
  
}

# calculate overlap percentage (Jaccard Index) for the entire data set. 
OverlapCount/AreaCount


## remove half of the zeros because disagreement was calculated twice for each element. 

# split 0s and 1s
Final_Claim_all_0 <- Final_Claim_all[Final_Claim_all == 0]
Final_Claim_all_1 <- Final_Claim_all[Final_Claim_all == 1]
# create the new set of agreement scores with inflated 0s removed 
Num_0 <- as.integer(length(Final_Claim_all_0)/2)
Final_Claim_new <- c(Final_Claim_all_1, Final_Claim_all_0[0:Num_0])
Final_Claim_new

# split 0s and 1s
Primary_Claim_all_0 <- Primary_Claim_all[Primary_Claim_all == 0]
Primary_Claim_all_1 <- Primary_Claim_all[Primary_Claim_all == 1]
# create the new set of agreement scores with inflated 0s removed 
Num_0 <- as.integer(length(Primary_Claim_all_0)/2)
Primary_Claim_new <- c(Primary_Claim_all_1, Primary_Claim_all_0[0:Num_0])
Primary_Claim_new

# split 0s and 1s
Data_all_0 <- Data_all[Data_all == 0]
Data_all_1 <- Data_all[Data_all == 1]
# create the new set of agreement scores with inflated 0s removed 
Num_0 <- as.integer(length(Data_all_0)/2)
Data_new <- c(Data_all_1, Data_all_0[0:Num_0])
Data_new

# split 0s and 1s
Counterclaim_all_0 <- Counterclaim_all[Counterclaim_all == 0]
Counterclaim_all_1 <- Counterclaim_all[Counterclaim_all == 1]
# create the new set of agreement scores with inflated 0s removed 
Num_0 <- as.integer(length(Counterclaim_all_0)/2)
Counterclaim_new <- c(Counterclaim_all_1, Counterclaim_all_0[0:Num_0])
Counterclaim_new

# split 0s and 1s
Rebuttal_all_0 <- Rebuttal_all[Rebuttal_all == 0]
Rebuttal_all_1 <- Rebuttal_all[Rebuttal_all == 1]
# create the new set of agreement scores with inflated 0s removed 
Num_0 <- as.integer(length(Rebuttal_all_0)/2)
Rebuttal_new <- c(Rebuttal_all_1, Rebuttal_all_0[0:Num_0])
Rebuttal_new

# calculate overall IAA using Cohen's Kappa 
IAA <- data.frame(
   Rater1 = rater1,
   Rater2 = rater2
)

kappa2(IAA) # use this one: unweighted 
cohen.kappa(IAA)
write.csv(IAA, 'IAA.csv')


# calculate agreement percentage for each element 
Final_Claim_agreement = round(sum(Final_Claim_new)/length(Final_Claim_new),3)
Final_Claim_agreement

Primary_Claim_agreement = round(sum(Primary_Claim_new)/length(Primary_Claim_new),3)
Primary_Claim_agreement

Data_agreement = round(sum(Data_new)/length(Data_new),3)
Data_agreement



a <- data.frame(
  Prompt = str_replace_all(basename(afile), "(^.*?)(_items_)(.*?)(_\\d+-\\d+-\\d+)(\\.json)", "\\3"),
  Type = str_replace_all(basename(afile), "(^.*?)(_items_)(.*?)(_\\d+-\\d+-\\d+)(\\.json)", "\\1"),
  Lead_agreement = round(sum(Lead_all)/length(Lead_all), 3), 
  Position_agreement = round(sum(Position_all)/length(Position_all),3),
  Claim_agreement = round(sum(Claim_all)/ length(Claim_all),3),
  Evidence_agreement = round(sum(Evidence_all) / length(Evidence_all),3),
  Counterclaim_agreement = round(sum(Counterclaim_all)/length(Counterclaim_all),3),
  Rebuttal_agreement = round(sum(Rebuttal_all) / length(Rebuttal_all),3),
  Concluding_statement_agreement = round(sum(Concluding_statement_all)/length(Concluding_statement_all),3),
  Overall_agreement = round((sum(Lead_all) + sum(Position_all) +sum(Claim_all) + sum(Evidence_all) + sum(Counterclaim_all) + sum(Rebuttal_all) + sum(Concluding_statement_all)) / Overall_num, 3)
)
filename <- str_replace_all(basename(afile), "(.*?_\\d+-\\d+-\\d+)(\\.json)", "\\1.csv")
write.csv(a, filename)





############################################################################

