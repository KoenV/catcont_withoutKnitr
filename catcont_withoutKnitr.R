############################################################################################################
# Function catcon with Knitr Lay-out: make summary tables of categorical and continuous data with optional between group tests
# Analyses: koen.vanbrabant@kuleuven.be
# date: 14/03/2017
############################################################################################################
# TODO: when which.group has more than two levels effect size (AUC or proportion) needs to be replaced with a p-value.

# required packages ---------------------------------------------------------
packages <- c("Hmisc", "pROC")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# catcon function ---------------------------------------------------------
catcont_noKnitr = function(list_of_variables=c(),data=c(),group=c(),which.group=c(),formal.test=c(),digits=2,caption='Summary Table'){
  full_data = data # keep an untouched part of the data to re-use in the NA subsetting
  # format p-values function (run in function environment)
  format_pval <- function(x){
    if (x < .001) return(paste('<', '.001'))
    else round(x, 3)   # 3 = no. of digits to round p value to if .001 < p < .250.
  }
  
  ## list to save info on all variables (length is number of considerd variables)
  seperate_info = vector('list',length(list_of_variables))
  if (group==FALSE){
    for(i in 1:length(list_of_variables)){
      data = full_data[!is.na(full_data[,list_of_variables[i]]),] # only work with non-missing data on the variable of interest
      # group is none ---------------------------------------
      if(is.numeric(data[,list_of_variables[i]]) | is.integer(data[,list_of_variables[i]])){
        ## Continuous --------------------------------------------------------------
        TemplateMatrix_OneGroup_Continuous = matrix(c(''),5,3)
        TemplateMatrix_OneGroup_Continuous[,2] = c('','N','mean (std)','median (IQR)','range')
        colnames(TemplateMatrix_OneGroup_Continuous) = c('variable','statistic','')
        seperate_info[[i]] = TemplateMatrix_OneGroup_Continuous
        # calculate the statistics 
        seperate_info[[i]][1,1] = paste0('**',ifelse(label(data[,list_of_variables[i]])=='',list_of_variables[i],label(data[,list_of_variables[i]])),'**')
        seperate_info[[i]][2,3] = nrow(data)
        seperate_info[[i]][3,3] = paste0(round(mean(data[,list_of_variables[i]],na.rm=TRUE),digits),'(',round(sd(data[,list_of_variables[i]],na.rm=TRUE),digits),')') 
        seperate_info[[i]][4,3] = paste0(round(median(data[,list_of_variables[i]],na.rm = TRUE),digits),'(',paste0(round(quantile(data[,list_of_variables[i]],c(.25),na.rm=TRUE),digits),';'
                                                                                        ,round(quantile(data[,list_of_variables[i]],c(.75),na.rm=TRUE),digits)),')')
        seperate_info[[i]][5,3] = paste0(round(range(data[,list_of_variables[i]],na.rm=TRUE)[1],digits),',',round(range(data[,list_of_variables[i]],na.rm=TRUE)[2],digits)) 
      }    
      
      if(is.factor(data[,list_of_variables[i]]) | is.character(data[,list_of_variables[i]]) ){
        ## Discrete --------------------------------------------------------------
        TemplateMatrix_OneGroup_Discrete = matrix(c(''),nlevels(data[,list_of_variables[i]])+1,3)
        TemplateMatrix_OneGroup_Discrete[1,1] = paste0('**',ifelse(label(data[,list_of_variables[i]])=='',list_of_variables[i],label(data[,list_of_variables[i]])),'**')
        TemplateMatrix_OneGroup_Discrete[2:(nlevels(data[,list_of_variables[i]])+1),2] = 'n/N (%)'
        colnames(TemplateMatrix_OneGroup_Discrete) = c('variable','statistic','')
        
        seperate_info[[i]] = TemplateMatrix_OneGroup_Discrete
        for(level in 1:nlevels(data[,list_of_variables[i]])){
          data[,'count_dummy'] = ifelse(data[,list_of_variables[i]]==levels(data[,list_of_variables[i]])[level],1,0)
          seperate_info[[i]][level+1,1] = levels(data[,list_of_variables[i]])[level]
          seperate_info[[i]][level+1,3] = paste0(sum(data[,'count_dummy']),'/',nrow(data),'(',(sum(data[,'count_dummy'])/nrow(data))*100,'%',')')
        }
      }
    }
    full_table = do.call("rbind", seperate_info)
    return(knitr::kable(full_table))
  }
  if (group==TRUE){
    for(i in 1:length(list_of_variables)){
      data = full_data[!is.na(full_data[,list_of_variables[i]]),] # only work with non-missing data on the variable of interest
      ## Continuous --------------------------------------------------------------
      if(is.numeric(data[,list_of_variables[i]]) | is.integer(data[,list_of_variables[i]])){
        TemplateMatrix_MultipleGroups_Continuous = matrix(c(''),5,nlevels(data[,which.group])+3)
        colnames(TemplateMatrix_MultipleGroups_Continuous) = c('variable','statistic','total',levels(data[,which.group]))
        TemplateMatrix_MultipleGroups_Continuous[1,1] = paste0('**',ifelse(label(data[,list_of_variables[i]])=='',list_of_variables[i],label(data[,list_of_variables[i]])),'**')
        TemplateMatrix_MultipleGroups_Continuous[2:5,2] = c('N','mean (std)','median (IQR)','range')
        seperate_info[[i]]=TemplateMatrix_MultipleGroups_Continuous
        # total information
        seperate_info[[i]][2,3] = nrow(data)
        seperate_info[[i]][3,3] = paste0(round(mean(data[,list_of_variables[i]],na.rm=TRUE),2),'(',round(sd(data[,list_of_variables[i]],na.rm=TRUE),digits),')') 
        seperate_info[[i]][4,3] = paste0(round(median(data[,list_of_variables[i]]),digits),'(',paste0(round(quantile(data[,list_of_variables[i]],c(.25),na.rm=TRUE),digits),';',
                                                                                        round(quantile(data[,list_of_variables[i]],c(.75),na.rm=TRUE),digits)),')')
        
        seperate_info[[i]][5,3] = paste0(round(range(data[,list_of_variables[i]],na.rm=TRUE)[1],digits),',',round(range(data[,list_of_variables[i]],na.rm=TRUE)[2],digits)) 
        # seperate group information
        for(ngroup in 1:nlevels(data[,which.group])){
          data_subset = subset(data,data[,which.group]==levels(data[,which.group])[ngroup]) # take subset on which.group levels
          
          seperate_info[[i]][2,3+ngroup] = length(data_subset[,list_of_variables[i]])
          seperate_info[[i]][3,3+ngroup] = paste0(round(mean(data_subset[,list_of_variables[i]],na.rm=TRUE),2),'(',round(sd(data_subset[,list_of_variables[i]],na.rm=TRUE),digits),')') 
          seperate_info[[i]][4,3+ngroup] = paste0(round(median(data_subset[,list_of_variables[i]]),digits),'(',paste0(round(quantile(data_subset[,list_of_variables[i]],c(.25),na.rm=TRUE),digits),',',
                                                                                                        round(quantile(data_subset[,list_of_variables[i]],c(.75),na.rm=TRUE),digits)),')')
          seperate_info[[i]][5,3+ngroup] = paste0(round(range(data_subset[,list_of_variables[i]],na.rm=TRUE)[1],digits),',',round(range(data_subset[,list_of_variables[i]],na.rm=TRUE)[2],digits)) 
          
        }
        if(formal.test==TRUE){
          # overal test
          Template_OveralTest = matrix('',nrow(TemplateMatrix_MultipleGroups_Continuous),1)
          colnames(Template_OveralTest) = 'Overall'
          Template_OveralTest[1,1] = 'p.val'
          Template_OveralTest[2,1] = format_pval(kruskal.test(data[,list_of_variables[i]] ~ data[,which.group])$p.value)
          
          
          Template_ContinuousTest = matrix('',nrow(TemplateMatrix_MultipleGroups_Continuous),
                                           nlevels(data[,which.group])*(nlevels(data[,which.group])-1)/2)
          colnames(Template_ContinuousTest) = letters[1:ncol(Template_ContinuousTest)]
          
          testnr=1
          for(k in 1:(length(levels(data[,which.group]))-1)){
            for(j in 2:length(levels(data[,which.group]))){
              if(k<j){
                colnames(Template_ContinuousTest)[testnr] = paste0('[',levels(data[,which.group])[k],' vs.',
                                                                   levels(data[,which.group])[j],']')
                
                Template_ContinuousTest[1,testnr] = 'AUC (95% CI)'
                
                data_subset = subset(data,data[,which.group] %in% levels(data[,which.group])[c(k,j)]) # take subset on which.group levels
                
                AUC.test = round(pROC::ci.auc(droplevels(data_subset[,which.group]), data_subset[,list_of_variables[i]]),digits)
                Template_ContinuousTest[2,testnr] = paste0(AUC.test[2],'(',AUC.test[1],';',AUC.test[3],')')
                testnr =  testnr + 1
              }
            }
          }
          seperate_info[[i]] = cbind(seperate_info[[i]],Template_OveralTest,Template_ContinuousTest)
        }
      }
      ## Discrete --------------------------------------------------------------
      if(is.factor(data[,list_of_variables[i]]) | is.character(data[,list_of_variables[i]])){
        TemplateMatrix_MultipleGroups_Discrete = matrix(c(''),nlevels(data[,list_of_variables[i]])+2,nlevels(data[,which.group])+3)
        TemplateMatrix_MultipleGroups_Discrete[1,1] = paste0('**',ifelse(label(data[,list_of_variables[i]])=='',list_of_variables[i],label(data[,list_of_variables[i]])),'**')
        TemplateMatrix_MultipleGroups_Discrete[3:(nlevels(data[,list_of_variables[i]])+2),2] = 'n/N (%)'
        colnames(TemplateMatrix_MultipleGroups_Discrete) = c('variable','statistic','total',c(levels(data[,which.group])))
        seperate_info[[i]] = TemplateMatrix_MultipleGroups_Discrete
        
        
        for(ngroup in 1:nlevels(data[,which.group])){
          data_subset = subset(data,data[,which.group]==levels(data[,which.group])[ngroup]) # take subset on which.group levels
          
          for(level in 1:nlevels(data[,list_of_variables[i]])){
            data[,'count_dummy'] = ifelse(data[,list_of_variables[i]]==levels(data[,list_of_variables[i]])[level],1,0)
            
            seperate_info[[i]][level+2,1] = levels(data_subset[,list_of_variables[i]])[level]
            
            seperate_info[[i]][level+2,3] = paste0(sum(data[,'count_dummy']),'/',nrow(data),'(',round((sum(data[,'count_dummy'])/nrow(data))*100,digits),'%',')')
            
            data_subset[,'count_dummy'] = ifelse(data_subset[,list_of_variables[i]]==levels(data_subset[,list_of_variables[i]])[level],1,0)
            seperate_info[[i]][level+2,3+ngroup] = paste0(sum(data_subset[,'count_dummy']),'/',nrow(data_subset),'(',round((sum(data_subset[,'count_dummy'])/nrow(data_subset))*100,digits),'%',')')
          }
        }
        if(formal.test==FALSE){
          seperate_info[[i]] = seperate_info[[i]][-2,] # fix bug in printing the table in knitr::kable()
        }
        if(formal.test==TRUE){
          # overall test -- based on Fisher exact test.
          Template_OveralTest_discrete = matrix('',nrow(TemplateMatrix_MultipleGroups_Discrete),1)
          colnames(Template_OveralTest_discrete) = 'Overall'
          Template_OveralTest_discrete[1,1] = 'p.val'
          Template_OveralTest_discrete[2,1] = format_pval((fisher.test(table(data[,which.group],data[,list_of_variables[i]]),alternative = "two.sided",conf.level=.95))$p.value)
          
          Template_DiscreteTest = matrix('',nrow(TemplateMatrix_MultipleGroups_Discrete),
                                         nlevels(data[,which.group])*(nlevels(data[,which.group])-1)/2)
          colnames(Template_DiscreteTest) = letters[1:ncol(Template_DiscreteTest)] # circumvent error when giving correct names in each pairwaise comparison
          if(nlevels(data[,list_of_variables[i]])==2){
            testnr=1
            for(k in 1:(length(levels(data[,which.group]))-1)){
              for(j in 2:length(levels(data[,which.group]))){
                if(k<j){
                  colnames(Template_DiscreteTest)[testnr] = paste0('[',levels(data[,which.group])[k],' vs.',
                                                                   levels(data[,which.group])[j],']')
                  
                  Template_DiscreteTest[1,testnr] = 'Diff (95% CI)'
                  
                  data_subset = subset(data,data[,which.group] %in% levels(data[,which.group])[c(k,j)]) # take subset on which.group levels
                  hold.CI = (prop.test(table(droplevels(data_subset[,which.group]),
                                             data_subset[,list_of_variables[i]]), correct=FALSE,conf.level=.95))$conf.int
                  
                  Template_DiscreteTest[2,testnr] = paste0(round(mean(hold.CI),digits),paste0('(',round(hold.CI[1],digits),',', round(hold.CI[2],digits),')'))
                  testnr =  testnr + 1
                }
              }
            }
          } else{
            testnr=1
            for(k in 1:(length(levels(data[,which.group]))-1)){
              for(j in 2:length(levels(data[,which.group]))){
                if(k<j){
                  colnames(Template_DiscreteTest)[testnr] = paste0('[',levels(data[,which.group])[k],' vs.',
                                                                   levels(data[,which.group])[j],']')
                  
                  Template_DiscreteTest[1,testnr] = '(p.val)'
                  
                  data_subset = subset(data,data[,which.group] %in% levels(data[,which.group])[c(k,j)]) # take subset on which.group levels
                  Template_DiscreteTest[2,testnr] = paste0('(',
                                                           format_pval((fisher.test(table(data_subset[,which.group],data_subset[,list_of_variables[i]]),alternative = "two.sided",conf.level=.95))$p.value),
                                                           ')')
                  testnr =  testnr + 1
                }
              }
            }
          }
          seperate_info[[i]] = cbind(seperate_info[[i]],Template_OveralTest_discrete,Template_DiscreteTest)
        }
      }
    }
  }
  full_table = do.call("rbind", seperate_info)
  #knitr::kable(full_table,caption = caption, digits = digits)
}