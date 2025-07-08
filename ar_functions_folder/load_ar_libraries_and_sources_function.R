load_ar_libraries_and_sources_function <- 
  function(
    location_of_functions_folder="C:/Users/User/Documents/NRL_R_work/iCAH_Blood_pressure/",
    location_of_data_files="C:/Users/User/Documents/NRL_R_work/iCAH_Blood_pressure/ar_data_files_to_load/",
    statement_to_print="Functions and themes loaded, and folder data_files_to_load is ready in the parent directory. The source of the functions was:"){
  print(statement_to_print)
  print(location_of_functions_folder)
    #first we create a list of dataframes that we save, if they exist, after each file to be able to split the code up   
    #add a frame to this list to ensure it is both saved after a file if it exists, and loaded at the beginning of a file if it exists
list_of_data_frames_to_save <- 
  as.list(c(
#These are all the ICAH BP files names that will be saved if they are present in the global environment when the function is run:
    
  #these are the four frames we load in the first place with raw data in them
      "ar_participants",
      "ar_medication",
      "ar_labs",
#no need to save ar_fludro, we just save ar_fludro_wide      
#      "ar_fludro",
      #this is a tidied lab frame that needs saving ready for it to be widened:
      "ar_labs_to_widen",
      "ar_labs_to_widen_with_units",
      #these are the wide frames we create for joining
      "ar_participants_longitudinal_data",
      "ar_participants_base_data",
      "ar_labs_wide",
      "ar_meds_wide",
      "ar_fludro_wide",
      "converted_17OHP_to_join",
      "converted_androstenedione_to_join",
      "converted_renin_to_join",
      "converted_plasma_renin_activity_to_join",
      "converted_combined_renin_to_join",
      "prednisolone_to_hydrocortisone_conversion",

      "dexamethasone_to_hydrocortisone_conversion",

      "cortisone_acetate_to_hydrocortisone_conversion",

      "methylprednisolone_to_hydrocortisone_conversion",
      #finally, we have a frame that is complete with all our data on one row per visit
      "ar_data",
      "ar_previous_extraction_joined_to_model",
      #this frame is used to take forward and model decimal ages:
      "ar_martin_joined_to_model",
      #these are old frames I used to save that I'm not sure need saving anymore
      "martin_extraction_decimal_age_to_join", #this comes from file 9
#      "ar_participants_without_base_data_with_z",
#      "who_z_scores_with_bp",
#      "ar_participants_with_labs",
#      "ar_labs_meds_with_z",
#      "ar_labs_meds_with_male_bmi_z"
      #when we run sitar through loads of different permutations of restrictions, we only plot and extract the one we decide is optimal. we save how we create that one here:
      "suffix_of_optimum_weight_model_restrictions", 
      "suffix_of_optimum_height_model_restrictions",
      "suffix_of_optimum_bmi_model_restrictions",
      "ln_17ohp_auc_bins_long",
      "ln_17ohp_auc_bins_wide",
      "ln_androstenedione_auc_bins_long",
      "ln_androstenedione_auc_bins_wide",
      "hyd_eq_dose_per_bsa_bins_wide",
      "hyd_eq_dose_per_bsa_bins_long",
      "hyd_eq_dose_absolute_bins_wide",
      "hyd_eq_dose_absolute_bins_long",
      "fludro_dose_per_bsa_bins_wide",
      "fludro_dose_per_bsa_bins_long",
      "fludro_dose_absolute_bins_wide",
      "fludro_dose_absolute_bins_long",
      "bins_with_sitar_long",

#we want to reload all of our optimum example frames
  "optimum_multivariable_example_frames_male_height",
  "optimum_multivariable_example_frames_male_weight",
  "optimum_multivariable_example_frames_male_bmi",
  "optimum_null_example_frames_male_height",
  "optimum_null_example_frames_male_weight",
  "optimum_null_example_frames_male_bmi",

  "optimum_multivariable_example_frames_fema_height",
  "optimum_multivariable_example_frames_fema_weight",
  "optimum_multivariable_example_frames_fema_bmi",
  "optimum_null_example_frames_fema_height",
  "optimum_null_example_frames_fema_weight",
  "optimum_null_example_frames_fema_bmi",

#we want to reload all of our optimum models
  "optimum_height_null_model_male",
  "optimum_weight_null_model_male",
  "optimum_bmi_null_model_male",
  "optimum_height_null_model_fema",
  "optimum_weight_null_model_fema",
  "optimum_bmi_null_model_fema",

  "optimum_height_covariate_model_male",
  "optimum_weight_covariate_model_male",
  "optimum_bmi_covariate_model_male",
  "optimum_height_covariate_model_fema",
  "optimum_weight_covariate_model_fema",
  "optimum_bmi_covariate_model_fema"


    ))

#assign the list to the global environment
    assign(x="list_of_data_frames_to_save", 
           value=list_of_data_frames_to_save, 
           env=.GlobalEnv)

#create a file where files can be saved and reloaded        
dir.create("ar_data_files_to_load")

#save the list outside of R 
    save(x=list_of_data_frames_to_save, 
         file = paste0(location_of_data_files, "list_of_data_frames_to_save.Rdata"), 
         compress=F)    

suppressWarnings({
  if(!require(rlang)){install.packages("rlang")}
  library(rlang)
  if(!require(piecewiseSEM)){install.packages("piecewiseSEM")}
  library(piecewiseSEM)
  if(!require(nlme)){install.packages("nlme")}
  library(nlme)
  if(!require(readxl)){install.packages("readxl")}
  library(readxl)
  if(!require(dplyr)){install.packages("dplyr")}
  library(dplyr)
  if(!require(mixtools)){install.packages("mixtools")}
  library(mixtools)
  if(!require(emmeans)){install.packages("emmeans")}
  library(emmeans)
  if(!require(readr)){install.packages("readr")}
  library(readr)
  if(!require(tidyverse)){install.packages("tidyverse")}
  library(tidyverse)
  if(!require(data.table)){install.packages("data.table")}
  library(data.table)
  if(!require(lubridate)){install.packages("lubridate")}
  library(lubridate)
  if(!require(ggplot2)){install.packages("ggplot2")}
  library(ggplot2)
  if(!require(ggpubr)){install.packages("ggpubr")}
  library(ggpubr)
  if(!require(rstatix)){install.packages("rstatix")}
  library(rstatix)
  if(!require(multcompView)){install.packages("multcompView")}
  library(multcompView)
  if(!require(knitr)){install.packages("knitr")}
  library(knitr)
  if(!require(DT)){install.packages("DT")}
  library(DT)
  if(!require(extrafont)){install.packages("extrafont")}
  library(extrafont)
#  if(!require(easycsv)){install.packages("easycsv")}
#  library(easycsv)
  if(!require(PairedData)){install.packages("PairedData")}
  library(PairedData)
  if(!require(compare)){install.packages("compare")}
  library(compare)
  if(!require(pastecs)){install.packages("pastecs")}
  library(pastecs)
  if(!require(pwr)){install.packages("pwr")}
  library(pwr)
  if(!require(gdata)){install.packages("gdata")}
  library(gdata)
  if(!require(skimr)){install.packages("skimr")}
  library(skimr)
  if(!require(summarytools)){install.packages("summarytools")}
  library(summarytools)
  if(!require(GGally)){install.packages("GGally")}
  library(GGally)
  if(!require(BSDA)){install.packages("BSDA")}
  library(BSDA)
  if(!requireNamespace("BiocManager", quietly = TRUE))
    if(!require(BiocManager)){install.packages("BiocManager")}
  if(!require(ggforce)){install.packages("ggforce")}
  library(ggforce)
  if(!require(svglite)){install.packages("svglite")}
  library(svglite)
  if(!require(devtools)) install.packages("devtools")
  if(!require(factoextra)){install.packages("factoextra")}
  library(factoextra)
  if(!require(mice)){install.packages("mice")}
  library(mice)
  if(!require(missForest)){install.packages("missForest")}
  library(missForest)
  if(!require(Hmisc)){install.packages("Hmisc")}
  library(Hmisc)
  if(!require(kableExtra)){install.packages("kableExtra")}
  library(kableExtra)
  if(!require(gt)){install.packages("gt")}
  library(gt)
  if(!require(glue)){install.packages("glue")}
  library(glue)
  if(!require(lmerTest)){install.packages("lmerTest")}
  library(lmerTest)# to get p-value estimations that are not part of the standard lme4 packages
  if(!require(effects)){install.packages("effects")}
  library(effects)
  if(!require(multilevelTools)){install.packages("multilevelTools")}
  library(multilevelTools)
  if(!require(RColorBrewer)){install.packages("RColorBrewer")}
  library(RColorBrewer)
  #note that gamlss masks functions from mfp and from lmer, so need to be careful
  if(!require(gamlss)){install.packages("gamlss")}
  library(gamlss)
  if(!require(lme4)){install.packages("lme4")}
  library(lme4)
  if(!require(mfp)){install.packages("mfp")}
  library(mfp)
  if(!require(glmnet)){install.packages("glmnet")}
  library(glmnet)
  if(!require(shrink)){install.packages("shrink_1.2.1.tar.gz", repos=NULL, type="source")}
  library(shrink)
  if(!require(slm)){install.packages("slm")}
  library(slm)
  if(!require(cowplot)){install.packages("cowplot")}
  library(cowplot)
  if(!require(lqmm)){install.packages("lqmm")}
  library(lqmm)
  if(!require(gridExtra)){install.packages("gridExtra")}
  library(gridExtra)
  if(!require(sitar)){install.packages("sitar")}
  library(sitar)
  if(!require(zscorer)){install.packages("zscorer")}
  library(zscorer)
  source(paste0(location_of_functions_folder, "icah_ar_plot_themes.R"))
  source(paste0(location_of_functions_folder, "save_ar_files_function.R"))
  source(paste0(location_of_functions_folder, "load_ar_files_function.R"))
  source(paste0(location_of_functions_folder, "extra_sitar_functions.R"))
  source(paste0(location_of_functions_folder, "data_attrition_function.R"))
  source(paste0(location_of_functions_folder, "calculate_adiposity_rebound_function.R"))
  
  
  
})
  }


