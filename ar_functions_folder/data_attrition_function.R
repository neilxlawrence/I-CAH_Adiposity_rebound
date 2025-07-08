create_sandwich_attrition_frame <-
  function(
    data_frame_to_assess_sandwich_attrition,
    min_age, # min_age <- 0.2
    max_age, # max_age <- 7
    min_visits, #min_visits <- 6
    variable){ #variable <- "bmi_using_interpolations"

    #make our data frame unique just in case someone is running a frame through with duplicated rows
    data_frame_to_assess_sandwich_attrition <- 
      unique(
        data_frame_to_assess_sandwich_attrition[,c(
#        ar_data[,c( #swap these when function is ready
          "id",
          "age_to_use",
          "earliest_visit_age",
          "oldest_visit_age",
          "visit_number",
          "interpolated_height",
          "interpolated_weight",
          "bmi_using_interpolations"
        )]
      )
    
    #filter just those visits that have the variable of interest
    data_with_variable <- 
      data_frame_to_assess_sandwich_attrition %>%
        filter(!is.na(.data[[variable]]))
    
    #ensure the frame is ordered within id by age
    data_with_variable <- 
      data_with_variable[order(data_with_variable$id, data_with_variable$age_to_use), ]
    
    #take out the data that applies within restrictions
    data_frame_within_ages <-
      subset(
        data_with_variable,
          age_to_use>min_age &
          age_to_use<max_age
      )
    
    #create visit_number_within_restrictions
    data_frame_within_ages <-
      data_frame_within_ages %>%
      group_by(id) %>%
      arrange(id, age_to_use) %>%  # Ensure correct order within each ID
      mutate(visit_number_within_ages = row_number())  # Add visit number per ID
    
    #find our maximum visit number within the age restriction, so we can also restrict by that
    maximum_visit_number_within_ages <-
      data_frame_within_ages %>%
      group_by(id) %>%
      slice_max(visit_number_within_ages, n=1, with_ties = F)
    
    #take out just the column we want and then join it back in
    maximum_visit_number_within_ages <-
      maximum_visit_number_within_ages[,c(
      "id",
      "visit_number_within_ages"
    )]
    
    maximum_visit_number_within_ages <-
      dplyr::rename(maximum_visit_number_within_ages, c("max_visit_number_within_ages" = "visit_number_within_ages"))
    
    data_frame_within_ages_with_max <-
      left_join(
        data_frame_within_ages,
        maximum_visit_number_within_ages,
        by="id"
      )
    #then we create our data frame within restrictions by simply subsetting by our min_visits
    data_frame_within_restrictions <-
      subset(data_frame_within_ages_with_max,
             max_visit_number_within_ages>=min_visits)
    
    #we therefore now have a list of ids that have gone into the overall modelling in unique(data_frame_within_restrictions$id)
    #at each age
    
    #we take the earliest and oldest age for each patient within restrictions
    earliest_visit_within_restrictions <-
      data_frame_within_restrictions %>%
      group_by(id) %>%
      slice_min(age_to_use, n=1, with_ties=F)
    oldest_visit_within_restrictions <-
      data_frame_within_restrictions %>%
      group_by(id) %>%
      slice_max(age_to_use, n=1, with_ties=F)
    
    #we now just create a long frame with age down the one column increasing from 0 through to 20
    attrition_frame <-
      data.frame(
        age = seq(min_age, max_age, by=0.1)
      )
    
    #within that frame, for each age, we want to know the number of patients that are within the study AND have a reading before AND after that particular age
    attrition_frame <- attrition_frame %>%
      rowwise() %>%  # Ensures computation per row
      mutate(within_model = 
               sum(
                 oldest_visit_within_restrictions$age_to_use > age &
                 earliest_visit_within_restrictions$age_to_use < age)
             ) %>%
      ungroup()

    #calculate the percentage of all of those within the model
    attrition_frame$percentage_of_patients_from_model_within_model_at_this_age <-
      attrition_frame$within_model / length(unique(data_frame_within_restrictions$id)) * 100
    #calculate the percentage of all patients
    attrition_frame$percentage_of_patients_from_whole_dataset_in_model_at_this_age <-
      attrition_frame$within_model / length(unique(data_frame_to_assess_sandwich_attrition$id)) * 100
    
    #we also want the percentage of all patients used to create the model within this attrition frame to make plotting easy
    attrition_frame$percentage_of_patients_from_whole_dataset_in_model_overall <-
      length(unique(data_frame_within_restrictions$id)) / length(unique(data_frame_to_assess_sandwich_attrition$id)) * 100
    
    return(attrition_frame)
  }
