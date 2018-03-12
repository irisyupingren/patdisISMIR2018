selectjsym <- function(songfile){
  songfea <- songfile[,3:158]
  idcol <- songfile[,1]
  songfea[is.na(songfea)] <-0
  songfea <- songfea[,apply(songfea ,2,var,na.rm=TRUE) != 0]
  polynames <- c("Most_Common_Vertical_Interval",
                 "Second_Most_Common_Vertical_Interval",
                 "Distance_Between_Two_Most_Common_Vertical_Intervals",
                 "Prevalence_of_Most_Common_Vertical_Interval",
                 "Prevalence_of_Second_Most_Common_Vertical_Interval",
                 "Prevalence_Ratio_of_Two_Most_Common_Vertical_Intervals",
                 "Vertical_Unisons",
                 "Vertical_Thirds",  
                 "Vertical_Perfect_Fourths",                                  
                 "Vertical_Sixths",                                            
                 "Perfect_Vertical_Intervals",                                
                 "Vertical_Dissonance_Ratio" ,                                 
                 "Vertical_Minor_Third_Prevalence",                           
                 "Chord_Duration" ,                                            
                 "Partial_Chords",
                 "Contrary_Motion",
                 "Average_Number_of_Simultaneous_Pitch_Classes",              
                 "Variability_of_Number_of_Simultaneous_Pitch_Classes",       
                 "Average_Number_of_Simultaneous_Pitches" ,                   
                 "Variability_of_Number_of_Simultaneous_Pitches",
                 "Number_of_Pitched_Instruments" ,
                 "String_Keyboard_Prevalence" ,
                 "Maximum_Number_of_Independent_Voices",
                 "Average_Number_of_Independent_Voices",
                 "Oblique_Motion" ,
                 "Initial_Tempo" ,
                 "Variation_of_Dynamics_In_Each_Voice",
                 "Average_Note_to_Note_Change_in_Dynamics",
                 "Variation_of_Dynamics",
                 "Vertical_Major_Third_Prevalence",
                 "Vertical_Perfect_Fifths",
                 "Dynamic_Range",
                 "Similar_Motion",
                 "Glissando_Prevalence",
                 "Microtone_Prevalence",
                 "Average_Variability_of_Time_Between_Attacks_for_Each_Voice",
                 "Relative_Range_of_Loudest_Voice",
                 "Average_Rest_Fraction_Per_Voice",
                 "Average_Time_Between_Attacks_for_Each_Voice",
                 "Relative_Note_Durations_of_Lowest_Line",
                 "Relative_Size_of_Melodic_Intervals_in_Lowest_Line",
                 "Relative_Range_of_Highest_Line",
                 "Relative_Note_Density_of_Highest_Line")
  songfea <- songfea[,!names(songfea) %in% polynames]
  songfea <- cbind(idcol, songfea)
  return(songfea)
}


selepatjsym <- function(songfile){
  songfea <- songfile[,2:157]
  songfea[is.na(songfea)] <-0
  songfea <- songfea[,apply(songfea ,2,var,na.rm=TRUE) != 0]
  polynames <- c("Most_Common_Vertical_Interval",
                 "Second_Most_Common_Vertical_Interval",
                 "Distance_Between_Two_Most_Common_Vertical_Intervals",
                 "Prevalence_of_Most_Common_Vertical_Interval",
                 "Prevalence_of_Second_Most_Common_Vertical_Interval",
                 "Prevalence_Ratio_of_Two_Most_Common_Vertical_Intervals",
                 "Vertical_Unisons",
                 "Vertical_Thirds",  
                 "Vertical_Perfect_Fourths",                                  
                 "Vertical_Sixths",                                            
                 "Perfect_Vertical_Intervals",                                
                 "Vertical_Dissonance_Ratio" ,                                 
                 "Vertical_Minor_Third_Prevalence",                           
                 "Chord_Duration" ,                                            
                 "Partial_Chords",
                 "Contrary_Motion",
                 "Average_Number_of_Simultaneous_Pitch_Classes",              
                 "Variability_of_Number_of_Simultaneous_Pitch_Classes",       
                 "Average_Number_of_Simultaneous_Pitches" ,                   
                 "Variability_of_Number_of_Simultaneous_Pitches",
                 "Number_of_Pitched_Instruments" ,
                 "String_Keyboard_Prevalence" ,
                 "Maximum_Number_of_Independent_Voices",
                 "Average_Number_of_Independent_Voices",
                 "Oblique_Motion" ,
                 "Initial_Tempo" ,
                 "Variation_of_Dynamics_In_Each_Voice",
                 "Average_Note_to_Note_Change_in_Dynamics",
                 "Variation_of_Dynamics",
                 "Vertical_Major_Third_Prevalence",
                 "Vertical_Perfect_Fifths",
                 "Dynamic_Range",
                 "Similar_Motion",
                 "Glissando_Prevalence",
                 "Microtone_Prevalence",
                 "Average_Variability_of_Time_Between_Attacks_for_Each_Voice",
                 "Relative_Range_of_Loudest_Voice",
                 "Average_Rest_Fraction_Per_Voice",
                 "Average_Time_Between_Attacks_for_Each_Voice",
                 "Relative_Note_Durations_of_Lowest_Line",
                 "Relative_Size_of_Melodic_Intervals_in_Lowest_Line",
                 "Relative_Range_of_Highest_Line",
                 "Relative_Note_Density_of_Highest_Line")
  songfea <- songfea[,!names(songfea) %in% polynames]
  return(songfea)
}

selemergedjsym <- function(songfile){
  songfea <- songfile[,6:161]
  songfea[is.na(songfea)] <-0
  songfea <- songfea[,apply(songfea ,2,var,na.rm=TRUE) != 0]
  polynames <- c("Most_Common_Vertical_Interval",
                 "Second_Most_Common_Vertical_Interval",
                 "Distance_Between_Two_Most_Common_Vertical_Intervals",
                 "Prevalence_of_Most_Common_Vertical_Interval",
                 "Prevalence_of_Second_Most_Common_Vertical_Interval",
                 "Prevalence_Ratio_of_Two_Most_Common_Vertical_Intervals",
                 "Vertical_Unisons",
                 "Vertical_Thirds",  
                 "Vertical_Perfect_Fourths",                                  
                 "Vertical_Sixths",                                            
                 "Perfect_Vertical_Intervals",                                
                 "Vertical_Dissonance_Ratio" ,                                 
                 "Vertical_Minor_Third_Prevalence",                           
                 "Chord_Duration" ,                                            
                 "Partial_Chords",
                 "Contrary_Motion",
                 "Average_Number_of_Simultaneous_Pitch_Classes",              
                 "Variability_of_Number_of_Simultaneous_Pitch_Classes",       
                 "Average_Number_of_Simultaneous_Pitches" ,                   
                 "Variability_of_Number_of_Simultaneous_Pitches",
                 "Number_of_Pitched_Instruments" ,
                 "String_Keyboard_Prevalence" ,
                 "Maximum_Number_of_Independent_Voices",
                 "Average_Number_of_Independent_Voices",
                 "Oblique_Motion" ,
                 "Initial_Tempo" ,
                 "Variation_of_Dynamics_In_Each_Voice",
                 "Average_Note_to_Note_Change_in_Dynamics",
                 "Variation_of_Dynamics",
                 "Vertical_Major_Third_Prevalence",
                 "Vertical_Perfect_Fifths",
                 "Dynamic_Range",
                 "Similar_Motion",
                 "Glissando_Prevalence",
                 "Microtone_Prevalence",
                 "Average_Variability_of_Time_Between_Attacks_for_Each_Voice",
                 "Relative_Range_of_Loudest_Voice",
                 "Average_Rest_Fraction_Per_Voice",
                 "Average_Time_Between_Attacks_for_Each_Voice",
                 "Relative_Note_Durations_of_Lowest_Line",
                 "Relative_Size_of_Melodic_Intervals_in_Lowest_Line",
                 "Relative_Range_of_Highest_Line",
                 "Relative_Note_Density_of_Highest_Line")
  songfea <- songfea[,!names(songfea) %in% polynames]
  songfea$tf <- as.factor(songfile[,2])
  songfea$des <- as.factor(songfile[,3])
  songfea$anno <-as.factor(songfile[,4])
  
  return(songfea)
}
