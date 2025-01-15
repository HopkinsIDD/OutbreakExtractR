#' @export
#' @title identify_epidemic_period
#' @name identify_epidemic_period
#' @description this function will be used in identify_outbreak** functions. This function will separate different outbreaks in the time series data for a certain location with a certain threshold.
identify_epidemic_period <- function(
    outbreak_data = outbreak_with_start,
    epidemic_tail = 6,
    max_missingdata_period = 12 #unit is in weeks (3 months)
){
  #outbreaks with epidemic starts
  #label outbreak_number (used to distinguish outbreaks at the same location period and same start weekdays)
  #compare the data between two consecutive high-risk weeks
  ## 1. greater than a tail_period+washout_period; 2. maximum_non_reporting time; 3.
  #identify tails of outbreaks
  ## 1. below outbreak threshold. 2. for certain number of consecutive weeks

  outbreak_data$outbreak_number <- NA
  outbreak_data$complete <- FALSE
  outbreak_data$epidemic_period <- FALSE

  if(!nrow(outbreak_data[which(outbreak_data$epidemic_start==T),]) == 0){

    date_seq=lubridate::ymd(unique(outbreak_data[which(outbreak_data$epidemic_start==T),]$TL))
    index = 1

    for (t in as.character(date_seq)) {
      #label the outbreak number and threshold based on the potential start of an outbreak
      outbreak_data[which(outbreak_data$TL>=lubridate::ymd(t)),]$outbreak_number=index
      index=index+1
    }
    
    #outbreak number after the last epidemic start is NA
    outbreak_data[which(outbreak_data$TL>lubridate::ymd(as.character(date_seq)[length(as.character(date_seq))])),]$outbreak_number=NA
    
    outbreak_with_outbreak_number <- data.frame()
    
    if(max(outbreak_data$outbreak_number,na.rm=TRUE)>1){

      for (outbreak_idx in 1:(max(outbreak_data[is.na(outbreak_data$outbreak_number)==F,]$outbreak_number)-1)) {

        outbreak_tmp_with_tail_washout_period <- outbreak_data %>% subset(outbreak_number == outbreak_idx)
        
        if(all(!outbreak_tmp_with_tail_washout_period$epidemic_tail)){
          #if no epidemic washout period, the reason could be:
          ## 1. data is complete but outbreak hasn't ended,
          ### 1.1 check if the time period beweeen the end of this outbreak and beginning of next outbreak number is greater than the max_missingdata_period,
          ### if yes, incomplete outbreaka and move that to the next outbreak number
          ### if no, combine this outbreak into the next outbreak number
          next_outbreak <- outbreak_data %>% subset(outbreak_number == outbreak_idx+1)
          
          #comparing two high risk weeks if the time exceed max missing reporting date
          #if no, combine the two outbreak number change the threshold, risk group, pop and outbreak number
          if((min(next_outbreak$TL)-max(outbreak_tmp_with_tail_washout_period$TL))/7<=max_missingdata_period){
            outbreak_tmp_with_tail_washout_period$outbreak_number <- unique(outbreak_tmp_with_tail_washout_period$outbreak_number +1)
            #relabel some parameters to match with the next outbreak
            outbreak_data[which(outbreak_data$outbreak_number == outbreak_idx),]$threshold <- unique(next_outbreak$threshold)
            outbreak_data[which(outbreak_data$outbreak_number == outbreak_idx),]$population <- min(next_outbreak$population)
            outbreak_data[which(outbreak_data$outbreak_number == outbreak_idx),]$risk <- ifelse(outbreak_data[which(outbreak_data$outbreak_number == outbreak_idx),]$sCh>=unique(next_outbreak$threshold)*min(next_outbreak$population),"high","low")
            outbreak_data[which(outbreak_data$outbreak_number == outbreak_idx),]$outbreak_number = unique(next_outbreak$outbreak_number) 
            
            #no data needs to be returned to outbreak_with_outbreak_number
            new_outbreak_data<- data.frame()

          }else{
            #if yes, this is an imcomplete outbreak
            ## label epidemic period
            outbreak_tmp_with_tail_washout_period[!outbreak_tmp_with_tail_washout_period$epidemic_start,]$epidmeic_period <- TRUE
            ## this is an incomplete outbreak
            outbreak_tmp_with_tail_washout_period$complete <- FALSE
            new_outbreak <- outbreak_tmp_with_tail_washout_period
          }
        } else {
          ## COMPLETE OUTBREAK ##
          # outbreak contains start, tail and washout period, thus it's a complete outbreak.
          # check if all data is complete and consecutive between start and washout period
          if(all(diff(outbreak_tmp_with_tail_washout_period[outbreak_tmp_with_tail_washout_period$TL<=min(outbreak_tmp_with_tail_washout_period[outbreak_tmp_with_tail_washout_period$epidemic_tail,]$TL),]$TL)==7)){
            new_outbreak_data <- outbreak_tmp_with_tail_washout_period[outbreak_tmp_with_tail_washout_period$TL<=min(outbreak_tmp_with_tail_washout_period[outbreak_tmp_with_tail_washout_period$epidemic_tail,]$TL)+(epidemic_tail-1) *7,]

            #label epidmeic period
            new_outbreak_data[new_outbreak_data$TL>max(new_outbreak_data[new_outbreak_data$epidemic_start,]$TL) &
                                new_outbreak_data$TL<min(new_outbreak_data[new_outbreak_data$epidemic_tail,]$TL),]$epidemic_period <- TRUE
            #mark outbreak as complete outbreak
            new_outbreak_data$complete <- TRUE
            
          } else {
            new_outbreak_data <- outbreak_tmp_with_tail_washout_period[outbreak_tmp_with_tail_washout_period$TL<=min(outbreak_tmp_with_tail_washout_period[outbreak_tmp_with_tail_washout_period$epidemic_tail,]$TL)+(epidemic_tail-1) *7,]
            #label epidmeic period
            new_outbreak_data[new_outbreak_data$TL>max(new_outbreak_data[new_outbreak_data$epidemic_start,]$TL) &
                                new_outbreak_data$TL<min(new_outbreak_data[new_outbreak_data$epidemic_tail,]$TL),]$epidemic_period <- TRUE
            #mark outbreak as incomplete outbreak
            new_outbreak_data$complete <- FALSE
          }
        }
        outbreak_with_outbreak_number <- rbind(outbreak_with_outbreak_number, new_outbreak_data)
      }
      
    }
    
    #For the last outbreak number
    last_outbreak_tmp <- outbreak_data %>% subset(outbreak_number == max(outbreak_data$outbreak_number,na.rm = T))
    last_outbreak_tmp_with_tail_washout_period <-  outbreak_data %>% subset(TL>=min(last_outbreak_tmp$TL))
    
    if(any(last_outbreak_tmp_with_tail_washout_period$epidemic_tail)){
      last_outbreak_tmp_with_tail_washout_period <- last_outbreak_tmp_with_tail_washout_period[last_outbreak_tmp_with_tail_washout_period$TL<=min(last_outbreak_tmp_with_tail_washout_period[last_outbreak_tmp_with_tail_washout_period$epidemic_tail,]$TL)+(epidemic_tail-1)*7,]
      last_outbreak_tmp_with_tail_washout_period[which(!last_outbreak_tmp_with_tail_washout_period$epidemic_start&
                                                   !last_outbreak_tmp_with_tail_washout_period$epidemic_tail),]$epidemic_period <- TRUE
      
      #check if outbreak data is complete
      if(all(diff(last_outbreak_tmp_with_tail_washout_period$TL)==7)){
        last_outbreak_tmp_with_tail_washout_period$complete <- TRUE
      } else{
        last_outbreak_tmp_with_tail_washout_period$complete <- FALSE
      }
      
      #reassign population, outbreak threshold, etc
      last_outbreak_tmp_with_tail_washout_period$outbreak_number=as.numeric(na.omit(unique(last_outbreak_tmp_with_tail_washout_period$outbreak_number)))
      last_outbreak_tmp_with_tail_washout_period$threshold=min(last_outbreak_tmp_with_tail_washout_period$threshold)
      last_outbreak_tmp_with_tail_washout_period$population=min(last_outbreak_tmp_with_tail_washout_period$population)
      
      outbreak_with_outbreak_number <- rbind(outbreak_with_outbreak_number,last_outbreak_tmp_with_tail_washout_period)

    } else {
      last_outbreak_tmp_with_tail_washout_period[!last_outbreak_tmp_with_tail_washout_period$epidemic_start,]$epidemic_period <- TRUE
      last_outbreak_tmp_with_tail_washout_period$complete  = FALSE
      
      #reassign outbreak_number, population, outbreak threshold, etc
      last_outbreak_tmp_with_tail_washout_period$outbreak_number=as.numeric(na.omit(unique(last_outbreak_tmp_with_tail_washout_period$outbreak_number)))
      last_outbreak_tmp_with_tail_washout_period$threshold=min(last_outbreak_tmp_with_tail_washout_period$threshold)
      last_outbreak_tmp_with_tail_washout_period$population=min(last_outbreak_tmp_with_tail_washout_period$population)
      
      
      outbreak_with_outbreak_number <- rbind(outbreak_with_outbreak_number,last_outbreak_tmp_with_tail_washout_period)
    }
    
    return(outbreak_with_outbreak_number)
    
  } else {
    outbreak_data$epidemic_period <- FALSE
    outbreak_data$epidemic_tail <- FALSE
    outbreak_data$complete <- FALSE

    return(outbreak_data)
    message("there's no outbreak in this time series")
  }
}