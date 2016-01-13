# Sample Data Set for Cohort Analysis

df <- data.frame(
  signup = c("2013-12-4 21:38:51", "2013-12-8 04:00:36", "2013-12-15 22:30:28", "2013-12-18 17:23:43"),
  event_1 = c("NULL", "2013-12-8 04:00:36", "2013-12-17 01:07:16", "NULL"),
  event_2 = c("2013-12-5 02:03:16", "NULL", "2013-12-21 03:47:55", "2013-12-25 07:05:58")
)

# The following function allows users to create a cohort analysis graph when starting with data in the format above

# convert to posix and convert null to na
df[df=="NULL"] <- NA
df <- as.data.frame(lapply(df, function(i) { ymd_hms(i) } ))

library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


cohort_analysis <- function(event, cohort, timediff="hours", title="Awesome Null Title", ytitle=NULL) {
  
   # customers are grouped into cohorts by the week they signed up
   # time between signup and event is calculated based on preference, "hours", "days", "weeks", etc
   df<-data.frame(
      weeks_signup = as.Date(cut(cohort, breaks= "week")),
      time_elapsed = floor(as.numeric(difftime(event, cohort, unit=timediff)))
    )
    
    
    # calculate counts for customers in each signup cohort by week
    signup_totals <- data.frame(weeks_signup = df[,c('weeks_signup')])
    signup_totals <- as.data.frame(table(signup_totals$weeks_signup))
    colnames(signup_totals) <- c('weeks_signup', 'signup_freq')
    signup_totals$weeks_signup <- as.Date(signup_totals$weeks_signup)

    # calculate counts of customers who completed event 1 per time period
    event <- df[,c('weeks_signup', 'time_elapsed')] 
    event <- na.omit(event)
    df_table <- as.data.frame(table(event))
    df_table$weeks_signup <- as.Date(df_table$weeks_signup)

    # Join counts for event 1 and signup into data frame
    final <- full_join(signup_totals, df_table, by="weeks_signup")
    final <- na.omit(final)

    # calculate percent of customers who complete event 1 per time period
    final$time_elapsed <- as.numeric(final$time_elapsed) 
    final$percent <- (final$Freq / final$signup_freq) * 100 

    # -----------------------------------------------------------------------------------------------------
      # graph code based on code from http://www.r-bloggers.com/cohort-analysis-with-heatmap/ 
      # graph will show raw number counts while shading is based on percentages
      # if you want percentages shown instead you can change line 59 aes(label=Freq) to aes(label=percent)
    
    t <- 100

    ggplot(final, aes(x=time_elapsed, y=weeks_signup, fill=percent)) +
      theme_minimal() +
      geom_tile(colour="white", linewidth=2) +
      geom_text(colour="black", aes(label=Freq), size=2) +
      scale_fill_gradientn(colours=rainbow(10) , limits=c(0, t),
                           breaks=seq(0, t, by=t/4),
                           labels=c("0", round(t/4*1, 1), round(t/4*2, 1), round(t/4*3, 1), round(t/4*4, 1)),
                           guide=guide_colourbar(ticks=T, nbin=50, barheight=.5, label=T, barwidth=10)) +
      theme(legend.position='bottom',
            legend.direction="horizontal",
            plot.title = element_text(size=20, face="bold", vjust=2),
            axis.text.x=element_text(size=8, angle=90, hjust=.5, vjust=.5, face="plain")) +
      xlab(timediff) +
      ylab(ytitle) +
      ggtitle(title)
}
# -----------------------------------------------------------------------------------------------------

cohort_analysis(event = df$event_1, cohort = df$signup, timediff="days", title = "Time between signup and event 1", ytitle = "Cohort by Week") 

