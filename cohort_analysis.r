library(plyr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Sample Data Set for Cohort Analysis
df <- data.frame(
  signup = c("2013-12-4 21:38:51", "2013-12-8 04:00:36", "2013-12-15 22:30:28", "2013-12-18 17:23:43"),
  event_1 = c("NULL", "2013-12-8 04:00:36", "2013-12-17 01:07:16", "NULL"),
  event_2 = c("2013-12-5 02:03:16", "NULL", "2013-12-21 03:47:55", "2013-12-25 07:05:58")
)

# convert to posix and convert null to na
df[df=="NULL"] <- NA
df <- as.data.frame(lapply(df, function(i) { ymd_hms(i) } ))

# The following function allows users to create a cohort analysis graph when starting with data in the format above
cohort_analysis <- function(event, cohort, time_unit="hours", title="Awesome Null Title", ytitle=NULL) {
  
   # customers are grouped into cohorts by the week they signed up
   # time between signup and event is calculated based on time_unit, "hours", "days", "weeks", etc
   # additionally, the signup cohorts are broken into weeks but that can also be changed to month or year by editing "breaks='week'"
   df <- data.frame(
      signup_week=as.Date(cut(cohort, breaks="week")),
      time_elapsed=floor(as.numeric(difftime(event, cohort, unit=time_unit)))
    )
    
    # find number of customers that signed up each week
    signup_totals <- data.frame(signup_week = df[,c('signup_week')])
    signup_totals <- as.data.frame(table(signup_totals$signup_week))
    colnames(signup_totals) <- c('signup_week', 'signup_freq')
    signup_totals$signup_week <- as.Date(signup_totals$signup_week)

    # calculate counts of customers who completed event 1 per time period
    event_totals <- df[,c('signup_week', 'time_elapsed')] 
    event_totals <- na.omit(event_totals)
    event_totals <- as.data.frame(table(event_totals))
    event_totals$signup_week <- as.Date(event_totals$signup_week)

    # Join counts for event 1 and signup into data frame
    final <- full_join(signup_totals, event_totals, by="signup_week")
    final <- na.omit(final)

    # calculate percent of customers who complete event 1 per time period
    final$time_elapsed <- as.numeric(final$time_elapsed) 
    final$percent <- (final$Freq / final$signup_freq) * 100 

    # -----------------------------------------------------------------------------------------------------
      # graph code based on code from http://www.r-bloggers.com/cohort-analysis-with-heatmap/ 
      # graph will show raw number counts while shading is based on percentages
      # if you want percentages shown instead you can change line 59 aes(label=Freq) to aes(label=percent)
    
    t <- 100
    
    ggplot(final, aes(x=time_elapsed, y=signup_week, fill=percent)) +
      theme_minimal() +
      geom_tile() +
      geom_text(colour= ifelse(final$percent > 40, "white", "black") , aes(label=Freq), size=3) +
      scale_fill_gradientn(colours= c("#CEFE98", "#A2DA74", "#7AB754", "#569438", "#377121", "#1D500E", "#093000") , limits=c(0, t),
                           breaks=seq(0, t, by=t/4),
                           labels=c("0", round(t/4*1, 1), round(t/4*2, 1), round(t/4*3, 1), round(t/4*4, 1)),
                           guide=guide_colourbar(ticks=T, nbin=50, barheight=.5, label=T, barwidth=10)) +
      theme(legend.position='bottom',
            legend.direction="horizontal",
            plot.title=element_text(size=20, face="bold", vjust=2),
            axis.text.x=element_text(size=8, angle=90, hjust=.5, vjust=.5, face="plain")) +
      xlab(time_unit) +
      ylab(ytitle) +
      ggtitle(title)
}
# -----------------------------------------------------------------------------------------------------

cohort_analysis(event=df$event_1, cohort=df$signup, time_unit="days", title="Time between signup and event 1", ytitle="Cohort by Week") 

