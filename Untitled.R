install.packages(c("devtools", "rjson", "bit64", "httr"))

library(devtools)
install_github("twitteR", username="geoffjentry")


#connect all libraries
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
require(RCurl)
library(tm)
library(wordcloud)
library(RColorBrewer)
install.packages("gtools", dependencies = T)
library(gtools) # if problems calling library, install.packages("gtools", dependencies = T)
library(qdap) # qualitative data analysis package (it masks %>%)
library(Rgraphviz) # depict the terms within the tm package framework
library(SnowballC); library(RWeka); library(rJava); library(RWekajars)  # wordStem is masked from SnowballC
library(Rstem) # stemming terms as a link from R to Snowball C stemmer


consumer_key <- "wOj58vqE8onMQYAFBeAnUgHcC"
consumer_secret <- "8Hh8l7FWeZoXjHV4emQ4EwQ9yBFMZjIYhlAt15O5vP4rNfawMa"
access_token <- "32600877-SUEQzAuJDuZPEdAvorNzX3usuLAENHTPmGVk0eXuF"
access_secret <- "PA16VhIdjerpVprzcRD6bZl4YlXervAUlQSPeztvAyA70"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets <- twListToDF(userTimeline(user = "earlylightbalt",includeRts = FALSE,n=5000))

install.packages("gtools", dependencies = T)
library(gtools) # if problems calling library, install.packages("gtools", dependencies = T)
library(qdap) # qualitative data analysis package (it masks %>%)
library(Rgraphviz) # depict the terms within the tm package framework
library(SnowballC); library(RWeka); library(rJava); library(RWekajars)  # wordStem is masked from SnowballC
library(Rstem) # stemming terms as a link from R to Snowball C stemmer

tweets$text <- as.character(tweets$text)
tweets$text <- sapply(tweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
tweets$text <- tolower(tweets$text)
tweets$text<- tm::removeNumbers(tweets$text)
tweets$text <- str_replace_all(tweets$text, "  ", "") # replace double spaces with single space
tweets$text <- str_replace_all(tweets$text, pattern = "[[:punct:]]", " ")

tweets <- tbl_df(tweets)
tweets$month <- strftime(tweets$created,format="%m",tz = "UTC")
tweets$dayofweek <- strftime(tweets$created,format="%w",tz = "UTC")
tweets$hourofday <- strftime(tweets$created,format="%H",tz = "UTC")
tweets$year <- strftime(tweets$created,format="%Y",tz = "UTC")

#,col=favoriteCount,size=favoriteCount

dayofweek <- ggplot(tweets)+
        geom_jitter(aes(x=dayofweek,y=hourofday))+
        theme_bw() +
        # Set the entire chart region to a light gray color
        theme(panel.background=element_rect(fill="#F0F0F0")) +
        theme(plot.background=element_rect(fill="#F0F0F0")) +
        theme(panel.border=element_rect(colour="#F0F0F0")) +
        # Format the grid
        theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
        theme(axis.ticks=element_blank()) +
        # Dispose of the legend
        #theme(legend.position="none") +
        # Set title and axis labels, and format these and tick marks
        ggtitle("@earlylightbalt Tweets by day of week and hour of day") +
        theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=20)) +
        ylab("Hour of Day") +
        xlab("Day of Week") +
        theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
        theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
        theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
        theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
        # Big bold line at y=0
        geom_hline(yintercept=0,size=1.2,colour="#535353") +
        # Plot margins and finally line annotations
        theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) 
#annotate("text",x=as.Date("2016-05-12"),y=500,label="$300 Goal",colour="#008489") 
#annotate("text",x=25,y=7.5,label="Line 2",colour="#00bdc4")

ggsave(dayofweek,file="dayofweek.png",width = 10,height = 10)

daybase <- tbl_df(data.frame(seq.Date(from = as.Date('2014-01-01'),to = as.Date('2016-07-20'),by = 'day')))
names(daybase) <- 'date'
tweets$date <- as.Date(tweets$created,format="%Y-%m-%d")
day <- tbl_df(tweets)%>%group_by(date)%>%summarise(tweets=n(),fav = mean(favoriteCount),retweet = mean(retweetCount))

day <- left_join(daybase,day,by='date')
day[is.na(day)] <- 0 

day$ratio <- day$fav/day$tweets

hist(day$fav)
day$year <- strftime(day$date,format="%Y",tz = "UTC")
day$avFav <- movingAverage(day$fav,n=30,F)
day$avRetweet <- movingAverage(day$retweet,n=30,F)

plotDays <- ggplot(data=day,aes(x=date)) +
        geom_line(aes(y=avFav),size=1,colour = "#7fbf7f") +
        geom_line(aes(y=avRetweet),size=1.5,colour="#faaca7")+
        theme_bw() +
        # Set the entire chart region to a light gray color
        theme(panel.background=element_rect(fill="#F0F0F0")) +
        theme(plot.background=element_rect(fill="#F0F0F0")) +
        theme(panel.border=element_rect(colour="#F0F0F0")) +
        # Format the grid
        theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
        theme(axis.ticks=element_blank()) +
        # Dispose of the legend
        theme(legend.position="none") +
        # Set title and axis labels, and format these and tick marks
        ggtitle("@earlylightbalt tweets per day") +
        theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=20)) +
        ylab("Frequency") +
        xlab("Date") +
        theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
        theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
        theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
        theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
        # Big bold line at y=0
        geom_hline(yintercept=0,size=1.2,colour="#535353") +
        # Plot margins and finally line annotations
        theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) 
#annotate("text",x=as.Date("2016-05-12"),y=500,label="$300 Goal",colour="#008489") 
#annotate("text",x=25,y=7.5,label="Line 2",colour="#00bdc4")

ggsave(plotDays,file="days.png",width = 10,height = 10)

retweet <- tbl_df(tweets)%>%group_by(isRetweet)%>%summarise(tweets=n())

TorR<- ggplot(data=retweet,aes(x=isRetweet,y=tweets)) +
        geom_bar(stat="identity")+
        theme_bw() +
        # Set the entire chart region to a light gray color
        theme(panel.background=element_rect(fill="#F0F0F0")) +
        theme(plot.background=element_rect(fill="#F0F0F0")) +
        theme(panel.border=element_rect(colour="#F0F0F0")) +
        # Format the grid
        theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
        theme(axis.ticks=element_blank()) +
        # Dispose of the legend
        theme(legend.position="none") +
        # Set title and axis labels, and format these and tick marks
        ggtitle("#netDE Tweet or Retweet") +
        theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=20)) +
        ylab("Frequency") +
        xlab("type") +
        scale_x_discrete(breaks=c("FALSE", "TRUE"),
                         labels=c("Tweet", "Retweet"))+
        theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
        theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
        theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
        theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
        # Big bold line at y=0
        geom_hline(yintercept=0,size=1.2,colour="#535353") +
        # Plot margins and finally line annotations
        theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) 
#annotate("text",x=as.Date("2016-05-12"),y=500,label="$300 Goal",colour="#008489") 
#annotate("text",x=25,y=7.5,label="Line 2",colour="#00bdc4")

ggsave(TorR,file="TorT.png",width = 10,height = 10)

##Stolen Code

require(xts)
#The xts function creates a timeline from a vector of values and a vector of timestamps.
#If we know how many tweets we have, we can just create a simple list or vector containing that number of 1s
ts=xts(rep(1,times=nrow(tweets)),tweets$created)

#We can now do some handy number crunching on the timeseries, such as applying a formula to values contained with day, week, month, quarter or year time bins.
#So for example, if we sum the unit values in daily bin, we can get a count of the number of tweets per day
ts.sum=apply.daily(ts,sum) 
#also apply. weekly, monthly, quarterly, yearly

#If for any resason we need to turn the timeseries into a dataframe, we can:
#http://stackoverflow.com/a/3387259/454773
ts.sum.df=data.frame(date=index(ts.sum), coredata(ts.sum))

colnames(ts.sum.df)=c('date','sum')

#We can then use ggplot to plot the timeseries...
ggplot(ts.sum.df)+geom_line(aes(x=date,y=sum))
acf(ts.sum)




##############################################################################
#                        Calendar Heatmap                                    #
#                                by                                          #
#                         Paul Bleicher                                      #
# an R version of a graphic from:                                            #
# http://stat-computing.org/dataexpo/2009/posters/wicklin-allison.pdf        #
#  requires lattice, chron, grid packages                                    #
############################################################################## 

## calendarHeat: An R function to display time-series data as a calendar heatmap 
## Copyright 2009 Humedica. All rights reserved.

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You can find a copy of the GNU General Public License, Version 2 at:
## http://www.gnu.org/licenses/gpl-2.0.html

calendarHeat <- function(dates, 
                         values, 
                         ncolors=99, 
                         color="r2g", 
                         varname="Values",
                         date.form = "%Y-%m-%d", ...) {
        require(lattice)
        require(grid)
        require(chron)
        if (class(dates) == "character" | class(dates) == "factor" ) {
                dates <- strptime(dates, date.form)
        }
        caldat <- data.frame(value = values, dates = dates)
        min.date <- as.Date(paste(format(min(dates), "%Y"),
                                  "-1-1",sep = ""))
        max.date <- as.Date(paste(format(max(dates), "%Y"),
                                  "-12-31", sep = ""))
        dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
        
        # Merge moves data by one day, avoid
        caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
        dates <- as.Date(dates) 
        caldat$value[match(dates, caldat$date.seq)] <- values
        
        caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
        caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
        caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
        caldat$month <- as.numeric(format(caldat$date.seq, "%m"))
        yrs <- as.character(unique(caldat$yr))
        d.loc <- as.numeric()                        
        for (m in min(yrs):max(yrs)) {
                d.subset <- which(caldat$yr == m)  
                sub.seq <- seq(1,length(d.subset))
                d.loc <- c(d.loc, sub.seq)
        }  
        caldat <- cbind(caldat, seq=d.loc)
        
        #color styles
        r2b <- c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020") #red to blue                                                                               
        r2g <- c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384")   #red to green
        w2b <- c("#045A8D", "#2B8CBE", "#74A9CF", "#BDC9E1", "#F1EEF6")   #white to blue
        
        assign("col.sty", get(color))
        calendar.pal <- colorRampPalette((col.sty), space = "Lab")
        def.theme <- lattice.getOption("default.theme")
        cal.theme <-
                function() {  
                        theme <-
                                list(
                                        strip.background = list(col = "transparent"),
                                        strip.border = list(col = "transparent"),
                                        axis.line = list(col="transparent"),
                                        par.strip.text=list(cex=0.8))
                }
        lattice.options(default.theme = cal.theme)
        yrs <- (unique(caldat$yr))
        nyr <- length(yrs)
        print(cal.plot <- levelplot(value~woty*dotw | yr, data=caldat,
                                    as.table=TRUE,
                                    aspect=.12,
                                    layout = c(1, nyr%%7),
                                    between = list(x=0, y=c(1,1)),
                                    strip=TRUE,
                                    main = paste("Calendar Heat Map of ", varname, sep = ""),
                                    scales = list(
                                            x = list(
                                                    at= c(seq(2.9, 52, by=4.42)),
                                                    labels = month.abb,
                                                    alternating = c(1, rep(0, (nyr-1))),
                                                    tck=0,
                                                    cex = 0.7),
                                            y=list(
                                                    at = c(0, 1, 2, 3, 4, 5, 6),
                                                    labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                                               "Friday", "Saturday"),
                                                    alternating = 1,
                                                    cex = 0.6,
                                                    tck=0)),
                                    xlim =c(0.4, 54.6),
                                    ylim=c(6.6,-0.6),
                                    cuts= ncolors - 1,
                                    col.regions = (calendar.pal(ncolors)),
                                    xlab="" ,
                                    ylab="",
                                    colorkey= list(col = calendar.pal(ncolors), width = 0.6, height = 0.5),
                                    subscripts=TRUE
        ) )
        panel.locs <- trellis.currentLayout()
        for (row in 1:nrow(panel.locs)) {
                for (column in 1:ncol(panel.locs))  {
                        if (panel.locs[row, column] > 0)
                        {
                                trellis.focus("panel", row = row, column = column,
                                              highlight = FALSE)
                                xyetc <- trellis.panelArgs()
                                subs <- caldat[xyetc$subscripts,]
                                dates.fsubs <- caldat[caldat$yr == unique(subs$yr),]
                                y.start <- dates.fsubs$dotw[1]
                                y.end   <- dates.fsubs$dotw[nrow(dates.fsubs)]
                                dates.len <- nrow(dates.fsubs)
                                adj.start <- dates.fsubs$woty[1]
                                
                                for (k in 0:6) {
                                        if (k < y.start) {
                                                x.start <- adj.start + 0.5
                                        } else {
                                                x.start <- adj.start - 0.5
                                        }
                                        if (k > y.end) {
                                                x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] - 0.5
                                        } else {
                                                x.finis <- dates.fsubs$woty[nrow(dates.fsubs)] + 0.5
                                        }
                                        grid.lines(x = c(x.start, x.finis), y = c(k -0.5, k - 0.5), 
                                                   default.units = "native", gp=gpar(col = "grey", lwd = 1))
                                }
                                if (adj.start <  2) {
                                        grid.lines(x = c( 0.5,  0.5), y = c(6.5, y.start-0.5), 
                                                   default.units = "native", gp=gpar(col = "grey", lwd = 1))
                                        grid.lines(x = c(1.5, 1.5), y = c(6.5, -0.5), default.units = "native",
                                                   gp=gpar(col = "grey", lwd = 1))
                                        grid.lines(x = c(x.finis, x.finis), 
                                                   y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                                                   gp=gpar(col = "grey", lwd = 1))
                                        if (dates.fsubs$dotw[dates.len] != 6) {
                                                grid.lines(x = c(x.finis + 1, x.finis + 1), 
                                                           y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                                                           gp=gpar(col = "grey", lwd = 1))
                                        }
                                        grid.lines(x = c(x.finis, x.finis), 
                                                   y = c(dates.fsubs$dotw[dates.len] -0.5, -0.5), default.units = "native",
                                                   gp=gpar(col = "grey", lwd = 1))
                                }
                                for (n in 1:51) {
                                        grid.lines(x = c(n + 1.5, n + 1.5), 
                                                   y = c(-0.5, 6.5), default.units = "native", gp=gpar(col = "grey", lwd = 1))
                                }
                                x.start <- adj.start - 0.5
                                
                                if (y.start > 0) {
                                        grid.lines(x = c(x.start, x.start + 1),
                                                   y = c(y.start - 0.5, y.start -  0.5), default.units = "native",
                                                   gp=gpar(col = "black", lwd = 1.75))
                                        grid.lines(x = c(x.start + 1, x.start + 1),
                                                   y = c(y.start - 0.5 , -0.5), default.units = "native",
                                                   gp=gpar(col = "black", lwd = 1.75))
                                        grid.lines(x = c(x.start, x.start),
                                                   y = c(y.start - 0.5, 6.5), default.units = "native",
                                                   gp=gpar(col = "black", lwd = 1.75))
                                        if (y.end < 6  ) {
                                                grid.lines(x = c(x.start + 1, x.finis + 1),
                                                           y = c(-0.5, -0.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                                grid.lines(x = c(x.start, x.finis),
                                                           y = c(6.5, 6.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                        } else {
                                                grid.lines(x = c(x.start + 1, x.finis),
                                                           y = c(-0.5, -0.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                                grid.lines(x = c(x.start, x.finis),
                                                           y = c(6.5, 6.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                        }
                                } else {
                                        grid.lines(x = c(x.start, x.start),
                                                   y = c( - 0.5, 6.5), default.units = "native",
                                                   gp=gpar(col = "black", lwd = 1.75))
                                }
                                
                                if (y.start == 0 ) {
                                        if (y.end < 6  ) {
                                                grid.lines(x = c(x.start, x.finis + 1),
                                                           y = c(-0.5, -0.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                                grid.lines(x = c(x.start, x.finis),
                                                           y = c(6.5, 6.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                        } else {
                                                grid.lines(x = c(x.start + 1, x.finis),
                                                           y = c(-0.5, -0.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                                grid.lines(x = c(x.start, x.finis),
                                                           y = c(6.5, 6.5), default.units = "native",
                                                           gp=gpar(col = "black", lwd = 1.75))
                                        }
                                }
                                for (j in 1:12)  {
                                        last.month <- max(dates.fsubs$seq[dates.fsubs$month == j])
                                        x.last.m <- dates.fsubs$woty[last.month] + 0.5
                                        y.last.m <- dates.fsubs$dotw[last.month] + 0.5
                                        grid.lines(x = c(x.last.m, x.last.m), y = c(-0.5, y.last.m),
                                                   default.units = "native", gp=gpar(col = "black", lwd = 1.75))
                                        if ((y.last.m) < 6) {
                                                grid.lines(x = c(x.last.m, x.last.m - 1), y = c(y.last.m, y.last.m),
                                                           default.units = "native", gp=gpar(col = "black", lwd = 1.75))
                                                grid.lines(x = c(x.last.m - 1, x.last.m - 1), y = c(y.last.m, 6.5),
                                                           default.units = "native", gp=gpar(col = "black", lwd = 1.75))
                                        } else {
                                                grid.lines(x = c(x.last.m, x.last.m), y = c(- 0.5, 6.5),
                                                           default.units = "native", gp=gpar(col = "black", lwd = 1.75))
                                        }
                                }
                        }
                }
                trellis.unfocus()
        } 
        lattice.options(default.theme = def.theme)
}

png(file="thm.png")
twitterheatmap <- calendarHeat(ts.sum.df$date, ts.sum.df$sum, varname="@earlylightbalt Twitter activity")
dev.off()




text <- tweets$text
chars_per_tweet = sapply(text, nchar)
summary(chars_per_tweet)

# split words
words_list = strsplit(text, " ")

# words per tweet
words_per_tweet = sapply(words_list, length)
# barplot
barplot(table(words_per_tweet), border=NA,
        main="Distribution of words per tweet", cex.main=1)

# most frequent words
mfw = sort(table(unlist(words_list)), decreasing=TRUE)

# top-20 most frequent
top20 = head(mfw, 20)

# barplot
barplot(top20, border=NA, las=2, main="Top 20 most frequent terms", cex.main=1)




dwy<- ggplot(tweets)+
        geom_jitter(aes(x=dayofweek,y=hourofday,col=favoriteCount,size=favoriteCount))+
        facet_grid(.~year)+
        theme_bw() +
        # Set the entire chart region to a light gray color
        theme(panel.background=element_rect(fill="#F0F0F0")) +
        theme(plot.background=element_rect(fill="#F0F0F0")) +
        theme(panel.border=element_rect(colour="#F0F0F0")) +
        # Format the grid
        theme(panel.grid.major=element_line(colour="#D0D0D0",size=.75)) +
        theme(axis.ticks=element_blank()) +
        # Dispose of the legend
        #theme(legend.position="none") +
        # Set title and axis labels, and format these and tick marks
        ggtitle("@earlylightbalt Tweets by day of week and hour of day by year") +
        theme(plot.title=element_text(face="bold",hjust=0,vjust=2,colour="#3C3C3C",size=20)) +
        ylab("Hour of Day") +
        xlab("Day of Week") +
        theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
        theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
        theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
        theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5)) +
        # Big bold line at y=0
        geom_hline(yintercept=0,size=1.2,colour="#535353") +
        # Plot margins and finally line annotations
        theme(plot.margin = unit(c(1, 1, .5, .7), "cm")) 
#annotate("text",x=as.Date("2016-05-12"),y=500,label="$300 Goal",colour="#008489") 
#annotate("text",x=25,y=7.5,label="Line 2",colour="#00bdc4")

ggsave(dwy,file="dwy.png",width = 10,height = 10)
