waterstats<-data.frame(reproductivestatus=levels(waterstats$status), percentbodyweightlostdaily=(tapply(waterstats$avgdailypercent, waterstats$status, mean)),n=tapply(waterstats$avgdailypercent, waterstats$status, length),sd=tapply(waterstats$avgdailypercent, waterstats$status, sd))

#calculate the standard error from the standard deviation and add it to the dataframe we made
waterstats$sem<-waterstats$sd/sqrt(waterstats$n)

#get ready to graph it
require(ggplot2)

#this line is going to make our "equation" to plot
#it is naming our x and y variables
# fill=treatment is saying that we want to color code it by the treatment
Barplotwaterstats<- ggplot(waterstats, aes(x=reproductivestatus, y=percentbodyweightlostdaily, fill=reproductivestatus))

#now we are actually going to take that equation and add the elements of our plot to it
#i am going to change the name so we can maintain the original equation if we change our mind about the plot
Barplotwaterstats<- Barplotwaterstats +
  geom_bar(position=position_dodge(), stat="identity") + #this line of code says we want to make a bar plot, it is going to "doge" the bars meaning place the next to each other instead of stacking them or something
  geom_errorbar(aes(ymin=percentbodyweightlostdaily-waterstats$sem, ymax=percentbodyweightlostdaily+waterstats$sem)) + #this adds error bars and specifies the values of them from the standard error we calculated
  labs( #designates the labels for your graph
    list( #it takes a list as an argument
      x="Reproductive Status",  # this first element of the list creates your x axis label
      y="Percent Body Weight Lost Daily")  #creates y axis label
  ) + #this closes the labs element
  theme( #theme allows you to dictate all aspects of your graph in the following lines
    panel.background=element_rect(fill="white"), #changes the background of the plot to white
    axis.line=element_line(size=.5,colour="black"),  #makes the axis lines black and thickness of .5
    text=element_text(colour="black",size=20, family="Times")  #lets you change text elements like color size and font
  ) + #this parentheses is closing the theme assignment
  scale_fill_discrete( #this is going to further designate color breaks for your data and creates a legend for it
    name="Reproductive Status", #this is going to give your legent a title
    breaks=c("EL","EV","LL","LV"), #this is a list of the different factor names you are coloring based on
    labels=c("EL", "EV", "LL", "LV") #this is what those names actually mean because you generally use a shorthand or abbreviation in your data
  ) + #this parentheses closes the scale_fill_discrete
  scale_y_continuous(expand = c(0, 0))+ #this removes the extra space between the bars and the xaxis line
  scale_x_discrete(  #this will let you change the labels on the actual tick-marks on your x-axis. it works similarly to the scale_fill_discrete
    breaks=c("EL","EV","LL","LV"), #this sets the breaks
    labels=c("EL", "EV", "LL", "LV") #what you actually want it to say
  ) #this closes the scale_x_discrete
#note there are no more plus signs at the end of the line so our command is complete.

Barplotwaterstats #running this line should show you your plot
