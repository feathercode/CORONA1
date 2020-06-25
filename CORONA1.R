library(shiny) # to make any Shiny app
library(shinydashboard) # to enable a fixed sidebar
library(seewave) # for filtering 
library(ggplot2) # for better plots
library(viridis) # for better colours
library(zoo) # for interpolation
library(gridExtra) # for side-by-side ggplots

# ??? dyn.load("xf_filter_bworth1_d_R.so")

s1="https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
s2=list.files(pattern="covid_2020*")
s3=tail(s2,1)

# rolling average function
f_rolling <- function(x,n){filter(x, rep(1/(1*n),1*n), sides=2)}

################################################################################
# User interface
################################################################################
ui <- dashboardPage(
  dashboardHeader(
    title = "Coronavirus Tracker"
  ), 
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      style = "position: fixed; overflow: visible;",
      selectInput("setsource", "Pick a data source",c(s1,s2), selected=s3),
      selectInput("setmindeath", "Min.cumulative deaths",c(0,1,5,10,100),selected=5),
      sliderInput("setmaxweeks", "Max. weeks to display",0,52,0),
      radioButtons("setsmooth","Smoothing",c("Raw data"="no","RollingAverage"="yes1","Butterworth Filter"="yes2"),selected="no"),
      uiOutput('newcontrols')
    )
  ),

  body=dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
    h4('File Preview',align="center"), 
    tableOutput("preview1"),
    tableOutput("preview2"),
    h4('Cases Versus Deaths',align="center"),
    plotOutput("plot1"),
    plotOutput("plot2")
  )
)


################################################################################
# server-side function
################################################################################
server <- function(input, output, session) {
    

  # read & clean the data - reactive to selecting of input 
  df0 <-  reactive({
    req(input$setsource)
    dfx <- read.csv(input$setsource, na.strings = "", fileEncoding = "UTF-8-BOM")
    if (!is.data.frame(dfx)) { validate(paste0("'", input$dataset, "' is not a data frame"))}
    # rename some columns
    names(dfx)[names(dfx)=="deaths"] <- "Deaths"
    names(dfx)[names(dfx)=="cases"] <- "Cases"
    names(dfx)[names(dfx)=="countriesAndTerritories"] <- "Country"
    # make a new Date variable
    dfx$Date= as.Date(paste(dfx$year,dfx$month,dfx$day,sep="-"))
    # remove negative corrections to cases and deaths and interpolate 
    dfx$Cases[dfx$Cases<0]= NaN
    dfx$Deaths[dfx$Deaths<0]= NaN
    dfx$Cases= na.fill(dfx$Cases, "extend")
    dfx$Deaths= na.fill(dfx$Deaths, "extend")
    # return only a subset of columns
    return(subset(dfx,select= c(Country,dateRep,Date,Cases,Deaths)))
  })


  # create variable-selection controls on-the-fly with renderUI
  output$newcontrols <- renderUI({
    a= df0()$Country
    b= unique(a)
    list(
      selectInput("setcountry1", "Select Country #1",b,selected="United_Kingdom"),
      selectInput("setcountry2", "Select Country #2",b,selected="France")
    )
  })
  
  # build the data frame for country-1
  df1 <- reactive({
    req(input$setcountry1)
    req(input$setmindeath)
    req(input$setmaxweeks)
    req(input$setsmooth)
    mindeath= as.numeric(input$setmindeath)
    maxdays= as.numeric(7*input$setmaxweeks)
    # define the temporary dataframe 
    dfx= subset(df0(),Country==input$setcountry1)
    dfx= dfx[order(dfx$Date),]
    dfx$Weekday= weekdays(as.POSIXct(dfx$Date))
    dfx$DeathsSum= cumsum(dfx$Deaths) # calculate unfiltered cumulative deaths
    dfx= subset(dfx,DeathsSum>=mindeath) # use cumulative-deaths to restrict the dataset
    if(maxdays==0) maxdays=nrow(dfx) # allow plotting up to current date
    npad= maxdays-nrow(dfx) # calculate required padding
    z=nrow(dfx) # determine the current number of rows
    # apply smoothing
    if(input$setsmooth=="yes1") {
      dfx$Cases= f_rolling(dfx$Cases,7)
      dfx$Deaths= f_rolling(dfx$Deaths,7)
      dfx$Cases= na.fill(dfx$Cases, "extend")
      dfx$Deaths= na.fill(dfx$Deaths, "extend")
    }
    if(input$setsmooth=="yes2") {
      dfx$Cases= bwfilter(as.numeric(dfx$Cases), f=7, n=3, bandpass=F, from=.5)
      dfx$Deaths= bwfilter(as.numeric(dfx$Deaths), f=7, n=3, bandpass=F, from=.5)
    }
    # if necessary, build data frame to expand dfx to fill the day-count - then bind it to dfx
    if(npad>0) {
      lastdate= tail(dfx$Date,1)
      lastdaterep= tail(dfx$dateRep,1)
      dfappend= setNames(data.frame(matrix(0,ncol=ncol(dfx),nrow=npad)),names(dfx))
      dfappend$Date= seq.Date(lastdate+1,to=lastdate+npad,by=1)
      dfappend$dateRep= as.factor(lastdaterep)
      dfappend$Country= input$setcountry1
      dfx= rbind(dfx,dfappend)
      z= maxdays
    }
    # now add a day-count and return the dataset, restricted by day=-count
    dfx$dayCount= 1:z
    return(subset(dfx,dayCount<=maxdays))
  })
  
  
  # build the data frame for country-2
  df2 <- reactive({
    req(input$setcountry2)
    req(input$setmindeath)
    req(input$setmaxweeks)
    req(input$setsmooth)
    mindeath= as.numeric(input$setmindeath)
    maxdays= as.numeric(7*input$setmaxweeks)
    # define the temporary dataframe 
    dfx= subset(df0(),Country==input$setcountry2)
    dfx= dfx[order(dfx$Date),]
    dfx$Weekday= weekdays(as.POSIXct(dfx$Date))
    dfx$DeathsSum= cumsum(dfx$Deaths) # calculate unfiltered cumulative deaths
    dfx= subset(dfx,DeathsSum>=mindeath) # use cumulative-deaths to restrict the dataset
    if(maxdays==0) maxdays=nrow(dfx) # allow plotting up to current date
    npad= maxdays-nrow(dfx) # calculate required padding
    z=nrow(dfx) # determine the current number of rows
    # apply smoothing
    if(input$setsmooth=="yes1") {
      dfx$Cases= f_rolling(dfx$Cases,7)
      dfx$Deaths= f_rolling(dfx$Deaths,7)
      dfx$Cases= na.fill(dfx$Cases, "extend")
      dfx$Deaths= na.fill(dfx$Deaths, "extend")
    }
    if(input$setsmooth=="yes2") {
      dfx$Cases= bwfilter(as.numeric(dfx$Cases), f=7, n=3, bandpass=F, from=.5)
      dfx$Deaths= bwfilter(as.numeric(dfx$Deaths), f=7, n=3, bandpass=F, from=.5)
    }
    # if necessary, build data frame to expand dfx to fill the day-count - then bind it to dfx
    if(npad>0) {
      lastdate= tail(dfx$Date,1)
      lastdaterep= tail(dfx$dateRep,1)
      dfappend= setNames(data.frame(matrix(0,ncol=ncol(dfx),nrow=npad)),names(dfx))
      dfappend$Date= seq.Date(lastdate+1,to=lastdate+npad,by=1)
      dfappend$dateRep= as.factor(lastdaterep)
      dfappend$Country= input$setcountry2
      dfx= rbind(dfx,dfappend)
      z= maxdays
    }
    # now add a day-count and return the dataset, restricted by day=-count
    dfx$dayCount= 1:z
    return(subset(dfx,dayCount<=maxdays))
  })

  
  # data preview #################################################################
  output$preview1 <- renderTable({
    tail(subset(df1(),select= c(Country,dateRep,Cases,Deaths,DeathsSum)),5)
  })
  output$preview2 <- renderTable({
    tail(subset(df2(),select= c(Country,dateRep,Cases,Deaths,DeathsSum)),5)
  })
  
  
  # plot1 ########################################################################
  output$plot1 <- renderPlot({
    req(input$setcountry1)
    req(input$setcountry2)
    
    o1= scale_fill_manual(values=c("#8888dd", "#990000"))
    o2= theme(axis.text.x = element_text(angle = 90))
    o3= scale_x_date(date_breaks = "weeks" , date_labels = "%b-%d")
    o4= theme(legend.position=c(.1,.9),legend.title=element_blank())
    
    # country 1 
    setcountry=input$setcountry1
    dfx= df1()
    ratio1= max(dfx$Deaths)/max(dfx$Cases)
    p1=ggplot(dfx) + 
      aes(x=Date) +
      geom_area(mapping=aes(y=Cases*ratio1,fill="Cases.norm"),alpha=0.5) +
      geom_area(mapping=aes(x=Date,y=Deaths,fill="Deaths"),alpha=0.5) + 
      ggtitle(setcountry) +
      o1 + o2 + o3 + o4 

    # country 2
    setcountry=input$setcountry2
    dfx= df2()
    ratio2= max(dfx$Deaths)/max(dfx$Cases)
    p2=ggplot(dfx) + 
      aes(x=Date) +
      geom_area(mapping=aes(y=Cases*ratio2,fill="Cases.norm"),alpha=0.5) +
      geom_area(mapping=aes(x=Date,y=Deaths,fill="Deaths"),alpha=0.5) + 
      ggtitle(setcountry) +
      o1 + o2 + o3 + o4 
    
    return(grid.arrange(p1,p2,ncol=2))
  })
  

}
shinyApp(ui, server)
