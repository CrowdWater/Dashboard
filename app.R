library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(rdrop2)
library(V8)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(lubridate)
# setwd('Z:/group/h2k-data/Projects/CrowdWater/Oeffentlichkeitsarbeit/Homepage/DataDashboard/CrowdWaterDashboard')
source('./CW_API_Download.R')
# icons: http://rstudio.github.io/shinydashboard/appearance.html
# the javascript code to refresh the entire page.
jscode <- "shinyjs.refresh = function() { history.go(0); }"
# Define UI for application ----
ui <- dashboardPage(
  dashboardHeader(titleWidth = 12,title = "CrowdWater Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall",tabName = "sb_stats", icon = icon("dashboard")),
      menuItem("Virtual Staff Gauge",tabName = 'sb_wl_stats',icon = icon("dashboard")),
      menuItem("Soil Moisture",tabName = 'sb_sm_stats',icon = icon("dashboard")),
      menuItem("Temporary Stream" ,tabName = 'sb_ts_stats',icon = icon("dashboard")),
      menuItem("Physical Staff Gauge",tabName = 'sb_ps_stats',icon = icon("dashboard")),
      menuItem("Plastic Pollution",tabName = 'sb_pp_stats',icon = icon("dashboard")),
      menuItem("Stream Type",tabName = 'sb_st_stats',icon = icon("dashboard")),
      menuItem("Contribution Plots", tabName = "sb_plots",icon = icon("chart-line")),
      menuItem("Monthly active users", tabName = "sb_mauPlots",icon = icon("chart-line")),
      menuItem("Citizen Scientist Plots", tabName = "sb_citsciplots",icon = icon("chart-line")),
      menuItem("Explore a Spot", tabName = "sb_explore", icon = icon("search")),
      menuItem("About", tabName = "about",icon = icon("info-circle"))
    )),
  dashboardBody(shinyjs::useShinyjs(),
                extendShinyjs(text = jscode, functions = c("winprint")),# the javascript code to refresh the entire page. #
                ##FUNCTIONS!!!!
                tags$head(tags$style(HTML(".small-box {height: 110px}"))),
                # includeCSS("www/bootstrap4CWDashboard.css"),
                # # add custom JS code to disable the header, however then the switching between sidebartabs is also disabled...
                # shinyjs::extendShinyjs(text = "shinyjs.hidehead = function(parm){
                #                     $('header').css('display', parm);
                #                 }"),
                tabItems(
                  tabItem(tabName="sb_stats",
                          fluidRow(
                            valueBoxOutput(width = 6,"TotnContribs") %>% withSpinner(color='#7dbdeb'),
                            valueBoxOutput(width = 6,"uqRootsSpots")),
                          fluidRow(valueBoxOutput(width = 4,"nWLContribs"),
                                   valueBoxOutput(width = 4,"nSMContribs"),
                                   valueBoxOutput(width = 4,"nTSContribs")),
                          fluidRow(valueBoxOutput(width = 4,"nPSContribs"),
                                   valueBoxOutput(width = 4,"nPLContribs"),
                                   valueBoxOutput(width = 4,"nSTContribs")),
                          fluidRow(box(width = 6,title = NULL,uiOutput('sliderAll')), # see output$sliderAll = renderUI() in server()
                                   valueBoxOutput(width=3,"stationsWithXcontribs"),
                                   valueBoxOutput(width=3,"UsersWithXcontribs")
                          ),
                          fluidPage(box(width = 12,title = "Leaflet Heatmap of CrowdWater spots",
                                        leafletOutput("heatmap_heatmap"))
                          )#,
                          # --> useless (because this way its only global)
                          # fluidPage(box(width = 12,title = "Kernel Heatmap of CrowdWater spots",
                          #                           leafletOutput("heatmap_kernel")))
                  ),
                  tabItem(tabName="sb_wl_stats",
                          fluidRow(valueBoxOutput(width=3,"nWLRootSpots")%>% withSpinner(color='#7dbdeb')),
                          fluidRow(box(width = 6,title=NULL,uiOutput('sliderWL')),
                                   valueBoxOutput(width = 3,"WLStationsWithXContribs"),
                                   valueBoxOutput(width = 3,"UsersWithXWLcontribs"))
                  ),
                  
                  tabItem(tabName="sb_sm_stats",
                          fluidRow(valueBoxOutput(width=3,"nSMRootSpots")%>% withSpinner(color='#7dbdeb')),
                          fluidRow(box(width = 6,title=NULL,uiOutput('sliderSM')),
                                   valueBoxOutput(width = 3,"SMStationsWithXContribs"),
                                   valueBoxOutput(width = 3,"UsersWithXSMcontribs"))
                  ),
                  tabItem(tabName="sb_ts_stats",
                          fluidRow(valueBoxOutput(width=3,"nTSRootSpots")%>% withSpinner(color='#7dbdeb')),
                          fluidRow(box(width = 6,title=NULL,uiOutput('sliderInterStr')),
                                   valueBoxOutput(width = 3,"IntStrStationsWithXContribs"),
                                   valueBoxOutput(width = 3,"UsersWithXTScontribs"))
                  ),
                  tabItem(tabName="sb_ps_stats",
                          fluidRow(valueBoxOutput(width=3,"nPSRootSpots")%>% withSpinner(color='#7dbdeb')),
                          fluidRow(box(width = 6,title=NULL,uiOutput('sliderPS')),
                                   valueBoxOutput(width = 3,"PSStationsWithXContribs"),
                                   valueBoxOutput(width = 3,"UsersWithXPScontribs"))
                  ),
                  tabItem(tabName="sb_pp_stats",
                          fluidRow(valueBoxOutput(width=3,"nPPRootSpots")%>% withSpinner(color='#7dbdeb')),
                          fluidRow(box(width = 6,title=NULL,uiOutput('sliderPP')),
                                   valueBoxOutput(width = 3,"PPStationsWithXContribs"),
                                   valueBoxOutput(width = 3,"UsersWithXPPcontribs"))
                  ),
                  tabItem(tabName="sb_st_stats",
                          fluidRow(valueBoxOutput(width=3,"nSTRootSpots")%>% withSpinner(color='#7dbdeb')),
                          fluidRow(box(width = 6,title=NULL,uiOutput('sliderST')),
                                   valueBoxOutput(width = 3,"STStationsWithXContribs"),
                                   valueBoxOutput(width = 3,"UsersWithXSTcontribs"))
                  ),
                  
                  tabItem(tabName="sb_plots",
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative CrowdWater Contributions",    
                                plotOutput("cumsumplot", height = "500px") %>% withSpinner(color='#7dbdeb'))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Virtual Staff Gauge Contributions",    
                                plotOutput("cumsumplotWL", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Soil Moisture contributions",    
                                plotOutput("cumsumplotSM", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Temporary Stream contributions",    
                                plotOutput("cumsumplotTS", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Physical Staff Gauge contributions",    
                                plotOutput("cumsumplotPS", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Plastic Pollution contributions",    
                                plotOutput("cumsumplotPP", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Stream Type contributions",    
                                plotOutput("cumsumplotST", height = "500px"))
                          )
                  ),
                  tabItem(tabName="sb_citsciplots",
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative CrowdWater Contributors",    
                                plotOutput("cumsumplotUsers", height = "500px") %>% withSpinner(color='#7dbdeb'))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Virtual Staff Gauge Contributors",    
                                plotOutput("cumsumplotUsersWL", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Soil Moisture Contributors",    
                                plotOutput("cumsumplotUsersSM", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Temporary Stream Contributors",    
                                plotOutput("cumsumplotUsersTS", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Pysical Staff Gauge Contributors",    
                                plotOutput("cumsumplotUsersPS", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Plastic Pollution Contributors",    
                                plotOutput("cumsumplotUsersPP", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Cumulative Stream Type Contributors",    
                                plotOutput("cumsumplotUsersST", height = "500px"))
                          )
                  ),
                  tabItem(tabName="sb_mauPlots",
                          fluidRow(
                            box(width = 12,    
                                title = "Monthly active contributors",    
                                plotOutput("mauPlotAll", height = "500px") %>% withSpinner(color='#7dbdeb'))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Monthly active virtual staff gauge contributors",    
                                plotOutput("mauPlotWL", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Monthly active soil moisture contributors",    
                                plotOutput("mauPlotSM", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Monthly active temporary stream contributors",    
                                plotOutput("mauPlotTS", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Monthly active physical staff gauge contributors",    
                                plotOutput("mauPlotPS", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Monthly active plastic pollution contributors",    
                                plotOutput("mauPlotPP", height = "500px"))
                          ),
                          fluidRow(
                            box(width = 12,    
                                title = "Monthly active stream type contributors",    
                                plotOutput("mauPlotST", height = "500px"))
                          )
                  ),
                  tabItem(tabName = "sb_explore", # explore Tab----
                          fluidPage(
                            box(width = 3,footer = tags$div(class = "submit",
                                                            tags$a(href = "https://www.spotteron.com/crowdwater", 
                                                                   "IDs can be found in the browser link of spots on the CrowdWater map.", 
                                                                   target="_blank")
                            ),title="Enter the ID of the Spot that you want to explore",textInput("stationID", "", "105679"), actionButton("expl_btn","Show me the Spot")),
                            box(width = 6,leafletOutput("expl_spotMap")),
                            valueBoxOutput(width=3, "expl_nrOfContribs"), br(),
                            valueBoxOutput(width = 3,"expl_contrPerDay"),
                            p(),
                            br(),
                            box(width = 12, title = "Timeline of contributions",    
                                plotOutput("expl_timelinePlot", height = "500px")),
                            br(),
                            # box(width=1,title = "Root Spot Number", textOutput("expl_statRootID")),
                            
                            box(width=12, title = "Image of this Spot", htmlOutput("expl_thisImg"),htmlOutput("expl_date_thisImg"),collapsible = T, collapsed = T),br(),
                            box(widht=12, title = "Root Spot Image", htmlOutput("expl_rootImg"),htmlOutput("expl_date_rootImg"),collapsible = T, collapsed = T),br(),
                            box(widht=12, title = "Latest Image", htmlOutput("expl_latestImg"),htmlOutput("expl_date_latestImg"),collapsible = T, collapsed = T)
                          )),
                  tabItem(tabName = "about",
                          fluidPage(
                            img(src='Logo_Crowdwater_pos.png', width = "240px"),
                            HTML('<br><br>This Dashboard was created for the <a href="www.crowdwater.ch">CrowdWater</a> project at the <a href="www.geo.uzh.ch">Department of Geography</a> of the <a href="www.uzh.ch">University of Zurich</a> in <a href="www.cran.org">R</a> and <a href="https://rstudio.com/">RStudio</a> and runs on <a href="www.shinyapps.io">Shinyapps.io</a> <br>'),
                            HTML('The CrowdWater project was funded by the <a href="http://www.snf.ch">Swiss National Science Foundation</a> <br>'),
                            HTML('Click <a href="https://crowdwater.shinyapps.io/CrowdWaterDashboard/"target="_blank">here</a> for the fullscreen version. <br>'),
                            HTML('&#169; Simon Etter, the code can be found on my <a href="https://github.com/SimonEtter/CrowdWaterDashboard"target="_blank">GitHub</a> account.<br><br><br>'),
                            img(src='uzh_logo_e_pos.png', width = "200px"),HTML('<br><br>'),img(src='SNF_RGB_E_POS.png', width = "200px"),
                            br(),br(),
                            fluidRow(actionButton("reloadAllCWdata","Refresh Dashboard from CrowdWater servers (might take a while)",icon = icon("download"))))
                  )
                )
  )
)



# Define server logic ----
server <- function(input, output, session) {
  # js$hidehead('none')    # would be to hide the header, but that also disables the sidebar controls, therefore not used
  
  # Re-Download Data - button in the about page ----
  # this is for the button in the about page:
  # deletes and re-downloads the entire dataset
  locFile4Attempt = 'CW_Data.csv'
  observeEvent(input$reloadAllCWdata,{
    CWdataFull = Download_AllCWdata_from_API()
    colnames(CWdataFull)[1]='Spot_ID'
    write.csv(CWdataFull,file=paste0("CWdata/",locFile4Attempt),row.names = F)
    js$refresh() # reloads the entire app with the new data
  })
  ############################################################################################################
  
  # Determine what data needs to be downloaded----
  # path to store the used the dateseries with the new dates appended
  fp_oldDateSeries = "CWdata/oldDateSeries"
  
  # if previous CrowdWater data exists then append the new contributions 
  # else download all contributions again and store them as CW_Data_2.csv
  if(file.exists(paste0("CWdata/",locFile4Attempt))){
    CWdataFull = read.csv(paste0("CWdata/",locFile4Attempt),stringsAsFactors = F)
    latestUpdate = CWdataFull$created_at[nrow(CWdataFull)]      #####change!!! for first time with new categories##############
    newCWdata = Download_LatestCWdata_from_API(lastDate = latestUpdate)
    if(!is.null(newCWdata)){
      colnames(newCWdata)[1]='Spot_ID'
      CWdataFull$created_at = ymd_hms(CWdataFull$created_at,tz='GMT')
      newCWdata$created_at = ymd_hms(newCWdata$created_at,tz='GMT')
      #CWdataFull = rbind(CWdataFull,newCWdata)
      CWdataFull = dplyr::bind_rows(CWdataFull,newCWdata) #should work with this in the final version!
      
      # write.csv(CWdataFull,file=paste0("CWdata/",locFile4Attempt),row.names = F)
      
      #newStartDate = ymd_hms(newCWdata$created_at[1])
      newStartDate = ymd_hms(paste0(as_date(ymd_hms(newCWdata$created_at[1])+days(1))," 00:00:00")) #???
      newEndDate =  ymd_hms(newCWdata$created_at[nrow(newCWdata)])
      if (newStartDate>newEndDate) {
        newStartDate = newEndDate
      }
      newDateSeries = seq(from=newStartDate,to=newEndDate,by='1 day')
      attr(newDateSeries,"tzone") = 'GMT'
      
      #newDateSeries = newDateSeries[!newDateSeries %in% dateSeries]
    }else{
      # if there were no new contributions to be downloaded, then make the newDateSeries a NULL-variable
      newDateSeries = NULL
      print("no new contributions since the app was opened last")
    }
  }else{
    CWdataFull = Download_AllCWdata_from_API()
    colnames(CWdataFull)[1]='Spot_ID'
    CWdataFull$created_at = ymd_hms(CWdataFull$created_at,tz='GMT')
    write.csv(CWdataFull,file=paste0("CWdata/",locFile4Attempt),row.names = F)
    
    # the start date is approximately when the app was officially launched, there are some pics from earlier dates that were added manually by us
    #dateSeries = seq(from=ymd_hms("2017-01-01 00:00:00 GMT"),to=CWdataFull$created_at[nrow(CWdataFull)]+3600,by='1 day')
    dateSeries = seq(from=ymd_hms("2017-01-01 00:00:00 GMT"),to=ymd_hms(paste0(as_date(CWdataFull$created_at[nrow(CWdataFull)])," 00:00:00")),by='1 day')
    
    attr(dateSeries,"tzone") = 'GMT'
    saveRDS(dateSeries,file=fp_oldDateSeries)
  }
  # Category datasets----
  CWdata = CWdataFull # select all CW data 
  CWdata$created_at = ymd_hms(CWdata$created_at,tz='GMT')
  CWdataTS = CWdata[CWdata$category==468,]
  CWdataSM = CWdata[CWdata$category==469,]
  CWdataPP = CWdata[CWdata$category==1919,]
  CWdataWL = CWdata[CWdata$category==470,]
  CWdataPS = CWdata[CWdata$category==3203,]
  CWdataST = CWdata[CWdata$category==3204,]
  
  # Basic values needed for dashboard ----
  uq.dates = unique(CWdata$created_at)
  uq.roots = unique(CWdata$root_id)
  uq.users = unique(CWdata$spotted_by)
  
  # filpath to the data file for the previous cumulative sums
  cumSumsFile = paste0("CWdata/cumSums")

  # if there are not yet downloaded contributions to the CrowdWater servers the variable "newDateSeries" will not exist
  # therefore the code will jump to the else statement (and calculate the cumsums over the entire dataset which presumably takes longer)
  # there might also be a good option to do that for the individual categories, but there it is more complicated
  # because the dateseries need to be calculated from the entire and newly downloaded dataset
  if (exists("newDateSeries")){
    dateSeries = readRDS(fp_oldDateSeries)
    dateSeries_newold = c(dateSeries,newDateSeries)
    saveRDS(dateSeries_newold,fp_oldDateSeries)

    # all cumSums
    newCumSumAll =  sapply(newDateSeries,function(x) length(CWdata$Spot_ID[CWdata$created_at<=x]))
    oldCumSumAll = readRDS(cumSumsFile)
    cumSums = c(oldCumSumAll,unlist(newCumSumAll))
    saveRDS(cumSums,file = cumSumsFile)

    # cumSumUsers
    newCumSumUsers = sapply(newDateSeries,function(x) length(unique(CWdata$spotted_by[CWdata$created_at<=x])))
    oldCumSumUsers = readRDS("CWdata/cumSumUsers")
    cumSumUsers = c(oldCumSumUsers,unlist(newCumSumUsers))
    saveRDS(cumSumUsers,"CWdata/cumSumUsers")

    dateSeries = dateSeries_newold
  }else{
    # all cumSums
    cumSums =  sapply(dateSeries,function(x) length(CWdata$Spot_ID[CWdata$created_at<=x]))
    saveRDS(cumSums,file = cumSumsFile)

    # cumsums at dates and Root IDs with corresponding updates
    cumSumUsers = sapply(dateSeries,function(x) length(unique(CWdata$spotted_by[CWdata$created_at<=x])))
    saveRDS(cumSumUsers,"CWdata/cumSumUsers")
  }


  # Monthly active users over the entire period----
  monthsAll = paste0(format(CWdata$created_at,'%Y'),'_',format(CWdata$created_at,'%m'))
  # create unique months from dateSeries, because there might be months wihtout contributions
  uq.months = unique(paste0(format(dateSeries,'%Y'),'_',format(dateSeries,'%m')))
  monthlyActiveUsersAll = sapply(uq.months,function(x){
    length(unique(CWdata$created_by[monthsAll==x]))
  })

  IdsPerRoot = sapply(uq.roots,function(x) CWdata$Spot_ID[CWdata$root_id==x])
  IdsPerUser = sapply(uq.users,function(x) CWdata$Spot_ID[CWdata$spotted_by==x])

  # for the slider further below
  maxcontribs = max(sapply(IdsPerRoot, function(x) length(x)))
  maxcontribUser = max(sapply(IdsPerUser, function(x) length(x)))

  # Temporary streams----
  IdsPerRootTS = sapply(uq.roots,function(x) CWdataTS$Spot_ID[CWdataTS$root_id==x])
  IdsPerUserTS = sapply(uq.users,function(x) CWdataTS$Spot_ID[CWdataTS$spotted_by==x])
  maxcontribsTS = max(sapply(IdsPerRootTS, function(x) length(x)))
  maxcontribUserTS = max(sapply(IdsPerUserTS, function(x) length(x)))
  uq.datesTS = unique(CWdataTS$created_at)
  dateSeriesTS = seq(from=min(ymd_hms("2017-01-01 00:00:00 GMT")),to=max(uq.datesTS)+3600,by='1 day')
  cumSumsTS = sapply(dateSeriesTS,function(x) length(CWdataTS$Spot_ID[CWdataTS$created_at<=x]))
  cumSumsUsersTS = sapply(dateSeriesTS,function(x) length(unique(CWdataTS$spotted_by[CWdataTS$created_at<=x])))
  # Monthly active users
  monthsTS = paste0(format(CWdataTS$created_at,'%Y'),'_',format(CWdataTS$created_at,'%m'))
  # create unique months from dateSeries, because there might be months without contributions
  monthlyActiveUsersTS = sapply(uq.months,function(x){
    length(unique(CWdataTS$created_by[monthsTS==x]))
  })

  # Soil moisture----
  IdsPerRootSM = sapply(uq.roots,function(x) CWdataSM$Spot_ID[CWdataSM$root_id==x])
  IdsPerUserSM = sapply(uq.users,function(x) CWdataSM$Spot_ID[CWdataSM$spotted_by==x])
  maxcontribsSM = max(sapply(IdsPerRootSM, function(x) length(x)))
  maxcontribUserSM = max(sapply(IdsPerUserSM, function(x) length(x)))
  uq.datesSM = unique(CWdataSM$created_at)
  dateSeriesSM = seq(from=min(ymd_hms("2017-01-01 00:00:00 GMT")),to=max(uq.datesSM)+3600,by='1 day')
  cumSumsSM = sapply(dateSeriesSM,function(x) length(CWdataSM$Spot_ID[CWdataSM$created_at<=x]))
  cumSumsUsersSM = sapply(dateSeriesSM,function(x) length(unique(CWdataSM$spotted_by[CWdataSM$created_at<=x])))
  # Monthly active users
  monthsSM = paste0(format(CWdataSM$created_at,'%Y'),'_',format(CWdataSM$created_at,'%m'))
  # create unique months from dateSeries, because there might be months wihtout contributions
  monthlyActiveUsersSM = sapply(uq.months,function(x){
    length(unique(CWdataSM$created_by[monthsSM==x]))
  })

  # Plastic pollution----
  IdsPerRootPP = sapply(uq.roots,function(x) CWdataPP$Spot_ID[CWdataPP$root_id==x])
  IdsPerUserPP = sapply(uq.users,function(x) CWdataPP$Spot_ID[CWdataPP$spotted_by==x])
  maxcontribsPP = max(sapply(IdsPerRootPP, function(x) length(x)))
  maxcontribUserPP = max(sapply(IdsPerUserPP, function(x) length(x)))
  uq.datesPP = unique(CWdataPP$created_at)
  dateSeriesPP = seq(from=min(ymd_hms("2017-01-01 00:00:00 GMT")),to=max(uq.datesPP)+3600,by='1 day')
  cumSumsPP = sapply(dateSeriesPP,function(x) length(CWdataPP$Spot_ID[CWdataPP$created_at<=x]))
  cumSumsUsersPP = sapply(dateSeriesPP,function(x) length(unique(CWdataPP$spotted_by[CWdataPP$created_at<=x])))
  #Monthly active users
  monthsPP = paste0(format(CWdataPP$created_at,'%Y'),'_',format(CWdataPP$created_at,'%m'))
  # create unique months from dateSeries, because there might be months wihtout contributions
  monthlyActiveUsersPP = sapply(uq.months,function(x){
    length(unique(CWdataPP$created_by[monthsPP==x]))
  })

  # Virtual Staff Gauges----
  IdsPerRootWL = sapply(uq.roots,function(x) CWdataWL$Spot_ID[CWdataWL$root_id==x])
  IdsPerUserWL = sapply(uq.users,function(x) CWdataWL$Spot_ID[CWdataWL$spotted_by==x])
  maxcontribsWL = max(sapply(IdsPerRootWL, function(x) length(x)))
  maxcontribUserWL = max(sapply(IdsPerUserWL, function(x) length(x)))
  uq.datesWL = unique(CWdataWL$created_at)
  dateSeriesWL = seq(from=min(ymd_hms("2017-01-01 00:00:00 GMT")),to=max(uq.datesWL)+3600,by='1 day')
  cumSumsWL = sapply(dateSeriesWL,function(x) length(CWdataWL$Spot_ID[CWdataWL$created_at<=x]))
  cumSumsUsersWL = sapply(dateSeriesWL,function(x) length(unique(CWdataWL$spotted_by[CWdataWL$created_at<=x])))
  #Monthly active users
  monthsWL = paste0(format(CWdataWL$created_at,'%Y'),'_',format(CWdataWL$created_at,'%m'))
  # create unique months from dateSeries, because there might be months wihtout contributions
  monthlyActiveUsersWL = sapply(uq.months,function(x){
    length(unique(CWdataWL$created_by[monthsWL==x]))
  })

  # Physical Staff Gauges ----
  IdsPerRootPS = sapply(uq.roots,function(x) CWdataPS$Spot_ID[CWdataPS$root_id==x])
  IdsPerUserPS = sapply(uq.users,function(x) CWdataPS$Spot_ID[CWdataPS$spotted_by==x])
  maxcontribsPS = max(sapply(IdsPerRootPS, function(x) length(x)))
  maxcontribUserPS = max(sapply(IdsPerUserPS, function(x) length(x)))
  uq.datesPS = unique(CWdataPS$created_at)
  dateSeriesPS = seq(from=min(ymd_hms("2017-01-01 00:00:00 GMT")),to=max(uq.datesPS)+3600,by='1 day')
  cumSumsPS = sapply(dateSeriesPS,function(x) length(CWdataPS$Spot_ID[CWdataPS$created_at<=x]))
  cumSumsUsersPS = sapply(dateSeriesPS,function(x) length(unique(CWdataPS$spotted_by[CWdataPS$created_at<=x])))
  #Monthly active users
  monthsPS = paste0(format(CWdataPS$created_at,'%Y'),'_',format(CWdataPS$created_at,'%m'))
  # create unique months from dateSeries, because there might be months wihtout contributions
  monthlyActiveUsersPS = sapply(uq.months,function(x){
    length(unique(CWdataPS$created_by[monthsPS==x]))
  })

  # Stream Types ----
  IdsPerRootST = sapply(uq.roots,function(x) CWdataST$Spot_ID[CWdataST$root_id==x])
  IdsPerUserST = sapply(uq.users,function(x) CWdataST$Spot_ID[CWdataST$spotted_by==x])
  maxcontribsST = max(sapply(IdsPerRootST, function(x) length(x)))
  maxcontribUserST = max(sapply(IdsPerUserST, function(x) length(x)))
  uq.datesST = unique(CWdataST$created_at)
  dateSeriesST = seq(from=min(ymd_hms("2017-01-01 00:00:00 GMT")),to=max(uq.datesST)+3600,by='1 day')
  cumSumsST = sapply(dateSeriesST,function(x) length(CWdataST$Spot_ID[CWdataST$created_at<=x]))
  cumSumsUsersST = sapply(dateSeriesST,function(x) length(unique(CWdataST$spotted_by[CWdataST$created_at<=x])))
  # Monthly active users
  monthsST = paste0(format(CWdataST$created_at,'%Y'),'_',format(CWdataST$created_at,'%m'))
  # create unique months from dateSeries, because there might be months wihtout contributions
  monthlyActiveUsersST = sapply(uq.months,function(x){
    length(unique(CWdataST$created_by[monthsST==x]))
  })
   
  # Plots with contributions ----
  cumPlot = cumplot(dateSeries, cumSums,'Total number of contributions') #??????
  output$cumsumplot = renderPlot({cumPlot})

  cumPlotWL = cumplot(dateSeriesWL, cumSumsWL,'Total number of contributions')
  output$cumsumplotWL = renderPlot({cumPlotWL})

  cumPlotSM = cumplot(dateSeriesSM, cumSumsSM,'Total number of contributions')
  output$cumsumplotSM = renderPlot({cumPlotSM})

  cumPlotTS = cumplot(dateSeriesTS, cumSumsTS,'Total number of contributions')
  output$cumsumplotTS = renderPlot({cumPlotTS})

  cumPlotPS = cumplot(dateSeriesPS, cumSumsPS,'Total number of contributions')
  output$cumsumplotPS = renderPlot({cumPlotPS})

  cumPlotPP = cumplot(dateSeriesPP, cumSumsPP,'Total number of contributions')
  output$cumsumplotPP = renderPlot({cumPlotPP})

  cumPlotST = cumplot(dateSeriesST, cumSumsST,'Total number of contributions')
  output$cumsumplotST = renderPlot({cumPlotST})

  # Plots with Users
  cumPlotUsers = cumplot(dateSeries, cumSumUsers,'Total number of contributors')
  output$cumsumplotUsers = renderPlot({cumPlotUsers})

  cumPlotUsersWL = cumplot(dateSeriesWL, cumSumsUsersWL,'Total number of contributors')
  output$cumsumplotUsersWL = renderPlot({cumPlotUsersWL})

  cumPlotUsersSM = cumplot(dateSeriesSM, cumSumsUsersSM,'Total number of contributors')
  output$cumsumplotUsersSM = renderPlot({cumPlotUsersSM})

  cumPlotUsersTS = cumplot(dateSeriesTS, cumSumsUsersTS, 'Total number of contributors')
  output$cumsumplotUsersTS = renderPlot({cumPlotUsersTS})

  cumPlotUsersPS = cumplot(dateSeriesPS, cumSumsUsersPS,'Total number of contributors')
  output$cumsumplotUsersPS = renderPlot({cumPlotUsersPS})

  cumPlotUsersPP = cumplot(dateSeriesPP, cumSumsUsersPP,'Total number of contributors')
  output$cumsumplotUsersPP = renderPlot({cumPlotUsersPP})

  cumPlotUsersST = cumplot(dateSeriesST, cumSumsUsersST,'Total number of contributors')
  output$cumsumplotUsersST = renderPlot({cumPlotUsersST})
  
  # Plots with montly active users----
  MauPlotAll = mauPlot(monthlyActiveUsersAll)
  output$mauPlotAll = renderPlot({MauPlotAll})
  
  MauPlotWL = mauPlot(monthlyActiveUsersWL)
  output$mauPlotWL = renderPlot({MauPlotWL})
  
  MauPlotSM = mauPlot(monthlyActiveUsersSM)
  output$mauPlotSM = renderPlot({MauPlotSM})
  
  MauPlotTS = mauPlot(monthlyActiveUsersTS)
  output$mauPlotTS = renderPlot({MauPlotTS})
  
  MauPlotPS = mauPlot(monthlyActiveUsersPS)
  output$mauPlotPS = renderPlot({MauPlotPS})
  
  MauPlotPP = mauPlot(monthlyActiveUsersPP)
  output$mauPlotPP = renderPlot({MauPlotPP})
  
  MauPlotST = mauPlot(monthlyActiveUsersST)
  output$mauPlotST = renderPlot({MauPlotST})
  
  # Heatmap
  output$heatmap_heatmap <- renderLeaflet({
    leaflet(CWdata) %>%
      addTiles(group="OSM") %>%
      addHeatmap(group="heat", lng=~longitude, lat=~latitude, max=.6, blur = 40, radius=20)
  })
  
  # # Heatmap based on kernel density
  # # --> rather useless (because this way its only global)
  # output$heatmap_kernel <- renderLeaflet({
  # leaflet() %>%
  #   addTiles(group = "OSM") %>%
  #   addHeatMap_kernel(data = CWdata, lon = longitude, lat = latitude)
  # })
  
  
  # Value Boxes ----
  # Contributions
  output$TotnContribs <- renderValueBox({
    valueBox(
      formatC(nrow(CWdata), format="d", big.mark=','),
      paste('All Contributions'),
      icon = icon("globe",lib='font-awesome'),
      color = "navy")    })
  
  output$nWLContribs <- renderValueBox({
    valueBox(
      formatC(nrow(CWdata[CWdata$category==470,]), format="d", big.mark=','),
      paste('Virtual Staff Gauge Contributions'),
      icon = icon("align-justify",lib='font-awesome'),
      color = "aqua")    })
  
  output$nSMContribs <- renderValueBox({
    valueBox(
      formatC(nrow(CWdata[CWdata$category==469,]), format="d", big.mark=','),
      paste('Soil Moisture Contributions'),
      icon = icon("allergies",lib='font-awesome'),
      color = "maroon")    })
  
  output$nTSContribs <- renderValueBox({
    valueBox(
      formatC(nrow(CWdataTS), format="d", big.mark=','),
      paste('Temporary Stream Contributions'),
      icon = icon("tint-slash",lib='font-awesome'),
      color = "olive")    })
  
  output$nPSContribs <- renderValueBox({
    valueBox(
      formatC(nrow(CWdataPS), format="d", big.mark=','),
      paste('Physical Staff Gauge Contributions'),
      icon = icon("ruler-vertical",lib='font-awesome'),
      color = "yellow")    })
  
  output$nPLContribs <- renderValueBox({
    valueBox(
      formatC(nrow(CWdata[CWdata$category==1919,]), format="d", big.mark=','),
      paste('Plastic Pollution Contributions'),
      icon = icon("puzzle-piece",lib='font-awesome'),
      color = "purple")    })
  
  output$nSTContribs <- renderValueBox({
    valueBox(
      formatC(nrow(CWdataST), format="d", big.mark=','),
      paste('Stream Type Contributions'),
      icon = icon("bacon",lib='font-awesome'),
      color = "orange")    })
  
  #RootSpots
  output$nWLRootSpots <- renderValueBox({
    valueBox(
      formatC(length(unique(CWdataWL$root_id)),format="d", big.mark = ','),
      "Virtual Staff Gauge Spots",
      icon=icon("align-justify",lib='font-awesome'),
      color = "aqua")
  })
  
  output$nSMRootSpots <- renderValueBox({
    valueBox(
      formatC(length(unique(CWdataSM$root_id)),format="d", big.mark = ','),
      "Soil Moisture Spots",
      icon=icon("allergies",lib='font-awesome'),
      color = "maroon")
  })
  
  output$nTSRootSpots <- renderValueBox({
    valueBox(
      formatC(length(unique(CWdataTS$root_id)),format="d", big.mark = ','),
      "Temporary Stream Spots",
      icon=icon("tint-slash",lib='font-awesome'),
      color = "olive")
  })
  
  output$nPSRootSpots <- renderValueBox({
    valueBox(
      formatC(length(unique(CWdataPS$root_id)),format="d", big.mark = ','),
      "Physical Staff Gauge Spots",
      icon=icon("ruler-vertical",lib='font-awesome'),
      color = "yellow")
  })
  
  output$nPPRootSpots <- renderValueBox({
    valueBox(
      formatC(length(unique(CWdataPP$root_id)),format="d", big.mark = ','),
      "Plastic Pollution Spots",
      icon=icon("puzzle-piece",lib='font-awesome'),
      color = "purple")
  })
  
  output$nSTRootSpots <- renderValueBox({
    valueBox(
      formatC(length(unique(CWdataST$root_id)),format="d", big.mark = ','),
      "Stream Type Spots",
      icon=icon("bacon",lib='font-awesome'),
      color = "orange")
  })
  
  
  
  #define the slider with adapted max value----
  output$sliderAll = renderUI({
    sliderInput(inputId = "XnrContribs", label = "Number of contributions:",step = 1,min = 1, max = max(c(maxcontribs,maxcontribUser)),value = 10)
  })
  output$sliderInterStr = renderUI({
    sliderInput(inputId = "XnrInterStrContribs", label = "Number of Temporary Stream contributions:",step = 1,min = 1, max = max(c(maxcontribsTS,maxcontribUserTS)),value = 10)
  })
  output$sliderSM = renderUI({
    sliderInput(inputId = "XnrSMContribs", label = "Number of Soil Moistre contributions:",step = 1,min = 1, max = max(c(maxcontribsSM,maxcontribUserSM)),value = 10)
  })
  output$sliderPP = renderUI({
    sliderInput(inputId = "XnrPPContribs", label = "Number of Plastic Pollution contributions:",step = 1,min = 1, max = max(c(maxcontribsPP,maxcontribUserPP)),value = 10)
  })
  output$sliderWL = renderUI({
    sliderInput(inputId = "XnrWLContribs", label = "Number of Virtual Staff Gauge contributions:",step = 1,min = 1, max = max(c(maxcontribsWL,maxcontribUserWL)),value = 10)
  })
  
  output$sliderPS = renderUI({
    sliderInput(inputId = "XnrPSContribs", label = "Number of Physical Staff Gauge contributions:",step = 1,min = 1, max = max(c(maxcontribsPS,maxcontribUserPS)),value = 10)
  })
  
  output$sliderST = renderUI({
    sliderInput(inputId = "XnrSTContribs", label = "Number of Stream Type contributions:",step = 1,min = 1, max = max(c(maxcontribsST,maxcontribUserST)),value = 10)
  })
  
  
  
  output$uqRootsSpots <- renderValueBox({
    valueBox(
      formatC(length(uq.roots), format="d", big.mark=','),
      paste('individual spots'),
      icon = icon("signal",lib='font-awesome'),
      color = "light-blue")    })
  
  # sliders
  # Users with more than X (slider) contributions ----
  output$UsersWithXcontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUser, function(x) length(x)>=input$XnrContribs)))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs),
      subtitle=paste0('Users with \u2265 ',input$XnrContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  output$UsersWithXWLcontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUserWL, function(x) length(x)>=input$XnrWLContribs)))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs),
      subtitle=paste0('Users with \u2265 ',input$XnrWLContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Users with more than X Soil Moisture (slider) contributions
  output$UsersWithXSMcontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUserSM, function(x) length(x)>=input$XnrSMContribs )))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs),
      subtitle=paste0('Users with \u2265 ',input$XnrSMContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Users with more than X Temporary Stream (slider) contributions
  output$UsersWithXTScontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUserTS, function(x) length(x)>=input$XnrInterStrContribs)))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs),
      subtitle=paste0('Users with \u2265 ',input$XnrInterStrContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Users with more than X Physical Staff Gauge (slider) contributions
  output$UsersWithXPScontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUserPS, function(x) length(x)>=input$XnrPSContribs)))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs),
      subtitle=paste0('Users with \u2265 ',input$XnrPSContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Users with more than X Plastic Pollution (slider) contributions
  output$UsersWithXPPcontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUserPP, function(x) length(x)>=input$XnrPPContribs)))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs),
      subtitle=paste0('Users with \u2265 ',input$XnrPPContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Users with more than X Stream Type (slider) contributions
  output$UsersWithXSTcontribs = renderValueBox({
    NrUsersWithMorethanXContribs=sum(unlist(lapply(IdsPerUserST, function(x) length(x)>=input$XnrSTContribs)))
    valueBox(
      value=formatC(NrUsersWithMorethanXContribs),
      subtitle=paste0('Users with \u2265 ',input$XnrSTContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  
  # Stations with more than X (slider) contributions ----
  output$stationsWithXcontribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRoot, function(x) length(x)>=input$XnrContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs),
      subtitle=paste0('Stations with \u2265 ',input$XnrContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Temporary Stream Stations with more than X (slider) contributions
  output$IntStrStationsWithXContribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRootTS, function(x) length(x)>=input$XnrInterStrContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs),
      subtitle=paste0('Temporary Stream Stations with \u2265 ',input$XnrInterStrContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Soil Moisture Stations with more than X (slider) contributions
  output$SMStationsWithXContribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRootSM, function(x) length(x)>=input$XnrSMContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs),
      subtitle=paste0('Soil Moisture Stations with \u2265 ',input$XnrSMContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Plastic Pollution Stations with more than X (slider) contributions----
  output$PPStationsWithXContribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRootPP, function(x) length(x)>=input$XnrPPContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs),
      subtitle=paste0('Plastic Pollution Stations with \u2265 ',input$XnrPPContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Virtual Staff Gauge Stations with more than X (slider) contributions ----
  output$WLStationsWithXContribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRootWL, function(x) length(x)>=input$XnrWLContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs),
      subtitle=paste0('Virtual Staff Gauge Stations with \u2265 ',input$XnrWLContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Physical Staff Gauge Stations with more than X (slider) contributions ----
  output$PSStationsWithXContribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRootPS, function(x) length(x)>=input$XnrPSContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs),
      subtitle=paste0('Physical Staff Gauge Stations with \u2265 ',input$XnrPSContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Stream Type Stations with more than X (slider) contributions ----
  output$STStationsWithXContribs = renderValueBox({
    NrStatsWithMorethanXContribs=sum(unlist(lapply(IdsPerRootST, function(x) length(x)>=input$XnrSTContribs)))
    valueBox(
      value=formatC(NrStatsWithMorethanXContribs),
      subtitle=paste0('Strean Type Stations with \u2265 ',input$XnrSTContribs,' contributions'),
      icon = icon("signal",lib='font-awesome'),
      color='teal')
  })
  
  # Explore contributions of stations with input id ----
  # UsrInputID = reactive({
  #   validate(
  #     need(length(uq.roots[unlist(lapply(IdsPerRoot,function(x) input$stationID %in% x))])==1, label = "Input Id")
  #   )
  #   get(input$stationID)
  # })
  # ---> handling of wrong inputs: https://shiny.rstudio.com/articles/validation.html
  observeEvent(input$expl_btn,{
    expl_rootId = uq.roots[unlist(lapply(IdsPerRoot,function(x) input$stationID %in% x))]
    
    # extract spot data
    expl_spotData = CWdata[CWdata$root_id==expl_rootId,]
    
    # create the text output
    # output$expl_statRootID <- renderText({paste0("The root spot id is: ",expl_rootId) })
    
    # download image of entered ID
    output$expl_thisImg <-
      renderText({
        shiny::validate(need(nrow(expl_spotData) > 0,message = ""))
        c('<img src=',
          paste0('https://files.spotteron.com/images/spots/',expl_spotData$image[expl_spotData$Spot_ID==input$stationID],'.jpg alt="latest spot image" width="100%;"'),
          '/>'
        )
      })
    output$expl_date_thisImg = renderText({
      shiny::validate(need(nrow(expl_spotData) > 0,message = ""))
      format(expl_spotData$created_at[expl_spotData$Spot_ID==input$stationID], "%d.%m.%Y %H:%M:%S")
    })
    # download root image
    output$expl_rootImg <-
      renderText({
        shiny::validate(need(nrow(expl_spotData) > 0,message = ""))
        c('<img src=',
          paste0('https://files.spotteron.com/images/spots/',expl_spotData$image[expl_spotData$Spot_ID==expl_rootId],'.jpg alt="root image" width="100%;"'),
          '/>'
        )
      })
    output$expl_date_rootImg = renderText({
      shiny::validate(need(nrow(expl_spotData) > 0,message = ""))
      format(expl_spotData$created_at[expl_spotData$Spot_ID==expl_rootId], "%d.%m.%Y %H:%M:%S")
    })
    #download image of latest contribution
    output$expl_latestImg <-
      renderText({
        shiny::validate(need(nrow(expl_spotData) > 0,message = ""))
        c('<img src=',
          paste0('https://files.spotteron.com/images/spots/',expl_spotData$image[length(expl_spotData$image)],'.jpg alt="latest spot image" width="100%;"'),
          '/>'
        )
      })
    output$expl_date_latestImg = renderText({
      shiny::validate(need(nrow(expl_spotData) > 0,message = ""))
      format(expl_spotData$created_at[length(expl_spotData$image)], "%d.%m.%Y %H:%M:%S")
    })
    # contribution ever X day----
    nrContribs = nrow(expl_spotData)
    firstDay = expl_spotData$created_at[1]
    lastDay = expl_spotData$created_at[nrow(expl_spotData)]
    duration = as.numeric(lastDay-firstDay)
    CntrEverXDays = 1/(nrContribs/duration)
    
    output$expl_nrOfContribs = renderValueBox({
      valueBox(
        nrContribs,
        paste('contributions'),
        icon = icon("signal",lib='font-awesome'),
        color = "purple")
    })
    
    output$expl_contrPerDay = renderValueBox({
      valueBox(
        formatC(round(CntrEverXDays,1),format="f",digits=1, big.mark=','),
        paste('average days between contributions'),
        icon = icon("hourglass-half",lib='font-awesome'),
        color = "light-blue")
    })
    
    # make chart of timeseries
    # water level	= 470 fld_05_00000066
    # soil moisture = 469 fld_05_00000052
    # temporary stream =	468 fld_05_00000051
    # plastic pollution =	1919 fld_05_00000286 (nr. of pieces)
    if(nrow(expl_spotData)>0){
      expl_plt = ggplot(data=expl_spotData,aes(x=as.Date(created_at)))+
        scale_x_date(date_labels = "%m.%Y")
      if(expl_spotData$category[1]==470){
        ylabs =  seq(min(expl_spotData$Streamlevel,na.rm = T),max(expl_spotData$Streamlevel,na.rm = T),by=1)
        expl_plt = expl_plt +
          geom_line(color = "#00AFBB", alpha = 0.5,aes(y=expl_spotData$Streamlevel),size=1.5)+
          geom_point(color = "#00AFBB",aes(y=expl_spotData$Streamlevel),size=2.5)+
          scale_y_continuous(breaks=ylabs,labels=ylabs)+
          xlab('')+ylab('Water level class') ####???
        
      }else if (expl_spotData$category[1]==469){
        ylabs = seq(min(expl_spotData$SMnr,na.rm = T),max(expl_spotData$SMnr,na.rm = T),by=1)
        expl_plt = expl_plt + geom_point(color = "#FC4E07",aes(y=expl_spotData$SMnr))+
          geom_line(color = "#FC4E07", alpha = 0.5,aes(y=expl_spotData$SMnr),size=1.5)+
          geom_point(color = "#FC4E07",aes(y=expl_spotData$SMnr),size=2.5)+
          scale_y_continuous(breaks=ylabs,labels=as.character(sapply(ylabs,function(x) SM_LUT$SMInput[SM_LUT$SMnr==x])))+
          geom_rect(ymin=7.5,ymax=8.5,xmin=min(as.Date(expl_spotData$created_at)),xmax=max(as.Date(expl_spotData$created_at)),fill = "grey50",alpha=0.01)+
          xlab('')+ylab('Soil Moisture Category')
        
      }else if (expl_spotData$category[1]==468){
        ylabs = seq(min(expl_spotData$TSnr,na.rm = T),max(expl_spotData$TSnr,na.rm = T),by=1)
        expl_plt = expl_plt + geom_point(color = "#0ADF91",aes(y=expl_spotData$TSnr))+
          geom_line(color = "#0ADF91", alpha = 0.5,aes(y=expl_spotData$TSnr),size=1.5)+
          geom_point(color = "#0ADF91",aes(y=expl_spotData$TSnr),size=2.5)+
          scale_y_continuous(breaks=ylabs,labels=as.character(sapply(ylabs,function(x) TS_LUT$TSInput[TS_LUT$TSnr==x])))+
          xlab('')+ylab('Temporary Stream Status')
        
      }else if (expl_spotData$category[1]==1919){
        ylabs = seq(min(expl_spotData$PPnr,na.rm = T),max(expl_spotData$PPnr,na.rm = T),by=1)
        expl_plt = expl_plt + geom_point(color = "#901AC3",aes(y=expl_spotData$PPnr))+
          geom_line(color = "#901AC3", alpha = 0.5,aes(y=expl_spotData$PPnr),size=1.5)+
          geom_point(color = "#901AC3",aes(y=expl_spotData$TSnr),size=2.5)+
          scale_y_continuous(breaks=ylabs,labels=as.character(sapply(ylabs,function(x) PP_LUT$PPInput[PP_LUT$PPnr==x])))+
          xlab('')+ylab('Plastic Pollution amount')
      }
      expl_plt = expl_plt +theme_minimal()+ theme(
        text = element_text(size=20),
        rect=element_blank(),
        panel.grid = element_blank(),
        panel.background= element_blank(),
        plot.background = element_blank())
      
    }
    output$expl_timelinePlot <- renderPlot({
      shiny::validate(
        need(nrow(expl_spotData) > 0,message = HTML('Enter an ID of an existing CrowdWater station. You can find the ID of a spot by clicking on a spot on www.spotteron.com/crowdwater and copying the last number of the link in the browser.'))
      )
      expl_plt}, bg="transparent", execOnResize = TRUE)
    
    # Leaflet map----
    expl_point = data.frame(RootID = expl_spotData$root_id,longitude=expl_spotData$longitude,latitude=expl_spotData$latitude)
    
    output$expl_spotMap <- renderLeaflet({
      leaflet(expl_point) %>%
        addProviderTiles(providers$OpenStreetMap.HOT,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(~longitude, ~latitude, popup = ~htmlEscape(RootID))
    })
    
  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)

