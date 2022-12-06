

library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(ggpmisc)
library(ggplot2)
library(glue)
options(spinner.color="#006272")


theme_update(plot.title = element_text(hjust = 0.5, size=40))
options(scipen =100000000) # to get the number in numeric term



# Data loading and cleanup


data=read.csv("final_raw_sample_0_percent.csv")

data1=data[data$Year=="2018",] # get the data for 2018

data1=subset(data1, select=-c(Total.Environmental.Intensity..Operating.Income.))



data1$Total.Environmental.Intensity..Revenue.=as.numeric(gsub("%","",data1$Total.Environmental.Intensity..Revenue.))


data1$Total.Environmental.Cost=abs(as.numeric(gsub("\\(|\\)|,","",data1$Total.Environmental.Cost)))


data1$Working.Capacity=abs(as.numeric(gsub("\\(|\\)|,","",data1$Working.Capacity)))



data1$Fish.Production.Capacity=abs(as.numeric(gsub("\\(|\\)|,","",data1$Fish.Production.Capacity)))

data1$Crop.Production.Capacity=abs(as.numeric(gsub("\\(|\\)|,","",data1$Crop.Production.Capacity)))

data1$Meat.Production.Capacity=abs(as.numeric(gsub("\\(|\\)|,","",data1$Meat.Production.Capacity)))

data1$Biodiversity=abs(as.numeric(gsub("\\(|\\)|,","",data1$Biodiversity)))

data1$Abiotic.Resources=abs(as.numeric(gsub("\\(|\\)|,","",data1$Abiotic.Resources)))

data1$Water.production.capacity..Drinking.water...Irrigation.Water.=abs(as.numeric(gsub("\\(|\\)|,","",data1$Water.production.capacity..Drinking.water...Irrigation.Water.)))

data1$Wood.Production.Capacity=abs(as.numeric(gsub("\\(|\\)|,","",data1$Wood.Production.Capacity)))

data1=data1 %>% drop_na(c(Wood.Production.Capacity,Abiotic.Resources) )



data1$revenue=abs(data1$Total.Environmental.Cost/data1$Total.Environmental.Intensity..Revenue.)*100
data1=data1 %>% relocate(revenue, .before = Total.Environmental.Intensity..Revenue.)

data1$Total.Environmental.Cost=data1$Total.Environmental.Cost* sign(data1$Total.Environmental.Intensity..Revenue.)


data1$Industry_code=as.numeric(gsub(".*?([0-9]+).*", "\\1", data1$Industry..Exiobase.))
data1=data1 %>% relocate(Industry_code,.after=Industry..Exiobase.)

# adding column Industry_category into the data1

data1= data1 %>% mutate(Industry_category=case_when(
  Industry_code %in% c(5:8) ~ "Electricity generation",
  Industry_code %in%  c(9:14) ~ "Mineral extraction",
  Industry_code %in%  c(15:25,36) ~ "Manufacturing goods",
  Industry_code %in%  c(4,26:29) ~ "Heavy Industry",
  Industry_code %in%  c(30:33) ~ "Electrical/Electronics",
  Industry_code %in%  c(34) ~ "Vehicle",
  Industry_code %in%  c(41) ~ "Water Industry",
  Industry_code %in%  c(45,70) ~ "construction", 
  Industry_code %in%  c(51:53,71) ~ "trade",
  Industry_code %in%  c(59:63) ~ "Transportation",
  Industry_code %in%  c(65:67) ~ "Finance",
  Industry_code %in%  c(64,72:93) ~ "Service sector and communication",
  Industry_code %in%  c(2,95,96) ~ "Agri and forestry"
  
  
) )



# adding pollution_level

data1=data1%>% mutate(pollution_level= case_when(
  ( Total.Environmental.Intensity..Revenue.>= -10) & (Total.Environmental.Intensity..Revenue.< 0) ~ 'low',
  (Total.Environmental.Intensity..Revenue.>= -50) & (Total.Environmental.Intensity..Revenue.< -10)  ~ 'medium',
  (Total.Environmental.Intensity..Revenue.>= -100) & (Total.Environmental.Intensity..Revenue.< -50) ~ 'high',
  ( Total.Environmental.Intensity..Revenue.>= -1000) & (Total.Environmental.Intensity..Revenue.< -100) ~ 'very high',
  Total.Environmental.Intensity..Revenue.< -1000 ~ 'Extreme',
  Total.Environmental.Intensity..Revenue. >=0 ~ 'positive'
  
  
))


data1$pollution_level=factor(data1$pollution_level, levels=c("Extreme","very high","high","medium","low","positive"))


# industry code and name
Industry_df=distinct(data1[,c("Industry_code","Industry..Exiobase.")])
Industry_df=Industry_df %>% arrange(Industry_code)

# for pie chart discription
piedata <- data1 %>% 
  group_by(Industry_category) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`) ) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))



country_df= data1 %>% group_by(Country,Industry..Exiobase.,Industry_code, Industry_category,pollution_level) %>% summarise(
  
  total_reve=sum(revenue),
  total.Environment_cost=sum(Total.Environmental.Cost),
  Working.Capacity=sum(Working.Capacity),
  Fish.Production.Capacity=sum(Fish.Production.Capacity),
  Crop.Production.Capacity=sum(Crop.Production.Capacity),
  Meat.Production.Capacity=sum(Meat.Production.Capacity),
  Biodiversity=sum(Biodiversity),
  total_Abiotic.Resources=sum(Abiotic.Resources),
  Water.production.capacity..Drinking.water...Irrigation.Water.=sum(Water.production.capacity..Drinking.water...Irrigation.Water.),
  Wood.Production.Capacity=sum(Wood.Production.Capacity))

# data about the revenue intensity 

data_revin=data1 %>% group_by(Country,Industry_category) %>% summarise(Total_rev= sum(revenue),
                                                                       total_cost=sum(Total.Environmental.Cost))


data_revin$environment_intensity=data_revin$total_cost/data_revin$Total_rev *100



header=dashboardHeader(title="DashBoard")

sidebar=dashboardSidebar(sidebarMenu(
                 menuItem("Home", tabName="home"),
                 menuItem("Data Analysis", tabName="dlysis"),
                 menuItem("Conclusion",tabName="conclusion")
))
body=dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            fluidRow( 
                     tabsetPanel(
                                tabPanel("Introduction", uiOutput("Intro")),
                                tabPanel("Data Description",
                                         fluidRow(
                                           column(7,fluidRow(uiOutput("disc")),
                                                  p(br(),
                                                    br()),
                                          
                                                  fluidRow(plotOutput("piedisc"))),
                                           column(5,box(width=8,tableOutput("table")))
                                           
                                             
                                           ))
                                         
                                )
                              )
                              
                              
                       ),
   tabItem(tabName="dlysis",
            fluidRow(
              tabsetPanel(
                tabPanel("Overview",
                         br(),
                         br(),
                         fluidRow( column(12,box(width = 10, title = "Pollution level and Revenue of a company", status = "primary", solidHeader = TRUE, background = "green",
                             fluidRow(
                           column (8, offset=1,
                                   sliderInput("pollution","Pollution Level", -500,1000,c(0,100)))
                         ),
                         fluidRow(plotOutput("reven_poll")) ),
                         
                         ))
                         
                ),
                tabPanel("Industrywise",
                         #add industry wise graph
                         
                         br(),
                         br(),
                        box(width=12,background = "green" ,fluidRow( column(6,offset=2, 
                                          selectInput("Ind",
                                                      label ="select the Industry",
                                                      choices = data1$Industry_category,
                                                      selected=" Finance"
                                          )
                                          
                         )
                         ),
                         fluidRow( column(7,
                                          plotOutput("Industry_pol")),
                                   column(5, 
                                          plotOutput("pieind"))
                                   
                         )
                         
                ))
                ,
                
                tabPanel("Countrywise",
                         # add countrywise data
                         br(),
                         br(),
                        box( width=12,background="green",fluidRow( column(8, offset=2, 
                                          selectInput("cont",
                                                      label ="select the country",
                                                      choices = data1$Country,
                                                      selected=" India"
                                          )
                           
                         )
                ),
                fluidRow( column(7,
                                 plotOutput("country_pol")),
                          column(5,
                                 plotOutput("piecont"))),
                          fluidRow(plotOutput("pollution_intensity"))
                  
                
               
              ))
            )
   )
  ),
  tabItem(
    tabName="conclusion",fluidRow(column(8, offset=2,
    br(),
    br(),
    p("The main conclusion from the Data Visualisation :",
    tags$ul(tags$li("most of the industry falls under the low pollution category, but it is low revenue company which have shown higher variance in pollution level from being the extreme polluter to positive one. Whereas most higher revenue company is low or medium polluter.
 "),
           br(), tags$li("Most of the European nation presented in the data are not much affecting the environment whereas big country
                    like India, USA, Canada, China are one of the highest pollutor especially in electricity generation. ")
          , br()
          , tags$li("Most of the Industry majorly affected the working capacity and then water production, drinking water and irrigation water 
                    capacity and marginally others whereas in case of Electricity genearation, it does affect this significantly. ")))
    
  )
    ))
                     
    
))


ui<- dashboardPage(skin="green",header,sidebar, body)

server <- function(input, output) {
  
  
  
  
  output$Intro = renderUI(
    p(
    br(),
    br(),
    img(src="environ_pic.jpg", width=400, height=200, align="center"),
    
    
    br(),
    br(),
    br(),
    br(),
    h1("Introduction", style="color:red"),
    br(),
    p("The Dataset used for this project is from the",
      tags$span("Corporate Environmental Impact",style= "colour:blue"),
      " . It contain the the data about environmental impact of company to the different factor of environment like water, health etc.
        For this project I have used the 2018 data from this dataset .
        The main aim is to analyze pollution level of each industry and compare there influence on different factor environment."),
    
    p("Source :",a("Kaggle: Monetization Factors for Corporate Impact on the Environment",
                   href="https://www.kaggle.com/datasets/mannmann2/corporate-environmental-impact?select=final_raw_sample_0_p"))
    ,
    br()
    ))
  
  
  # data discription
  output$disc=renderUI( tags$div( tags$br(),
                                  tags$br(),
                                  tags$ul(
                                    tags$li("The dataset I have choosen has 16 columns and 1754 rows."), tags$br(),
                                    tags$li("for analysis I have created three column :", tags$br(), tags$br(),
                                            tags$ul(tags$li("Industry_code: Based on International Standard Industrial Classification Revision 3.1 (ISIC)"), tags$br(),
                                                    tags$li(" pollution level : contain categorical value according to environment intensity :",
                                                            tags$ul(tags$li(" greater than 0 : Positive"),
                                                              tags$li("-10 to 0 : low"),
                                                                    tags$li("-50 to -10 : Medium"),
                                                                    tags$li("-100 to -50 : High"),
                                                                    tags$li("-1000 to -100 : Very High"),
                                                                    tags$li(" below -1000 : Extreme"))),tags$br(),
                                                    tags$li("Industry_category : to broadly define the Industry to which any company belong like finance, electricity generation etc.")
                                            ))
                                  )
  )
  
  
  )
  
  output$piedisc=renderPlot(
                            
                            
                            
                            ggplot(piedata, aes(x = "", y = perc, fill = Industry_category)) +
                              geom_col() + geom_text(aes(x=1.58, label=labels),
                                                     position = position_stack(vjust=0.5),
                                                     show.legend = F, angle=75, size=2) +
                              coord_polar(theta = "y") +
                              scale_fill_discrete()+
                              theme_void() +ggtitle("Fig-1:  % of Industry_category representation in dataset ")+theme(plot.title = element_text(size=15)))
  
  output$table=renderTable(Industry_df,striped=F,
                           hover=T,
                           bordered =T,
                           spacing = "s",
                           digits=0
  )
  
output$reven_poll=renderPlot({
   
   data2=data1[data1$Total.Environmental.Intensity..Revenue.> input$pollution[1] & data1$Total.Environmental.Intensity..Revenue.<=input$pollution[2],]
   
  ggplot(data2)+geom_jitter(aes(revenue,Total.Environmental.Intensity..Revenue., color=Industry_category) )
    })
  
  
  
  

output$country_pol= renderPlot({
  cd=country_df[country_df$Country==input$cont,]
  
  ggplot(cd,aes(Industry_category))+
    geom_bar(aes(fill=pollution_level))+theme(axis.text.x = element_text(angle=40, vjust=1.05, hjust=1.06),axis.text=element_text(size=8), plot.title = element_text(size=20))
})
    
output$Industry_pol = renderPlot({
  ind_df=data1[data1$Industry_category==input$Ind,]
  
  ggplot(ind_df,aes(Industry..Exiobase.))+
    geom_bar(aes(fill=pollution_level))+theme(axis.text.x = element_text(angle=40, vjust=1.05, hjust=1.06),                                  
                                              axis.text=element_text(size=8), plot.title = element_text(size=20))
}) 
  
output$pieind=renderPlot({
  
  hd_df=data1[data1$Industry_category==input$Ind,]
  hd_df=hd_df[,c(8:16)]
  hd.df1=hd_df[1,]
  
  for (i in colnames(hd_df)){
    hd.df1$i=sum(hd_df$i)
  }
  
  for (i in c(2:9)){
    hd.df1[,i]=hd.df1[,i]/hd.df1[,1] *100
  }
  hd.df1=data.frame(t(hd.df1))
  hd.df1$factor=rownames(hd.df1)
  
  colnames(hd.df1)=c("perc","factor")
  hd.df1=hd.df1[c(-1,-10),]
  
  
  ggplot(hd.df1, aes(x = "", y=perc , fill = factor)) +
    geom_col()+
    coord_polar(theta = "y") +
    scale_fill_discrete()+
    theme_void()+theme(plot.title = element_text(size=8))
})
  


output$piecont = renderPlot({
  hd_df=data1[data1$Country==input$cont,]
  hd_df=hd_df[,c(8:16)]
  hd.df1=hd_df[1,]
  
  for (i in colnames(hd_df)){
    hd.df1$i=sum(hd_df$i)
  }
  
  for (i in c(2:9)){
    hd.df1[,i]=hd.df1[,i]/hd.df1[,1] *100
  }
  hd.df1=data.frame(t(hd.df1))
  hd.df1$factor=rownames(hd.df1)
  
  colnames(hd.df1)=c("perc","factor")
  hd.df1=hd.df1[c(-1,-10),]
  
  
  ggplot(hd.df1, aes(x = "", y=perc , fill = factor)) +
    geom_col()+
    coord_polar(theta = "y") +
    scale_fill_discrete()+
    theme_void()+theme(plot.title = element_text(size=8))
})

output$pollution_intensity= renderPlot({
  c_df=data_revin[data_revin$Country==input$cont,]
  ggplot(c_df,aes(Industry_category, environment_intensity,group=1))+ geom_line()+ylab("Environment intensity as percentage of Revenue")+geom_point()
})
}

   
shinyApp(ui = ui, server = server)
