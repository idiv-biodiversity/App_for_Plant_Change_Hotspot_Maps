library(shiny)
library(stringr)
library(sf)
library(leaflet)
library(mapview)
library(gridExtra)
library(devtools)
library(leaflet.minicharts)
library(manipulateWidget)
library(viridis)
library(pals)
library(raster)
library(htmltools)
library(htmlwidgets)
library(scales)
library(cowplot)
library(assertthat)
library(backports)
library(callr)
library(cli)
library(colorspace)
library(crosstalk)
library(digest)
library(ellipsis)
library(evaluate)
library(fansi)
library(farver)
library(ggplot2)
library(glue) 
library(gtable)
library(htmltools) 
library(htmlwidgets)
library(jsonlite)
library(lazyeval)
library(leaflet)
library(leaflet.providers)
library(markdown)
library(mime) 
library(pillar) 
library(pkgbuild)
library(pkgconfig)
library(pkgload)
library(praise)
library(prettyunits)
library(processx)
library(ps)
library(R6)
library(raster) 
library(Rcpp)
library(rlang) 
library(rstudioapi)
library(scales)
library(sp)
library(testthat)
library(tibble) 
library(vctrs)
library(withr)
library(xfun)
library(yaml)
library(rgdal)

## inputs will be:
# different base maps
# different floristic statuses
## will be 2 drop downs

myLabelFormat = function(..., reverse_order = FALSE){ 
  if(reverse_order){ 
    function(type = "numeric", cuts){ 
      cuts <- sort(cuts, decreasing = T)
    } 
  }else{
    labelFormat(...)
  }
}

##outputs will be:
# maps

# need to be added to the fluid page as arguments


## ideally, we will have an opacity slider for every floristic status
## if possible also a slider for min and max of changes

## Generate basic map (i.e. Germany and the respective federal states)
## This is the map that will be loaded into each tab in the browser app

bundeslander<-readRDS("./datasets/bundeslander.rds")

germanmap<- leaflet() %>%
  addTiles() %>% 
  addPolygons(data=bundeslander, color="black", fillColor = NA, fillOpacity = 0, weight=0.8) %>%
  addScaleBar(position=c("bottomright")) %>% syncWith(groupname="germanmaps")

## Color functions



################## Set up the User interface #########
### Set up user interface page
### this specifies the general design of the App
## i.e. I will have some side panel on the left with certain features
## that the user will be able to interact with

ui<- fluidPage(
      sidebarPanel(radioButtons(inputId = "Basemap", 
                              label="Basemap",
                              choices=list("Open Street Map"="OpenStreetMap.Mapnik",
                                           "Satellite (ESRI)"="Esri.WorldImagery"),
                              selected="OpenStreetMap.Mapnik"),# here, the user will be able to select the base maps (I offer OSM and Satellite)
                  br(), # introduces a line break
                  selectInput(inputId="url", label="Natural Regions",
                              choices=list( "On" = "http://geodienste.bfn.de/ogc/wms/gliederungen?",
                                            "Off" = NA),
                                selected="Off"),## Option on wether a WMS layer from the respective url (here: Naturräume) should be loaded
                  br(),
                  radioButtons(inputId = "Florstat",
                                     label="Floristic status group",
                                     choices = list("Natives"="Natives",
                                                    "Archaeophytes"="Archaeophytes",
                                                    "Neophytes"="Neophytes",
                                                    "All Species"="All Species"),
                                     selected="All Species"
                                      ), # here, the user will be able to select the floristic status group
                  br(),
                  br(),
                  sliderInput(inputId = "Opacity",
                              label="Opacity of active layer",
                              value=0.8,
                              min=0, max=1,step = 0.1), ## the layers should not alsways cover the basemap details; here you can change the transparency of the active layer
              width=2),
      ### Now we begin with the main panel (i.e. right of the side panel)
      ### the main panel will hold the map(s).
      ### I want to have several Tabs, relating to different aspects of the data (i.e. species richness,
      ### absolute/relative changes, hotspots of changes)
                  mainPanel(tags$style(type="text/css",
                       ".shiny-output-error {visibility: hidden; }",
                       ".shiny-output-error:before {visibility: hidden; }",
                       ".nav-tabs {font-size: 10px} ",
                       ".leaflet .legend i {border-radius: 20%;
                         width:8px;
                         height: 8px;
                         margin-top: 4px}"),
                       h1("Maps of species-richness and its changes in Germany"),
                       "The values in this visualization refer to the study of Eichenberg et al. (2020)",
                       tabsetPanel(type = "tabs",## sets up different tabs, one for each tabPanel argument below
                        tabPanel("Species Richness",
                                 fluidRow(column(4,"1960-1987",leafletOutput(outputId="sopgrid_map1")),
                                          column(4,"1988-1996",leafletOutput(outputId="sopgrid_map2")),
                                          column(4,"1960-1987",leafletOutput(outputId="sopgrid_map3")))),
                        tabPanel("Absolute Changes",
                                 fluidRow(column(4,"1960-1987 vs. 1988-1996",leafletOutput(outputId="abschange_map1")),
                                          column(4,"1988-1996 vs. 1997-2017",leafletOutput(outputId="abschange_map2")),
                                          column(4,"1960-1987 vs. 1997-2017",leafletOutput(outputId="abschange_map3")))),
                        tabPanel("Relative Change [%]",
                                 fluidRow(column(4,"1960-1987 vs. 1988-1996",leafletOutput(outputId="relchange_map1")),
                                          column(4,"1988-1996 vs. 1997-2017",leafletOutput(outputId="relchange_map2")),
                                          column(4,"1960-1987 vs. 1997-2017",leafletOutput(outputId="relchange_map3")))),
                        tabPanel("Hotspots of relative Change",
                                 fluidRow(column(4,"1960-1987 vs. 1988-1996",leafletOutput(outputId="hotspot_rel_map1")),
                                          column(4,"1988-1996 vs. 1997-2017",leafletOutput(outputId="hotspot_rel_map2")),
                                          column(4,"1960-1987 vs. 1997-2017",leafletOutput(outputId="hotspot_rel_map3"))))
                        ),width=10
                    )
          )

server<- function(input, output) {
  ## add the basemaps and the floristic status
  ## modularize the selection of the datasets
 
  ### Here I set up the basic maps (germany_map from above) as the defult for every tab
output$sopgrid_map1<- renderLeaflet({germanmap}) 
output$sopgrid_map2<- renderLeaflet({germanmap}) 
output$sopgrid_map3<- renderLeaflet({germanmap}) 

output$abschange_map1<- renderLeaflet({germanmap})
output$abschange_map2<- renderLeaflet({germanmap})
output$abschange_map3<- renderLeaflet({germanmap})

output$relchange_map1<- renderLeaflet({germanmap}) 
output$relchange_map2<- renderLeaflet({germanmap})
output$relchange_map3<- renderLeaflet({germanmap}) 

output$hotspot_rel_map1<- renderLeaflet({germanmap}) 
output$hotspot_rel_map2<- renderLeaflet({germanmap}) 
output$hotspot_rel_map3<- renderLeaflet({germanmap}) 

## Now I will sequentially add information to the basic maps. On each tab, specific information will be displayed
## and therefor, different arguments will be necessary per tab.

observeEvent(c(input$Florstat,input$Basemap,input$Opacity,input$url),{
    opac<- input$Opacity
    url<- input$url
    Florstat<- input$Florstat
    
    #SOPGrid data
    mapData1<- readRDS(paste0("./datasets/ras_sopgrid_",Florstat,"_1.rds"))  
    mapData2<- readRDS(paste0("./datasets/ras_sopgrid_",Florstat,"_2.rds"))
    mapData3<- readRDS(paste0("./datasets/ras_sopgrid_",Florstat,"_3.rds"))
    
    ## Which have the biggest values
    val_mdat1<- values(mapData1)[-which(is.na(values(mapData1)))]
    val_mdat2<- values(mapData2)[-which(is.na(values(mapData2)))]
    val_mdat3<- values(mapData3)[-which(is.na(values(mapData3)))]
    
    allvalssop<-as.data.frame(c(val_mdat1,val_mdat2,val_mdat3))
    allvalssop$colnum<-as.numeric(as.factor(cut(allvalssop[,1],800)))
    
    select_sop1<-allvalssop$colnum[allvalssop==val_mdat1]
    select_sop1<- select_sop1[order(val_mdat1)]
    select_sop2<-allvalssop$colnum[allvalssop==val_mdat2]
    select_sop2<- select_sop2[order(val_mdat2)]
    select_sop3<-allvalssop$colnum[allvalssop==val_mdat3]
    select_sop3<- select_sop3[order(val_mdat3)]
    
    maxsop<-max(c(val_mdat1,val_mdat2,val_mdat3))
    minsop<-min(c(val_mdat1,val_mdat2,val_mdat3))
    
    k<- which(c(max(val_mdat1),max(val_mdat2),max(val_mdat3))==max(c(val_mdat1,val_mdat2,val_mdat3)))
    
    colfunc_sop_grid<- readRDS(paste0("./datasets/colfunc_sopgrid_",Florstat,".rds"))
    
    palette_SOPgrid<- colorNumeric(
      rev(colfunc_sop_grid(800)),
      domain=seq(from=minsop,to=maxsop,length.out=12024),
      na.color = "transparent"
    )
    
    #Abs_changes_data
    mapData4<- readRDS(paste0("./datasets/ras_abs_change_",Florstat,"_1.rds"))
    mapData5<- readRDS(paste0("./datasets/ras_abs_change_",Florstat,"_2.rds"))
    mapData6<- readRDS(paste0("./datasets/ras_abs_change_",Florstat,"_3.rds"))
    
    
    ## Which have the biggest values
    val_mdat4<- values(mapData4)[-which(is.na(values(mapData4)))]
    val_mdat5<- values(mapData5)[-which(is.na(values(mapData5)))]
    val_mdat6<- values(mapData6)[-which(is.na(values(mapData6)))]
    
    allvalsabs<-as.data.frame(c(val_mdat4,val_mdat5,val_mdat6))
    allvalsabs$colnum<-as.numeric(as.factor(cut(allvalsabs[,1],800)))
    
    select_abs1<-allvalsabs$colnum[allvalsabs==val_mdat4]
    select_abs1<- select_abs1[order(val_mdat4)]
    select_abs2<-allvalsabs$colnum[allvalsabs==val_mdat5]
    select_abs2<- select_abs2[order(val_mdat5)]
    select_abs3<-allvalsabs$colnum[allvalsabs==val_mdat6]
    select_abs3<- select_abs3[order(val_mdat6)]
    
    maxabschng<- max(c(val_mdat4,val_mdat5,val_mdat6))
    minabschng<- min(c(val_mdat4,val_mdat5,val_mdat6))
    l<- which(c(max(val_mdat4),max(val_mdat5),max(val_mdat6))==max(c(val_mdat4,val_mdat5,val_mdat6)))
    
    
    colfunc_abs_change<- readRDS(paste0("./datasets/colfunc_abschange_",Florstat,".rds"))
      
    palette_abs_change<- colorNumeric(
      rev(colfunc_abs_change(800)),
      domain=seq(from=minabschng,to=maxabschng,length.out = 12024),
      na.color = "transparent"
    )
    
    #Rel_changes_data
    mapData7<- readRDS(paste0("./datasets/ras_rel_change_",Florstat,"_1.rds"))
    mapData8<- readRDS(paste0("./datasets/ras_rel_change_",Florstat,"_2.rds"))
    mapData9<- readRDS(paste0("./datasets/ras_rel_change_",Florstat,"_3.rds"))
    
    ## Which have the biggest values
    val_mdat7<- values(mapData7)[-which(is.na(values(mapData7)))]
    val_mdat8<- values(mapData8)[-which(is.na(values(mapData8)))]
    val_mdat9<- values(mapData9)[-which(is.na(values(mapData9)))]
    
    allvalsrel<-as.data.frame(c(val_mdat7,val_mdat8,val_mdat9))
    allvalsrel$colnum<-as.numeric(as.factor(cut(allvalsrel[,1],800)))
    
    select_rel1<-allvalsrel$colnum[allvalsrel==val_mdat7]
    select_rel1<- select_rel1[order(val_mdat7)]
    select_rel2<-allvalsrel$colnum[allvalsrel==val_mdat8]
    select_rel2<- select_rel2[order(val_mdat8)]
    select_rel3<-allvalsrel$colnum[allvalsrel==val_mdat9]
    select_rel3<- select_rel3[order(val_mdat9)]
    
    maxrelchng<- max(c(val_mdat7,val_mdat8,val_mdat9))
    minrelchng<- min(c(val_mdat7,val_mdat8,val_mdat9))
    
    m<- which(c(max(val_mdat7),max(val_mdat8),max(val_mdat9))==max(c(val_mdat7,val_mdat8,val_mdat9)))
    
    colfunc_rel_change<- readRDS(paste0("./datasets/colfunc_relchange_",Florstat,".rds"))
    
    palette_rel_change1<- colorNumeric(
      rev(colfunc_rel_change(800)),
      domain=values(mapData7),
      na.color = "transparent"
    )
    
    palette_rel_change<- colorNumeric(
      rev(colfunc_rel_change(800)),
      domain=seq(from=minrelchng,to=maxrelchng,length.out = 12024),
      na.color = "transparent"
    )
    
    #Hotspot_changes
    mapData10<- readRDS(paste0("./datasets/ras_rel_change_hotspots_",Florstat,"_1.rds"))
    mapData11<- readRDS(paste0("./datasets/ras_rel_change_hotspots_",Florstat,"_2.rds"))
    mapData12<- readRDS(paste0("./datasets/ras_rel_change_hotspots_",Florstat,"_3.rds"))
    
    
    ## Fill the maps with the data
    ## SOPGrid ts1
    leafletProxy("sopgrid_map1") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData1, colors = colfunc_sop_grid(800)[select_sop1],opacity =opac) %>%
      addLegend("bottomleft", 
                opacity=0.9, 
                pal = palette_SOPgrid, 
                values = seq(from=minsop,to=maxsop,length.out = 12024), 
                labFormat =  myLabelFormat(reverse_order = T),
                title ="SOP<sub>Grid</sub>")
    
    ## SOPGrid ts2
    leafletProxy("sopgrid_map2") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData2, colors = colfunc_sop_grid(800)[select_sop2],opacity =opac) 
    
    ## SOPGrid ts3
    leafletProxy("sopgrid_map3") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData3, colors = colfunc_sop_grid(800)[select_sop3],opacity =opac)
    
    ## Fill the maps with the data
    ## abs changes ts1
    leafletProxy("abschange_map1") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData4, colors = colfunc_abs_change(800)[select_abs1],opacity =opac) %>%
      addLegend("bottomleft", 
                opacity=0.9, 
                pal = palette_abs_change, 
                values = seq(from=minabschng,to=maxabschng,length.out = 12024), 
                labFormat =  myLabelFormat(reverse_order = T),
                title ="&Delta;SOP<sub>Grid</sub><br>abs.")
    
    ## abs changes ts2
    leafletProxy("abschange_map2") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData5, colors = colfunc_abs_change(800)[select_abs2],opacity =opac)
    
    ## abs changes ts3
    leafletProxy("abschange_map3") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData6, colors = colfunc_abs_change(800)[select_abs3],opacity =opac)
    
    
    ## Fill the maps with the data
    ## rel changes ts1
    leafletProxy("relchange_map1") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData7, colors = colfunc_rel_change(800)[select_rel1],opacity =opac) %>%
      addLegend("bottomleft", 
                opacity=0.9, 
                pal = palette_rel_change, 
                values = seq(from=minrelchng,to=maxrelchng,length.out = 12024), 
                labFormat =  myLabelFormat(reverse_order = T),
                title ="&Delta;SOP<sub>Grid</sub><br>[%]")
    
    ## rel changes ts2
    leafletProxy("relchange_map2") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData8, colors = colfunc_rel_change(800)[select_rel2],opacity =opac)
    
    ## rel changes ts3
    leafletProxy("relchange_map3") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData9, colors = colfunc_rel_change(800),opacity =opac)
    
    ## Fill the maps with the data
    ## hotspot rel changes ts1
    leafletProxy("hotspot_rel_map1") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData10, colors = colfunc_rel_change(800),opacity =opac) %>%
      addLegend("bottomleft", 
                opacity=0.9, 
                pal = palette_rel_change, 
                values = seq(from=minrelchng,to=maxrelchng,length.out = 12024), 
                labFormat =  myLabelFormat(reverse_order = T),
                title ="&Delta;SOP<sub>Grid</sub><br>[%]")
    
    ## hotspot rel changes ts1
    leafletProxy("hotspot_rel_map2") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData11, colors = colfunc_rel_change(800),opacity =opac)
    
    ## hotspot rel changes ts1
    leafletProxy("hotspot_rel_map3") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN); Meyen et al. (1953 - 1962)") %>%
      addRasterImage(mapData12, colors = colfunc_rel_change(800),opacity =opac)
  })  #the $ argument must match the plotOutput in "ui()"
}

shinyApp(ui= ui, server= server)

