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

myLabelFormat <- function(..., reverse_order = FALSE){ 
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

bundeslander <- readRDS("./datasets/bundeslander.rds")

germanmap <- leaflet() %>%
  addTiles() %>% 
  addPolygons(data = bundeslander, 
              color = "black", 
              fillColor = NA, 
              fillOpacity = 0, 
              weight = 0.8) %>%
  addScaleBar(position = c("bottomright")) %>% 
  syncWith(groupname = "germanmaps")

## Color functions
colfunc_native_num<- colorRampPalette(c("#1f78b4ff", 
                                        "#1f78b4ff", 
                                        "#1f78b4ff"), 
                                      interpolate="linear")

colfunc_neo_num<- colorRampPalette(c("#fef208ff", 
                                     "#fef208ff", 
                                     "#fef208ff"), 
                                   interpolate="linear")

colfunc_archaeo_num<- colorRampPalette(c("#a6cee3ff", 
                                         "#a6cee3ff", 
                                         "#a6cee3ff"), 
                                       interpolate="linear")

colfunc_overlapp<- colorRampPalette(c("#e31a1cff", 
                                      "#e31a1cff", 
                                      "#e31a1cff"), 
                                    interpolate="linear")

colfunc_palette_overlap<- colorRampPalette(c("#1f78b4ff", 
                                             "#fef208ff", 
                                             "#a6cee3ff", 
                                             "#e31a1cff"), 
                                           interpolate="linear")


################## Set up the User interface #########
### Set up user interface page
### this specifies the general design of the App
## i.e. I will have some side panel on the left with certain features
## that the user will be able to interact with

ui <- fluidPage(
        sidebarPanel(radioButtons(inputId = "Basemap", 
                                  label = "Basemap",
                                  choices = list("Open Street Map" = "OpenStreetMap.Mapnik",
                                            "Satellite (ESRI)" = "Esri.WorldImagery"),
                                  selected = "OpenStreetMap.Mapnik"),# here, the user will be able to select the base maps (I offer OSM and Satellite)
                  br(), # introduces a line break
                  selectInput(inputId = "url", 
                              label = "Natural Regions",
                              choices = list( "On"  = "http://geodienste.bfn.de/ogc/wms/gliederungen?",
                                              "Off" = NA),
                                selected = "Off"),## Option on wether a WMS layer from the respective url (here: Naturräume) should be loaded
                  br(),
                  radioButtons(inputId = "Florstat",
                               label = "Floristic status group",
                               choices = list("Natives" = "Natives",
                                              "Archaeophytes" = "Archaeophytes",
                                              "Neophytes" = "Neophytes",
                                              "All Species" = "All Species"),
                                     selected = "All Species"
                                      ),                  # here, the user will be able to select the floristic status group
                  br(),
                  br(),
                  sliderInput(inputId = "Opacity",
                              label = "Opacity of active layer",
                              value = 0.8,
                              min = 0, 
                              max = 1,
                              step = 0.1),
                  HTML("<font size=1> <p align='justify'> <em> We are grateful to 
                       the tremendous effort of the countless people who 
                       contributed their plant occurrence records to the 
                       different sources that underlie this visualization!</em> 
                       </p><font>"),
                  fluidRow(HTML("<font size=1> <p align='center'> <em>Project Links:</em>"),
                           br(),
                           column(4,
                                  tags$a(imageOutput("image1"),
                                         href = "https://www.idiv.de/en/smon",
                                         target = "_blank",
                                         alt = "Go to sMon Homepage")),
                           column(8,
                                  tags$a(imageOutput("image2"),
                                         href = "https://www.idiv.de/",
                                         target = "_blank"))
                  ),## the layers should not alsways cover the basemap details; here you can change the transparency of the active layer
              width = 2
              ),
      
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
                       tabsetPanel(type = "tabs", ## sets up different tabs, one for each tabPanel argument below
                                   id = "mode_of_maps", 
                                  tabPanel("Species Richness",
                                      fluidRow(column(4,
                                                      h5("1960-1987", align = "center"),
                                                      leafletOutput(outputId = "sopgrid_map1")
                                                      ),
                                          column(4,
                                                 h5("1988-1996", align = "center"),
                                                 leafletOutput(outputId = "sopgrid_map2")
                                                 ),
                                          column(4,
                                                 h5("1997-2017", align = "center"),
                                                 leafletOutput(outputId = "sopgrid_map3")
                                                 )
                                          )
                                      ),
                        tabPanel("Absolute Changes",
                                 fluidRow(column(4,
                                                 h5("1960-1987 vs. 1988-1996", align = "center"),
                                                 leafletOutput(outputId = "abschange_map1")
                                                 ),
                                          column(4,
                                                 h5("1988-1996 vs. 1997-2017", align = "center"),
                                                 leafletOutput(outputId = "abschange_map2")
                                                 ),
                                          column(4,
                                                 h5("1960-1987 vs. 1997-2017", align = "center"),
                                                 leafletOutput(outputId = "abschange_map3")
                                                 )
                                          )
                                 ),
                        tabPanel("Relative Change [%]",
                                 fluidRow(column(4,
                                                 h5("1960-1987 vs. 1988-1996", align = "center"),
                                                 leafletOutput(outputId = "relchange_map1")
                                                 ),
                                          column(4,
                                                 h5("1988-1996 vs. 1997-2017", align = "center"),
                                                 leafletOutput(outputId = "relchange_map2")
                                                 ),
                                          column(4,
                                                 h5("1960-1987 vs. 1997-2017", align = "center"),
                                                 leafletOutput(outputId = "relchange_map3")
                                                 )
                                          )
                                 ),
                        tabPanel("Hotspots of relative Change",
                                 fluidRow(column(4,
                                                 h5("1960-1987 vs. 1988-1996", align = "center"),
                                                 leafletOutput(outputId = "hotspot_rel_map1")
                                                 ),
                                          column(4,
                                                 h5("1988-1996 vs. 1997-2017", align = "center"),
                                                 leafletOutput(outputId = "hotspot_rel_map2")
                                                 ),
                                          column(4,
                                                 h5("1960-1987 vs. 1997-2017",align = "center"),
                                                 leafletOutput(outputId = "hotspot_rel_map3")
                                                 )
                                          )
                                 ),
                        tabPanel("Overlaps of hotspots across the full study period",
                                 fluidRow(column(4,
                                                 h5("Natives vs. Archaeophytes", align = "center"),
                                                 leafletOutput(outputId = "nat_vs_arch")
                                 ),
                                 column(4,
                                        h5("Natives vs. Neophytes", align = "center"),
                                        leafletOutput(outputId = "nat_vs_neo")
                                 ),
                                 column(4,
                                        h5("Archaeophytes vs. Neophytes",align = "center"),
                                        leafletOutput(outputId = "arch_vs_neo")
                                 )
                             )
                         ),
                        tabPanel("Methodological information",
                                 h3("Data compilation"),
                                 br(),
                                 HTML("We compiled an extensive dataset of approx. 29
                                      million occurrence records in Germany between 
                                      1960 and 2017 from 23 different data 
                                      sources (Table S1 in Eichenberg et al., 2020). 
                                      The full dataset comprises the non-aggregated data
                                      underlying the German Distribution Atlas of 
                                      Ferns and Flowering Plants, restricted to 
                                      observations between 1960 and 2013. We extended 
                                      this dataset to the year 2017 by integrating data 
                                      from more recent habitat mapping projects of 
                                      federal states, vegetation relevés provided in 
                                      two major German databases, GVRD (
                                      <a href='http://vegetation-db.biologie.uni-halle.de'>
                                      http://vegetation-db.biologie.uni-halle.de</a>) 
                                      and vegetweb 2.0 (<a href='https://www.vegetweb.de'>
                                      https://www.vegetweb.de</a>) 
                                      and from universities and private collections 
                                      (see Eichenberg et al. 2020 for further details)."),
                                 br(),
                                 br(),
                                 h3("Determination of study periods"),
                                 br(),
                                 HTML("For analyzing trends, the dataset was binned into three periods 
                                      (1960-1987, 1988-1996, 1997-2017), each of them with similar 
                                      number of total records and covering all 12024 German grid-cells. 
                                      The temporal extent of these periods was determined by the need 
                                      to find periods of similar coverage (spatial and taxonomically) 
                                      of the whole nation (see Appendix I in Eichenberg et al. 2020 
                                      for further details)."),
                                 br(),
                                 br(),
                                 h3("Species selection and Nomenclature"),
                                 br(),
                                 HTML("Taxa were harmonized using an accepted common taxonomic reference 
                                      list for Germany (GermanSL; <a href='https://germansl.infinitenature.org/'>
                                      https://germansl.infinitenature.org/</a>). 
                                      Subspecies, variants etc. were raised to the species or, if necessary, to the 
                                      aggregate level. We excluded species that were recorded in only one of the 
                                      three periods or had fewer than 23 records in total. 
                                      Species were characterized according to their floristic status either as natives, 
                                      archaeophytes (non-natives introduced before 1492) or 
                                      neophytes (non-natives introduced after 1492,) using 
                                      information available from the database BiolFlor (<a href='https://www.ufz.de/biolflor/index.jsp'>
                                      https://www.ufz.de/biolflor/index.jsp</a>) 
                                      and FloraWeb (<a href='http://www.floraweb.de'>
                                      http://www.floraweb.de/</a>). 
                                      Species with an unknown floristic status were excluded. This left 
                                      us with a total of 2245 species for analysis, 
                                      equaling 58% of all German vascular plants 
                                      (if raised to a similar taxonomic level as used for the analyses 
                                      in Eichenberg et al., 2020)."),
                                 br(),
                                 br(),
                                 h3("Correction for false absences"),
                                 HTML("Where occurrence records do not originate from a project focusing 
                                      on the complete floristic inventory or does not use complete 
                                      check-lists, false absences (i.e. not reporting a species that was 
                                      present, but was either not detected or detected but not reported) 
                                      are an issue. In addition, atlas projects, which aim at taxonomic 
                                      completeness may not be finished in a federal state completely 
                                      within one of the defined study periods, leading to taxonomic or 
                                      spatial gaps in the data of a single study period. 
                                      To correct for this so-called reporting bias, we used the Frescalo 
                                      algorithm available in the R package ‘sparta’ 
                                      (<a href='https://github.com/BiologicalRecordsCentre/sparta'>
                                        https://github.com/BiologicalRecordsCentre/sparta</a>). 
                                      Briefly, the Frescalo algorithm calculates the occurrence probability (OP) 
                                      of a species not detected or reported in a focal grid-cell, 
                                      based on the frequency of this species in the local neighborhood 
                                      (here: 100 grid-cells) of this cell, while accounting for the 
                                      ecological similarity of the neighborhood. Ecological similarity of 
                                      the neighboring grid-cells was calculated based on a set of 76 variables, 
                                      comprising climatic, topographic and edaphic measures. A detailed 
                                      description of the specifications for the Frescalo algorithm is 
                                      given in Appendix I (c.f. Eichenberg et al. 2020)."),
                                 br(),
                                 br(),
                                 h3("Definition of 'Occurrence' in the context of Eichenberg et al. (2020)"),
                                 br(),
                                 HTML("Occurrence is defined as the sum of occurrence probabilities over all grid cells
                                      in which a species occurred in a certain time step. 
                                      Maps of the spatial distribution of a given species across 
                                      the study region at a given time are not a direct outcome 
                                      of the Frescalo algorithm as available in ’sparta’. 
                                      The spatial distribution of the probability of a species 
                                      being present at the focal grid-cell of neighborhood in a 
                                      certain period can be readily calculated from the available 
                                      output using the Equations given in Eichenberg et al. (2020). 
                                      Nationwide occurrence is approximated by summing up the values of 
                                      grid-cell occurrence probability across all grid cells in which a 
                                      species is detected in a certain timestep."),
                                 br(),
                                 br(),
                                 h3("Definition of 'species-richness' in the context of Eichenberg et al. (2020)"),
                                 br(),
                                 HTML("We summed up the occurrence probabilities of all species within 
                                      a grid-cell as an estimate of species-richness (SOP<sub>Grid</sub>), 
                                      while acknowledging that it is not species-richness per-se, since 
                                      our analysis does not include the very rare species (see 'Species selection and Nomenclature').
                                      Hence, our species-richness values are underestimating actual grid-cell 
                                      species-richness. However, SOP<sub>Grid</sub> was found to be highly significantly 
                                      correlated (r= 0.39, p< 0.001) to species numbers in grid-cells of the 
                                      FlorKart dataset that were identified to be well-sampled by previous research. 
                                      Therefore, changes in SOP<sub>Grid</sub> can be interpreted as meaningfully 
                                      representing relative changes between grid-cells and timesteps.")
                        )
                      ), width = 10)
                    
          )


server<- function(input, output) {
  ## add the basemaps and the floristic status
  ## modularize the selection of the datasets
 
  ### Here I set up the basic maps (germany_map from above) as the defult for every tab
output$sopgrid_map1 <- renderLeaflet({germanmap}) 
output$sopgrid_map2 <- renderLeaflet({germanmap}) 
output$sopgrid_map3 <- renderLeaflet({germanmap}) 

output$abschange_map1 <- renderLeaflet({germanmap})
output$abschange_map2 <- renderLeaflet({germanmap})
output$abschange_map3 <- renderLeaflet({germanmap})

output$relchange_map1 <- renderLeaflet({germanmap}) 
output$relchange_map2 <- renderLeaflet({germanmap})
output$relchange_map3 <- renderLeaflet({germanmap}) 

output$hotspot_rel_map1 <- renderLeaflet({germanmap}) 
output$hotspot_rel_map2 <- renderLeaflet({germanmap}) 
output$hotspot_rel_map3 <- renderLeaflet({germanmap}) 

output$nat_vs_arch <- renderLeaflet({germanmap}) 
output$nat_vs_neo <- renderLeaflet({germanmap}) 
output$arch_vs_neo <- renderLeaflet({germanmap}) 


output$image1 <- renderImage({
                            list(src = "./sMon_image.png",
                            contentType = "image/png",
                            width = "41px",
                            height = "40px"
                      )}, 
                      deleteFile = FALSE)

output$image2 <- renderImage({
                            list(src = "./iDiv_image.png",
                            contentType = "image/png",
                            width = "60px",
                            height = "24px"
                      )}, 
                      deleteFile = FALSE)

## Now I will sequentially add information to the basic maps. On each tab, specific information will be displayed
## and therefor, different arguments will be necessary per tab.

observeEvent(c(input$Florstat,
               input$Basemap,
               input$Opacity,
               input$url,
               input$mode_of_maps,
               input$Overlap),{
    
    opac <- input$Opacity
    url <- input$url
    Florstat <- input$Florstat
    
    #SOPGrid data
    mapData1 <- readRDS(paste0("./datasets/ras_sopgrid_",Florstat,"_1.rds"))  
    mapData2 <- readRDS(paste0("./datasets/ras_sopgrid_",Florstat,"_2.rds"))
    mapData3 <- readRDS(paste0("./datasets/ras_sopgrid_",Florstat,"_3.rds"))
    
    ## Which have the biggest values
    val_mdat1 <- values(mapData1)[-which(is.na(values(mapData1)))]
    val_mdat2 <- values(mapData2)[-which(is.na(values(mapData2)))]
    val_mdat3 <- values(mapData3)[-which(is.na(values(mapData3)))]
    
    allvalssop <- as.data.frame(c(val_mdat1,
                                  val_mdat2,
                                  val_mdat3)
                                )
    allvalssop$colnum <- as.numeric(as.factor(cut(allvalssop[,1],800)))
    
    select_sop1 <- allvalssop$colnum[allvalssop == val_mdat1]
    select_sop1 <- select_sop1[order(val_mdat1)]
    select_sop2 <- allvalssop$colnum[allvalssop == val_mdat2]
    select_sop2 <- select_sop2[order(val_mdat2)]
    select_sop3 <- allvalssop$colnum[allvalssop == val_mdat3]
    select_sop3 <- select_sop3[order(val_mdat3)]
    
    maxsop <- max(c(val_mdat1,val_mdat2,val_mdat3))
    minsop <- min(c(val_mdat1,val_mdat2,val_mdat3))
    
    k <- which(c(max(val_mdat1),
                 max(val_mdat2),
                 max(val_mdat3)) == 
               max(c(val_mdat1,val_mdat2,val_mdat3)
                     )
               )
    
    colfunc_sop_grid <- readRDS(paste0("./datasets/colfunc_sopgrid_",Florstat,".rds"))
    
    palette_SOPgrid <- colorNumeric(
                                   rev(colfunc_sop_grid(800)),
                                   domain = seq(from = minsop,
                                                to = maxsop,
                                                length.out = 12024),
                                     na.color = "transparent"
                                    )
    
    #Abs_changes_data
    mapData4 <- readRDS(paste0("./datasets/ras_abs_change_", Florstat, "_1.rds"))
    mapData5 <- readRDS(paste0("./datasets/ras_abs_change_", Florstat, "_2.rds"))
    mapData6 <- readRDS(paste0("./datasets/ras_abs_change_", Florstat, "_3.rds"))
  
    ## Which have the biggest values
    val_mdat4 <- values(mapData4)[-which(is.na(values(mapData4)))]
    val_mdat5 <- values(mapData5)[-which(is.na(values(mapData5)))]
    val_mdat6 <- values(mapData6)[-which(is.na(values(mapData6)))]
    
    allvalsabs <- as.data.frame(c(val_mdat4,
                                  val_mdat5,
                                  val_mdat6)
                                )
    
    allvalsabs$colnum <- as.numeric(as.factor(cut(allvalsabs[,1], 800)))
    
    select_abs1 <- allvalsabs$colnum[allvalsabs == val_mdat4]
    select_abs1 <- select_abs1[order(val_mdat4)]
    select_abs2 <- allvalsabs$colnum[allvalsabs == val_mdat5]
    select_abs2 <- select_abs2[order(val_mdat5)]
    select_abs3 <- allvalsabs$colnum[allvalsabs == val_mdat6]
    select_abs3 <- select_abs3[order(val_mdat6)]
    
    maxabschng <- max(c(val_mdat4,
                        val_mdat5,
                        val_mdat6)
                      )
    minabschng <- min(c(val_mdat4,
                        val_mdat5,
                        val_mdat6)
                      )
    
    l <- which(c(max(val_mdat4),
                 max(val_mdat5),
                 max(val_mdat6)) == 
               max(c(val_mdat4,
                     val_mdat5,
                     val_mdat6)
                   )
               )
    
    
    colfunc_abs_change <- readRDS(paste0("./datasets/colfunc_abschange_", 
                                         Florstat,
                                         ".rds"))
      
    palette_abs_change <- colorNumeric(
                                        rev(colfunc_abs_change(800)),
                                        domain = seq(from = minabschng,
                                                     to = maxabschng,
                                                     length.out = 12024),
                                         na.color = "transparent"
                                       )
    
    #Rel_changes_data
    mapData7 <- readRDS(paste0("./datasets/ras_rel_change_", Florstat, "_1.rds"))
    mapData8 <- readRDS(paste0("./datasets/ras_rel_change_", Florstat, "_2.rds"))
    mapData9 <- readRDS(paste0("./datasets/ras_rel_change_", Florstat, "_3.rds"))
    
    ## Which have the biggest values
    val_mdat7 <- values(mapData7)[-which(is.na(values(mapData7)))]
    val_mdat8 <- values(mapData8)[-which(is.na(values(mapData8)))]
    val_mdat9 <- values(mapData9)[-which(is.na(values(mapData9)))]
    
    allvalsrel <- as.data.frame(c(val_mdat7,
                                  val_mdat8,
                                  val_mdat9)
                                )
    
    allvalsrel$colnum <- as.numeric(as.factor(cut(allvalsrel[,1],800)))
    
    select_rel1 <- allvalsrel$colnum[allvalsrel == val_mdat7]
    select_rel1 <- select_rel1[order(val_mdat7)]
    select_rel2 <- allvalsrel$colnum[allvalsrel == val_mdat8]
    select_rel2 <- select_rel2[order(val_mdat8)]
    select_rel3 <- allvalsrel$colnum[allvalsrel == val_mdat9]
    select_rel3 <- select_rel3[order(val_mdat9)]
    
    maxrelchng <- max(c(val_mdat7,
                        val_mdat8,
                        val_mdat9)
                      )
    minrelchng <- min(c(val_mdat7,
                        val_mdat8,
                        val_mdat9)
                      )
    
    m <- which(c(max(val_mdat7),
                 max(val_mdat8),
                 max(val_mdat9)) == 
                max(c(val_mdat7,
                      val_mdat8,
                      val_mdat9)
                    )
               )
    
    colfunc_rel_change <- readRDS(paste0("./datasets/colfunc_relchange_",
                                         Florstat, ".rds"))
    
    palette_rel_change1 <- colorNumeric(
                                        rev(colfunc_rel_change(800)),
                                        domain = values(mapData7),
                                        na.color = "transparent"
                                       )
    
    palette_rel_change<- colorNumeric(
                                      rev(colfunc_rel_change(800)),
                                      domain = seq(from = minrelchng,
                                                   to = maxrelchng,
                                                   length.out = 12024),
                                      na.color = "transparent"
                                      )
    
    #Hotspot_changes
    mapData10<- readRDS(paste0("./datasets/ras_rel_change_hotspots_", 
                               Florstat,
                               "_1.rds")
                        )
    
    mapData11<- readRDS(paste0("./datasets/ras_rel_change_hotspots_", 
                               Florstat,
                               "_2.rds")
                        )
    
    mapData12<- readRDS(paste0("./datasets/ras_rel_change_hotspots_", 
                               Florstat,
                               "_3.rds")
                        )
    
    
    ras_hotsp_nat<- readRDS("./datasets/ras_hotsp_nat.rds")
    ras_hotsp_archaeo<- readRDS("./datasets/ras_hotsp_archaeo.rds")
    ras_hotsp_neo<- readRDS("./datasets/ras_hotsp_neo.rds")
    
    ras_nat_vs_arch<- readRDS("./datasets/ras_nat_vs_arch.rds")
    ras_nat_vs_neo<- readRDS("./datasets/ras_nat_vs_neo.rds")
    ras_arch_vs_neo<- readRDS("./datasets/ras_arch_vs_neo.rds")
    
    
    
    ## Fill the maps with the data
    ## SOPGrid ts1
    leafletProxy("sopgrid_map1") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution = "Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData1, 
                     colors = colfunc_sop_grid(800)[select_sop1],
                     opacity = opac) %>%
      addLegend("bottomleft", 
                opacity = 0.9, 
                pal = palette_SOPgrid, 
                values = seq(from = minsop,
                             to = maxsop,
                             length.out = 12024), 
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
                  attribution="Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData2, 
                     colors = colfunc_sop_grid(800)[select_sop2],
                     opacity = opac) 
    
    ## SOPGrid ts3
    leafletProxy("sopgrid_map3") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution ="Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData3, 
                     colors = colfunc_sop_grid(800)[select_sop3],
                     opacity = opac)
    
    ## Fill the maps with the data
    ## abs changes ts1
    leafletProxy("abschange_map1") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData4, 
                     colors = colfunc_abs_change(800)[select_abs1],
                     opacity = opac) %>%
      addLegend("bottomleft", 
                opacity = 0.9, 
                pal = palette_abs_change, 
                values = seq(from = minabschng,
                             to = maxabschng,
                             length.out = 12024), 
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
                  attribution="Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData5, 
                     colors = colfunc_abs_change(800)[select_abs2],
                     opacity = opac)
    
    ## abs changes ts3
    leafletProxy("abschange_map3") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE, format = "image/png"), 
                  attribution = "Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData6, 
                     colors = colfunc_abs_change(800)[select_abs3],
                     opacity = opac)
    
    
    ## Fill the maps with the data
    ## rel changes ts1
    leafletProxy("relchange_map1") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution="Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData7, 
                     colors = colfunc_rel_change(800)[select_rel1],
                     opacity = opac) %>%
      addLegend("bottomleft", 
                opacity=0.9, 
                pal = palette_rel_change, 
                values = seq(from = minrelchng,
                             to = maxrelchng,
                             length.out = 12024), 
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
                  attribution = "Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData8, 
                     colors = colfunc_rel_change(800)[select_rel2],
                     opacity = opac)
    
    ## rel changes ts3
    leafletProxy("relchange_map3") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution = "Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData9, 
                     colors = colfunc_rel_change(800),
                     opacity = opac)
    
    ## Fill the maps with the data
    ## hotspot rel changes ts1
    leafletProxy("hotspot_rel_map1") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution = "Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData10, 
                     colors = colfunc_rel_change(800),
                     opacity = opac) %>%
      addLegend("bottomleft", 
                opacity=0.9, 
                pal = palette_rel_change, 
                values = seq(from = minrelchng,
                             to = maxrelchng,
                             length.out = 12024), 
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
                  attribution = "Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData11, 
                     colors = colfunc_rel_change(800),
                     opacity = opac)
    
    ## hotspot rel changes ts1
    leafletProxy("hotspot_rel_map3") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution = "Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(mapData12, 
                     colors = colfunc_rel_change(800),
                     opacity = opac)
  
    ## Overlap of hotspot regions
    leafletProxy("nat_vs_arch") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution = "Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(ras_hotsp_nat, 
                     colors = colfunc_native_num(1),
                     opacity = 0.7) %>%
      addRasterImage(ras_hotsp_archaeo, 
                     colors = colfunc_archaeo_num(1),
                     opacity = 0.7) %>%
      addRasterImage(ras_nat_vs_arch, 
                     colors = colfunc_overlapp(1),
                     opacity = opac) %>%
      addLegend("topright", 
                opacity=0.9, 
                colors = c(colfunc_palette_overlap(4)[1], 
                           colfunc_palette_overlap(4)[2], 
                           colfunc_palette_overlap(4)[3], 
                           colfunc_palette_overlap(4)[4]), 
                labels = c("Natives", "Neophytes", "Archaeophytes", "Overlap"),
                title ="Floristic group")

    
    ## Overlap of hotspot regions
    leafletProxy("nat_vs_neo") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution = "Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(ras_hotsp_nat, 
                     colors = colfunc_native_num(1),
                     opacity = 0.7) %>%
      addRasterImage(ras_hotsp_neo, 
                     colors = colfunc_neo_num(1),
                     opacity = 0.7) %>%
      addRasterImage(ras_nat_vs_neo, 
                     colors = colfunc_overlapp(1),
                     opacity = opac)

    
    ## Overlap of hotspot regions
    leafletProxy("arch_vs_neo") %>% 
      clearImages() %>%
      clearControls() %>%
      addProviderTiles(provider = input$Basemap) %>%
      addWMSTiles(baseUrl = url,
                  layers = "Naturraeume", 
                  options = WMSTileOptions(transparent = TRUE,format = "image/png"), 
                  attribution = "Bundesamt für Naturschutz (BfN)") %>%
      addRasterImage(ras_hotsp_archaeo, 
                     colors = colfunc_archaeo_num(1),
                     opacity = 0.7) %>%
      addRasterImage(ras_hotsp_neo, 
                     colors = colfunc_neo_num(1),
                     opacity = 0.7) %>%
      addRasterImage(ras_nat_vs_arch, 
                     colors = colfunc_overlapp(1),
                     opacity = opac)
    
    })  #the $ argument must match the plotOutput in "ui()"
}

shinyApp(ui = ui, server = server)

