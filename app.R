##  'super'  by M Harbison.  July, 2019 ##        #  setwd('C:/Users/micks/Desktop/funShiny/great') 

library(sp)            # for polygons             Note:  grDevices (for rainbow) is already covered.
library(rgeos)         # since in showLogs()
library(dplyr)         # for filter
library(shiny)         # for layout + reactivity
library(leaflet)       # for a zoomable map

dfile  <- read.csv( 'data/longData.csv',          # The app does not use all of the columns. 
                     stringsAsFactors = F)        #   Col(1) = 'shortNm' = matching.   Col(2) = 'LongNms' = labels.

options( stringsAsFactors = FALSE )               # required for ex. str( spdf@data$data.Campus) = chr, not Factor.

M1     <- dfile[  1:10, 1 ]                                           # CMC^3 regions
M2     <- dfile[ 11:15, 1 ]                                           # South regions (also in CA)
M3     <- dfile[ 16:19, 1 ]                                           # comm. colleges outside of CA
M4     <- dfile[ 20:27, 1 ]                                           # 4-yr. universities  +  misc. categories
CList  <- list( M1, M2, M3, M4 )                                      # a meta-List used for x-axis labels                 

dt     <- data.frame( Colr = c( rep( '#82dd98', 10 ),                 # 10 light green cmc^3 regions
                                rep( '#80b599', 17 ) ),               # 17 dark  green other regions
                      data = dfile,                                                   
          stringsAsFactors = FALSE  )                                 # end d.f.

xLb <- paste0( 'def. Potential = ', c(
             'total f.t. + p.t. math faculty per CMC^3 CC, as of 2016',
             'total f.t. + p.t. math faculty per CMC^3-South CC, as of 2016',
             'roughly estimated to be 40 math faculty per CC outside of CA',
             'roughly estimated to be 40 math faculty per Institution' 
             ) )                                                      # 4 x-axis labels for '% Potential' PP style.

SclBy <- 500                               # Using a ScaleFactor allows for projecting to a globe w/out distortion. Leaflet fails w/ large 'degrees', even though it is good for zoom + pan.  
                                           #  ex. 395 / 500 < 1 is safe for leaflet, but 395 > 90 is too distorted.  

source( 'coordntes.R' )                    # create 27 objects ( or0 ... out12 ) from a text file in the same dir,
                                           #  each matrix dim = n x 2 (for a variable n of points in each polygon). 

sps   <- SpatialPolygons( list( Polygons(list( Polygon(oR0  )), ID =  '1'), Polygons(list( Polygon(oR1  )), ID =  '2'), Polygons(list( Polygon(oR2  )), ID =  '3'), Polygons(list( Polygon(oR3a ), Polygon(oR3b)), ID =  '4'), Polygons(list(  Polygon(oR4  )), ID =  '5'), Polygons(list( Polygon(oR5  )), ID =  '6'), Polygons(list( Polygon(oR6  )), ID =  '7'), Polygons(list( Polygon(oR7  )) , ID =  '8'), Polygons(list( Polygon(oR8  )) , ID =  '9'), 
                                Polygons(list( Polygon(oR9  )), ID = '10'), Polygons(list( Polygon(oRS1 )), ID = '11'), Polygons(list( Polygon(oRS2 )), ID = '12'), Polygons(list( Polygon(oRS3 )               ), ID = '13'), Polygons(list(  Polygon(oRS4 )), ID = '14'), Polygons(list( Polygon(oRS5 )), ID = '15'), Polygons(list( Polygon(out16)), ID = '16'), Polygons(list( Polygon(out17)) , ID = '17'), Polygons(list( Polygon(out18)) , ID = '18'), 
                                Polygons(list( Polygon(out19)), ID = '19'), Polygons(list( Polygon(out20)), ID = '20'), Polygons(list( Polygon(out21)), ID = '21'), Polygons(list( Polygon(out22)               ), ID = '22'), Polygons(list(  Polygon(out23)), ID = '23'), Polygons(list( Polygon(out24)), ID = '24'), Polygons(list( Polygon(out25)), ID = '25'), Polygons(list( Polygon(out26)) , ID = '26'), Polygons(list( Polygon(out27)) , ID = '27') 
                      ) )                                                # end def  sps (class sp) 'geometry'

spdf <- SpatialPolygonsDataFrame( sps, dt )                              # Also class sp.  Bind attendance data w/ geom.

HLOp <- highlightOptions( color = 'orange', weight = 5, bringToFront = TRUE              )   # 'AddPolygons' Options.
lbOp <- labelOptions( direction = 'left'  , noHide = T, textOnly = TRUE                  )   # 'AddLabels'   Options.
lbOz <- labelOptions( direction = 'top'   , noHide = T, textOnly = TRUE, offset = c(0,7) )   # pos. y  =  down

nYr    <-  13                                                            # 13 years from 2006 to 2018           

SpcBtn <-  12                                                            # SpaceBetween legend squares found by experimenting: prepares for sorting legend clicks.

noTitleCtrY <- 311                    # If 0 pixels are used for the title,              0.5(Lgnd) + sub-title + instr + padd + RadBtn + h4   + span + ClrBtn + padd
                                      #    then the Center y-coord of the Legend div. =  0.5(200)  + 55        + 16    + 13.6 + 45     + 19.2 + 25.6 + 32.4   + 7    pixels Height from the top of 'body'.

shinyApp( 
  ui     <- fluidPage( 
    tags$script( "
      $(document).on('shiny:sessioninitialized', function(event) {             // wait for shiny to be loaded
        Shiny.onInputChange( 'TitleHt', 
               document.getElementsByTagName('H4')[0].clientHeight ) ;         // variable titlePanel height
        document.getElementById('CoolLegnd').onclick = function(elem) { 
               Shiny.onInputChange( 'clkCoord', elem.clientY ) ;               // for de-bug  console.log(elem) ;
         };  }); "                        
     ),                                                                         # end js scripts sending (ht. info. to input$TitleHt) + (click info. to input$clkCoord).

    tags$head( tags$style(
      HTML( " .shiny-split-layout > div     { overflow:   visible ; }" ),       # ok w/ only 1 'div'.  https://stackoverflow.com/questions/40077388/shiny-splitlayout-and-selectinput-issue
            " #CoolLegnd { overflow-y: auto ; overflow-x: hidden  ; }"  
     ) ),                                                                       # end tags.  ?? Maybe not useful ??  Unknown how to decrease the RadioBtn padding.  Why not with this?

    span( textOutput( outputId = 'instr'),      
                         style = 'font-size:80%'                                # always-visible instructions
     ),

    titlePanel( h4( HTML( "Click to show conference attendance trends 
                           with the CA Mathematics Council of Community 
                           Colleges, CMC<sup>3</sup>" )                         # end html
       ),          windowTitle = 'click the map'                                # end h4
     ),                                                                         # end title

    span( textOutput( outputId = 'subTtl'),                                     # a sub-title
                         style = 'font-size: 80%'                               #   = always-visible instructions
     ),

    splitLayout(    cellWidths = c('24%', '20%', '56%'),                        # 3rd = min. 50% assuming unscaled Resrch[2] label to fit.
                      cellArgs = list(style = "padding: 9px"),                  # padding insures space between 'col.1' + 'col.2'              

      leafletOutput(  outputId = 'theMap', height = '72vh'
       ),                                                                       # end   1st 'col' out of 3.  

      fluidRow(                                                                 # start 2nd 'col'.
        radioButtons( inputId = 'RadBtn',                                       # 'width' can not be set here.
                        label = htmlOutput('mhURL'),                            # A good place to advertise mh since the space must be included, even if no button title is needed.
                  choiceNames = list(
                   HTML("<span style ='font-size:80%'>Discrete Count</span>"),  # The combination  'cNames' + 'cValues'  does allow HTML, even if  'choices'  does not.
                   HTML("<span style ='font-size:80%'>% of Potential</span>")
                                    ),                                          # end list
                 choiceValues = list('DC', 'PP'), selected = 'DC',  
                       inline = TRUE                                            # side-by-side horizontally
         ),                                                                     # end radio Btns

        actionButton( inputId = 'clrData', label = 'Clear All Selections', 
                                           style = 'padding: 8px ; 
                                                  font-size: 75% ;'
         ),                                                                     # end reset Btn

        h5(   textOutput( outputId = 'nSl')
         ),                                                                     # text 'n Selected', even if n = 0.

        span( htmlOutput( 'cnInst' ), style = 'font-size: 67% ;'
         ),                                                                     # conditional Instructions 

        plotOutput( 'CoolLegnd', height = '200px' 
         )                                                                      # end legend             

       ),                                                                       # end fluidR = 2nd 'col.' out of 3.

      plotOutput( 'CoolPlot', height = '325px' 
       )                                                                        # matrixplot = 3rd 'col.' out of 3.

     )                                                                          # end Layout
   ),                                                                           # end ui

  server <- function(input, output, session) {         # 7 outputs + 5 obs + 5 reactiveValues + 8 reactiveFunctions.                  
                                               
    output$subTtl  <- renderText( 'Click a region again to remove it.'          # this sub-title is always visible.  
     )                                                               

    output$cnInst  <- renderUI({
        if( n() == 0 ) { return(NULL) }
        else           { HTML( paste( 'Click the legend below',                 # these instr = conditionally vis.
                  'to de-select individual institutions.', sep = '<br/>' ) )
     } })

    output$nSl  <- renderText({
      if( n() == 1 ) { paste( n(), ' selected Institution: ' ) }                # 'number of Selected' text is always 
      else           { paste( n(), ' selected Institutions:' ) }                #      visible, even if n = 0 .
     })                                                         

    output$theMap <- renderLeaflet({                                            # map of CA + other regions + labels.
      leaflet( options = leafletOptions( minZoom = 7, maxZoom = 12 ))  %>%      # No useful details beyond 7 < 9 < 12 zoom.  
        addPolygons( data  = spdf, weight = 1, color = 'black',                 # default  weight = 5 
                     group = 'regions',    fillColor = spdf$Colr,               # two colors, depending if an official CMC^3 region (Northern CA)  or  not CMC^3 (all others).
               fillOpacity = 0.4, layerId = spdf$data.shortNm,                  # default  fillOpacity = 0.2
          highlightOptions = HLOp,  label = spdf$data.LongNms       )  %>%    

        setView( lng = spdf@bbox[3]/2, lat = spdf@bbox[4]/2,                    # [3] = xMax ,  [4] = yMax
                            zoom = 9                                )  %>%

        addPolylines( lng = c(122,224)/SclBy, lat = c(320,320)/SclBy, 
                          weight = 2,       color = 'black'         )  %>%      # a horiz. line separates the 'out of ca' squares from the 'in ca' ones.

        addCircleMarkers(    lng =  64/SclBy, lat = 180.5/SclBy, 
                          weight = 2,      radius = 4               )  %>%      # default rad = 10.  One circle for Monterey, CA.

        addLabelOnlyMarkers( lng =  63/SclBy, lat = 180.5/SclBy, 
                           label = 'Monterey' , labelOptions = lbOp )  %>%      # lbOp direction = 'left'

        addLabelOnlyMarkers( lng = 204/SclBy, lat = 292/SclBy  , 
                           label = 'pvt CA U' , labelOptions = lbOp )  %>%

        addLabelOnlyMarkers( lng = 204/SclBy, lat = 253/SclBy  , 
                           label = 'CSU'      , labelOptions = lbOp )  %>%

        addLabelOnlyMarkers( lng = 204/SclBy, lat = 214/SclBy  , 
                           label = 'UC'       , labelOptions = lbOp )  %>%

        addLabelOnlyMarkers( lng =  35/SclBy, lat =  88/SclBy  , 
                           label = 'misc.'    , labelOptions = lbOz )  %>%      # lbOz direction = 'top'

        addLabelOnlyMarkers( lng =  65/SclBy, lat =  28/SclBy  , 
                   label =   'cc\'s out of CA', labelOptions = lbOz )  %>%

        addLabelOnlyMarkers( lng = 173/SclBy, lat = 342/SclBy  ,                # end of labels
                   label = 'Univ\'s out of CA', labelOptions = lbOz ) 
     })                                                                         # end of map rendering

    output$CoolLegnd <- renderPlot( {  
      if( n() == 0 )   { return(NULL) }                                     # invisible legend if no selections
      par( mar = c( 0, 0, 0, 0 ) )
      legend(  x = 'center', legend = selC$C, y.intersp = 0.8,              # default interspacing = 1
            fill = rainbow( n(), start = 0.3,       end = 1    )            # end fill.  Skip yellow < 0.3 . 
            )                                                               # end legend
       }                                                                    # was height = function() { 170 + 11*n() }
     )                                                                      # end render legend

    output$CoolPlot  <- renderPlot( {                                       # 'col.3' ht. = '200 + 200' in 'col.2'
      if( n() == 0 )   { return(NULL) }                                     # invisible plot if no selections
      matplot(  x = c(2006:2018), y = ToGrph(), pch = 19,                   # a matrix plot for multiple trendlines at once
             type = 'o',       yaxp = ToYaxp(), cex = 1.2,                  # 'o' = overlay points + segments.
             xlab = ToXLab(),  ylab = ToYLab(), lty = 'solid',              #   x = 13 yr from 2006 to 2018.
             main = bquote(CMC^3~~'Monterey Conference'),                   # bqt works better than html here.
             col  = rainbow( n(), start = 0.3,  end = 1 )                   # plot 'col' must = legend 'fill'.
             )                                                              # end scatterplot
     })                                                                     # end render plot                          

    urlM <- a( "www.harbison.one", href = "http://www.harbison.one" )
    output$mhURL <- renderUI({ tagList( '', urlM )                          # an always-empty label to the left of the link
     })                                                                     # end hyperlinkRender

    clickdI   <- reactiveValues( ids = vector()
     )                                                                      # initially empty ex. 'a2', 'b5'

    selM      <- reactiveValues( eta = vector()
     )                                                                      # initially empty Meta (type of group) ex. 'M1'

    selC      <- reactiveValues(   C = vector(),                            # College name
                                  Yr = vector(),                            # attendance data by Year
                                  Pt = vector()                             # Potential n attendees by College
     )                                                                      # initially empty 3 types of data.  It would be nice to save a  13 x n  matrix  for  n Colleges, but only 1-column named char vectors are possible ??

    Bnds      <- reactiveValues( Lwr = vector(),
                                 Upr = vector()                             # initially empty lower + upper bound
     )                                                                      #   pixel bins for future legend clicks.

    Ctr       <- reactiveValues(   Y = vector()
     )                                                                      # initially empty y-coord( Legnd Center ).

    observeEvent( input$clrData, { session$reload()                         # ?? could be replaced by a method which retains current SetView ??
     })                                                                     # end resetButton

    observeEvent( input$theMap_shape_click, {                               # A different obs covers legend clicks.
      click       <- input$theMap_shape_click                               # Create an object for a clicked polygon on id 'theMap'
      clickdI$ids <- c( clickdI$ids, click$id )                             # Append all click id's to a vector that starts empty 

      if( anyDuplicated(clickdI$ids) ) {                                    #   if the current click ID already exists in the clicked polygon (from a prev. click)
         clickdI$ids <- clickdI$ids[ !clickdI$ids  %in%  click$id ]         #   then remove the current  click$id  from clickedPs ('toggle off')
       }                                                                    #   end conditional
      if( length(clickdI$ids) == 0  ||  n() == 0 ) {                        # reset the selected data either 
          selC$C  <- character(0)                                           #     if all regions have been 'clicked again'
          selC$Yr <- character(0)                                           #  or if all Institutions have been removed (by legend clicks).
          selC$Pt <- character(0)
       }                                                                    #   end conditional
      selM$eta  <- click$id                                                 # for x-axis 'def. potential' labels.
      clickedPs <- spdf                              %>% 
                   as.data.frame                     %>%                    # geometries dropped for df subset  
                   filter( data.shortNm  %in%  clickdI$ids )           
      u         <- unlist( clickedPs )                                      # one list
        tmpCc   <-     u[ grep('data.Camp', names(u)) ]                     # list of Campuses = Institutions = Colleges
      selC$C    <- tmpCc[           tmpCc != ''       ]                                 
        tmpYr   <-     u[ grep('data.X20' , names(u)) ]                     # matrix of data (names X2006 to X2018)
      selC$Yr   <- tmpYr[           tmpYr != ''       ]
        tmpPt   <-     u[ grep('data.surv', names(u)) ]                     # list of 'surveyRepsPot' = Potential
      selC$Pt   <- tmpPt[           tmpPt != ''       ]                     
      }, ignoreNULL = FALSE
     )                                                                      # end map_click obs

    observeEvent( input$TitleHt, {      
      Ctr$Y       <- noTitleCtrY + input$TitleHt                            # add  19px if 1 line  or  38px (or more) if a small window width.
     })                                                                     # end Center-y-coord obs

    observe( {                                                              # 'obsEvent' does not work here, since 2 events cause a change in Bounds: both map_click + legend_click
      req( Ctr$Y )                                                          # Bnds depend on Ctr$Y to be loaded.
      Bnds$Lwr <- seq( Ctr$Y - (n()/2)*SpcBtn,                              # l.b. for each group
                         length.out = n(), by = SpcBtn )                    #      for legend-click set-up
      Bnds$Upr <-   c( Bnds$Lwr[-1],                                        # u.b. for each group are 'less 1'
                         Ctr$Y + (n()/2)*SpcBtn ) - 1                       #      to avoid overlaps
     })                                                                     # end Bounds obs

    observeEvent( input$clkCoord, {                                         # js click event defined in ui
      yIn    <-   input$clkCoord
      DeSel  <-   which((  yIn >= Bnds$Lwr  )                               # l.b. + u.b. for each group of y-pixels 
                      & (  yIn <= Bnds$Upr  ))                              # index 1 or ... or n 
      if( length(DeSel) != 0 ) {        
          selC$C  <- selC$C [ - DeSel ]                                     # each legend click = remove one DeSel item 
          selC$Pt <- selC$Pt[ - DeSel ]                                     #   from the legend + plot.
           frstYr <- nYr *( DeSel - 1 ) + 1                                 # nYr init. 13 for 2006 to 2018
           lastYr <- nYr *  DeSel
          selC$Yr <- selC$Yr[ - (frstYr:lastYr) ]                           # All parts of selC must be done separately.
       }                                                                    # end conditional
      if( n() == 0 )  { clickdI$ids <- character(0) }                       # Erase the memory of old map clicks after all C's were removed from those regions of the map.
     })                                                                     # end Legend_click obs

    n      <- reactive({ length( selC$C )                                   # n = number of (Institutions =                        
     })                                                                     #     Campuses = Colleges) selected 

    SelD   <- reactive({ matrix( as.numeric(selC$Yr), nrow = nYr )          # structure the data.  Needs 'validate' to be safe (future version).
     })                                                                     # D = Discrete Counts.  13 years = 2006 to 2018. 

    SelP   <- reactive({ t( t( SelD() ) / as.numeric(selC$Pt) )             # use SelD's structure + divide by
     })                                                                     #   P = Proportions of Potentials.  The 'mat %*% diag( 1/k )' method fails when dim = 1x1.

    ToGrph <- reactive({ switch( input$RadBtn, DC = SelD(),                 # set the type of data plotted.
                                               PP = SelP(), SelD()          # the unnamedItem = default
     ) })  

    ToYaxp <- reactive({ switch( input$RadBtn, DC = c(0,50,50),             # tickMarks( low, up, nIntervals )   
                                               PP = c(0,1,40), c(0,50,50)   # DC: assume max < 50
     ) })                                                                   # PP: assume 40 tickmarks between 0 and 1, since min = 1/40. 

    ToXLab <- reactive({ switch( input$RadBtn, DC = '',                     # DC style requires no PP def.                              
                                               PP = Resrch(), ''            # Show x-axis definitions of PP  
     ) })                                                                   #     for  4  Meta types of groups.

    ToYLab <- reactive({ switch( input$RadBtn, 
      DC = 'number of attendees',  
      PP = '% of the Potential attendees by college', 'number of attendees' 
     ) })                                                                   # end y-axis labels

    Resrch <- reactive({ 
      i <- which( sapply( CList, function(w) { selM$eta %in% w } ) == 1 )   # index must be 1 or 2 or 3 or 4.
      switch( i, xLb[1], xLb[2], xLb[3], xLb[4] )                           # assume only 4 possible answers.
     })                                                                     # end Resrch func. for x-axis labels if PP.

  }                                                                         # end server
)                                                                           # end app





# fluidRow( verbatimTextOutput( outputId = 'dbg' ),                       # for de-bug in '4th col.' if 50% -> less in '3rd col.'
#           verbatimTextOutput( outputId = 'dbh' ) ),                     # for de-bug
#   output$dbg <- renderPrint( cat( length(Bnds$Lwr), 'l.b. since obs', Bnds$Lwr ) )   # send to ui for de-bug
#   output$dbh <- renderPrint( cat( length(Bnds$Upr), 'u.b. since obs', Bnds$Upr ) )   # send to ui for de-bug
