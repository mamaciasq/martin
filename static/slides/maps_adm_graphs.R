
source("maps_adm_script.R", encoding = 'UTF-8')

#### Mapa de colombia PEAMA por sede Orinoquia ####

pal_col3 <- colorBin(palette=c('#c7e9c0','#a1d99b','#74c476','#31a354','#006d2c'),bins=c(0,1,  101, 201, 500, Inf))


labelorinoquia <- sprintf(
  "%s <strong> %s </strong> (%s) <br/> <strong>%s</strong>",
  "DE", orinoquia$ciudad_asp, orinoquia$depart_asp, "PEAMA - ORINOQUIA"
) %>% lapply(htmltools::HTML)



icon_orinoquia <- icons(
  iconUrl = ifelse(orinoquia$sexo =="Hombres",  "boy.png","girl.png"), 25, 25,
  iconAnchorY = 13, iconAnchorX = 2
)


labels <- sprintf(
  "<strong>%s</strong><br/>%g admitidos",
  colombia.R@data$NOMBRE_DPT, colombia.R@data$ORINOQUIA
) %>% lapply(htmltools::HTML)



colombia4 <-leaflet(data=colombia.R)

for (k in c(1,2,3)) {
  colombia4 <- colombia4 %>% addProviderTiles(names(esri[k]) ,group = names.esri[k], options= providerTileOptions(minZoom=5))
} 


colombia4 <- colombia4 %>% addLayersControl( baseGroups = names.esri, overlayGroups = c("Mostrar sedes UNAL", "PEAMA - Orinoquía"),
                                             options = layersControlOptions(collapsed = T)) %>%
  
  setView(lng = centroidecol$lon, lat = centroidecol$lat, zoom = 6)%>%
  addPolygons(stroke = T, smoothFactor = 0.3, fillOpacity = 1, color="white", weight=2,
              fillColor = ~pal_col3(ORINOQUIA),
              label = labels, labelOptions=labelOptions(     style = list("font-weight" = "normal", padding = "3px 8px"),  textsize = "12px",direction = "auto"   ), highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = F)) %>%
  addLegend("bottomright", colors = c('#c7e9c0','#a1d99b','#74c476','#31a354','#006d2c'), values = ~ORINOQUIA, 
            title = "Admitidos",
            opacity = 1, bins = c(0,1, 101, 201, 500, Inf), labels = c("0","1 - 100 ", "101 - 200","200 - 500", "Más de 500"))%>%
  addLabelOnlyMarkers(lat=~capitaless$Latitud, lng=~capitaless$Longitud, label=  ~paste0(sapply(tolower(capitaless$`Nombre Municipio`),simpleCap)),  labelOptions = labelOptions(zoomAnimation=T, noHide = T, textOnly = T,textsize="8px") )%>%
  addLabelOnlyMarkers(lat=centro_dept$lat, lng=centro_dept$lon,label=  ~paste0(sapply(tolower(NOMBRE_DPT),simpleCap)),  labelOptions = labelOptions(zoomAnimation=T, noHide = T, direction = 'top', textOnly = T,textsize="9px") )%>%
  addMiniMap(position="bottomleft",zoomAnimation=T, toggleDisplay=T, autoToggleDisplay=T)%>%
  addEasyButton(easyButton(
    icon="glyphicon-screenshot", title="Retornar",
    onClick=JS("function(btn, map){ map.setView(L.latLng(4.796276,-74.11589), 6); }")))%>%
  addCircleMarkers(radius=2, fillOpacity= 0.9, stroke=T, color='#d95f02',fill= T, fillColor = "orangelight" ,lat=~capitaless$Latitud, lng=~capitaless$Longitud)%>%
  addAwesomeMarkers(lat=sedes$Latitud, lng=sedes$Longitud, icon = sedeIcon, label= label_sede, labelOptions=labelOptions(     style = list("font-weight" = "large", padding = "3px 8px"),  textsize = "15px", direction = "auto"), group = "Mostrar sedes UNAL"  )%>%
  addScaleBar(position = "bottomleft",scaleBarOptions(metric=T, imperial= F))%>%
  showGroup("Mostrar sedes UNAL")%>%
  showGroup("PEAMA - Orinoquía") %>%
  addMarkers(data=orinoquia,group = "PEAMA - Orinoquía", lat = ~lat_asp, lng = ~long_asp,  label = labelorinoquia, labelOptions=labelOptions(direction = "auto",zoomAnimation=T, riseOnHover =T,     style = list("font-weight" = "normal", padding = "3px 8px")), icon = icon_orinoquia, clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = T, animate = T, maxClusterRadius = 70,singleMarkerMode= F, animateAddingMarkers = T ), clusterId = "individuos5") %>% 
  addEasyButton(easyButton(
    states = list(
      easyButtonState(
        stateName="unfrozen-markers",
        icon="ion-toggle",
        title="Freeze Clusters",
        onClick = JS("
                     function(btn, map) {
                     var clusterManager =
                     map.layerManager.getLayer('cluster', 'individuos5');
                     clusterManager.freezeAtZoom();
                     btn.state('frozen-markers');
                     }")
      ),
      easyButtonState(
        stateName="frozen-markers",
        icon="ion-toggle-filled",
        title="UnFreeze Clusters",
        onClick = JS("
                     function(btn, map) {
                     var clusterManager =
                     map.layerManager.getLayer('cluster', 'individuos5');
                     clusterManager.unfreeze();
                     btn.state('unfrozen-markers');
                     }")
      )
        )
        ))

colombia4


