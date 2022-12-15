#### Load packages ####
library(readxl)
library(dplyr)
library(lemon)
library(ggplot2)
library(wesanderson)
library(GGally)

library(googledrive)
library(patchwork)
library(ggplot2)
library(magick)
library(tidyverse)
library(svglite)

#### Read google drive sheets ####
setwd("~/ShuramOman")
drive_download("Shuram Oman Project", type = "csv", overwrite = TRUE)
1

rawdata <-read.csv("Shuram Oman Project.csv", stringsAsFactors = FALSE)


#theme shortcut
bw_shortcut <- theme_bw() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  panel.border = element_rect(fill = NA),
                                  legend.title= element_blank(),
                                  # axis.line = element_line(colour = "black"),
                                  # axis.title.y = element_text(size = 15),
                                  #strip.text.x = element_text(size = 15),
                                  strip.background = element_blank(), 
                                  strip.placement = "outside",
                                  text = element_text(size = 18)
)


#subset dataframe to be only what i want
dataSO <- rawdata[,c(1:7,13,31,44,53:58,60)] #minus samples I don't want to analyze
dataSO <- dataSO[dataSO$analyzing. == "yes",]
as.data.frame(dataSO)

data_all <- rawdata[c(1:7,13,31,44,53:58,60)] #minus procedural blank, including diagenesis model
as.data.frame(data_all)

#renamed data frame to easier column names
names(dataSO) <- c("sampleID","names", "formation", "minerology", "height", "d13c", 
                                    "d18o","rad", "stab", "d44", "MnSr", "MgCa", 
                   "SrCa", "MnCa", "d44higg", "prim_min","analyzing?")

names(data_all) <-  c("sampleID","names", "formation", "minerology", "height", "d13c", 
                      "d18o","rad", "stab", "d44", "MnSr", "MgCa", 
                      "SrCa", "MnCa", "d44higg", "prim_min","analyzing?")

factor(dataSO$formation, levels = c("Khufai", "Shuram", "Shuram/Buah", "Buah"))
factor(dataSO$minerology, levels = c("Calcite", "Dolomite"))
factor(dataSO$prim_min, levels = c("Primary Aragonite", "Primary Calcite", "Dolomite"))


factor(data_all$formation, levels = c("Khufai", "Shuram", "Shuram/Buah", "Buah", "Dengying", "Doushantuo"))
factor(data_all$minerology, levels = c("Calcite", "Dolomite"))
factor(data_all$prim_min, levels = c("Primary Aragonite", "Primary Calcite", "Dolomite", "dolomite", "limestone"))



## facet wrap ##
solong <- tidyr::pivot_longer(data = dataSO, c("d13c", "d18o","rad", "stab", "d44"), 
                              names_to = "parameter", values_to = "values", values_drop_na = TRUE)


solong$formation_f <- factor(solong$formation, levels = c("Khufai", "Shuram","Shuram/Buah", "Buah"))

solong$parameter_f <- factor(solong$parameter, levels = c("d13c", "d18o","rad", "stab", 
                                                          "d44", "height", "minerology"),
                               labels = c(paste("δ^{13}*C*",expression(paste("  (VPDB, ‰)"))),
                                          paste("δ^{18}*O*",expression(paste("  (VPDB, ‰)"))),
                                          "NULL^{87}*Sr/NULL^{86}*Sr",
                                          paste("δ^{88/86}*Sr*", expression(paste("  (NBS 987, ‰)"))), 
                                          paste("δ^{44/40}*Ca*", expression(paste("  (SW, ‰)"))) ,
                                          "Height (m)", "Minerology") )


#i was trying to remove d18o from figure 1 but idk whats going on 
# solong$parameter_f2 <- factor(solong2$parameter2, levels = c("d13c","rad", "stab", 
#                                                           "d44", "height", "minerology"),
#                              labels = c(paste("δ^{13}*C*",expression(paste("  (VPDB, ‰)"))),
#                                         "NULL^{87}*Sr/NULL^{86}*Sr",
#                                         paste("δ^{88/86}*Sr*", expression(paste("  (NBS 987, ‰)"))), 
#                                         paste("δ^{44/40}*Ca*", expression(paste("  (SW, ‰)"))) ,
#                                         "Height (m)", "Minerology") )


#i was trying to remove d18o from figure 1 but idk whats going on 
# solong2 <- tidyr::pivot_longer(data = dataSO_edited, c("d13c","rad", "stab", "d44"), 
#                               names_to = "parameter2", values_to = "values", values_drop_na = TRUE)

#all data


first_fig_shuram2 <-
  solong %>%
  drop_na(prim_min) %>%
  drop_na(formation_f) %>%
  ggplot(aes(x= values, y = height)) + 
  lemon:: facet_rep_grid(. ~ parameter_f, scales = "free_x", 
                         labeller = label_parsed, switch = "x") + #facet grid labelparsed pretty labels (bottom)
  ggplot2:: geom_point( aes(shape = prim_min, color= formation_f), size = 5) + 
  scale_color_manual(values = c("orange", "red", "#56B4E9", "blue")) + 
  bw_shortcut + theme(title = element_blank(), text = element_text(family ="serif"), 
        strip.background = element_blank(), strip.text = element_text(size =18), strip.placement = "outside", 
        legend.title= element_blank(), panel.border = element_rect(linewidth = 1.5) ) +
  scale_x_continuous(name = element_blank()) + 
  scale_y_continuous(name = element_blank(),breaks = seq(0, 1000, by = 100))

#save as svg
#ggsave("d13c_mainfig_shuram.svg", first_fig_shuram2, height= 8, width = 22)

solong_excursion <- solong[solong$height > 250 & solong$height < 400,]


solong_excursion$parameter_g <- factor(solong_excursion$parameter, levels = c("d13c", "d18o","rad", "stab", 
                                                          "d44", "height", "minerology"),
                             labels = c(paste("δ^{13}*C*",expression(paste("  (VPDB, ‰)"))),
                                        paste("δ^{18}*O*",expression(paste("  (VPDB, ‰)"))),
                                        "NULL^{87}*Sr/NULL^{86}*Sr",
                                        paste("δ^{88/86}*Sr*", expression(paste("  (NBS 987, ‰)"))), 
                                        paste("δ^{44/40}*Ca*", expression(paste("  (SW, ‰)"))) ,
                                        "Height (m)", "Minerology") )

### zoomed in on excursion long plot 


solong_excursion %>% 
  drop_na(height) %>% ggplot(aes(x= values, y = height)) + 
  lemon:: facet_rep_grid(. ~ parameter_g, scales = "free_x", 
                         labeller = label_parsed, switch = "x") + #facet grid labelparsed pretty labels (bottom)
  ggplot2:: geom_point(aes(shape = prim_min, color= formation), size = 5) + 
  scale_color_manual(values = c("orange", "red", "#56B4E9", "blue")) + 
  bw_shortcut + theme(title = element_blank(), text = element_text(family ="serif"), 
                      strip.background = element_blank(), strip.text = element_text(size = 15), 
                      strip.placement = "outside", legend.title= element_blank(),
                      panel.border = element_rect(size = 1.5) ) +
  scale_x_continuous(name = element_blank()) + 
  scale_y_continuous(name = element_blank())
  


# 
# #individual data
# # d13C
# d13c_fig <- ggplot(data = dataSO, aes( x =d13c, y = height)) + geom_point(size = 3) + bw_shortcut+
#   scale_color_gradientn(colors = wes_palette(name = "Zissou1", n =5))+ 
#   labs(y = "Height (m)", x = expression(δ^{13}*C))
# 
# #d18O
# d18o_fig <- ggplot(data = dataSO, aes( x =d18o, y = height)) + geom_point(size = 3) + bw_shortcut+
#   scale_color_gradientn(colors = wes_palette(name = "Zissou1", n =5))+ 
#   labs(y = "Height (m)", x = expression(δ^{18}*O) )
#                                    
# #rad
# rad_fig <- ggplot(data = dataSO, aes( x =rad, y = height)) + geom_point(size = 3) + bw_shortcut+
#   labs(y = "Height (m)", x = expression(""^{87}*Sr*"/"^{86}*Sr))
# 
# #stab (weird stab values dont have )
# stab_fig <- ggplot(data = dataSO, aes( x =stab, y = height, color = Srconc)) + geom_point(size = 3) + bw_shortcut+
#   scale_color_gradientn(colors = wes_palette(name = "Zissou1", n =5))+ 
#   labs(y = "Height (m)", x = expression(δ^{88/86}*Sr))
# 
# #ca
# ca_fig <- ggplot(data = dataSO, aes( x =d44, y = height)) + geom_point(size = 3) + bw_shortcut+
#   scale_color_gradientn(colors = wes_palette(name = "Zissou1", n =5))+ 
#   labs(y = "Height (m)", x = expression(δ^{44/40}*Ca))
# 
# #Sr conc
# sr_fig <- ggplot(data = dataSO, aes( x =Srconc, y = height)) + geom_point(size = 3) + bw_shortcut+
#   scale_color_gradientn(colors = wes_palette(name = "Zissou1", n =5))+ 
#   labs(y = "Height (m)", x = expression(Sr))
# 
# #all on one plot
# #sr & ca (weird stab values dont have heights yet so they dont show up )
# plot(dataSO$d44,dataSO$height,pch=20,col="red", xaxt="n", type= "p", xlab= " ", ylab = " ") 
# mtext(expression(δ^{44/40}*Ca), side=3, line=2, cex.lab=1, col="red")
# axis(side = 3)
# par(new=TRUE)
# plot(dataSO$stab,dataSO$height,pch=20,col="blue", xaxt="n", xlab = " ", ylab = "Height (m)" )
# mtext(expression(δ^{88/86}*Sr), side=1, line=2, cex.lab=1, col="blue")
# axis(side =1)


#correlogram!
ggcorr(dataSO, method = c("pairwise", "pearson")) 


#cross plot #main figure 
#sr vs ca cross plots 

#unfiltered data (weird stab values)
# ggplot(data = dataSO, aes(x=d44, y = stab, shape = prim_min, color = rad)) + geom_point(size =3) +
#   scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) +
#   scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
#   #geom_smooth(method = 'lm', se = FALSE) +
#    bw_shortcut +
#   theme (panel.border = element_rect(size = 1.5) ) + scale_color_gradientn(colors = wes_palette(name = "Zissou1"))




# ggplot(data = dataSO, aes(x=d44, y = stab, color = SrCa)) + 
#   geom_point(size =3, data = dataSO, aes(shape=minerology)) +
#   scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
#   scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
#   geom_smooth(method = 'lm', se = FALSE) + 
#   bw_shortcut + 
#   theme (panel.border = element_rect(size = 1.5) ) + 
#   scale_color_gradientn(colors = wes_palette(name = "Zissou1"))


#linear regression
summary(lm(stab ~ d44, dataSO))

dataSO_filter2 <- dataSO[dataSO$stab > 0 & dataSO$stab < 2, ] #doesnt include outliers in stab 
dataSO_filter3 <- dataSO[dataSO$minerology == "Calcite", ]
dataSO_filter4 <- dataSO[dataSO$minerology == "Dolomite", ]
dataSO_filter5 <- dataSO[c(1:24),]  # just samples 1-25B, doesnt include new samples 
dataSO_filter11 <- dataSO[dataSO$SrCa < 7,]
# dataSO_filter6 <- dataSO[dataSO$stab > -2 & dataSO$stab < 3 & "MgCa" > 0, ] #only includes MgCa values that have stab and Ca values + outliers
# dataSO_filter7 <- dataSO[dataSO$stab > 0 & dataSO$stab < 2 & "MgCa" > 0, ] #only includes MgCa values that have stab and Ca values 
# dataSO_filter71 <- dataSO[dataSO$stab > -2 & dataSO$stab < 3 & dataSO$d44 < 0 & "SrCa" > 0, ] #only includes MgCa values that have stab and Ca values + outliers
# dataSO_filter8 <- dataSO[dataSO$stab > -2 & dataSO$stab < 3 & dataSO$d44 < 0 & "MnCa" > 0, ] #only includes MgCa values that have stab and Ca values + outliers
# dataSO_filter9 <- dataSO[dataSO$stab > 0 & dataSO$stab < 2 & dataSO$d44 < 0 & "MnCa" > 0, ] #only includes MgCa values that have stab and Ca values + outliers
# dataSO_filter10 <- dataSO[dataSO$stab > 0 & dataSO$stab < 2 & dataSO$d44 < 0 & "MnSr" > 0, ] #only includes MnSr values that have stab and Ca values + no outliers
dataSO_filter_primcalcite <- dataSO[dataSO$prim_min == "Primary Calcite", ]

dataSO_nomountains <- dataSO[-c(25:29) ,]
dataSO_mountains <- dataSO[dataSO$names == "SO23" | dataSO$names == "SO24" | dataSO$names == "SO25A" | 
                             dataSO$names == "SO25B" | dataSO$names == "SO26", ]
Zissou1 <- c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00")
# Main Cross Plot Figure (Sr vs Ca with height colored and minerology as shape --------
######## second slide #########

#second_slide <-
  
  ggplot(data = dataSO, aes(x= d44, y = stab, color = d13c)) + 
  #scale_fill_distiller('pr',palette='Spectral', breaks = c(200, 400)) +
  geom_point(size =5, data = dataSO, aes(shape=prim_min)) +
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  #scale_x_continuous(name = expression(NULL^{87}*Ss/NULL^{86}*Sr)) + 
  scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
  bw_shortcut + theme (legend.title = element_text(size=14)) + 
  labs(shape="Primary Minerology", color=" δ13C") +
  theme (panel.border = element_rect(size = 1.5)) +
  scale_colour_gradientn(colors = wes_palette(name = "Zissou1")) + scale_shape_discrete(na.translate = F) +
  theme(legend.position=c(0.15, 0.33), legend.background = element_rect( size=0.5, linetype="solid", colour ="black"))
  #scale_colour_gradient2(guide = 'colorbar', low = 'blue', mid = 'orange', high = "red", midpoint = 0)

#ggsave( "SrvCa_Shuram_minerology.svg", second_slide, height= 7, width = 8.5)




# #make plot of Sr/Ca vs d44  --------------------------------------------- Tang and Ahm 2018

dataSO_filter11 %>%

ggplot( aes(x= d44, y = SrCa, color = d13c)) + 
  #scale_fill_distiller('pr',palette='Spectral', breaks = c(200, 400)) +
  geom_point(size =5, aes(shape=prim_min)) +
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  #scale_x_continuous(name = expression(NULL^{87}*Ss/NULL^{86}*Sr)) + 
  scale_y_continuous(name = "Sr/Ca (ppm/ppm)") +
  bw_shortcut + theme (legend.title = element_text(size=14)) + 
  labs(shape="Primary Minerology", color=" δ13C") +
  theme (panel.border = element_rect(size = 1.5)) +
  scale_colour_gradientn(colors = wes_palette(name = "Zissou1")) + scale_shape_discrete(na.translate = F) #+
  #theme(legend.position=c(0.15, 0.33), legend.background = element_rect( size=0.5, linetype="solid", colour ="black"))
summary(lm(SrCa ~ d44, dataSO_filter11))

##### plot of Mn/Sr vs d13c -- removing the really high Mn/Sr that was like 140 (SO23)

dataSO[dataSO$MnSr <50 ,] %>%
  
  ggplot( aes(x= d13c, y = MnSr, color = MnCa)) +
  geom_point(size =5, aes(shape=prim_min)) +
  scale_x_continuous(name = expression(delta^{13}*C~("VPDB,‰"))) + 
  scale_y_continuous(name = "Mn/Sr (mmol/mmol)") +
  bw_shortcut + theme (legend.title = element_text(size=14)) + 
  labs(shape="Primary Minerology", color="Mg/Ca (mmol/mol") +
  theme (panel.border = element_rect(size = 1.5)) +
  scale_colour_gradientn(colors = wes_palette(name = "Zissou1")) + scale_shape_discrete(na.translate = F) 


#sorted by minerology - just prim calcite  - normalized axis
ggplot(data = dataSO_filter3, aes(x=d44, y = stab, color = formation)) + 
  geom_point(size =5, data = dataSO_filter3, aes(shape=prim_min)) +
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰")), limits= c(-1.4,0)) + 
  scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰")), limits= c(0,0.5)) +
  bw_shortcut + theme (legend.title = element_text(size=14)) + 
  labs(shape="Primary Minerology", color="Formation") +
  theme (panel.border = element_rect(size = 1.5))# + 
  #scale_color_gradientn(colors = wes_palette(name = "Zissou1"))



#sorted by minerology - just dolomite - normalized axis 
ggplot(data = dataSO_filter4, aes(x=d44, y = stab, color = formation)) + 
  geom_point(size =5, data = dataSO_filter4, aes(shape=prim_min)) +
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰")), limits= c(-1.4,0)) + 
  scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰")), limits= c(0,0.5)) +
  bw_shortcut + theme (legend.title = element_text(size=14)) + 
  labs(shape="Primary Minerology", color="Formation") +
  scale_shape_manual(values = c("Primary Aragonite" = 2, "Primary Calcite" = 0)) + scale_shape_discrete(solid = T) +
  theme (panel.border = element_rect(size = 1.5)) #+ 
  #scale_color_gradientn(colors = wes_palette(name = "Zissou1"))


  
  
  
  
  
  
  
  
  
  
  

# d13c and d18O -----------------------------------------------------------

  
#d18o vs d13C colored by rad
ggplot(data = dataSO_nomountains, aes(x=d18o, y = d13c, color = rad)) + 
  geom_point(size =5, data = dataSO_nomountains, aes(shape=prim_min)) +
  scale_x_continuous(name = expression(delta^{18}*O~("SMOW,‰"))) + 
  scale_y_continuous(name = expression(delta^{13}*C~("VPDB, ‰"))) +
  bw_shortcut + theme (legend.title = element_text(size=14)) + 
  labs(shape="Primary Minerology", color="87Sr/86Sr") +
  theme (panel.border = element_rect(size = 1.5)) + 
  scale_color_gradientn(colors = wes_palette(name = "Zissou1"))

#linear regression
summary(lm(d18o ~ d13c, dataSO_nomountains))# 0.29 r squared with mountians # 0.69 without mountians

#summary(lm(stab ~ d44, dataSO_filter2))


# data SO elemental plots -------------------------------------------------


#sr vs ca by formation
dataSO_khufai <- dataSO[dataSO$formation == "Khufai",]
dataSO_shuram <- dataSO[dataSO$formation == "Shuram",]
dataSO_SB <- dataSO[dataSO$formation == "Shuram/Buah",]
dataSO_buah <- dataSO[dataSO$formation == "Buah",]

#by formation long form data frame 
khuf_long <- solong[solong$formation == "Khufai",]
shuram_long <-  solong[solong$formation == "Shuram",]
SB_long <-  solong[solong$formation == "Shuram/Buah",]
buah_long <-  solong[solong$formation == "Buah",]

# all data by formation ---------------------------------------------------

#all data khuf
ggplot(data = khuf_long, aes(x= values, y = height, color = parameter)) + 
  facet_rep_grid(. ~ parameter_f, scales = "free_x", labeller = label_parsed, switch = "x") + #facet grid labelparsed pretty labels (bottom)
  geom_point(size = 2) + bw_shortcut + 
  theme(title = element_text(size =10), text = element_text(family ="serif"), 
        strip.background = element_blank(), strip.text = element_text(size = 12), strip.placement = "outside", 
        legend.title= element_blank(), panel.border = element_rect(size = 1.5) ) + 
  scale_x_continuous(name = element_blank()) + scale_y_continuous(name = "Height (m)")



#all data shuram
ggplot(data = shuram_long, aes(x= values, y = height, color = parameter)) + 
  facet_rep_grid(. ~ parameter_f, scales = "free_x", labeller = label_parsed, switch = "x") + #facet grid labelparsed pretty labels (bottom)
  geom_point(size = 2) + bw_shortcut + 
  theme(title = element_text(size =10), text = element_text(family ="serif"), 
        strip.background = element_blank(), strip.text = element_text(size = 12), strip.placement = "outside", 
        legend.title= element_blank(), panel.border = element_rect(size = 1.5) ) + 
  scale_x_continuous(name = element_blank()) + scale_y_continuous(name = "Height (m)")

#all data SB
ggplot(data = SB_long, aes(x= values, y = height, color = parameter)) + 
  facet_rep_grid(. ~ parameter_f, scales = "free_x", labeller = label_parsed, switch = "x") + #facet grid labelparsed pretty labels (bottom)
  geom_point(size = 2) + bw_shortcut + 
  theme(title = element_text(size =10), text = element_text(family ="serif"), 
        strip.background = element_blank(), strip.text = element_text(size = 12), strip.placement = "outside", 
        legend.title= element_blank(), panel.border = element_rect(size = 1.5) ) + 
  scale_x_continuous(name = element_blank()) + scale_y_continuous(name = "Height (m)")

#all data Buah
ggplot(data = buah_long, aes(x= values, y = height, color = parameter)) + 
  facet_rep_grid(. ~ parameter_f, scales = "free_x", labeller = label_parsed, switch = "x") + #facet grid labelparsed pretty labels (bottom)
  geom_point(size = 2) + bw_shortcut + 
  theme(title = element_text(size =10), text = element_text(family ="serif"), 
        strip.background = element_blank(), strip.text = element_text(size = 12), strip.placement = "outside", 
        legend.title= element_blank(), panel.border = element_rect(size = 1.5) ) + 
  scale_x_continuous(name = element_blank()) + scale_y_continuous(name = "Height (m)")


#sr vs ca Khufai
ggplot(data = dataSO_khufai, aes(x=d44, y = stab)) + geom_point() +  
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
  geom_smooth(method = 'lm', se = FALSE) + bw_shortcut + 
  theme (panel.border = element_rect(size = 1.5)) + scale_color_gradientn(colors = wes_palette(name = "Zissou1"))

summary(lm(stab ~ d44, dataSO_khufai))

#sr vs ca Shuram
ggplot(data = dataSO_shuram, aes(x=d44, y = stab)) + geom_point() +  
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
  geom_smooth(method = 'lm', se = FALSE) + bw_shortcut + 
  theme (panel.border = element_rect(size = 1.5)) + scale_color_gradientn(colors = wes_palette(name = "Zissou1"))

summary(lm(stab ~ d44, dataSO_shuram))

#sr vs ca SB
ggplot(data = dataSO_SB, aes(x=d44, y = stab)) + geom_point() +  
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
  geom_smooth(method = 'lm', se = FALSE) + bw_shortcut + 
  theme (panel.border = element_rect(size = 1.5)) + scale_color_gradientn(colors = wes_palette(name = "Zissou1"))

summary(lm(stab ~ d44, dataSO_SB))

#sr vs ca Buah
ggplot(data = dataSO_buah, aes(x=d44, y = stab)) + geom_point() +  
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
  geom_smooth(method = 'lm', se = FALSE) + bw_shortcut + 
  theme (panel.border = element_rect(size = 1.5)) + scale_color_gradientn(colors = wes_palette(name = "Zissou1"))

#all data 

#rad vs ca
ggplot(data = dataSO, aes(x=d44, y = rad, color = Srconc)) + geom_point() +  
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  scale_y_continuous(name = expression(NULL^{87}*Sr/NULL^{86}*Sr)) +
  geom_smooth(method = 'lm', se = FALSE) + bw_shortcut + 
  theme (panel.border = element_rect(size = 1.5)) + scale_color_gradientn(colors = wes_palette(name = "Zissou1"))

# d13c vs ca
ggplot(data = dataSO, aes(x=d44, y = d13c)) + geom_point() +  
  #scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  #scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
  geom_smooth(method = 'lm', se = FALSE) + bw_shortcut + 
  theme (panel.border = element_rect(size = 1.5)) + scale_color_gradientn(colors = wes_palette(name = "Zissou1"))
summary(lm(d13c ~ d44, dataSO))

# stab vs d13c
ggplot(data = dataSO, aes(x=d13c, y = stab)) + geom_point() +  
  #scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  #scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
  geom_smooth(method = 'lm', se = FALSE) + bw_shortcut + 
  theme (panel.border = element_rect(size = 1.5)) + scale_color_gradientn(colors = wes_palette(name = "Zissou1"))





# all data plots ----------------------------------------------------------

#second_slide2 <-

  data_all %>%
  drop_na(prim_min) %>% 
  ggplot(aes(x= d44, y = stab, color = rad)) + 
  geom_point(size =5, aes(shape=prim_min)) +
  scale_x_continuous(name = expression(delta^{44/40}*Ca~("SW,‰"))) + 
  #scale_x_continuous(name = expression(NULL^{87}*Sr/NULL^{86}*Sr)) + 
  scale_y_continuous(name = expression(delta^{88/86}*Sr~("NIST 987, ‰"))) +
  bw_shortcut + theme (legend.title = element_text(size=14)) + 
  labs(shape="Primary Minerology", color="rad") +
  theme (panel.border = element_rect(size = 1.5)) + 
  scale_color_gradientn(colors = wes_palette(name = "Zissou1"))



