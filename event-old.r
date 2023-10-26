#l
# load("data/leopard_rda/hmm_df.rda")
#
# # RSF
# l %>%
#   with(mapply(function(x,y) st_point(c(x,y)) ,x,y, SIMPLIFY=F)) %>%
#   st_sfc(crs=st_get_crs("utm40")) %>%
#   rsf_ready(hmm_df$time, samples = length(.) * 5)  -> yy
#
# read_gis_data() %>%
#     with(spatial_covariates(yy,elevation,shps,hfi)) -> yy
#
# #yy %>% as.data.frame %>% subset(select=-geom) -> yy
#
# # hmm mode null
# (yy %>%
#   merge(hmm_df[,c('mode_null','time')],by="time",all.x=T) -> yy) %>% str
#
# yy$c1_dead <- (yy[,'time',drop=T][[1]] >= t_c1_death)
# yy$ch_after_first_big <- (yy[,'time',drop=T][[1]] >= first_big)
#
# names(l)[3] <- 'date'
# (l_bef_aft <- l %>% dl(proj4string = st_get_crs("utm40") %>% CRS) %>% ld %>%
#   merge(yy,by.x='date',by.y='time',all.y=T,sort=F)) %>% str

#save(l_bef_aft,file="data/leopard_rda/l_bef_aft.rda")


# cheetah
# load("data/cheetah_rda/hmm_df.rda")
#
# # RSF
# c1 %>%
#   with(mapply(function(x,y) st_point(c(x,y)) ,x,y, SIMPLIFY=F)) %>%
#   st_sfc(crs=st_get_crs("utm40")) %>%
#   rsf_ready(hmm_df$time, samples = length(.) * 5)  -> yy
#
# read_gis_data() %>%
#   with(spatial_covariates(yy,elevation,shps,hfi)) -> yy
#
# #yy %>% as.data.frame %>% subset(select=-geom) -> yy
#
# # hmm mode null
# (yy %>%
#     merge(hmm_df[,c('mode_null','time')],by="time",all.x=T) -> yy) %>% str
#
# #yy$c1_dead <- (yy[,'time',drop=T][[1]] >= t_c1_death)
# yy$ch_after_first_big <- (yy[,'time',drop=T][[1]] >= first_big)
#
# names(c1)[3] <- 'date'
# (c1_bef_aft <- c1 %>% dl(proj4string = st_get_crs("utm40") %>% CRS) %>% ld %>%
#     merge(yy,by.x='date',by.y='time',all.y=T,sort=F)) %>% str

#save(c1_bef_aft,file="data/leopard_rda/c1_bef_aft.rda")

# RSF ---------------------------------------------------------------------
#c1_dead
(fit_aft <- glm(used~log(water_dist) + poly(elevation,2)+poly(path_dist,1)+poly(highway_dist,2),#+poly(village_dist,2),
                l_bef_aft,subset = c1_dead,family='binomial')) %>% summary

(fit_bef <- glm(used~log(water_dist) + poly(elevation,2)+poly(path_dist,2)+poly(highway_dist,2)+poly(village_dist,2),
                l_bef_aft,subset = !c1_dead,family='binomial')) %>% summary

#pdf("plots/bef_aft/l_rsf_bef_after_c1_kill.pdf")
png_inch("plots/bef-aft/l-rsf-bef-after-c1-kill-%02d.png",
         width=28,
         height=28)
#par_reset()

par(mar=c(3,3,1.5,0), mgp=c(1.5,.5,0), tck=-0.01, cex= 1.5, cex.axis=1.25, cex.lab=1.25, bty="l", cex.main=1.1)

#RSF of leopard before c1 death
layout(matrix(1:6,2,byrow = T))
termplot(fit_bef,se=T,partial.resid = T,ylim=c(-15,15),
         xlabs = c("Water distance (m)","Elevation (m)","Pathway distance (m)","Highway distance (m)","Village distance (m)"),
         ylabs = c("Partials for 2nd order poly") %>% rep(5),
         main="before",
         lwd.term = 2,col.res = color_alpha("grey",.2))
#RSF of leopard after c1 death
layout(matrix(1:6,2,byrow = T))
termplot(fit_aft,se=T,partial.resid = T,ylim=c(-15,15),
         xlabs = c("Water distance (m)","Elevation (m)","Pathway distance (m)","Highway distance (m)"),
         ylabs = c("Partials for 2nd order poly","Partials for 2nd order poly","Partials for 1st order poly","Partials for 2nd order poly"),
         main="after",
         lwd.term = 2,col.res = color_alpha("grey",.2))
dev.off()

#Leo
(fit_aft <- glm(used~poly(water_dist,1) + poly(elevation,2)+poly(path_dist,2)+poly(highway_dist,1),
                c1_bef_aft,subset = ch_after_first_big,family='binomial')) %>% summary

(fit_bef <- glm(used~poly(water_dist,2) + poly(elevation,2)+poly(path_dist,2)+poly(village_dist,2),
                c1_bef_aft,subset = !ch_after_first_big,family='binomial')) %>% summary
#pdf("plots/bef_aft/c1_rsf_bef_after_big.pdf")
png_inch("plots/bef-aft/c1-rsf-bef-after-big-%02d.png")

par_reset()
layout(matrix(1:4,2))
par(mar=c(3,3,1.5,0),mgp=c(1.5,.5,0),tck=-0.01,cex.axis=1.25,cex.lab=1.25,bty="l",cex.main=1.1)
#CCPR
termplot(fit_bef,se=T,partial.resid = T,ylim=c(-15,15),
         main="before",
         xlabs = c("Water distance (m)","Elevation (m)","Pathway distance (m)","Village distance (m)"),
         ylabs = "Partials for 2nd order poly" %>% rep(4),lwd.term = 2,col.res = color_alpha("grey",.2))
#dev.off()
#png_inch("plots/bef-aft/c1-rsf-bef-after-big2.png")

termplot(fit_aft,se=T,partial.resid = T,ylim=c(-15,15),
         xlabs = c("Water distance (m)","Elevation (m)","Pathway distance (m)","Highway distance (m)"),
         ylabs = c("Partials for 1st order poly","Partials for 2nd order poly","Partials for 2nd order poly","Partials for 1st order poly"),
         main="after",
         lwd.term = 2,col.res = color_alpha("grey",.2))
dev.off()
