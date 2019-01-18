load("pred_length.RData")

d <- augment(dfa_1_equal, interval="confidence")
d$.rownames = revalue(d$.rownames, c("Y1"=spp.names[1], 
                                     "Y2"=spp.names[2],
                                     "Y3"=spp.names[3],
                                     "Y4"=spp.names[4],
                                     "Y5"=spp.names[5],
                                     "Y6"=spp.names[6]))
names(d)[1] <- "Species"
d$t <- tot_years[d$t]
require(ggsidekick)
#png("Figures/Fitted.png")
p <- ggplot(data = d, aes(fill=Species, color=Species)) +
  geom_line(aes(t, .fitted)) +
  geom_point(aes(t, y)) +
  geom_ribbon(aes(x=t, ymin=.conf.low, 
                  ymax=.conf.up), linetype=2, 
              alpha=0.2) +
  facet_wrap(vars(Species)) +
  xlab("Year") + ylab("Scaled Length") + 
  scale_fill_manual(values=colors_to_use)+
  scale_color_manual(values=colors_to_use)+
  theme_sleek() +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1))
print(p)
