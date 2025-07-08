
#plot themes:
theme <-  theme(panel.background = element_rect(fill="white",colour="azure2"), 
                legend.position = "bottom",
                panel.grid.major = element_line(colour="azure2") ,
                axis.line.x.bottom = element_line(colour="black") ,
                axis.line.y.left = element_line(colour="black") ,
                plot.title = element_text(size=10, hjust=0.5),
                plot.subtitle = element_text(size=9, hjust=0.5),
                #  axis.title.x=element_blank(),
                axis.text=element_text(size=6),
                axis.title=element_text(size=8,
                                        face="bold"),
                #change stuff for facet labels
                strip.background =element_rect(fill="black", colour="black"),
                strip.text = element_text(colour = 'white', size=12))

themewithlegend <-  theme(panel.background = element_rect(fill="white",colour="azure2"), 
                          legend.position = "bottom",
                          panel.grid.major = element_line(colour="azure2") ,
                          axis.line.x.bottom = element_line(colour="black") ,
                          axis.line.y.left = element_line(colour="black") ,
                          plot.title = element_text(size=10, hjust=0.5),
                          plot.subtitle = element_text(size=9, hjust=0.5),
                          axis.text=element_text(size=6),
                          axis.title=element_text(size=8,
                                                  face="bold"),
                          #change stuff for facet labels
                          strip.background =element_rect(fill="black", colour="black"),
                          strip.text = element_text(colour = 'white', size=12))

themenolegend <-  theme(panel.background = element_rect(fill="white",colour="azure2"), 
                        legend.position = "none",
                        panel.grid.major.x = element_line(colour="azure2") ,
                        panel.grid.major.y = element_line(colour="azure2") ,
                        axis.line.x.bottom = element_line(colour="black") ,
                        axis.line.y.left = element_line(colour="black") ,
                        plot.title = element_text(size=10, hjust=0.5),
                        plot.subtitle = element_text(size=9, hjust=0.5),
                        axis.text=element_text(size=6),
                        axis.title=element_text(size=8,
                                                face="bold"),
                        #change stuff for facet labels
                        strip.background =element_rect(fill="black", colour="black"),
                        strip.text = element_text(colour = 'white', size=12))


theme8cm <-  theme(panel.background = element_rect(fill="white",colour="azure2"), 
                   legend.position = "none",
                   panel.grid.major.x = element_blank() ,
                   panel.grid.major.y = element_line(colour="azure2")  ,
                   axis.line.x.bottom = element_line(colour="black") ,
                   axis.line.y.left = element_line(colour="black") ,
                   plot.title = element_text(size=8, hjust=0.5),
                   plot.subtitle = element_text(size=8, hjust=0.5),
                   axis.text=element_text(size=6),
                   axis.title=element_text(size=6, face="bold"),
                   #change stuff for facet labels
                   strip.background =element_rect(fill="black", colour="black"),
                   strip.text = element_text(colour = 'white', size=12))

themehist <-  theme(panel.background = element_rect(fill="white",colour="azure2"), legend.position = "none",
                    panel.grid.major = element_line(colour="azure2") ,
                    axis.line.x.bottom = element_line(colour="black") ,
                    axis.line.y.left = element_line(colour="black") ,
                    plot.title = element_text(size=9, hjust=0.5),
                    plot.subtitle = element_text(size=9, hjust=0.5),
                    axis.text=element_text(size=9),
                    axis.title=element_text(size=9,
                                            face="bold"),
                    #change stuff for facet labels
                    strip.background =element_rect(fill="black", colour="black"),
                    strip.text = element_text(colour = 'white', size=12))

#powerpoint themes have now been tweaked to best render at 6x3 (so text is slightly smaller)
themepowerpoint <-  theme(panel.background = element_rect(fill="white",colour="azure2"), legend.position = "none",
                          panel.grid.major = element_line(colour="azure2") ,
                          axis.line.x.bottom = element_line(colour="black") ,
                          axis.line.y.left = element_line(colour="black") ,
                          plot.title = element_blank(),
                          plot.subtitle = element_text(size=9, hjust=0.5),
                          axis.text=element_text(size=9),
                          axis.title=element_text(size=9, face="bold"),
                          #change stuff for facet labels
                          strip.background =element_rect(fill="black", colour="black"),
                          strip.text = element_text(colour = 'white', size=12))

themepowerpointtitle <-  theme(panel.background = element_rect(fill="white",colour="azure2"), 
                               legend.position = "none",
                               panel.grid.major = element_line(colour="azure2") ,
                               axis.line.x.bottom = element_line(colour="black") ,
                               axis.line.y.left = element_line(colour="black") ,
                               plot.title = element_text(size=9, hjust=0.5),
                               plot.subtitle = element_text(size=8, hjust=0.5),
                               axis.text=element_text(size=9),
                               axis.title=element_text(size=9, face="bold"),
                               #change stuff for facet labels
                               strip.background =element_rect(fill="black", colour="black"),
                               strip.text = element_text(colour = 'white', size=12))

themepowerpointlegend <-  theme(panel.background = element_rect(fill="white",colour="azure2"), 
                                legend.position = "bottom",
                                panel.grid.major = element_line(colour="azure2") ,
                                axis.line.x.bottom = element_line(colour="black") ,
                                axis.line.y.left = element_line(colour="black") ,
                                plot.title = element_text(size=9, hjust=0.5),
                                plot.subtitle = element_text(size=8, hjust=0.5),
                                axis.text=element_text(size=9),
                                axis.title=element_text(size=9, face="bold"),
                                #change stuff for facet labels
                                strip.background =element_rect(fill="black", colour="black"),
                                strip.text = element_text(colour = 'white', size=12))

thememultiplot <-  theme(panel.background = element_rect(fill="white",colour="azure2"), legend.position = "none",
                         panel.grid.major = element_line(colour="azure2") ,
                         axis.line.x.bottom = element_line(colour="black") ,
                         axis.line.y.left = element_line(colour="black") ,
                         plot.title = element_blank(),
                         plot.subtitle = element_text(size=9, hjust=0.5),
                         axis.text=element_text(size=20),
                         axis.title=element_text(size=22, face="bold"),
                         #change stuff for facet labels
                         strip.background =element_rect(fill="black", colour="black"),
                         strip.text = element_text(colour = 'white', size=12))

themeblank <- theme(panel.background = element_rect(fill="white", colour="white"), 
                    legend.position = "none",
                    panel.grid.major = element_blank() ,
                    axis.line.x.bottom = element_blank() ,
                    axis.line.y.left = element_blank() ,
                    plot.title = element_blank(),
                    plot.subtitle = element_blank(),
                    axis.ticks=element_blank(),
                    axis.text.x=element_blank(),
                    axis.text.y=element_blank(),
                    axis.title.y=element_blank(),
                    axis.title.x=element_blank(),
                    #change stuff for facet labels
                    strip.background =element_rect(fill="black", colour="black"),
                    strip.text = element_text(colour = 'white', size=12)
)

themeblankwithaxes <- theme(panel.background = element_rect(fill="white", colour="white"), 
                            legend.position = "none",
                            panel.grid.major = element_blank() ,
                            axis.line.x.bottom = element_line(colour="black") ,
                            axis.line.y.left = element_line(colour="black") ,
                            plot.title = element_blank(),
                            plot.subtitle = element_blank(),
                            axis.text.x=element_text(size=20),
                            axis.text.y=element_text(size=20),
                            axis.title.y=element_text(size=20),
                            axis.title.x=element_text(size=20),
                            #change stuff for facet labels
                            strip.background =element_rect(fill="black", colour="black"),
                            strip.text = element_text(colour = 'white', size=12)
)
