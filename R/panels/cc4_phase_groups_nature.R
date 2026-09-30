# Presentation-only refinement of the frozen Stage 31b descriptive estimates.
# No calculation of new scientific estimates, intervals or p-values.
cc4_nature_theme <- function() {
  theme_nature_manuscript_panel(base_size=6.5,base_family="Arial",publication_legible=TRUE) +
    ggplot2::theme(axis.line=ggplot2::element_line(linewidth=.18),
      axis.ticks=ggplot2::element_line(linewidth=.18),
      axis.text=ggplot2::element_text(size=6,colour="black"),
      axis.title=ggplot2::element_text(size=6.5,colour="black"),
      axis.ticks.length=grid::unit(.8,"mm"),
      plot.title=ggplot2::element_text(size=7,hjust=.5,margin=ggplot2::margin(b=4)),
      plot.tag=ggplot2::element_text(size=7,face="bold",hjust=0,vjust=1),
      plot.tag.position=c(0,1),
      plot.margin=ggplot2::margin(2.5,3,2,3,unit="mm"),
      legend.position="top",legend.title=ggplot2::element_blank(),
      legend.text=ggplot2::element_text(size=6.5),
      legend.key.width=grid::unit(7,"mm"),legend.key.height=grid::unit(3,"mm"),
      legend.margin=ggplot2::margin(0,0,1,0,unit="mm"))
}

cc4_nature_plots <- function(data) {
  palette <- nature_palette("group")
  if (!identical(palette,c(CON="#3E3C6F",RES="#C6C3BB",SUS="#E63A48")))
    stop("Unexpected behavioural palette.",call.=FALSE)
  shapes <- c(CON=21,RES=24,SUS=22)
  line_types <- c(CON="solid",RES="longdash",SUS="dotdash")
  position <- function(d) {
    d$Group <- factor(d$Group,c("CON","RES","SUS"))
    d$Number <- as.integer(sub("^[IA]","",d$PhaseLabel))
    d$X <- d$Number+c(-.12,0,.12)[as.integer(d$Group)]
    d
  }
  family_limits <- function(phase,measure) {
    s <- subset(data$summary,Phase==phase & Measure==measure)
    b <- subset(data$batch,Phase==phase)
    if (measure=="Rate") return(c(0,ceiling(max(b$Rate,s$Estimate)/5)*5))
    s <- subset(s,PhaseLabel %in% c("I3","I4","I5","A3","A4","A5"))
    b <- subset(b,PhaseLabel %in% c("I3","I4","I5","A3","A4","A5"))
    c(floor(min(0,s$Lower95,b$Change)/5)*5,ceiling(max(0,s$Upper95,b$Change)/5)*5)
  }
  panel <- function(sex,phase,measure,tag) {
    s <- position(subset(data$summary,Sex==sex & Phase==phase & Measure==measure))
    b <- position(subset(data$batch,Sex==sex & Phase==phase))
    if (measure=="Change") { s<-subset(s,Number>=3);b<-subset(b,Number>=3) }
    b$Value <- if (measure=="Rate") b$Rate else b$Change
    b$X <- b$X+c(B1=-.035,B2=0,B5=.035,B3=-.035,B4=0,B6=.035)[b$Batch]
    ns <- sort(unique(s$Number)); labs <- paste0(substr(phase,1,1),ns)
    if (measure=="Rate") labs[ns==2] <- paste0(labs[ns==2],"\nreference")
    p <- ggplot2::ggplot(s,ggplot2::aes(X,Estimate,group=Group,colour=Group))
    if (measure=="Change") p <- p +
      ggplot2::geom_hline(yintercept=0,colour="grey55",linewidth=.15,linetype="dotted") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=Lower95,ymax=Upper95),width=.075,linewidth=.23,show.legend=FALSE)
    p + ggplot2::geom_point(data=b,ggplot2::aes(y=Value,fill=Group,shape=Group),
        colour="grey50",size=1.1,stroke=.18,alpha=.55,show.legend=FALSE) +
      ggplot2::geom_line(ggplot2::aes(linetype=Group),linewidth=.32) +
      ggplot2::geom_point(ggplot2::aes(fill=Group,shape=Group),colour="grey20",size=1.75,stroke=.23) +
      ggplot2::scale_colour_manual(values=palette,drop=FALSE) + ggplot2::scale_fill_manual(values=palette,drop=FALSE) +
      ggplot2::scale_shape_manual(values=shapes,drop=FALSE) + ggplot2::scale_linetype_manual(values=line_types,drop=FALSE) +
      ggplot2::scale_x_continuous(breaks=ns,labels=labs,expand=ggplot2::expansion(add=.3)) +
      ggplot2::scale_y_continuous(limits=family_limits(phase,measure),expand=ggplot2::expansion(mult=c(0,.05))) +
      ggplot2::labs(tag=tag,title=paste(sex,tolower(phase),sep=" | "),x=NULL,
        y=if (measure=="Rate") "RFID position changes (h\u207b\u00b9)" else paste0("Change from ",ifelse(phase=="Inactive","I2","A2")," (h\u207b\u00b9)")) +
      cc4_nature_theme()
  }
  combine <- function(measure) {
    plots <- list(panel("Female","Inactive",measure,"a"),panel("Male","Inactive",measure,"b"),
      panel("Female","Active",measure,"c"),panel("Male","Active",measure,"d"))
    note <- if (measure=="Rate") "Large symbols: equal-batch means. Small symbols: the three batch estimates per sex." else
      "Large symbols: means and pointwise 95% t intervals (3 batches per sex). Small symbols: batch changes."
    patchwork::wrap_plots(plots,ncol=2,guides="collect") +
      patchwork::plot_annotation(caption=note,
        theme=ggplot2::theme(plot.caption=ggplot2::element_text(family="Arial",size=5.5,hjust=0))) &
      ggplot2::theme(legend.position="top")
  }
  contrast_panel <- function(sex,phase,tag) {
    all <- subset(data$contrasts,Phase==phase & Measure=="ChangeDifference" & PhaseLabel %in% c("I3","I4","I5","A3","A4","A5"))
    d <- subset(all,Sex==sex)
    order <- expand.grid(PhaseLabel=paste0(substr(phase,1,1),3:5),Contrast=c("RES-CON","SUS-CON","SUS-RES"),stringsAsFactors=FALSE)
    levels <- paste(order$Contrast,order$PhaseLabel,sep=" | ")
    d$Row <- factor(paste(d$Contrast,d$PhaseLabel,sep=" | "),rev(levels))
    lim <- c(floor(min(0,all$Lower95)/5)*5,ceiling(max(0,all$Upper95)/5)*5)
    ggplot2::ggplot(d,ggplot2::aes(Estimate,Row)) +
      ggplot2::geom_hline(yintercept=c(3.5,6.5),colour="grey85",linewidth=.13) +
      ggplot2::geom_vline(xintercept=0,colour="grey55",linewidth=.15,linetype="dotted") +
      ggplot2::geom_errorbar(ggplot2::aes(xmin=Lower95,xmax=Upper95),orientation="y",width=.14,linewidth=.23,colour="#2B2B2B") +
      ggplot2::geom_point(size=1.3,colour="#2B2B2B") +
      ggplot2::scale_x_continuous(limits=lim,breaks=scales::breaks_pretty(4),expand=ggplot2::expansion(mult=.025)) +
      ggplot2::scale_y_discrete(labels=function(x) gsub("-","\u2212",x,fixed=TRUE),expand=ggplot2::expansion(add=.5)) +
      ggplot2::labs(tag=tag,title=paste(sex,tolower(phase),sep=" | "),x="Difference in phase change (h\u207b\u00b9)",y=NULL) +
      cc4_nature_theme()
  }
  contrasts <- patchwork::wrap_plots(contrast_panel("Female","Inactive","a"),contrast_panel("Male","Inactive","b"),
    contrast_panel("Female","Active","c"),contrast_panel("Male","Active","d"),ncol=2) +
    patchwork::plot_annotation(caption="Paired batch contrasts; pointwise 95% t intervals (3 batches per sex). Positive values favour a larger change in the first group.",
      theme=ggplot2::theme(plot.caption=ggplot2::element_text(family="Arial",size=5.5,hjust=0)))
  list(cc4_nature_activity=combine("Rate"),cc4_nature_changes=combine("Change"),cc4_nature_group_contrasts=contrasts)
}
