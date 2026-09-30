# Render frozen Stage31d inference alongside Stage31b descriptive trajectories.
cc4g_verify <- function(src) {
  path<-file.path(src,"manifest.csv")
  if(!file.exists(path))stop("Phenotype source manifest missing.",call.=FALSE)
  m<-utils::read.csv(path,stringsAsFactors=FALSE)
  required<-c("omnibus.csv","contrasts.csv","bootstrap_status.csv","population.csv","animal_period_input.csv","fit_diagnostics.csv","protocol.txt")
  if(!all(c("File","SHA256")%in%names(m))||anyNA(m)||anyDuplicated(m$File)||
     any(grepl("(^[/\\\\]|:|(^|[/\\\\])[.][.]([/\\\\]|$))",m$File))||!all(required%in%m$File))stop("Invalid phenotype manifest.",call.=FALSE)
  paths<-file.path(src,m$File)
  if(!all(file.exists(paths))||!identical(unname(tools::sha256sum(paths)),m$SHA256))stop("Phenotype hash mismatch.",call.=FALSE)
  invisible(m)
}

cc4g_read <- function(src) {
  cc4g_verify(src)
  read<-function(n)utils::read.csv(file.path(src,paste0(n,".csv")),stringsAsFactors=FALSE)
  d<-list(omnibus=read("omnibus"),contrasts=read("contrasts"),bootstrap=read("bootstrap_status"),population=read("population"))
  o<-d$omnibus;c<-d$contrasts;b<-d$bootstrap
  if(!all(c("Sex","Phase","P","HolmP","Status")%in%names(o))||
     !all(c("Sex","Phase","Contrast","Estimate","Lower95","Upper95","P","HolmP","TestStatus","IntervalStatus")%in%names(c))||
     !all(c("Sex","Phase","Hypothesis","Status","Used")%in%names(b)))stop("Phenotype schema mismatch.",call.=FALSE)
  expected<-expand.grid(Sex=c("Female","Male"),Phase=c("Inactive","Active"),Contrast=c("RES-CON","SUS-CON","SUS-RES"),stringsAsFactors=FALSE)
  if(nrow(o)!=4L||nrow(c)!=12L||anyNA(o[c("Sex","Phase","Status")])||anyNA(c[c("Sex","Phase","Contrast","TestStatus","IntervalStatus")])||
     anyDuplicated(o[c("Sex","Phase")])||anyDuplicated(c[c("Sex","Phase","Contrast")])||
     nrow(merge(expected,c,by=c("Sex","Phase","Contrast")))!=12L||
     any(!o$Sex%in%c("Female","Male"))||any(!o$Phase%in%c("Inactive","Active")))stop("Incomplete phenotype design.",call.=FALSE)
  if(any(grepl("VALIDATION",c(o$Status,c$TestStatus,c$IntervalStatus))))stop("Validation-only inference cannot be rendered.",call.=FALSE)
  for(i in seq_len(nrow(c))) {
    r<-c[i,];boot<-b[b$Sex==r$Sex&b$Phase==r$Phase,]
    for(pair in list(c("TestStatus",r$Contrast),c("IntervalStatus","full")))if(r[[pair[1]]]=="PASS") {
      z<-boot[boot$Hypothesis==pair[2],]
      if(nrow(z)!=1L||z$Status!="PASS"||z$Used<4999)stop("Insufficient phenotype refit evidence.",call.=FALSE)
    }
    if(r$TestStatus=="PASS"&&(!is.finite(r$P)||!is.finite(r$HolmP)||r$P<0||r$HolmP<r$P||r$HolmP>1))stop("Invalid phenotype P value.",call.=FALSE)
    if(r$IntervalStatus=="PASS"&&(!all(is.finite(c(r$Estimate,r$Lower95,r$Upper95)))||r$Estimate<=0||r$Lower95<=0||r$Lower95>r$Upper95))stop("Invalid phenotype interval.",call.=FALSE)
  }
  for(i in seq_len(nrow(o)))if(o$Status[i]=="PASS") {
    z<-b[b$Sex==o$Sex[i]&b$Phase==o$Phase[i]&b$Hypothesis=="omnibus",]
    if(nrow(z)!=1L||z$Status!="PASS"||z$Used<4999||!all(is.finite(c(o$P[i],o$HolmP[i])))||o$P[i]<0||o$HolmP[i]<o$P[i]||o$HolmP[i]>1)stop("Invalid omnibus evidence.",call.=FALSE)
  }
  d
}

cc4g_p_label<-function(p)ifelse(p<.001,"P < 0.001",paste0("P = ",formatC(p,digits=3,format="f")))

cc4g_compare_rosters<-function(model,phases) {
  keys<-c("Sex","Phase","Group","Batch","CageID","AnimalKey")
  if(!all(c(keys,"Period")%in%names(model))||!all(c(keys,"PhaseLabel","IncludedInPrimary")%in%names(phases)))stop("Roster identity schema mismatch.",call.=FALSE)
  a<-model[model$Period=="Reference",keys,drop=FALSE]
  b<-phases[phases$PhaseLabel%in%c("I2","A2") & as.character(phases$IncludedInPrimary)=="TRUE",keys,drop=FALSE]
  if(!nrow(a)||anyNA(a)||anyNA(b)||anyDuplicated(a[c("Sex","Phase","AnimalKey")])||anyDuplicated(b[c("Sex","Phase","AnimalKey")]))stop("Invalid roster identity.",call.=FALSE)
  canonical<-function(x){x[]<-lapply(x,as.character);x<-x[do.call(order,x),,drop=FALSE];rownames(x)<-NULL;x}
  if(!identical(canonical(a),canonical(b)))stop("Trajectory/model animal or cage identity mismatch.",call.=FALSE)
  invisible(TRUE)
}

cc4g_panels <- function(stats,descriptive) {
  palette<-nature_palette("group")
  if(!identical(palette,c(CON="#3E3C6F",RES="#C6C3BB",SUS="#E63A48")))stop("Behaviour palette changed.",call.=FALSE)
  shapes<-c(CON=21,RES=24,SUS=22);lines<-c(CON="solid",RES="longdash",SUS="dotdash")
  contexts<-data.frame(Sex=c("Female","Male","Female","Male"),Phase=c("Inactive","Inactive","Active","Active"))
  panels<-list()
  ci<-stats$contrasts[stats$contrasts$IntervalStatus=="PASS",]
  xlim<-range(c(.5,1.5,ci$Lower95*.9,ci$Upper95*1.1),na.rm=TRUE)
  for(i in 1:4) {
    sex<-contexts$Sex[i];phase<-contexts$Phase[i]
    s<-subset(descriptive$summary,Sex==sex&Phase==phase&Measure=="Rate"&PhaseLabel!="A1")
    b<-subset(descriptive$batch,Sex==sex&Phase==phase&PhaseLabel!="A1")
    position<-function(x){x$Group<-factor(x$Group,c("CON","RES","SUS"));x$X<-as.integer(sub("^[IA]","",x$PhaseLabel))+c(-.1,0,.1)[as.integer(x$Group)];x}
    s<-position(s);b<-position(b)
    b$X<-b$X+c(B1=-.025,B2=0,B5=.025,B3=-.025,B4=0,B6=.025)[b$Batch]
    ymax<-ceiling(max(subset(descriptive$batch,Phase==phase&PhaseLabel!="A1")$Rate)/5)*5
    labs<-paste0(substr(phase,1,1),2:5);labs[1]<-paste0(labs[1],"\nreference")
    panels[[i]]<-ggplot2::ggplot(s,ggplot2::aes(X,Estimate,group=Group,colour=Group))+
      ggplot2::geom_point(data=b,ggplot2::aes(y=Rate,fill=Group,shape=Group),colour="grey50",alpha=.5,size=1,stroke=.15,show.legend=FALSE)+
      ggplot2::geom_line(ggplot2::aes(linetype=Group),linewidth=.3)+
      ggplot2::geom_point(ggplot2::aes(fill=Group,shape=Group),colour="grey20",size=1.65,stroke=.2)+
      ggplot2::scale_colour_manual(values=palette,drop=FALSE)+ggplot2::scale_fill_manual(values=palette,drop=FALSE)+
      ggplot2::scale_shape_manual(values=shapes,drop=FALSE)+ggplot2::scale_linetype_manual(values=lines,drop=FALSE)+
      ggplot2::scale_x_continuous(breaks=2:5,labels=labs,expand=ggplot2::expansion(add=.25))+
      ggplot2::scale_y_continuous(limits=c(0,ymax),expand=ggplot2::expansion(mult=c(0,.04)))+
      ggplot2::labs(tag=letters[i],title=paste(sex,tolower(phase),sep=" | "),x=NULL,y="RFID position changes (h\u207b\u00b9)")+cc4_nature_theme()
    c<-subset(stats$contrasts,Sex==sex&Phase==phase);o<-subset(stats$omnibus,Sex==sex&Phase==phase)
    c$Y<-match(c$Contrast,c("SUS-RES","SUS-CON","RES-CON"))
    c$Text<-ifelse(c$TestStatus=="PASS",cc4g_p_label(c$HolmP),"Test withheld")
    good<-c[c$IntervalStatus=="PASS",]
    subtitle<-if(o$Status=="PASS")paste0("Overall group-by-period: adjusted ",cc4g_p_label(o$HolmP)) else "Overall test withheld"
    panels[[i+4L]]<-ggplot2::ggplot(c,ggplot2::aes(y=Y))+
      ggplot2::geom_vline(xintercept=1,linewidth=.18,linetype="dotted",colour="grey50")+
      ggplot2::geom_segment(data=good,ggplot2::aes(x=Lower95,xend=Upper95,yend=Y),linewidth=.32,colour="grey20")+
      ggplot2::geom_point(data=good,ggplot2::aes(x=Estimate),size=1.6,colour="grey20")+
      ggplot2::geom_text(ggplot2::aes(x=xlim[2],label=Text),hjust=0,size=5.8/ggplot2::.pt,family="Arial")+
      ggplot2::scale_x_log10(limits=c(xlim[1],xlim[2]*2),breaks=c(.25,.5,.75,1,1.5,2,3,4)[c(.25,.5,.75,1,1.5,2,3,4)<=xlim[2]])+
      ggplot2::scale_y_continuous(breaks=3:1,labels=c("RES-CON","SUS-CON","SUS-RES"),limits=c(.5,3.5))+
      ggplot2::labs(tag=letters[i+4L],title=paste(sex,tolower(phase),sep=" | "),subtitle=subtitle,x="Ratio of rate ratios",y=NULL)+
      cc4_nature_theme()+ggplot2::theme(legend.position="none",plot.subtitle=ggplot2::element_text(size=5.8,hjust=.5))
  }
  panels
}

cc4g_figure <- function(stats,descriptive) {
  panels<-cc4g_panels(stats,descriptive)
  patchwork::wrap_plots(panels,ncol=2,guides="collect",heights=c(1.15,1.15,1,1))+
    patchwork::plot_annotation(caption=paste("a-d: frozen equal-batch trajectories; small points are batch means. e-h: modelled change from I2/A2 to pooled I3-I5/A3-A5.",
      "Ratios >1 indicate a larger proportional change in the first group. Pointwise 95% bootstrap intervals; 4,999 successful draws.",
      "Holm correction across four overall tests and, separately, twelve planned contrasts. P values shown are adjusted.",
      "All groups received grid sessions; exact times were unrecorded. Exploratory phenotype associations, not a causal grid or recovery test.",sep="\n"),
      theme=ggplot2::theme(plot.caption=ggplot2::element_text(size=5.5,family="Arial",hjust=0),plot.margin=ggplot2::margin(2,2,2,2,unit="mm"))) &
    ggplot2::theme(legend.position="top")
}
