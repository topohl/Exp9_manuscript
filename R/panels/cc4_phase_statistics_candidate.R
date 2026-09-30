# Render-only Stage 31c handoff. No fitting or calculation of inference here.
cc4_stats_verify_bundle <- function(src) {
  path<-file.path(src,"manifest.csv")
  if(!file.exists(path)) stop("CC4 statistical manifest missing.",call.=FALSE)
  m<-utils::read.csv(path,stringsAsFactors=FALSE)
  required<-c("primary_ratios.csv","primary_effects.csv","secondary_ratios.csv","fit_diagnostics.csv",
    "bootstrap_status.csv","sensitivity_estimates.csv","predictive_checks.csv","input_manifest.csv","protocol.txt")
  if(!all(c("File","SHA256") %in% names(m)) || anyNA(m) || anyDuplicated(m$File) ||
    any(grepl("(^[/\\\\]|:|(^|[/\\\\])[.][.]([/\\\\]|$))",m$File)) || !all(required %in% m$File)) stop("Invalid statistical manifest.",call.=FALSE)
  files<-file.path(src,m$File)
  if(!all(file.exists(files)) || !identical(unname(tools::sha256sum(files)),m$SHA256)) stop("Statistical source hash mismatch.",call.=FALSE)
  invisible(m)
}

cc4_stats_read_bundle <- function(src) {
  cc4_stats_verify_bundle(src)
  d<-utils::read.csv(file.path(src,"primary_ratios.csv"),stringsAsFactors=FALSE)
  required<-c("Sex","Phase","Estimate","Lower95","Upper95","P","HolmP","Status","Cages","Animals","CONCages")
  if(!all(required %in% names(d)) || nrow(d)!=4L || anyDuplicated(d[c("Sex","Phase")]) ||
    any(!d$Sex %in% c("Female","Male")) || any(!d$Phase %in% c("Active","Inactive")) || anyNA(d$Status)) stop("Invalid primary statistical design.",call.=FALSE)
  ok<-d$Status=="PASS"
  if(any(!is.finite(as.matrix(d[ok,c("Estimate","Lower95","Upper95","P","HolmP")]))) ||
    any(d$Lower95[ok]<=0 | d$Upper95[ok]<d$Lower95[ok] | d$P[ok]<0 | d$P[ok]>1 | d$HolmP[ok]<d$P[ok] | d$HolmP[ok]>1)) stop("Invalid passing statistical result.",call.=FALSE)
  if(any(d$Status=="VALIDATION_ONLY_NO_INFERENCE")) stop("Validation-only bundle cannot be rendered as inference.",call.=FALSE)
  boot<-utils::read.csv(file.path(src,"bootstrap_status.csv"),stringsAsFactors=FALSE)
  if(!all(c("Sex","Phase","Type","Status","Used") %in% names(boot)) || anyDuplicated(boot[c("Sex","Phase","Type")])) stop("Invalid bootstrap evidence.",call.=FALSE)
  d$BootstrapDraws<-NA_integer_
  for(i in which(ok)) {
    z<-boot[boot$Sex==d$Sex[i] & boot$Phase==d$Phase[i],]
    if(nrow(z)!=2L || !setequal(z$Type,c("full","null")) || any(z$Status!="PASS" | z$Used<4999)) stop("Insufficient successful bootstrap evidence.",call.=FALSE)
    d$BootstrapDraws[i]<-min(z$Used)
  }
  d
}

cc4_stats_plot <- function(d) {
  # All scientific numbers, confidence bounds and adjusted P values are frozen.
  d$Row<-match(paste(d$Sex,d$Phase),c("Male Active","Female Active","Male Inactive","Female Inactive"))
  d$Label<-paste0(d$Sex," | ",tolower(d$Phase),"\n",d$Animals," animals; ",d$Cages," cages (",d$CONCages," CON)")
  d$Text<-ifelse(d$Status=="PASS",sprintf("%.2f [%.2f, %.2f]   P = %.3f",d$Estimate,d$Lower95,d$Upper95,d$HolmP),"Inference withheld: fitting diagnostics")
  good<-d[d$Status=="PASS",]
  limits<-if(nrow(good)) range(c(.5,2,good$Lower95*.9,good$Upper95*1.1)) else c(.5,2)
  base<-cc4_nature_theme()+ggplot2::theme(legend.position="none",axis.title.y=ggplot2::element_blank(),
    axis.text.y=ggplot2::element_text(size=6.5),plot.margin=ggplot2::margin(3,3,3,3,unit="mm"))
  forest<-ggplot2::ggplot(d,ggplot2::aes(y=Row))+
    ggplot2::geom_vline(xintercept=1,linetype="dotted",linewidth=.2,colour="grey45")+
    ggplot2::geom_segment(data=good,ggplot2::aes(x=Lower95,xend=Upper95,yend=Row),linewidth=.35,colour="grey20")+
    ggplot2::geom_point(data=good,ggplot2::aes(x=Estimate),size=1.8,shape=21,fill="grey20",colour="grey20",stroke=.2)+
    ggplot2::scale_x_log10(limits=limits,breaks=c(.25,.5,.75,1,1.5,2,3,4))+
    ggplot2::scale_y_continuous(breaks=d$Row,labels=d$Label,limits=c(.5,4.6),expand=c(0,0))+
    ggplot2::labs(x="Ratio of rate ratios (SIS / CON)",title="Relative change across the three later phases")+base
  labels<-ggplot2::ggplot(d,ggplot2::aes(y=Row,x=0,label=Text))+
    ggplot2::geom_text(hjust=0,size=6.5/ggplot2::.pt,family="Arial")+
    ggplot2::scale_y_continuous(limits=c(.5,4.6),expand=c(0,0))+
    ggplot2::scale_x_continuous(limits=c(0,1),expand=c(0,0))+
    ggplot2::labs(title="Estimate [95% CI]   Holm-adjusted P")+
    ggplot2::theme_void(base_size=6.5,base_family="Arial")+
    ggplot2::theme(plot.title=ggplot2::element_text(size=7,hjust=0),plot.margin=ggplot2::margin(3,3,9,0,unit="mm"))
  patchwork::wrap_plots(forest,labels,nrow=1,widths=c(1.2,1))+
    patchwork::plot_annotation(caption=paste("Reference: I2 or A2. Later phases: I3-I5 or A3-A5. All conditions received grid sessions.",
      if(nrow(good)) paste0("Parametric bootstrap, at least ",format(min(good$BootstrapDraws),big.mark=",")," successful replicates; pointwise 95% intervals. Four primary tests.") else "Statistical inference withheld by the fitting checks.",
      "Exploratory models; only three CON cages per sex. See the accompanying sensitivity analysis.",sep="\n"),
      theme=ggplot2::theme(plot.caption=ggplot2::element_text(size=5.5,family="Arial",hjust=0),plot.margin=ggplot2::margin(1,2,1,2,unit="mm")))
}
