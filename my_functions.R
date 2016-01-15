library(rCharts)
QC <-
  function(cpt){
    id = cpt$id
    code = cpt$code
    site =  cpt$site
    # check same length of the input variables
    if(length(id)!=length(code)){
      stop("IDs and CPT codes have different lengths")
    } else if(length(code)!=length(site)){
      stop("Sites and CPT codes have different lengths")
    } else if(length(site)!=length(id)){
      stop("Sites and IDs have different lengths")
    }
    
    # check format of site variable
    u.site = unique(site)
    if(length(u.site)==1){
      stop("Only one value in site")
    }else if(length(u.site)>2){
      stop("More than two values in site")
    }else if( (which(u.site==1)+which(u.site==0)) != 3){
      stop("Site is a vector of 0 and 1. Please check values of site")
    }
    
    # check NA in data
    cpt = data.frame(id,code,site)
    cpt$code = as.character(code)
    if(length( (msgind=which(is.na(id)))==T )>0){
      msg<-sprintf("Observations with NA's in ID deleted")
      warning(msg,call.=TRUE)
      cpt = cpt[-msgind,]
    } else if(length( (msgind=which(is.na(code)))==T )>0){
      msg<-sprintf("Observations with NA's in code deleted")
      warning(msg,call.=TRUE)
      cpt = cpt[-msgind,]
    } else if(length( (msgind=which(is.na(site)))==T )>0){
      msg<-sprintf("Observations with NA's in site deleted")
      warning(msg,call.=TRUE)
      cpt = cpt[-msgind,]
    }
    
    # check code digits length
    if(length(  (msgind = which(nchar(as.character(cpt$code))>5))   )>0){
      msg<-sprintf("CPT code longer than 5 digits dropped")
      warning(msg,call.=TRUE)
      cpt = cpt[-msgind,]
    } else if(length(   (msgind = which(nchar(as.character(cpt$code))<5))   )>0){
      msg<-sprintf("CPT code shorter than 5 digits dropped")
      warning(msg,call.=TRUE)
      cpt = cpt[-msgind,]
    }
    
    # check numeric code range
    nonnumericind = grep("[A-Z,a-z]",as.character(cpt$code))
    tmp = cpt[nonnumericind,]
    if(length(nonnumericind)>0 & length(nonnumericind)!=nrow(cpt)){
      cptnonnum = cpt[nonnumericind,]
      cptnonnum$numericcode = as.character(cptnonnum$code) # keep char in cptnonnum
      cptnum = cpt[-nonnumericind,]
      cptnum$numericcode = as.numeric(as.character(cptnum$code)) # char->num in cptnum
      if(length(which(cptnum$numericcode>99608))>0 | length(which(cptnum$numericcode<as.numeric(00100)))>0){
        msg<-sprintf("CPT codes category I out of range (00100, 99607)")
        warning(msg,call.=TRUE)
        cptnum = cptnum[-which(cptnum$numericcode>99607 | cptnum$numericcode<as.numeric(00100)),] # out of range of cpt codes
      }
      
    }else if(length(nonnumericind)==0){
      cptnum = cpt
      cptnum$numericcode = as.numeric(as.character(cptnum$code)) # char->num in cptnum
      cptnonnum = NULL
      if(length(which(cptnum$numericcode>99608))>0 | 
           length(which(cptnum$numericcode<as.numeric(00100)))>0){
        msg<-sprintf("CPT codes category I out of range (00100, 99607)")
        warning(msg,call.=TRUE)
        cptnum = cptnum[-which(cptnum$numericcode>99607 | cptnum$numericcode<as.numeric(00100)),] # out of range of cpt codes
      }
    }else if(length(nonnumericind)==nrow(cpt)){
      cptnonnum = cpt
      cptnonnum$numericcode = as.character(cptnonnum$code) # keep char in cptnonnum
      cptnum = NULL
    }
    
    print("QC done")
    # return cpt data
    cpt = rbind(cptnum, cptnonnum)  
    return(cpt)
  }

groupcode <-
  function(cptnonnum,cptnum){
    
    cptnonnum$numericcode = cptnonnum$code
    cptnum$numericcode = as.numeric(as.character(cptnum$code))
    ### 1. categorize non-numericc code "cptnonnum"
    if(!is.null(cptnonnum)){
      cptnonnum = merge(cptnonnum,groupbound.nonnum,by="code",all.x=T,all.y=F)
      if(length(which(is.na(cptnonnum$codegrp.order)))>0){
        cptnonnum = cptnonnum[!is.na(cptnonnum$codegrp.order),]
      }
    }
    
    ### 2. categorize numericc code "cpt"
    if(!is.null(cptnum)){
      # NB: min in our data doesnt include "00***"
      cptnum = merge(cptnum,groupbound.num,by="code",all.x=T,all.y=F)
      if(length(which(is.na(cptnum$codegrp.order)))>0){
        cptnum = cptnum[!is.na(cptnum$codegrp.order),]
      }
    }
    
    
    print("grpcode done")
    cpt = rbind(cptnonnum,cptnum)
    cpt$code = as.character(cpt$code)
    return(cpt)
    
  }


renameNonnumeric <- function(cpt){
  code.all = cpt$code
  cpt$numericcode = as.character(cpt$code)
  nonnumericind = grep("[A-Z,a-z]",as.character(code.all))
  uni.nonnumcode = unique(code.all[nonnumericind])
  n = length(uni.nonnumcode)
  if(n>0){
    for (i in 1:n){
      cpt$numericcode[which(code.all==uni.nonnumcode[i])]=99607+i
    }
  }
  cpt$numericcode = as.numeric(cpt$numericcode)
  cpt$id = as.numeric(cpt$id)
  
  # check non-integer codes
  if(length(which((cpt$numericcode*100)%%100 != 0))>0){
    msg<-sprintf("CPT code is not an integer: deleted")
    warning(msg,call.=TRUE)
    cpt = cpt[-which((cpt$numericcode*100)%%100 != 0),]
  }
  
  cpt = cpt[order(cpt$codegrp.order),]
  print("renameNonnumeric done")
  return(cpt)
}

dat4manhattan = function(cpt){
  names(cpt) = c("code","studyid","study_site",
                 "numericcode","codegrp","codegrp.order")
  cpt$code_grp = paste(cpt[,"numericcode"],"_",cpt[,"codegrp.order"],sep="")
  #####################################
  ## gen sufficient stat (count or pt) based data
  ## pt level offset: count of pt with assignment per site
  cpt$offset.pt = cpt$study_site
  cpt$offset.pt[cpt$offset.pt==0] = length(unique(cpt$studyid[cpt$study_site==0]))
  cpt$offset.pt[cpt$offset.pt==1] = length(unique(cpt$studyid[cpt$study_site==1]))
  ## (3.1) sum visit by studyid*code (site covered by studyid) 
  cpt$visit = rep(1, nrow(cpt))
  cpt.dt = data.table(cpt)
  ## sum over time
  cpt.dt_ptcode = cpt.dt[,list(visit.sum = sum(visit)), 
                         by = c("studyid","numericcode","code","study_site",
                                "offset.pt","codegrp.order","codegrp","code_grp")] 
  ptcode = cpt.dt_ptcode[,c("study_site","offset.pt","studyid", 
                            "code_grp","visit.sum"),with=F]
  ## reshape to pt level code counts
  ptcode = dcast.data.table(ptcode,studyid+study_site+offset.pt~code_grp, 
                            value.var="visit.sum",drop=T,fill=0)
  ## (3.2) sum visit by studyid*grp (site covered by studyid)
  cpt$visit = rep(1, nrow(cpt))
  cpt.dt = data.table(cpt)
  ## sum over time
  cpt.dt_grpcode = cpt.dt[,list(visit.sum = sum(visit)), 
                          by = c("studyid","study_site",
                                 "offset.pt","codegrp.order","codegrp")] 
  grpcode = cpt.dt_grpcode[,c("study_site","offset.pt","studyid",
                              "codegrp.order","visit.sum"),with=F]
  ## reshape to pt level code counts
  grpcode = dcast.data.table(grpcode,studyid+study_site+offset.pt~codegrp.order, 
                             value.var="visit.sum",drop=T,fill=0)
  
  return(list(ptcode=ptcode,grpcode=grpcode,cpt=cpt))
}

dat4RR = function(cpt){
  ## (4) sum pt&visit by code*site # for clickme
  cpt$visit = rep(1, nrow(cpt))
  cpt.dt = data.table(cpt)
  dat = data.frame(dcast.data.table(
    cpt.dt, code_grp+study_site~visit,
    fun=sum,value.var="visit",fill=0,drop=F))
  
  dat$numericcode = substr(dat$code_grp,start=1,
                           stop=unlist(gregexpr(pattern ='_',dat$code_grp))-1)
  dat$codegrp.order = substr(dat$code_grp,unlist(gregexpr(pattern ='_',dat$code_grp))+1,
                             stop=20)
  dat$offset.pt = dat$study_site
  dat$offset.pt[dat$offset.pt==0] = cpt$offset.pt[cpt$study_site==0][1]
  dat$offset.pt[dat$offset.pt==1] = cpt$offset.pt[cpt$study_site==1][1]
  dat = data.frame(CPT=dat$numericcode,site=dat$study_site,offset=dat$offset.pt,
                   block=dat$codegrp.order,count=dat$X1)
  dat$block = as.numeric(as.character(dat$block))
  dat = dat[order(dat$block),]
  dat$logN = log(dat$offset)
  dat$CPTblock = factor(factor(dat$block):factor(dat$CPT))
  return(dat)
}

myexactTestDoubleTail_New = function(s.y1,s.y2,dispersion=0,n1,n2)
{
  s.y1 = round(s.y1)
  s.y2 = round(s.y2)
  ncodes = length(s.y1)
  if(length(dispersion)==1) dispersion = rep(dispersion,ncodes)
  
  #  Null fitted values
  s = s.y1+s.y2
  r = size = 1/dispersion # same dispersion param for two sites
  mu = (s.y1+s.y2)/(n1+n2)
  # "mu" is lambda under the null; dont use (s.y1/n1+s.y2/n2)/2
  size1 = n1/dispersion; size2=n2/dispersion; sizes = (n1+n2)/dispersion
  mu1 = n1*mu; mu2=n2*mu; mus = (n1+n2)*mu
  
  p.bot = p.top = pvals = rep(10,ncodes)
  
  left = s.y1<=mu1
  if(any(left)) {
    p.bot[left] <- dnbinom(s[left],size=sizes[left],mu=mus[left])
    for (g in which(left)) {
      x = 0:s.y1[g]
      tmp.p.top = dnbinom(x,size=size1[g],mu=mu1[g]) * 
        dnbinom(s[g]-x,size=size2[g],mu=mu2[g])
      p.top[g] = 2*sum(tmp.p.top)
    }
    pvals[left] = p.top[left]/p.bot[left]
  }
  right = s.y1>=mu1
  if(any(right)) {
    p.bot[right] = dnbinom(s[right],size=sizes[right],mu=mus[right])
    for (g in which(right)) {
      x = s.y1[g]:s[g]
      tmp.p.top = dnbinom(x,size=size1[g],mu=mu1[g]) * 
        dnbinom(s[g]-x,size=size2[g],mu=mu2[g])
      p.top[g] = 2*sum(tmp.p.top)
    }
    pvals[right] = p.top[right]/p.bot[right]
  }
  pmin(pvals,1)
}

all_count_Pval = function(site = site, y = y,
                          numericcode,
                          method = "NBLRT"){
  n1 = length(which(site==1)); n2 = length(which(site==0))
  n.lambda = ncol(y)
  y1 = y[site==1,]; y2 = y[site==0,]
  s.y1 = apply(y1,2,sum)
  s.y2 = apply(y2,2,sum)
  bin.y1 = apply( y1,2,FUN=function(x){ as.numeric(x>0) } ) # binary data, dim = (n1)*length(lambda)
  bin.y2 = apply( y2,2,FUN=function(x){ as.numeric(x>0) } ) # binary data, dim = (n2)*length(lambda)
  bin.y = apply( y,2,FUN=function(x){ as.numeric(x>0) } ) #rbind(bin.y1,bin.y2)
  s.bin.y1=apply(bin.y1,2,sum);s.bin.y2=apply(bin.y2,2,sum)
  
  print("Now get p vals")
  if(method=="NBLRT"){
    Neg.log.like_alte = function (x,param,site){ 
      intercept = param[1]
      slope = param[2]
      size = param[3]
      mu = exp(intercept + slope*site)
      out = -sum(log(dnbinom(x = x, size = size, mu = mu)))
      if(!is.finite(out)) {out = 1e+200}
      return(out)
    }
    Neg.log.like_null = function (x,param){ 
      intercept = param[1]
      size = param[2]
      mu = exp(intercept)
      out = -sum(log(dnbinom(x = x, size = size, mu = mu)))
      if(!is.finite(out)) {out = 1e+200}
      return(out)
    }
    NB_byhand = apply(y, 2, FUN=function(x){
      m = try(glm.nb(x~factor(site),data=data.frame(x,site),
                      control=list(maxit = 100,epsilon = 1e-8,trace=F)), silent=TRUE)
      if(inherits(m, "try-error")){
        Negloglik_alte = try(optim(par=c(0.5,0.5,0.5),fn=Neg.log.like_alte,
                                   method="L-BFGS-B",x=x,site=site,
                                   lower=c(-Inf,-Inf,0),
                                   control=list(maxit=1000)), silent=TRUE)
        Negloglik_null = try(optim(par=c(0.5,0.5),fn=Neg.log.like_null,
                                   method="L-BFGS-B",x=x,
                                   lower=c(-Inf,0),
                                   control=list(maxit=1000)), silent=TRUE)
        if(inherits(Negloglik_alte, "try-error") | inherits(Negloglik_null, "try-error")){
          list(dev=0,error=2)
        }else{
          list(dev=2*(Negloglik_null$value-Negloglik_alte$value),error=1)
        }
      }else {
        list(dev=m$null.deviance-m$deviance,error=0)
      }
    })
    T_NB_byhand = as.data.frame(NB_byhand[1:length(NB_byhand)])[,seq(1,2*n.lambda,by=2)]
    #error = as.numeric(as.data.frame(NB_byhand[1:length(NB_byhand)])[,seq(2,2*n.lambda,by=2)])
    p = pchisq(unlist(T_NB_byhand),df=1,lower.tail = F)
  }else if(method=="PoisLRT"){
    model_alte_pois = apply(y, 2, FUN=function(x){
      glm(x~factor(site),data=data.frame(x,site),family="poisson",
          maxit = 50,epsilon = 1e-8,trace=F)
    })
    deviance_alte_pois = unlist(lapply(model_alte_pois,FUN=function(x){x$deviance}))
    deviance_alte.null_pois = unlist(lapply(model_alte_pois,FUN=function(x){x$null.deviance}))
    T_NB_byhand2_pois = deviance_alte.null_pois-deviance_alte_pois # same as LRbinom below
    p = pchisq(T_NB_byhand2_pois,df=1,lower.tail = F)
  }else if(method=="BinLRT"){
    model_alte_bin = apply(bin.y, 2, FUN=function(x){
      glm(x~factor(site),data=data.frame(x,site),family="binomial",
          maxit = 50,epsilon = 1e-8,trace=F)
    })
    deviance_alte_bin = unlist(lapply(model_alte_bin,FUN=function(x){x$deviance}))
    deviance_alte.null_bin = unlist(lapply(model_alte_bin,FUN=function(x){x$null.deviance}))
    T_NB_byhand_bin = deviance_alte.null_bin-deviance_alte_bin # same as LRbinom below
    p = pchisq(T_NB_byhand_bin,df=1,lower.tail = F)
  }else if(method=="NBExact"){
    # ## NB exact test
    ## estimate code-wise phi
    NB_byhand = apply(y, 2, FUN=function(x){
      m <- try(glm.nb(x~factor(site),data=data.frame(x,site),
                      control=list(maxit = 100,epsilon = 1e-8,trace=F)), silent=TRUE)
      if(inherits(m, "try-error")){
        Negloglik_alte = try(optim(par=c(0.5,0.5,0.5),fn=Neg.log.like_alte,
                                   method="L-BFGS-B",x=x,site=site,
                                   lower=c(-Inf,-Inf,0),
                                   control=list(maxit=1000)), silent=TRUE)
        Negloglik_null = try(optim(par=c(0.5,0.5),fn=Neg.log.like_null,
                                   method="L-BFGS-B",x=x,
                                   lower=c(-Inf,0),
                                   control=list(maxit=1000)), silent=TRUE)
        if(inherits(Negloglik_alte, "try-error") | inherits(Negloglik_null, "try-error")){
          #moment est of theta under the null..
          #have to assume heterogeneity bc under alternative cant converge
          size.m = try(theta.mm(x, mu=mean(x),dfr=length(site)-1), silent=T)
          if(inherits(size.m, "try-error")){return(list(dev=0,error=T,size=1e200))}else{
            return(list(dev=0,error=T,size=size.m))}
        }else{
          list(dev=2*(Negloglik_null$value-Negloglik_alte$value),error=T,
               size=Negloglik_alte$par[3])}
      }else{
        list(dev=m$null.deviance-m$deviance,error=F,size=m$theta)
      }
    })
    size_alte = as.numeric(as.data.frame(NB_byhand[1:length(NB_byhand)])[,seq(3,3*n.lambda,by=3)])
    Negbin.y1=s.y1; Negbin.y2=s.y2
    p = myexactTestDoubleTail_New(Negbin.y1,Negbin.y2, dispersion=1/(size_alte),n1,n2)
    #note: size_alte instead of size_null*norm cuz this time I used pt-level data to est 
    #code-wise dispersion parameter (captures variability within elements of sumation for s.y1)
  }else if(method=="PoisExact"){
    p=apply(cbind(s.y2,s.y1),1,FUN=function(x){
      poisson.test(x,c(n2,n1),r=1,alternative="two.sided")$p.value
    })
  }else if(method=="FishersExact"){
    table = data.frame(s.bin.y1,m1=n1-s.bin.y1,s.bin.y2,m2=n2-s.bin.y2)
    p = apply(table,1,FUN=function(x){
      fisher.test(matrix(x,nrow=2,ncol=2),or=1,alternative = "two.sided")$p.value
    })
  }else if(method=="PredSiteLRT"){
    Pred_site = apply(y, 2, FUN=function(x){
      m <- try(glm(site~x,data=data.frame(x,site),
                   control=list(maxit = 50,epsilon = 1e-8,trace=F),
                   family="binomial"), silent=TRUE)
      list(dev=m$null.deviance-m$deviance,error=F)
    })
    T_Pred_site = as.data.frame(Pred_site[1:length(Pred_site)])[,seq(1,2*n.lambda,by=2)]
    p = pchisq(unlist(T_Pred_site),df=1,lower.tail = F)
  }else if(method=="Ttest"){
    p = apply(y, 2, FUN=function(x){
      myttest = t.test(x~site)
      if(is.nan(myttest$p.value)){
        1
      }else{
        myttest$p.value
      }
    })
  }else if(method=="Dynamic"){
    ### NBExact
    NB_byhand = apply(y, 2, FUN=function(x){
      m <- try(glm.nb(x~factor(site),data=data.frame(x,site),
                      control=list(maxit = 100,epsilon = 1e-8,trace=F)), silent=TRUE)
      if(inherits(m, "try-error")){
        Negloglik_alte = try(optim(par=c(0.5,0.5,0.5),fn=Neg.log.like_alte,
                                   method="L-BFGS-B",x=x,site=site,
                                   lower=c(-Inf,-Inf,0),
                                   control=list(maxit=1000)), silent=TRUE)
        Negloglik_null = try(optim(par=c(0.5,0.5),fn=Neg.log.like_null,
                                   method="L-BFGS-B",x=x,
                                   lower=c(-Inf,0),
                                   control=list(maxit=1000)), silent=TRUE)
        if(inherits(Negloglik_alte, "try-error") | inherits(Negloglik_null, "try-error")){
          #moment est of theta under the null..
          #have to assume heterogeneity bc under alternative cant converge
          size.m = try(theta.mm(x, mu=mean(x),dfr=length(site)-1), silent=T)
          if(inherits(size.m, "try-error")){return(list(dev=0,error=T,size=1e200))}else{
            return(list(dev=0,error=T,size=size.m))}
        }else{
          list(dev=2*(Negloglik_null$value-Negloglik_alte$value),error=T,
               size=Negloglik_alte$par[3])}
      }else{
        list(dev=m$null.deviance-m$deviance,error=F,size=m$theta)
      }
    })
    size_alte = as.numeric(as.data.frame(NB_byhand[1:length(NB_byhand)])[,seq(3,3*n.lambda,by=3)])
    Negbin.y1=s.y1; Negbin.y2=s.y2
    p_NB_exact = myexactTestDoubleTail_New(Negbin.y1,Negbin.y2, dispersion=1/(size_alte),n1,n2)
    ### T test
    p_ttest = apply(y, 2, FUN=function(x){
      myttest = t.test(x~site)
      if(is.nan(myttest$p.value)){
        1
      }else{
        myttest$p.value
      }
    })
    s.y = apply(y,2,sum)
    p = rep(10,length(p_ttest))
    p[which(s.y>40)] = p_ttest[which(s.y>40)]
    p[which(s.y<=40)] = p_NB_exact[which(s.y<=40)]

  }
  
  manhattan.data = data.frame(code=numericcode,
                              p,method=method)
  manhattan.data$p.trunc = manhattan.data$p
  if(length(manhattan.data$p.trunc[which(manhattan.data$p.trunc<1e-17)])>0){
    manhattan.data$p.trunc[which(manhattan.data$p.trunc<1e-17)] = 1e-17
  }
  if(length(manhattan.data$p.trunc[which(manhattan.data$p.trunc>1)])>0){
    manhattan.data$p.trunc[which(manhattan.data$p.trunc>1)] = 1
  }
  
  print("P vals done")
  return(manhattan.data)
}

SKAT4manhattan = function(site = ptcode$study_site, y = ptcode[,c(4:ncol(ptcode))],
                          codegrp.order = codegrp.order){
  obj<-SKAT_Null_Model(site ~ 1, out_type="D",Adjustment=F)#!!!!! Adjustment is false
  ## group codes with same procedure description
  manhattan.data3 = data.frame(
    p = numeric(), codegrp.order = numeric())
  # beta.param = c(0.1, 0.1)
  codegrp.order.levels = as.numeric(as.character(unique(codegrp.order)))
  print("Now SKAT")
  for(i in 1:length(codegrp.order.levels)){
    ind = which(codegrp.order==codegrp.order.levels[i])
    if(length(ind)>0){
      tmp.visit = as.matrix(y[,ind]) #!!!! order of column(Z) = order of row(grp)
      weight.visit = 1/colMeans(tmp.visit, na.rm = TRUE)
      weight.visit=weight.visit/sum(weight.visit) # scale weights
      tmp = data.frame(
        p = SKAT(tmp.visit, obj, weights= weight.visit, 
                 is_check_genotype = F)$p.value, # is_check_genotype=F!!
        codegrp.order = codegrp.order.levels[i])
      manhattan.data3 = rbind(manhattan.data3,tmp)
    }
  }
  manhattan.data3$method="SKAT"
  manhattan.data3$p.trunc = manhattan.data3$p
  if(length(manhattan.data3$p.trunc[which(manhattan.data3$p.trunc<1e-17)])>0){
    manhattan.data3$p.trunc[which(manhattan.data3$p.trunc<1e-17)] = 1e-17
  }
  if(length(manhattan.data3$p.trunc[which(manhattan.data3$p.trunc>1)])>0){
    manhattan.data3$p.trunc[which(manhattan.data3$p.trunc>1)] = 1
  }
  
  print("SKAT done")
  return(manhattan.data3)
}

RR_glmnet = function(dat,cpt){
  attach(dat)
  x <- model.matrix( ~ factor(CPT)*site + factor(block)*site )
  # the structure of the x matrix:
  # intercept; code2-2378; site; blk2-191; code2-2378*site; blk2-191*site
  n.code = length(unique(CPT)); n.blk = length(unique(block))
  site.all = grep("site",colnames(x))
  ind.codes.all = grep("CPT",colnames(x))
  ind.block.all = grep("block",colnames(x))
  ind.codes.site = intersect(ind.codes.all,site.all);length(ind.codes.site)
  ind.codes = setdiff(ind.codes.all,site.all);length(ind.codes)
  ind.block.site = intersect(ind.block.all,site.all);length(ind.block.site)
  ind.block = setdiff(ind.block.all,site.all);length(ind.block)
  
  elements = strsplit(colnames(x),"[)*:]") # regular expression
  code.del = as.numeric(as.character(matrix(unlist(elements[ind.codes]),nrow=2)[2,]))
  codebysite.del = as.numeric(as.character(matrix(unlist(elements[ind.codes.site]),nrow=3)[2,]))
  # check if same:
  if(length(which(code.del - codebysite.del>0))==0){
    print("colnames of design matrix managed")
  }
  ## the codes chosen to be ref, excludes blk 1 (ref) and the blk whose code is ref in x
  ## first find the code&blk that is already ref
  x.alr_refcode = as.numeric(setdiff(as.numeric(as.character(unique(CPT))), code.del))
  ## second find which blk the code blong to
  x.alr_refblk = cpt$codegrp.order[which(cpt$numericcode == x.alr_refcode)[1]]
  ## now pick one ref code per block
  refcodes = dat$CPT[sapply(setdiff(unique(block),as.numeric(as.character(x.alr_refblk))),
                            FUN = function(y){which(dat$block==y)[1]})]
  ## delete ref codes
  refcodes = as.numeric(as.character(refcodes))
  code.del = which( as.numeric(unlist(lapply(elements,FUN=function(x){
    if(length(grep("CPT",x))>0){x[2]}else{""}  }))) %in% refcodes)
  #dim(x)[2]
  x = x[,-c(code.del)]
  #dim(x)[2] + (n.blk-2)*2 ## *2 bc interaction and main effect both deleted the refcodes
  y = count
  penalty = rep(1,ncol(x))
  # no penalty on site
  penalty[c(which(colnames(x)=="site"))]=0
  ## hp ##
  ## exclude the intercept 
  ## less penalty on anything that has block in it
  x.hp = x[,-1]
  blah = grep("block",colnames(x))
  penalty[blah]=0.5
  print("now fit glmnet")
  fitL <- glmnet( x.hp, y, family="poisson", offset = logN, alpha=0
                  ,penalty.factor=penalty
  )
  bbb <- fitL$beta[,95] #hp
  lp <- as.vector( x.hp%*%c(bbb)) + fitL$a[95]
  rrr_big <- (lp[site==1]) - (lp[site==0])
  print("glmnet done")
  return(rrr_big)
}

get_pdata = function(use.rrr = log2(exp(rrr_big)), 
                     dat=dat,cpt=cpt,
                     manhattan.data = manhattan.data){
  pdata = data.frame(RR=exp(use.rrr)  , log2RR=use.rrr, 
                     block=as.numeric(as.character(dat$block[dat$site==1])),
                     CPT=as.numeric(as.character(dat$CPT[dat$site==1])))
  pdata = merge(pdata,manhattan.data[,c("numericcode","p.trunc")],by.x="CPT",by.y="numericcode")
  ## add raw data (count per site)
  rawdat = data.frame(data.table(dat)[,list(count0=paste(count[site==0],offset[site==0],sep="/"),
                                            count1=paste(count[site==1],offset[site==1],sep="/")),by="CPT"])
  pdata = merge(pdata,rawdat,by="CPT",all.x=T)
  ## add group info
  grp.info = data.frame(data.table(cpt)[,list(trueCPT=unique(as.character(code)),
                                              codegrp=unique(codegrp),grporder=unique(codegrp.order)),
                                        by=c("numericcode")])
  pdata = merge(pdata,grp.info,by.x="CPT",by.y="numericcode")
  # numericcode is numericonized cpt code; CPT is the true CPT code (including e.g. G codes)
  pdata$numericcode = pdata$CPT; pdata$CPT = pdata$trueCPT; pdata = pdata[,!names(pdata) %in% "trueCPT"]
  ## construct extra info to show in clickme
  pdata$Raw_data = paste(pdata$count1, "; ", pdata$count0, sep="")
  pdata$extra = paste(pdata$codegrp,", RR = ", sprintf("%.2f",pdata$RR), ", "
                      ,"Raw data = ", pdata$count1, "; ", pdata$count0, sep="")
  pdata$RR = sprintf("%.1f",pdata$RR)
  ## add place holders for codegrps (last step)
  allblock = data.frame(
    data.table(groupbound)[,list(codegrp.order2=mean(as.numeric(as.character(codegrp.order))),
                                 codegrp=unique(as.character(codegrp))),
                           by="codegrp.order"])
  t = merge(pdata,allblock,by.x="grporder",by.y="codegrp.order",all.y=T)
  t = t[,!names(t) %in% c("codegrp.x","codegrp.order2")]#t[,-c(ncol(t)-1,ncol(t)-2)]
  t$extra[is.na(t$extra)] = as.character(t$codegrp.y[is.na(t$extra)])
  t$RR[is.na(t$RR)]=min(t$RR,na.rm=T); t$log2RR[is.na(t$log2RR)]=floor(min(t$log2RR,na.rm=T)-1); 
  t$CPT[is.na(t$CPT)]="Blocks not present"; t$numericcode[is.na(t$numericcode)]=0; 
  t$block[is.na(t$block)] = t$grporder[is.na(t$block)]
  t$p.trunc[is.na(t$p.trunc)] = 2
  
  names(t) = c("codegrp.order","CPT","RR","log2RR","blocknum","p.trunc",
               "count0","count1","numericcode","Raw_data","extra","Block")
  pdata = t
  ## significance level
  pdata$level = cut(pdata$p.trunc,breaks=c(3,1.1,0.05,0.01,
                                           0.05/nrow(pdata),0),right=F)
  t = levels(pdata$level); t = strsplit(t, "[[*,*)]")
  levels(pdata$level)[unlist(lapply(t,FUN=function(x){x[2]=="0"}))] = "sig. (Bonferroni)"
  levels(pdata$level)[unlist(lapply(t,FUN=function(x){x[3]=="0.01"}))] = "0.01 - sig. (Bonferroni)"
  levels(pdata$level)[unlist(lapply(t,FUN=function(x){x[3]=="0.05"}))] = "0.05 - 0.01"
  levels(pdata$level)[unlist(lapply(t,FUN=function(x){x[3]=="1.1"}))] = "> 0.05"
  levels(pdata$level)[unlist(lapply(t,FUN=function(x){x[3]=="3"}))] = "Blocks not present"
  
  pdata$blocknum = as.numeric(as.character(pdata$blocknum))
  pdata$p = sprintf("%1.2e", pdata$p.trunc)
  pdata = pdata[order(pdata$blocknum),]
}

plot_elements = function(results){
  manhattan.data = results[[1]]
  manhattan.data2 = results[[2]]
  manhattan.data3 = results[[3]]
  manhattan.data = manhattan.data[order(manhattan.data$block),]
  manhattan.data2 = manhattan.data2[order(manhattan.data2$code),]
  manhattan.data3 = manhattan.data3[order(as.numeric(as.character(manhattan.data3$codegrp.order))),]
  #working parameter ********************************************
  t1=-log10(0.05/nrow(manhattan.data2))         #first threshold
  t2=-log10(0.05/nrow(manhattan.data))         #second threshold
  maxp=max(-log10(manhattan.data$p.trunc),-log10(manhattan.data3$p.trunc)) #use in ylim
  minp = min(-log10(manhattan.data$p.trunc),-log10(manhattan.data3$p.trunc))
  # make p=0 to p=1e-17
  manhattan.data$p=manhattan.data$p.trunc
  p.order = manhattan.data$p[order(manhattan.data$p)]
  ind = which(manhattan.data$p==0)
  if(length(ind)>0){
    manhattan.data$p[ind] = p.order[1+length(ind)]
  }
  manhattan.data$log10p = -log10(manhattan.data$p)
  manhattan.data$color = as.numeric(as.character(manhattan.data$block))%%2
  ### Cumulative bp
  chr=as.numeric(manhattan.data$block)     #chromosome 
  cn=length(unique(chr))          #total chromosome number
  pn=length(manhattan.data$p)                    #total base pair number
  bp=c(1:pn)                      #distance
  histonum = table(as.factor(chr))
  new.chr = rep(c(1:cn), histonum) # if ordered, chr=new.chr
  cum.ends = c(0:(cn-1))*cn
  cum.bp=numeric(length=pn)
  for (i in 1:cn){
    cum.bp[chr==unique(chr)[i]] = bp[chr==unique(chr)[i]] + cum.ends[i]
  }
  cum.bp = cum.bp/mean(cum.bp)  #17000/scale 
  manhattan.data$cum.bp = cum.bp # must order manhattan.data rows!
  ## Chr centers (annotate codegrp.big at centers)
  bp.mean_by_grp=data.table(chr,cum.bp)[,list(cum.bp.mean=mean(cum.bp)),by="chr"]
  cum.centers = data.frame(bp.mean_by_grp)$cum.bp.mean
  manhattan.data$cum.bp = cum.bp
  print("elements done")
  return(list(manhattan.data,manhattan.data2,manhattan.data3))  
}

renderChart3 <- function( expr, env = parent.frame(), quoted = FALSE ){
  func <- shiny::exprToFunction(expr, env, quoted)
  function() {
    rChart_ <- func()
    cht_style <- sprintf("<style>.rChart {width: %spx; height: %spx} </style>", 
                         rChart_$params$width, rChart_$params$height)
    cht <- paste(
      capture.output(cat(
        rChart_$print()
        ,render_template(
          rChart_$templates$afterScript %||% 
            "<script></script>"
          , list(chartId = rChart_$params$dom, container = rChart_$container)
        )
        ,sep = ""
      ))
      , collapse = "\n")
    HTML(paste(c(cht_style, cht), collapse = "\n"))
  }
}

wrapup_Manhattan = function(cpt,inputmethod){
  cpt = QC(cpt)
  nonnumericind = grep("[A-Z,a-z]",as.character(cpt$code))
  if(length(nonnumericind)>0){
    cptnonnum=cpt[nonnumericind,];cptnum=cpt[-nonnumericind,]
    cpt = groupcode(cptnonnum,cptnum)
  }else{
    cpt = groupcode(NULL, cpt)
  }
  cpt = renameNonnumeric(cpt)
  rslt = dat4manhattan(cpt)
  ptcode = data.frame(rslt[[1]]); grpcode = data.frame(rslt[[2]])
  cpt = data.frame(rslt[[3]])
  ## manhattan.data
  code_grp = matrix(unlist(strsplit(names(ptcode[,4:ncol(ptcode)]),"_")),nrow=2)
  numericcode = substr(code_grp[1,],start=2,stop=6)
  codegrp.order = code_grp[2,]
  manhattan.data = all_count_Pval(site=ptcode$study_site,y=ptcode[,c(4:ncol(ptcode))],
                                  numericcode,method=inputmethod)#"Ttest")
  manhattan.data$code = as.numeric(as.character(manhattan.data$code))
  names(manhattan.data)[which(names(manhattan.data)=="code")] = "numericcode"
  grp.info = data.frame(data.table(cpt)[,list(codename=unique(as.character(code)),
                                              codegrp=unique(codegrp),grporder=unique(codegrp.order)),
                                        by=c("numericcode")])
  manhattan.data=merge(manhattan.data,grp.info,by="numericcode",all.x=F)
  ## manhattan.data2
  codegrp.order2 = substr(names(grpcode[,4:ncol(grpcode)]),start=2,stop=5)
  manhattan.data2 = all_count_Pval(site=grpcode$study_site,y=grpcode[,c(4:ncol(grpcode))],
                                   numericcode=codegrp.order2,method=inputmethod)#"Ttest")
  grp.info2 = data.frame(data.table(cpt)[,list(codegrp=unique(codegrp),grporder=unique(codegrp.order)),
                                         by=c("codegrp.order")])
  manhattan.data2=merge(manhattan.data2,grp.info2,by.x="code",by.y="codegrp.order",all.x=F)
  ## manhattan.data3
  manhattan.data3 = SKAT4manhattan(site = ptcode$study_site, y = ptcode[,c(4:ncol(ptcode))],
                                   codegrp.order = codegrp.order)
  manhattan.data3=merge(manhattan.data3,grp.info2,by="codegrp.order",all.x=F)
  names(manhattan.data)=c("numericcode","p","Method","p.trunc","CPT","Block","block")
  manhattan.data$block = as.numeric(as.character(manhattan.data$block))
  manhattan.data2 = manhattan.data2[order(as.numeric(as.character(manhattan.data2$grporder))),]
  manhattan.data3 = manhattan.data3[order(as.numeric(as.character(manhattan.data3$grporder))),]
  return(list(manhattan.data,manhattan.data2,manhattan.data3))
}

wrapup_RR = function(cpt,inputmethod){
  cpt = QC(cpt)
  nonnumericind = grep("[A-Z,a-z]",as.character(cpt$code))
  if(length(nonnumericind)>0){
    cptnonnum=cpt[nonnumericind,];cptnum=cpt[-nonnumericind,]
    cpt = groupcode(cptnonnum,cptnum)
  }else{
    cpt = groupcode(NULL, cpt)
  }
  cpt = renameNonnumeric(cpt)
  rslt = dat4manhattan(cpt)
  ptcode = data.frame(rslt[[1]]); grpcode = data.frame(rslt[[2]])
  cpt = data.frame(rslt[[3]])
  ## manhattan.data
  code_grp = matrix(unlist(strsplit(names(ptcode[,4:ncol(ptcode)]),"_")),nrow=2)
  numericcode = substr(code_grp[1,],start=2,stop=6)
  codegrp.order = code_grp[2,]
  manhattan.data = all_count_Pval(site=ptcode$study_site,y=ptcode[,c(4:ncol(ptcode))],
                                  numericcode,method=inputmethod)#"Ttest")
  manhattan.data$code = as.numeric(as.character(manhattan.data$code))
  names(manhattan.data)[which(names(manhattan.data)=="code")] = "numericcode"
  grp.info = data.frame(data.table(cpt)[,list(codename=unique(as.character(code)),
                                              codegrp=unique(codegrp),grporder=unique(codegrp.order)),
                                        by=c("numericcode")])
  manhattan.data=merge(manhattan.data,grp.info,by="numericcode",all.x=F)
  # RR
  dat = dat4RR(cpt)
  rrr_big = RR_glmnet(dat,cpt)
  data = get_pdata(use.rrr = log2(exp(rrr_big)),
                   dat=dat,cpt=cpt,manhattan.data = manhattan.data)
  data$block = data$blocknum
  return(data)
}


Manhattanplot = function(results){
  manhattan.data = results[[1]]
  manhattan.data2 = results[[2]]
  manhattan.data3 = results[[3]]
  manhattan.data2$codegrp.order=as.numeric(as.character(manhattan.data2$code))
  manhattan.data$codegrp.order=as.numeric(as.character(manhattan.data$block))
  manhattan.data$codegrp=manhattan.data$Block
  manhattan.data = manhattan.data[order(manhattan.data$codegrp.order),]
  manhattan.data2 = manhattan.data2[order(manhattan.data2$codegrp.order),]
  manhattan.data3 = manhattan.data3[order(as.numeric(as.character(manhattan.data3$codegrp.order))),]
  manhattan.data$p=manhattan.data$p.trunc
  #working parameter ********************************************
  t1=-log10(0.05/nrow(manhattan.data2))         #first threshold
  t2=-log10(0.05/nrow(manhattan.data))         #second threshold
  maxp=max(-log10(manhattan.data$p.trunc),-log10(manhattan.data3$p.trunc)) #use in ylim
  minp = min(-log10(manhattan.data$p.trunc),-log10(manhattan.data3$p.trunc))
  # make p=0 to p=1e-17
  p.order = manhattan.data$p[order(manhattan.data$p)]
  ind = which(manhattan.data$p==0)
  if(length(ind)>0){
    manhattan.data$p[ind] = p.order[1+length(ind)]
  }
  manhattan.data$log10P = -log10(manhattan.data$p)
  ### Cumulative bp
  chr=as.numeric(manhattan.data$codegrp.order)     #chromosome 
  cn=length(unique(chr))          #total chromosome number
  pn=length(manhattan.data$p)                    #total base pair number
  bp=c(1:pn)                      #distance
  histonum = table(as.factor(chr))
  new.chr = rep(c(1:cn), histonum) # if ordered, chr=new.chr
  cum.ends = c(0:(cn-1))*cn
  cum.bp=numeric(length=pn)
  for (i in 1:cn){
    cum.bp[chr==unique(chr)[i]] = bp[chr==unique(chr)[i]] + cum.ends[i]
  }
  cum.bp = cum.bp/median(cum.bp)  #17000/scale 
  manhattan.data$cum.bp = cum.bp # must order manhattan.data rows!
  ## Chr centers (annotate codegrp.big at centers)
  bp.mean_by_grp=data.table(chr,cum.bp)[,list(cum.bp.mean=mean(cum.bp)),by="chr"]
  cum.centers = data.frame(bp.mean_by_grp)$cum.bp.mean
  
  
  ### plot preparation ###
  scale=2
  #title="CPT Code (Proc+Img Numeric only) Relative Risk Plot \n P(Yes|N. Ca)/P(Yes|Detroit)"
  title=""
  plot.h = max(-log10(manhattan.data$p))*36
  plot.w = 1077*max(cum.bp)
  plot.mymar = c(60,24,0.5,0.5)*0.3
  plot.mymgp = c(3,1,0)+c(-1.7,0,0)
  plot.res = 72*2 #72 # or 72*2
  plot.background = "transparent"
  xlim.minus = 0.005/2
  xlim.plus = 0.005/2
  plot.pch = 16 #smaller
  fontsize = 1.5*1.7/scale*0.6 #matters!
  plot.font.axis = 6/scale*0.6 #matters!
  axis.lwd = 2/scale*0.6
  points.lwd = 2.7/scale*0.6
  plot.lty = 3/scale*0.6
  text.srt = 60
  pos.big.grp = minp/scale
  pos.codegrp = minp-2/scale # height of code grp name (y position)
  pos.yaxis = -xlim.minus # y axis's x position
  at.n = 5 # y axis
  
  # colors for chromosomes
  use = rainbow(cn)
  col = use[chr%%cn+1]
  # # Color for RR=0
  # orderRR = manhattan.data$p[order(manhattan.data$p)]
  # dump = which(orderRR<0);
  # if(length(dump)>0){
  #   last.dump = max(dump)
  #   col[log(p)==floor(log(orderRR[last.dump+1]))]="grey"
  # }
  
  ### plot ###
  par(mar=plot.mymar,font.axis=plot.font.axis)
  par(mgp=plot.mymgp)
  ### Plot
  xtxt = ""
  plot(cum.bp,-log10(manhattan.data$p),
       xlim=c(-xlim.minus, cum.bp[pn]+xlim.plus),ylim=c(minp,maxp)+xlim.plus,xaxs="i",
       xlab=xtxt,ylab="",cex.lab=fontsize,cex=fontsize*.9,
       col=col,axes=F,frame.plot=T,pch=plot.pch,font.axis=plot.font.axis,
       main=title, cex.main=fontsize,
       mtext(expression(-log[10](P)), side=2, line=2,cex=fontsize*1.3))
  
  
  
  
  # label names
  codegrp.labels = unique(manhattan.data$codegrp)
  # big.centers = (cum.centers[c(1,12,29,36,50)]+cum.centers[c(11,28,35,49,77)])/2+c(-1,8,-1,-2,5)/1000
  # big.labels = c("Evaluation and Management","Surgery","Radiology","Pathology & Laboratory","Medicine")
  # plot label
  # y label
  axis(side=2, 
       #      at = seq(floor(minp)*100,maxp*100,length.out=at.n)/100, 
       #      labels = seq(floor(minp)*100,maxp*100,length.out=at.n)/100,
       at = seq(floor(minp),maxp,by=at.n), 
       labels = seq(floor(minp),maxp,by=at.n),
       las=1, cex.axis=fontsize,lwd=axis.lwd,
       pos=pos.yaxis)
  # x label code grp
  text(x=cum.centers,y=pos.codegrp,srt=text.srt,adj=1,labels=codegrp.labels,xpd=TRUE,
       cex=fontsize*0.8,lwd=axis.lwd) #maxp -1.6
  
  p2=-log10(manhattan.data2$p.trunc)
  p3=-log10(manhattan.data3$p.trunc)
  points(cum.centers,p2,pch=5,cex=fontsize*.8,lwd = points.lwd*1.2)
  points(cum.centers,p3,pch=4,cex=fontsize*.8,lwd = points.lwd*1.2)
  legend(#"topright", 
    x=0.0,y=18,
    #title = "By Code Group", 
    bg="transparent",#box.lwd=1,
    legend=c("Burden","SKAT"), pch=c(5,4),col="black",
    cex=fontsize*1.2,lwd = axis.lwd*2.2,
    lty=0,
    box.lwd=0,bty="n")
  
  abline(h=t1,lty=3,col="black") # bonferoni for block level (t1<t2)
  abline(h=t2,lty=2,col="blue") # bonferoni for code level
  box() # !!!! added on May17, to make it usual
  legend(#"topright", 
    x=0.1,y=18,
    #title = "By Code Group", 
    bg="transparent",#box.lwd=1,
    legend=c("Significant level for Code","Significant level for Block"), 
    lty=c(2,3),col=c("blue","black"),
    cex=fontsize*1.2,lwd = axis.lwd*2.2,
    box.lwd=0,bty="n")
}


