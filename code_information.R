# @ Written by Chong Wang

#1: Meta-analysis #############################################################

##1.1.1：yield_family ######
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='yield_meta')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$yieldc_sd == 0, yieldc_sd := 0.05*yieldc_mean]
d2[d2$yieldt_sd == 0, yieldt_sd := 0.05*yieldt_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = yieldt_mean, sd1i = yieldt_sd, n1i = replication,
               m2i = yieldc_mean, sd2i = yieldc_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$family)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='Amaranthaceae',desc := 'Amaranthaceae']
d02.treat[treatment=='Asteraceae',desc := 'Asteraceae']
d02.treat[treatment=='Brassicaceae',desc := 'Brassicaceae']
d02.treat[treatment=='Fabaceae',desc := 'Fabaceae']
d02.treat[treatment=='Liliaceae',desc := 'Liliaceae']
d02.treat[treatment=='Linaceae',desc := 'Linaceae']
d02.treat[treatment=='Multiple',desc := 'Multiple crops']
d02.treat[treatment=='Pedaliaceae',desc := 'Pedaliaceae']
d02.treat[treatment=='Poaceae',desc := 'Poaceae']
d02.treat[treatment=='Solanaceae',desc := 'Solanaceae']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[family==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/yield_meta_family.csv")

##1.1.2：yield_growing season######
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='yield_meta')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$yieldc_sd == 0, yieldc_sd := 0.05*yieldc_mean]
d2[d2$yieldt_sd == 0, yieldt_sd := 0.05*yieldt_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = yieldt_mean, sd1i = yieldt_sd, n1i = replication,
               m2i = yieldc_mean, sd2i = yieldc_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$growing)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='early',desc := 'early rice']
d02.treat[treatment=='late',desc := 'late rice']
d02.treat[treatment=='single',desc := 'single rice']
d02.treat[treatment=='ratoon',desc := 'ratoon rice']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[growing==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/yield_meta_growing.csv")

##1.1.3：yield_subspecies#####
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='yield_meta')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$yieldc_sd == 0, yieldc_sd := 0.05*yieldc_mean]
d2[d2$yieldt_sd == 0, yieldt_sd := 0.05*yieldt_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = yieldt_mean, sd1i = yieldt_sd, n1i = replication,
               m2i = yieldc_mean, sd2i = yieldc_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$subspecies)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='indica',desc := 'indica rice']
d02.treat[treatment=='japonica',desc := 'japonica rice']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[subspecies==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/yield_meta_subspecies.csv")

##1.1.4：yield_specific rice-based systems#####
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='yield_meta_rotation')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$yieldc_sd == 0, yieldc_sd := 0.05*yieldc_mean]
d2[d2$yieldt_sd == 0, yieldt_sd := 0.05*yieldt_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = yieldt_mean, sd1i = yieldt_sd, n1i = replication,
               m2i = yieldc_mean, sd2i = yieldc_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$rotation)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='rice-barley',desc := 'rice-barley']
d02.treat[treatment=='rice-berseem',desc := 'rice-berseem']
d02.treat[treatment=='rice-brassica chinensis',desc := 'rice-brassica chinensis']
d02.treat[treatment=='rice-broad bean',desc := 'rice-broad bean']
d02.treat[treatment=='rice-cabbage',desc := 'rice-cabbage']
d02.treat[treatment=='rice-chickpea',desc := 'rice-chickpea']
d02.treat[treatment=='rice-Chinese cabbage',desc := 'rice-Chinese cabbage']
d02.treat[treatment=='rice-Chinese milk vetch',desc := 'rice-Chinese milk vetch']
d02.treat[treatment=='rice-common vetch',desc := 'rice-common vetch']
d02.treat[treatment=='rice-forage wheat',desc := 'rice-forage wheat']
d02.treat[treatment=='rice-garlic',desc := 'rice-garlic']
d02.treat[treatment=='rice-hairy vetch',desc := 'rice-hairy vetch']
d02.treat[treatment=='rice-lathyrus',desc := 'rice-lathyrus']
d02.treat[treatment=='rice-lentil',desc := 'rice-lentil']
d02.treat[treatment=='rice-linseed',desc := 'rice-linseed']
d02.treat[treatment=='rice-lupin',desc := 'rice-lupin']
d02.treat[treatment=='rice-maize',desc := 'rice-maize']
d02.treat[treatment=='rice-multiple crops',desc := 'rice-multiple crops']
d02.treat[treatment=='rice-mustard',desc := 'rice-mustard']
d02.treat[treatment=='rice-pea',desc := 'rice-pea']
d02.treat[treatment=='rice-persian clover',desc := 'rice-persian clover']
d02.treat[treatment=='rice-potato',desc := 'rice-potato']
d02.treat[treatment=='rice-rapeseed',desc := 'rice-rapeseed']
d02.treat[treatment=='rice-ryegrass',desc := 'rice-ryegrass']
d02.treat[treatment=='rice-sesame',desc := 'rice-sesame']
d02.treat[treatment=='rice-sesbania',desc := 'rice-sesbania']
d02.treat[treatment=='rice-spinach',desc := 'rice-spinach']
d02.treat[treatment=='rice-sunflower',desc := 'rice-sunflower']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[rotation==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/yield_meta_rotation.csv")

##1.2.1：N2O_family#####
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='N2O_meta')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$N2Oc_sd == 0, N2Oc_sd := 0.05*N2Oc_mean]
d2[d2$N2Ot_sd == 0, N2Ot_sd := 0.05*N2Ot_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = N2Ot_mean, sd1i = N2Ot_sd, n1i = replication,
               m2i = N2Oc_mean, sd2i = N2Oc_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$family)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='Brassicaceae',desc := 'Brassicaceae']
d02.treat[treatment=='Fabaceae',desc := 'Fabaceae']
d02.treat[treatment=='Liliaceae',desc := 'Liliaceae']
d02.treat[treatment=='Poaceae',desc := 'Poaceae']
d02.treat[treatment=='Solanaceae',desc := 'Solanaceae']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[family==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/N2O_meta_family.csv")

##1.2.2：N2O_growing season#####
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='N2O_meta')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$N2Oc_sd == 0, N2Oc_sd := 0.05*N2Oc_mean]
d2[d2$N2Ot_sd == 0, N2Ot_sd := 0.05*N2Ot_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = N2Ot_mean, sd1i = N2Ot_sd, n1i = replication,
               m2i = N2Oc_mean, sd2i = N2Oc_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$growing)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='late',desc := 'late rice']
d02.treat[treatment=='single',desc := 'single rice']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[growing==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/N2O_meta_growing.csv")

##1.2.3：N2O_subspecies#####
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='N2O_meta')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$N2Oc_sd == 0, N2Oc_sd := 0.05*N2Oc_mean]
d2[d2$N2Ot_sd == 0, N2Ot_sd := 0.05*N2Ot_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = N2Ot_mean, sd1i = N2Ot_sd, n1i = replication,
               m2i = N2Oc_mean, sd2i = N2Oc_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$subspecies)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='indica',desc := 'indica rice']
d02.treat[treatment=='japonica',desc := 'japonica rice']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[subspecies==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/N2O_meta_subspecies.csv")

##1.2.4：N2O_specific rice-based systems#####
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='N2O_meta_rotation')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$N2Oc_sd == 0, N2Oc_sd := 0.05*N2Oc_mean]
d2[d2$N2Ot_sd == 0, N2Ot_sd := 0.05*N2Ot_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = N2Ot_mean, sd1i = N2Ot_sd, n1i = replication,
               m2i = N2Oc_mean, sd2i = N2Oc_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$rotation)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='rice-alfalfa',desc := 'rice-alfalfa']
d02.treat[treatment=='rice-barley',desc := 'rice-barley']
d02.treat[treatment=='rice-broad bean',desc := 'rice-broad bean']
d02.treat[treatment=='rice-Chinese cabbage',desc := 'rice-Chinese cabbage']
d02.treat[treatment=='rice-Chinese milk vetch',desc := 'rice-Chinese milk vetch']
d02.treat[treatment=='rice-forage wheat',desc := 'rice-forage wheat']
d02.treat[treatment=='rice-garlic',desc := 'rice-garlic']
d02.treat[treatment=='rice-potato',desc := 'rice-potato']
d02.treat[treatment=='rice-rapeseed',desc := 'rice-rapeseed']
d02.treat[treatment=='rice-ryegrass',desc := 'rice-ryegrass']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[rotation==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/N2O_meta_rotation.csv")

##1.3.1：CH4_family#####
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='CH4_meta')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$CH4c_sd == 0, CH4c_sd := 0.05*CH4c_mean]
d2[d2$CH4dt_sd == 0, CH4t_sd := 0.05*CH4t_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = CH4t_mean, sd1i = CH4t_sd, n1i = replication,
               m2i = CH4c_mean, sd2i = CH4c_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$family)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='Brassicaceae',desc := 'Brassicaceae']
d02.treat[treatment=='Fabaceae',desc := 'Fabaceae']
d02.treat[treatment=='Liliaceae',desc := 'Liliaceae']
d02.treat[treatment=='Poaceae',desc := 'Poaceae']
d02.treat[treatment=='Solanaceae',desc := 'Solanaceae']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[family==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/CH4_meta_family.csv")

##1.3.2：CH4_growing season#####
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='CH4_meta_growing')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$CH4c_sd == 0, CH4c_sd := 0.05*CH4c_mean]
d2[d2$CH4t_sd == 0, CH4t_sd := 0.05*CH4t_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = CH4t_mean, sd1i = CH4t_sd, n1i = replication,
               m2i = CH4c_mean, sd2i = CH4c_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$growing)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='single',desc := 'single rice']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[growing==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/CH4_meta_growing.csv")

##1.3.3：CH4_subspecies####
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='CH4_meta')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$CH4c_sd == 0, CH4c_sd := 0.05*CH4c_mean]
d2[d2$CH4t_sd == 0, CH4t_sd := 0.05*CH4t_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = CH4t_mean, sd1i = CH4t_sd, n1i = replication,
               m2i = CH4c_mean, sd2i = CH4c_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$subspecies)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='indica',desc := 'indica rice']
d02.treat[treatment=='japonica',desc := 'japonica rice']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[subspecies==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/CH4_meta_subspecies.csv")

##1.3.4：CH4_specific rice-based systems#####
d1 <- readxl::read_xlsx('C:/Users/17371/Desktop/rice_wheat_rotation_R4.xlsx', sheet='CH4_meta_rotation')
d1 <- as.data.table(d1)
d2 <- d1
d2[d2$CH4c_sd == 0, CH4c_sd := 0.05*CH4c_mean]
d2[d2$CH4t_sd == 0, CH4t_sd := 0.05*CH4t_mean]
es21 <- escalc(measure = "ROM", data = d2, 
               m1i = CH4t_mean, sd1i = CH4t_sd, n1i = replication,
               m2i = CH4c_mean, sd2i = CH4c_sd, n2i = replication )
results <- es21[, c("yi", "vi")]
write.csv(results, "C:/Users/17371/Desktop/yi_vi_output.csv", row.names = FALSE)
d02 <- as.data.table(es21)
d02.treat <- data.table(treatment =  c('ALL', unique(d02$rotation)))
d02.treat[treatment=='ALL',desc := 'All']
d02.treat[treatment=='rice-alfalfa',desc := 'rice-alfalfa']
d02.treat[treatment=='rice-broad bean',desc := 'rice-broad bean']
d02.treat[treatment=='rice-Chinese milk vetch',desc := 'rice-Chinese milk vetch']
d02.treat[treatment=='rice-forage wheat',desc := 'rice-forage wheat']
d02.treat[treatment=='rice-garlic',desc := 'rice-garlic']
d02.treat[treatment=='rice-potato',desc := 'rice-potato']
d02.treat[treatment=='rice-rapeseed',desc := 'rice-rapeseed']
d02.treat[treatment=='rice-ryegrass',desc := 'rice-ryegrass']
out2 = out3 = list()
for (i in d02.treat$treatment){
  print(i) # i <- 'IA'
  if (i=='ALL'){
    print(i)
    r_meta <- rma.mv(yi,vi, data=d02, random=list(~ 1|studyid), method="REML",sparse = TRUE)
  } else {
    r_meta <- rma.mv(yi,vi, data=d02[rotation==i, ],random= list(~ 1|studyid), method="REML",sparse = TRUE)
  }
  out2[[i]] <- data.table(mean = as.numeric((exp(r_meta$b)-1)*100),
                          se = as.numeric((exp(r_meta$se)-1)*100),
                          pval = round(as.numeric(r_meta$pval), 4),
                          label =  paste0(d02.treat[treatment==i, desc],' (n=',r_meta$k,')'))
}
out2 <- rbindlist(out2)
export(out2, "C:/Users/17371/Desktop/CH4_meta_rotation.csv")

#2: SEM #############################################################

##2.1： yield #####
data <- readxl::read_xlsx('rice_wheat_rotation_R4_SEM.xlsx', sheet='yield_SEM_Z')
model <- '
  clay ~ a1*map + mat + aridity + n_dose
  tn   ~ a2*map + mat + a3*aridity
  soc  ~ a4*map + mat + aridity + a5*n_dose
  ph   ~ mat + aridity
  bd   ~ map + mat
  lnyield ~ c1*map + b1*tn  + c2*n_dose
  clay ~~ tn + soc + bd
  tn   ~~ soc + bd
  soc ~~ bd
  indirect_map_tn := a2 * b1      # map -> tn -> lnyield
  indirect_aridity_tn := a3 * b1  # aridity -> tn -> lnyield
  total_map := c1 + indirect_map_tn 
  total_n_dose := c2
'
fit <- sem(model, data = data)
fitMeasures(fit,c("chisq","df","pvalue","nnfi","gfi","cfi","rmr","srmr","rmsea","aic"))
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

##2.2： N2O #####
data <- readxl::read_xlsx('rice_wheat_rotation_R4_SEM.xlsx', sheet='N2O_SEM_Z')
model <- '
  clay ~ a1*map + a2*aridity
  tn ~ a4*map + a6*n_dose
  soc ~ a7*mat + a8*aridity
  ph ~ a9*map + a10*mat
  bd ~ a11*map + a12*mat + a13*aridity + a14*n_dose
  lnN2O ~ b1*clay + b2*bd + b4*n_dose
  clay ~~ bd
  tn ~~ soc
  tn ~~ ph
  tn ~~ bd
  ph ~~ bd
  ind_clay_map := a1 * b1
  ind_clay_aridity := a2 * b1
  ind_bd_map := a11 * b2
  ind_bd_mat := a12 * b2
  ind_bd_aridity := a13 * b2
  ind_bd_n_dose := a14 * b2
  
'
fit <- sem(model, data = data, estimator = "ML", se = "bootstrap", bootstrap = 1000)
fitMeasures(fit,c("chisq","df","pvalue","nnfi","gfi","cfi","rmr","srmr","rmsea","aic"))
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

##2.3： CH4 #####
data <- readxl::read_xlsx('rice_wheat_rotation_R4_SEM.xlsx', sheet='CH4_SEM_Z')
model <- '
  lnCH4 ~ c1*map + c2*tn + c3*bd
  tn ~ a1*map
  bd ~ a2*map + a3*aridity
  soc ~ a4*aridity
  clay ~ b1*map + b2*aridity
  ph ~ b3*map + b4*mat
  clay ~~ tn
  clay ~~ soc
  clay ~~ bd
  tn ~~ soc
  tn ~~ ph
  tn ~~ bd
  soc ~~ ph
  soc ~~ bd
  ph ~~ bd
  indirect_tn := a1 * c2
  indirect_bd_mp := a2 * c3
  total_map := c1 + (a1 * c2) + (a2 * c3)
'
fit <- sem(model, data = data)
fitMeasures(fit,c("chisq","df","pvalue","nnfi","gfi","cfi","rmr","srmr","rmsea","aic"))
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#3: RF #############################################################

##3.1： yield #####
d2 <- read_excel("rice_wheat_rotation_R4.xlsx", sheet="yield_RF")
dat <- d2
data <- dat %>% rename ("Precipitation"=map, "Temperature"=mat, "Aridity"=aridity, "Clay"=clay, 
                        "TN"=tn, "SOC"=soc, "pH"=ph, "BD"=bd, "Nrate"=n_dose, "Type"=type)
data.train <- sample(nrow(data),7/10*nrow(data))
train <- data[data.train,]
test<-data[-data.train,]
check_conv <- randomForest(as.formula(paste0("yield~", paste(moderators, collapse = "+"))),
                           data = train,
                           whichweights = "random",
                           num.trees = 500)
X <- dplyr::select(train, all_of(moderators)) 
mf_cv <- train(y = train$yield,
               x = X,
               method = "rf",
               keep.inbag = TRUE,
               num.threads = 6,
               verbose=TRUE,
               num.trees = 500)
mf_cv$results[which.min(mf_cv$results$RMSE), ] 
importance.export.rel <- varImp(mf_cv)$importance  
fit.forest <- mf_cv

##3.2： N2O #####
d2 <- read_excel("rice_wheat_rotation_R4.xlsx", sheet="N2O_RF")
dat <- d2
data <- dat %>% rename ("Precipitation"=map, "Temperature"=mat, "Aridity"=aridity, "Clay"=clay, 
                        "TN"=tn, "SOC"=soc, "pH"=ph, "BD"=bd, "Nrate"=n_dose, "Type"=type)
data.train <- sample(nrow(data),7/10*nrow(data))
train <- data[data.train,]
test<-data[-data.train,]
check_conv <- randomForest(as.formula(paste0("N2O~", paste(moderators, collapse = "+"))),
                           data = train,
                           whichweights = "random",
                           num.trees = 500)
X <- dplyr::select(train, all_of(moderators)) 
mf_cv <- train(y = train$N2O,
               x = X,
               method = "rf",
               keep.inbag = TRUE,
               num.threads = 6,
               verbose=TRUE,
               num.trees = 500)
mf_cv$results[which.min(mf_cv$results$RMSE), ] 
importance.export.rel <- varImp(mf_cv)$importance  
fit.forest <- mf_cv


##3.3： CH4 #####
d2 <- read_excel("rice_wheat_rotation_R4.xlsx", sheet="CH4_RF")
dat <- d2
data <- dat %>% rename ("Precipitation"=map, "Temperature"=mat, "Aridity"=aridity, "Clay"=clay, 
                        "TN"=tn, "SOC"=soc, "pH"=ph, "BD"=bd, "Nrate"=n_dose, "Type"=type)
data.train <- sample(nrow(data),7/10*nrow(data))
train <- data[data.train,]
test<-data[-data.train,]
check_conv <- randomForest(as.formula(paste0("CH4~", paste(moderators, collapse = "+"))),
                           data = train,
                           whichweights = "random",
                           num.trees = 800)
X <- dplyr::select(train, all_of(moderators)) 
mf_cv <- train(y = train$CH4,
               x = X,
               method = "rf",
               keep.inbag = TRUE,
               num.threads = 6,
               verbose=TRUE,
               num.trees = 800)
mf_cv$results[which.min(mf_cv$results$RMSE), ] 
importance.export.rel <- varImp(mf_cv)$importance  
fit.forest <- mf_cv
