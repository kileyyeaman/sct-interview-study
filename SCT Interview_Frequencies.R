library(dplyr)
library(summarytools)
# Loading csv file
SCTdat <- read.csv('/Users/kileyyeaman/Desktop/SCT Interview Codes_MERGED 1.10.21.csv')

# Converting -99 to NA
SCTdat[SCTdat==-99] <- NA

# Subsetting datasets by parent and child
SCTdat_c <- subset(SCTdat, Interviewee == "Child")
SCTdat_p <- subset(SCTdat, Interviewee == "Parent")

# Frequencies
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100,2))
  colnames(res) <- c('Count','Percentage')
  res
}

child_freq <- lapply(SCTdat_c, tblFun)
parent_freq <- lapply(SCTdat_p, tblFun)

# Treatment domain frequencies
tx_top2 <- cbind(SCTdat_p$tx_domain1, SCTdat_p$tx_domain2)
tblFun(tx_top2)

tbl_d1 <- tblFun(SCTdat_p$tx_domain1)
tbl_d2 <- tblFun(SCTdat_p$tx_domain2)

tbl_d1 <- as.data.frame(tbl_d1)
tbl_d2 <- as.data.frame(tbl_d2)

rownames(tbl_d1) <- c("1", "2", "3", "4", "6", "8", "9", "13")
tbl_d1 <- data.frame(Domain=rownames(tbl_d1), tbl_d1)
rownames(tbl_d1) <- NULL
tbl_d1 <- subset(tbl_d1,select = -Percentage)

       
rownames(tbl_d2) <- c("1", "2", "5", "6", "7", "8", "9", "10", "13")
tbl_d2 <- data.frame(Domain=rownames(tbl_d2), tbl_d2)
rownames(tbl_d2) <- NULL
tbl_d2 <- subset(tbl_d2,select = -Percentage)

tbl_all <- dplyr::full_join(tbl_d1, tbl_d2, by = "Domain")
tbl_all$First <- tbl_all$Count.x
tbl_all$Second <- tbl_all$Count.y
tbl_all <- subset(tbl_all,select = -c(Count.x, Count.y))
0 -> tbl_all[is.na(tbl_all)] 

tbl <- cbind(tbl_all[,2], tbl_all[,3])
tbl_all$Combined <- rowSums(tbl)

tbl_all$Percentage <- tbl_all$Combined/15
tbl_all$Percentage <- round(tbl_all$Percentage,4)*100

tx_top5 <- cbind(SCTdat_p$tx_domain3, SCTdat_p$tx_domain4, SCTdat_p$tx_domain5)
tblFun(tx_top5)
View(SCTdat_p)

## PARENT DATA

# Removing global functioning 
drop <- grepl("_diff", names(SCTdat_p))
SCTdat_p <- SCTdat_p[!drop]

# Removing empty columns
emptycols <- sapply(SCTdat_p, function (k) all(is.na(k)))
SCTdat_p <- SCTdat_p[!emptycols]

# Making values dichotomous 
values <- grepl("_neg|_pos", names(SCTdat_p))

SCTdat_p[values] <- lapply(SCTdat_p[values], 
                          function(x) replace(x,x %in% 0:1,
                                              "None/Minimal"))
SCTdat_p[values] <- lapply(SCTdat_p[values], 
                             function(x) replace(x,x %in% 2:3,
                                                 "Moderate/Major"))


## CHILD DATA

# Removing global functioning 
drop <- grepl("_diff", names(SCTdat_c))
SCTdat_c <- SCTdat_c[!drop]

# Removing empty columns
emptycols <- sapply(SCTdat_c, function (k) all(is.na(k)))
SCTdat_c <- SCTdat_c[!emptycols]

# Making values dichotomous 
values <- grepl("_neg|_pos", names(SCTdat_c))

SCTdat_c[values] <- lapply(SCTdat_c[values], 
                           function(x) replace(x,x %in% 0:2,
                                               "None/Minimal"))
SCTdat_c[values] <- lapply(SCTdat_c[values], 
                           function(x) replace(x,x %in% 2:3, 
                                               "Moderate/Major"))

# Relabelling impact variables

impact <- grepl("_imp|_ext|_term|SCT_overall|CDD_overall", names(SCTdat_p))

SCTdat_p[impact] <- lapply(SCTdat_p[impact], 
                           function(x) replace(x,x %in% 0,
                                               "Neutral"))
SCTdat_p[impact] <- lapply(SCTdat_p[impact], 
                           function(x) replace(x,x %in% 1, 
                                               "Positive"))
SCTdat_p[impact] <- lapply(SCTdat_p[impact], 
                           function(x) replace(x,x %in% 2, 
                                               "Negative"))


yesno <- grepl("_avoid|_involve|_relax|_active|strengths_overall|awareness|_professional|_skills", names(SCTdat_p))

SCTdat_p[yesno] <- lapply(SCTdat_p[yesno], 
                           function(x) replace(x,x %in% 0,
                                               "No"))
SCTdat_p[yesno] <- lapply(SCTdat_p[yesno], 
                           function(x) replace(x,x %in% 1, 
                                               "Yes"))

SCTdat_p$ec_set[SCTdat_p$ec_set == 0] <- "No preference"
SCTdat_p$ec_set[SCTdat_p$ec_set == 1] <- "Smaller, 1:1 settings"
SCTdat_p$ec_set[SCTdat_p$ec_set == 2] <- "Larger group settings"

SCTdat_p$ec_type[SCTdat_p$ec_type == 0] <- "No preference"
SCTdat_p$ec_type[SCTdat_p$ec_type == 1] <- "Prefers leisurely/relaxing activities"
SCTdat_p$ec_type[SCTdat_p$ec_type == 2] <- "Prefers physical/active activties"

SCTdat_p$social_bf[SCTdat_p$social_bf == 0] <- "Does not have a best friend"
SCTdat_p$social_bf[SCTdat_p$social_bf == 1] <- "Has a best friend"


SCTdat_p$social_friends[SCTdat_p$social_friends == 0] <- "Does not have many friends"
SCTdat_p$social_friends[SCTdat_p$social_friends == 1] <- "Has a few good friends"
SCTdat_p$social_friends[SCTdat_p$social_friends == 2] <- "Has a lot of friends"


SCTdat_p$tx_length[SCTdat_p$tx_length == 0] <- "No strong preference"
SCTdat_p$tx_length[SCTdat_p$tx_length == 1] <- "Fewer than 4 weeks"
SCTdat_p$tx_length[SCTdat_p$tx_length == 2] <- "4-8 weeks"
SCTdat_p$tx_length[SCTdat_p$tx_length == 3] <- "More than 8 weeks"

SCTdat_p$tx_set[SCTdat_p$tx_set == 0] <- "No strong preference"
SCTdat_p$tx_set[SCTdat_p$tx_set == 1] <- "In-school with counselor"
SCTdat_p$tx_set[SCTdat_p$tx_set == 2] <-"Outside of school with therapist"

SCTdat_p$tx_pullout[SCTdat_p$tx_pullout == 0] <- "No strong preference"
SCTdat_p$tx_pullout[SCTdat_p$tx_pullout == 1] <- "Not okay with child being pulled out of school"
SCTdat_p$tx_pullout[SCTdat_p$tx_pullout == 2] <-  "Okay with child being pulled out of school"

SCTdat_p$tx_structure[SCTdat_p$tx_structure == 0] <- "No strong preference"
SCTdat_p$tx_structure[SCTdat_p$tx_structure == 1] <- "Meet individually as family"
SCTdat_p$tx_structure[SCTdat_p$tx_structure == 2] <- "Meet with other families"


SCTdat_p$tx_phoneVid[SCTdat_p$tx_phoneVid == 0] <- "No strong preference"
SCTdat_p$tx_phoneVid[SCTdat_p$tx_phoneVid == 1] <- "No interest in phone/video check in"
SCTdat_p$tx_phoneVid[SCTdat_p$tx_phoneVid == 2] <- "Interested in phone/video check in"

SCTdat_p$tx_childInvolve[SCTdat_p$tx_childInvolve == 0] <- "No strong preference"
SCTdat_p$tx_childInvolve[SCTdat_p$tx_childInvolve == 1] <- "Child meets individually"
SCTdat_p$tx_childInvolve[SCTdat_p$tx_childInvolve == 2] <- "Child meets with other peers"

SCTdat_p$tx_famInvolve[SCTdat_p$tx_famInvolve == 0] <- "No strong preference"
SCTdat_p$tx_famInvolve[SCTdat_p$tx_famInvolve == 1] <- "Child meets 1:1"
SCTdat_p$tx_famInvolve[SCTdat_p$tx_famInvolve == 2] <- "Parent(s) meet 1:1"
SCTdat_p$tx_famInvolve[SCTdat_p$tx_famInvolve == 3] <- "Meet collectively as a family"


SCTdat_p$tx_meds[SCTdat_p$tx_meds == 0] <- "No strong preference"
SCTdat_p$tx_meds[SCTdat_p$tx_meds == 1] <- "Does not want to pursue medication"
SCTdat_p$tx_meds[SCTdat_p$tx_meds == 2]  <- "Comfortable with pursuing medication"


SCTdat_p$tx_approach[SCTdat_p$tx_approach == 0] <- "No strong preference"
SCTdat_p$tx_approach[SCTdat_p$tx_approach == 1] <- "Prefers medication" 
SCTdat_p$tx_approach[SCTdat_p$tx_approach == 2] <- "Prefers psychoscial treatment"
SCTdat_p$tx_approach[SCTdat_p$tx_approach == 3] <- "Prefers combination of both"


# Data frame summary
parent_frequencies  <- subset(SCTdat_p,
       select = -c(tx_domain1, tx_domain2, tx_domain3,
                   tx_domain4, tx_domain5, ID, Interviewee))

freqPsum<-dfSummary(parent_frequencies,
               varnumbers = F,
               graph.col = F,
               plain.ascii = T,
               labels.col = T, 
               max.string.width = 60)
view(freqPsum)

# domains of functioning
parent_domains <- subset(parent_frequencies,
                              select = -c(tx_length, tx_famInvolve,
                                          tx_set, tx_pullout, tx_structure,
                                          tx_phoneVid, tx_childInvolve, 
                                          tx_meds, tx_approach, SCT_overall,
                                          SCT_neg, SCT_pos, CDD_overall, CDD_neg,
                                          CDD_pos, alt_term, SCTtx_skills, SCTtx_pos,
                                          SCTtx_neg, tx_professional, ADHDtx_pos,
                                          ADHDtx_neg, ADHDtx_imp))
domainP<- dfSummary(parent_domains,
                    varnumbers = F,
                    graph.col = F,
                    plain.ascii = T,
                    labels.col = T, 
                    max.string.width = 40)
view(domainP)

# SCT treatment
treatment <- subset(parent_frequencies,
                         select = c( tx_length, tx_childInvolve, 
                                    tx_famInvolve,tx_set, tx_pullout, tx_structure,
                                     tx_phoneVid,  tx_meds, tx_approach,
                                    tx_professional, SCTtx_skills, SCTtx_pos,
                                     SCTtx_neg,  ADHDtx_pos, ADHDtx_neg, ADHDtx_imp))
txP<- dfSummary(treatment,
                    varnumbers = F,
                    graph.col = F,
                    plain.ascii = T,
                    labels.col = T, 
                    max.string.width = 40)
view(txP)

# SCT terminology

terminology <- subset(parent_frequencies,
                      select = c( SCT_overall,
                                  SCT_neg, SCT_pos, CDD_overall, CDD_neg,
                                  CDD_pos, alt_term))
term<- dfSummary(terminology,
                varnumbers = F,
                graph.col = F,
                plain.ascii = T,
                labels.col = T, 
                max.string.width = 40)
view(term)

# CHILD FREQUENCIES

View(SCTdat_c)
SCTdat_c <- subset(SCTdat_c, 
                   select = -c(ID, Interviewee))
# Relabelling impact variables
impact <- grepl("_imp", names(SCTdat_c))

SCTdat_c[impact] <- lapply(SCTdat_c[impact], 
                           function(x) replace(x,x %in% 0,
                                               "Neutral"))
SCTdat_c[impact] <- lapply(SCTdat_c[impact], 
                           function(x) replace(x,x %in% 1, 
                                               "Positive"))
SCTdat_c[impact] <- lapply(SCTdat_c[impact], 
                           function(x) replace(x,x %in% 2, 
                                               "Negative"))

# treatment extent
extent <- grepl("_extent", names(SCTdat_c))

SCTdat_c[extent] <- lapply(SCTdat_c[extent], 
                           function(x) replace(x,x %in% 0:1,
                                               "None/Minimal"))

SCTdat_c[extent] <- lapply(SCTdat_c[extent], 
                           function(x) replace(x,x %in% 2,
                                               "Major"))


SCTdat_c$ec_set[SCTdat_c$ec_set == 0] <- "No preference"
SCTdat_c$ec_set[SCTdat_c$ec_set == 1] <- "Smaller, 1:1 settings"
SCTdat_c$ec_set[SCTdat_c$ec_set == 2] <- "Larger group settings"

SCTdat_c$ec_type[SCTdat_c$ec_type == 0] <- "No preference"
SCTdat_c$ec_type[SCTdat_c$ec_type == 1] <- "Prefers leisurely/relaxing activities"
SCTdat_c$ec_type[SCTdat_c$ec_type == 2] <- "Prefers physical/active activties"


SCTdat_c$social_bf[SCTdat_c$social_bf == 0] <- "Does not have a best friend"
SCTdat_c$social_bf[SCTdat_c$social_bf == 1] <- "Has a best friend"


SCTdat_c$social_friends[SCTdat_c$social_friends == 0] <- "Does not have many friends"
SCTdat_c$social_friends[SCTdat_c$social_friends == 1] <- "Has a few good friends"
SCTdat_c$social_friends[SCTdat_c$social_friends == 2] <- "Has a lot of friends"

SCTdat_c$posper[SCTdat_c$posper == 0] <- "Neutral"
SCTdat_c$posper[SCTdat_c$posper == 1] <- "No"
SCTdat_c$posper[SCTdat_c$posper == 2] <- "Yes"

SCTdat_c$negper[SCTdat_c$negper == 0] <- "Neutral"
SCTdat_c$negper[SCTdat_c$negper == 1] <- "No"
SCTdat_c$negper[SCTdat_c$negper == 2] <- "Yes"

SCTdat_c$tx_interest[SCTdat_c$tx_interest == 0] <- "No preference"
SCTdat_c$tx_interest[SCTdat_c$tx_interest == 1] <- "No"
SCTdat_c$tx_interest[SCTdat_c$tx_interest == 2] <- "Yes"


SCTdat_c$SCTtx_skills[SCTdat_c$SCTtx_skills == 0] <- "No"
SCTdat_c$SCTtx_skills[SCTdat_c$SCTtx_skills == 1] <- "Yes"

SCTdat_c$dayd_constraint[SCTdat_c$dayd_constraint == 0] <- "Neutral"
SCTdat_c$dayd_constraint[SCTdat_c$dayd_constraint == 1] <- "No control"
SCTdat_c$dayd_constraint[SCTdat_c$dayd_constraint == 2] <- "Control"
SCTdat_c$dayd_constraint[SCTdat_c$dayd_constraint == 3] <- "Both control and no control"
  
  
SCTdat_c$dayd_content[SCTdat_c$dayd_content == 0] <- "Neutral"
SCTdat_c$dayd_content[SCTdat_c$dayd_content == 1] <- "Fantasy, imagination, make believe"
SCTdat_c$dayd_content[SCTdat_c$dayd_content == 2] <- "Real-life experiences"
SCTdat_c$dayd_content[SCTdat_c$dayd_content == 3] <- "Both kinds of content"

child_frequencies <- SCTdat_c
childFreq <- dfSummary(child_frequencies,
                 varnumbers = F,
                 graph.col = F,
                 plain.ascii = T,
                 labels.col = T, 
                 max.string.width = 40,
                 style = "multiline")
view(childFreq)
?dfSummary
