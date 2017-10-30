indiv <- individuals %>% select(cmte_id, transaction_amt, occupation, employer, state) %>% filter(state=="MA")
commit <- committees %>% select(cmte_id, cmte_name, cmte_type, cmte_party_affiliation, org_type)
commit$org_type[commit$org_type=="C"] <- "Corporation"
commit$org_type[commit$org_type=="L"] <- "Labor organization"
commit$org_type[commit$org_type=="M"] <- "Membership organization"
commit$org_type[commit$org_type=="T"] <- "Trade association"
commit$org_type[commit$org_type=="V"] <- "Cooperative"
commit$org_type[commit$org_type=="W"] <- "Corporation w/o capital stock"
commit$org_type[commit$org_type==" "] <- "NA"



comit$cmte_party_affiliation[comit$cmte_party_affiliation==" "] <- "NA"
comit$cmte_party_affiliation[comit$cmte_party_affiliation=="UNK"] <- "NA"
comit$cmte_party_affiliation[comit$cmte_party_affiliation=="NNE"] <- "None"
comit$cmte_party_affiliation[comit$cmte_party_affiliation=="REP"] <- "Rep"
comit$cmte_party_affiliation[comit$cmte_party_affiliation=="DEM"] <- "Dem"

df1 <- indiv %>% left_join(comit, by ="cmte_id")
df <- df1 %>% group_by(org_type) %>% summarise(summ=sum(transaction_amt)) %>% filter(org_type %in% c("Corporation", "Labor organization", "Membership organization", "Trade association", "Cooperative", "Corporation w/o capital stock"))
df2 <- df1 %>% group_by(cmte_party_affiliation, org_type) %>% filter(org_type %in% c("Corporation", "Labor organization", "Membership organization", "Trade association", "Cooperative", "Corporation w/o capital stock"), cmte_party_affiliation %in% c("None", "Rep", "Dem"))

indiv_state <- individuals %>% select(cmte_id, transaction_amt, occupation, employer, state)
df_state <- indiv_state %>% left_join(commit, by ="cmte_id")


df_state <- df_state %>% group_by(state, org_type) %>% summarise(summ=sum(transaction_amt)) %>% filter(org_type != "NA", state !=" ")
View(df_state)

ggplot(df_state, aes(x=factor(org_type), y=summ)) + geom_bar(stat="identity", aes(fill=factor(org_type))) + theme(axis.text.x=element_text(angle=90, hjust=1)) +   facet_wrap(~state)
