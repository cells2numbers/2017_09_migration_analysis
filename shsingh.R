
dfx <- profile %>% 
  filter(Metadata_condition %in%  c("pred", "vehi", "cont")) %>% 
  select(-RadialDistribution_ZernikePhase_neutrosIlluCorrected_2_0) # remove bad feature 

# 
variables <- dfx %>% 
  select(-matches("Metadata")) %>% 
  colnames()

# 
vehi_mat <- dfx %>%
  filter(Metadata_condition == "vehi") %>%
  select(-matches("Metadata")) %>% 
  as.matrix()

# 
m <- prcomp(na.omit(vehi_mat), scale. = T)

apply()
dfx_mat <- as.matrix(dfx)

variables <- dfx %>% select(-matches("Metadata")) %>% colnames() 
variables <- setdiff(variables, str_subset(variables, "Track"))

dfx %<>% select(matches("Metadata"), variables)
dfw <- cytominer::whiten(population = dfx, sample = dfx %>% filter(Metadata_condition == "vehi"), variables = variables)


PCn <- 140
xx <- m$x[,PCn]
xx <- dplyr::bind_cols(xx %>% as_data_frame(), dfx %>% select(Metadata_condition, Metadata_id, Metadata_matrix))


#ggplot(xx, aes(PC1, PC2, color = Metadata_condition)) + geom_point() + facet_wrap(~interaction(Metadata_id, Metadata_matrix), nrow = 2)

ggplot(xx, aes(value, fill = Metadata_condition)) + geom_histogram(alpha = 0.3, binwidth = .5, position = "identity") + facet_wrap(~Metadata_matrix, nrow = 2)


ggplot(dfw, aes(PC1, fill = Metadata_condition)) + geom_histogram(alpha = 0.3, binwidth = .5, position = "identity") + facet_wrap(~Metadata_matrix, nrow = 2)

