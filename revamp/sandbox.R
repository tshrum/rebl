# Item Fit ----------------------------------------------------------------
## ltm ---------------------------------------------------------------------


get_str(model_uncon)

# Item Fit
get_str(ltm::item.fit(model_uncon))
ltm::item.fit(model_uncon)

ltm::item.fit(model_uncon)$p.values %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  setNames(c('REBL Item', 'P')) %>%
  arrange(P)



# Person Fit --------------------------------------------------------------
## ltm ---------------------------------------------------------------------


fits <- ltm::person.fit(model_uncon)
get_str(ltm::person.fit(model_uncon))
fits$Tobs
# Want to combine Tobs and p.values

ltmfits <- bind_cols(fits$Tobs, fits$p.values) %>%
  setNames(c('L0', 'Lz', 'p'))
get_str(ltmfits)
# L0 stat from Levine and Rubin 1979
# Lz standardized versino from Drasgow et al 1985
# p values calculated for Lz assuming standard normal in null



# Person Ability ----------------------------------------------------------
## ltm ---------------------------------------------------------------------


scores <- ltm::factor.scores(model_uncon)
get_str(scores)

fix_lumped_ltm_scores(df, scores, rebl_items)

scores <- ltm::factor.scores(model_tpm)
get_str(scores)
fix_lumped_ltm_scores(df, scores, rebl_items)

