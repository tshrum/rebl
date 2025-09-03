#' Get Rasch Analysis Outputs
#'
#' Get all the diagrams, fits, and statistics from Rasch models (eRm). 
#' This function generates comprehensive outputs including plots and CSV files
#' with person and item fit statistics.
#'
#' @param models List of CML models from eRm
#' @param surveys List of clean survey data to extract prolificIDs
#' @param plot_path Path to folder where plots should be saved
#' @param csv_path Path to folder where CSV files should be saved
#' @param rebl_items Vector of final REBL item names
#' @param seed Random seed for reproducible sampling (default: 42)
#'
#' @return List containing person_fit and item_fit dataframes for all surveys
#' @export
#'
#' @details
#' This function generates and saves:
#' \itemize{
#'   \item PI Maps saved to plot folder
#'   \item PW Maps saved to plot folder
#'   \item ICC Plots saved to plot folder
#'   \item REBL Score Histograms saved to plot folder
#'   \item Person Fit CSV files and dataframes
#'   \item Item Fit CSV files and dataframes
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage
#' results <- get_rasch_outputs(
#'   models = rasch_models,
#'   surveys = clean_surveys,
#'   plot_path = "6_outputs/plots/",
#'   csv_path = "6_outputs/csv/",
#'   rebl_items = final_rebl_items
#' )
#' }
get_rasch_outputs <- function(models,
                              surveys,
                              plot_path,
                              csv_path,
                              rebl_items,
                              seed = 42) {
  
  # Housekeeping -----
  results <- list()
  cat('\nSaving outputs:')
      
  
  # PI Maps  -----
  purrr::iwalk(models, ~ {
    grDevices::png(
      filename = paste0(plot_path, 'pi_map_', .y, '.png'),
      width = 3000,
      height = 2500,
      units = 'px',
      res = 300
    )
    plotPImap2(
      .x, 
      sorted = TRUE,
      main = NA,
      latdim = 'Latent Trait',
      pplabel = 'Latent Trait\nDistribution',
      cex.gen = 0.8,
      margins = c(3, 11, 0, 1)
    )
    grDevices::dev.off()
    cat('\n', paste0(plot_path, 'pi_map_', .y, '.png'))
  })
  
  # PW Item Maps -----
  purrr::iwalk(models, ~ {
    grDevices::png(filename = paste0(plot_path, 'pw_item_map_', .y, '.png'),
        width = 3000,
        height = 2500,
        units = 'px',
        res = 300)
    eRm::plotPWmap(
      .x, 
      pmap = FALSE, 
      imap = TRUE,
      mainitem = NA,
      latdim = 'Item Difficulty',
      tlab = 'Infit t Statistic',
      cex.pch = 1.25,
      cex.gen = 1
    )
    grDevices::dev.off()
    cat('\n',
        paste0(plot_path, 'pw_item_map_', .y, '.png'))
  })
  
  # PW Person Maps -----
  purrr::iwalk(models, ~ {
    grDevices::png(filename = paste0(plot_path, 'pw_person_map_', .y, '.png'),
        width = 3000,
        height = 2500,
        units = 'px',
        res = 300)
    eRm::plotPWmap(
      .x, 
      pmap = TRUE, 
      imap = FALSE,
      mainperson = NA,
      latdim = 'Latent Trait',
      cex.pch = 1.25,
      cex.gen = 1
    )
    grDevices::dev.off()
    cat('\n',
        paste0(plot_path, 'pw_person_map_', .y, '.png'))
  })
  
  # Sampled Person Item Map -----
  purrr::iwalk(models, ~ {

    grDevices::png(filename = paste0(plot_path, 'sampled_person_item_map_', .y, '.png'),
        width = 3000,
        height = 2500,
        units = 'px',
        res = 300)
    set.seed(seed)
    eRm::plotPWmap(
      .x, 
      pmap = TRUE, 
      imap = TRUE,
      item.subset = seq(1, length(.x$betapar), 2),
      person.subset = sample(1:nrow(.x$X), size = 25, replace = FALSE),
      latdim = 'Latent Trait',
      cex.pch = 1.25,
      cex.gen = 1
    )
    grDevices::dev.off()
    cat('\n',
        paste0(plot_path, 'sampled_person_item_map_', .y, '.png'))
  })
  
  # Person Fit -----
  # Get all person fit stats, including parameters (rebl) and prolificID
  # Note that st.res (residuals) are nested matrices
  results$person_fit <- purrr::map2(models, surveys, ~ {
    pp <- eRm::person.parameter(.x)
    
    # Now just person scores with person # and prolific ID
    person_scores <- stats::coef(pp) %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column() %>% 
      stats::setNames(c('person', 'rebl_score')) %>% 
      dplyr::mutate(prolificID = .y$prolificID)
    
    # All other fit statistics for all people. 
    # Separate so that we can pull names from it later
    pfit <- eRm::personfit(pp)[1:7] %>% 
      .[names(.) != 'st.res']

    # Continue getting person fit, join with person_scores    
    person_fit <- pfit %>% 
      purrr::map(as.vector) %>% 
      dplyr::bind_rows() %>%
      as.data.frame() %>%
      dplyr::mutate(person = names(pfit$p.fit)) %>% 
      dplyr::inner_join(person_scores, by = 'person')
    
    if (nrow(person_scores) != nrow(person_fit)) {
      lost <- person_scores %>%
        dplyr::anti_join(person_fit, by = 'person') %>% 
        dplyr::select(person, prolificID) %>% 
        as.vector()
      
      for (i in 1:length(lost$person)) {
        warning(
          lost$person[i],
          ' was lost because they had no pfit statistic. (prolificID: ',
          lost$prolificID[i],
          ')',
          call. = FALSE
        )
      }
    }
    return(person_fit)    
  })
  
  # Now save these as separate CSVs.
  purrr::walk2(results$person_fit, names(results$person_fit), ~ {
    
    .x %>% 
      readr::write_csv(file = paste0(csv_path, 'person_fit_', .y, '.csv'))
    
    cat('\n',
        paste0(csv_path, 'person_fit_', .y, '.csv'))
  })
  
  
  # REBL Hist All -----
  # Get min and max scores from all person fits to set x axis limits
  lower_limit <- purrr::map_dbl(results$person_fit, ~ {
    min(.x$rebl_score)
  }) %>% min()
  
  upper_limit <- purrr::map_dbl(results$person_fit, ~ {
    max(.x$rebl_score)
  }) %>% max()
  
  # Make a hist for each survey with common x axis limits
  rebl_hists <- purrr::map2(results$person_fit, names(results$person_fit), ~ {
    
    .x %>% 
      as.data.frame() %>% 
      ggplot2::ggplot(ggplot2::aes(x = rebl_score)) +
      ggplot2::geom_histogram(binwidth = 0.1,
                     color = "black",
                     fill = "grey") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = 'REBL Score', 
           y = 'Count',
           title = .y) +
      ggplot2::theme(text = ggplot2::element_text(size = 16)) +
      ggplot2::coord_cartesian(ylim = c(0, 125), 
                      xlim = c(lower_limit, upper_limit)) +
      ggplot2::scale_x_continuous(breaks = seq(floor(lower_limit),
                                      ceiling(upper_limit),
                                      1))
  })
  
  # Save arrange them and save it
  arranged <- ggpubr::ggarrange(plotlist = rebl_hists,
                        ncol = 1,
                        nrow = length(models))
  
  ggplot2::ggsave(
    filename = paste0(plot_path, 'rebl_hist_all.png'),
    plot = arranged,
    width = 3000,
    height = 2500,
    units = 'px',
    dpi = 300
  )
  
  cat('\n', paste0(plot_path, 'rebl_hist_all.png'))
  
  # REBL Hists Indy -----
  
  # Individual Histograms without setting axes equal
  rebl_hists_indy <- purrr::map2(results$person_fit, names(results$person_fit), ~ {
    
    .x %>% 
      as.data.frame() %>% 
      ggplot2::ggplot(ggplot2::aes(x = rebl_score)) +
      ggplot2::geom_histogram(binwidth = 0.3,
                     color = "black",
                     fill = "grey") +
      ggplot2::theme_classic() +
      ggplot2::labs(x = 'REBL Score', 
           y = 'Count') +
      ggplot2::theme(text = ggplot2::element_text(size = 16))
  })
  
  # Save individual histograms
  purrr::walk2(rebl_hists_indy, names(models), ~ {
    ggplot2::ggsave(
      filename = paste0(plot_path, 'rebl_hist_', .y, '.png'),
      plot = .x,
      width = 3000,
      height = 2500,
      units = 'px',
      dpi = 300
    ) 
    cat('\n', paste0(plot_path, 'rebl_hist_', .y, '.png'))
  })
  
  
  # ICC Plots -----
  purrr::iwalk(models, ~ {
    
    grDevices::png(filename = paste0(plot_path, 'icc_plot_', .y, '.png'),
        width = 3000,
        height = 2500,
        units = 'px',
        res = 300)
    
    eRm::plotINFO(
      .x,
      type = 'item',
      legpos = NULL,
      mainI = NA,
      xlab = 'REBL Score',
      ylabI = 'Item Information'
    )
    
    grDevices::dev.off()
    
    cat('\n',
        paste0(plot_path, 'icc_plot_', .y, '.png'))
  })
  
  
  # Joint ICC Plots -----
  purrr::iwalk(models, ~ {
    
    grDevices::png(filename = paste0(plot_path, 'joint_icc_plot_', .y, '.png'),
        width = 7,
        height = 6,
        units = 'in',
        res = 300)
    
    graphics::par(mar = c(5, 4, 1, 1) + 0.1)
    
    eRm::plotjointICC(
      .x,
      legend = TRUE,
      legpos = 'bottomright',
      lwd = 2,
      main = NULL,
      xlab = 'REBL Score',
      ylab = 'Probability to Perform REBL Item',
      xlim = c(-5, 6)
    )
    grDevices::dev.off()
    
    cat('\n', paste0(plot_path, 'joint_icc_plot_', .y, '.png'))
  })
  
  # Item Fit Stats -----
  #' Getting all item info for all surveys. Includes fit, infit, outfit, 
  #' discrimination, difficulty, percent_eco, item parameters, SEs, residuals
  results$item_fit <- purrr::map2(models, surveys, ~ {
    
    # Get DF of percent eco for REBL items
    percent_eco_df <- .y %>%
      dplyr::select(dplyr::any_of(rebl_items)) %>%
      colMeans(na.rm = TRUE) %>%
      as.data.frame() %>%
      stats::setNames('percent_eco') %>%
      tibble::rownames_to_column(var = 'rebl_item')
    
    # Get DF of most item fit statistics
    item_fit_df <- .x %>%
      eRm::person.parameter() %>%
      eRm::itemfit() %>% 
      .[-3] %>% 
      purrr::map(as.vector) %>%
      as.data.frame() %>% 
      dplyr::mutate(rebl_item = percent_eco_df$rebl_item)
    
    #' Get DF of item difficulties. There is one less than item fit, I think
    #' because they are all relative to the first?
    item_difficulty_df <- data.frame(eta = .x$etapar,
                                     eta_se = .x$se.eta) %>%
      tibble::rownames_to_column(var = 'rebl_item') %>% 
      dplyr::mutate(rebl_item = stringr::str_remove(rebl_item, '_'))
  
    # Join all three DFs together
    percent_eco_df %>%
      dplyr::full_join(item_fit_df, by = 'rebl_item') %>%
      dplyr::full_join(item_difficulty_df, by = 'rebl_item')
  })
  
  # Now save item fit stats these as 3 separate csvs.
  purrr::walk2(results$item_fit, names(results$item_fit), ~ {
    
    .x %>% 
      readr::write_csv(file = paste0(csv_path, 'item_fit_', .y, '.csv'))
    
    cat('\n',
        paste0(csv_path, 'item_fit_', .y, '.csv'))
  })
  
  # Describe object contents
  cat('\n\nResults object contains:',
      paste(names(results), collapse = ', '),
      '\n')
  
  # Output item fit and person fit 
  return(results)
}