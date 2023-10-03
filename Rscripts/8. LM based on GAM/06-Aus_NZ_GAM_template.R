
################################################################################
# library
  library(geodata)
  library(tidyverse)
  library(terra)
  library(sf)
  library(mgcv)
  library(ggrepel)
  
  rm(list = ls())

  source("RScripts/7. GAM/04-GAM_model_terra_Aus.R")
  source("RScripts/7. GAM/05-GAM_model_terra_NZ.R")
  
################################################################################
# predict unscaled species richness Australia -> NZ 
  aus_pr <- function(x) {
    pred_x <- exp(predict(x, newdata = nz_env))
    r <- temp_nz
    # val <- as.vector(pred_x / max(pred_x, na.rm = T))
    val <- as.vector(pred_x)
    values(r) <- val
    r <- setValues(r, val)
    return(r)
  }
  
  pr_aus_nz_nc3 <- aus_pr(m_nat_C3_au)
  pr_aus_nz_nc4 <- aus_pr(m_nat_C4_au)
  pr_aus_nz_nnc3 <- aus_pr(m_exo_C3_au)
  pr_aus_nz_nnc4 <- aus_pr(m_exo_C4_au)
  
  pr_aus_aus_nc3 <- exp(m_nat_C3_au)
  
  mod_f1 <- c(pr_aus_nz_nc3, pr_aus_nz_nc4, pr_aus_nz_nnc3, pr_aus_nz_nnc4)
  names(mod_f1) <- c("pr_aus_nz_nc3", "pr_aus_nz_nc4", "pr_aus_nz_nnc3",
                     "pr_aus_nz_nnc4")
  mod_f1
  

  
# scale species richness
  scale <- function(j){
    val <- app(j, fun=function(x){ x / max(x, na.rm = T)} )
    return(val)
  }
  
  zero <- function(k){
    k[is.na(k)] <- 0
    k <- crop(k, temp_nz)
  return(k)
  }  
  
# scale variables to 0 - 1  and
# zero exotic NA cells
  mod_f1$exo_c3_sc <- zero(scale(rich$exo_C3))
  mod_f1$exo_c4_sc <- zero(scale(rich$exo_C4))
  
  mod_f1$pr_aus_nz_nc3_sc <- scale(mod_f1$pr_aus_nz_nc3)
  mod_f1$pr_aus_nz_nc4_sc <- scale(mod_f1$pr_aus_nz_nc4)
  
  mod_f1$pr_nz_nz_nc3_sc <- scale(nz_mod$nat_C3)
  mod_f1$pr_nz_nz_nc4_sc <- scale(nz_mod$nat_C4)
  
# get the differences
  mod_f1$dif_aus_c3 <- mod_f1$pr_aus_nz_nc3_sc - mod_f1$exo_c3_sc
  mod_f1$dif_aus_c4 <- mod_f1$pr_aus_nz_nc4_sc - mod_f1$exo_c3_sc
  
  mod_f1$dif_nz_c3 <- mod_f1$pr_nz_nz_nc3_sc - mod_f1$exo_c3_sc
  mod_f1$dif_nz_c4 <- mod_f1$pr_nz_nz_nc4_sc - mod_f1$exo_c3_sc
  
  
# plot native nz predicted - non-native nz obs   
  pdf("Results/figures/ESA F1.pdf")
  
  par(mfrow = c(3, 2), mar = c(2, 4, 4, 1))
  
  plot(mod_f1$pr_aus_nz_nc3, main = "Aus-origin native C3")
  plot(nz_sh, add = T)
  mtext("A", line = 0.5, adj = 0, cex = 1.2)
  
  plot(mod_f1$pr_aus_nz_nc4, main = "Aus-origin native C4")
  plot(nz_sh, add = T)
  mtext("B", line = 0.5, adj = 0, cex = 1.2)
  
# max and min values for legend
  mm_aus <- range(c(values(mod_f1$dif_aus_c3)[, 1], values(mod_f1$dif_aus_c4)[, 1]), na.rm = T)
  plot(mod_f1$dif_aus_c3, col = grDevices::hcl.colors(50, "Blue-Red 2"), range = mm_aus,
       main = "A-O C3 difference")
  plot(nz_sh, add = T)
  mtext("C", line = 0.5, adj = 0, cex = 1.2)
  
  plot(mod_f1$dif_aus_c4, col = grDevices::hcl.colors(50, "Blue-Red 2"), range = mm_aus,
       main = "A-O C4 difference")
  plot(nz_sh, add = T)
  mtext("C", line = 0.5, adj = 0, cex = 1.2)
  
  mm_nz <- range(c(values(mod_f1$dif_nz_c3)[, 1], values(mod_f1$dif_nz_c4)[, 1]), na.rm = T)
  
  plot(mod_f1$dif_aus_c3, col = grDevices::hcl.colors(50, "Blue-Red 2"), range = mm_nz,
       main = "NZ-O C3 difference")
  plot(nz_sh, add = T)
  mtext("E", line = 0.5, adj = 0, cex = 1.2)
  
  plot(mod_f1$dif_aus_c4, col = grDevices::hcl.colors(50, "Blue-Red 2"), 
       range = mm_nz, main = "NZ-O C4 difference")
  plot(nz_sh, add = T)
  mtext("F", line = 0.5, adj = 0, cex = 1.2)
  
  dev.off()
  
################################################################################  
# correlation plots
  cor_df_nz <- data.frame(aus_nz_pred = as.vector(mod_f1$pr_aus_nz_nc3),
                          nz_nat_obs = nz_vars$nat_C3,
                          nz_nat_pred = as.vector(nz_mod$nat_C3),
                          nz_exo = nz_vars$exo_C3)
  

  cor_df_aus <- data.frame(aus_nat_obs = aus_vars$nat_C3,
                           aus_nat_pred = as.vector(aus_mod$nat_C3))
  
  cor_table <- data.frame(aus = round(cor(cor_df_aus$aus_nat_obs, 
                                            cor_df_aus$aus_nat_pred, 
                                            use = "complete.obs", 
                                            method = "spearman"), 2),
                          nz_nz_nat = round(cor(cor_df_nz$nz_nat_obs, 
                                            cor_df_nz$nz_nat_pred, 
                                            use = "complete.obs", 
                                            method = "spearman"), 2),
                          aus_nz_nat = round(cor(cor_df_nz$aus_nz_pred, 
                                            cor_df_nz$nz_nat_pred, 
                                            use = "complete.obs", 
                                            method = "spearman"), 2),
                          nz_nz_exo = round(cor(cor_df_nz$nz_exo, 
                                            cor_df_nz$nz_nat_pred, 
                                            use = "complete.obs", 
                                            method = "spearman"), 2),
                          aus_nz_exo = round(cor(cor_df_nz$aus_nz_pred, 
                                            cor_df_nz$nz_exo, 
                                            use = "complete.obs", 
                                            method = "spearman"), 2)
                          )
  
# plot function  
 raw_cor <- function(dat, title, xlab, ylab, cor, label, x_pos, y_pos, save) {
    q <- ggplot(aes(x = x, y = y), data = dat) +
      geom_point(shape = "circle", size = 2) +
      #geom_abline(intercept = 0, slope = 1, size = 1, linetype = "dashed") +
      theme_bw() + 
      labs(x = xlab, y = ylab) +
      theme(panel.border = element_blank(),
            plot.title = element_text(size = 22),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "black", size = 1),
            axis.text.x = element_text(colour = "black", size = 16),
            axis.text.y = element_text(colour = "black", size = 16),
            axis.ticks.length = unit(0.25, "cm"),
            axis.ticks = element_line(colour = "black", size = 1))
      # scale_x_continuous(breaks = c(0, 0.5, 1), 
      #                    limits = c(0, 1), 
      #                    expand = c(0, 0.05)) +
      # scale_y_continuous(breaks = c(0, 0.5,  1), 
      #                    limits = c(0, 1), 
      #                    expand = c(0, 0.05)) +
      # annotate("text", x = x_pos, y = y_pos, label = label, size = 7) 
    q
    ggsave(save, plot = last_plot(), dpi = 500, width = 10, height = 9, 
           units = "cm", device = "jpeg")   
    return(q)
 }
 
# aus nat obs x aus nat pred 
  y <- cor_df_aus$aus_nat_pred
  x <- cor_df_aus$aus_nat_obs
  dat <- data.frame(cbind(x, y))
  
# labels
  title <- "ESA Aus C3 native obs - pred"
  xlab <- "Observed native AU\nrichness"
  ylab <- "Predicted native AU\nrichness" 
# correlation
  cor <- round(cor(x, y, use = "complete.obs", method = "spearman"), 2)
  label <- paste0("r = ", cor)
# # text position
#   x_pos <- 0.2
#   y_pos <- 0.95
# save
  save <- paste0("Results/correlation plots/", title, ".jpeg")
# run  
  raw_cor(dat, title, xlab, ylab, cor, label, x_pos, y_pos, save)
