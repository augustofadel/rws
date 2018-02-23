rws_consultar <- function(file_in,
                          delim = NULL,
                          file_out = "ReceitaWS.csv",
                          comp = F,
                          url = "http://www.receitaws.com.br/v1/cnpj/",
                          tout = 0.5,
                          verbose = T) {
   cnpj <- ler_cnpj(file_in, delim)
   n_cnpj <- length(cnpj)
   cat("Carregado arquivo contendo", n_cnpj, "registros.\n")
   resp <- ""
   while (!resp %in% c("s", "n")) {
      resp <- readline("Iniciar a consulta? [s/n] ")
   }
   if (resp == "n") {
      cat("Consulta cancelada.\n")
      break
   }

   result <- vector("list", n_cnpj)
   names(result) <- cnpj
   file_tmp <- file_out %>% str_sub(1, -5) %>% paste0(".rds")

   if (!verbose) pb <- txtProgressBar(0, n_cnpj, style = 3)
   for (i in cnpj) {
      dat_i <- list(dat = NULL, code = 429, msg = "")
      while (dat_i$code == 429) {
         dat_i <- rws_get(i, url, tout)
         if (!is.null(dat_i$dat)) result[[i]] <- dat_i$dat
         if (verbose) cat(dat_i$msg)
         if (dat_i$code == 429) round(25 * runif(1, 2, 3)) %>% Sys.sleep
      }
      saveRDS(result, file_tmp, compress = comp)
      if (!verbose) setTxtProgressBar(pb, i)
   }
   if (!verbose) close(pb)
   rws_converter(result) %>% write_csv(file_out)
   if (coalesce(F, as.numeric(Sys.time() - file.info(file_out)$mtime,
                              units = "hours") < .00015)) {
      file.remove(file_tmp)
   } else {
      cat("\nNao foi possivel salvar o arquivo ", file_out,".\n", sep = "")
      cat("Os dados estao salvos em ", file_tmp, ".\n", sep = "")
      cat("\nInformacoes de CNAE e quadro societario foram excluidas.\n")
   }
   tot <- sum(!unlist(lapply(result, is.null)))
   cat("\n\nConsulta concluida. \n\nForam encontradas informacoes de ",
       tot, " [", round(tot/n_cnpj), "%] CNPJ.\n", sep = "")
}
