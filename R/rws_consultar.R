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
   file_tmp <- file_out %>% stringr::str_replace("\\..*", "rds")

   if (!verbose) {
      cat("\nConsulta iniciada:\n\n")
      pb <- txtProgressBar(0, n_cnpj, style = 3)
      pb_cnt <- 1
   }
   for (i in cnpj) {
      dat_i <- list(dat = NULL, code = 429, msg = "")
      while (dat_i$code == 429) {
         dat_i <- rws_get(i, url, tout, msg_end = "")
         if (!is.null(dat_i$dat)) result[[i]] <- dat_i$dat
         if (verbose) cat(dat_i$msg,
                          round(which(i == cnpj) / n_cnpj * 100), "%\n",
                          sep = "")
         if (dat_i$code == 429) round(25 * runif(1, 2, 3)) %>% Sys.sleep
      }
      saveRDS(result, file_tmp, compress = comp)
      if (!verbose) {
         setTxtProgressBar(pb, pb_cnt)
         pb_cnt <- pb_cnt + 1
      }
   }
   if (!verbose) close(pb)
   tab <- rws_converter(result)
   if (is.null(tab)) {
      cat("\nConsulta concluida.",
          "\n\nNenhum dos CNPJ fornecidos foi encontrado.\n\n")
      file.remove(file_tmp)
   } else {
      # ext <- file_out %>% stringr::str_extract("\\..*")
      ext <- tools::file_ext(file_out)
      if (ext %in% c("xls", "xlsx")) {
         writexl::write_xlsx(tab, file_out)
      } else if (ext == "csv") {
         readr::write_csv(tab, file_out)
      }
      if (dplyr::coalesce(as.numeric(Sys.time() - file.info(file_out)$mtime,
                                     units = "hours") < .0015, F)) {
         file.remove(file_tmp)
      } else {
         cat("\nNao foi possivel salvar o arquivo ", file_out,".\n", sep = "")
         cat("Os dados estao salvos em ", file_tmp, ".\n", sep = "")
      }
      tot <- sum(!unlist(lapply(result, is.null)))
      cat("\nConsulta concluida. Foram encontradas informacoes de ",
          tot, " [", round(tot / n_cnpj * 100), "%] CNPJ.\n\n", sep = "")
      cat("Informacoes de CNAE e quadro societario foram excluidas.\n\n")
   }
   return(tab)
}
