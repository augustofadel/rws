# ler arquivo de entrada --------------------------------------------------

ler_cnpj <- function(file_in, delim = NULL) {
   if (is.null(delim)) {
      delim <- ifelse(stringr::str_sub(file_in, -4) == ".txt", " ", ",")
   }
   n_col <- readr::count_fields(file_in, readr::tokenizer_delim(delim), n_max = 1)
   dat <-
      readr::read_delim(
         file_in,
         delim = delim,
         col_types = rep("c", n_col) %>% paste0(collapse = ""),
         col_names = F
      ) %>%
      apply(1, paste, collapse = "")

   return(dat)
}


# consultar receitaws -----------------------------------------------------

rws_get <- function(i, url, tout, msg_end = "\n") {

   aux <- try(httr::GET(paste0(url, i), httr::timeout(tout)), silent = T)

   if (attr(aux, "class") == "try-error") {
      msg <- paste("cnpj:", i, "[nao encontrado]", msg_end)
      return(list(dat = NULL, code = 0, msg = msg))
   } else if (aux$status_code == 429) {
      msg <- paste("cnpj:", i, "[aguardando...]", msg_end)
      return(list(dat = NULL, code = aux$status_code, msg = msg))
   } else if(aux$status_code == 200) {
      dat <- httr::content(aux, as = "parsed")
      if (dat$status == "ERROR") {
         msg <- paste("cnpj:", i, "[invalido]", msg_end)
         return(list(dat = NULL, code = 0, msg = msg))
      } else {
         msg <- paste("cnpj:", i, "[coletado]", msg_end)
         return(list(dat = dat, code = aux$status_code, msg = msg))
      }
   } else {
      msg <- paste("cnpj:", i, "[nao foi possivel coletar]", msg_end)
      return(list(dat = NULL, code = 0, msg = msg))
   }
}


# converter lista para csv ------------------------------------------------

lista <- function(x) { unlist(lapply(x, class)) == "list" }

rws_converter <- function(dat) {
   nnull <- unlist(lapply(dat, is.null))
   if (all(nnull))
      return(NULL)
   tab <- do.call(
      plyr::rbind.fill,
      dat[!nnull] %>% lapply(function(x) as.data.frame(x[!lista(x)]))
   )
   tab <-
      tab %>%
      dplyr::select(
         cnpj,
         nome,
         fantasia,
         tipo,
         situacao,
         motivo_situacao,
         data_situacao,
         situacao_especial,
         data_situacao_especial,
         logradouro,
         numero,
         complemento,
         bairro,
         municipio,
         uf,
         cep,
         telefone,
         email,
         natureza_juridica,
         abertura,
         capital_social,
         ultima_atualizacao
      ) %>%
      dplyr::mutate(
         cnpj = stringr::str_replace_all(cnpj, "[^[:digit:]]", ""),
         cep = stringr::str_replace_all(cep, "[^[:digit:]]", ""),
         natureza_juridica = stringr::str_replace_all(natureza_juridica, "[^[:digit:]]", "")
      )

   return(tab)
}
