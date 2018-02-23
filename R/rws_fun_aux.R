# ler arquivo de entrada --------------------------------------------------

ler_cnpj <- function(file_in, delim = NULL) {
   if (is.null(delim)) {
      delim <- ifelse(str_sub(file_in, -4) == ".txt", " ", ",")
   }
   n_col <- count_fields(file_in, tokenizer_delim(delim), n_max = 1)
   dat <-
      read_delim(
         file_in,
         delim = delim,
         col_types = rep("c", n_col) %>% paste0(collapse = ""),
         col_names = F
      ) %>%
      apply(1, paste, collapse = "")

   return(dat)
}


# consultar receitaws -----------------------------------------------------

rws_get <- function(i, url, tout) {

   aux <- try(GET(paste0(url, i), timeout(tout)))

   if (attr(aux, "class") == "try-error") {
      msg <- paste("cnpj:", i, "[nao encontrado]\n")
      return(list(dat = NULL, code = 0, msg = msg))
   } else if (aux$status_code == 429) {
      msg <- paste("cnpj:", i, "[aguardando...]\n")
      return(list(dat = NULL, code = aux$status_code, msg = msg))
   } else if(aux$status_code == 200) {
      dat <- content(aux, as = "parsed")
      msg <- cat("cnpj:", i, "[coletado]\n")
      return(list(dat = dat, code = aux$status_code, msg = msg))
   } else {
      msg <- paste("cnpj:", i, "[nao foi possivel coletar]\n")
      return(list(dat = NULL, code = 0, msg = msg))
   }
}


# converter lista para csv ------------------------------------------------

rws_converter <- function(dat) {
   notnull <- !unlist(lapply(dat, is.null))
   lista <- function(x) { unlist(lapply(x, class)) == "list" }
   tab1 <- do.call(
      "rbind.fill",
      dat[notnull] %>% lapply(function(x) as.data.frame(x[!lista(x)]))
   )
   tab1 <-
      tab1 %>%
      select(
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
      mutate(
         cnpj = str_replace_all(cnpj, "[^[:digit:]]", ""),
         cep = str_replace_all(cep, "[^[:digit:]]", ""),
         natureza_juridica = str_replace_all(natureza_juridica, "[^[:digit:]]", "")
      )

   return(tab)
}
