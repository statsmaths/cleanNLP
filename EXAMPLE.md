Code for building the example dataset in the package:

```{r}
library(cleanNLP)
library(sotu)

cnlp_init_spacy(model_name = "en_core_web_sm", entity_flag = TRUE)
txt <- sotu_text[sotu_meta$president == "Barack Obama"]
obama <- cnlp_annotate(txt,
                        as_strings = TRUE,
                        doc_ids = sprintf("doc%d", 2009:2016),
                        meta = sotu_meta[sotu_meta$president == "Barack Obama",])

# fix encoding issue from BOMs in spacy:
Encoding(obama$token$lemma) <- "latin1"
obama$token$lemma <- iconv(obama$token$lemma, from = "latin1", to = "ASCII", sub = "")

save(obama, file = "data/obama.rda")
```

