## get list of each flip pkg in Remotes
desc.text <- readLines("./DESCRIPTION", warn = FALSE)
m <- regexec("Displayr/[[:alnum:]]*", desc.text)
displayr.remotes <- unlist(regmatches(desc.text, m))

## if none exit

## for each flip pkg above, search for envir var with branch name,
## if none, set branch to master
for (i in seq_along(displayr.remotes))
{
    r <- displayr.remotes[i]
    env.var <- paste0(sub("Displayr/", "", r), "_BRANCH_NAME")
    branch <- Sys.getenv(env.var)
    if (!nzchar(branch))
        branch <- "master"
    ## update remotes in DESCRIPTION to include branch name
    desc.text <- sub(paste0("(", r, ")"), paste0("\\1@", branch), desc.text)
    ## if (nzchar(branch))
    ##     devtools::install_github(paste0(r, "@", branch))
}
writeLines(desc.text, "./DESCRIPTION")
