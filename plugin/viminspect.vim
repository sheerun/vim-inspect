" So we can use :Inspect to call our function
command! -nargs=+ -complete=shellcmd Inspect call viminspect#Inspect(<q-args>)
