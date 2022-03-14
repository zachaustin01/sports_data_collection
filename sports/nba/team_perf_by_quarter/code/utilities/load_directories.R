# File containing directories to be used in project (should be loaded at
# beginning of all scripts)
#
# Code chunk to run:
#
# source(file.path('C:/Users/zacha/OneDrive/Desktop/nba_live_betting_model',
#                  'code',
#                  'utilities',
#                  'load_directories.r'),
#        local = knitr::knit_global())


base_path = repo_path

code_path = file.path(base_path,
                      'code')
dev_path = file.path(code_path,
                     'dev')
prod_path = file.path(code_path,
                      'prod')
objects_path = file.path(prod_path,
                         'objects')
util_path = file.path(code_path,
                      'utilities')

source(file.path(util_path,'general_utilities.r'))
