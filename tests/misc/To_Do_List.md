Notes about **quanteda**:
    
*  Need a complete regex audit, to make sure the expressions take valid Unicode classes as in **stringi**.

*  Want to put docvars into other objects, that follow subsetting and selection rules for documents.

*  Some strange things in `brexit/R/04a_analysis_topicmodels_by_side.R`.  Rerun, debug.

*  `dfm_trim` doesn't do anything without arguments, but should still enforce the selection according to the defaults.  So `dfm_trim(mydfm)` should remove features according to `min_count = 1`, even if this argument is not supplied.  Right now it does nothing.



