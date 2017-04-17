#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP quanteda_qatd_CanberraPara_cpp(SEXP, SEXP);
extern SEXP quanteda_qatd_CanberraPara_cpp2(SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_chars_remove(SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_fcm(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_kwic(SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_sequences(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_tokens_compound(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_tokens_detect(SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_tokens_lookup(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_tokens_match(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_tokens_ngrams(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_tokens_recompile(SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_tokens_replace(SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_cpp_tokens_select(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_ManhattanPara_cpp(SEXP, SEXP);
extern SEXP quanteda_qatd_ManhattanPara_cpp2(SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_MaximumPara_cpp(SEXP, SEXP);
extern SEXP quanteda_qatd_MaximumPara_cpp2(SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_MinkowskiPara_cpp(SEXP, SEXP, SEXP);
extern SEXP quanteda_qatd_MinkowskiPara_cpp2(SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_wordfishcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_wordfishcpp_dense(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP quanteda_wordfishcpp_mt(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"quanteda_qatd_CanberraPara_cpp",     (DL_FUNC) &quanteda_qatd_CanberraPara_cpp,     2},
    {"quanteda_qatd_CanberraPara_cpp2",    (DL_FUNC) &quanteda_qatd_CanberraPara_cpp2,    3},
    {"quanteda_qatd_cpp_chars_remove",     (DL_FUNC) &quanteda_qatd_cpp_chars_remove,     2},
    {"quanteda_qatd_cpp_fcm",              (DL_FUNC) &quanteda_qatd_cpp_fcm,              8},
    {"quanteda_qatd_cpp_kwic",             (DL_FUNC) &quanteda_qatd_cpp_kwic,             4},
    {"quanteda_qatd_cpp_sequences",        (DL_FUNC) &quanteda_qatd_cpp_sequences,        6},
    {"quanteda_qatd_cpp_tokens_compound",  (DL_FUNC) &quanteda_qatd_cpp_tokens_compound,  5},
    {"quanteda_qatd_cpp_tokens_detect",    (DL_FUNC) &quanteda_qatd_cpp_tokens_detect,    2},
    {"quanteda_qatd_cpp_tokens_lookup",    (DL_FUNC) &quanteda_qatd_cpp_tokens_lookup,    5},
    {"quanteda_qatd_cpp_tokens_match",     (DL_FUNC) &quanteda_qatd_cpp_tokens_match,     5},
    {"quanteda_qatd_cpp_tokens_ngrams",    (DL_FUNC) &quanteda_qatd_cpp_tokens_ngrams,    5},
    {"quanteda_qatd_cpp_tokens_recompile", (DL_FUNC) &quanteda_qatd_cpp_tokens_recompile, 2},
    {"quanteda_qatd_cpp_tokens_replace",   (DL_FUNC) &quanteda_qatd_cpp_tokens_replace,   4},
    {"quanteda_qatd_cpp_tokens_select",    (DL_FUNC) &quanteda_qatd_cpp_tokens_select,    5},
    {"quanteda_qatd_ManhattanPara_cpp",    (DL_FUNC) &quanteda_qatd_ManhattanPara_cpp,    2},
    {"quanteda_qatd_ManhattanPara_cpp2",   (DL_FUNC) &quanteda_qatd_ManhattanPara_cpp2,   3},
    {"quanteda_qatd_MaximumPara_cpp",      (DL_FUNC) &quanteda_qatd_MaximumPara_cpp,      2},
    {"quanteda_qatd_MaximumPara_cpp2",     (DL_FUNC) &quanteda_qatd_MaximumPara_cpp2,     3},
    {"quanteda_qatd_MinkowskiPara_cpp",    (DL_FUNC) &quanteda_qatd_MinkowskiPara_cpp,    3},
    {"quanteda_qatd_MinkowskiPara_cpp2",   (DL_FUNC) &quanteda_qatd_MinkowskiPara_cpp2,   4},
    {"quanteda_wordfishcpp",               (DL_FUNC) &quanteda_wordfishcpp,               9},
    {"quanteda_wordfishcpp_dense",         (DL_FUNC) &quanteda_wordfishcpp_dense,         7},
    {"quanteda_wordfishcpp_mt",            (DL_FUNC) &quanteda_wordfishcpp_mt,            9},
    {NULL, NULL, 0}
};

void R_init_quanteda(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}