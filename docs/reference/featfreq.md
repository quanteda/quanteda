# Compute the frequencies of features

For a [dfm](https://quanteda.io/reference/dfm.md) object, returns a
frequency for each feature, computed across all documents in the dfm.
This is equivalent to `colSums(x)`.

## Usage

``` r
featfreq(x)
```

## Arguments

- x:

  a [dfm](https://quanteda.io/reference/dfm.md)

## Value

a (named) numeric vector of feature frequencies

## See also

[`dfm_tfidf()`](https://quanteda.io/reference/dfm_tfidf.md),
[`dfm_weight()`](https://quanteda.io/reference/dfm_weight.md)

## Examples

``` r
dfmat <- dfm(tokens(data_char_sampletext))
featfreq(dfmat)
#>       instead            we          have             a          fine 
#>             1             5             1             4             1 
#>   gael-labour         party    government             ,        coming 
#>             1             1             4            16             1 
#>          into         power     promising          real        change 
#>             3             1             1             1             1 
#>           but     slavishly     following           the      previous 
#>             4             2             1            46             1 
#>  government’s        policy             .          that           has 
#>             1             4            14             8             4 
#>          been      dictated            by           imf            eu 
#>             2             1             4             1             1 
#>           and           ecb           not            to          bail 
#>            20             1             4            11             1 
#>           out         irish        people       salvage        german 
#>             3             5             7             1             1 
#>        french       british         banks         those            of 
#>             1             1             3             2            37 
#>         other     countries          from         their    disastrous 
#>             3             1             3             3             1 
#>      frenzied       embrace       bankers   speculators            in 
#>             1             1             1             2            15 
#>      property        market        bubble            it            is 
#>             1             2             1             4             9 
#>      criminal            an         would          ever         agree 
#>             1             1             2             1             1 
#>          make        vassal         state      republic       ireland 
#>             1             1             1             1             1 
#>           its       squeeze       tribute           our          save 
#>             1             1             1             3             1 
#>    capitalist        europe         doing            so       destroy 
#>             1             1             1             1             1 
#>         lives      hundreds     thousands           now       plunged 
#>             1             2             6             2             1 
#>  unemployment     financial      hardship        social   dislocation 
#>             3             2             1             1             1 
#>          this        budget          past          four         years 
#>             6             1             1             1             2 
#>     austerity         means             €            25       billion 
#>             3             2             3             1             1 
#>        reefed       economy       pursuit      cringing    acceptance 
#>             1             5             1             1             1 
#>       diktats       markets           can           see          only 
#>             1             1             1             1             1 
#>       immoral        unjust       extreme    decimating      domestic 
#>             2             1             1             1             3 
#>             ?            as           are         tired      pointing 
#>             1             1             6             1             1 
#>            if        savage       ability      majority      purchase 
#>             1             1             1             1             2 
#>         goods       utilise      services          then          tens 
#>             2             1             2             1             3 
#>       workers     depending            on        demand           for 
#>             2             1             5             2             4 
#>          jobs          will            be        thrown     scrapheap 
#>             3             4             4             1             1 
#>    tragically          what     happening           all           key 
#>             1             1             1             1             1 
#>    indicators          show        abject       failure       private 
#>             1             1             1             1             1 
#>    investment     collapsed           vat      receipts          more 
#>             3             1             2             1             1 
#>          than           400       million        behind         risen 
#>             1             1             2             1             1 
#>         since       entered        office          much          made 
#>             1             1             1             1             1 
#>        growth       exports         every           job        export 
#>             1             2             1             1             1 
#>        sector         vital        defend          type       capital 
#>             1             1             1             1             1 
#>     intensive        nature          goes         where          need 
#>             1             1             1             1             1 
#>       created          next        period          hand        taking 
#>             1             1             1             1             2 
#>           670       pockets      ordinary       through     increases 
#>             1             1             1             1             1 
#>         money          cuts         child       benefit     elsewhere 
#>             1             1             1             1             1 
#>       further           add      downward        spiral      interest 
#>             1             1             1             1             1 
#>        relief  householders           who       trapped     nightmare 
#>             1             1             3             2             2 
#>      negative        equity  extortionate       monthly      mortgage 
#>             1             1             1             1             1 
#>      payments      welcomed        degree      dismally    inadequate 
#>             1             1             1             1             1 
#>    generation       victims     extortion   perpetrated          them 
#>             1             1             1             1             1 
#>       housing    legislated        fianna          fáil           pds 
#>             1             1             1             1             1 
#>          huge    proportion       incomes     continues            go 
#>             1             1             1             1             1 
#>     massively    dislocates     otherwise         funds         going 
#>             1             1             1             1             1 
#>   stimulating    sustaining          dole unfortunately 
#>             1             1             1             1 
```
