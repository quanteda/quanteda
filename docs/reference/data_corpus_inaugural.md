# US presidential inaugural address texts

US presidential inaugural address texts, and metadata (for the corpus),
from 1789 to present.

## Usage

``` r
data(data_corpus_inaugural)
```

## Format

a [corpus](https://quanteda.io/reference/corpus.md) object with the
following docvars:

- `Year` a four-digit integer year

- `President` character; President's last name

- `FirstName` character; President's first name (and possibly middle
  initial)

- `Party` factor; name of the President's political party

## Source

<https://archive.org/details/Inaugural-Address-Corpus-1789-2009> and
<https://www.presidency.ucsb.edu/documents/presidential-documents-archive-guidebook/inaugural-addresses>.

## Details

`data_corpus_inaugural` is the
[quanteda-package](https://quanteda.io/reference/quanteda-package.md)
corpus object of US presidents' inaugural addresses since 1789. Document
variables contain the year of the address and the last name of the
president.

## Examples

``` r
# some operations on the inaugural corpus
summary(data_corpus_inaugural)
#> Corpus consisting of 60 documents, showing 60 documents:
#> 
#>             Text Types Tokens Sentences Year  President       FirstName
#>  1789-Washington   625   1537        24 1789 Washington          George
#>  1793-Washington    96    147         5 1793 Washington          George
#>       1797-Adams   826   2577        37 1797      Adams            John
#>   1801-Jefferson   717   1923        43 1801  Jefferson          Thomas
#>   1805-Jefferson   804   2380        45 1805  Jefferson          Thomas
#>     1809-Madison   535   1261        21 1809    Madison           James
#>     1813-Madison   541   1302        33 1813    Madison           James
#>      1817-Monroe  1040   3677       121 1817     Monroe           James
#>      1821-Monroe  1259   4886       131 1821     Monroe           James
#>       1825-Adams  1003   3147        75 1825      Adams     John Quincy
#>     1829-Jackson   517   1208        25 1829    Jackson          Andrew
#>     1833-Jackson   499   1267        30 1833    Jackson          Andrew
#>    1837-VanBuren  1315   4158        95 1837  Van Buren          Martin
#>    1841-Harrison  1896   9125       210 1841   Harrison   William Henry
#>        1845-Polk  1334   5186       153 1845       Polk      James Knox
#>      1849-Taylor   496   1178        22 1849     Taylor         Zachary
#>      1853-Pierce  1165   3636       105 1853     Pierce        Franklin
#>    1857-Buchanan   945   3083        90 1857   Buchanan           James
#>     1861-Lincoln  1075   3999       138 1861    Lincoln         Abraham
#>     1865-Lincoln   360    775        27 1865    Lincoln         Abraham
#>       1869-Grant   485   1229        41 1869      Grant      Ulysses S.
#>       1873-Grant   552   1472        44 1873      Grant      Ulysses S.
#>       1877-Hayes   831   2707        59 1877      Hayes   Rutherford B.
#>    1881-Garfield  1021   3209       112 1881   Garfield        James A.
#>   1885-Cleveland   676   1816        44 1885  Cleveland          Grover
#>    1889-Harrison  1352   4721       157 1889   Harrison        Benjamin
#>   1893-Cleveland   821   2125        58 1893  Cleveland          Grover
#>    1897-McKinley  1232   4353       130 1897   McKinley         William
#>    1901-McKinley   854   2437       100 1901   McKinley         William
#>   1905-Roosevelt   404   1079        33 1905  Roosevelt        Theodore
#>        1909-Taft  1437   5821       158 1909       Taft  William Howard
#>      1913-Wilson   658   1882        68 1913     Wilson         Woodrow
#>      1917-Wilson   549   1652        60 1917     Wilson         Woodrow
#>     1921-Harding  1169   3719       150 1921    Harding       Warren G.
#>    1925-Coolidge  1220   4440       198 1925   Coolidge          Calvin
#>      1929-Hoover  1090   3860       159 1929     Hoover         Herbert
#>   1933-Roosevelt   743   2057        86 1933  Roosevelt     Franklin D.
#>   1937-Roosevelt   725   1989        96 1937  Roosevelt     Franklin D.
#>   1941-Roosevelt   526   1519        68 1941  Roosevelt     Franklin D.
#>   1945-Roosevelt   275    633        28 1945  Roosevelt     Franklin D.
#>      1949-Truman   781   2504       116 1949     Truman        Harry S.
#>  1953-Eisenhower   900   2743       123 1953 Eisenhower       Dwight D.
#>  1957-Eisenhower   621   1907        92 1957 Eisenhower       Dwight D.
#>     1961-Kennedy   566   1541        52 1961    Kennedy         John F.
#>     1965-Johnson   568   1710        94 1965    Johnson   Lyndon Baines
#>       1969-Nixon   743   2416       106 1969      Nixon Richard Milhous
#>       1973-Nixon   544   1995        69 1973      Nixon Richard Milhous
#>      1977-Carter   527   1370        53 1977     Carter           Jimmy
#>      1981-Reagan   902   2781       130 1981     Reagan          Ronald
#>      1985-Reagan   925   2909       126 1985     Reagan          Ronald
#>        1989-Bush   795   2674       144 1989       Bush          George
#>     1993-Clinton   642   1833        82 1993    Clinton            Bill
#>     1997-Clinton   773   2436       113 1997    Clinton            Bill
#>        2001-Bush   621   1806        98 2001       Bush       George W.
#>        2005-Bush   772   2312        99 2005       Bush       George W.
#>       2009-Obama   938   2689       112 2009      Obama          Barack
#>       2013-Obama   814   2317        90 2013      Obama          Barack
#>       2017-Trump   582   1660        89 2017      Trump       Donald J.
#>       2021-Biden   812   2766       229 2021      Biden       Joseph R.
#>       2025-Trump  1000   3347       177 2025      Trump       Donald J.
#>                  Party
#>                   none
#>                   none
#>             Federalist
#>  Democratic-Republican
#>  Democratic-Republican
#>  Democratic-Republican
#>  Democratic-Republican
#>  Democratic-Republican
#>  Democratic-Republican
#>  Democratic-Republican
#>             Democratic
#>             Democratic
#>             Democratic
#>                   Whig
#>                   Whig
#>                   Whig
#>             Democratic
#>             Democratic
#>             Republican
#>             Republican
#>             Republican
#>             Republican
#>             Republican
#>             Republican
#>             Democratic
#>             Republican
#>             Democratic
#>             Republican
#>             Republican
#>             Republican
#>             Republican
#>             Democratic
#>             Democratic
#>             Republican
#>             Republican
#>             Republican
#>             Democratic
#>             Democratic
#>             Democratic
#>             Democratic
#>             Democratic
#>             Republican
#>             Republican
#>             Democratic
#>             Democratic
#>             Republican
#>             Republican
#>             Democratic
#>             Republican
#>             Republican
#>             Republican
#>             Democratic
#>             Democratic
#>             Republican
#>             Republican
#>             Democratic
#>             Democratic
#>             Republican
#>             Democratic
#>             Republican
#> 
head(docvars(data_corpus_inaugural), 10)
#>    Year  President   FirstName                 Party
#> 1  1789 Washington      George                  none
#> 2  1793 Washington      George                  none
#> 3  1797      Adams        John            Federalist
#> 4  1801  Jefferson      Thomas Democratic-Republican
#> 5  1805  Jefferson      Thomas Democratic-Republican
#> 6  1809    Madison       James Democratic-Republican
#> 7  1813    Madison       James Democratic-Republican
#> 8  1817     Monroe       James Democratic-Republican
#> 9  1821     Monroe       James Democratic-Republican
#> 10 1825      Adams John Quincy Democratic-Republican
```
