use testTable, clear

gen n111 = (w1=="are" & w2=="trapped" & w3=="in")
gen n112 = (w1=="are" & w2=="trapped" & w3!="in")
gen n121 = (w1=="are" & w2!="trapped" & w3=="in")
gen n122 = (w1=="are" & w2!="trapped" & w3!="in")
gen n211 = (w1!="are" & w2=="trapped" & w3=="in")
gen n212 = (w1!="are" & w2=="trapped" & w3!="in")
gen n221 = (w1!="are" & w2!="trapped" & w3=="in")
gen n222 = (w1!="are" & w2!="trapped" & w3!="in")
collapse (sum) n*
list
