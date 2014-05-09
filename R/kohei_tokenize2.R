tokenizeText <- function(text, clean = TRUE, min = 3){
  if(clean){
    text <- cleanText(text, min = min)
  }  
  tokens <- unlist(strsplit(text, ' ', fixed = TRUE))
  #attr(tokens, 'class') <- 'token'
  return(tokens)
}

#' This text cleaning function removes 's (apostrophe s) and very short words
#' @export
cleanText <- function(s, removeDigits = TRUE, lower=FALSE, min = 3) {
  
  s <- paste0(' ', s, ' ') #add space top and end of texts for next step
  s <- gsub('.', '', s, fixed = TRUE) #for U.S. or N.B.A.
  s <- gsub("'s ", ' ', s, fixed = TRUE) #for country's
  s <- gsub("s' ", 's ', s, fixed = TRUE) #for countries'
  s <- gsub("'", '', s, fixed = TRUE) #for couldn't or don't
  if (removeDigits) {
    s <- gsub("[[:digit:][:punct:]]", " ", s, perl = FALSE)
  }else{
    s <- gsub('[[:punct:]]', " ", s, perl=TRUE)
  }
  if(lower){
    s <- tolower(s)
  }
  s <- gsub('\\s', "  ", s) #double the spaces for next step
  if(min > 1){
    s <- gsub(paste0('\\s.{1,', (min - 1) , '}\\s'), ' ', s) #remove short words
  }
  s <- gsub('\\s\\s+', " ", s)
  s <- gsub("^\\s+|\\s+$", "", s)
  return(s)
}

#' Biger list of stopwrods for news texts
#' @export
removeStopwords <- function(texts){
  
  #http://www.ai.mit.edu/projects/jmlr/papers/volume5/lewis04a/lyrl2004_rcv1v2_README.htm
  stopwords <- "a,a's,able,about,above,according,accordingly,across,actually,after,afterwards,again,against,ain't,all,allow,allows,almost,alone,along,already,also,although,always,am,among,amongst,an,and,another,any,anybody,anyhow,anyone,anything,anyway,anyways,anywhere,apart,appear,appreciate,appropriate,are,aren't,around,as,aside,ask,asking,associated,at,available,away,awfully,b,be,became,because,become,becomes,becoming,been,before,beforehand,behind,being,believe,below,beside,besides,best,better,between,beyond,both,brief,but,by,c,c'mon,c's,came,can,can't,cannot,cant,cause,causes,certain,certainly,changes,clearly,co,com,come,comes,concerning,consequently,consider,considering,contain,containing,contains,corresponding,could,couldn't,course,currently,d,definitely,described,despite,did,didn't,different,do,does,doesn't,doing,don't,done,down,downwards,during,e,each,edu,eg,eight,either,else,elsewhere,enough,entirely,especially,et,etc,even,ever,every,everybody,everyone,everything,everywhere,ex,exactly,example,except,f,far,few,fifth,first,five,followed,following,follows,for,former,formerly,forth,four,from,further,furthermore,g,get,gets,getting,given,gives,go,goes,going,gone,got,gotten,greetings,h,had,hadn't,happens,hardly,has,hasn't,have,haven't,having,he,he's,hello,help,hence,her,here,here's,hereafter,hereby,herein,hereupon,hers,herself,hi,him,himself,his,hither,hopefully,how,howbeit,however,i,i'd,i'll,i'm,i've,ie,if,ignored,immediate,in,inasmuch,inc,indeed,indicate,indicated,indicates,inner,insofar,instead,into,inward,is,isn't,it,it'd,it'll,it's,its,itself,j,just,k,keep,keeps,kept,know,knows,known,l,last,lately,later,latter,latterly,least,less,lest,let,let's,like,liked,likely,little,look,looking,looks,ltd,m,mainly,many,may,maybe,me,mean,meanwhile,merely,might,more,moreover,most,mostly,much,must,my,myself,n,name,namely,nd,near,nearly,necessary,need,needs,neither,never,nevertheless,new,next,nine,no,nobody,non,none,noone,nor,normally,not,nothing,novel,now,nowhere,o,obviously,of,off,often,oh,ok,okay,old,on,once,one,ones,only,onto,or,other,others,otherwise,ought,our,ours,ourselves,out,outside,over,overall,own,p,particular,particularly,per,perhaps,placed,please,plus,possible,presumably,probably,provides,q,que,quite,qv,r,rather,rd,re,really,reasonably,regarding,regardless,regards,relatively,respectively,right,s,said,same,saw,say,saying,says,second,secondly,see,seeing,seem,seemed,seeming,seems,seen,self,selves,sensible,sent,serious,seriously,seven,several,shall,she,should,shouldn't,since,six,so,some,somebody,somehow,someone,something,sometime,sometimes,somewhat,somewhere,soon,sorry,specified,specify,specifying,still,sub,such,sup,sure,t,t's,take,taken,tell,tends,th,than,thank,thanks,thanx,that,that's,thats,the,their,theirs,them,themselves,then,thence,there,there's,thereafter,thereby,therefore,therein,theres,thereupon,these,they,they'd,they'll,they're,they've,think,third,this,thorough,thoroughly,those,though,three,through,throughout,thru,thus,to,together,too,took,toward,towards,tried,tries,truly,try,trying,twice,two,u,un,under,unfortunately,unless,unlikely,until,unto,up,upon,us,use,used,useful,uses,using,usually,uucp,v,value,various,very,via,viz,vs,w,want,wants,was,wasn't,way,we,we'd,we'll,we're,we've,welcome,well,went,were,weren't,what,what's,whatever,when,whence,whenever,where,where's,whereafter,whereas,whereby,wherein,whereupon,wherever,whether,which,while,whither,who,who's,whoever,whole,whom,whose,why,will,willing,wish,with,within,without,won't,wonder,would,would,wouldn't,x,y,yes,yet,you,you'd,you'll,you're,you've,your,yours,yourself,yourselves,z,zero"
  index <- unique(tokenizeText(stopwords, clean = TRUE, min = 1))
  #print(index)
  texts2 <- unlist(lapply(texts, function(x) findStopwords(x, index)))
  return(texts2)
}

findStopwords <- function(text, index){
  
  tokens <- tokenizeText(text, clean = FALSE)
  tokens2 <- tokens[!tokens %in% index]
  text2 <- paste(tokens2, collapse = ' ')
  return(text2)
}
