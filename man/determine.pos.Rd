\name{determine.pos}
\alias{determine.pos}
\title{Returns a table of the occurrences of differen parts of speech in a sentence
This function takes a sentence and tags each word with it's part of speech using
openNLP's POS tagger, then returns a table of the parts of speech}
\usage{
determine.pos(sentence)
}
\arguments{
  \item{text}{Text to be tagged}
}
\description{
THE Penn Treebank Part of Speech tags: CC Coordinating
conjunction CD Cardinal number DT Determiner EX Existential
there FW Foreign word IN Preposition or subordinating
conjunction JJ Adjective JJR Adjective, comparative JJS
Adjective, superlative LS List item marker MD Modal NN
Noun, singular or mass NNS Noun, plural NNP Proper noun,
singular NNPS Proper noun, plural PDT Predeterminer POS
Possessive ending PRP Personal pronoun PRP$ Possessive
pronoun RB Adverb RBR Adverb, comparative RBS Adverb,
superlative RP Particle SYM Symbol TO to UH Interjection VB
Verb, base form VBD Verb, past tense VBG Verb, gerund or
present participle VBN Verb, past participle VBP Verb,
non-3rd person singular present VBZ Verb, 3rd person
singular present WDT Wh-determiner WP Wh-pronoun WP$
Possessive wh-pronoun WRB Wh­adverb
}
\examples{
determine.pos(sentence)
}

