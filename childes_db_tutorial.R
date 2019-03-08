library('childesr')
library('ggplot2')

# 1. Follow up questions for Spanish 

# 1a. Q: Which Spanish corpora have audio data / utterance timestamps?
spa_utts = childesr::get_utterances(collection = "Spanish")
spa_utts_with_audio = subset(spa_utts, !is.na(media_end))
spa_corpora_with_audio = unique(spa_utts_with_audio$corpus_name)
spa_corpora_with_audio
# A: Remedi, Nieva, Ornat

#1b. What is the coverage of child ages for this dataset?
spa_utts_with_audio$is_child = spa_utts_with_audio$speaker_code == "CHI"
ggplot(spa_utts_with_audio) + geom_density(aes(x=target_child_age, colour=is_child)) + theme_classic()
# A: 20 - 28 months

#1c. How many tokens / types exhibit the V.3.s / Adj.f ambiguity?
spa_tokens = childesr::get_tokens(collection = "Spanish", corpus= spa_corpora_with_audio)
# fails, with good reason

spa_tokens = childesr::get_tokens(collection = "Spanish", corpus= spa_corpora_with_audio, token="*")
names(spa_tokens)

# Due diligence: what are the pos we want to use for adjectives and verbs
pos_counts = table(spa_tokens$part_of_speech)
pos_counts = pos_counts[order(pos_counts, decreasing=T)]
pos_counts

# Due diligence: what proportion are tagged
sum(spa_tokens$part_of_speech =='') / nrow(spa_tokens)
#50% are tagged 

# get the adjectives
adj_tokens = subset(spa_tokens, part_of_speech == 'adj' & speaker_code != 'CHI')
a_adj_tokens = adj_tokens[grep('.*a$', adj_tokens $gloss),]

# aggregate the adjective counts
a_adj_counts = aggregate(stem ~ gloss, a_adj_tokens, length)
names(a_adj_counts) = c('gloss', 'adj_count')
a_adj_counts = a_adj_counts[order(a_adj_counts$adj_count, decreasing=T),]

# get the verbs
v_tokens = subset(spa_tokens, part_of_speech == 'v' & speaker_code != 'CHI')
a_v_tokens = v_tokens[grep('.*a$', v_tokens$gloss),]

# aggregate the verb counts
a_v_counts = aggregate(stem ~ gloss, a_v_tokens, length)
names(a_v_counts) = c('gloss', 'v_count')
a_v_counts = a_v_counts[order(a_v_counts$v_count, decreasing=T),]

# merge adjectives and erb_counts
a_counts = merge(a_adj_counts, a_v_counts, all=T)
a_counts[is.na(a_counts)] = 0
nonzero_a = subset(a_counts, adj_count > 0 & v_count > 0)
nonzero_a

# 2 types, each with a handful of tokens

# 1d. V.1.s vs. Adj.m (not useful for N (V|Adj) ambiguity)

# funtionalize the analysis above, pulling out 'grep_term'
getAmbiguousTokens = function(tokens, final_vowel) {
	grep_term = paste('.*', final_vowel,'$', sep='')

	adj_tokens = subset(spa_tokens, part_of_speech == 'adj' & speaker_code != 'CHI')		
	a_adj_tokens = adj_tokens[grep(grep_term,adj_tokens$gloss),]
	
	# aggregate the adjective counts
	a_adj_counts = aggregate(stem ~ gloss, a_adj_tokens, length)
	names(a_adj_counts) = c('gloss', 'adj_count')
	a_adj_counts = a_adj_counts[order(a_adj_counts$adj_count, decreasing=T),]
	
	# get the verbs
	v_tokens = subset(spa_tokens, part_of_speech == 'v' & speaker_code != 'CHI')
	a_v_tokens = v_tokens[grep(grep_term, v_tokens$gloss),]
	
	# aggregate the verb counts
	a_v_counts = aggregate(stem ~ gloss, a_v_tokens, length)
	names(a_v_counts) = c('gloss', 'v_count')
	a_v_counts = a_v_counts[order(a_v_counts$v_count, decreasing=T),]
	
	# merge adjectives and erb_counts
	a_counts = merge(a_adj_counts, a_v_counts, all=T)
	a_counts[is.na(a_counts)] = 0
	return(subset(a_counts, adj_count > 0 & v_count > 0)	)
}

# reproduce the previous analysis
getAmbiguousTokens(spa_tokens, 'a')

# extend to -o forms
getAmbiguousTokens(spa_tokens, 'o')

# 1e follow up with the specific ones that we found -- look at the utterances

a_indices = subset(spa_tokens, gloss %in% nonzero_a$gloss & part_of_speech %in% c('v','adj'))
nrow(a_indices)

sapply(a_indices$utterance_id, function(x){
	subset(spa_utts, id == x)$gloss	
})

# 1f. Extract the audio for comparing prosody of N + Adj.f vs. N + V.3s
# BUT not seeing any (?) in the naturalistic data in the form N + A
# A quick sketch:

extractAudio = function(utterance, wav_dir){
	infile = utterance$filename
	outfile = paste(utterance$id, "_extracted.wav", sep='')
	start = utterance$media_start
	duration = utterance$media_end - utterance$media_start
	command = paste("sox ", infile, ' ', outfile, ' trim ', start, ' ', duration, sep='')
	print(command)
	system(command)
}

lapply(utterances, function(x){extractAudio(x,wav_dir)})

# 2. Cross-linguistic preference for /b/-initial words

#https://pdfs.semanticscholar.org/090e/749647a90e30a6631745256623f286439c0a.pdf

getBwords = function(collectionName){
	tokens = childesr::get_tokens(collection = collectionName, token="*", part_of_speech='n')
	
	tokens$b_initial = F 
	tokens$b_initial[grep('^b.*', tokens$gloss)] = T

	tokens$ischi = tokens$speaker_code == 'CHI'
	tokens$target_child_age_in_months = 	floor(tokens$target_child_age) 
	
		
	tokenCounts = aggregate(b_initial ~ ischi + target_child_age_in_months, tokens, mean)
	
	return(tokenCounts) 	
}

eng_b_tokens = getBwords('Eng-NA')
eng_b_tokens$collection = 'Eng-NA'

spa_b_tokens = getBwords('Spanish')
spa_b_tokens$collection = 'Spanish'


french_b_tokens = getBwords('French')
french_b_tokens$collection = 'French'

japanese_b_tokens = getBwords('Japanese')
japanese_b_tokens$collection = 'Japanese'

b_tokens = rbind(eng_b_tokens, spa_b_tokens, french_b_tokens, japanese_b_tokens)
ggplot(subset(b_tokens, target_child_age_in_months < 50)) + geom_line(aes(x= target_child_age_in_months, y=b_initial, colour=ischi)) + facet_wrap(~collection) + theme_classic() 