grammar regex;

regex:
	| regex regex		# concatenation
	| regex '|' regex	# union
	| group quantifier?	# group
	| set quantifier?	# set
	| '.'				# any
	| '^'				# beginning
	| '$'				# end;
group:
	'(' regex ')'
	| '(?<' NAME '>' regex ')'	# namedgroup
	| '(?:' regex ')'			# noncapturinggroup
	| '(?=' regex ')'			# positivelookahead
	| '(?!' regex ')'			# negativelookahead;
set:
	'[' setItem* ']'	# positiveset
	| '[^' setItem* ']'	# negativeset;
setItem: CHAR '-' CHAR | CHAR;
quantifier:
	'?'							# oneorzero
	| '+'						# oneormore
	| '*'						# zeroormore
	| '{' NUMBER '}'			# exactlydigit
	| '{' NUMBER ',' NUMBER '}'	# fromdigittodigit
	| '{' NUMBER ',' '}'		# digitormore
	| quantifier '?'			# lazy;

NAME: LETTER [LETTER | DIGIT]*;
CHAR: LETTER | DIGIT | '\\' METACHAR;
LETTER: [a-zA-Z];
METACHAR: [\.\,\\,\(\),\[,\],\{,\}];
NUMBER: [DIGIT]+;
DIGIT: [0-9];
WS: [\s]+ -> skip;