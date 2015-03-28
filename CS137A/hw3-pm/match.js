/*
 * UCLA CS137A Homework 3
 * Xiaohui, Zhou
 * 104-014-248
 */

function match(value /* , pat1, fun1, pat2, fun2, ... */) {
	for(var i = 1; i < arguments.length; i+=2){
		var result = patternMatch(value,arguments[i]);
		if(result[0])	return arguments[i+1].apply(undefined,result[1])
	}
throw new Error('match failed');
}

function patternMatch(value,pattern){
	if( isPrimValue(pattern) ) {
		return (value === pattern) ? [true,[]] : [false];
	} else if ( pattern === _ ) {
		return [true,[value]];
	} else if ( isWhen(pattern)){
		if(typeof pattern[1] != 'function') 
			throw new Error("the argument for function 'when' should be a function");
		return pattern[1].call(this,value) ? [true,[value]] : [false];
	} else if ( pattern instanceof Array) {
		return arrayPatternMatch(value,pattern);	
	} else {
		return [false];
	}
}

function arrayPatternMatch(value,pattern){
	var argumentList = [];
	for(var i = 0; i < pattern.length; i++){
		if (isMany(pattern[i])){
			var subPattern = pattern[i][1];
			var subList = [];
			var subResult;
			while(value.length > 0){
				subResult = patternMatch(value[0],subPattern);
				if(!subResult[0]) break;				
				value.shift();
				subList = subList.concat(subResult[1]);
			}
			argumentList.push(subList);
		} else {
			var result = patternMatch(value.shift(),pattern[i]);
			if(!result[0]) return [false];
			argumentList = argumentList.concat(result[1]);
		}
	}
	return value.length === 0 ? [true, argumentList] : [false];
}

function isPrimValue(variable){
	if ( typeof variable === 'number' 
		|| typeof variable === 'boolean' 
		|| typeof variable === 'string' 
		|| variable === undefined
		|| variable === null 
		)
		return true;
}


//Thanks for the answer at question No.96 on Piazza
try{ global._ = Symbol("__underscore__"); } catch(e) { window._ = Symbol("__underscore__"); }

function isMany(p){ return ((p instanceof Array) && (p.length === 2) && (p[0] === '__many__')); }
function isWhen(p){ return ((p instanceof Array) && (p.length === 2) && (p[0] === '__when__')); }

function many(argu){ return ['__many__',argu]; }
function when(argu){ return ['__when__',argu]; }