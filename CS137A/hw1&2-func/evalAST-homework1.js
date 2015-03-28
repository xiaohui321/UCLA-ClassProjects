/*
 * UCLA CS137A Homework 1
 * Xiaohui, Zhou
 * 104-014-248
 */

F.evalAST = function(ast) {
	return ev(ast,new Env(undefined, undefined, undefined));
};

function ev(ast,env) {
  if (isPrimValue(ast) || isClosure(ast)){
    return ast;
  } else {
    var tag = ast[0];
    var args = ast.slice(1);
    if(op.hasOwnProperty(tag)){	
      args.push(env);
    	return op[tag].apply(undefined, args);
    }else{
    	throw new Error("Unsupported in Homework 1");
    }
  }
}

var op = {
  "+": function(x, y, env) {
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,"number");
    typeCheck(v_y,"number");
    return v_x + v_y;
  },
  "-": function(x, y, env) {
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,"number");
    typeCheck(v_y,"number");
    return v_x - v_y;
  },
  "*": function(x, y, env) {
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,"number");
    typeCheck(v_y,"number");
    return v_x * v_y;
  },
  "/": function(x, y, env) {
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,"number");
    typeCheck(v_y,"number");
    notZeroCheck(v_y);
    return v_x / v_y;
  },
  "%": function(x, y, env){
  	var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,"number");
    typeCheck(v_y,"number");
    notZeroCheck(v_y);
    return v_x % v_y;
  },
  "=": function(x, y, env){
  	return ev(x,env) === ev(y,env);
  },
  "!=": function(x, y, env){
  	return ev(x,env) !== ev(y,env);
  },
  ">": function(x, y, env){
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,"number");
    typeCheck(v_y,"number")
  	return v_x > v_y;
  },
  "<": function(x, y, env){
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,"number");
    typeCheck(v_y,"number")
    return v_x < v_y;
  },
  'or': function(x, y, env){
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,"boolean");
    typeCheck(v_y,"boolean");
    return v_x || v_y;
  },
  'and': function(x, y, env){
  	var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,"boolean");
    typeCheck(v_y,"boolean");
    return v_x && v_y;
  },
  'id': function(x, env){
    return env.lookup(x);
  },
  'fun': function(xlist, e, env){ /* fun x1 x2 ... -> e */
    return ['closure',xlist,e,env];
  },
  'call': function(){/* (ef, e1, e2, ..., env) ef e1 e2 e3 ... */
    var args = Array.prototype.slice.call(arguments);
    args = args.slice(1,args.length - 1);
    var env = arguments[arguments.length - 1];
    var fun = ev(arguments[0], env);
    
    argumentLengthCheck(args, fun[1]);

    var subEnv = fun[3];
    for(var i = 0; i < args.length; i++){
      subEnv = new Env(fun[1][i],ev(args[i],env),subEnv);
    }

    return ev(fun[2],subEnv);
  },
  'let': function(x, e1, e2, env){/* let x = e1 in e2 */
    var v_e1 = ev(e1, env);
    var subEnv = new Env(x,v_e1,env);
    return ev(e2,subEnv);
  },
  'if': function(e1, e2, e3, env){/* if e1 then e2 else e3 */
    var v_cond = ev(e1,env);
    typeCheck(v_cond,"boolean");
    if(v_cond){
      return ev(e2,env);
    }else{
      return ev(e3,env);
    }
  }
};

/******************************************************************************
 * Helper functions
 */
function Env(name, value, parent) {
  this.name = name;
  this.value = value;
  this.parent = parent;
}

function isPrimValue(variable){
  if (typeof variable === "number" || typeof variable === "boolean" 
      || variable === null || typeof variable === "undefined"){
    return true;
  }
}
function isClosure(variable){
  if (typeof variable === 'object' && variable[0] === 'closure'){
    return true;
  }
}

function typeCheck(variable, type){
	if(typeof variable !== type){
		throw new Error("varible " + variable + " is not of type " + type);
	}
}

function notZeroCheck(varible){
  if(typeof variable === 0 ){
    throw new Error("Can not be zero");
  }
}

function argumentLengthCheck(arg1, arg2){
  if(arg1.length != arg2.length){
    throw new Error("Wrong number of arguments");
  }
}

Env.prototype.lookup = function(name) {
  if(name === this.name){
    return this.value;
  }else if(this.parent){
    return this.parent.lookup(name);
  }else{
    throw new Error('unbounded identifier: ' + name);
  }
}
