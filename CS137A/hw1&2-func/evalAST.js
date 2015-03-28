/*
 * UCLA CS137A Homework 2
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
    	throw new Error('Unsupported in Homework 1/2');
    }
  }
}
var op = {
  '+': function(x, y, env) {
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,'number');
    typeCheck(v_y,'number');
    return v_x + v_y;
  },
  '-': function(x, y, env) {
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,'number');
    typeCheck(v_y,'number');
    return v_x - v_y;
  },
  '*': function(x, y, env) {
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,'number');
    typeCheck(v_y,'number');
    return v_x * v_y;
  },
  '/': function(x, y, env) {
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,'number');
    typeCheck(v_y,'number');
    notZeroCheck(v_y);
    return v_x / v_y;
  },
  '%': function(x, y, env){
  	var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,'number');
    typeCheck(v_y,'number');
    notZeroCheck(v_y);
    return v_x % v_y;
  },
  '=': function(x, y, env){
  	return ev(x,env) === ev(y,env);
  },
  '!=': function(x, y, env){
  	return ev(x,env) !== ev(y,env);
  },
  '>': function(x, y, env){
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,'number');
    typeCheck(v_y,'number')
    return v_x > v_y;
  },
  '<': function(x, y, env){
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,'number');
    typeCheck(v_y,'number')
    return v_x < v_y;
  },
  'or': function(x, y, env){
    var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,'boolean');
    typeCheck(v_y,'boolean');
    return v_x || v_y;
  },
  'and': function(x, y, env){
  	var v_x = ev(x,env);
    var v_y = ev(y,env);
    typeCheck(v_x,'boolean');
    typeCheck(v_y,'boolean');
    return v_x && v_y;
  },
  'id': function(x, env){
    return env.lookup(x);
  },
  'fun': function(xlist, e, env){ /* fun x1 x2 ... -> e */
    return ['closure',xlist,e,env];
  },
  'if': function(e1, e2, e3, env){/* if e1 then e2 else e3 */
    var v_cond = ev(e1,env);
    typeCheck(v_cond,'boolean');
    if(v_cond){
      return ev(e2,env);
    }else{
      return ev(e3,env);
    }
  },
  'let': function(x, e1, e2, env){/* let x = e1 in e2 */
    var v_e1 = ev(e1, env);
    env = new Env(x,v_e1,env);
    v_e1[3] = new Env(x,v_e1,v_e1[3]);
    return ev(e2,env);
  },
  'call': function(){/* (ef, e1, e2, ..., env) ef e1 e2 e3 ... */
    var args = Array.prototype.slice.call(arguments);
    args = args.slice(1,args.length - 1);
    var env = arguments[arguments.length - 1];
    var fun = ev(arguments[0], env);
    
    if(args.length === fun[1].length){ //Normal calling 
      var subEnv = mergeEnv(env,fun[3]);
      for(var i = 0; i < args.length; i++){
        subEnv = new Env(fun[1][i],ev(args[i],env),subEnv);
      }
      return ev(fun[2],subEnv);

    } else if (args.length  < fun[1].length){//currying 
      /*['closure',xlist,e,env]; */
      var subEnv = fun[3];
      var subArg = fun[1];
      for(var i = 0; i < args.length; i++){
        subEnv = new Env(subArg.shift(),args[i],subEnv);
      } 
      return ['closure',subArg,fun[2],subEnv];
    } else { 
      throw new Error('Extra number of arguments');
    }
  },
  'cons': function(e1,e2,env){
    return ['cons',ev(e1,env),ev(e2,env)];
  },
  'match': function(){/* e,p1,e1,p2,p2,...,env */ 
    /* match e with p1 -> e1  | p2 -> e2 … */
    var args = Array.prototype.slice.call(arguments);
    args = args.slice(1,args.length - 1);
    var env = arguments[arguments.length - 1];
    var e = ev(arguments[0], env);

    for(var i = 0; i < args.length; i+= 2){
      var result = match(e,args[i],env);
      if(result[0]){
        return ev(args[i+1],result[1]);
      }
    }
    throw new Error('Match Failure');
  },
  'listComp':function(e,x,elist,env){/* e | x <- elist [,epred] */
    var epred = undefined;
    if(arguments.length === 5){
      epred = arguments[3];
      env = arguments[4];
    }
    var result = [];
    elist = ev(elist,env);

    while(elist != null){
      var newEnv = new Env(x,elist[1],env);
      if(epred === undefined || ev(epred,newEnv)){
        result.push(ev(e,newEnv));
      }
      elist = elist[2];
    }

    var r = null;
    for(var i = result.length - 1; i >= 0 ; i--){
      r = ['cons',result[i],r];
    }
    return r;
  },
  'set': function(x,e,env){
    var v_e = ev(e,env);
    setValue(x,v_e,env);
    return v_e;
  },
  'seq':function(e1,e2,env){
    ev(e1,env);
    return ev(e2,env);
  },
  'delay':function(e,env){
    return ['delay',e];
  },

  'force':function(e,env){
    if(e instanceof Array && e[0] === 'delay'){
      return ev(e[1],env);
    } else {
      var v_e = ev(e,env);
      if(isPrimValue(v_e)) return v_e;
      return ev(['force',v_e],env);
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

//env1 has higher priority
function mergeEnv(env1, env2){
  while(env1.name != undefined){
    env2 = new Env(env1.name, env1.value, env2);
    env1 = env1.parent;
  }
  return env2;
}
function isPrimValue(variable){
  if (typeof variable === 'number' || typeof variable === 'boolean' 
    || variable === null || typeof variable === 'undefined' || typeof variable == 'string')
    return true;
}

function isClosure(variable){
  if (typeof variable === 'object' && variable[0] === 'closure'){
    return true;
  }
}

function typeCheck(variable, type){
	if(typeof variable !== type){
		throw new Error('varible ' + variable + ' is not of type ' + type);
	}
}

function notZeroCheck(varible){
  if(typeof variable === 0 ){
    throw new Error('Can not be zero');
  }
}

function match(e,p,env){ 
  if(isPrimValue(e) && isPrimValue(p) && e === p){
    return [true,env];
  } else if (isPrimValue(p)){
    return [false];
  } else if(p[0] === 'id'){
    return [true,new Env(p[1],e,env)];
  } else if(p[0] === 'cons'){
    if(typeof e === 'object' && e[0] === 'cons'){
      var result = match(e[1],p[1],env);
      if(!result[0]) return [false];
      result = match(e[2],p[2],result[1]);
      if(!result[0]) return [false];
      return [true,result[1]];
    }else{
      return [false];
    }
  }
  if(p[0] === '_'){
    return [true,env];
  }
  throw new Error('unknown pattern: ' + p);
}

function setValue(x,v,env){
  if(env === undefined) throw new Error("wat?");
  if(x == env.name){
    env.value = v;
  }else{
    if(env.parent === null) throw new Error(x + " Not Found");
    setValue(x,v,env.parent);
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
