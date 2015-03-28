/******************************************************************************
 * Author: Xiaohui,Zhou
 * ID: 104-014-248
 * CS137A HW5
 */

/******************************************************************************
 * Constructors
 */

function OOClass(name,superClassName,instVarNames,functions){
    this.name = name;
    this.superClassName = superClassName || "Object";
    this.instVarNames   = instVarNames   || [];
    this.functions      = functions      || [];

}

function OOClassInstance(className){
    this.className = className;
    this.instanceVariables = {};
    while(className !== "__NULL__"){
        var theClass = OO.getClass(className);
        for(var i = 0; i < theClass.instVarNames.length; i++){
            this.instanceVariables[theClass.instVarNames[i]] =
                "__uninitialized__";
        }
        className = theClass.superClassName;
    }
    return this;
}

/******************************************************************************
 *  OO
 */

var OO = {
    classTable : []
};

/*
 * Initializes the class table. After this method is called, the class table
 * should only contain the Object class, which must support the following
 * methods:
 *   initialize(), which does nothing.
 *   === x, which returns true if the receiver and x are the same object, false
 *   otherwise.This method has the same semantics as JavaScript's === operator.
 *   !== x, which returns true if the receiver and x are not the same object,
 *   false otherwise.This method has the same semantics as JavaScript's !==
 *   operator. See declareMethod below for more on representation of methods.
 */
OO.initializeCT = function() {
    OO.classTable = [];

    OO.classTable[0] = new OOClass("Object","__NULL__");
    OO.classTable[1] = new OOClass("Number","Object",["value"]);
    OO.classTable[2] = new OOClass("Null");
    OO.classTable[3] = new OOClass("Boolean");
    OO.classTable[4] = new OOClass("True","Boolean");
    OO.classTable[5] = new OOClass("False","Boolean");
    OO.classTable[6] = new OOClass("Block","Object",["fun"]);

    OO.declareMethod("Object","initialize",function(_this){
    });
    OO.declareMethod("Object","===",function(_this,that){
        return _this === that;
    });
    OO.declareMethod("Object","!==",function(_this,that){
        return _this !== that;
    });
    OO.declareMethod("Object","isNumber",function(_this){
        return false;
    });

    OO.declareMethod("Number","isNumber",function(_this){
        return true;
    });
    OO.declareMethod("Number","===",function(_this,that){
        return _this === that;
    });
    OO.declareMethod("Number","!==",function(_this,that){
        return _this !== that;
    });
    OO.declareMethod("Number","+",function(_this,other){
        return  _this + other;
    });
    OO.declareMethod("Number","*",function(_this,other){
        return  _this * other;
    });
    OO.declareMethod("Number","-",function(_this,other){
        return  _this - other;
    });
    OO.declareMethod("Number","/",function(_this,other){
        return  _this / other;
    });
    OO.declareMethod("Number","%",function(_this,other){
        return  _this % other;
    });
    OO.declareMethod("Number","<",function(_this,other){
        return  _this < other;
    });
    OO.declareMethod("Number","<=",function(_this,other){
        return  _this <= other;
    });
    OO.declareMethod("Number",">=",function(_this,other){
        return  _this >= other;
    });
    OO.declareMethod("Number",">",function(_this,other){
        return  _this > other;
    });

    OO.declareMethod("Block","initialize",function(_this,fun){
      OO.setInstVar(_this,"fun",fun);
    });
    OO.declareMethod("Block","call",function(_this){
        var args = Array.prototype.slice.call(arguments,1);
        return _this.instanceVariables["fun"].apply(undefined,args);
    });
};

/*
 * Creates a new class with the appropriate name, superclass, and instance
 * variable names, and adds that class to the class table.
 * Throws an exception if:
 *   The class table already contains a class with the same name (duplicate
 *   class declaration).
 *   There is no entry in the class table for superClassName (undeclared class)
 *   There are duplicates in instVarNames, or one or more elements of
 *   instVarNames are also instance variable names of a (possibly transitive)
 *   superclass (duplicate instance variable declaration).
 *   E.g., OO.declareClass("Point", "Object", ["x", "y"])
 */
OO.declareClass = function(name,superClassName,instVarNames){
    var superClass = OO.getClass(superClassName);
    if(superClass === undefined)
        throw new Error("the superclass `" + superClassName
                        + "` is not found for class `" + name + "`");

    //check if a class with the same name existed
    for(var i = 0; i < OO.classTable.length; i++)
        if(OO.classTable[i].name === name)
            throw new Error("duplicated class name");

    //check duplication in instance variables
    for(var i = 0; i < instVarNames.length; i++){
        for(var j = i + 1; j < instVarNames.length; j++){
            if(instVarNames[i] === instVarNames[j])
                throw new Error("duplicates in instance variable names");
        }

        for(var j = 0; j < superClass.instVarNames.length; j++){
            if(instVarNames[i] === superClass.instVarNames[j])
                throw new Error("duplicated instance variables found in the"
                                + " superclass " + superClass.name);
        }
    }

    OO.classTable.push(new OOClass(name, superClassName,instVarNames));
};

/*
 * Adds a method named selector to the class named className, whose associated
 * method implementation is implFn.
 * The implementation function implFn should have _this as its first argument,
 * followed by the formal arguments of the method that is being declared.
 * (When a method is called, _this will be bound to the receiver.)
 * E.g.,
 *   OO.declareMethod("Point", "initialize",
 *     function(_this, x, y) {
 *       OO.setInstVar(_this, "x", x);
 *       OO.setInstVar(_this, "y", y);
 *     }
 *   );
 * and
 *    OO.declareMethod("Point", "+",
 *      function(_this, that) {
 *        return OO.instantiate(
 *          "Point",
 *          OO.send(OO.getInstVar(_this, "x"), "+", send(that, "getX")),
 *          OO.send(OO.getInstVar(_this, "y"), "+", send(that, "getY"))
 *        );
 *      }
 *    );
 * Note that declareMethod can also be used to override a method of the same
 * name in the superclass.
 */
OO.declareMethod = function(className, selector, implFn){
    var theClass = OO.getClass(className);
    if(theClass === undefined)
        throw new Error("Class with name `" + className + "` is not found");
    theClass.functions.push([selector,implFn]);
};

/*
 * Creates a new instance of the class named className, calls its initialize
 * method with arg1, arg2, … as arguments, and returns the new instance.
 * Throws an exception if there is no entry in the class table for className
 * (undeclared class).
 * E.g., OO.instantiate("Point", 1, 2) should evaluate to a new Point whose x
 * and y instance variables have been initialized to 1 and 2, respectively.
 * (Assuming that Point.initialize was declared as shown above.)
 */
OO.instantiate = function(/*className, arg1, arg2, ... */){
    var className = arguments[0];
    var args = Array.prototype.slice.call(arguments,1);
    var theClassInstance = new OOClassInstance(className);
    var theFunction = OO.getFunction("initialize",className);
    theFunction.apply(undefined,[theClassInstance].concat(args));
    return theClassInstance;
};

/*
 * Looks up the method with the name selector in the class of recv (the
 * receiver of the message), calls the method with the appropriate arguments,
 * and returns the result of that call.
 * Throws an exception if recv's class does not have such a method (message not
 * understood).
 * E.g., OO.send(OO.instantiate("Point", 1, 2), "+", OO.instantiate("Point", 3,
 * 4)) should evaluate to a new Point whose x and y instance variables are
 * equal to 4 and 6, respectively.
 */
OO.send = function(/*recv, selector, arg1, arg2, ... */){
    var recv = arguments[0];
    var selector = arguments[1];
    var args = Array.prototype.slice.call(arguments,2);

    if(typeof recv === "number"){
      var theFunction = OO.getFunction(selector,"Number");
    } else if (recv === null) {
      var theFunction = OO.getFunction(selector,"Null");
    } else if (recv === true) {
      var theFunction = OO.getFunction(selector,"True");
    } else if (recv === false) {
      var theFunction = OO.getFunction(selector,"False");
    } else{
      var theFunction = OO.getFunction(selector,recv.className);
    }
    return theFunction.apply(undefined, [recv].concat(args));
};

/*
 * Looks up the method that corresponds to selector in the class called
 * superClassName, calls it with the appropriate arguments, and returns the
 * result of that call.
 * Throws an exception if:
 * There is no entry in the class table for superClassName (undeclared class).
 * The superclass does not have such a method (message not understood).
 * E.g., OO.superSend("Object", myPoint, "initialize")
 */
OO.superSend = function(/*superClassName, recv, selector, arg1, arg2, ... */){
    var superClassName = arguments[0];
    var recv = arguments[1];
    var selector = arguments[2];
    var args = Array.prototype.slice.call(arguments, 3);
    var theFunction = OO.getFunction(selector,superClassName);
    return theFunction.apply(undefined,[recv].concat(args));
};

/*
 * Returns the value of the instance variable called instVarName in recv.
 * Throws an exception if recv does not have an instance variable with that
 * name (undeclared instance variable).
 * E.g., OO.getInstVar(myPoint, "x")
 */
OO.getInstVar = function(recv, instVarName){
    var result = recv.instanceVariables[instVarName];
    if(result === undefined)
        throw new Error("Unknown instance variable `" + instVarName + "`");
    return result;
};


/*
 * Sets the value of the instance variable called instVarName in recv to value
 * and returns that value.
 * Throws an exception if recv does not have an instance variable with that
 * name (undeclared instance variable).
 * E.g., OO.setInstVar(myPoint, "x", 5)
 */
OO.setInstVar = function(recv, instVarName, value){
    var result = recv.instanceVariables[instVarName];
    if(result === undefined)
        throw new Error("Unknown instance variable `" + instVarName + "`");
    recv.instanceVariables[instVarName] = value;
    return value;
};

/******************************************************************************
 * Helper Functions
 */

/*
 * Returns the object that represents the class with the given name.
 * Throws an exception if there is no such class (undeclared class).
 */
OO.getClass = function(name){
    for(var i = 0; i < OO.classTable.length; i++){
        if(OO.classTable[i].name == name) return OO.classTable[i];
    }

    return undefined;
};

/*
 * Returns the object that represents the class of the object x.
 * E.g., OO.classOf(OO.instantiate("Point", 1, 2)) should evaluate to the
 * object that represents the Point class.
 */
OO.classOf = function(x){
    return OO.getClass(x.className);
};

/*
 * Returns the function called functionName in the class with name ClassName
 * or its superClass
 */
OO.getFunction = function(functionName,className){
    while(className !== "__NULL__"){
        var theClass = OO.getClass(className);
        if(theClass === undefined)
            throw new Error("the class called `"+className+"` is not found");

        for(var i = theClass.functions.length - 1; i >= 0; i--){
            if(theClass.functions[i][0] == functionName){
                return theClass.functions[i][1];
            }
        }

        className = theClass.superClassName;
    }

    throw new Error("the function `" + functionName + "` is not found for " +
                    "the class `" + className + "`");
}


/*
 * Returns the superclass name for the class called className
 */
OO.getSuperClassName = function(className){
    for(var i = 0; i < OO.classTable.length; i++){
        if(OO.classTable[i].name == className)
            return OO.classTable[i].superClassName;
    }
    throw new Error("Class with name " + className + " is not found");
}

/******************************************************************************
 * Translator
 */
//For storing current class when declearing a method
var currentClass = "Object";

function __RETURN__(id,ret){
  this.__ID__ = id;
  this.__RET__ = ret;
}

O.transAST = function(ast) {
    var tag = ast[0];
    var args = ast.slice(1);
    var result = "";
    if(tag === 'program'){
        result = 'OO.initializeCT();\n'
               + 'var __RESULT__ = null;\n' 
               + 'var __ID__ = Symbol();\n';
        for(var i = 0; i < args.length; i++){
            result += O.transAST(args[i]) + ';\n';
        }
        result += '__RESULT__';

    } else if(tag === 'classDecl'){
        var className = '"' + args[0] + '"';
        var superClassName = '"' + args[1] + '"';
        var instVarNames = args[2].length > 0 ? 
          '["' + args[2].join('","') + '"]' : "[]";
        result = 'OO.declareClass('+ className + ', ' + superClassName + ', ' +
            instVarNames + ');\n';

    } else if(tag === 'varDecls'){
        result = 'var ';
        for(var i = 0; i < args.length; i++){
            result += args[i][0] + " = " + O.transAST(args[i][1]) + ",";
        }
        result = result.substring(0,result.length -1);

    } else if(tag === 'methodDecl'){
        var className = '"' + args[0] + '"';
        currentClass  = className;
        var methodName = '"' + args[1] + '"';
        var functionBody = "function(_this";

        for(var i = 0; i < args[2].length; i++) 
          functionBody += ", " + args[2][i];

        functionBody += ') { \n' 
                      + '  var __RET__ = null;\n' 
                      + '  var __ID__ = Symbol();\n' 
                      + '  try {\n';

        for(var i = 0; i < args[3].length; i++)
          functionBody += '    ' + O.transAST(args[3][i]) + ';\n';


        functionBody += '  } catch(e) {\n'
                      + '    if(e instanceof __RETURN__ && e.__ID__ == __ID__){\n'
                      + '      __RET__ = e.__RET__;\n'
                      + '    } else {\n'
                      + '      throw e;\n'
                      + '    }\n'
                      + '  }\n'
                      + '  return __RET__;\n' 
                      + '}\n';
        
        result = "OO.declareMethod(" + className + ', ' + methodName + ',\n' +
            functionBody + ')';

    } else if(tag === 'return'){
        result = 'throw new __RETURN__(__ID__,' + O.transAST(args[0]) + ')';

    } else if(tag === 'this'){
        result = "_this";

    } else if(tag === 'setVar'){
        result = args[0] + " = " + O.transAST(args[1]);

    } else if(tag === 'setInstVar'){
        result = 'OO.setInstVar(_this, "' + args[0] + '",' + O.transAST(args[1]) + ')';

    } else if(tag === 'exprStmt'){
        result = ' __RESULT__ = ' + O.transAST(args[0]);

    } else if(tag === 'null' || tag === 'true' ||tag === 'false'){
        result = tag;

    } else if(tag === 'number' || tag === 'getVar'){
        result = args[0];

    } else if(tag === 'getInstVar'){
        result = 'OO.getInstVar(_this, "' + args[0] + '")';

    } else if(tag === 'new'){
        var className = '"' + args[0] + '"';
        result = "OO.instantiate(" + className;

        for(var i = 1; i < args.length; i++){
            result += ',' + O.transAST(args[i]);
        }
        result += ")";

    } else if(tag === 'send'){
        result = "OO.send(" + O.transAST(args[0]) + ',"' + args[1] + '"';
        for(var i = 2; i < args.length; i++){
            result += ', ' + O.transAST(args[i]);
        }
        result += ')';

    } else if(tag === 'super'){
        result = 'OO.superSend(OO.getSuperClassName(' + currentClass
            + '), _this, "' + args[0] + '"';

        for(var i = 1; i < args.length; i++){
            result += ', ' + O.transAST(args[i]);
        }
        result += ')';

    } else if(tag === 'block'){
        var argList = args[0];
        var stmtList = args[1];
        var theFunction = "function(" +  argList.join(', ') + "){";
        theFunction += 'var __RESULT__ = null;\n';
        for(var i = 0; i < stmtList.length; i++){
            theFunction += O.transAST(stmtList[i]) + ';\n';
        }
        theFunction += 'return __RESULT__;\n}';
        result = 'OO.instantiate("Block",' + theFunction +')';
    } else {
        throw new Error("Unknown tag: " + tag);
    }

    return result;
};
