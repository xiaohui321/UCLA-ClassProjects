import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

public class SymbolTable {

	public enum TYPES{
		INT, INT_ARRAY, BOOLEAN, CLASS, VOID
	}
	
	/*************************************************************
	 *   _Type
	 */
	public class _Type{
		public TYPES type;
		public String className; //class name if t == CLASS
		
		public _Type(TYPES t, String class_name){
			type = t;
			className = class_name;
		}
		
		public _Type(TYPES t){
			type = t;
		}
		
		public String getTypeInString() {
			switch(type){
			case BOOLEAN:
				return "BOOLEAN";
			case CLASS:
				return className;
			case INT:
				return "INT";
			case INT_ARRAY:
				return "INT_ARRAY";
			default:
				return null;
			}
		}
	}
	
	/*************************************************************
	 *   Symbol
	 */
	public class _Symbol{
		public _Type type;
		public String symbolName;
		
		public _Symbol(String symbol_name, TYPES t, String type_class_name) {
			type = new _Type(t,type_class_name);
			symbolName = symbol_name;
		}
		
		public _Symbol(String symbol_name, TYPES t) {
			type = new _Type(t);
			symbolName = symbol_name;
		}
		
		public _Symbol(String symbol_name, _Type t) {
			type = t;
			symbolName = symbol_name;
		}
	}
	
	/*************************************************************
	 *   Method
	 */
	public class _Method{
		private Hashtable<String, _Symbol> symbol_table = new Hashtable<String, _Symbol>(); //symbols that are shown within the method
		private ArrayList<_Symbol> parameters_list = new ArrayList<_Symbol>();
		private _Type returnType;
		
		public String methodName;
		
		public _Method(String method_name, TYPES return_type, String return_class_name) {
			methodName = method_name;
			returnType = new _Type(return_type, return_class_name);
		}
		
		public _Method(String method_name, TYPES return_type) {
			methodName = method_name;
			returnType = new _Type(return_type);
		}
		
		public boolean checkVariableExisted(String variable_name) {
			_Symbol s = symbol_table.get(variable_name);
			if(s != null)
				return true;
			else
				return false;
		}
		
		public boolean checkParameterExisted(String variable_name) {
			for(int i = 0; i < parameters_list.size(); i++){
				if(parameters_list.get(i).symbolName == variable_name)
					return true;
			}
			return false;
		}
		
		public void addParameter(String variable, TYPES type, String type_class_name){
			if(checkParameterExisted(variable))
				throw new Error("Duplicated parameter" +  variable + " in method " + methodName);
			
			parameters_list.add(new _Symbol(variable, type, type_class_name));
		}
		
		public void addParameter(String variable, TYPES type){
			if(checkParameterExisted(variable))
				throw new Error("Duplicated parameter" +  variable + " in method " + methodName);
			
			parameters_list.add(new _Symbol(variable, type));
		}
		
		public void addVariable(String variable, TYPES type, String type_class_name){
			if(checkParameterExisted(variable))
				throw new Error("Variable " +  variable + " is already existed as parameter in method " + methodName);
			
			if(checkVariableExisted(variable))
				throw new Error("Duplicated variable " +  variable + " in method " + methodName);
			
			symbol_table.put(variable,new _Symbol(variable,type, type_class_name));
		}
		
		public void addVariable(String variable, TYPES type){
			if(checkParameterExisted(variable))
				throw new Error("Variable " +  variable + " is already existed as parameter in method " + methodName);
			
			if(checkVariableExisted(variable))
				throw new Error("Duplicated variable " +  variable + " in method " + methodName);
			
			symbol_table.put(variable,new _Symbol(variable,type));
		}
		
		public _Type getSymbolType(String variable){
			for(int i = 0; i < parameters_list.size(); i++){
				if(parameters_list.get(i).symbolName == variable)
					return parameters_list.get(i).type;
			}
				
			_Symbol s = symbol_table.get(variable);
			if(s != null)
				return s.type;

			return null;
		}

		public _Type getReturnType() {
			return returnType;
		}

		public String getMethodParametersInString() {
			if(parameters_list.size() == 0)
				return null;
			String result = parameters_list.get(0).type.getTypeInString() ;
			for(int i = 1; i< parameters_list.size(); i++){
				result += ", " + parameters_list.get(i).type.getTypeInString() ;
			}
			return result;
		}
	}
	
	
	/*************************************************************
	 *   Class
	 */
	
	public class _Class{
		Hashtable<String, _Symbol> class_variable_table = new Hashtable<String, _Symbol>();
		Hashtable<String, _Method> class_method_table = new Hashtable<String, _Method>();
		
		public String className;
		
		public String parentClassName = null;
		
		public _Class parentClass = null;
		
		public _Class(String name, String parent_class_name){
			className = name;
			parentClassName = parent_class_name;
		}
		
		public _Class(String name){
			className = name;
		}

		public boolean checkMethodExisted(String method) {
			_Method m = class_method_table.get(method);
			if (m != null)
				return true;
			else
				return false;
		}
		
		public boolean checkVariableExisted(String variable) {
			_Symbol s = class_variable_table.get(variable);
			if (s != null)
				return true;
			else
				return false;
		}
		
		public boolean checkMethodVariableExisted(String method, String variable) {
			_Method m = class_method_table.get(method);
			if (m != null)
				return m.checkVariableExisted(variable);
			else
				return false;
		}
		
		public boolean checkMethodParameterExisted(String method, String variable) {
			_Method m = class_method_table.get(method);
			if (m != null)
				return m.checkParameterExisted(variable);
			else
				return false;
		}
		
		public void addClassVariable(String variable_name, TYPES type, String type_class_name) {
			if (checkVariableExisted(variable_name))
				throw new Error("Duplicated variable " +  variable_name + " for Class " + className);
		
			class_variable_table.put(variable_name,  new _Symbol(variable_name, type,type_class_name));
		}
		
		public void addClassVariable(String variable_name, TYPES type) {
			if (checkVariableExisted(variable_name))
				throw new Error("Duplicated variable " +  variable_name + " for Class " + className);
		
			class_variable_table.put(variable_name,  new _Symbol(variable_name, type));
		}
		
		public void addMethod(String method, TYPES type, String type_class_name) {
			if (checkMethodExisted(method))
				throw new Error("Duplicated method " + method + " for Class " + className);
			
			class_method_table.put(method,  new _Method(method, type, type_class_name));
		}
		
		public void addMethod(String method, TYPES type) {
			if (checkMethodExisted(method))
				throw new Error("Duplicated method " + method + " for Class " + className);
			
			class_method_table.put(method,  new _Method(method, type));
		}

		public void addMethodVariable(String method, String variable, TYPES type, String type_class_name) {
			if (!checkMethodExisted(method))
				throw new Error("Create method variable  " + variable + " for nonexist method " + method + " in Class " + className);
			
			if (checkMethodVariableExisted(method, variable))
				throw new Error("Duplicated method variable  " + variable + " for method " + method + " in Class " + className);
			
			class_method_table.get(method).addVariable(variable, type, type_class_name);
		}
		
		public void addMethodVariable(String method, String variable, TYPES type) {
			if (!checkMethodExisted(method))
				throw new Error("Create method variable  " + variable + " for nonexist method " + method + " in Class " + className);
			
			if (checkMethodVariableExisted(method, variable))
				throw new Error("Duplicated method variable  " + variable + " for method " + method + " in Class " + className);
			
			class_method_table.get(method).addVariable(variable, type);
		}
		
		public void addMethodParameter(String method, String variable, TYPES type, String type_class_name) {
			if (!checkMethodExisted(method))
				throw new Error("Create method parameter " + variable + " for nonexist method " + method + " in Class " + className);
			
			if (checkMethodParameterExisted(method, variable))
				throw new Error("Duplicated method parameter  " + variable + " for method " + method + " in Class " + className);
			
			class_method_table.get(method).addParameter(variable, type, type_class_name);
		}
		
		public void addMethodParameter(String method, String variable, TYPES type) {
			if (!checkMethodExisted(method))
				throw new Error("Create method parameter " + variable + " for nonexist method " + method + " in Class " + className);
			
			if (checkMethodParameterExisted(method, variable))
				throw new Error("Duplicated method parameter  " + variable + " for method " + method + " in Class " + className);
			
			class_method_table.get(method).addParameter(variable, type);
		}
		
		public _Type getSymbolType(String method, String variable){
			if (method != null && checkMethodExisted(method)){
				_Type t = class_method_table.get(method).getSymbolType(variable);
				if (t != null)
					return t;
			}
			
			_Symbol s = class_variable_table.get(variable);
			if (s != null)
				return s.type;
			else
				return null;
		}

		public _Type getMethodReturnType(String method) {
			if(checkMethodExisted(method))
				return class_method_table.get(method).getReturnType();
			else
				return null;
		}

		public String getMethodParameterInString(String method) {
			if(!checkMethodExisted(method)){
				return null;
			}
			return class_method_table.get(method).getMethodParametersInString();
		}
	}
	
	/*************************************************************
	 *   Symbol Table
	 */
	Hashtable<String, _Class> classTable= new Hashtable<String, _Class>();
	
	public void addClass(String className){
		if (checkClassExisted(className))
			throw new Error("Duplicated Class " + className);
		
		classTable.put(className, new _Class(className));
	}
	
	public void addExtendedClass(String className, String extend){
		if (checkClassExisted(className))
			throw new Error("Duplicated Class " + className);
		
		classTable.put(className, new _Class(className,extend));
	}
	
	public void addMethod(String className, String method, TYPES type, String type_class_name){
		if (!checkClassExisted(className))
			throw new Error("Create method " + method + " for nonexist Class " + className);
		
		classTable.get(className).addMethod(method, type, type_class_name);
	}
	
	public void addMethod(String className, String method, TYPES type){
		if (!checkClassExisted(className))
			throw new Error("Create method " + method + " for nonexist Class " + className);
		
		classTable.get(className).addMethod(method, type);
	}
	
	public void addClassVariable(String className, String variable, TYPES type, String type_class_name){
		if (!checkClassExisted(className))
			throw new Error("Create class variable " + variable + " for nonexist Class " + className);
		
		classTable.get(className).addClassVariable(variable, type, type_class_name);
	}
	
	public void addClassVariable(String className, String variable, TYPES type){
		if (!checkClassExisted(className))
			throw new Error("Create class variable " + variable + " for nonexist Class " + className);
		
		classTable.get(className).addClassVariable(variable, type);
	}
	
	public void addMethodVariable(String className, String method, String variable, TYPES type, String type_class_name){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		classTable.get(className).addMethodVariable(method,variable,type, type_class_name);
	}
	
	public void addMethodVariable(String className, String method, String variable, TYPES type){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		classTable.get(className).addMethodVariable(method,variable,type);
	}
	
	public void addMethodParameter(String className, String method, String variable, TYPES type, String type_class_name){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		classTable.get(className).addMethodParameter(method,variable,type, type_class_name);
	}
	
	public void addMethodParameter(String className, String method, String variable, TYPES type){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		classTable.get(className).addMethodParameter(method,variable,type);
	}
		
	public boolean checkClassExisted(String className){
		if (classTable.get(className) != null)
			return true;
		else
			return false;
	}
	
	public boolean checkClassMethodExisted(String className, String methodName){
		if(classTable.get(className).checkMethodExisted(methodName))
			return true;
		
		String tmp = classTable.get(className).parentClassName;
		while(tmp != null){

			if(classTable.get(tmp).checkMethodExisted(methodName))
				return true;
			
			tmp = classTable.get(tmp).parentClassName;
		}
		return false;
	}
	
	public boolean checkClassMethodVariableExisted(String className, String method, String variable){
		_Class c = classTable.get(className);
		if ( c != null)
			return c.checkMethodVariableExisted(method, variable);
		else
			return false;
	}
	
	public boolean checkClassMethodParameterExisted(String className, String method, String variable){
		_Class c = classTable.get(className);
		if ( c != null)
			return c.checkMethodParameterExisted(method, variable);
		else
			return false;
	}
	
	public boolean checkClassVariableExisted(String className, String variable){
		if(classTable.get(className).checkVariableExisted(variable))
			return true;
		
		String tmp = classTable.get(className).parentClassName;
		while(tmp != null){
			if(classTable.get(tmp).checkVariableExisted(variable))
				return true;
			
			tmp = classTable.get(tmp).parentClassName;
		}
		return false;
		
	}
	
	public void checkIdentifierExistence(String className, String method, String variable) {
		if(checkClassMethodVariableExisted(className, method, variable) || 
		   checkClassMethodParameterExisted(className, method, variable) ||
			checkClassVariableExisted(className, variable))
			return;
		else if(classTable.get(className).parentClassName == null)
			throw new Error("unidentified " + variable + " in class " + className + " method " + method);
	}
	
	public void checkIdentifierExistence_Extended(String className, String method, String variable) {
		if(checkClassMethodVariableExisted(className, method, variable) || 
		   checkClassMethodParameterExisted(className, method, variable) ||
			checkClassVariableExisted(className, variable))
			return;
		
		String tmp = classTable.get(className).parentClassName;
		while(tmp != null){
			if(checkClassVariableExisted(tmp, variable))
				return;
			tmp = classTable.get(tmp).parentClassName;
		}
		throw new Error("unidentified " + variable + " in class " + className + " method " + method);
	}

	public _Type getSymbolType(String className, String method, String variable){
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		
		String tmp = className;
		_Type t = classTable.get(tmp).getSymbolType(method, variable);
		if(t != null)
			return t;
		tmp = classTable.get(tmp).parentClassName;
		
		while(tmp != null){
			t = classTable.get(tmp).getSymbolType(null, variable);
			if(t != null)
				return t;
			tmp = classTable.get(tmp).parentClassName;
		}
		
		return null;
	}
	
	public _Type getMethodReturnType(String className, String method){
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		
		String tmp = className;
		_Type t = null;
		
		while(tmp != null){
			t = classTable.get(tmp).getMethodReturnType(method);
			if(t != null)
				return t;
			tmp = classTable.get(tmp).parentClassName;
		}
		return t;
	}

	public boolean checkIsSubClass(String parent, String child) {
		String tmp = child;
		while(tmp != null){
			tmp = classTable.get(tmp).parentClassName;
			if (tmp == parent)
				return true;
			
		}
		return false;
	}

	public String getMethodParamterInString(String className, String method) {
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		String tmp = className;
		String t = null;
		
		while(tmp != null){
			t = classTable.get(tmp).getMethodParameterInString(method);
			if(t != null)
				return t;
			tmp = classTable.get(tmp).parentClassName;
		}
		return t;
	}
	
	public void checkMethodParameter(String className, String method, String parameter){
		String t1 = getMethodParamterInString(className,method);
		if(t1 != parameter && !parameter.equals(t1) && !checkParameter(t1,parameter))
				throw new Error("incorrect Parameters for method " + method + " in class " + className +
						"\nGIVEN:" + parameter + "\nShould Be:" + getMethodParamterInString(className,method));
	}

	private boolean checkParameter(String parent, String child) {
		if(parent == null || child == null)
			return false;
		
		String [] p = parent.split(", ");
		String [] c = child.split(", ");
		
		if(p.length != c.length)
			return false;
		
		for(int i = 0; i < p.length; i++){
			if(!p[i].equals(c[i]) && (isPrimaryType(c[i]) || !checkIsSubClass(p[i], c[i]))  ){
					return false;
			}
			
		}

		return true;
	}
	
	private Boolean isPrimaryType(String a){
		if("BOOLEAN".equals(a) || "INT".equals(a) ||  "INT_ARRAY".equals(a)){
			return true;
		}else{
			return false;
		}
	}

	public String getClassAndMethodDeclaration() {
		String result = "\n";
		
		for(Enumeration<_Class> classes = classTable.elements(); classes.hasMoreElements();){
			 _Class c = classes.nextElement();
			 
			 if(c.checkMethodExisted("main")) continue;
			 
			 result += "const vmt_" + c.className + "\n";
			 
			 for(Enumeration<_Method> methods = c.class_method_table.elements(); methods.hasMoreElements();){
				 _Method m = methods.nextElement();
				 result += "\t:" + c.className + "." + m.methodName +"\n";
			 }
			 
			 result += "\n";
		}
		
		return result;
	}
}
