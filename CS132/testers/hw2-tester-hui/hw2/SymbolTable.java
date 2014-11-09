

import java.util.ArrayList;
import java.util.Hashtable;

public class SymbolTable {
	
	public enum Type{
		INT, INT_ARRAY, BOOLEAN, STRING_ARRAY, VOID, CLASS
	}
	/*************************************************************
	 *   Symbol
	 */
	public class _Symbol{
		public Type type;
		public String name;
		public String class_name = null;
		
		public _Symbol(String variable, Type t, String typeName) {
			type = t;
			name = variable;
			if(t == Type.CLASS)
				class_name = typeName;
		}

		public String getSymbolType_STRING() {
			switch(type){
			case BOOLEAN:
				return "BOOLEAN";
			case CLASS:
				return class_name;
			case INT:
				return "INT";
			case INT_ARRAY:
				return "INT_ARRAY";
			case STRING_ARRAY:
				return "STING_ARRAY";
			case VOID:
				return "VOID";
			default:
				return null;
			}
		}

	}
	
	/*************************************************************
	 *   Method
	 */
	public class _Method{
		Hashtable<String, _Symbol> symbol_table = new Hashtable<String, _Symbol>();
		ArrayList<_Symbol> parameter_list = new ArrayList<_Symbol>();

		String name;
		
		_Symbol returnSymbol;
		public _Method(String method, Type t, String tn) {
			name = method;
			returnSymbol = new _Symbol("RETURN",t,tn);
		}

		public boolean checkVariableExisted(String variable) {
			_Symbol s = symbol_table.get(variable);
			if(s != null)
				return true;
			else
				return false;
		}
		
		public boolean checkParameterExisted(String variable) {
			for(int i = 0; i < parameter_list.size(); i++){
				if(parameter_list.get(i).name == variable)
					return true;
			}
			return false;
		}
		
		public void addParameter(String variable, Type t, String typeName){
			if(checkParameterExisted(variable))
				throw new Error("Duplicated parameter" +  variable + " in method " + name);
			
			parameter_list.add(new _Symbol(variable,t, typeName));
		}
		
		public void addVariable(String variable, Type t, String typeName){
			if(checkParameterExisted(variable))
				throw new Error("Variable " +  variable + " is already existed as parameter in method " + name);
			
			if(checkVariableExisted(variable))
				throw new Error("Duplicated variable " +  variable + " in method " + name);
			
			symbol_table.put(variable,new _Symbol(variable,t, typeName));
		}
		
		public Type getSymbolType(String variable){
			for(int i = 0; i < parameter_list.size(); i++){
				if(parameter_list.get(i).name == variable)
					return parameter_list.get(i).type;
			}
				
			_Symbol s = symbol_table.get(variable);
			if(s != null)
				return s.type;

			return null;
		}

		public Type getReturnType() {
			return returnSymbol.type;
		}

		public String getIndentifierClassName(String variable) {
			_Symbol s = symbol_table.get(variable);
			if(s != null)
				return s.class_name;
			
			for(int i = 0; i < parameter_list.size(); i++){
				
				if(parameter_list.get(i).name == variable)
					return parameter_list.get(i).class_name;
			}
			return null;
			
		}

		public String getSymbolType_STRING(String variable) {
			for(int i = 0; i < parameter_list.size(); i++){
				if(parameter_list.get(i).name == variable)
					return parameter_list.get(i).getSymbolType_STRING();
			}
				
			_Symbol s = symbol_table.get(variable);
			if(s != null)
				return s.getSymbolType_STRING();

			return null;
		}

		public String getReturnType_STRING() {
			return returnSymbol.getSymbolType_STRING();
		}

		public String getMethodParameter_STRING() {
			if(parameter_list.size() == 0)
				return null;
			String result = parameter_list.get(0).getSymbolType_STRING() ;
			for(int i = 1; i< parameter_list.size(); i++){
				result += ", " + parameter_list.get(i).getSymbolType_STRING() ;
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
		String className;
		
		String extended_class = null;
		
		public _Class(String name, String extend){
			className = name;
			extended_class = extend;
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
		
		public void addVariable(String variable, Type type, String typeName) {
			if (checkVariableExisted(variable))
				throw new Error("Duplicated variable " +  variable + " for Class " + className);
		
			class_variable_table.put(variable,  new _Symbol(variable, type,typeName));
		}
		
		public void addMethod(String method, Type type, String typeName) {
			if (checkMethodExisted(method))
				throw new Error("Duplicated method " + method + " for Class " + className);
			
			class_method_table.put(method,  new _Method(method, type, typeName));
		}

		public void addMethodVariable(String method, String variable, Type type, String typeName) {
			if (!checkMethodExisted(method))
				throw new Error("Create method variable  " + variable + " for nonexist method " + method + " in Class " + className);
			
			if (checkMethodVariableExisted(method, variable))
				throw new Error("Duplicated method variable  " + variable + " for method " + method + " in Class " + className);
			
			class_method_table.get(method).addVariable(variable, type, typeName);
		}
		
		public void addMethodParameter(String method, String variable, Type type, String typeName) {
			if (!checkMethodExisted(method))
				throw new Error("Create method parameter " + variable + " for nonexist method " + method + " in Class " + className);
			
			if (checkMethodParameterExisted(method, variable))
				throw new Error("Duplicated method parameter  " + variable + " for method " + method + " in Class " + className);
			
			class_method_table.get(method).addParameter(variable, type, typeName);
		}
		
		public Type getSymbolType(String method, String variable){
			if (method != null && checkMethodExisted(method)){
				Type t = class_method_table.get(method).getSymbolType(variable);
				if (t != null)
					return t;
			}
			
			_Symbol s = class_variable_table.get(variable);
			if (s != null)
				return s.type;
			else
				return null;
		}

		public Type getMethodReturnType(String method) {
			if(checkMethodExisted(method))
				return class_method_table.get(method).getReturnType();
			else
				return null;
		}

		public String getIndentifierClassName(String method, String variable) {
			_Symbol s = class_variable_table.get(variable);
			if (s != null)
				return s.class_name;
			else
				return class_method_table.get(method).getIndentifierClassName(variable);
			
		}

		public String getSymbolType_String(String method, String variable) {
			if (method != null && checkMethodExisted(method)){
				String t = class_method_table.get(method).getSymbolType_STRING(variable);
				if (t != null)
					return t;
			}
			
			_Symbol s = class_variable_table.get(variable);
			if (s != null)
				return s.getSymbolType_STRING();
			else
				return null;
		}

		public String getMethodReturnType_STRING(String method) {
			if(checkMethodExisted(method))
				return class_method_table.get(method).getReturnType_STRING();
			else
				return null;
					
		}

		public String getMethodParameter_STRING(String method) {
			if(!checkMethodExisted(method)){
				return null;
			}
			return class_method_table.get(method).getMethodParameter_STRING();
		}
	}
	
	/*************************************************************
	 *   Symbol Table
	 */

	Hashtable<String, _Class> class_table= new Hashtable<String, _Class>();
	
	public void addClass(String className){
		if (checkClassExisted(className))
			throw new Error("Duplicated Class " + className);
		
		class_table.put(className, new _Class(className));
	}
	
	public void addExtendedClass(String className, String extend){
		if (checkClassExisted(className))
			throw new Error("Duplicated Class " + className);
		
		class_table.put(className, new _Class(className,extend));
	}
	
	public void addMethod(String className, String method, Type type, String typeName){
		if (!checkClassExisted(className))
			throw new Error("Create method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethod(method, type, typeName);
	}
	
	public void addClassVariable(String className, String variable, Type type, String typeName){
		if (!checkClassExisted(className))
			throw new Error("Create class variable " + variable + " for nonexist Class " + className);
		
		class_table.get(className).addVariable(variable, type, typeName);
	}
	
	public void addMethodVariable(String className, String method, String variable, Type type, String typeName){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethodVariable(method,variable,type, typeName);
	}
	
	public void addMethodParameter(String className, String method, String variable, Type type, String typeName){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethodParameter(method,variable,type, typeName);
	}
	
	
	public boolean checkClassExisted(String className){
		if (class_table.get(className) != null)
			return true;
		else
			return false;
	}
	
	public boolean checkClassMethodExisted(String className, String method){
		if(class_table.get(className).checkMethodExisted(method))
			return true;
		
		String tmp = class_table.get(className).extended_class;
		while(tmp != null){

			if(class_table.get(tmp).checkMethodExisted(method))
				return true;
			
			tmp = class_table.get(tmp).extended_class;
		}
		return false;
	}
	
	public boolean checkClassMethodVariableExisted(String className, String method, String variable){
		_Class c = class_table.get(className);
		if ( c != null)
			return c.checkMethodVariableExisted(method, variable);
		else
			return false;
	}
	
	public boolean checkClassMethodParameterExisted(String className, String method, String variable){
		_Class c = class_table.get(className);
		if ( c != null)
			return c.checkMethodParameterExisted(method, variable);
		else
			return false;
	}
	
	public boolean checkClassVariableExisted(String className, String variable){
		if(class_table.get(className).checkVariableExisted(variable))
			return true;
		
		String tmp = class_table.get(className).extended_class;
		while(tmp != null){
			if(class_table.get(tmp).checkVariableExisted(variable))
				return true;
			
			tmp = class_table.get(tmp).extended_class;
		}
		return false;
		
	}

	
	public void checkIdentifierExistance(String className, String method, String variable) {
		if(checkClassMethodVariableExisted(className, method, variable) || 
		   checkClassMethodParameterExisted(className, method, variable) ||
			checkClassVariableExisted(className, variable))
			return;
		else if(class_table.get(className).extended_class == null)
			throw new Error("unidentified " + variable + " in class " + className + " method " + method);
	}
	
	public void checkIdentifierExistance_Extended(String className, String method, String variable) {
		if(checkClassMethodVariableExisted(className, method, variable) || 
		   checkClassMethodParameterExisted(className, method, variable) ||
			checkClassVariableExisted(className, variable))
			return;
		
		String tmp = class_table.get(className).extended_class;
		while(tmp != null){
			if(checkClassVariableExisted(tmp, variable))
				return;
			tmp = class_table.get(tmp).extended_class;
		}
		throw new Error("unidentified " + variable + " in class " + className + " method " + method);
	}

	public Type getSymbolType(String className, String method, String variable){
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		
		String tmp = className;
		Type t = class_table.get(tmp).getSymbolType(method, variable);
		if(t != null)
			return t;
		tmp = class_table.get(tmp).extended_class;
		
		while(tmp != null){
			t = class_table.get(tmp).getSymbolType(null, variable);
			if(t != null)
				return t;
			tmp = class_table.get(tmp).extended_class;
		}
		
		return null;

	}
	
	public Type getMethodReturnType(String className, String method){
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		
		String tmp = className;
		Type t = null;
		
		while(tmp != null){
			t = class_table.get(tmp).getMethodReturnType(method);
			if(t != null)
				return t;
			tmp = class_table.get(tmp).extended_class;
		}
		return t;
	}

	public String checkVariableExistAndGetIdentifierClassName(String className, String method, String variable) {
		checkIdentifierExistance_Extended(className,method,variable);

		String s =  class_table.get(className).getIndentifierClassName(method, variable);
		if(s != null)
			return s;
		
		String tmp = class_table.get(className).extended_class;
		
		while(tmp != null){
			s =  class_table.get(tmp).getIndentifierClassName(null, variable);
			if(s != null)
				return s;
			tmp = class_table.get(tmp).extended_class;
		}
		
		return null;
	}

	public String getSymbolType_STRING(String className,
			String method, String variable) {
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		
		String tmp = className;
		String t = class_table.get(tmp).getSymbolType_String(method, variable);
		if(t != null)
			return t;
		tmp = class_table.get(tmp).extended_class;
		
		
		while(tmp != null){
			t = class_table.get(tmp).getSymbolType_String(null, variable);
			if(t != null)
				return t;
			tmp = class_table.get(tmp).extended_class;
		}
		
		return t;
	}


	public String getMethodReturnType_STRING(String className, String method) {
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		
		String tmp = className;
		String t = null;
		
		while(tmp != null){
			t = class_table.get(tmp).getMethodReturnType_STRING(method);
			if(t != null)
				return t;
			tmp = class_table.get(tmp).extended_class;
		}
		return t;
	}

	public boolean checkIsSubClass(String parent, String child) {
		String tmp = child;
		while(tmp != null){
			tmp = class_table.get(tmp).extended_class;
			if (tmp == parent)
				return true;
			
		}
		return false;
	}

	public String getMethodParamter(String className, String method) {
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		String tmp = className;
		String t = null;
		
		while(tmp != null){
			t = class_table.get(tmp).getMethodParameter_STRING(method);
			if(t != null)
				return t;
			tmp = class_table.get(tmp).extended_class;
		}
		return t;
	}
	
	public void checkMethodParameter(String className, String method, String parameter){
		String t1 = getMethodParamter(className,method);
		if(t1 != parameter && !parameter.equals(t1) && !checkParameter(t1,parameter))
				throw new Error("incorrect Parameters for method " + method + " in class " + className +
						"\nGIVEN:" + parameter + "\nShould Be:" + getMethodParamter(className,method));
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
	
	public Boolean isPrimaryType(String a){
		if("BOOLEAN".equals(a) || "INT".equals(a) ||  "INT_ARRAY".equals(a)){
			return true;
		}else{
			return false;
		}
	}
}
