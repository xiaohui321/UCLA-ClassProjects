package analysis;

import java.util.ArrayList;
import java.util.Hashtable;

public class SymbolTable {
	boolean debug = false;
	
	public enum Type{
		INT, INT_ARRAY, BOOLEAN, STRING_ARRAY, VOID, CLASS
	}
	/*************************************************************
	 *   Symbol
	 */
	public class _Symbol{
		public Type type;
		public String name;
		public String class_name = "";
		
		public _Symbol(String variable, Type t, String typeName) {
			type = t;
			name = variable;
			if(t == Type.CLASS)
				class_name = typeName;
		}

	}
	
	/*************************************************************
	 *   Method
	 */
	public class _Method{
		Hashtable<String, _Symbol> symbol_table = new Hashtable<String, _Symbol>();
		ArrayList<_Symbol> parameter_list = new ArrayList<_Symbol>();

		String name;
		Type returnType;
		String typeName;
		public _Method(String method, Type t, String tn) {
			name = method;
			returnType = t;
			if(t == Type.CLASS)
				typeName = tn;
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
			return returnType;
		}

		public String getIndentifierClassName(String variable) {
			_Symbol s = symbol_table.get(variable);
			if(s != null)
				return s.class_name;
			
			for(int i = 0; i < parameter_list.size(); i++){
				if(parameter_list.get(i).name == variable)
					return parameter_list.get(i).class_name;
			}
			return "";
			
		}
	}
	
	
	/*************************************************************
	 *   Class
	 */
	
	public class _Class{
		Hashtable<String, _Symbol> class_variable_table = new Hashtable<String, _Symbol>();
		Hashtable<String, _Method> class_method_table = new Hashtable<String, _Method>();
		String className;
		
		String extended_class = "";
		
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
			
			if (method != "" && checkMethodExisted(method)){
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
			if(!checkMethodExisted(method)){
				throw new Error("Class " + className + " Method " + method + " not exist.");
			}
			return class_method_table.get(method).getReturnType();
		}

		public String getIndentifierClassName(String method, String variable) {
			_Symbol s = class_variable_table.get(variable);
			if (s != null)
				return s.class_name;
			else
				return class_method_table.get(method).getIndentifierClassName(variable);
			
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
		if(debug) System.out.println("DEBUG add class " + className );
	}
	
	public void addExtendedClass(String className, String extend){
		if (checkClassExisted(className))
			throw new Error("Duplicated Class " + className);
		
		class_table.put(className, new _Class(className,extend));
		if(debug) System.out.println("DEBUG add class " + className + " Extend " + extend );
	}
	
	public void addMethod(String className, String method, Type type, String typeName){
		if (!checkClassExisted(className))
			throw new Error("Create method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethod(method, type, typeName);
		if(debug) System.out.println("DEBUG add class " + className + " method " + method + " of type " + type);
	}
	
	public void addClassVariable(String className, String variable, Type type, String typeName){
		if (!checkClassExisted(className))
			throw new Error("Create class variable " + variable + " for nonexist Class " + className);
		
		class_table.get(className).addVariable(variable, type, typeName);
		if(debug) System.out.println("DEBUG add class " + className + " variable " + variable + " of type " + type);
	}
	
	public void addMethodVariable(String className, String method, String variable, Type type, String typeName){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethodVariable(method,variable,type, typeName);
		if(debug) System.out.println("DEBUG add class " + className + " method " + method + " variable " + variable + " of type " + type);
	}
	
	public void addMethodParameter(String className, String method, String variable, Type type, String typeName){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethodParameter(method,variable,type, typeName);
		if(debug) System.out.println("DEBUG add class " + className + " method " + method + " parameter " + variable + " of type " + type);
	}
	
	
	public boolean checkClassExisted(String className){
		if (class_table.get(className) != null)
			return true;
		else
			return false;
	}
	
	public boolean checkClassMethodExisted(String className, String method){
		_Class c = class_table.get(className);
		if ( c != null)
			return c.checkMethodExisted(method);
		else
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
		_Class c = class_table.get(className);
		if ( c != null)
			return c.checkVariableExisted(variable);
		else
			return false;
	}

	
	public void checkIdentifierExistance(String className, String method, String variable) {
		if(checkClassMethodVariableExisted(className, method, variable) || 
		   checkClassMethodParameterExisted(className, method, variable) ||
			checkClassVariableExisted(className, variable))
			return;
		else if(class_table.get(className).extended_class == "")
			throw new Error("unidentified " + variable + " in class " + className + " method " + method);
	}
	
	public void checkIdentifierExistance_Extended(String className, String method, String variable) {
		if(checkClassMethodVariableExisted(className, method, variable) || 
		   checkClassMethodParameterExisted(className, method, variable) ||
			checkClassVariableExisted(className, variable))
			return;
		
		String extendClass = class_table.get(className).extended_class;
		if(extendClass != "" && checkClassVariableExisted(extendClass, variable))
			return;
			
		throw new Error("unidentified " + variable + " in class " + className + " method " + method);
	}

	public Type getSymbolType(String className, String method, String variable){
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		Type t = class_table.get(className).getSymbolType(method, variable);
		
		if ( t == null){
			String extendClass = class_table.get(className).extended_class;
			if(extendClass != "" && checkClassVariableExisted(extendClass, variable)){
				t =  class_table.get(extendClass).getSymbolType("", variable);
			}
		}
		
		return t;

	}
	
	public Type getMethodReturnType(String className, String method){
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		Type t =  class_table.get(className).getMethodReturnType(method);
		
		if ( t == null){
			String extendClass = class_table.get(className).extended_class;
			if(extendClass != "" && checkClassMethodExisted(extendClass, method)){
				t =  class_table.get(extendClass).getMethodReturnType(method);
			}
		}
		
		return t;
	}

	public String checkVariableExistAndGetIdentifierClassName(String className, String method, String variable) {
		checkIdentifierExistance_Extended(className,method,variable);
		String s =  class_table.get(className).getIndentifierClassName(method, variable);
		if ( s == ""){
			String extendClass = class_table.get(className).extended_class;
			if(extendClass != "" && checkClassVariableExisted(extendClass, variable)){
				s =  class_table.get(extendClass).getIndentifierClassName("", variable);
			}
		}
		
		return s;
	}

	public String getSymbolType_STRING(String currentClass,
			String currentMethod, String currentName) {
		// TODO Auto-generated method stub
		return null;
	}
}
