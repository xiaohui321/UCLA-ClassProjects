package analysis;

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
		public _Symbol(String variable, Type t) {
			type = t;
			name = variable;
		}
		public Type type;
		public String name;
	}
	
	/*************************************************************
	 *   Method
	 */
	public class _Method{
		Hashtable<String, _Symbol> symbol_table = new Hashtable<String, _Symbol>();
		ArrayList<_Symbol> parameter_list = new ArrayList<_Symbol>();

		String name;
		Type returnType;
		public _Method(String method, Type t) {
			name = method;
			returnType = t;
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
		
		public void addParameter(String variable, Type t){
			if(checkParameterExisted(variable))
				throw new Error("Duplicated parameter" +  variable + " in method " + name);
			
			parameter_list.add(new _Symbol(variable,t));
		}
		
		public void addVariable(String variable, Type t){
			if(checkParameterExisted(variable))
				throw new Error("Variable " +  variable + " is already existed as parameter in method " + name);
			
			if(checkVariableExisted(variable))
				throw new Error("Duplicated variable " +  variable + " in method " + name);
			
			symbol_table.put(variable,new _Symbol(variable,t));
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
		
		public void addVariable(String variable, Type type) {
			if (checkVariableExisted(variable))
				throw new Error("Duplicated variable " +  variable + " for Class " + className);
		
			class_variable_table.put(variable,  new _Symbol(variable, type));
		}
		
		public void addMethod(String method, Type type) {
			if (checkMethodExisted(method))
				throw new Error("Duplicated method " + method + " for Class " + className);
			
			class_method_table.put(method,  new _Method(method, type));
		}

		public void addMethodVariable(String method, String variable, Type type) {
			if (!checkMethodExisted(method))
				throw new Error("Create method variable  " + variable + " for nonexist method " + method + " in Class " + className);
			
			if (checkMethodVariableExisted(method, variable))
				throw new Error("Duplicated method variable  " + variable + " for method " + method + " in Class " + className);
			
			class_method_table.get(method).addVariable(variable, type);
		}
		
		public void addMethodParameter(String method, String variable, Type type) {
			if (!checkMethodExisted(method))
				throw new Error("Create method parameter " + variable + " for nonexist method " + method + " in Class " + className);
			
			if (checkMethodParameterExisted(method, variable))
				throw new Error("Duplicated method parameter  " + variable + " for method " + method + " in Class " + className);
			
			class_method_table.get(method).addParameter(variable, type);
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
	}
	
	/*************************************************************
	 *   Symbol Table
	 */

	Hashtable<String, _Class> class_table= new Hashtable<String, _Class>();
	
	public void addClass(String className){
		if (checkClassExisted(className))
			throw new Error("Duplicated Class " + className);
		
		class_table.put(className, new _Class(className));
		//System.out.println("DEBUG add class " + className );
	}
	
	public void addExtendedClass(String className, String extend){
		if (checkClassExisted(className))
			throw new Error("Duplicated Class " + className);
		
		class_table.put(className, new _Class(className,extend));
		//System.out.println("DEBUG add class " + className + " Extend " + extend );
	}
	
	public void addMethod(String className, String method, Type type){
		if (!checkClassExisted(className))
			throw new Error("Create method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethod(method, type);
		//System.out.println("DEBUG add class " + className + " method " + method + " of type " + type);
	}
	
	public void addClassVariable(String className, String variable, Type type){
		if (!checkClassExisted(className))
			throw new Error("Create class variable " + variable + " for nonexist Class " + className);
		
		class_table.get(className).addVariable(variable, type);
		//System.out.println("DEBUG add class " + className + " variable " + variable + " of type " + type);
	}
	
	public void addMethodVariable(String className, String method, String variable, Type type){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethodVariable(method,variable,type);
		//System.out.println("DEBUG add class " + className + " method " + method + " variable " + variable + " of type " + type);
	}
	
	public void addMethodParameter(String className, String method, String variable, Type type){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethodParameter(method,variable,type);
		//System.out.println("DEBUG add class " + className + " method " + method + " parameter " + variable + " of type " + type);
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
		return class_table.get(className).getSymbolType(method, variable);
	}
	
	public Type getMethodReturnType(String className, String method){
		if(!checkClassExisted(className)){
			throw new Error("Class " + className + " not exist.");
		}
		return class_table.get(className).getMethodReturnType(method);
	}
}
