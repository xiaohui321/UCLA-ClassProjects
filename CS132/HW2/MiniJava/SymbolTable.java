package MiniJava;

import java.util.Hashtable;

public class SymbolTable {
	
	public enum SymbolType{
		INT, INT_ARRAY, BOOLEAN
	}
	
	/*************************************************************
	 *   Symbol
	 */
	public class _Symbol{
		public _Symbol(String variable, SymbolType t) {
			type = t;
			name = variable;
		}
		public SymbolType type;
		public String name;
	}
	
	/*************************************************************
	 *   Method
	 */
	public class _Method{
		Hashtable<String, _Symbol> symbol_table = new Hashtable<String, _Symbol>();
		String name;
		
		public _Method(String method) {
			name = method;
		}

		public boolean checkVariableExisted(String variable) {
			_Symbol s = symbol_table.get(variable);
			if(s != null)
				return true;
			else
				return false;
		}
		
		public void addVariable(String variable, SymbolType t){
			if(checkVariableExisted(variable))
				throw new Error("Duplicated variable " +  variable + " in method " + name);
			
			symbol_table.put(variable,new _Symbol(variable,t));
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

		public void addMethod(String method) {
			if (checkMethodExisted(method))
				throw new Error("Duplicated method " + method + " for Class " + className);
			
			class_method_table.put(method,  new _Method(method));
		}

		public void addVariable(String variable, SymbolType type) {
			if (checkVariableExisted(variable))
				throw new Error("Duplicated variable " +  variable + " for Class " + className);
		
			class_variable_table.put(variable,  new _Symbol(variable, type));
		}

		
		public void addMethodVariable(String method, String variable, SymbolType type) {
			if (!checkMethodExisted(method))
				throw new Error("Create method variable  " + variable + " for nonexist method " + method + " in Class " + className);
			
			if (checkMethodVariableExisted(method, variable))
				throw new Error("Duplicated method variable  " + variable + " for method " + method + " in Class " + className);
			
			class_method_table.get(method).addVariable(variable, type);
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
	
	public void addMethod(String className, String method){
		if (!checkClassExisted(className))
			throw new Error("Create method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethod(method);
	}
	
	public void addClassVariable(String className, String variable, SymbolType type){
		if (!checkClassExisted(className))
			throw new Error("Create class variable " + variable + " for nonexist Class " + className);
		
		class_table.get(className).addVariable(variable, type);
	}
	
	public void addMethodVariable(String className, String method, String variable, SymbolType type){
		if (!checkClassExisted(className))
			throw new Error("Create method variable  " + variable + " in method " + method + " for nonexist Class " + className);
		
		class_table.get(className).addMethodVariable(method,variable,type);
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
	
	public boolean checkClassMethodVaribleExisted(String className, String method, String variable){
		_Class c = class_table.get(className);
		if ( c != null)
			return c.checkMethodVariableExisted(method, variable);
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
}
