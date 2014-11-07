package visitor;

import analysis.SymbolTable;

import java.util.Enumeration;

import syntaxtree.*;

public class ThirdRoundVisitor extends GJDepthFirst<String,Object> {
	public SymbolTable st;
	public String currentClass = "";
	public String currentMethod = "";
	public String currentName = "";
    public String currentusedClass = "";
	public ThirdRoundVisitor(SymbolTable t){
		st = t;
	}

	/**********************************
	 * Helpers
	 */
	public void checkIsBoolean(String t){
		if(t != "BOOLEAN")
			throw new Error("Expression Type is not boolean" + "[class " + currentClass + " method " + currentMethod + "]");
	}
	
	public void checkIsInt(String t){
		if(t != "INT")
			throw new Error("Expression Type is not int" + "[class " + currentClass + " method " + currentMethod + "]");
	}
	
	public void checkIsIntArray(String t){
		if(t != "INT_ARRAY")
			throw new Error("Expression Type is not int[]" + "[class " + currentClass + " method " + currentMethod + "]");
	}
	
	
	/*
	public void checkIsSame(String t1, String t2){
		if(t1 != t2)
			throw new Error("Expression Types are not equal:" + t1 +"," + t2 + "[class " + currentClass + " method " + currentMethod + "]");
	}
	
	public void checkReturnType(String t1, String classname, String method){
		if(st.getMethodReturnType(classname,method) != t1)
			throw new Error("Incorrect Return type for class " + classname + " method " + method);
	}
	
	
	
	public void checkClassExist(String name){
		if(! st.checkClassExisted(name))
			throw new Error("Class " + name + " is not found"  + "[class " + currentClass + " method " + currentMethod + "]");
	}
	
	public void checkClassMethodExist(String className, String method){
		if(! st.checkClassMethodExisted(className,method))
			throw new Error("Class " + className + " method " + method + " is not found"  + "[class " + currentClass + " method " + currentMethod + "]");
	}
*/	

	/**********************************
	 * Visitors
	 */

	public  String visit(NodeList n, Object argu) {
		String _ret=null;
		int _count=0;
		for ( Enumeration<Node> e = n.elements(); e.hasMoreElements(); ) {
			e.nextElement().accept(this,argu);
			_count++;
		}
		return _ret;
	}

	public  String visit(NodeListOptional n, Object argu) {
		if ( n.present() ) {
			String _ret=null;
			int _count=0;
			for ( Enumeration<Node> e = n.elements(); e.hasMoreElements(); ) {
				e.nextElement().accept(this,argu);
				_count++;
			}
			return _ret;
		}
		else
			return null;
	}

	public  String visit(NodeOptional n, Object argu) {
		if ( n.present() )
			return n.node.accept(this,argu);
		else
			return null;
	}

	public  String visit(NodeSequence n, Object argu) {
		String _ret=null;
		int _count=0;
		for ( Enumeration<Node> e = n.elements(); e.hasMoreElements(); ) {
			e.nextElement().accept(this,argu);
			_count++;
		}
		return _ret;
	}

	public  String visit(NodeToken n, Object argu) { return null; }

	//
	// User-generated visitor methods below
	//

	/**
	 * f0 -> MainClass()
	 * f1 -> ( StringDeclaration() )*
	 * f2 -> <EOF>
	 */
	public  String visit(Goal n, Object argu) {
		n.f0.accept(this, argu);
		n.f1.accept(this, argu);
		n.f2.accept(this, argu);
		return null;
	}

	/**
	 * f0 -> "class"
	 * f1 -> Identifier()
	 * f2 -> "{"
	 * f3 -> "public"
	 * f4 -> "static"
	 * f5 -> "void"
	 * f6 -> "main"
	 * f7 -> "("
	 * f8 -> "String"
	 * f9 -> "["
	 * f10 -> "]"
	 * f11 -> Identifier()
	 * f12 -> ")"
	 * f13 -> "{"
	 * f14 -> ( VarDeclaration() )*
	 * f15 -> ( Statement() )*
	 * f16 -> "}"
	 * f17 -> "}"
	 */
	public  String visit(MainClass n, Object argu) {
		currentClass = n.f1.f0.tokenImage;
		currentMethod = "main";
		n.f14.accept(this, argu);
		n.f15.accept(this, argu);
		return null;
	}

	/**
	 * f0 -> ClassDeclaration()
	 *       | ClassExtendsDeclaration()
	 */
	public  String visit(TypeDeclaration n, Object argu) {
		return n.f0.accept(this, argu);
	}

	/**
	 * f0 -> "class"
	 * f1 -> Identifier()
	 * f2 -> "{"
	 * f3 -> ( VarDeclaration() )*
	 * f4 -> ( MethodDeclaration() )*
	 * f5 -> "}"
	 */
	public  String visit(ClassDeclaration n, Object argu) {
		currentClass = n.f1.f0.tokenImage;
		currentMethod = "";
		n.f3.accept(this, argu);
		n.f4.accept(this, argu);
		return null;
	}

	/**
	 * f0 -> "class"
	 * f1 -> Identifier()
	 * f2 -> "extends"
	 * f3 -> Identifier()
	 * f4 -> "{"
	 * f5 -> ( VarDeclaration() )*
	 * f6 -> ( MethodDeclaration() )*
	 * f7 -> "}"
	 */
	public  String visit(ClassExtendsDeclaration n, Object argu) {
		currentClass = n.f1.f0.tokenImage;
		currentMethod = "";
		n.f5.accept(this, argu);
		n.f6.accept(this, argu);
		return null;
	}

	/**
	 * f0 -> Type()
	 * f1 -> Identifier()
	 * f2 -> ";"
	 */
	public  String visit(VarDeclaration n, Object argu) {
		n.f0.accept(this, argu);
		n.f1.accept(this, argu);
		n.f2.accept(this, argu);
		return null;
	}

	/**
	 * f0 -> "public"
	 * f1 -> Type()
	 * f2 -> Identifier()
	 * f3 -> "("
	 * f4 -> ( FormalParameterList() )?
	 * f5 -> ")"
	 * f6 -> "{"
	 * f7 -> ( VarDeclaration() )*
	 * f8 -> ( Statement() )*
	 * f9 -> "return"
	 * f10 -> Expression()
	 * f11 -> ";"
	 * f12 -> "}"
	 */
	public  String visit(MethodDeclaration n, Object argu) {
		String _ret=null;
		n.f1.accept(this, argu);
		n.f2.accept(this, argu);
		currentMethod = currentName;
		
		n.f4.accept(this, argu);
		
		n.f7.accept(this, argu);
		n.f8.accept(this, argu);
		
		checkReturnType(n.f10.accept(this, argu),currentClass, currentMethod);
		return null;
	}

	/**
	 * f0 -> FormalParameter()
	 * f1 -> ( FormalParameterRest() )*
	 */
	public  String visit(FormalParameterList n, Object argu) {
		n.f0.accept(this, argu);
		n.f1.accept(this, argu);
		return null;
	}

	/**
	 * f0 -> Type()
	 * f1 -> Identifier()
	 */
	public  String visit(FormalParameter n, Object argu) {
		n.f0.accept(this, argu);
		n.f1.accept(this, argu);
		return null;
	}

	/**
	 * f0 -> ","
	 * f1 -> FormalParameter()
	 */
	public  String visit(FormalParameterRest n, Object argu) {
		n.f0.accept(this, argu);
		n.f1.accept(this, argu);
		return null;
	}

	/**
	 * f0 -> ArrayType()
	 *       | BooleanType()
	 *       | IntegerType()
	 *       | Identifier()
	 */
	public  String visit(Type n, Object argu) {
		return n.f0.accept(this, argu);
	}

	/**
	 * f0 -> "int"
	 * f1 -> "["
	 * f2 -> "]"
	 */
	public  String visit(ArrayType n, Object argu) {
		return String.INT_ARRAY;
	}

	/**
	 * f0 -> "boolean"
	 */
	public  String visit(BooleanType n, Object argu) {
		return String.BOOLEAN;
	}

	/**
	 * f0 -> "int"
	 */
	public  String visit(IntegerType n, Object argu) {
		return  String.INT;
	}

	/**
	 * f0 -> Block()
	 *       | AssignmentStatement()
	 *       | ArrayAssignmentStatement()
	 *       | IfStatement()
	 *       | WhileStatement()
	 *       | PrintStatement()
	 */
	public  String visit(Statement n, Object argu) {
		return n.f0.accept(this, argu);
	}

	/**
	 * f0 -> "{"
	 * f1 -> ( Statement() )*
	 * f2 -> "}"
	 */
	public  String visit(Block n, Object argu) {
		return n.f1.accept(this, argu);
	}

	/**
	 * f0 -> Identifier()
	 * f1 -> "="
	 * f2 -> Expression()
	 * f3 -> ";"
	 */
	public  String visit(AssignmentStatement n, Object argu) {
		n.f0.accept(this, argu);
		st.checkIdentifierExistance_Extended(currentClass, currentMethod, currentName);
		String t1 = st.getSymbolType(currentClass, currentMethod, currentName);
		String t2 = n.f2.accept(this, argu);
		checkIsSame(t1,t2);
		return null;
	}

	/**
	 * f0 -> Identifier()
	 * f1 -> "["
	 * f2 -> Expression()
	 * f3 -> "]"
	 * f4 -> "="
	 * f5 -> Expression()
	 * f6 -> ";"
	 */
	public  String visit(ArrayAssignmentStatement n, Object argu) {
		checkIsIntArray(n.f0.accept(this, argu));
		st.checkIdentifierExistance_Extended(currentClass, currentMethod, currentName);
		
		checkIsInt(n.f2.accept(this, argu));
		checkIsInt(n.f5.accept(this, argu));
		return null;
	}

	/**
	 * f0 -> "if"
	 * f1 -> "("
	 * f2 -> Expression()
	 * f3 -> ")"
	 * f4 -> Statement()
	 * f5 -> "else"
	 * f6 -> Statement()
	 */
	public  String visit(IfStatement n, Object argu) {
		checkIsBoolean(n.f2.accept(this, argu));
		n.f4.accept(this, argu);
		n.f6.accept(this, argu);
		return null;
	}

	/**
	 * f0 -> "while"
	 * f1 -> "("
	 * f2 -> Expression()
	 * f3 -> ")"
	 * f4 -> Statement()
	 */
	public  String visit(WhileStatement n, Object argu) {
		checkIsBoolean(n.f2.accept(this, argu));
		
		n.f4.accept(this, argu);
		return null;
		//UNSURE
	}

	/**
	 * f0 -> "System.out.println"
	 * f1 -> "("
	 * f2 -> Expression()
	 * f3 -> ")"
	 * f4 -> ";"
	 */
	public  String visit(PrintStatement n, Object argu) {
		n.f2.accept(this, argu);
		//checkIsInt(n.f2.accept(this, argu));
		return null;
	}

	/**
	 * f0 -> AndExpression()
	 *       | CompareExpression()
	 *       | PlusExpression()
	 *       | MinusExpression()
	 *       | TimesExpression()
	 *       | ArrayLookup()
	 *       | ArrayLength()
	 *       | MessageSend()
	 *       | PrimaryExpression()
	 */
	public  String visit(Expression n, Object argu) {
		return n.f0.accept(this, argu);
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "&&"
	 * f2 -> PrimaryExpression()
	 */
	public  String visit(AndExpression n, Object argu) {
		checkIsBoolean(n.f0.accept(this, argu));
		checkIsBoolean(n.f2.accept(this, argu));
		return String.BOOLEAN;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "<"
	 * f2 -> PrimaryExpression()
	 */
	public  String visit(CompareExpression n, Object argu) {
		checkIsInt(n.f0.accept(this, argu));
		checkIsInt(n.f2.accept(this, argu));
		return String.BOOLEAN;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "+"
	 * f2 -> PrimaryExpression()
	 */
	public  String visit(PlusExpression n, Object argu) {
		checkIsInt(n.f0.accept(this, argu));
		checkIsInt(n.f2.accept(this, argu));
		return String.INT;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "-"
	 * f2 -> PrimaryExpression()
	 */
	public  String visit(MinusExpression n, Object argu) {
		checkIsInt(n.f0.accept(this, argu));
		checkIsInt(n.f2.accept(this, argu));
		return String.INT;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "*"
	 * f2 -> PrimaryExpression()
	 */
	public  String visit(TimesExpression n, Object argu) {
		checkIsInt(n.f0.accept(this, argu));
		checkIsInt(n.f2.accept(this, argu));
		return String.INT;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "["
	 * f2 -> PrimaryExpression()
	 * f3 -> "]"
	 */
	public  String visit(ArrayLookup n, Object argu) {
		checkIsIntArray(n.f0.accept(this, argu));
		checkIsInt(n.f2.accept(this, argu));
		return String.INT;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "."
	 * f2 -> "length"
	 */
	public  String visit(ArrayLength n, Object argu) {
		checkIsIntArray(n.f0.accept(this, argu));
		return String.INT;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "."
	 * f2 -> Identifier()
	 * f3 -> "("
	 * f4 -> ( ExpressionList() )?
	 * f5 -> ")"
	 */
	public  String visit(MessageSend n, Object argu) {
		if(n.f0.f0.choice instanceof ThisExpression){
			n.f2.accept(this, argu);
			checkClassMethodExist(currentClass,currentName);
			String t = st.getMethodReturnType(currentClass, currentName);
			//TODO check parameter
			n.f4.accept(this, argu);
			return t;
		}else if(n.f0.f0.choice instanceof Identifier){
			n.f0.accept(this, argu);
			currentusedClass = st.checkVariableExistAndGetIdentifierClassName(currentClass, currentMethod, currentName);
			n.f2.accept(this, argu);
			checkClassMethodExist(currentusedClass,currentName);
			String t = st.getMethodReturnType(currentusedClass,currentName);
			//TODO check parameter
			n.f4.accept(this, argu);
			return t;
		}else if(n.f0.f0.choice instanceof AllocationExpression){
			currentusedClass = ((AllocationExpression)n.f0.f0.choice).f1.f0.tokenImage;
			n.f2.accept(this, argu);
			checkClassMethodExist(currentusedClass,currentName);
			String t = st.getMethodReturnType(currentusedClass,currentName);
			//TODO check parameter
			n.f4.accept(this, argu);
			return t;
		}else if(n.f0.f0.choice instanceof BracketExpression){
			((BracketExpression)n.f0.f0.choice).f1.f0.accept(this,argu);
			n.f2.accept(this, argu);
			checkClassMethodExist(currentusedClass,currentName);
			String t = st.getMethodReturnType(currentusedClass,currentName);
			//TODO check parameter
			n.f4.accept(this, argu);
			return t;
		}
		
		throw new Error("Wrong Grammar");
	}

	/**
	 * f0 -> Expression()
	 * f1 -> ( ExpressionRest() )*
	 */
	public  String visit(ExpressionList n, Object argu) {
		return n.f0.accept(this, argu) + n.f1.accept(this, argu);
	}

	/**
	 * f0 -> ","
	 * f1 -> Expression()
	 */
	public  String visit(ExpressionRest n, Object argu) {
		return "," + n.f1.accept(this, argu);
	}

	/**
	 * f0 -> IntegerLiteral()
	 *       | TrueLiteral()
	 *       | FalseLiteral()
	 *       | Identifier()
	 *       | ThisExpression()
	 *       | ArrayAllocationExpression()
	 *       | AllocationExpression()
	 *       | NotExpression()
	 *       | BracketExpression()
	 */
	public  String visit(PrimaryExpression n, Object argu) {
		return n.f0.accept(this, argu);
	}

	/**
	 * f0 -> <INTEGER_LITERAL>
	 */
	public  String visit(IntegerLiteral n, Object argu) {
		return "INT";
	}

	/**
	 * f0 -> "true"
	 */
	public  String visit(TrueLiteral n, Object argu) {
		return "BOOLEAN"; 
	}

	/**
	 * f0 -> "false"
	 */
	public  String visit(FalseLiteral n, Object argu) {
		return "BOOLEAN"; 
	}

	/**
	 * f0 -> <IDENTIFIER>
	 */
	public  String visit(Identifier n, Object argu) {
		currentName = n.f0.tokenImage;
		return st.getSymbolType_STRING(currentClass,currentMethod, currentName);
	}

	/**
	 * f0 -> "this"
	 */
	public  String visit(ThisExpression n, Object argu) {
		return "THIS";
		//TODO
	}

	/**
	 * f0 -> "new"
	 * f1 -> "int"
	 * f2 -> "["
	 * f3 -> Expression()
	 * f4 -> "]"
	 */
	public  String visit(ArrayAllocationExpression n, Object argu) {		
		return "INT_ARRAY";
	}

	/**
	 * f0 -> "new"
	 * f1 -> Identifier()
	 * f2 -> "("
	 * f3 -> ")"
	 */
	public  String visit(AllocationExpression n, Object argu) {
		n.f1.accept(this, argu);
		return "CLASS_" + n.f1.f0.tokenImage;
	}

	/**
	 * f0 -> "!"
	 * f1 -> Expression()
	 */
	public  String visit(NotExpression n, Object argu) {
		checkIsBoolean(n.f1.accept(this, argu));
		return "BOOLEAN"; 
	}

	/**
	 * f0 -> "("
	 * f1 -> Expression()
	 * f2 -> ")"
	 */
	public  String visit(BracketExpression n, Object argu) {
		return n.f1.accept(this, argu);
	}


}

