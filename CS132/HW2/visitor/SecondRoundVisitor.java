package visitor;

import analysis.SymbolTable;

import java.util.Enumeration;

import syntaxtree.*;

public class SecondRoundVisitor extends GJDepthFirst<SymbolTable.Type,Object> {
	public SymbolTable st;
	public String currentClass = "";
	public String currentMethod = "";
	public SymbolTable.Type currentType = null;
	//	public SymbolTable.Type expressionType = null;
	public String currentName = "";

	public SecondRoundVisitor(SymbolTable t){
		st = t;
	};

	/**********************************
	 * Helpers
	 */
	public void checkCurrentType_INT(){
		if( st.getSymbolType(currentClass, currentMethod, currentName)!= SymbolTable.Type.INT)
			throw new Error(currentName + " is not of type INT in class " + currentClass + " method " + currentMethod);
	}

	public void checkCurrentType_INT_ARRAY(){
		if(st.getSymbolType(currentClass, currentMethod, currentName) != SymbolTable.Type.INT_ARRAY)
			throw new Error(currentName + " is not of type INT_ARRAY in class " + currentClass + " method " + currentMethod);
	}


	/**********************************
	 * Visitors
	 */

	public  SymbolTable.Type visit(NodeList n, Object argu) {
		SymbolTable.Type _ret=null;
		int _count=0;
		for ( Enumeration<Node> e = n.elements(); e.hasMoreElements(); ) {
			e.nextElement().accept(this,argu);
			_count++;
		}
		return _ret;
	}

	public  SymbolTable.Type visit(NodeListOptional n, Object argu) {
		if ( n.present() ) {
			SymbolTable.Type _ret=null;
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

	public  SymbolTable.Type visit(NodeOptional n, Object argu) {
		if ( n.present() )
			return n.node.accept(this,argu);
		else
			return null;
	}

	public  SymbolTable.Type visit(NodeSequence n, Object argu) {
		SymbolTable.Type _ret=null;
		int _count=0;
		for ( Enumeration<Node> e = n.elements(); e.hasMoreElements(); ) {
			e.nextElement().accept(this,argu);
			_count++;
		}
		return _ret;
	}

	public  SymbolTable.Type visit(NodeToken n, Object argu) { return null; }

	//
	// User-generated visitor methods below
	//

	/**
	 * f0 -> MainClass()
	 * f1 -> ( SymbolTable.TypeDeclaration() )*
	 * f2 -> <EOF>
	 */
	 public  SymbolTable.Type visit(Goal n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
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
	 public  SymbolTable.Type visit(MainClass n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 n.f6.accept(this, argu);
		 n.f7.accept(this, argu);
		 n.f8.accept(this, argu);
		 n.f9.accept(this, argu);
		 n.f10.accept(this, argu);
		 n.f11.accept(this, argu);
		 n.f12.accept(this, argu);
		 n.f13.accept(this, argu);
		 n.f14.accept(this, argu);
		 n.f15.accept(this, argu);
		 n.f16.accept(this, argu);
		 n.f17.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> ClassDeclaration()
	  *       | ClassExtendsDeclaration()
	  */
	 public  SymbolTable.Type visit(TypeDeclaration n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "class"
	  * f1 -> Identifier()
	  * f2 -> "{"
	  * f3 -> ( VarDeclaration() )*
	  * f4 -> ( MethodDeclaration() )*
	  * f5 -> "}"
	  */
	 public  SymbolTable.Type visit(ClassDeclaration n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 return _ret;
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
	 public  SymbolTable.Type visit(ClassExtendsDeclaration n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 n.f6.accept(this, argu);
		 n.f7.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> SymbolTable.Type()
	  * f1 -> Identifier()
	  * f2 -> ";"
	  */
	 public  SymbolTable.Type visit(VarDeclaration n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "public"
	  * f1 -> SymbolTable.Type()
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
	 public  SymbolTable.Type visit(MethodDeclaration n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 n.f6.accept(this, argu);
		 n.f7.accept(this, argu);
		 n.f8.accept(this, argu);
		 n.f9.accept(this, argu);
		 n.f10.accept(this, argu);
		 n.f11.accept(this, argu);
		 n.f12.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> FormalParameter()
	  * f1 -> ( FormalParameterRest() )*
	  */
	 public  SymbolTable.Type visit(FormalParameterList n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> SymbolTable.Type()
	  * f1 -> Identifier()
	  */
	 public  SymbolTable.Type visit(FormalParameter n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> ","
	  * f1 -> FormalParameter()
	  */
	 public  SymbolTable.Type visit(FormalParameterRest n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> ArraySymbolTable.Type()
	  *       | BooleanSymbolTable.Type()
	  *       | IntegerSymbolTable.Type()
	  *       | Identifier()
	  */
	 public  SymbolTable.Type visit(Type n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "int"
	  * f1 -> "["
	  * f2 -> "]"
	  */
	 public  SymbolTable.Type visit(ArrayType n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "boolean"
	  */
	 public  SymbolTable.Type visit(BooleanType n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "int"
	  */
	 public  SymbolTable.Type visit(IntegerType n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> Block()
	  *       | AssignmentStatement()
	  *       | ArrayAssignmentStatement()
	  *       | IfStatement()
	  *       | WhileStatement()
	  *       | PrintStatement()
	  */
	 public  SymbolTable.Type visit(Statement n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "{"
	  * f1 -> ( Statement() )*
	  * f2 -> "}"
	  */
	 public  SymbolTable.Type visit(Block n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> Identifier()
	  * f1 -> "="
	  * f2 -> Expression()
	  * f3 -> ";"
	  */
	 public  SymbolTable.Type visit(AssignmentStatement n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 return _ret;
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
	 public  SymbolTable.Type visit(ArrayAssignmentStatement n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 n.f6.accept(this, argu);
		 return _ret;
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
	 public  SymbolTable.Type visit(IfStatement n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 n.f6.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "while"
	  * f1 -> "("
	  * f2 -> Expression()
	  * f3 -> ")"
	  * f4 -> Statement()
	  */
	 public  SymbolTable.Type visit(WhileStatement n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "System.out.println"
	  * f1 -> "("
	  * f2 -> Expression()
	  * f3 -> ")"
	  * f4 -> ";"
	  */
	 public  SymbolTable.Type visit(PrintStatement n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 return _ret;
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
	 public  SymbolTable.Type visit(Expression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "&&"
	  * f2 -> PrimaryExpression()
	  */
	 public  SymbolTable.Type visit(AndExpression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "<"
	  * f2 -> PrimaryExpression()
	  */
	 public  SymbolTable.Type visit(CompareExpression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "+"
	  * f2 -> PrimaryExpression()
	  */
	 public  SymbolTable.Type visit(PlusExpression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "-"
	  * f2 -> PrimaryExpression()
	  */
	 public  SymbolTable.Type visit(MinusExpression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "*"
	  * f2 -> PrimaryExpression()
	  */
	 public  SymbolTable.Type visit(TimesExpression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "["
	  * f2 -> PrimaryExpression()
	  * f3 -> "]"
	  */
	 public  SymbolTable.Type visit(ArrayLookup n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "."
	  * f2 -> "length"
	  */
	 public  SymbolTable.Type visit(ArrayLength n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "."
	  * f2 -> Identifier()
	  * f3 -> "("
	  * f4 -> ( ExpressionList() )?
	  * f5 -> ")"
	  */
	 public  SymbolTable.Type visit(MessageSend n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> Expression()
	  * f1 -> ( ExpressionRest() )*
	  */
	 public  SymbolTable.Type visit(ExpressionList n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> ","
	  * f1 -> Expression()
	  */
	 public  SymbolTable.Type visit(ExpressionRest n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
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
	 public  SymbolTable.Type visit(PrimaryExpression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> <INTEGER_LITERAL>
	  */
	 public  SymbolTable.Type visit(IntegerLiteral n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "true"
	  */
	 public  SymbolTable.Type visit(TrueLiteral n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "false"
	  */
	 public  SymbolTable.Type visit(FalseLiteral n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> <IDENTIFIER>
	  */
	 public  SymbolTable.Type visit(Identifier n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "this"
	  */
	 public  SymbolTable.Type visit(ThisExpression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "new"
	  * f1 -> "int"
	  * f2 -> "["
	  * f3 -> Expression()
	  * f4 -> "]"
	  */
	 public  SymbolTable.Type visit(ArrayAllocationExpression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "new"
	  * f1 -> Identifier()
	  * f2 -> "("
	  * f3 -> ")"
	  */
	 public  SymbolTable.Type visit(AllocationExpression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "!"
	  * f1 -> Expression()
	  */
	 public  SymbolTable.Type visit(NotExpression n, Object argu) {
		 SymbolTable.Type _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
	 }

	 /**
	  * f0 -> "("
	  * f1 -> Expression()
	  * f2 -> ")"
	  */
	 public  SymbolTable.Type visit(BracketExpression n, Object argu) {
		 return n.f1.accept(this, argu);
	 }




}

