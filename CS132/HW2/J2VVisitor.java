import java.util.Enumeration;

import syntaxtree.AllocationExpression;
import syntaxtree.AndExpression;
import syntaxtree.ArrayAllocationExpression;
import syntaxtree.ArrayAssignmentStatement;
import syntaxtree.ArrayLength;
import syntaxtree.ArrayLookup;
import syntaxtree.ArrayType;
import syntaxtree.AssignmentStatement;
import syntaxtree.Block;
import syntaxtree.BooleanType;
import syntaxtree.BracketExpression;
import syntaxtree.ClassDeclaration;
import syntaxtree.ClassExtendsDeclaration;
import syntaxtree.CompareExpression;
import syntaxtree.Expression;
import syntaxtree.ExpressionList;
import syntaxtree.ExpressionRest;
import syntaxtree.FalseLiteral;
import syntaxtree.FormalParameter;
import syntaxtree.FormalParameterList;
import syntaxtree.FormalParameterRest;
import syntaxtree.Goal;
import syntaxtree.Identifier;
import syntaxtree.IfStatement;
import syntaxtree.IntegerLiteral;
import syntaxtree.IntegerType;
import syntaxtree.MainClass;
import syntaxtree.MessageSend;
import syntaxtree.MethodDeclaration;
import syntaxtree.MinusExpression;
import syntaxtree.Node;
import syntaxtree.NodeList;
import syntaxtree.NodeListOptional;
import syntaxtree.NodeOptional;
import syntaxtree.NodeSequence;
import syntaxtree.NodeToken;
import syntaxtree.NotExpression;
import syntaxtree.PlusExpression;
import syntaxtree.PrimaryExpression;
import syntaxtree.PrintStatement;
import syntaxtree.Statement;
import syntaxtree.ThisExpression;
import syntaxtree.TimesExpression;
import syntaxtree.TrueLiteral;
import syntaxtree.Type;
import syntaxtree.TypeDeclaration;
import syntaxtree.VarDeclaration;
import syntaxtree.WhileStatement;
import visitor.GJDepthFirst;

public class J2VVisitor extends GJDepthFirst<String, Object> {
	SymbolTable st;
	public J2VVisitor(SymbolTable symbol_table) {
		st = symbol_table;
	}

	//
	// Auto class visitors--probably don't need to be overridden.
	//
	public String visit(NodeList n, Object argu) {
		String _ret=null;
		int _count=0;
		for ( Enumeration<Node> e = n.elements(); e.hasMoreElements(); ) {
			e.nextElement().accept(this,argu);
			_count++;
		}
		return _ret;
	}

	public String visit(NodeListOptional n, Object argu) {
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

	public String visit(NodeOptional n, Object argu) {
		if ( n.present() )
			return n.node.accept(this,argu);
		else
			return null;
	}

	public String visit(NodeSequence n, Object argu) {
		String _ret=null;
		int _count=0;
		for ( Enumeration<Node> e = n.elements(); e.hasMoreElements(); ) {
			e.nextElement().accept(this,argu);
			_count++;
		}
		return _ret;
	}

	public String visit(NodeToken n, Object argu) { return null; }

	//
	// User-generated visitor methods below
	//

	/**
	 * f0 -> MainClass()
	 * f1 -> ( TypeDeclaration() )*
	 * f2 -> <EOF>
	 */
	 public String visit(Goal n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
		 //TODO
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
	 public String visit(MainClass n, Object argu) {
		 String _ret=null;
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
		 //TODO
	 }

	 /**
	  * f0 -> ClassDeclaration()
	  *       | ClassExtendsDeclaration()
	  */
	 public String visit(TypeDeclaration n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "class"
	  * f1 -> Identifier()
	  * f2 -> "{"
	  * f3 -> ( VarDeclaration() )*
	  * f4 -> ( MethodDeclaration() )*
	  * f5 -> "}"
	  */
	 public String visit(ClassDeclaration n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 return _ret;
		 //TODO
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
	 public String visit(ClassExtendsDeclaration n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 n.f6.accept(this, argu);
		 n.f7.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> Type()
	  * f1 -> Identifier()
	  * f2 -> ";"
	  */
	 public String visit(VarDeclaration n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
		 //TODO
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
	 public String visit(MethodDeclaration n, Object argu) {
		 String _ret=null;
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
		 //TODO
	 }

	 /**
	  * f0 -> FormalParameter()
	  * f1 -> ( FormalParameterRest() )*
	  */
	 public String visit(FormalParameterList n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> Type()
	  * f1 -> Identifier()
	  */
	 public String visit(FormalParameter n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> ","
	  * f1 -> FormalParameter()
	  */
	 public String visit(FormalParameterRest n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> ArrayType()
	  *       | BooleanType()
	  *       | IntegerType()
	  *       | Identifier()
	  */
	 public String visit(Type n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "int"
	  * f1 -> "["
	  * f2 -> "]"
	  */
	 public String visit(ArrayType n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "boolean"
	  */
	 public String visit(BooleanType n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "int"
	  */
	 public String visit(IntegerType n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> Block()
	  *       | AssignmentStatement()
	  *       | ArrayAssignmentStatement()
	  *       | IfStatement()
	  *       | WhileStatement()
	  *       | PrintStatement()
	  */
	 public String visit(Statement n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "{"
	  * f1 -> ( Statement() )*
	  * f2 -> "}"
	  */
	 public String visit(Block n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> Identifier()
	  * f1 -> "="
	  * f2 -> Expression()
	  * f3 -> ";"
	  */
	 public String visit(AssignmentStatement n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 return _ret;
		 //TODO
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
	 public String visit(ArrayAssignmentStatement n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 n.f6.accept(this, argu);
		 return _ret;
		 //TODO
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
	 public String visit(IfStatement n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 n.f6.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "while"
	  * f1 -> "("
	  * f2 -> Expression()
	  * f3 -> ")"
	  * f4 -> Statement()
	  */
	 public String visit(WhileStatement n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "System.out.println"
	  * f1 -> "("
	  * f2 -> Expression()
	  * f3 -> ")"
	  * f4 -> ";"
	  */
	 public String visit(PrintStatement n, Object argu) {
		 return "PrintIntS("
				 + n.f2.accept(this, argu)
				 + ")\n";
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
	 public String visit(Expression n, Object argu) {
		 return n.f0.accept(this, argu);
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "&&"
	  * f2 -> PrimaryExpression()
	  */
	 public String visit(AndExpression n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "<"
	  * f2 -> PrimaryExpression()
	  */
	 public String visit(CompareExpression n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "+"
	  * f2 -> PrimaryExpression()
	  */
	 public String visit(PlusExpression n, Object argu) {
		 return "Add(" 
				 + n.f0.accept(this, argu)
				 + " "
				 + n.f2.accept(this, argu)
				 + ")";
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "-"
	  * f2 -> PrimaryExpression()
	  */
	 public String visit(MinusExpression n, Object argu) {
		 return "Sub(" 
				 + n.f0.accept(this, argu)
				 + " "
				 + n.f2.accept(this, argu)
				 + ")";
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "*"
	  * f2 -> PrimaryExpression()
	  */
	 public String visit(TimesExpression n, Object argu) {
		 return "Muls(" 
				 + n.f0.accept(this, argu)
				 + " "
				 + n.f2.accept(this, argu)
				 + ")";
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "["
	  * f2 -> PrimaryExpression()
	  * f3 -> "]"
	  */
	 public String visit(ArrayLookup n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "."
	  * f2 -> "length"
	  */
	 public String visit(ArrayLength n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> PrimaryExpression()
	  * f1 -> "."
	  * f2 -> Identifier()
	  * f3 -> "("
	  * f4 -> ( ExpressionList() )?
	  * f5 -> ")"
	  */
	 public String visit(MessageSend n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 n.f5.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> Expression()
	  * f1 -> ( ExpressionRest() )*
	  */
	 public String visit(ExpressionList n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> ","
	  * f1 -> Expression()
	  */
	 public String visit(ExpressionRest n, Object argu) {
		 //TODO
		 String _ret=null;
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
	 public String visit(PrimaryExpression n, Object argu) {
		 return n.f0.accept(this, argu);
	 }

	 /**
	  * f0 -> <INTEGER_LITERAL>
	  */
	 public String visit(IntegerLiteral n, Object argu) {
		 return n.f0.tokenImage;
	 }

	 /**
	  * f0 -> "true"
	  */
	 public String visit(TrueLiteral n, Object argu) {
		 return "1";
	 }

	 /**
	  * f0 -> "false"
	  */
	 public String visit(FalseLiteral n, Object argu) {
		 return "0";
	 }

	 /**
	  * f0 -> <IDENTIFIER>
	  */
	 public String visit(Identifier n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "this"
	  */
	 public String visit(ThisExpression n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "new"
	  * f1 -> "int"
	  * f2 -> "["
	  * f3 -> Expression()
	  * f4 -> "]"
	  */
	 public String visit(ArrayAllocationExpression n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 n.f3.accept(this, argu);
		 n.f4.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "new"
	  * f1 -> Identifier()
	  * f2 -> "("
	  * f3 -> ")"
	  */
	 public String visit(AllocationExpression n, Object argu) {
		 String _ret=null;
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
	 public String visit(NotExpression n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 return _ret;
		 //TODO
	 }

	 /**
	  * f0 -> "("
	  * f1 -> Expression()
	  * f2 -> ")"
	  */
	 public String visit(BracketExpression n, Object argu) {
		 String _ret=null;
		 n.f0.accept(this, argu);
		 n.f1.accept(this, argu);
		 n.f2.accept(this, argu);
		 return _ret;
		 //TODO
	 }

}
