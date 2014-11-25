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
import visitor.DepthFirstVisitor;


public class SymbolTableConstructionVisitor extends DepthFirstVisitor{
	public SymbolTable st = new SymbolTable();

	public SymbolTable getSymbolTable() {
		return st;
	}

	public String currentClass = null;
	public String currentMethod = null;
	public SymbolTable.TYPES currentType = null;
	public String currentTypeClassName = null;
	public String currentSymbolName = null;

	/**********************************
	 * Visitors
	 */

	/**
	 * f0 -> MainClass()
	 * f1 -> ( TypeDeclaration() )*
	 * f2 -> <EOF>
	 */
	public void visit(Goal n) {
		n.f0.accept(this);
		n.f1.accept(this);
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
	public void visit(MainClass n) {
		//add main class
		currentClass = n.f1.f0.tokenImage;
		st.addClass(currentClass); 

		//add main method
		currentMethod = "main";
		st.addMethod(currentClass, "main", SymbolTable.TYPES.VOID);

		n.f14.accept(this);
		n.f15.accept(this);
	}

	/**
	 * f0 -> ClassDeclaration()
	 *       | ClassExtendsDeclaration()
	 */
	public void visit(TypeDeclaration n) {
		n.f0.accept(this);
	}

	/**
	 * f0 -> "class"
	 * f1 -> Identifier()
	 * f2 -> "{"
	 * f3 -> ( VarDeclaration() )*
	 * f4 -> ( MethodDeclaration() )*
	 * f5 -> "}"
	 */
	public void visit(ClassDeclaration n) {
		//add class
		currentClass = n.f1.f0.tokenImage;
		currentMethod = null;
		st.addClass(currentClass); 

		n.f3.accept(this);
		n.f4.accept(this);
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
	public void visit(ClassExtendsDeclaration n) {
		//add class
		currentClass = n.f1.f0.tokenImage;
		currentMethod = null;
		st.addExtendedClass(currentClass, n.f3.f0.tokenImage); 

		n.f5.accept(this);
		n.f6.accept(this);
	}

	/**
	 * f0 -> Type()
	 * f1 -> Identifier()
	 * f2 -> ";"
	 */
	public void visit(VarDeclaration n) {
		n.f0.accept(this);
		n.f1.accept(this);
		if(currentMethod == null)
			st.addClassVariable(currentClass, currentSymbolName, currentType, currentTypeClassName);
		else
			st.addMethodVariable(currentClass, currentMethod, currentSymbolName, currentType,currentTypeClassName);
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
	public void visit(MethodDeclaration n) {

		n.f1.accept(this);
		n.f2.accept(this);
		currentMethod = currentSymbolName;
		st.addMethod(currentClass,currentSymbolName, currentType, currentTypeClassName);

		n.f4.accept(this);

		n.f7.accept(this);
		n.f8.accept(this);

		n.f10.accept(this);
	}

	/**
	 * f0 -> FormalParameter()
	 * f1 -> ( FormalParameterRest() )*
	 */
	public void visit(FormalParameterList n) {
		n.f0.accept(this);
		n.f1.accept(this);
	}

	/**
	 * f0 -> Type()
	 * f1 -> Identifier()
	 */
	public void visit(FormalParameter n) {
		n.f0.accept(this);
		n.f1.accept(this);
		st.addMethodParameter(currentClass, currentMethod, currentSymbolName, currentType, currentTypeClassName);
	}

	/**
	 * f0 -> ","
	 * f1 -> FormalParameter()
	 */
	public void visit(FormalParameterRest n) {
		n.f1.accept(this);
	}

	/**
	 * f0 -> ArrayType()
	 *       | BooleanType()
	 *       | IntegerType()
	 *       | Identifier()
	 */
	public void visit(Type n) {
		n.f0.accept(this);
		if(n.f0.choice instanceof Identifier){
			currentType = SymbolTable.TYPES.CLASS;
			currentTypeClassName = ((Identifier) n.f0.choice).f0.tokenImage;
		}
	}

	/**
	 * f0 -> "int"
	 * f1 -> "["
	 * f2 -> "]"
	 */
	public void visit(ArrayType n) {
		currentType = SymbolTable.TYPES.INT_ARRAY;
	}

	/**
	 * f0 -> "boolean"
	 */
	public void visit(BooleanType n) {
		currentType = SymbolTable.TYPES.BOOLEAN;
	}

	/**
	 * f0 -> "int"
	 */   
	public void visit(IntegerType n) {
		currentType = SymbolTable.TYPES.INT;
	}


	/**
	 * f0 -> Block()
	 *       | AssignmentStatement()
	 *       | ArrayAssignmentStatement()
	 *       | IfStatement()
	 *       | WhileStatement()
	 *       | PrintStatement()
	 */
	public void visit(Statement n) {
		n.f0.accept(this);
	}

	/**
	 * f0 -> "{"
	 * f1 -> ( Statement() )*
	 * f2 -> "}"
	 */
	public void visit(Block n) {
		n.f1.accept(this);
	}

	/**
	 * f0 -> Identifier()
	 * f1 -> "="
	 * f2 -> Expression()
	 * f3 -> ";"
	 */
	public void visit(AssignmentStatement n) {
		n.f0.accept(this);
		n.f2.accept(this);
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
	public void visit(ArrayAssignmentStatement n) {
		n.f0.accept(this);
		n.f2.accept(this);
		n.f5.accept(this);
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
	public void visit(IfStatement n) {
		n.f2.accept(this);
		n.f4.accept(this);
		n.f6.accept(this);
	}

	/**
	 * f0 -> "while"
	 * f1 -> "("
	 * f2 -> Expression()
	 * f3 -> ")"
	 * f4 -> Statement()
	 */
	public void visit(WhileStatement n) {
		n.f2.accept(this);
		n.f4.accept(this);
	}

	/**
	 * f0 -> "System.out.println"
	 * f1 -> "("
	 * f2 -> Expression()
	 * f3 -> ")"
	 * f4 -> ";"
	 */
	public void visit(PrintStatement n) {
		n.f2.accept(this);
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
	public void visit(Expression n) {
		n.f0.accept(this);
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "&&"
	 * f2 -> PrimaryExpression()
	 */
	public void visit(AndExpression n) {
		n.f0.accept(this);
		n.f2.accept(this);
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "<"
	 * f2 -> PrimaryExpression()
	 */
	public void visit(CompareExpression n) {
		n.f0.accept(this);
		n.f2.accept(this);
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "+"
	 * f2 -> PrimaryExpression()
	 */
	public void visit(PlusExpression n) {
		n.f0.accept(this);
		n.f2.accept(this);
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "-"
	 * f2 -> PrimaryExpression()
	 */
	public void visit(MinusExpression n) {
		n.f0.accept(this);
		n.f2.accept(this);
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "*"
	 * f2 -> PrimaryExpression()
	 */
	public void visit(TimesExpression n) {
		n.f0.accept(this);
		n.f2.accept(this);
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "["
	 * f2 -> PrimaryExpression()
	 * f3 -> "]"
	 */
	public void visit(ArrayLookup n) {
		n.f0.accept(this);
		n.f2.accept(this);
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "."
	 * f2 -> "length"
	 */
	public void visit(ArrayLength n) {
		n.f0.accept(this);
		n.f2.accept(this);
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "."
	 * f2 -> Identifier()
	 * f3 -> "("
	 * f4 -> ( ExpressionList() )?
	 * f5 -> ")"
	 */
	public void visit(MessageSend n) {
		n.f0.accept(this);
		n.f2.accept(this);
		n.f4.accept(this);
	}

	/**
	 * f0 -> Expression()
	 * f1 -> ( ExpressionRest() )*
	 */
	public void visit(ExpressionList n) {
		n.f0.accept(this);
		n.f1.accept(this);
	}

	/**
	 * f0 -> ","
	 * f1 -> Expression()
	 */
	public void visit(ExpressionRest n) {
		n.f1.accept(this);
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
	public void visit(PrimaryExpression n) {
		n.f0.accept(this);
	}

	/**
	 * f0 -> <INTEGER_LITERAL>
	 */
	public void visit(IntegerLiteral n) {
		n.f0.accept(this);
	}

	/**
	 * f0 -> "true"
	 */
	public void visit(TrueLiteral n) {
		n.f0.accept(this);
	}

	/**
	 * f0 -> "false"
	 */
	public void visit(FalseLiteral n) {
		n.f0.accept(this);
	}

	/**
	 * f0 -> <IDENTIFIER>
	 */
	public void visit(Identifier n) {
		n.f0.accept(this);
		currentSymbolName = n.f0.tokenImage;
	}

	/**
	 * f0 -> "this"
	 */
	public void visit(ThisExpression n) {
		n.f0.accept(this);
	}

	/**
	 * f0 -> "new"
	 * f1 -> "int"
	 * f2 -> "["
	 * f3 -> Expression()
	 * f4 -> "]"
	 */
	public void visit(ArrayAllocationExpression n) {
		n.f3.accept(this);
	}

	/**
	 * f0 -> "new"
	 * f1 -> Identifier()
	 * f2 -> "("
	 * f3 -> ")"
	 */
	public void visit(AllocationExpression n) {
		n.f1.accept(this);
	}

	/**
	 * f0 -> "!"
	 * f1 -> Expression()
	 */
	public void visit(NotExpression n) {
		n.f1.accept(this);
	}

	/**
	 * f0 -> "("
	 * f1 -> Expression()
	 * f2 -> ")"
	 */
	public void visit(BracketExpression n) {
		n.f1.accept(this);
	}
}