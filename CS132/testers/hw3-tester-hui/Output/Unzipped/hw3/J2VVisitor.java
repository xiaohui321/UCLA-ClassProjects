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

public class J2VVisitor extends GJDepthFirst<Result, Info> {
	SymbolTable st;
	int var_counter = 0;
	int branch_counter = 0;
	public String getNextVarName(){
		return "v" + var_counter++;
	};
	
	public String getNextBranchName(){
		return "branch" + branch_counter++;
	};
	
	
	public J2VVisitor(SymbolTable symbol_table) {
		st = symbol_table;
	}

	//
	// Auto class visitors--probably don't need to be overridden.
	//
	public Result visit(NodeList n, Info info) {
		Result r = new Result();
		for ( Enumeration<Node> e = n.elements(); e.hasMoreElements(); ) {
			r.mergeResult(e.nextElement().accept(this,info));
		}
		return r;
	}

	public Result visit(NodeListOptional n, Info info) {
		if ( n.present() ) {
			Result r = new Result();
			for ( Enumeration<Node> e = n.elements(); e.hasMoreElements(); ) {
				r.mergeReturnResultAndValue(e.nextElement().accept(this,info));
			}
			return r;
		}
		else
			return new Result();
	}

	public Result visit(NodeOptional n, Info info) {
		if ( n.present() )
			return n.node.accept(this,info);
		else
			return new Result();
	}

	public Result visit(NodeSequence n, Info info) {
		Result r = new Result();
		for ( Enumeration<Node> e = n.elements(); e.hasMoreElements(); ) {
			r.mergeResult(e.nextElement().accept(this,info));
		}
		return r;
	}

	public Result visit(NodeToken n, Info info) { return null; }

	//
	// User-generated visitor methods below
	//
	
	/**
	 * f0 -> FormalParameter()
	 * f1 -> ( FormalParameterRest() )*
	 */
	public Result visit(FormalParameterList n, Info info) {
		return null;
	}

	/**
	 * f0 -> Type()
	 * f1 -> Identifier()
	 */
	public Result visit(FormalParameter n, Info info) {
		return null;
	}

	/**
	 * f0 -> ","
	 * f1 -> FormalParameter()
	 */
	public Result visit(FormalParameterRest n, Info info) {
		return null;
	}

	/**
	 * f0 -> ArrayType()
	 *       | BooleanType()
	 *       | IntegerType()
	 *       | Identifier()
	 */
	public Result visit(Type n, Info info) {
		return n.f0.accept(this, info);
	}

	/**
	 * f0 -> "int"
	 * f1 -> "["
	 * f2 -> "]"
	 */
	public Result visit(ArrayType n, Info info) {
		return null;
	}

	/**
	 * f0 -> "boolean"
	 */
	public Result visit(BooleanType n, Info info) {
		return null;
	}

	/**
	 * f0 -> "int"
	 */
	public Result visit(IntegerType n, Info info) {
		return null;
	}

	/**
	 * f0 -> Block()
	 *       | AssignmentStatement()
	 *       | ArrayAssignmentStatement()
	 *       | IfStatement()
	 *       | WhileStatement()
	 *       | PrintStatement()
	 */
	public Result visit(Statement n, Info info) {
		return n.f0.accept(this, info);
	}

	/**
	 * f0 -> ClassDeclaration()
	 *       | ClassExtendsDeclaration()
	 */
	public Result visit(TypeDeclaration n, Info info) {
		return n.f0.accept(this, info);
	}

	/**
	 * f0 -> "class"
	 * f1 -> Identifier()
	 * f2 -> "{"
	 * f3 -> ( VarDeclaration() )*
	 * f4 -> ( MethodDeclaration() )*
	 * f5 -> "}"
	 */
	public Result visit(ClassDeclaration n, Info info) {
		info.currentClass = n.f1.f0.tokenImage;
		info.currentMethod = null;
		return n.f4.accept(this, info);
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
	public Result visit(ClassExtendsDeclaration n, Info info) {
		info.currentClass = n.f1.f0.tokenImage;
		info.currentMethod = null;
		return n.f6.accept(this, info);
	}

	/**
	 * f0 -> Type()
	 * f1 -> Identifier()
	 * f2 -> ";"
	 */
	public Result visit(VarDeclaration n, Info info) {
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
	public Result visit(Expression n, Info info) {
		return n.f0.accept(this, info);
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
	public Result visit(PrimaryExpression n, Info info) {
		return n.f0.accept(this, info);
	}

	
	/***********************************************/

	/**
	 * f0 -> MainClass()
	 * f1 -> ( TypeDeclaration() )*
	 * f2 -> <EOF>
	 */
	public Result visit(Goal n, Info info) {
		Result r = n.f0.accept(this, info);
		r.mergeResult(n.f1.accept(this, info));
		r.appendResult("\n");
		return r;
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
	public Result visit(MainClass n, Info info) {
		info.currentClass = n.f1.f0.tokenImage;
		info.currentMethod = "main";
		info.incrementIndent();
		Result r = new Result();
		r.appendResult("func Main()\n");
		r.mergeResult(n.f15.accept(this, info));
		r.appendResult(info.getIndent() + "ret\n\n");
		return r;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "+"
	 * f2 -> PrimaryExpression()
	 */
	public Result visit(PlusExpression n, Info info) {
		Result r = new Result();
		Result r1 = n.f0.accept(this, info);
		Result r2 = n.f2.accept(this, info);
		if(r1.hasReturnValue){
			r.mergeResult(r1);
		}
		if(r2.hasReturnValue){
			r.mergeResult(r2);
		}
		
		if(r1.hasReturnValue || r2.hasReturnValue){
			String var = getNextVarName();
			r.appendResult(info.getIndent() + var + " = Add(");
			r.setReturnValue(var);
		}else{
			r.appendResult("Add(");
		}
		
		if(r1.hasReturnValue){
			r.appendResult(r1.returnValue);
		}else{
			r.mergeResult(r1);
		}
		r.appendResult(" ");
		
		if(r2.hasReturnValue){
			r.appendResult(r2.returnValue);
		}else{
			r.mergeResult(r2);
		}
		
		r.appendResult(")\n");
		return r;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "-"
	 * f2 -> PrimaryExpression()
	 */
	public Result visit(MinusExpression n, Info info) {
		Result r = new Result();
		Result r1 = n.f0.accept(this, info);
		Result r2 = n.f2.accept(this, info);
		if(r1.hasReturnValue){
			r.mergeResult(r1);
		}
		if(r2.hasReturnValue){
			r.mergeResult(r2);
		}
		
		if(r1.hasReturnValue || r2.hasReturnValue){
			String var = getNextVarName();
			r.appendResult(info.getIndent() + var + " = Sub(");
			r.setReturnValue(var);
		}else{
			r.appendResult("Sub(");
		}
		
		if(r1.hasReturnValue){
			r.appendResult(r1.returnValue);
		}else{
			r.mergeResult(r1);
		}
		r.appendResult(" ");
		
		if(r2.hasReturnValue){
			r.appendResult(r2.returnValue);
		}else{
			r.mergeResult(r2);
		}
		
		r.appendResult(")\n");
		return r;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "*"
	 * f2 -> PrimaryExpression()
	 */
	public Result visit(TimesExpression n, Info info) {
		Result r = new Result();
		Result r1 = n.f0.accept(this, info);
		Result r2 = n.f2.accept(this, info);
		if(r1.hasReturnValue){
			r.mergeResult(r1);
		}
		if(r2.hasReturnValue){
			r.mergeResult(r2);
		}
		
		if(r1.hasReturnValue || r2.hasReturnValue){
			String var = getNextVarName();
			r.appendResult(info.getIndent() + var + " = MulS(");
			r.setReturnValue(var);
		}else{
			r.appendResult("MulS(");
		}
		
		if(r1.hasReturnValue){
			r.appendResult(r1.returnValue);
		}else{
			r.mergeResult(r1);
		}
		r.appendResult(" ");
		
		if(r2.hasReturnValue){
			r.appendResult(r2.returnValue);
		}else{
			r.mergeResult(r2);
		}
		
		r.appendResult(")\n");
		return r;
	}
	
	/**
	 * f0 -> <INTEGER_LITERAL>
	 */
	public Result visit(IntegerLiteral n, Info info) {
		Result r = new Result();
		r.setReturnValue(n.f0.tokenImage);
		return r;
	}
	
	/**
	 * f0 -> "System.out.println"
	 * f1 -> "("
	 * f2 -> Expression()
	 * f3 -> ")"
	 * f4 -> ";"
	 */
	public Result visit(PrintStatement n, Info info) {
		Result r = new Result();
		Result r1 = n.f2.accept(this, info);
		r.mergeResult(r1);
		r.appendResult(info.getIndent() + "PrintIntS(");
		r.appendResult(r1.returnValue);
		r.appendResult(")\n");
		return r;
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
	public Result visit(MethodDeclaration n, Info info) {
		info.currentMethod = n.f2.f0.tokenImage;
		
		Result r = new Result();
		r.appendResult("func " + info.currentClass + "." + info.currentMethod);
		r.appendResult("(this" + st.getMethodParameterNameInString(info.currentClass, info.currentMethod) + ")\n");
		
		r.mergeResult(n.f8.accept(this, info));
		
		Result r1 = n.f10.accept(this, info);
		r.mergeResult(r1);
		r.appendResult(info.getIndent() + "ret ");
		r.appendResult(r1.returnValue);
		r.appendResult("\n\n");
		return r;
	}
	
	/**
	 * f0 -> Identifier()
	 * f1 -> "="
	 * f2 -> Expression()
	 * f3 -> ";"
	 */
	public Result visit(AssignmentStatement n, Info info) {
		Result r = new Result();
		Result r1 = n.f2.accept(this, info);
		if(r1.hasReturnValue){
			r.mergeResult(r1);
		}
		r.appendResult(info.getIndent() + n.f0.accept(this, info).returnValue + " = ");
		if(r1.hasReturnValue){
			r.appendResult(r1.returnValue);
		}else{
			r.mergeResult(r1);
		}
		r.appendResult("\n");
		return r;
	}
	
	/**
	 * f0 -> "true"
	 */
	public Result visit(TrueLiteral n, Info info) {
		Result r = new Result();
		r.setReturnValue("1");
		return r;
	}

	/**
	 * f0 -> "false"
	 */
	public Result visit(FalseLiteral n, Info info) {
		Result r = new Result();
		r.setReturnValue("0");
		return r;
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
	public Result visit(IfStatement n, Info info) {
		Result r = new Result();
		String branchName = getNextBranchName();
		Result r1 = n.f2.accept(this, info);
		r.mergeResult(r1);
		r.appendResult(info.getIndent() + "if0 " + r1.returnValue + " goto :" + branchName + "_else\n");
		info.incrementIndent();
		r.mergeResult(n.f4.accept(this, info));
		r.appendResult(info.getIndent() + "goto :" + branchName + "_end\n");
		info.decrementIndent();
		r.appendResult(info.getIndent() + branchName + "_else:\n");
		info.incrementIndent();
		r.mergeResult(n.f6.accept(this, info));
		info.decrementIndent(); 
		r.appendResult(info.getIndent() + branchName + "_end:\n");
		return r;
	}
	
	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "&&"
	 * f2 -> PrimaryExpression()
	 */
	public Result visit(AndExpression n, Info info) {
		Result r = new Result();
		Result r1 = n.f0.accept(this, info);
		Result r2 = n.f2.accept(this, info);
		r.mergeResult(r1);
		r.mergeResult(r2);
		String var = getNextVarName();
		r.appendResult(info.getIndent() + var + " = MulS(" + r1.returnValue + " " + r2.returnValue + ")\n");
		r.setReturnValue(var);
		return r;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "<"
	 * f2 -> PrimaryExpression()
	 */
	public Result visit(CompareExpression n, Info info) {
		Result r = new Result();
		Result r1 = n.f0.accept(this, info);
		Result r2 = n.f2.accept(this, info);
		r.mergeResult(r1);
		r.mergeResult(r2);
		String var = getNextVarName();
		r.appendResult(info.getIndent() + var + " = LtS(" + r1.returnValue + " " + r2.returnValue + ")\n");
		r.setReturnValue(var);
		return r;
	}
	
	/**
	 * f0 -> "!"
	 * f1 -> Expression()
	 */
	public Result visit(NotExpression n, Info info) {
		Result r = new Result();
		Result r1 = n.f1.accept(this, info);
		String var = getNextVarName();
		r.mergeResult(r1);
		r.appendResult(info.getIndent() + var +" = Eq( 0 "+ r1.returnValue + "\n");
		r.setReturnValue(var);
		return r;
	}

	/**
	 * f0 -> "("
	 * f1 -> Expression()
	 * f2 -> ")"
	 */
	public Result visit(BracketExpression n, Info info) {
		return n.f1.accept(this, info);
	}

	/**
	 * f0 -> "this"
	 */
	public Result visit(ThisExpression n, Info info) {
		Result r = new Result();
		r.usedClass = info.currentClass;
		r.setReturnValue("this");
		return r;
	}

	/**
	 * f0 -> "while"
	 * f1 -> "("
	 * f2 -> Expression()
	 * f3 -> ")"
	 * f4 -> Statement()
	 */
	public Result visit(WhileStatement n, Info info) {
		Result r = new Result();
		String branchName = getNextBranchName();
		r.appendResult(info.getIndent() + branchName + "_top:\n");
		info.incrementIndent();
		Result r1 = n.f2.accept(this, info);
		r.mergeResult(r1);
		r.appendResult(info.getIndent() + "if0 " + r1.returnValue + " goto :" + branchName + "_end:\n");
		r.mergeResult(n.f4.accept(this, info));
		r.appendResult(info.getIndent() + "goto :" + branchName + "_top:\n");
		info.decrementIndent();
		r.appendResult(info.getIndent() + branchName + "_end:\n");
		return r;
	}
	/////////////////////////////////////////////////

	/**
	 * f0 -> "{"
	 * f1 -> ( Statement() )*
	 * f2 -> "}"
	 */
	public Result visit(Block n, Info info) {
		return n.f1.accept(this, info);
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
	public Result visit(ArrayAssignmentStatement n, Info info) {
		Result r = new Result();
		n.f0.accept(this, info);
		n.f2.accept(this, info);
		n.f5.accept(this, info);
		return r;
		//TODO
	}


	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "["
	 * f2 -> PrimaryExpression()
	 * f3 -> "]"
	 */
	public Result visit(ArrayLookup n, Info info) {
		Result r = new Result();
		Result r1 = n.f0.accept(this, info);
		Result r2 = n.f2.accept(this, info);
		r.mergeResult(r1);
		r.mergeResult(r2);
		String var1 = getNextVarName();
		String var2 = getNextVarName();
		String branch1 = getNextBranchName();
		String branch2 = getNextBranchName();
		r.appendResult(info.getIndent() + var1 + " = " + r1.returnValue + "\n");
		r.appendResult(info.getIndent() + "if " + var1 + " goto :" + branch1 + "_null\n");
		r.appendResult(info.getIndent() + "\tError(\"null pointer\")\n");
		r.appendResult(info.getIndent() + branch1 + "_null:\n");
		r.appendResult(info.getIndent() + var2 + " = [" + var1 + "]\n");
		r.appendResult(info.getIndent() + var2 + " = Lt(" + r2.returnValue + " " + var2 + ")\n");
		r.appendResult(info.getIndent() + "if " + var2 + " goto :" + branch2 + "_null\n");
		r.appendResult(info.getIndent() + "\tError(\"array index out of bounds\")\n");
		r.appendResult(info.getIndent() + branch2 + "_null:\n");
		r.appendResult(info.getIndent() + var2 + " = MulS(" + r2.returnValue + " 4)\n");
		r.appendResult(info.getIndent() + var2 + " = Add(" + var2 + " " + var1 + ")\n");
		r.appendResult(info.getIndent() + var2 + " = [" + var2 + " + 4]\n");
		r.setReturnValue(var2);
		return r;
	}

	/**
	 * f0 -> PrimaryExpression()
	 * f1 -> "."
	 * f2 -> "length"
	 */
	public Result visit(ArrayLength n, Info info) {
		Result r = new Result();
		n.f0.accept(this, info);
		return r;
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
	public Result visit(MessageSend n, Info info) {
		Result r = new Result();
		Result r1 = n.f0.accept(this, info);
		r.mergeResult(r1);
		String var = getNextVarName();
		r.appendResult(info.getIndent() + var + " = [" + r1.returnValue + "]\n");
		if(r1.usedClass == null)
			r1.usedClass = r1.returnValue;
		//TODO debug
		int pos = st.getFunctionPosition(r1.usedClass, n.f2.f0.tokenImage);
		r.appendResult(info.getIndent() + var + " = [" + var + " + " + pos + "]\n");
		Result r2 = n.f4.accept(this, info);
		r.mergeResult(r2);
		String var2 = getNextVarName();
		r.appendResult(info.getIndent() + var2 + " = call " + var + "( " + r1.returnValue  +  " " + r2.returnValue + " )\n");
		r.setReturnValue(var2);
		return r;
	}

	/**
	 * f0 -> Expression()
	 * f1 -> ( ExpressionRest() )*
	 */
	public Result visit(ExpressionList n, Info info) {
		Result r = new Result();
		Result r1 = n.f0.accept(this, info);
		Result r2 = n.f1.accept(this, info);
		r.mergeResult(r1);
		r.mergeResult(r2);
		r.setReturnValue(r1.returnValue + r2.returnValue);
		return r;
	}

	/**
	 * f0 -> ","
	 * f1 -> Expression()
	 */
	public Result visit(ExpressionRest n, Info info) {
		Result r = new Result();
		Result r1 = n.f1.accept(this, info);
		r.mergeResult(r1);
		r.setReturnValue(" " + r1.returnValue);
		return r;
	}

	/**
	 * f0 -> "new"
	 * f1 -> "int"
	 * f2 -> "["
	 * f3 -> Expression()
	 * f4 -> "]"
	 */
	public Result visit(ArrayAllocationExpression n, Info info) {
		Result r = new Result();
		Result r1 = n.f3.accept(this, info);
		r.mergeResult(r1);
		String var = getNextVarName();
		r.appendResult(info.getIndent() + var + " = call :AllocArray(" + r1.returnValue + ")\n");
		r.setReturnValue(var);
		return r;
	}

	/**
	 * f0 -> "new"
	 * f1 -> Identifier()
	 * f2 -> "("
	 * f3 -> ")"
	 */
	public Result visit(AllocationExpression n, Info info) {
		Result r = new Result();
		String className = n.f1.accept(this, info).returnValue;
		int classSize = st.getAllocationSize(className);
		String varName = getNextVarName();
		String branchName = getNextBranchName();
		r.appendResult(info.getIndent() + varName + " = HeapAllocZ("+ classSize +")\n");
		r.appendResult(info.getIndent() + "[" + varName + "] = :vmt_"+ className + "\n");
		r.appendResult(info.getIndent() + "if " + varName + " goto :"+ branchName + "_null\n");
		r.appendResult(info.getIndent() + "\tError(\"null pointer\")\n");
		r.appendResult(info.getIndent() + branchName + "_null:\n");
		r.setReturnValue(varName);
		r.usedClass = className;
		return r;
	}

	/**
	 * f0 -> <IDENTIFIER>
	 */
	public Result visit(Identifier n, Info info) {
		Result r = new Result();
		r.setReturnValue(n.f0.tokenImage);
		return r;
		
	}
	

}
