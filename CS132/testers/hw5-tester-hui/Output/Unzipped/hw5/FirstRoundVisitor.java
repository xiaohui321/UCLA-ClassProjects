import cs132.vapor.ast.VAssign;
import cs132.vapor.ast.VBranch;
import cs132.vapor.ast.VBuiltIn;
import cs132.vapor.ast.VBuiltIn.Op;
import cs132.vapor.ast.VInstr.VisitorPR;
import cs132.vapor.ast.VCall;
import cs132.vapor.ast.VCodeLabel;
import cs132.vapor.ast.VDataSegment;
import cs132.vapor.ast.VFunction;
import cs132.vapor.ast.VGoto;
import cs132.vapor.ast.VInstr;
import cs132.vapor.ast.VMemRead;
import cs132.vapor.ast.VMemRef;
import cs132.vapor.ast.VMemRef.Global;
import cs132.vapor.ast.VMemWrite;
import cs132.vapor.ast.VOperand;
import cs132.vapor.ast.VReturn;
import cs132.vapor.ast.VaporProgram;

public class FirstRoundVisitor extends VisitorPR<VisitorInfo,String,Exception>{
	public static String generateMIPSCode(VaporProgram vp) throws Exception {
		VisitorInfo vi = new VisitorInfo();
		String result = ".data\n\n";

		for(VDataSegment segment : vp.dataSegments) {
			result += segment.ident + ":\n";
			for(VOperand.Static op : segment.values){
				result += "\t" + op.toString().substring(1) + "\n";
			}
			result += "\n";
		}
		result += "\n.text\n" 
				+ "\tjal Main\n"
				+ "\tli $v0 10\n" 
				+ "\tsyscall\n\n";
		
		for(VFunction function : vp.functions){
			FirstRoundVisitor visitor = new FirstRoundVisitor(function);
			result += function.ident + ":\n"
					+ "\tsw $fp -8($sp)\n"
					+ "\tmove $fp $sp\n"
					+ "\tsubu $sp $sp " + ((function.stack.local + function.stack.out + 2) * 4) + "\n"
					+ "\tsw $ra -4($fp)\n";
			
			for(VInstr instr : function.body){
				result += instr.accept(vi,visitor);
				result += visitor.printLabel();
			}
			
			result += "\taddu $sp $sp " + ((function.stack.local + function.stack.out + 2) * 4) + "\n"
					+ "\tjr $ra\n\n";
		}
		result += vi.getFinalPart();
		return result;
	}
	
	VFunction fun;
	int index = -1;
	
	public FirstRoundVisitor(VFunction fun){
		this.fun = fun;
		printLabel();
	}
	
	public String printLabel(){
		index++;
		String result = "";
		for(VCodeLabel label : fun.labels){
			if(label.instrIndex == index){
				result += label.ident + ":\n";
			}
		}
		return result;
	}
	
	@Override
	public String visit(VisitorInfo vi, VAssign arg) throws Exception {
		if(arg.source.toString().charAt(0) == '$'){
			return "\tmove " + arg.dest.toString() + " " + arg.source.toString() + "\n";
		}else if(arg.source.toString().charAt(0) == ':'){
			return "\tla " + arg.dest.toString() + " " + arg.source.toString().substring(1) + "\n";
		}else{
			return "\tli " + arg.dest.toString() + " " + arg.source.toString() + "\n";
		}
	}

	@Override
	public String visit(VisitorInfo vi, VCall arg) throws Exception {
		if(arg.addr.toString().charAt(0) == ':')
			return "\tjal " + arg.addr.toString().substring(1) + "\n";
		else
			return "\tjalr " + arg.addr.toString() + "\n";
	}

	@Override
	public String visit(VisitorInfo vi, VBuiltIn arg)
			throws Exception {
		String result = "";
		if(arg.op == Op.Add){
			if(arg.args[0].toString().charAt(0) == '$'){
				result += "\taddu " + arg.dest.toString() + " " + arg.args[0] + " " + arg.args[1] + "\n";
			}else{
				if(arg.args[1].toString().charAt(0) == '$'){
					result += "\tli $t9 " + arg.args[0] + "\n"
							+ "\taddu "+ arg.dest.toString() + " $t9 " + arg.args[1] + "\n";
				}else{
					result += "\tli " + arg.dest.toString() + " " 
							+  (Integer.parseInt(arg.args[0].toString()) + Integer.parseInt(arg.args[1].toString())) + "\n";
				}
			}
			
		}else if(arg.op == Op.Sub){
			if(arg.args[0].toString().charAt(0) == '$'){
				result += "\tsubu " + arg.dest.toString() + " " + arg.args[0] + " " + arg.args[1] + "\n";
			}else{
				if(arg.args[1].toString().charAt(0) == '$'){
					result += "\tli $t9 " + arg.args[0] + "\n"
							+ "\tsubu "+ arg.dest.toString() + " $t9 " + arg.args[1] + "\n";
				}else{
					result += "\tli " + arg.dest.toString() + " " 
							+  (Integer.parseInt(arg.args[0].toString()) - Integer.parseInt(arg.args[1].toString())) + "\n";
				}
			}
			
		}else if(arg.op == Op.MulS){
			if(arg.args[0].toString().charAt(0) == '$'){
				result += "\tmul " + arg.dest.toString() + " " + arg.args[0] + " " + arg.args[1] + "\n";
			}else{
				if(arg.args[1].toString().charAt(0) == '$'){
					result += "\tmul "+ arg.dest.toString() + " " + arg.args[1] + " " + arg.args[0] + "\n";
				}else{
					result += "\tli " + arg.dest.toString() + " " 
							+  (Integer.parseInt(arg.args[0].toString()) * Integer.parseInt(arg.args[1].toString())) + "\n";
				}
			}
			
		}else if(arg.op == Op.Lt){
			if(arg.args[0].toString().charAt(0) == '$'){
				result += "\tsltu " + arg.dest.toString() + " " + arg.args[0] + " " + arg.args[1] + "\n";
 			}else{
				result += "\tli $t9 " + arg.args[0] + "\n"
						+ "\tsltu " + arg.dest.toString() + " $t9 " + arg.args[1] + "\n";
			}
			
		}else if(arg.op == Op.LtS){
			if(arg.args[0].toString().charAt(0) == '$' && arg.args[1].toString().charAt(0) == '$'){
				result += "\tslt " + arg.dest.toString() + " " + arg.args[0] + " " + arg.args[1] + "\n";
			}else{
				result += "\tslti " + arg.dest.toString() + " " + arg.args[0] + " " + arg.args[1] + "\n";
			}
		}else if(arg.op == Op.PrintIntS){
			vi.print = true;
			
			if(arg.args[0].toString().charAt(0) == '$')
				result += "\tmove $a0 " + arg.args[0] +"\n";
			else
				result += "\tli $a0 " + arg.args[0] + "\n";
			
			result += "\tjal _print\n";
			
		}else if(arg.op == Op.HeapAllocZ){
			vi.heapAlloc = true;
			
			if(arg.dest != null) vi.print = true;
			
			if(arg.args[0].toString().charAt(0) == '$')
				result += "\tmove $a0 " + arg.args[0] + "\n";
			else
				result += "\tli $a0 " + arg.args[0] +"\n";
			
			result += "\tjal _heapAlloc\n"
					+ "\tmove " + arg.dest.toString() + " $v0 \n";
		
		}else if(arg.op == Op.Error){
			vi.error = true;
			
			String name = arg.args[0].toString();
			name = name.substring(0,name.length() - 1) + "\\n\"";
			
			if(vi.table.containsKey(name)) name = vi.table.get(name);
			else{
				name = vi.addNewString(name);
			}
			result += "\tla $a0 "+ name +"\n"
					+ "\tj _error\n";
		}
		return result;
	}

	@Override
	public String visit(VisitorInfo vi, VMemWrite arg)
			throws Exception {
		String result = "";
		
		if(arg.dest instanceof VMemRef.Global){
			VMemRef.Global dest = (VMemRef.Global)arg.dest;
			if(arg.source.toString().charAt(0) == ':'){
				result += "\tla $t9 " + arg.source.toString().substring(1) + "\n"
						+ "\tsw $t9 " + dest.byteOffset + "(" + dest.base.toString() +")\n";
			}else if(arg.source.toString().charAt(0) == '$'){
				result += "\tsw " + arg.source.toString() + " " + dest.byteOffset + "(" + dest.base.toString() + ")\n";
			}else{
				if(Integer.parseInt(arg.source.toString()) == 0){
					result += "\tsw $" + arg.source.toString() + " " + dest.byteOffset + "(" + dest.base.toString() + ")\n";
				}else{
					result += "\tli $t9 " + arg.source.toString() + "\n" 
							+ "\tsw $t9 " + dest.byteOffset + "(" + dest.base.toString() +")\n";
				}
			}
		}else if (arg.dest instanceof VMemRef.Stack){
			VMemRef.Stack dest = (VMemRef.Stack)arg.dest;
			if(arg.source.toString().charAt(0) == '$'){
				String reg = ( (dest.region == VMemRef.Stack.Region.Local ) 
						|| (dest.region == VMemRef.Stack.Region.Out) ) ? "$sp" : "$fp";
				result += "\tsw " + arg.source.toString() + " " + 4 * dest.index + "(" + reg + ")\n";
			}else{
				String reg = ( (dest.region == VMemRef.Stack.Region.In ) 
						|| (dest.region == VMemRef.Stack.Region.Out) ) ? "$sp" : "$fp";
				result += "\tli $t9 " + arg.source.toString() + "\n"
						+ "\tsw $t9 " + 4 * dest.index + "(" + reg + ")\n";
			}
		}
		
		return result;
	}
	
	@Override
	public String visit(VisitorInfo vi, VMemRead arg) throws Exception {
		if(arg.source instanceof Global){
			return "\tlw " + arg.dest.toString() + " " + ((Global)arg.source).byteOffset 
					+ "(" + ((Global)arg.source).base.toString() + ")\n";
		}else{
			return "\tlw " + arg.dest.toString() + " " + (((VMemRef.Stack)arg.source).index * 4) 
					+ "(" + ((((VMemRef.Stack)arg.source).region == VMemRef.Stack.Region.Local) ? "$sp" : "$fp") + ")\n";
		}
	}

	@Override
	public String visit(VisitorInfo vi, VBranch arg) throws Exception {
		return "\t" + (arg.positive ? "bnez " : "beqz ") + arg.value.toString() + " " 
				+ arg.target.toString().substring(1) + "\n"; 
	}

	@Override
	public String visit(VisitorInfo vi, VGoto arg) throws Exception {
		return "\tj " + arg.target.toString().substring(1) + "\n"; 
	}

	@Override
	public String visit(VisitorInfo vi, VReturn arg) throws Exception {
		return "\tlw $ra -4($fp)\n"
			+ "\tlw $fp -8($fp)\n"; 
	}
}