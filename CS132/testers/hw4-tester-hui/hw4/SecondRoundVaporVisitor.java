import java.util.TreeSet;

import cs132.vapor.ast.VAssign;
import cs132.vapor.ast.VBranch;
import cs132.vapor.ast.VBuiltIn;
import cs132.vapor.ast.VCall;
import cs132.vapor.ast.VCodeLabel;
import cs132.vapor.ast.VFunction;
import cs132.vapor.ast.VGoto;
import cs132.vapor.ast.VMemRead;
import cs132.vapor.ast.VMemWrite;
import cs132.vapor.ast.VOperand;
import cs132.vapor.ast.VReturn;
import cs132.vapor.ast.VInstr.VisitorPR;
import cs132.vapor.ast.VMemRef.Global;


public class SecondRoundVaporVisitor extends VisitorPR<VisitorInfo,String,Exception>{
	
	@Override
	public String visit(VisitorInfo vi, VAssign arg) throws Exception {
		String pre = "";
		String dest = vi.getCorrespondingVariable(arg.dest.toString());
		String src  = vi.getCorrespondingVariable(arg.source.toString());
		if(dest == null) return "";
		if(src.charAt(0) == 'l' || dest.charAt(0) == 'l'){
			pre += "\t$v1 = " + src + "\n";
			src = "$v1";
		}
		return pre + "\t" + dest + " = " + src + "\n";
	}

	@Override
	public String visit(VisitorInfo vi, VCall arg) throws Exception {
		String result = "";
		
		for(int i = 0; i < arg.args.length && i < 4; i++){
			result += "\t$a" + i + " = " + vi.getCorrespondingVariable(arg.args[i].toString()) + "\n";
		}
		for(int i = 4; i < arg.args.length; i++){
			String src = vi.getCorrespondingVariable(arg.args[i].toString());
			if(src.charAt(0) == 'l')
				result += "\t$v1 = " + src + "\n\tout[" + (i-4) + "] = $v1\n";
			else
				result += "\tout[" + (i-4) + "] = " + src + "\n";
		}
		
		String addr = vi.getCorrespondingVariable(arg.addr.toString());
		if(addr.charAt(0) == 'l')
			result += "\t$v1 = " + addr + "\n\tcall $v1\n";
		else
			result += "\tcall " + addr + "\n"; 
		
		String dest = vi.getCorrespondingVariable(arg.dest.ident);
		if(dest != null){
			result += "\t" + dest + " = $v0\n";
		}
		return result;
	}

	@Override
	public String visit(VisitorInfo vi, VBuiltIn arg) throws Exception {
		String pre = "";
		String last = "";
		String src = "";
		String dest = "";
		switch(arg.op.name){
		case "Add":
		case "Sub":
		case "MulS":
		case "Eq":
		case "Lt":
		case "LtS":
			String arg0 = vi.getCorrespondingVariable(arg.args[0].toString());
			String arg1 = vi.getCorrespondingVariable(arg.args[1].toString());
			dest = vi.getCorrespondingVariable(arg.dest.toString());
			if(arg0.charAt(0) == 'l'){
				pre += "\t$v0 = " + arg0 + "\n";
				arg0  = "$v0";
			}
			if(arg1.charAt(0) == 'l'){
				pre += "\t$v1 = " + arg1 + "\n";
				arg1  = "$v1";
			}
			if(dest.charAt(0) == 'l'){
				last = "\t" + dest + " = $v1\n";
				dest  = "$v1";
			}
			return pre + "\t" + dest + " = " + arg.op.name + "(" + arg0 +  " " + arg1 + ")\n" + last;
		case "PrintIntS":
			src = vi.getCorrespondingVariable(arg.args[0].toString());
			if(src.charAt(0) == 'l')
				return "\t$v1 = " + src +"\n\tPrintIntS($v1)\n";
			else
				return "\tPrintIntS(" + src +  ")\n";
		case "HeapAllocZ":
			src = vi.getCorrespondingVariable(arg.args[0].toString());
			dest = vi.getCorrespondingVariable(arg.dest.toString());
			if(src.charAt(0) == 'l'){
				pre += "\t$v1 = " + src + "\n";
				src = "$v1";
			}
			if(dest.charAt(0) == 'l'){
				last = "\t" + dest + " = $v1\n";
				dest = "$v1";
			}
			return pre + "\t" + dest + " = HeapAllocZ(" + src + ")\n" + last;
		case "Error":
			return "\tError(" + arg.args[0].toString() + ")\n";
		default:
			throw new Error();
		}
	}

	@Override
	public String visit(VisitorInfo vi, VMemWrite arg) throws Exception {
		String pre = "";
		String last = "";
		String dest = vi.getCorrespondingVariable(((Global)arg.dest).base.toString());
		String src  = vi.getCorrespondingVariable(arg.source.toString());
		if(src.charAt(0) == 'l'){
			pre = "\t$v1 = " + src + "\n";
			src = "$v1";
		}
		if(dest.charAt(0) == 'l'){
			last = "\t" + dest + " = $v1\n";
			dest = "$v1";
		}
		return pre + "\t[" + dest + " + " + ((Global) arg.dest).byteOffset + "] = " +  src + "\n" + last;
	}

	@Override
	public String visit(VisitorInfo vi, VMemRead arg) throws Exception {
		String pre = "";
		String last = "";
		String dest = vi.getCorrespondingVariable(arg.dest.toString());
		String src  = vi.getCorrespondingVariable(((Global)arg.source).base.toString());
		if(src.charAt(0) == 'l'){
			pre= "\t$v1 = " + src + "\n";
			src = "$v1";
		}
		if(dest.charAt(0) == 'l'){
			last = "\t" + dest + " = $v1\n";
			dest = "$v1";
		}
		return pre + "\t" + dest + " =  [" + src + " + " + ((Global) arg.source).byteOffset + "]\n" + last;
	}

	@Override
	public String visit(VisitorInfo vi, VBranch arg) throws Exception {
		String pre = "";
		String cond = vi.getCorrespondingVariable(arg.value.toString());
		if(cond.charAt(0) == 'l'){
			pre = "\t$v1 = " + cond + "\n";
			cond = "$v1";
		}
		return pre + "\tif" + (arg.positive ? " " : "0 ") +  cond + " goto :" + arg.target.ident + "\n";
	}

	@Override
	public String visit(VisitorInfo vi, VGoto arg) throws Exception {
		return "\tgoto " + arg.target.toString() + "\n";
	}

	@Override
	public String visit(VisitorInfo vi, VReturn arg) throws Exception {
		return arg.value == null ? "" : "\t$v0 = " + vi.getCorrespondingVariable(arg.value.toString()) + "\n";		
	}
}

