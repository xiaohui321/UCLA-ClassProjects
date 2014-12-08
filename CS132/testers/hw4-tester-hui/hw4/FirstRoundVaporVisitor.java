

import java.util.TreeSet;

import cs132.vapor.ast.*;
import cs132.vapor.ast.VBuiltIn.Op;
import cs132.vapor.ast.VInstr.VisitorPR;
import cs132.vapor.ast.VMemRef.Global;


public class FirstRoundVaporVisitor extends VisitorPR<VisitorInfo,VisitorResult,Exception>{
	VFunction fun;
	
	public FirstRoundVaporVisitor(VFunction fun){
		this.fun = fun;
	}
	
	public Integer getNextLine(int line){
		for(VCodeLabel l : fun.labels){
			if(l.sourcePos.line == line){
				line ++;
			}
		}
		return new Integer(line);
	}
	
	@Override
	public VisitorResult visit(VisitorInfo vi, VAssign arg) throws Exception {
		vi.updateAssignment(arg.dest.toString(),arg.sourcePos.line);
		vi.updateUsage(arg.source.toString(),arg.sourcePos.line);
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line + 1));
		return new VisitorResult(arg.sourcePos.line,set);
	}

	@Override
	public VisitorResult visit(VisitorInfo vi, VCall arg) throws Exception {
		vi.updateUsage(arg.addr.toString(), arg.sourcePos.line);
		if(arg.dest != null)
			vi.updateAssignment(arg.dest.toString(), arg.sourcePos.line);
		
		for(VOperand op : arg.args){
			vi.updateUsage(op.toString(), arg.sourcePos.line);
		}

		vi.updateOutSize(arg.args.length - 4);
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line + 1));
		return new VisitorResult(arg.sourcePos.line,set);
	}

	@Override
	public VisitorResult visit(VisitorInfo vi, VBuiltIn arg) throws Exception {
		switch(arg.op.name){
		case "Add":
		case "Sub":
		case "MulS":
		case "Eq":
		case "Lt":
		case "LtS":
			vi.updateAssignment(arg.dest.toString(),arg.sourcePos.line);
			vi.updateUsage(arg.args[0].toString(),arg.sourcePos.line);
			vi.updateUsage(arg.args[1].toString(),arg.sourcePos.line);
			break;
		case "PrintIntS":
			vi.updateUsage(arg.args[0].toString(),arg.sourcePos.line);
			break;
		case "HeapAllocZ":
			vi.updateAssignment(arg.dest.toString(),arg.sourcePos.line);
			vi.updateUsage(arg.args[0].toString(),arg.sourcePos.line);
		case "Error":
			break;
		default:
			throw new Error();
		}
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line + 1));
		return new VisitorResult(arg.sourcePos.line,set);
	}

	@Override
	public VisitorResult visit(VisitorInfo vi, VMemWrite arg) throws Exception {
		vi.updateUsage(((Global) arg.dest).base.toString(),arg.sourcePos.line);
		vi.updateUsage(arg.source.toString(),arg.sourcePos.line);
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line + 1));
		return new VisitorResult(arg.sourcePos.line,set);
	}

	@Override
	public VisitorResult visit(VisitorInfo vi, VMemRead arg) throws Exception {
		vi.updateUsage(((Global) arg.source).base.toString(),arg.sourcePos.line);
		vi.updateAssignment(arg.dest.toString(),arg.sourcePos.line);
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line + 1));
		return new VisitorResult(arg.sourcePos.line,set);
	}

	@Override
	public VisitorResult visit(VisitorInfo vi, VBranch arg) throws Exception {
		TreeSet<Integer> set = new TreeSet<Integer>();
		for(VCodeLabel l : fun.labels){
			if(arg.target.ident.equals(l.ident)){
				set.add(getNextLine(l.sourcePos.line));
			}
		}
		vi.updateUsage(arg.value.toString(),arg.sourcePos.line);
		return new VisitorResult(arg.sourcePos.line,set);
		
	}

	@Override
	public VisitorResult visit(VisitorInfo vi, VGoto arg) throws Exception {
		TreeSet<Integer> set = new TreeSet<Integer>();
		for(VCodeLabel l : fun.labels){
			if(arg.target.toString().substring(1).equals(l.ident)){
				set.add(getNextLine(l.sourcePos.line));
			}
		}
		return new VisitorResult(arg.sourcePos.line,set);
	}

	@Override
	public VisitorResult visit(VisitorInfo vi, VReturn arg) throws Exception {
		if(arg.value != null){
			vi.updateUsage(arg.value.toString(), arg.sourcePos.line);
		}
		return new VisitorResult(arg.sourcePos.line,new TreeSet<Integer>());
	}
	
	

}
