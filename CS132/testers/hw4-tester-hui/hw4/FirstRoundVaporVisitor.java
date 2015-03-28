import java.util.TreeSet;

import cs132.vapor.ast.*;
import cs132.vapor.ast.VInstr.VisitorPR;
import cs132.vapor.ast.VMemRef.Global;


public class FirstRoundVaporVisitor extends VisitorPR<VisitorInfo,Line,Exception>{
	VFunction fun;
	
	public FirstRoundVaporVisitor(VFunction fun){
		this.fun = fun;
	}
	
	public Integer getNextLine(int line){
		line ++;
		for(VCodeLabel l : fun.labels){
			if(l.sourcePos.line == line){
				line ++;
			}
		}
		return new Integer(line);	
	}
	
	@Override
	public Line visit(VisitorInfo vi, VAssign arg) throws Exception {
		vi.updateAssignment(arg.dest.toString(),arg.sourcePos.line);
		vi.updateUsage(arg.source.toString(),arg.sourcePos.line);
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line));
		return new Line(arg.sourcePos.line,set);
	}

	@Override
	public Line visit(VisitorInfo vi, VCall arg) throws Exception {
		vi.updateUsage(arg.addr.toString(), arg.sourcePos.line);
		
		if(arg.dest != null)
			vi.updateAssignment(arg.dest.toString(), arg.sourcePos.line);
		
		for(VOperand op : arg.args){
			vi.updateUsage(op.toString(), arg.sourcePos.line);
		}

		vi.updateOutSize(arg.args.length - 4);
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line));
		return new Line(arg.sourcePos.line,set);
	}

	@Override
	public Line visit(VisitorInfo vi, VBuiltIn arg) throws Exception {
		if(arg.op.name.equals("Add") || arg.op.name.equals("Sub") || arg.op.name.equals("MulS") 
				|| arg.op.name.equals("Eq") || arg.op.name.equals("Lt") || arg.op.name.equals("LtS") ){
			vi.updateAssignment(arg.dest.toString(),arg.sourcePos.line);
			vi.updateUsage(arg.args[0].toString(),arg.sourcePos.line);
			vi.updateUsage(arg.args[1].toString(),arg.sourcePos.line);
		}else if(arg.op.name.equals("PrintIntS")){
			vi.updateUsage(arg.args[0].toString(),arg.sourcePos.line);
		}else if(arg.op.name.equals("HeapAllocZ")){
			vi.updateAssignment(arg.dest.toString(),arg.sourcePos.line);
			vi.updateUsage(arg.args[0].toString(),arg.sourcePos.line);
		}
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line));
		return new Line(arg.sourcePos.line,set);
	}

	@Override
	public Line visit(VisitorInfo vi, VMemWrite arg) throws Exception {
		vi.updateUsage(((Global) arg.dest).base.toString(),arg.sourcePos.line);
		vi.updateUsage(arg.source.toString(),arg.sourcePos.line);
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line));
		return new Line(arg.sourcePos.line,set);
	}

	@Override
	public Line visit(VisitorInfo vi, VMemRead arg) throws Exception {
		vi.updateUsage(((Global) arg.source).base.toString(),arg.sourcePos.line);
		vi.updateAssignment(arg.dest.toString(),arg.sourcePos.line);
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line));
		return new Line(arg.sourcePos.line,set);
	}

	@Override
	public Line visit(VisitorInfo vi, VBranch arg) throws Exception {
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(getNextLine(arg.sourcePos.line));
		for(VCodeLabel label : fun.labels){
			if(arg.target.ident.equals(label.ident)){
				set.add(getNextLine(label.sourcePos.line));
			}
		}
		vi.updateUsage(arg.value.toString(),arg.sourcePos.line);
		return new Line(arg.sourcePos.line,set);		
	}

	@Override
	public Line visit(VisitorInfo vi, VGoto arg) throws Exception {
		TreeSet<Integer> set = new TreeSet<Integer>();
		for(VCodeLabel label : fun.labels){
			if(arg.target.toString().substring(1).equals(label.ident)){
				set.add(getNextLine(label.sourcePos.line));
			}
		}
		return new Line(arg.sourcePos.line,set);
	}

	@Override
	public Line visit(VisitorInfo vi, VReturn arg) throws Exception {
		if(arg.value != null){
			vi.updateUsage(arg.value.toString(), arg.sourcePos.line);
		}
		return new Line(arg.sourcePos.line,new TreeSet<Integer>());
	}
}
