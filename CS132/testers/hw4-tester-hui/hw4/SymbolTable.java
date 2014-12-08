import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.List;
import java.util.TreeSet;

import cs132.vapor.ast.VFunction;
import cs132.vapor.ast.VInstr;
import cs132.vapor.ast.VVarRef.Local;


public class SymbolTable {
	String result;
	
	public SymbolTable(VFunction fun) throws Exception{
		result = "";
		VisitorInfo vi = new VisitorInfo(fun);
		FirstRoundVaporVisitor visitor1 = new FirstRoundVaporVisitor(fun);
		SecondRoundVaporVisitor visitor2 = new SecondRoundVaporVisitor();
		Hashtable<Integer, VisitorResult> visitorResultTable = new Hashtable<Integer, VisitorResult>();
		Hashtable<String, Variable> variableTable = new Hashtable<String,Variable>();
		
		int stackInSize = (fun.params.length - 4)  > 0 ? (fun.params.length - 4) : 0;
		
		//add params
		for(Local param : fun.params) {
			TreeSet<Integer> ts = new TreeSet<Integer>();
			ts.add(fun.sourcePos.line);
			vi.symbolAssignmentTable.put(param.ident, ts);
		}
		
		//process results from first round visitor
		for(VInstr instr : fun.body){
			VisitorResult vr = instr.accept(vi,visitor1);
			visitorResultTable.put(vr.line, vr);
		}
	
		for(VisitorResult vr : visitorResultTable.values()){
			for(Integer nextLine : vr.nextLines){
				visitorResultTable.get(nextLine).prevLines.add(vr.line);
			}
		}
		for(VisitorResult vr : visitorResultTable.values()){
			if(vr.prevLines.isEmpty())
				vr.prevLines.add(fun.sourcePos.line);
		}
		
		//liveAnalysis
		for(String var : fun.vars){
			if( ! vi.symbolUsageTable.containsKey(var)) continue;
			
			int begin = Integer.MAX_VALUE; 
			int end = 0;
			for(Integer line : vi.symbolAssignmentTable.get(var)){
				begin = begin > line ? line : begin;
				end   = end   < line ? line : end;
			}

			for(Integer line : vi.symbolUsageTable.get(var)){
				begin = begin > line ? line : begin;
				end   = end   < line ? line : end;
			}
			
			Variable v = new Variable(var,begin,end);
			variableTable.put(var,v);
		}
		
		//register assignment
		LinkedList<String> allRegisters = new LinkedList<String>();
		LinkedList<String> freeRegisters = new LinkedList<String>();
		if(vi.hasCall){
			for(String r : new String[]{"$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"}){
				freeRegisters.add(r);
				allRegisters.add(r);
			}
			
		}else{
			for(String r : new String[]{"$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8"}){
				freeRegisters.add(r);
				allRegisters.add(r);
			}
		}
		
		int maxconcurrentVariable = 0;
		int spill = 0;
		int calleeSaveSize = 0;
		int stackLocal = 0;
		int stackIn = fun.params.length > 4 ? fun.params.length - 4 : 0;
		int stackOut = vi.outSize;
		ArrayList<Variable> allvarlist = new ArrayList<Variable>(variableTable.values());
		HashSet<Variable> liveVar = new HashSet<Variable>();
		Collections.sort(allvarlist,Variable.liveRangeComparator);
		
		for(Variable v : allvarlist){
			int begin = v.begin;
			ArrayList<Variable> varToRemove = new ArrayList<Variable>();
			for(Variable v1: liveVar){
				if(v1.end <= begin){
					varToRemove.add(v1);
					freeRegisters.add(vi.symbolToRegisterTable.get(v1.name));
				}
			}
			
			for(Variable v1 : varToRemove)
				liveVar.remove(v1);
			
			liveVar.add(v);
			maxconcurrentVariable = liveVar.size() > maxconcurrentVariable ? liveVar.size() : maxconcurrentVariable;
			if(freeRegisters.isEmpty()){
				vi.symbolToRegisterTable.put(v.name,Integer.toString(spill));
				spill ++;
			}else{
				vi.symbolToRegisterTable.put(v.name, freeRegisters.poll());
			}
		}
		
		if(vi.hasCall){
			calleeSaveSize = maxconcurrentVariable > 8 ? 8 :maxconcurrentVariable;
			spill = maxconcurrentVariable > 8 ? maxconcurrentVariable - 8: 0;
		}else{
			spill = maxconcurrentVariable > 9 ? maxconcurrentVariable - 9: 0;
		}
		stackLocal = calleeSaveSize + spill;
		
		
		//append code
		////function header
		result += "func " + fun.ident + " [in " + stackIn + ", out " + stackOut + ", local " + stackLocal + "]\n";
		
		////update registerTable
		for(String name : vi.symbolToRegisterTable.keySet()){
			String reg = vi.symbolToRegisterTable.get(name);
			if(vi.isNumeric(reg)){
				reg = "local[" + (calleeSaveSize + Integer.valueOf(reg)) + "]";
				vi.symbolToRegisterTable.put(name, reg);
			}
		}
			
		////save callee-save registers
		if(vi.hasCall){
			for(int i = 0; i < calleeSaveSize; i++){
				result += "\tlocal[" + i + "] = $s" + i + "\n";
			}
		}
		
		////save passed arguments in register and in stack
		for(int i = 0; i < fun.params.length; i++){
			String arg = vi.symbolToRegisterTable.get(fun.params[i].toString());
			if(arg != null){
				if( i < 4){
					result += "\t" + arg + " = $a" + i + "\n";
				}else{
					result += "\t" + arg + " = in[" + (i - 4) + "]\n";
				}
			}
		}
		
		int labelNum = 0;
		for(VInstr instr : fun.body){
			//add label
			for(;labelNum < fun.labels.length && fun.labels[labelNum].sourcePos.line < instr.sourcePos.line; labelNum ++)
				result += "  " + fun.labels[labelNum].ident + ":\n";
			
			result += instr.accept(vi,visitor2);
		}
		
		VInstr instr = fun.body[fun.body.length - 1];
		if(vi.hasCall){
			for(int i = 0; i < calleeSaveSize; i++){
				result += "\t$s" + i + " = local[" + i + "]\n";
			}
		}
		result += "\tret\n\n";
	}
}
