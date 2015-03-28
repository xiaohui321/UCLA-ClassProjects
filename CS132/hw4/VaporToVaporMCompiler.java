import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedList;
import java.util.TreeSet;

import cs132.vapor.ast.VFunction;
import cs132.vapor.ast.VInstr;
import cs132.vapor.ast.VVarRef.Local;


public class VaporToVaporMCompiler {
	String result;
	
	VisitorInfo info;
	FirstRoundVaporVisitor visitor1;
	SecondRoundVaporVisitor visitor2;
	Hashtable<Integer, Line> lineTable = new Hashtable<Integer, Line>();
	Hashtable<String, Variable> variableTable = new Hashtable<String,Variable>();

	public class Range{
		int begin;
		int end;
		public Range(int n){
			begin = n;
			end = n;
		}
	}
	
	public VaporToVaporMCompiler(VFunction fun) throws Exception{
		result = "";
		info = new VisitorInfo(fun);
		visitor1 = new FirstRoundVaporVisitor(fun);
		visitor2 = new SecondRoundVaporVisitor();
		
		//add parameters
		for(Local param : fun.params) {
			TreeSet<Integer> ts = new TreeSet<Integer>();
			ts.add(fun.sourcePos.line);
			info.updateAssignment(param.ident, fun.sourcePos.line);
		}
		
		//process results from first round visitor
		for(VInstr instr : fun.body){
			Line vr = instr.accept(info,visitor1);
			lineTable.put(vr.line, vr);
		}
	
		for(Line vr : lineTable.values()){
			for(Integer nextLine : vr.nextLines){
				lineTable.get(nextLine).prevLines.add(vr.line);
			}
		}
		for(Line vr : lineTable.values()){
			if(vr.prevLines.isEmpty())
				vr.prevLines.add(fun.sourcePos.line);
		}
		
		
		//live Analysis
		for(String var : fun.vars){
			if( ! info.symbolUsageTable.containsKey(var)) continue;
			
			int begin = Integer.MAX_VALUE; 
			int end = 0;
			for(Integer line : info.symbolUsageTable.get(var)){
				Range r = new Range(line);
				findAssignments(line,info.symbolAssignmentTable.get(var),r,new HashSet<Integer>());
				begin = begin > r.begin ? r.begin : begin;
				end   = end   < r.end ? r.end : end;
			}
			for(Integer line : info.symbolAssignmentTable.get(var)){
				begin = begin > line ? line : begin;
				end   = end   < line ? line : end;
			}

			Variable v = new Variable(var,begin,end);
			variableTable.put(var,v);
		}
		
		//register assignment
		TreeSet<String> freeRegisters = new TreeSet<String>(String.CASE_INSENSITIVE_ORDER);
		if(info.hasCall){
			for(String r : new String[]{"$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"}){
				freeRegisters.add(r);
			}
		}else{
			for(String r : new String[]{"$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8"}){
				freeRegisters.add(r);
			}
		}
		
		int maxconcurrentVariable = 0;
		int spill = 0;
		int calleeSaveSize = 0;
		int stackLocal = 0;
		int stackIn = fun.params.length > 4 ? fun.params.length - 4 : 0;
		int stackOut = info.outSize;
		ArrayList<Variable> allvarlist = new ArrayList<Variable>(variableTable.values());
		HashSet<Variable> liveVar = new HashSet<Variable>();
		Collections.sort(allvarlist,Variable.liveRangeComparator);
		
		for(Variable v : allvarlist){
			int begin = v.begin;
			ArrayList<Variable> varToRemove = new ArrayList<Variable>();
			for(Variable v1: liveVar)
				if(v1.end <= begin)
					varToRemove.add(v1);
			
			for(Variable v1 : varToRemove){
				liveVar.remove(v1);
				freeRegisters.add(info.symbolToRegisterTable.get(v1.name));
			}
			
			liveVar.add(v);
			
			maxconcurrentVariable = liveVar.size() > maxconcurrentVariable ? liveVar.size() : maxconcurrentVariable;
			
			if(freeRegisters.isEmpty()){
				info.symbolToRegisterTable.put(v.name,Integer.toString(spill++));
			}else{
				info.symbolToRegisterTable.put(v.name, freeRegisters.first());
				freeRegisters.remove(freeRegisters.first());
			}
		}
		spill = 0;
		if(info.hasCall){
			calleeSaveSize = maxconcurrentVariable > 8 ? 8 :maxconcurrentVariable;
			spill = maxconcurrentVariable > 8 ? maxconcurrentVariable - 8: 0;
		}else{
			spill = maxconcurrentVariable > 9 ? maxconcurrentVariable - 9: 0;
		}
		stackLocal = calleeSaveSize + spill;
		
		
		//append code
		result += "func " + fun.ident + " [in " + stackIn + ", out " + stackOut + ", local " + stackLocal + "]\n";
		
		////update registerTable
		for(String name : info.symbolToRegisterTable.keySet()){
			String reg = info.symbolToRegisterTable.get(name);
			
			if(isNumeric(reg)){
				reg = "local[" + (calleeSaveSize + Integer.valueOf(reg)) + "]";
				info.symbolToRegisterTable.put(name, reg);
			}
		}
			
		////save callee-save registers
		if(info.hasCall){
			for(int i = 0; i < calleeSaveSize; i++){
				result += "\tlocal[" + i + "] = $s" + i + "\n";
			}
		}
		
		////save passed arguments in register and in stack
		for(int i = 0; i < fun.params.length; i++){
			String arg = info.symbolToRegisterTable.get(fun.params[i].toString());
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
				result += fun.labels[labelNum].ident + ":\n";
			
			result += instr.accept(info,visitor2);
		}
		
		VInstr instr = fun.body[fun.body.length - 1];
		if(info.hasCall){
			for(int i = 0; i < calleeSaveSize; i++){
				result += "\t$s" + i + " = local[" + i + "]\n";
			}
		}
		result += "\tret\n\n";
	}

	private void findAssignments(Integer currentline, TreeSet<Integer> assignmentLines,
			Range r, HashSet<Integer> visited) {
		if(visited.contains(currentline)) return;
		visited.add(currentline);
		r.begin = r.begin > currentline ? currentline : r.begin;
		r.end   = r.end   > currentline ? r.end : currentline;
	
		if(assignmentLines.contains(currentline)) return;
		
		Line vr = lineTable.get(currentline);
		if(vr == null) return;
		for(Integer prevLine : vr.prevLines){
			findAssignments(prevLine,assignmentLines, r, visited);
		}
	}
	
	public boolean isNumeric(String str) {
		try{
			Double.parseDouble(str);  
		}catch(NumberFormatException e) {
			return false;  
		}return true;  
	}
}
