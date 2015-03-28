import java.util.Hashtable;
import java.util.TreeSet;

import cs132.vapor.ast.VFunction;


public class VisitorInfo {
	//map symbol name to a set of assignment positions
	Hashtable<String, TreeSet<Integer>> symbolAssignmentTable = new Hashtable<String,TreeSet<Integer>>();
	Hashtable<String, TreeSet<Integer>> symbolUsageTable = new Hashtable<String,TreeSet<Integer>>();
	Hashtable<String, String> symbolToRegisterTable = new Hashtable<String,String>();

	VFunction fun;
	public int outSize;
	public boolean hasCall;

	public VisitorInfo(VFunction fun){
		this.fun = fun;
		outSize = 0;
		hasCall = false;
	}

	public void updateOutSize(int size){
		hasCall = true;
		outSize = size > outSize ? size : outSize;  
	}

	public void updateAssignment(String name, int line) {
		if(isActualValue(name)) return;
		if(!symbolAssignmentTable.containsKey(name)){
			symbolAssignmentTable.put(name,new TreeSet<Integer>());
		}
		symbolAssignmentTable.get(name).add(new Integer(line));
	}

	public void updateUsage(String name, int line) {
		if(isActualValue(name)) return;
		if(!symbolUsageTable.containsKey(name)){
			symbolUsageTable.put(name,new TreeSet<Integer>());
		}
		symbolUsageTable.get(name).add(new Integer(line));		
	}

	public static boolean isActualValue(String str) {
		if(str.charAt(0) == ':') return true;
		try{
			Double.parseDouble(str);  
		}catch(NumberFormatException e) {
			return false;  
		}
		return true;  
	}

	public String getCorrespondingVariable(String name){
		if(isActualValue(name)) return name;
		return symbolToRegisterTable.get(name);
	}
}
