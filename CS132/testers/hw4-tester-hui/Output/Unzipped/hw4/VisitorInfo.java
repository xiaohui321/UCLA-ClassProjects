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
		if(name.charAt(0) == ':' || isNumeric(name)) return;
		
		TreeSet<Integer> ts;
		if(symbolAssignmentTable.contains(name)){
			ts = symbolAssignmentTable.get(name);
		}else{
			ts = new TreeSet<Integer>();
		}
		ts.add(new Integer(line));
		symbolAssignmentTable.put(name,ts);
	}
	
	public void updateUsage(String name, int line) {
		if(name.charAt(0) == ':' || isNumeric(name)) return;
		
		TreeSet<Integer> ts;
		if(symbolUsageTable.contains(name)){
			ts = symbolUsageTable.get(name);
		}else{
			ts = new TreeSet<Integer>();
		}
		ts.add(new Integer(line));
		symbolUsageTable.put(name,ts);
	}
	
	public static boolean isNumeric(String str) {  
	  try{
	    Double.parseDouble(str);  
	  }catch(NumberFormatException e) {
	    return false;  
	  }return true;  
	}
	
	public String getCorrespondingVariable(String name){
		if(isNumeric(name) || name.charAt(0) == ':') return name;
		return symbolToRegisterTable.get(name);
	}
}
