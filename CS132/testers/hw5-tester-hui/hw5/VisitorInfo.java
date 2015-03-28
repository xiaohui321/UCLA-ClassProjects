import java.util.Hashtable;
import java.util.Iterator;

public class VisitorInfo{
		boolean heapAlloc = false;
		boolean print = false;
		boolean error = false;
		int counter = 0;
		Hashtable<String,String> table= new Hashtable<String,String>();
		
		String getFinalPart(){
			String result = "\n";
			
			if(heapAlloc){ 
				result += "_heapAlloc:\n" 
						+ "\tli $v0 9\n"
						+ "\tsyscall\n"
						+ "\tjr $ra\n\n";
			}
						
			if(print){
				result += "_print:\n"
						+ "\tli $v0 1\n"
						+ "\tsyscall\n"
						+ "\tla $a0 _newline\n"
						+ "\tli $v0 4\n"
						+ "\tsyscall\n"
						+ "\tjr $ra\n\n";
			}

			if(error){
				result += "_error:\n"
						+ "\tli $v0 4\n"
						+ "\tsyscall\n"
						+ "\tli $v0 10\n"
						+ "\tsyscall\n\n";
			}
			
			result += ".data\n"
					+ ".align 0\n"
					+ "_newline: .asciiz \"\\n\"\n\n";

			for(String key : table.keySet()){
				result += table.get(key) + ": .asciiz " + key + "\n";
			}
			return result;
		}

		public String addNewString(String name) {
			String result =  "_str" + counter++;
			table.put(name,result);
			return result;
		}
	}