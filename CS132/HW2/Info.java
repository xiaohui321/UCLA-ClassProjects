
public class Info {
	int indent;
	String currentClass;
	String currentMethod;
	String usedClass;
	boolean need_return_value;
	public Info(){
		indent = 0;
		need_return_value = false;
	}
	
	public void incrementIndent(){
		indent++;
	}
	
	public void decrementIndent(){
		indent--;
	}
	
	public String getIndent(){
		String ret_ = "";
		for(int i = 0; i < indent; i++)
			ret_ += "\t";
		return ret_;
	}
}
