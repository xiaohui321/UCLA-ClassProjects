
public class Result {
	public enum RESULT_TYPE{
		INT, INT_ARRAY, BOOLEAN,STRING
	}

	String usedClass;
	
	RESULT_TYPE type;

	String result = "";
	
	String returnValue = "";
	boolean hasReturnValue;
	public Result(){
		hasReturnValue = false;
	}
	
	public void mergeResult(Result r){
		if (r == null) return;
		result += r.result;
	}

	public void appendResult(String string) {
		result += string;
	}
	
	public void setReturnValue(String s){
		if(returnValue != "")
			throw new Error();
		hasReturnValue = true;
		returnValue = s;
	}

	public void mergeReturnResultAndValue(Result r) {
		if (r == null) return;
		result += r.result;
		returnValue += r.returnValue;
		
	}
}
