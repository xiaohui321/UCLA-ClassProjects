import visitor.GJDepthFirst;


public class J2V_Visitor extends GJDepthFirst<String,Object>{
	public SymbolTable st;

	public SymbolTable getSymbolTable() {
		return st;
	}
}
