import syntaxtree.*;
import visitor.*;

public class J2V {
  public static void main(String [] args) {
    try {
      Node root = new MiniJavaParser(System.in).Goal();
      SymbolTableConstructionVisitor visitor1 = new SymbolTableConstructionVisitor();
      root.accept(visitor1);
      SymbolTable st = visitor1.getSymbolTable();
      J2VVisitor visitor2 = new J2VVisitor(st);
      String result = st.getClassAndMethodDeclaration() + root.accept(visitor2, new Info()).result;
      if(st.needArrayAllocation)
        result += "func AllocArray(size)\n\tbytes = MulS(size 4)\n\tbytes = Add(bytes 4)\n\tv = HeapAllocZ(bytes)\n\t[v] = size\n\tret v\n\n";
      System.out.println(result);
    }
    catch (ParseException e) {
      System.out.println("Type error");
      System.exit(1);
    }
    catch (Error e1) {
     System.out.println("Type error");
     System.exit(1);
    }
 }
}