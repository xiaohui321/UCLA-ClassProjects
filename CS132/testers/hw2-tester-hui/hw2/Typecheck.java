import syntaxtree.*;
import visitor.*;

public class Typecheck {
    public static void main(String [] args) {
       try {

            Node root = new MiniJavaParser(System.in).Goal();
                    
            FirstRoundVisitor visitor1= new FirstRoundVisitor();
            root.accept(visitor1);
            SymbolTable st = visitor1.getsymbolTable();
            
            GJDepthFirst<SymbolTable.Type,Object> visitor2 = new SecondRoundVisitor(st);
            root.accept(visitor2,null);
                    
            GJDepthFirst<String,Object> visitor3 = new ThirdRoundVisitor(st);
            root.accept(visitor3,null);
            System.out.println("Program type checked successfully");
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